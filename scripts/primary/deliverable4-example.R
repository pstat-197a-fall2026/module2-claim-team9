#set up
multi_mod <- readRDS("results/4b-svm-multiclass.rds")
bin_mod <- readRDS("results/bclass-glmnet.rds")
load("data/claims-test.RData")

library(tidyverse)
library(tidymodels)
library(textrecipes)
library(glmnet)


#generate binary predictions
source('scripts/primary/4apreprocessing.R')
parsed_test <- parse_data2(claims_test)   # may drop pages without '<!'
counts_test <- count_tokens(parsed_test)

ids_test <- claims_test$.id               # ALL 929, not just the parsed ones
X_test   <- make_dtm(counts_test, ids_test, bin_mod$idf)

p <- as.numeric(predict(bin_mod$fit, X_test, s = 'lambda.min', type = 'response'))
bclass.pred <- ifelse(p > 0.5, bin_mod$levels[2], bin_mod$levels[1])
empty <- Matrix::rowSums(X_test) == 0
bclass.pred[empty] <- bin_mod$fallback

sum(empty)
table(bclass.pred)

bin_df <- tibble(.id = ids_test, bclass.pred = bclass.pred)

#generate multiclass predictions
source('scripts/primary/4bpreprocessing.R')
test_clean <- claims_test %>% parse_data() %>% add_tokens()   # may drop pages without '<!'
pred_m <- tibble(
  .id         = test_clean$.id,
  mclass.pred = predict(multi_mod, test_clean)$.pred_class
)

mclass_levels <- levels(extract_mold(multi_mod)$outcomes$mclass)
majority <- "N/A: No relevant content."
stopifnot(majority %in% mclass_levels)

multi_df <- tibble(.id = ids_test) %>%
  left_join(pred_m, by = '.id') %>%
  mutate(mclass.pred = coalesce(
    mclass.pred,
    factor(majority, levels = mclass_levels)))

table(multi_df$mclass.pred)

#predictions from both models, one row per test page
pred_df <- bin_df %>%
  left_join(multi_df, by = '.id')

stopifnot(nrow(pred_df) == length(ids_test), !anyNA(pred_df))
head(pred_df)

#check: should match predictions from preds-group9.RData
if (file.exists('results/preds-group9.RData')) {
  submitted <- new.env()
  load('results/preds-group9.RData', envir = submitted)   # contains pred_df
  chk <- inner_join(pred_df, submitted$pred_df, by = '.id', suffix = c('.new', '.sub'))
  c(binary     = mean(chk$bclass.pred.new == chk$bclass.pred.sub),
    multiclass = mean(chk$mclass.pred.new == chk$mclass.pred.sub))   # expect 1 and 1
}