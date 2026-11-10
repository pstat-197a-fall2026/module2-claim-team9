source('scripts/4apreprocessing.R')
library(glmnet)
load('data/claims-test.RData')            
mod <- readRDS('results/bclass-glmnet.rds')

parsed_test <- parse_data2(claims_test)   # may drop pages without '<!'
counts_test <- count_tokens(parsed_test)

ids_test <- claims_test$.id               # ALL 929, not just the parsed ones
X_test   <- make_dtm(counts_test, ids_test, mod$idf)

p <- as.numeric(predict(mod$fit, X_test, s = 'lambda.min', type = 'response'))
bclass.pred <- ifelse(p > 0.5, mod$levels[2], mod$levels[1])

# pages that were dropped or had no usable tokens get an all-zero row;
# give them the fallback class (N/A, which is most common among empty training pages)
empty <- Matrix::rowSums(X_test) == 0
bclass.pred[empty] <- mod$fallback

sum(empty)
table(bclass.pred)

pred_df <- tibble(.id = ids_test, bclass.pred = bclass.pred)
save(pred_df, file = 'results/preds-4a.RData')
