library(glmnet)
library(yardstick)
source('scripts/primary/4apreprocessing.R')

load('data/claims-raw.RData')   
set.seed(197)

# ---- preprocess (slow: lemmatizing); cache the result ----
parsed_train <- parse_data2(claims_raw)
counts_train <- count_tokens(parsed_train)
saveRDS(counts_train, 'data/counts-train.rds')

# one row per document, label aligned to ids
labels <- parsed_train %>% distinct(.id, bclass)
ids    <- labels$.id
y      <- factor(labels$bclass)
levels(y)    # check the label strings

# ---- hold-out split for honest accuracy; IDF learned on the training part only ----
tr <- sample(length(ids), 0.8 * length(ids))
idf_tr <- fit_idf(filter(counts_train, .id %in% ids[tr]))
X_tr <- make_dtm(filter(counts_train, .id %in% ids[tr]),  ids[tr],  idf_tr)
X_va <- make_dtm(filter(counts_train, .id %in% ids[-tr]), ids[-tr], idf_tr)

foldid <- sample(rep(1:10, length.out = length(tr)))   # same folds for every alpha
results <- map_dfr(c(0, 0.25, 0.5, 0.75, 1), function(a){
  cv <- cv.glmnet(X_tr, y[tr], family = 'binomial', alpha = a,
                  type.measure = 'class', foldid = foldid)
  p  <- as.numeric(predict(cv, X_va, s = 'lambda.min', type = 'response'))
  pred <- factor(ifelse(p > 0.5, levels(y)[2], levels(y)[1]), levels = levels(y))
  tibble(alpha = a,
         acc = accuracy_vec(y[-tr], pred),
         auc = roc_auc_vec(y[-tr], p, event_level = 'second'))
})
results

# ---- refit on ALL labeled data with the best alpha ----
best_alpha <- results$alpha[which.max(results$auc)]
idf_all <- fit_idf(counts_train)
X_all   <- make_dtm(counts_train, ids, idf_all)
final   <- cv.glmnet(X_all, y, family = 'binomial', alpha = best_alpha,
                     type.measure = 'class', nfolds = 10)

empty_tr <- Matrix::rowSums(X_all) == 0
fallback <- names(which.max(table(y[empty_tr])))   # "N/A: No relevant content."

saveRDS(list(fit = final, idf = idf_all, levels = levels(y),
             fallback = fallback),
        'results/bclass-glmnet.rds')