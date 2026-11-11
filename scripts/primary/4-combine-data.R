load("results/preds-4a.RData")
binary_df = pred_df
load("results/4b-mclass-preds.RData")
multi_df = pred_df

pred_df = merge(binary_df, multi_df, by = ".id")
nrow(final_df)
nrow(binary_df)
nrow(multi_df)

save(pred_df, file = 'results/preds-group9.RData')
