library(tidyverse)
library(tidytext)
library(rvest)
library(rsample)
library(Matrix)
library(irlba)
library(textstem)

source('scripts/preliminary 1&2/preprocessing.R')
load('data/claims-raw.RData')

# Keep the same labeled pages and stratified split for both comparisons.
eligible <- str_detect(claims_raw$text_tmp, '<!')
claims <- claims_raw[eligible, ] |> mutate(bclass = factor(bclass))
set.seed(110122)
split <- initial_split(claims, prop = 0.8, strata = bclass)
train <- training(split)
holdout <- testing(split)
train_ids <- train$.id
holdout_ids <- holdout$.id
positive <- levels(claims$bclass)[2]
labels <- function(ids) as.integer(claims$bclass[match(ids, claims$.id)] == positive)
train_y <- labels(train_ids)

# Extract paragraph and heading text, then tokenize each page.
paragraph_elements <- extract_pages(claims, 'p')
heading_elements <- extract_pages(claims, 'p,h1,h2,h3,h4,h5,h6')
paragraph_tokens <- make_tokens(paragraph_elements)
heading_tokens <- make_tokens(heading_elements)
bigram_tokens <- make_tokens(paragraph_elements, 2L)

# Count tokens by page before filtering to the training vocabulary.
make_matrix <- function(tokens, ids, vocabulary, idf) {
  counts <- tokens |>
    filter(.id %in% ids) |>
    count(.id, term, name = 'n') |>
    group_by(.id) |>
    mutate(total = sum(n)) |>
    ungroup()
  rows <- match(counts$.id, ids)
  cols <- match(counts$term, vocabulary)
  known <- !is.na(cols)
  sparseMatrix(i = rows[known], j = cols[known],
               x = (counts$n / counts$total * idf[cols])[known],
               dims = c(length(ids), length(vocabulary)),
               dimnames = list(ids, vocabulary))
}

# Fit TF-IDF and PCA using only the fitting pages.
fit_pca <- function(tokens, ids, k, min_df = 1L, filtered = FALSE) {
  df <- tokens |>
    filter(.id %in% ids) |>
    distinct(.id, term) |>
    count(term, name = 'df') |>
    filter(df >= min_df)
  if (filtered) {
    frequency <- tokens |>
      filter(.id %in% ids) |>
      count(term, name = 'frequency')
    df <- df |>
      filter(df >= 3, df < 0.9 * length(ids)) |>
      inner_join(frequency, by = 'term') |>
      arrange(desc(frequency), term) |>
      slice_head(n = 400)
  }
  idf <- log(length(ids) / df$df)
  x <- make_matrix(tokens, ids, df$term, idf)
  pca <- prcomp_irlba(x, n = k, center = TRUE, scale. = FALSE)
  list(vocabulary = df$term, idf = idf, pca = pca)
}

# Apply the fitted vocabulary, weights, and PCA rotation to pages in ID order.
scores <- function(tokens, ids, fitted) {
  x <- make_matrix(tokens, ids, fitted$vocabulary, fitted$idf)
  out <- as.matrix(x %*% fitted$pca$rotation)
  out <- sweep(out, 2, as.numeric(crossprod(fitted$pca$center,
                                           fitted$pca$rotation)), '-')
  as.data.frame(out)
}

# Fit logistic PCR and return predicted word log-odds.
fit_pcr <- function(tokens, ids, k, min_df = 1L, filtered = FALSE) {
  pca <- fit_pca(tokens, ids, k, min_df, filtered)
  x <- scores(tokens, ids, pca)
  fit <- glm(y ~ ., data = data.frame(y = labels(ids), x), family = binomial())
  list(pca = pca, fit = fit, tokens = tokens)
}
link <- function(model, ids) {
  as.numeric(predict(model$fit, scores(model$tokens, ids, model$pca), type = 'link'))
}
accuracy <- function(link_values, ids) mean(as.integer(link_values >= 0) == labels(ids))

# Calculate ROC AUC with label 1 as the positive class.
auc <- function(link_values, ids) {
  truth <- labels(ids)
  positive_count <- sum(truth == 1)
  negative_count <- sum(truth == 0)
  ranks <- rank(link_values, ties.method = 'average')
  (sum(ranks[truth == 1]) - positive_count * (positive_count + 1) / 2) /
    (positive_count * negative_count)
}

# Select PC counts using the same training folds for both Task 1 models.
set.seed(110122)
folds <- vfold_cv(train, v = 5, strata = bclass)$splits
choices <- c(10L, 25L, 50L, 100L)
cv_grid <- function(tokens, min_df = 1L, filtered = FALSE,
                    pc_choices = choices) {
  fold_results <- lapply(folds, function(fold) {
    fit_ids <- analysis(fold)$.id
    val_ids <- assessment(fold)$.id
    fitted <- fit_pca(tokens, fit_ids, max(pc_choices), min_df, filtered)
    x_fit <- scores(tokens, fit_ids, fitted)
    x_val <- scores(tokens, val_ids, fitted)
    vapply(pc_choices, function(k) {
      model <- glm(
        y ~ .,
        data = data.frame(y = labels(fit_ids), x_fit[, seq_len(k), drop = FALSE]),
        family = binomial()
      )
      pred <- predict(model, x_val[, seq_len(k), drop = FALSE], type = 'link')
      accuracy(pred, val_ids)
    }, numeric(1))
  })
  rowMeans(do.call(cbind, fold_results))
}
paragraph_cv <- cv_grid(paragraph_tokens)
heading_cv <- cv_grid(heading_tokens)
base_k1 <- choices[which.max((paragraph_cv + heading_cv) / 2)]

# Preserve the original Task 2 random-number sequence before the new Task 1 experiment.
base_paragraph_fit <- fit_pcr(paragraph_tokens, train_ids, base_k1)
base_heading_fit <- fit_pcr(heading_tokens, train_ids, base_k1)
task2_random_state <- .Random.seed

# Compare the two vocabularies at 100 PCs and test filtered 20 and 50 PC models.
filtered_choices <- c(20L, 50L, 100L)
filtered_paragraph_cv <- cv_grid(paragraph_tokens, filtered = TRUE,
                                 pc_choices = filtered_choices)
filtered_heading_cv <- cv_grid(heading_tokens, filtered = TRUE,
                               pc_choices = filtered_choices)
config_accuracy <- c(
  mean(c(paragraph_cv[choices == 100], heading_cv[choices == 100])),
  (filtered_paragraph_cv + filtered_heading_cv) / 2
)
selected_config <- which.max(config_accuracy)
task1_filtered <- selected_config > 1
task1_pcs <- if (task1_filtered) filtered_choices[selected_config - 1] else 100L

# Fit the selected setting for both texts and predict the same holdout pages.
if (!task1_filtered && base_k1 == 100L) {
  paragraph_fit <- base_paragraph_fit
  heading_fit <- base_heading_fit
} else {
  paragraph_fit <- fit_pcr(paragraph_tokens, train_ids, task1_pcs,
                           filtered = task1_filtered)
  heading_fit <- fit_pcr(heading_tokens, train_ids, task1_pcs,
                         filtered = task1_filtered)
}
task1_paragraph_link <- link(paragraph_fit, holdout_ids)
task1_heading_link <- link(heading_fit, holdout_ids)
task1_p <- accuracy(task1_paragraph_link, holdout_ids)
task1_h <- accuracy(task1_heading_link, holdout_ids)
task1_p_auc <- auc(task1_paragraph_link, holdout_ids)
task1_h_auc <- auc(task1_heading_link, holdout_ids)

# Restore the original random-number state before Task 2 model selection.
.Random.seed <- task2_random_state

# Select word and bigram PC counts using training folds only.
bigram_cv <- cv_grid(bigram_tokens, 2L)
kw <- choices[which.max(paragraph_cv)]
kb <- choices[which.max(bigram_cv)]

# Predict each training fold using the word model fitted on the other folds.
oof <- rep(NA_real_, length(train_ids))
for (fold in folds) {
  fit_ids <- analysis(fold)$.id
  val_ids <- assessment(fold)$.id
  idx <- match(val_ids, train_ids)
  oof[idx] <- link(fit_pcr(paragraph_tokens, fit_ids, kw), val_ids)
}

# Fit the full-training word model and prepare holdout log-odds.
word_fit <- fit_pcr(paragraph_tokens, train_ids, kw)
word_hold <- link(word_fit, holdout_ids)

# Fit bigram PCA on training pages and score both partitions.
bigram_pca <- fit_pca(bigram_tokens, train_ids, kb, 2L)
bigram_train <- scores(bigram_tokens, train_ids, bigram_pca)
bigram_hold <- scores(bigram_tokens, holdout_ids, bigram_pca)
names(bigram_train) <- names(bigram_hold) <- paste0('bigram_PC', seq_len(kb))

# Fit the two second-stage models and compare their holdout accuracy.
stage2_word <- glm(
  y ~ .,
  data = data.frame(y = train_y, word_log_odds = oof),
  family = binomial()
)
stage2_bigram <- glm(
  y ~ .,
  data = data.frame(y = train_y, word_log_odds = oof, bigram_train),
  family = binomial()
)
task2_word_link <- as.numeric(predict(
  stage2_word, data.frame(word_log_odds = word_hold), type = 'link'
))
task2_bigram_link <- as.numeric(predict(
  stage2_bigram, data.frame(word_log_odds = word_hold, bigram_hold), type = 'link'
))

# Calculate holdout accuracy and ROC AUC from the predicted log-odds.
task2_word <- accuracy(task2_word_link, holdout_ids)
task2_bigram <- accuracy(task2_bigram_link, holdout_ids)
task2_word_auc <- auc(task2_word_link, holdout_ids)
task2_bigram_auc <- auc(task2_bigram_link, holdout_ids)

# Save each controlled comparison as one row of percentages and percentage points.
write_csv(
  tibble(
    paragraph_only_accuracy_pct = sprintf('%.2f', 100 * task1_p),
    paragraph_plus_headings_accuracy_pct = sprintf('%.2f', 100 * task1_h),
    change_after_adding_headings_pp = sprintf('%.2f', 100 * (task1_h - task1_p)),
    paragraph_only_roc_auc = sprintf('%.4f', task1_p_auc),
    paragraph_plus_headings_roc_auc = sprintf('%.4f', task1_h_auc),
    change_after_adding_headings_auc = sprintf('%.4f', task1_h_auc - task1_p_auc)
  ),
  'results/table/task1-accuracy.csv'
)
write_csv(
  tibble(
    word_log_odds_only_accuracy_pct = sprintf('%.2f', 100 * task2_word),
    word_log_odds_plus_bigram_pcs_accuracy_pct = sprintf('%.2f', 100 * task2_bigram),
    change_after_adding_bigrams_pp = sprintf('%.2f', 100 * (task2_bigram - task2_word)),
    word_log_odds_only_roc_auc = sprintf('%.4f', task2_word_auc),
    word_log_odds_plus_bigram_pcs_roc_auc = sprintf('%.4f', task2_bigram_auc),
    change_after_adding_bigrams_auc = sprintf('%.4f', task2_bigram_auc - task2_word_auc)
  ),
  'results/table/task2-accuracy.csv'
)

# Print the training CV comparison and the selected Task 1 setting.
cat('Task 1 CV accuracy at 100 PCs: original paragraph',
    paragraph_cv[choices == 100], 'original headings',
    heading_cv[choices == 100], 'filtered paragraph',
    filtered_paragraph_cv[filtered_choices == 100], 'filtered headings',
    filtered_heading_cv[filtered_choices == 100], '\n')
cat('Task 1 selected:', if (task1_filtered) 'filtered' else 'original',
    'vocabulary,', task1_pcs, 'PCs\n')
