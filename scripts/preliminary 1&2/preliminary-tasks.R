# Load the packages, preprocessing function, and original labeled data
library(tidyverse)
library(tidytext)
library(rvest)
library(rsample)
library(Matrix)
library(irlba)
library(textstem)

source("scripts/preliminary 1&2/preprocessing.R")
load("data/claims-raw.RData")

# Extract paragraph text and headings from the HTML
claims <- claims_raw %>%
  filter(str_detect(text_tmp, "<!")) %>%
  mutate(bclass = factor(bclass))

claims$paragraph <- map_chr(seq_len(nrow(claims)), function(i) {
  if (i %% 50 == 0) gc()
  clean_text(claims$text_tmp[[i]])
})
claims$paragraph_heading <- map_chr(seq_len(nrow(claims)), function(i) {
  if (i %% 50 == 0) gc()
  clean_text(claims$text_tmp[[i]], "p, h1, h2, h3, h4, h5, h6")
})

# Use one stratified split for both tasks
set.seed(110122)
split <- initial_split(claims, prop = 0.8, strata = bclass)
train <- training(split)
validation <- testing(split)
positive_class <- levels(claims$bclass)[2]
train_y <- as.integer(train$bclass == positive_class)
validation_y <- as.integer(validation$bclass == positive_class)

# Build TF-IDF matrices using the training vocabulary and weights
make_matrix <- function(tokens, ids, vocabulary, idf) {
  counts <- tokens %>%
    filter(.id %in% ids) %>%
    count(.id, term, name = "n")
  rows <- match(counts$.id, ids)
  columns <- match(counts$term, vocabulary)
  known <- !is.na(columns)
  matrix <- sparseMatrix(
    i = rows[known], j = columns[known], x = counts$n[known],
    dims = c(length(ids), length(vocabulary))
  )
  matrix %*% Diagonal(x = idf)
}

fit_text_pca <- function(tokens, ids, number_of_components, minimum_documents = 1) {
  document_counts <- tokens %>%
    filter(.id %in% ids) %>%
    distinct(.id, term) %>%
    count(term, name = "documents") %>%
    filter(documents >= minimum_documents)
  vocabulary <- document_counts$term
  idf <- log((length(ids) + 1) / (document_counts$documents + 1)) + 1
  matrix <- make_matrix(tokens, ids, vocabulary, idf)
  components <- min(number_of_components, nrow(matrix) - 1, ncol(matrix) - 1)
  pca <- prcomp_irlba(matrix, n = components, center = TRUE, scale. = FALSE)
  list(vocabulary = vocabulary, idf = idf, pca = pca)
}

get_scores <- function(tokens, ids, text_pca) {
  matrix <- make_matrix(tokens, ids, text_pca$vocabulary, text_pca$idf)
  center_offset <- as.numeric(crossprod(text_pca$pca$center, text_pca$pca$rotation))
  scores <- as.data.frame(sweep(as.matrix(matrix %*% text_pca$pca$rotation), 2, center_offset, "-"))
  names(scores) <- paste0("PC", seq_len(ncol(scores)))
  scores
}

# Task 1: compare paragraphs with paragraphs plus headings
fit_pcr <- function(text_column) {
  tokens <- claims %>%
    select(.id, text = all_of(text_column)) %>%
    unnest_tokens(term, text) %>%
    anti_join(stop_words, by = c("term" = "word"))

  text_pca <- fit_text_pca(tokens, train$.id, 10)
  train_scores <- get_scores(tokens, train$.id, text_pca)
  validation_scores <- get_scores(tokens, validation$.id, text_pca)
  model <- glm(train_y ~ ., data = cbind(train_y, train_scores), family = binomial())
  predicted <- as.integer(predict(model, validation_scores, type = "response") >= 0.5)
  mean(predicted == validation_y)
}

paragraph_accuracy <- fit_pcr("paragraph")
heading_accuracy <- fit_pcr("paragraph_heading")
task1_difference <- heading_accuracy - paragraph_accuracy

cat("Task 1\n")
cat("Paragraph-only accuracy:", round(paragraph_accuracy, 4), "\n")
cat("Paragraph-plus-heading accuracy:", round(heading_accuracy, 4), "\n")
cat("Difference (with headings minus paragraphs):", round(task1_difference, 4), "\n\n")

task1_comparison <- tibble(
  model = c("Paragraph only", "Paragraph plus headings"),
  validation_accuracy = c(paragraph_accuracy, heading_accuracy),
  difference_from_baseline = c(0, task1_difference)
)
dir.create("results/table", showWarnings = FALSE)
write_csv(task1_comparison, "results/table/task1-accuracy.csv")

# Task 2: tokenize the paragraph text into words and bigrams
word_tokens <- claims %>%
  select(.id, text = paragraph) %>%
  unnest_tokens(term, text) %>%
  anti_join(stop_words, by = c("term" = "word")) %>%
  mutate(term = lemmatize_words(term)) %>%
  filter(str_length(term) > 2)

bigram_tokens <- claims %>%
  select(.id, text = paragraph) %>%
  unnest_tokens(term, text, token = "ngrams", n = 2)

# Fit the word model and get its predicted log-odds
fit_word_model <- function(ids) {
  text_pca <- fit_text_pca(word_tokens, ids, 10)
  scores <- get_scores(word_tokens, ids, text_pca)
  y <- as.integer(train$bclass[match(ids, train$.id)] == positive_class)
  model <- glm(y ~ ., data = cbind(y, scores), family = binomial())
  list(text_pca = text_pca, model = model)
}

word_log_odds <- function(fit, ids) {
  scores <- get_scores(word_tokens, ids, fit$text_pca)
  as.numeric(predict(fit$model, scores, type = "link"))
}

# Train the second model with out-of-fold word predictions
set.seed(110122)
folds <- vfold_cv(train, v = 5, strata = bclass)
oof_log_odds <- numeric(nrow(train))
for (fold in folds$splits) {
  fold_train <- analysis(fold)$.id
  fold_holdout <- assessment(fold)$.id
  fold_model <- fit_word_model(fold_train)
  oof_log_odds[match(fold_holdout, train$.id)] <- word_log_odds(fold_model, fold_holdout)
}

word_model <- fit_word_model(train$.id)
validation_log_odds <- word_log_odds(word_model, validation$.id)
baseline_predicted <- as.integer(plogis(validation_log_odds) >= 0.5)
baseline_accuracy <- mean(baseline_predicted == validation_y)

bigram_pca <- fit_text_pca(bigram_tokens, train$.id, 10, minimum_documents = 2)
train_bigrams <- get_scores(bigram_tokens, train$.id, bigram_pca)
validation_bigrams <- get_scores(bigram_tokens, validation$.id, bigram_pca)
names(train_bigrams) <- names(validation_bigrams) <- paste0("bigram_PC", seq_len(ncol(train_bigrams)))

second_train <- data.frame(y = train_y, word_log_odds = oof_log_odds, train_bigrams)
second_validation <- data.frame(word_log_odds = validation_log_odds, validation_bigrams)
second_model <- glm(y ~ ., data = second_train, family = binomial())
combined_predicted <- as.integer(predict(second_model, second_validation, type = "response") >= 0.5)
combined_accuracy <- mean(combined_predicted == validation_y)
task2_difference <- combined_accuracy - baseline_accuracy

# Compare validation accuracy and save the Task 2 results
cat("Task 2\n")
cat("Word-only accuracy:", round(baseline_accuracy, 4), "\n")
cat("Word-plus-bigram accuracy:", round(combined_accuracy, 4), "\n")
cat("Difference (with bigrams minus word only):", round(task2_difference, 4), "\n")

task2_comparison <- tibble(
  model = c("Word only", "Word plus bigrams"),
  validation_accuracy = c(baseline_accuracy, combined_accuracy),
  difference_from_baseline = c(0, task2_difference)
)
write_csv(task2_comparison, "results/table/task2-accuracy.csv")
