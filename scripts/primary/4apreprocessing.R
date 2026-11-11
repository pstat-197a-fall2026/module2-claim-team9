source('scripts/raw/preprocessing.R')   # keeps your original functions
require(Matrix)

# shared text cleaning (same steps as your parse_fn)
clean_text <- function(x){
  x %>%
    str_c(collapse = ' ') %>%
    rm_url() %>%
    rm_email() %>%
    str_remove_all('\'') %>%
    str_replace_all(paste(c('\n', '[[:punct:]]', 'nbsp',
                            '[[:digit:]]', '[[:symbol:]]'),
                          collapse = '|'), ' ') %>%
    str_replace_all("([a-z])([A-Z])", "\\1 \\2") %>%
    tolower() %>%
    str_replace_all("\\s+", " ")
}

# selector = 'p' reproduces the original; add headers to capture them
parse_fn2 <- function(.html, selector = 'h1, h2, h3, h4, p'){
  read_html(.html) %>%
    html_elements(selector) %>%
    html_text2() %>%
    clean_text()
}

parse_data2 <- function(.df, selector = 'h1, h2, h3, h4, p'){
  .df %>%
    filter(str_detect(text_tmp, '<!')) %>%
    rowwise() %>%
    mutate(text_clean = parse_fn2(text_tmp, selector)) %>%
    ungroup()
}

# tokens -> per-document counts (no labels needed, so works on test too)
count_tokens <- function(parsed){
  parsed %>%
    unnest_tokens(output = token, input = text_clean, token = 'words',
                  stopwords = str_remove_all(stop_words$word, '[[:punct:]]')) %>%
    mutate(token.lem = lemmatize_words(token)) %>%
    filter(str_length(token.lem) > 2) %>%
    count(.id, token.lem, name = 'n')
}

# learn vocabulary + IDF from TRAINING counts only
fit_idf <- function(counts, min_df = 5){
  N <- n_distinct(counts$.id)
  counts %>%
    group_by(token.lem) %>%
    summarize(df = n(), .groups = 'drop') %>%
    filter(df >= min_df) %>%
    mutate(idf = log(N / df))
}

# apply saved vocab/IDF to any counts -> sparse tf-idf matrix, one row per id
make_dtm <- function(counts, ids, idf_tbl){
  vocab <- idf_tbl$token.lem
  d <- counts %>%
    group_by(.id) %>% mutate(tf = n / sum(n)) %>% ungroup() %>%  # tf before vocab filter
    inner_join(idf_tbl, by = 'token.lem') %>%
    mutate(tf_idf = tf * idf)
  sparseMatrix(i = match(d$.id, ids), j = match(d$token.lem, vocab),
               x = d$tf_idf, dims = c(length(ids), length(vocab)),
               dimnames = list(ids, vocab))
}
