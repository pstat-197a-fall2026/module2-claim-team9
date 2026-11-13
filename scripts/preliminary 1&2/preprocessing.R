extract_elements <- function(html, selector) {
  tryCatch(
    read_html(html) |> html_elements(selector) |> html_text2(),
    error = function(e) character()
  )
}

normalize_text <- function(x) {
  x <- str_remove_all(x, 'https?://\\S+|www\\.\\S+')
  x <- str_remove_all(x, '[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}')
  str_squish(str_to_lower(str_replace_all(x, '[^[:alpha:] ]', ' ')))
}

# Extract visible text while retaining each paragraph's boundary.
extract_pages <- function(pages, selector) {
  map2_dfr(pages$.id, pages$text_tmp, function(id, html) {
    elements <- extract_elements(html, selector)
    tibble(.id = id, element = seq_along(elements), text = normalize_text(elements))
  })
}

# Clean words consistently and form bigrams before removing unwanted pairs.
make_tokens <- function(elements, ngram = 1L) {
  if (!nrow(elements)) return(tibble(.id = character(), term = character()))
  raw <- unnest_tokens(elements, token, text, token = 'words', drop = TRUE)
  if (ngram == 1L) {
    raw |>
      filter(!token %in% stop_words$word) |>
      mutate(term = lemmatize_words(token)) |>
      filter(nchar(term) > 2) |>
      select(.id, term)
  } else {
    raw |>
      group_by(.id, element) |>
      mutate(next_token = lead(token)) |>
      ungroup() |>
      filter(!is.na(next_token),
             !token %in% stop_words$word,
             !next_token %in% stop_words$word) |>
      mutate(first = lemmatize_words(token),
             second = lemmatize_words(next_token)) |>
      filter(nchar(first) > 2, nchar(second) > 2) |>
      transmute(.id, term = paste(first, second, sep = ' '))
  }
}
