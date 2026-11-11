# Extract paragraph text, with headings when requested
clean_text <- function(html, selector = "p") {
  extracted <- tryCatch(
    read_html(html) %>% html_elements(selector) %>% html_text2() %>% str_c(collapse = " "),
    error = function(e) {
      tags <- if (selector == "p") "p" else "p|h[1-6]"
      pattern <- paste0("(?is)<(?:", tags, ")\\b[^>]*>.*?</(?:", tags, ")>")
      str_extract_all(html, pattern)[[1]] %>%
        str_remove_all("<[^>]*>") %>%
        str_c(collapse = " ")
    }
  )

  extracted %>%
    str_to_lower() %>%
    str_replace_all("[^[:alpha:] ]", " ") %>%
    str_squish()
}
