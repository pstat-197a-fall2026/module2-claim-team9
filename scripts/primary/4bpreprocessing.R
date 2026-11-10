require(tidyverse)
require(tidytext)
require(textstem)
require(rvest)
require(qdapRegex)
require(stopwords)
require(tokenizers)

# same cleaning steps as the starter
clean_txt <- function(x){
  x <- x[!is.na(x)]
  if (length(x) == 0) return('')
  x %>%
    str_c(collapse = ' ') %>%
    rm_url() %>%
    rm_email() %>%
    str_remove_all('\'') %>%
    str_replace_all(paste(c('\n',
                            '[[:punct:]]',
                            'nbsp',
                            '[[:digit:]]',
                            '[[:symbol:]]'),
                          collapse = '|'), ' ') %>%
    str_replace_all("([a-z])([A-Z])", "\\1 \\2") %>%
    tolower() %>%
    str_replace_all("\\s+", " ")
}

#applying to title, meta description, paragraph text
#ie keeping title and meta description as well 

parse_fn <- function(.html){
  doc <- read_html(.html)
  
  # case-insensitive match on <meta name="description" content="...">
  meta_xp <- paste0("//meta[translate(@name,",
                    "'ABCDEFGHIJKLMNOPQRSTUVWXYZ',",
                    "'abcdefghijklmnopqrstuvwxyz')='description']")
  
  tibble(
    title = doc %>% html_elements('title') %>% html_text2() %>% clean_txt(),
    meta  = doc %>% html_elements(xpath = meta_xp) %>%
      html_attr('content') %>% clean_txt(),
    para  = doc %>% html_elements('p') %>% html_text2() %>% clean_txt()
  )
}

parse_data <- function(.df){
  out <- .df %>%
    filter(str_detect(text_tmp, '<!')) %>%
    rowwise() %>%
    mutate(fields = list(parse_fn(text_tmp))) %>%
    unnest(fields) %>%
    ungroup() %>%
    mutate(text_clean = str_squish(str_c(title, meta, para, sep = ' ')))
  return(out)
}

#tokenization 
# unigram for title, meta description; uni+bigram for paragraph text
stop_set <- stopwords::stopwords('en', source = 'smart') %>%
  str_remove_all('[[:punct:]]') %>%
  unique()

make_bigrams <- function(t){
  if (length(t) < 2) return(character(0))
  str_c(t[-length(t)], t[-1], sep = '_')
}

normalize_field <- function(x, prefix, bigram = FALSE){
  toks <- tokenizers::tokenize_words(x, lowercase = FALSE)
  
  # remove stopwords
  toks <- map(toks, ~ .x[!(.x %in% stop_set)])
  
  # lemmatize each unique word once, then look it up (much faster)
  u   <- unique(unlist(toks))
  lem <- setNames(textstem::lemmatize_words(u), u)
  
  map_chr(toks, function(t){
    t <- unname(lem[t])
    t <- t[!is.na(t) & str_length(t) > 2]
    if (bigram) t <- c(t, make_bigrams(t))   # unigrams + bigrams
    if (length(t) == 0) return('')
    str_c(prefix, t, sep = '_', collapse = ' ')
  })
}

# adds a `tokens` column:
#   title: unigrams | meta: unigrams | para: unigrams + bigrams
add_tokens <- function(df){
  df %>%
    mutate(tokens = str_squish(str_c(
      normalize_field(title, 'title'),
      normalize_field(meta,  'meta'),
      normalize_field(para,  'para', bigram = TRUE),
      sep = ' ')))
}