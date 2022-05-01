library(gutenbergr)
library(ggplot2)
library(dplyr)
library(qdap)
library(tidytext)
library(SentimentAnalysis)
library(word2vec)
library(tictoc)

devtools::load_all()

dec_of_ind <- gutenberg_download(2, mirror = "http://mirrors.xmission.com/gutenberg/") 

dec_of_ind %>%
  tidytext::unnest_tokens(
    output = "word",
    input = "text",
    token = "regex"
  ) %>%
  dplyr::mutate(word_num = 1:nrow(.))

tic("first test")
obj <- get_context(dec_of_ind$word)
toc() 0.016

gutenberg_works(author == "Austen, Jane") %>% View()

emma <- gutenberg_download(158, mirror = "http://mirrors.xmission.com/gutenberg/") %>%
  tidytext::unnest_tokens(
    output = "word",
    input = "text"
  ) %>%
  dplyr::mutate(word_num = 1:nrow(.))

tic("emma test")
obj <- get_context(emma$word)
toc() # 4.156 seconds

## Hmm, I think we need to make this faster
# get_context <- function(words, window_width = 100) {
#   res <- character(length = length(words))
#   for (i in 1:length(words)) {
#     max_indx <- i + window_width
#     min_indx <- i - window_width
#     if (min_indx < 1) min_indx <- 1
#     if (max_indx > length(words)) max_indx <- length(words)
#     res[i] <- stringr::str_c(words[min_indx:max_indx], collapse = " ")
#   }
#   res
# }

## Let's try something with indices, because I just need the end points
tic("other method")
window_width <- 100
length_text <- nrow(emma)
mins <- c(rep(1, window_width), 1:(length_text - window_width))
maxes <- c(window_width:(length_text-1), rep(length_text, window_width))

test_obj <- purrr::map2_chr(
  .x = mins,
  .y = maxes,
  .f = ~{stringr::str_c(emma$word[.x:.y], collapse = " ")}
)
toc() #5.59 seconds (actually slower, oof)


stringr::str_split(dec_of_ind$text, " ") %>%
  purrr::flatten_chr() %>%
  `[`(. != "") %>%
  stringr::str_c(collapse = " ")

raw_text <- stringr::str_split(dec_of_ind$text, " ") 
breaks <- cumsum(purrr::map_int(raw_text, ~{ifelse(all(.x == ""), 0L, length(.x))})) %>%
  unique()
raw_text <- raw_text %>%
  purrr::flatten_chr() %>%
  `[`(. != "")
end_num <- nrow(rv$text_dat)
word_num <- s$x
win_l <- isolate(floor(input$smooth / 2))
min_indx <- 1 #ifelse(word_num - win_l < 1, 1, word_num - win_l)
max_indx <- 200 # ifelse(word_num + win_l > end_num, end_num, word_num + win_l)
sub_breaks <- breaks[breaks > min_indx & breaks < max_indx]
out_words <- c(raw_text[min_indx:max_indx], rep("<br>", length(sub_breaks)))
ids <- c(seq_along(raw_text[min_indx:max_indx]), sub_breaks + 0.5) 

out_words[order(ids)] %>%
  stringr::str_c(collapse = " ") %>%
  HTML() %>%
  p()



