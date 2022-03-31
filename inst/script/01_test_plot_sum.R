library(gutenbergr)
library(ggplot2)
library(dplyr)
library(qdap)
library(tidytext)
library(SentimentAnalysis)
library(word2vec)

gutenberg_works(author == "Austen, Jane") %>% View()

gutenberg_works() %>%
  View()

dec_of_ind <- gutenberg_download(2, mirror = "http://mirrors.xmission.com/gutenberg/")

docs <- dec_of_ind$text
res <- analyzeSentiment(docs, rules=list("SentimentLM"=list(ruleSentiment, 
                                                            loadDictionaryLM())))
alice <- gutenberg_download(11, mirror = "http://mirrors.xmission.com/gutenberg/")

docs <- alice$text
res2 <- analyzeSentiment(
  docs, 
  rules=list(
    "SentimentQDAP"=list(
      ruleSentiment, 
      loadDictionaryQDAP()
    ),
    "NegativityQDAP" = list(
      ruleNegativity,
      loadDictionaryQDAP()
    ),
    "PositivityQDAP" = list(
      rulePositivity,
      loadDictionaryQDAP()
    )
  )
)
sentiment_score <- res2$SentimentQDAP[!is.nan(res2$SentimentQDAP)]
positivity_score <- res2$PositivityQDAP[!is.nan(res2$PositivityQDAP)]
negativity_score <- res2$NegativityQDAP[!is.nan(res2$NegativityQDAP)]

plot(zoo::rollmean(sentiment_score, k = 100), type = "l", ylim = c(-0.12, 0.15))
lines(zoo::rollmean(positivity_score, k = 100), type = "l", col = "green")
lines(zoo::rollmean(-negativity_score, k = 100), type = "l", col = "red")


form_test <- formality(docs, order.by.formality = FALSE)


model <- read.word2vec("data/models/word2vec_1/model.bin", normalize = TRUE)

predict(model, newdata = c("fries", "money"), type = "nearest", top_n = 15)

wv <- predict(model, newdata = c("king", "man", "woman"), type = "embedding") 
wv <- wv["king", ]- wv["man", ] + wv["woman", ] 
predict(model, newdata = wv, type = "nearest", top_n = 5)


wv <- predict(model, newdata = c("paris", "germany", "berlin"), type = "embedding") 
wv <- wv["paris", ]- wv["berlin", ] + wv["germany", ] 
predict(model, newdata = wv, type = "nearest", top_n = 5)

wv <- predict(model, newdata = c("good", "worst", "bad"), type = "embedding") 
wv <- wv["bad", ] - wv["worst", ] 
predict(model, newdata = wv, type = "nearest", top_n = 5)


tokenized_words <- data.frame(line = 1:length(docs), text = docs) %>%
  filter(text != "") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(
    word = tolower(word),
    word = stringr::str_remove_all(word, "'s"),
    word = stringr::str_extract(word, "[a-z]+" )
  ) %>%
  filter(!is.na(word))

preds_romance <- predict(model, newdata = "romance", type = "nearest", top_n = 20000)$romance %>%
  filter(!stringr::str_detect(term2, "::")) %>%
  mutate(similarity = (similarity - min(similarity)) / (max(similarity) - min(similarity)))
joined <- left_join(tokenized_words, preds_romance %>% select(term2, similarity), by = c("word" = "term2")) %>%
  mutate(
    similarity = tidyr::replace_na(similarity, 0),
    roll_similarity = zoo::rollmean(similarity, k = 300, fill = NA)
  )

joined %>%
  ggplot() +
  aes(x = 1:nrow(joined), y = roll_similarity) +
  geom_line()

docs[2280:2330]



preds_confused <- predict(model, newdata = "confused", type = "nearest", top_n = 20000)$confused %>%
  filter(!stringr::str_detect(term2, "::")) %>%
  mutate(similarity = (similarity - min(similarity)) / (max(similarity) - min(similarity)))
joined <- left_join(tokenized_words, preds_confused %>% select(term2, similarity), by = c("word" = "term2")) %>%
  mutate(
    similarity = tidyr::replace_na(similarity, 0),
    roll_similarity = zoo::rollmean(similarity, k = 1000, fill = NA)
  )

joined %>%
  ggplot() +
  aes(x = 1:nrow(joined), y = roll_similarity) +
  geom_line()



