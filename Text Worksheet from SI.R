## RegEx

library(stringr)

str_extract_all('Email info@sesync.org or twee @SESYNC', '\\b\\S+@\\S+\\b')

library(tm)

enron <- VCorpus(DirSource("data/enron"))
email <- enron[[1]]

match <- str_match(content(email), '^From: (.*)')
head(match)


## Data Extraction

enron <- tm_map(enron, function(email) {
  body <- content(email)
  match <- str_match(body, '^From: (.*)')
  match <- na.omit(match)
  meta(email, 'author') <- match[[1, 2]]
  return(email)
})

email <- enron[[2]]

head(content(email))

## Relational Data Exrtraction

get_to <- function(email) {
  body <- content(email)
  match <- str_detect(body, '^To:')
  if (any(match)) {
    to_start <- which(match)[[1]]
    match <- str_detect(body, '^Subject:')
    to_end <- which(match)[[1]] - 1
    to <- paste(body[to_start:to_end], collapse = '')
    to <- str_extract_all(to, '\\b\\S+@\\S+\\b')
    return(unlist(to))
  } else {
    return(NA)
  }
}

get_to(email)

edges <- lapply(enron, FUN = function(email) {
  from <- meta(email, 'author')
  to <- get_to(email)
  return(cbind(from, to))
})
edges <- do.call(rbind, edges)
edges <- na.omit(edges)
attr(edges, 'na.action') <- NULL

library(network)

g <- network(edges)
plot(g)

## Text Mining

enron <- tm_map(enron, function(email) {
  body <- content(email)
  match <- str_detect(body, '^X-FileName:')
  begin <- which(match)[[1]] + 1
  match <- str_detect(body, '^[>\\s]*[_\\-]{2}')
  match <- c(match, TRUE)
  end <- which(match)[[1]] - 1
  content(email) <- body[begin:end]
  return(email)
})

## Cleaning Text

library(magrittr)

enron_words <- enron %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)

remove_link <- function(body) {
  match <- str_detect(body, '(http|www|mailto)')
  body[!match]
}

enron_words <- enron_words %>%
  tm_map(content_transformer(remove_link))

## Stopwords and Stems

enron_words <- enron_words %>%
  tm_map(stemDocument) %>%
  tm_map(removeWords, stopwords("english"))

## Bag-of-Words

dtm <- DocumentTermMatrix(enron_words)

dtm

## Long Form

library(tidytext)
library(dplyr)
dtt <- tidy(dtm)
words <- dtt %>%
  group_by(term) %>%
  summarise(
    n = n(),
    total = sum(count)) %>%
  mutate(nchar = nchar(term))

library(ggplot2)
ggplot(words, aes(x=nchar)) +
  geom_histogram(binwidth = 1)

dtt_trimmed <- words %>%
  filter(
    nchar < 16,
    n > 1,
    total > 3) %>%
  select(term) %>%
  inner_join(dtt)

dtm_trimmed <- dtt_trimmed %>%
  cast_dtm(document, term, count)

## Term Correlations

word_assoc <- findAssocs(dtm_trimmed, 'ken', 0.6)
word_assoc <- data.frame(
  word = names(word_assoc[[1]]),
  assoc = word_assoc,
  row.names = NULL)

View(word_assoc)

## Latent Dirichlet allocation

library(topicmodels)

seed = 12345
fit = LDA(dtm_trimmed, k = 5, control = list(seed=seed))
email_topics <- as.data.frame(
  posterior(fit, dtm_trimmed)$topics)

head(email_topics)

library(ggwordcloud)

topics <- tidy(fit) %>%
  filter(beta > 0.004)

ggplot(topics,
       aes(size = beta, label = term)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  facet_wrap(vars(topic))
