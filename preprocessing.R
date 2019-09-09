library(tm)
library(tmcn)
library(tidytext)
library(SnowballC)
library(jiebaR)
library(stringr)
library(magrittr)

###-----Functions-----
## Define Chinese Segmentation Engine
segmentCN <- worker()

## Functions to remove stopwords and seperate into terms
processing <- function(doc) {
  # split and clean text
  doc_en <- sapply(doc, function(x) if(!grepl("[^\001-\177]", x)) x) %>%
    unlist
  # English words
  if (length(doc_en)) {
    doc %<>% .[grepl("[^\001-\177]", doc)]
    doc_en <-  Corpus(VectorSource(doc_en)) %>% 
      tm_map(., content_transformer(tolower)) %>% 
      tm_map(., removePunctuation) %>% 
      tm_map(., removeNumbers) %>% 
      tm_map(., function(x) removeWords(x, stopwords_en))
    doc_en %<>% TermDocumentMatrix(Corpus(VectorSource(doc_en$content)))
    doc_en  <- doc_en[["dimnames"]][["Terms"]]
  }
  # Chinese words
  if (length(doc)) {
    doc %<>% segmentCN[.] %>%
      unlist %>% removeWords(., stopwords_zh) %>% removeNumbers
  }
  # sentiment analysis
  sentiment <- c(sentiment_term_zh %$% sentiment[word %in% doc],
                 sentiment_term_en %$% sentiment[word %in% doc_en])
  prop <- prop.table(table(sentiment))
  if (length(prop) == 1) {
    if (names(prop) == "negative") prop <- c(1, 0)
    else prop <- c(0, 1)
  } else if (length(prop) == 0) prop <- c(0, 0)
  prop %<>% as.numeric
  return(prop)
}

###-----Members list-----
members_name <- readRDS("members_name.rds")
pro_est <- rbind(readRDS("pro_est5.rds"), readRDS("pro_est6.rds")) %>%
  .[!duplicated(.), ]

###-----Sentiment terms-----
## Chinese
data(NTUSD)
sentiment_term_zh <- data.frame(word = c(NTUSD[[3]], NTUSD[[4]]),
                                sentiment = c(rep("positive", length(NTUSD[[3]])), 
                                              rep("negative", length(NTUSD[[4]]))), 
                                stringsAsFactors = FALSE)
stopwords_zh <- c(toTrad(stopwordsCN()), members_name$NameChi, "議員", "特首")

## English
sentiment_term_en <- get_sentiments("bing")
sentiment_term_en$word %<>% wordStem(., "english")
sentiment_term_en %<>% .[!duplicated(.$word), ]
stopwords_en <- c(stopwords("en"),
                  "chief executive", "president", "mr", "mrs", "ms", "dr",
                  tolower(unlist(strsplit(members_name$NameEng, "-| "))))
