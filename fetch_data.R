library(legco)
library(legcoplus)
library(magrittr)

###-----Functions-----
## Function to split parameters into acceptable length
split_param <- function(param) {
  if (length(param) > 15) {
    n <- ceiling(length(param) / 15)
    tmp <- list()
    for (i in 1:n) {
      starting <- 15 * i - 14
      ending <- ifelse(15 * i < length(param), 15 * i, length(param))
      tmp[[i]] <- param[starting:ending]
    }
    param <- tmp
  }
  return(param)
}

## Function to extract members' speech in Council meetings with CE
extract_speech <- function(hansard_id) {
  # fetch full text of speeches
  hansard_id <- split_param(hansard_id)
  text <- data.frame()
  for (x in hansard_id) {
    n <- rundown(hansard_id = x, count = TRUE)
    text <- rbind(text, rundown(hansard_id = x, n = n))
  }
  text <- text[order(text$RundownID), ] %>%
    .[!is.na(.$SpeakerID), ]
  text$Content %<>% gsub("\\(.*\\)", "", .)
  text <- text[!text$SpeakerID %in% c(1, 6), ] # drop non-members' speech
  text$Content %<>% gsub("^.+議員：", "", .)
  # create a date-sorted list that contains member-sorted lists
  dates <- unique(text$MeetingDate) %>% .[order(.)]
  text <- lapply(dates, function(x) text %$% .[MeetingDate == x, ])
  for (i in 1:length(text)) {
    tmp <- list()
    for(j in unique(text[[i]]$SpeakerID)) {
      tmp[[as.character(j)]] <- text[[i]] %$% Content[SpeakerID == j]
    }
    text[[i]] <- tmp
  }
  names(text) <- dates
  return(text)
}

###-----CY Leung-----
w <- hansard(from = "2012-07-01", to = "2017-06-30", floor = TRUE)
cy_legco_text <- extract_speech(w$HansardID[w$isCEQT == 1 | w$isCEQandA == 1])
saveRDS(cy_legco_text, file = "cy_legco_text.rds")

###-----Carrie Lam-----
w <- hansard(from = "2017-07-01", floor = TRUE)
cl_legco_text <- extract_speech(w$HansardID[w$isCEQT == 1 | w$isCEQandA == 1])
saveRDS(cl_legco_text, file = "cl_legco_text.rds")

###-----Members List-----
members_name <- search_member()
saveRDS(members_name, file = "members_name.rds")
