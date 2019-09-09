library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(magrittr)

source("preprocessing.R", encoding = "UTF-8")

###-----CY Leung-----
cy_legco_text <- readRDS("cy_legco_text.rds")

cy_all <- unlist(cy_legco_text, recursive = FALSE) # expand list to remove date categorisation
names(cy_all) %<>% gsub(".*\\.", "", .)
# merge all speech made by a member
cy_all[unique(names(cy_all)[duplicated(names(cy_all))])] <- 
  sapply(unique(names(cy_all)[duplicated(names(cy_all))]),
         function(x) unlist(cy_all[names(cy_all) == x]))
cy_all %<>% .[!duplicated(names(.))]
# generate prop tables for each members
cy_sent_all <- sapply(cy_all, function(x) processing(x))
# pro-establishment lawmakers name list
cy_sent_pro_est <- colnames(cy_sent_all)[colnames(cy_sent_all) %in% pro_est$id] %>%
  sapply(., function(x) members_name %$% NameEng[SpeakerID == x])
# construct data frame
cy_sent_all <- data.frame(name = rep(sapply(colnames(cy_sent_all),
                                            function(x) members_name %$% NameEng[SpeakerID == x]),
                                     each = 2),
                          name_zh = rep(sapply(colnames(cy_sent_all),
                                            function(x) members_name %$% NameChi[SpeakerID == x]),
                                     each = 2),
                          prop = as.numeric(cy_sent_all),
                          sentiment = rep(c("negative", "positive"), ncol(cy_sent_all)),
                          sentiment_zh = rep(c("負面", "正面"), ncol(cy_sent_all)))

# sort English
cy_sent_all <- arrange(cy_sent_all, sentiment, prop)
cy_sent_all$name %<>% factor(., levels = unique(.))

# plot English
colours <- ifelse(cy_sent_all$name %in% cy_sent_pro_est, "#D32F2F", "#1976D2")

png("cy_en.png", width = 1200, height = 800, units = "px")
ggplot(cy_sent_all, aes(x = name, y = prop, fill = sentiment)) + 
  geom_bar(position = "fill", stat = "identity") + ylab("Proportion") +
  ggtitle("Legislators' Sentiment in CE-attended Sessions (CY Leung)") +
  theme(legend.position = "top", axis.title.x = element_blank(),
        axis.text.x = element_text(color = colours, angle = 45, hjust = 1),
        plot.title = element_text(hjust = .5, size = 18),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))
dev.off()

# sort Chinese
cy_sent_all$sentiment_zh %<>% factor(., levels = c("負面", "正面"))
cy_sent_all <- arrange(cy_sent_all, sentiment_zh, prop)
cy_sent_all$name_zh %<>% factor(., levels = unique(.))

# plot Chinese
png("cy_zh.png", width = 1200, height = 800, units = "px")
ggplot(cy_sent_all, aes(x = name_zh, y = prop, fill = sentiment_zh)) + 
  geom_bar(position = "fill", stat = "identity") + ylab("比例") + labs(fill = "情感") +
  ggtitle("議員於行政長官出席立法會會議時表達之情感（梁振英）") +
  theme(text = element_text(family = "Heiti TC Light"), 
        legend.position = "top", axis.title.x = element_blank(),
        axis.text.x = element_text(color = colours, angle = 45, hjust = 1),
        plot.title = element_text(hjust = .5, size = 18),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))
dev.off()

###-----Carrie Lam-----
cl_legco_text <- readRDS("cl_legco_text.rds")

cl_all <- unlist(cl_legco_text, recursive = FALSE)
names(cl_all) %<>% gsub(".*\\.", "", .)

cl_all[unique(names(cl_all)[duplicated(names(cl_all))])] <- # merge duplicated rows
  sapply(unique(names(cl_all)[duplicated(names(cl_all))]),
         function(x) unlist(cl_all[names(cl_all) == x]))
cl_all %<>% .[!duplicated(names(.))]

cl_sent_all <- sapply(cl_all, function(x) processing(x))

cl_sent_pro_est <- colnames(cl_sent_all)[colnames(cl_sent_all) %in% pro_est$id] %>%
  sapply(., function(x) members_name %$% NameEng[SpeakerID == x])

cl_sent_all <- data.frame(name = rep(sapply(colnames(cl_sent_all),
                                            function(x) members_name %$% NameEng[SpeakerID == x]),
                                     each = 2),
                          name_zh = rep(sapply(colnames(cl_sent_all),
                                               function(x) members_name %$% NameChi[SpeakerID == x]),
                                        each = 2),
                          prop = as.numeric(cl_sent_all),
                          sentiment = rep(c("negative", "positive"), ncol(cl_sent_all)),
                          sentiment_zh = rep(c("負面", "正面"), ncol(cl_sent_all)))

# sort English
cl_sent_all <- arrange(cl_sent_all, sentiment, prop)
cl_sent_all$name %<>% factor(., levels = unique(.))

# plot English
colours <- ifelse(cl_sent_all$name %in% cl_sent_pro_est, "#D32F2F", "#1976D2")

png("cl_en.png", width = 1200, height = 800, units = "px")
ggplot(cl_sent_all, aes(x = name, y = prop, fill = sentiment)) + 
  geom_bar(position = "fill", stat = "identity") + ylab("Proportion") +
  ggtitle("Legislators' Sentiment in CE-attended Sessions (Carrie Lam)") +
  theme(legend.position = "top", axis.title.x = element_blank(),
        axis.text.x = element_text(color = colours, angle = 45, hjust = 1),
        plot.title = element_text(hjust = .5, size = 18),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))
dev.off()

# sort Chinese
cl_sent_all$sentiment_zh %<>% factor(., levels = c("負面", "正面"))
cl_sent_all <- arrange(cl_sent_all, sentiment_zh, prop)
cl_sent_all$name_zh %<>% factor(., levels = unique(.))

# plot Chinese
png("cl_zh.png", width = 1200, height = 800, units = "px")
ggplot(cl_sent_all, aes(x = name_zh, y = prop, fill = sentiment_zh)) + 
  geom_bar(position = "fill", stat = "identity") + ylab("比例") + labs(fill = "情感") +
  ggtitle("議員於行政長官出席立法會會議時表達之情感（林鄭月娥）") +
  theme(text = element_text(family = "Heiti TC Light"), 
        legend.position = "top", axis.title.x = element_blank(),
        axis.text.x = element_text(color = colours, angle = 45, hjust = 1),
        plot.title = element_text(hjust = .5, size = 18),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))
dev.off()
