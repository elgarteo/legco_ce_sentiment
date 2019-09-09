library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(magrittr)

source("preprocessing.R", encoding = "UTF-8")

###-----CY Leung-----
cy_legco_text <- readRDS("cy_legco_text.rds")

cy_sent_meeting_all <- sapply(cy_legco_text, function(x) sapply(x, function(y) processing(y)))
cy_sent_meeting_pro_est <-  sapply(cy_sent_meeting_all, function(x) x[, colnames(x) %in% pro_est$id])
cy_sent_meeting_non_pro_est <-  sapply(cy_sent_meeting_all, function(x) x[, !colnames(x) %in% pro_est$id])

cy_sent_meeting_all %<>% sapply(., function(x) apply(x, 1, mean))
cy_sent_meeting_pro_est %<>% sapply(., function(x) apply(x, 1, mean))
cy_sent_meeting_non_pro_est %<>% sapply(., function(x) apply(x, 1, mean))

cy_sent_meeting <- data.frame(date = rep(colnames(cy_sent_meeting_all), 3) %>%
                                gsub("T.*", "", .),
                              positive = cbind(cy_sent_meeting_all,
                                               cy_sent_meeting_pro_est,
                                               cy_sent_meeting_non_pro_est)[2, ],
                              camp = rep(c("all", "pro-establishment", "non-pro-establishment"),
                                         each = ncol(cy_sent_meeting_all)))
# remove meetings with only one camp attending
cy_sent_meeting <- cy_sent_meeting %$% .[!date == date[is.nan(positive)], ] 

# plot
g_cy <- ggplot(cy_sent_meeting, aes(x = date, y = positive, group = camp)) +
  geom_line(aes(linetype = camp, color = camp), size = 1) +
  scale_color_manual(values=c("#999999", "#00C957", "#0000FF")) +
  scale_linetype_manual(values=c("twodash", "solid", "solid")) +
  theme(axis.text.y= element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.position = "none", axis.title.x = element_blank(), plot.title = element_text(size = 18)) +
  ylim(0, 1) +
  geom_point()

g_cy_en <- g_cy + ylab("Proportion") + ggtitle("CY Leung")

g_cy_zh <- g_cy + ylab("比例") + ggtitle("梁振英") + theme(text = element_text(family = "Heiti TC Light"))

###-----Carrie Lam-----
cl_legco_text <- readRDS("cl_legco_text.rds")

cl_sent_meeting_all <- sapply(cl_legco_text, function(x) sapply(x, function(y) processing(y)))
cl_sent_meeting_pro_est <-  sapply(cl_sent_meeting_all, function(x) x[, colnames(x) %in% pro_est$id])
cl_sent_meeting_non_pro_est <-  sapply(cl_sent_meeting_all, function(x) x[, !colnames(x) %in% pro_est$id])

cl_sent_meeting_all %<>% sapply(., function(x) apply(x, 1, mean))
cl_sent_meeting_pro_est %<>% sapply(., function(x) apply(x, 1, mean))
cl_sent_meeting_non_pro_est %<>% sapply(., function(x) apply(x, 1, mean))

cl_sent_meeting <- data.frame(date = rep(colnames(cl_sent_meeting_all), 3) %>%
                                gsub("T.*", "", .),
                              positive = cbind(cl_sent_meeting_all,
                                               cl_sent_meeting_pro_est,
                                               cl_sent_meeting_non_pro_est)[2, ],
                              camp = rep(c("all", "pro-establishment", "non-pro-establishment"),
                                         each = ncol(cl_sent_meeting_all)))
# remove meetings with only one camp attending
cl_sent_meeting <- cl_sent_meeting %$% .[!date == date[is.nan(positive)], ] 

# plot
g_cl <- ggplot(cl_sent_meeting, aes(x = date, y = positive, group = camp)) +
  geom_line(aes(linetype = camp, color = camp), size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), legend.position = c(.8, .8),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.text = element_text(size = 14), legend.title = element_text(size = 14),
        plot.title = element_text(size = 18)) +
  ylim(0, 1) +
  geom_point()

g_cl_en <- g_cl + ggtitle("Carrie Lam") + 
  scale_linetype_manual(values = c("twodash", "solid", "solid")) + 
  scale_color_manual(values = c("#999999", "#00C957", "#0000FF"))

g_cl_zh <- g_cl + ggtitle("林鄭月娥") +
  scale_linetype_manual(values = c("twodash", "solid", "solid"),
                        labels = c("全部", "建制派", "非建制派"),
                        name = "政治陣營") +
  scale_color_manual(values = c("#999999", "#00C957", "#0000FF"),
                     labels = c("全部", "建制派", "非建制派"),
                     name = "政治陣營") +
  theme(text = element_text(family = "Heiti TC Light"))

###-----Combine-----
# English
png("timetrend_en.png", width = 1500, height = 750, units = "px")
grid.arrange(g_cy_en, g_cl_en, nrow = 1, bottom = "Date",
             top = textGrob("Legislators' Postive Sentiment in CE-attended Sessions\n",
                            gp = gpar(fontsize = 24, font = 2)))
dev.off()

# Chinese
png("timetrend_zh.png", width = 1500, height = 750, units = "px")
grid.arrange(g_cy_zh, g_cl_zh, nrow = 1, 
             bottom = textGrob("日期", gp = gpar(fontfamily = "Heiti TC Light")),
             top = textGrob("議員於行政長官出席立法會會議時表達之正面情感\n",
                            gp = gpar(fontsize = 24, font = 2, fontfamily = "Heiti TC Light")))
dev.off()
