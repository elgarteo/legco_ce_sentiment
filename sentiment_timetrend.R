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
  geom_line(aes(linetype = camp, color = camp)) +
  scale_color_manual(values=c("#999999", "#00C957", "#0000FF")) +
  scale_linetype_manual(values=c("twodash", "solid", "solid")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
        axis.title.x = element_blank()) +
  ylim(0, 1) +
  ggtitle("CY Leung") +
  geom_point()

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
  geom_line(aes(linetype = camp, color = camp)) +
  scale_color_manual(values=c("#999999", "#00C957", "#0000FF")) +
  scale_linetype_manual(values=c("twodash", "solid", "solid")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = c(.85, .9),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylim(0, 1) +
  ggtitle("Carrie Lam") +
  geom_point()

###-----Combine-----
png("timetrend.png", width = 20, height = 10, units = "in", res = 600)
grid.arrange(g_cy, g_cl, nrow = 1, bottom = "date",
             top = textGrob("Legislators' Sentiment in CE-attending Sessions\n",
                            gp = gpar(fontsize = 18, font = 2)))
dev.off()
