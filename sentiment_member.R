library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(magrittr)

source("preprocessing.R", encoding = "UTF-8")

###-----Function-----
## Function to extract legend to share between graphs
# from https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

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
cy_sent_pro_est <- cy_sent_all[, colnames(cy_sent_all) %in% pro_est$id]
cy_sent_non_pro_est <- cy_sent_all[, !colnames(cy_sent_all) %in% pro_est$id]
# construct data frame
cy_sent_all <- data.frame(name = rep(sapply(colnames(cy_sent_all),
                                            function(x) members_name %$% NameEng[SpeakerID == x]),
                                     each = 2),
                          prop = as.numeric(cy_sent_all),
                          sentiment = rep(c("negative", "positive"), ncol(cy_sent_all)))
cy_sent_pro_est <- data.frame(name = rep(sapply(colnames(cy_sent_pro_est),
                                                function(x) members_name %$% NameEng[SpeakerID == x]),
                                         each = 2),
                              prop = as.numeric(cy_sent_pro_est),
                              sentiment = rep(c("negative", "positive"), ncol(cy_sent_pro_est)))
cy_sent_non_pro_est <- data.frame(name = rep(sapply(colnames(cy_sent_non_pro_est),
                                                    function(x) members_name %$% NameEng[SpeakerID == x]),
                                             each = 2),
                                  prop = as.numeric(cy_sent_non_pro_est),
                                  sentiment = rep(c("negative", "positive"), ncol(cy_sent_non_pro_est))) 
# sort
cy_sent_all <- arrange(cy_sent_all, sentiment, prop)
cy_sent_all$name %<>% factor(., levels = unique(.))
cy_sent_pro_est <- arrange(cy_sent_pro_est, sentiment, prop)
cy_sent_pro_est$name %<>% factor(., levels = unique(.))
cy_sent_non_pro_est <- arrange(cy_sent_non_pro_est, sentiment, prop)
cy_sent_non_pro_est$name %<>% factor(., levels = unique(.))
# plot
colours <- ifelse(cy_sent_all$name %in% cy_sent_pro_est$name, "#D32F2F", "#1976D2")
g1 <- ggplot(cy_sent_all, aes(x = name, y = prop, fill = sentiment)) + 
  geom_bar(position = "fill", stat = "identity") + 
  ggtitle("All") +
  theme(legend.position = "bottom", axis.title.y = element_blank(),
        axis.text.y = element_text(color = colours)) +
  coord_flip()
g2 <- ggplot(cy_sent_pro_est, aes(x = name, y = prop, fill = sentiment)) + 
  geom_bar(position = "fill", stat = "identity") + 
  ggtitle("Pro-establishment") +
  theme(legend.position = "none", axis.title.y = element_blank(),
        axis.text.y = element_text(color = "#D32F2F")) +
  coord_flip()
g3 <- ggplot(cy_sent_non_pro_est, aes(x = name, y = prop, fill = sentiment)) + 
  geom_bar(position = "fill", stat = "identity") + 
  ggtitle("Non-pro-establishment") +
  theme(legend.position = "none", axis.title.y = element_blank(),
        axis.text.y = element_text(color = "#1976D2")) +
  coord_flip()
glegend <- g_legend(g1)

png("cy.png", width = 18, height = 12, units = "in", res = 600)
grid.arrange(glegend, arrangeGrob(g1 + theme(legend.position="none"), g2, g3, nrow = 1),
             top = textGrob("Legislators' Sentiment in CE-attending Sessions\n(CY Leung)",
                            gp = gpar(fontsize = 18, font = 2)), nrow = 2, heights = c(1, 10))
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
cl_sent_pro_est <- cl_sent_all[, colnames(cl_sent_all) %in% pro_est$id]
cl_sent_non_pro_est <- cl_sent_all[, !colnames(cl_sent_all) %in% pro_est$id]

cl_sent_all <- data.frame(name = rep(sapply(colnames(cl_sent_all),
                                            function(x) members_name %$% NameEng[SpeakerID == x]),
                                     each = 2),
                          prop = as.numeric(cl_sent_all),
                          sentiment = rep(c("negative", "positive"), ncol(cl_sent_all)))
cl_sent_pro_est <- data.frame(name = rep(sapply(colnames(cl_sent_pro_est),
                                                function(x) members_name %$% NameEng[SpeakerID == x]),
                                         each = 2),
                              prop = as.numeric(cl_sent_pro_est),
                              sentiment = rep(c("negative", "positive"), ncol(cl_sent_pro_est)))
cl_sent_non_pro_est <- data.frame(name = rep(sapply(colnames(cl_sent_non_pro_est),
                                                    function(x) members_name %$% NameEng[SpeakerID == x]),
                                             each = 2),
                                  prop = as.numeric(cl_sent_non_pro_est),
                                  sentiment = rep(c("negative", "positive"), ncol(cl_sent_non_pro_est))) 

cl_sent_all <- arrange(cl_sent_all, sentiment, prop)
cl_sent_all$name %<>% factor(., levels = unique(.))
cl_sent_pro_est <- arrange(cl_sent_pro_est, sentiment, prop)
cl_sent_pro_est$name %<>% factor(., levels = unique(.))
cl_sent_non_pro_est <- arrange(cl_sent_non_pro_est, sentiment, prop)
cl_sent_non_pro_est$name %<>% factor(., levels = unique(.))
# plot
colours <- ifelse(cl_sent_all$name %in% cl_sent_pro_est$name, "#D32F2F", "#1976D2")
g1 <- ggplot(cl_sent_all, aes(x = name, y = prop, fill = sentiment)) + 
  geom_bar(position = "fill", stat = "identity") + 
  ggtitle("All") +
  theme(legend.position = "top", axis.title.y = element_blank(),
        axis.text.y = element_text(color = colours)) +
  coord_flip()
g2 <- ggplot(cl_sent_pro_est, aes(x = name, y = prop, fill = sentiment)) + 
  geom_bar(position = "fill", stat = "identity") + 
  ggtitle("Pro-establishment") +
  theme(legend.position = "none", axis.title.y = element_blank(),
        axis.text.y = element_text(color = "#D32F2F")) +
  coord_flip()
g3 <- ggplot(cl_sent_non_pro_est, aes(x = name, y = prop, fill = sentiment)) + 
  geom_bar(position = "fill", stat = "identity") + 
  ggtitle("Non-pro-establishment") +
  theme(legend.position = "none", axis.title.y = element_blank(),
        axis.text.y = element_text(color = "#1976D2")) +
  coord_flip()
glegend <- g_legend(g1)

png("cl.png", width = 18, height = 10, units = "in", res = 600)
grid.arrange(glegend, arrangeGrob(g1 + theme(legend.position="none"), g2, g3, nrow = 1),
             top = textGrob("Legislators' Sentiment in CE-attending Sessions\n(Carrie Lam)",
                            gp = gpar(fontsize = 18, font = 2)), nrow = 2, heights = c(1, 10))
dev.off()
