library(dplyr)
library(jtools)

load("~/GitHub/ltta_workshop/hackathon_data/features/eurocss_media_data_sentiment.RData") # You need to change this
kmdf <- dt.data_sentiment[,22:121]

# Clustering, you don't have to run this again
# # ### check # of clusters
# min_k = 1
# max_k = 30
# wss_sum = numeric()
# for(i in min_k:max_k){
#   eff_kmeans = MiniBatchKmeans(kmdf
#                                , clusters = i
#                                , batch_size = 100
#                                , num_init = 10
#                                , initializer = 'kmeans++'
#                                , verbose = T
#   )
#   wss_sum[i] = sum(eff_kmeans$WCSS_per_cluster)
# }
# 

#save(wss_sum, file = 'wss_cluster_1_30.RData')

load('wss_cluster_1_30.RData')
plot(1:30,
     wss_sum,
     type='b',
     main = "Within Cluster Sum of Squares by Number of Clusters",
     ylab = "within cluster sum of squares",
     xlab = "number of clusters")

set.seed(42)

library(ClusterR)

#run with k=7
# k_means_model = MiniBatchKmeans(kmdf
#                                 , clusters = 7
#                                 , batch_size = 100
#                                 , num_init = 10
#                                 , initializer = 'kmeans++'
#                                 , verbose = T
# )
#save(k_means_model, file = 'k_means_model.RData')
load('k_means_model.RData')

k_means = predict_MBatchKMeans(kmdf
                               , k_means_model$centroids)
k_means <- as.character(k_means)

df.kmeans = data.frame(cbind(kmdf, k_means))
View(df.kmeans)
write.csv(df.kmeans, "dfkmeans.csv", row.names = FALSE)

# Generating df.data_sentiment_withcluster
df.data_sentiment_withcluster <- cbind(dt.data_sentiment, k_means)
df.data_sentiment_withcluster$k_means <- factor(df.data_sentiment_withcluster$k_means, labels=c("Rags to riches","Riches to rags","Down hill from here", "End on a high note", "Up hill from here", "End on a low note", "Mood swing"))
df.data_sentiment_withcluster$pol <- as.factor(df.data_sentiment_withcluster$pol)
df.data_sentiment_withcluster$channel_id <- as.factor(df.data_sentiment_withcluster$channel_id)

# Calculate normalised votes: upvotes / all votes
df.data_sentiment_withcluster$norm_votes <- 
  df.data_sentiment_withcluster$upvotes/
  (df.data_sentiment_withcluster$upvotes+df.data_sentiment_withcluster$downvotes)

df.data_sentiment_withcluster %>% 
  select(date_posted, days_until_reference)

# Calculate upvote ratio: upvotes / view_count_corrected
df.data_sentiment_withcluster$upvote_ratio <- 
  df.data_sentiment_withcluster$upvotes/
  df.data_sentiment_withcluster$view_count/
  df.data_sentiment_withcluster$days_until_reference

df.data_sentiment_withcluster$upvote_corrected <- 
  df.data_sentiment_withcluster$upvotes/
  df.data_sentiment_withcluster$days_until_reference

group_by(df.data_sentiment_withcluster, k_means) %>%
  summarise(count = n(),
            mean = mean(upvote_ratio, na.rm = TRUE),
            sd = sd(upvote_ratio, na.rm = TRUE))
mean(df.data_sentiment_withcluster$upvote_ratio, na.rm = TRUE)
nrow(df.data_sentiment_withcluster)
(descri <- group_by(df.data_sentiment_withcluster, k_means) %>%
  summarise("number of vlogs" = n(),
    "% of vlogs" = n()/nrow(df.data_sentiment_withcluster)*100,
            "Avg. viewcount" = mean(view_count_corrected, na.rm = TRUE),
            "Avg. upvotes" = mean(upvote_corrected, na.rm = TRUE),
            "Avg. Upvotes Ratio" = mean(upvote_ratio, na.rm =TRUE)))
colnames(descri)[1] <- "Cluster"
total <- c("Total",nrow(df.data_sentiment_withcluster),"100",mean(df.data_sentiment_withcluster$view_count_corrected, na.rm = TRUE),mean(df.data_sentiment_withcluster$upvote_corrected, na.rm = TRUE),mean(df.data_sentiment_withcluster$upvote_ratio, na.rm =TRUE))
View(descri)
descri <- rbind(descri, total)
write.csv(descri, file = "des.csv", row.names = FALSE)

(corpusdes <- group_by(df.data_sentiment_withcluster, channel_id) %>%
    summarise("number of vlogs" = n(),
              "Avg. wordcount" = mean(nwords, na.rm = TRUE),
              "Political Stance" = unique(pol)))

write.csv(corpusdes, file = "corpusdes.csv", row.names = FALSE)


lrtable <- table(df.data_sentiment_withcluster$k_means, df.data_sentiment_withcluster$pol)
write.csv(lrtable, file = "lrtable.csv", row.names = TRUE)


chisq <- chisq.test(df.data_sentiment_withcluster$k_means, df.data_sentiment_withcluster$pol, correct=FALSE)
round(chisq$residuals, 3)

if(!require(rcompanion)){install.packages("rcompanion")}
library(rcompanion)
pairwise <- pairwiseNominalIndependence(lrtable,
                            fisher = FALSE,
                            gtest  = FALSE,
                            chisq  = TRUE,
                            method = "fdr")
write.csv(pairwise, "pairwise.csv", row.names = FALSE)

# # Changing 6 to the reference category
# # df.data_sentiment_withcluster <- within(df.data_sentiment_withcluster, 
# #                                         k_means <- factor(k_means, levels = c( "6", "1", "2", "3", "4", "5", "7")))
# 
# df.data_sentiment_withcluster$k_means <- relevel(df.data_sentiment_withcluster$k_means, ref = "End on a low note")
# 
# 
# model <- lm(upvote_ratio ~ k_means + pol, data= df.data_sentiment_withcluster)
# model.r <- lm(upvote_ratio ~ k_means, data= df.data_sentiment_withcluster[df.data_sentiment_withcluster$pol == "r",])
# model.l <- lm(upvote_ratio ~ k_means, data= df.data_sentiment_withcluster[df.data_sentiment_withcluster$pol == "l",])
# 
# 
# names(model$coefficients) <- c("(Intercept)", 
#                                "Rags to riches","Riches to rags","Down hill from here",
#                                "End on a high note","Up hill from here",
#                                "Mood swing", "Right-wing")
# 
# names(model.r$coefficients) <- c("(Intercept)", 
#                                  "Rags to riches","Riches to rags","Down hill from here",
#                                  "End on a high note","Up hill from here",
#                                  "Mood swing")
# 
# names(model.l$coefficients) <- c("(Intercept)", 
#                                  "Rags to riches","Riches to rags","Down hill from here",
#                                  "End on a high note","Up hill from here",
#                                  "Mood swing")
# 
# summ(model)
# plot_summs(model)
# plot_summs(model.r, model.l, model.names = c("Right", "Left"))
# export_summs(model, model.r, model.l, to.file = "docx", file.name = "modellr.docx")

