install.packages("tidyverse")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("ClusterR")
install.packages("cluster")
install.packages("ggthemes")
install.packages("moments")
install.packages("mice")
install.packages("crayon")
install.packages("devtools")

library(tidyverse)
library(ggplot2)
library(reshape2)
library(ClusterR)
library(cluster)
library(ggthemes)
library(moments)
library(mice)
library(crayon)
library(devtools)

ipeds$Total_Undergrad
ipeds$LogTotal_Undergrad <- log(ipeds$Total_Undergrad)
test <- ipeds$Total_Undergrad
test[ipeds$Total_Undergrad > 100000] <- NA
max(test, na.rm=T)
ipeds$Minus3Total_Undergrad <- test








popdata <- ipeds %>% select(UNITID, PCT_Female, TitleIV_Status)
sample <- popdata[(1:100),]



sample <- data.frame(sample, in_sample = rep(1, dim(sample)[1]))

fulldata <- popdata %>%
  full_join(sample) %>%
  mutate(in_sample = replace(in_sample, is.na(in_sample), 0))

pr_model <- glm(fulldata$in_sample ~ as.matrix(fulldata[,2]) + as.matrix(fulldata[,3]), family = binomial())

fulldata <- fulldata %>%
  mutate(prop_score = predict(pr_model, type = "response"))

dat1b <- fulldata %>%
  filter(in_sample == 1) %>%
  select(prop_score)
dat1b <- as.numeric(unlist(dat1b))

dat2b <- fulldata %>%
  filter(in_sample == 0) %>%
  select(prop_score)
dat2b <- as.numeric(unlist(dat2b))

gen_ind <- Bindex(dat1b, dat2b)






cluster1 <- read.csv("recruitment_list_for_1.csv", header=T)$UNITID[(1:25)]
length(cluster1); head(cluster1)
cluster2 <- read.csv("recruitment_list_for_2.csv", header=T)$UNITID[(1:8)]
# cluster2 <- read.csv("recruitment_list_for_2.csv", header=T)$UNITID[(1:100)]
cluster3 <- read.csv("recruitment_list_for_3.csv", header=T)$UNITID[(1:32)]
cluster4 <- read.csv("recruitment_list_for_4.csv", header=T)$UNITID[(1:35)]
sampleids <- cbind(c(cluster1, cluster2, cluster3, cluster4))
sampleids <- cbind(c(cluster2))
sample <- sampleids
colnames(sample) <- "UNITID"

## If you pull, say, all 100 schools from Cluster 2 (when it only asked for 8),
## you get a generalizer index value of 0.25!
## However -- if you actually pull the perfect number of schools from each cluster,
## it goes to 0. Need to check the Tipton paper.


## Other note to self. The B Index is the "histogram distance." So we should
## be able (ha! get it) to visually confirm the similarity or differences of
## different samples vs. the population. Could be worthwhile looking at this,
## at least to confirm that it's behaving reasonably.
