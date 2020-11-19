#BIA652
#Final Project
#Tianrui Wang & Zi-Qi Liu

#library
library(tidyverse)
# library(igraph)
library(stringr)
library(VIM)
library(InformationValue)
library(varhandle)
#library(glmnet)
#library(arm)
#library(e1071)
library(randomForest)
library(cluster)


#Input data and delete unuse variables
data <- read.csv(file = "/Users/shelly/Downloads/ks-projects-201801_clean_delete.csv", header = T, sep = ",")
data$usd.pledged <-  NULL
data$id <- NULL
data$goal <- NULL
data$pledged <- NULL
data$deadline <- as.Date(data$deadline)
data$launched_date <- as.Date(data$launched)





#Create variables
days <- as.numeric(data$deadline-data$launched_date)
# data$days <- days

#maincat <- to.dummy(data$main_category, "main_category")
maincat <- model.matrix(~main_category, data)
nstate <- as.factor(ifelse(data$state=="successful", 1, 0))

# Crowd
crowd <- as.data.frame(cbind(data,days, maincat, nstate))

#tidy Variables names
names(crowd) <- str_replace_all(names(crowd), fixed(" "), "")
names(crowd) <- str_replace_all(names(crowd), '&', "")
names(crowd) <- str_replace_all(names(crowd), "'", "")
names(crowd) <- str_replace_all(names(crowd), "-", "")
names(crowd) <- str_replace_all(names(crowd), "__", "")

#Checking missing value
# aggr(crowd)

# scatter plot for all variables inside data?
# pairs(iris[,1:4], pch = 19)

# # outliers
# crowd_no_outlier <- crowd
# source("http://goo.gl/UUyEzD")
# outlierKD(crowd_no_outlier, backers)
# outlierKD(crowd_no_outlier, usd_goal_real)
# outlierKD(crowd_no_outlier, usd_pledged_real)
# outlierKD(crowd_no_outlier, days)
# write_csv(crowd_no_outlier, "/Users/shelly/Google Drive/BIA652/DATA/crowd_no_outlier.csv")
# 
# 

# capping the outliers
# 
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
#   H <- 1.5 * IQR(x, na.rm = T)
#   if((x < (qnt[1] - H)) | x > (qnt[2] + H)){
#     return(FALSE) 
#   } else { return(TRUE)}
#   
# 
# backers_cap <- crowd$backers
# backers_cap <- capping(crowd$backers)

# Standardization
standardize <- function(x){(x -mean(x))/sd(x)}
crowd$backers <- standardize(crowd$backers)
crowd$usd_pledged_real <- standardize(crowd$usd_pledged_real)
crowd$usd_goal_real <- standardize(crowd$usd_goal_real)
crowd$days <- standardize(crowd$days)


# create cluster dataset
clusternumeric <- as.data.frame(cbind(crowd$backers, crowd$usd_pledged_real, crowd$usd_goal_real, crowd$days))
names(clusternumeric) <- c("backers", "usd_pledged_real", "usd_goal_real","days")

clusterreal <- as.data.frame(cbind(data$backers, data$usd_goal_real, data$usd_pledged_real, days))
names(clusterreal) <- c("backers", "usd_pledged_real", "usd_goal_real","days")


# cluster:5means
set.seed(1)
clusters5 <- kmeans(clusternumeric, 5)
crowd$cluster5 <- as.factor(clusters5$cluster)
table(crowd$main_category, clusters5$cluster)
# table(crowd$nstate, clusters5$cluster, crowd_no_outlier$backers_outlier, crowd_no_outlier$pledged_outlier, crowd_no_outlier)
data_cluster <- cbind(data, days, clusters5$cluster)
write_csv(data_cluster, "/Users/shelly/Google Drive/BIA652/DATA/data_cluster.csv")
data$cluster5 <- as.factor(clusters5$cluster)
data$'log(pledged_goal)' <- NA
data$log_pledged_goal_ratio <- log(data$usd_pledged_real/data$usd_goal_real)
cluster1 <- filter(data, cluster5 == 1)
cluster2 <- filter(data, cluster5 == 2)
cluster3 <- filter(data, cluster5 == 3)
cluster4 <- filter(data, cluster5 == 4)
cluster5 <- filter(data, cluster5 == 5)
cluster_1_4 <- rbind(filter(data, cluster5 == 1), filter(data, cluster5 == 4))
cluster_2_3_5 <- rbind(cluster2, cluster3, cluster5)
ggplot(cluster_1_4, aes(x= log_pledged_goal_ratio)) + geom_density(aes(group=cluster5, colour=cluster5, fill=cluster5), alpha=0.3)
ggplot(cluster_2_3_5, aes(x= log_pledged_goal_ratio)) + geom_density(aes(group=cluster5, colour=cluster5, fill=cluster5), alpha=0.3)




# #plot cluster
# # 1: PDA, not very pretty
# clusplot(clusternumeric, clusters5$cluster, main = "2D Representation of the Cluster Solution", color = TRUE)
# clusplot(clusternorm, clustersnorm$cluster, main = "2D Representation of the Cluster Solution", color = TRUE)
# 
# 
# # 2: pair plots
# plot(clusterreal[c("backers", "usd_goal_real")], col = clusters5$cluster)
# points(clusters5$centers[,c("backers", "usd_goal_real")], col = 1:5, pch = 8, cex = 2)
# plot(clusters5$cluster, clusternumeric)



#Logistic regression
#lrdata <- subset(crowd, select = c(nstate, main_category.Art, main_category.Comics, main_category.Crafts, main_category.Fashion, main_category.Games, main_category.Photography, main_category.Theater, main_category.Dance, main_category.FilmVideo, main_category.Journalism, main_category.Publishing, main_category.Design, main_category.Food, main_category.Music, main_category.Technology, backers, usd_pledged_real, usd_goal_real, days))
crowd14 <- rbind(filter(crowd, cluster5 == 1), filter(crowd, cluster5 == 4))
crowd235 <- rbind(filter(crowd, cluster5 == 2), filter(crowd, cluster5 == 3), filter(crowd, cluster5 == 5))

lrdata <- subset(crowd, select = c(nstate, main_categoryComics, main_categoryCrafts, main_categoryFashion, main_categoryGames, main_categoryPhotography, main_categoryTheater, main_categoryDance, main_categoryFilmVideo, main_categoryJournalism, main_categoryPublishing, main_categoryDesign, main_categoryFood, main_categoryMusic, main_categoryTechnology, backers,usd_pledged_real, usd_goal_real, days))

lrdata14 <- subset(crowd14, select = c(nstate, main_categoryComics, main_categoryCrafts, main_categoryFashion, main_categoryGames, main_categoryPhotography, main_categoryTheater, main_categoryDance, main_categoryFilmVideo, main_categoryJournalism, main_categoryPublishing, main_categoryDesign, main_categoryFood, main_categoryMusic, main_categoryTechnology, backers,usd_pledged_real, usd_goal_real, days))
lrdata235 <- subset(crowd235, select = c(nstate, main_categoryComics, main_categoryCrafts, main_categoryFashion, main_categoryGames, main_categoryPhotography, main_categoryFilmVideo, main_categoryPublishing, main_categoryDesign, main_categoryFood, main_categoryMusic, main_categoryTechnology, backers,usd_pledged_real, usd_goal_real, days))
#lrdata <- subset(crowd, select = c(nstate, backers,usd_pledged_real, usd_goal_real, days))



#train&test data
set.seed(1)
train.index <- sample(x=1:nrow(lrdata), size=ceiling(0.05*nrow(crowd) ))
train <- lrdata[train.index, ]
test <- sample_n(lrdata[-train.index, ], size = length(train.index)/3 )
# all data 
train.index.75perc <- sample(x=1:nrow(lrdata), size=ceiling(0.75*nrow(lrdata) ))
train.75perc <- lrdata[train.index.75perc, ]
test.75perc <- lrdata[-train.index.75perc,]
# all data 
train.index.75perc <- sample(x=1:nrow(lrdata235), size=ceiling(0.75*nrow(lrdata235) ))
train.75perc <- lrdata235[train.index.75perc, ]
test.75perc <- lrdata235[-train.index.75perc,]

train.index.75perc <- sample(x=1:nrow(lrdata14), size=ceiling(0.75*nrow(lrdata14) ))
train.75perc <- lrdata14[train.index.75perc, ]
test.75perc <- lrdata14[-train.index.75perc,]
# LDA on 0.75 data
library(MASS)
lda_75perc <- lda(nstate ~ ., data = train.75perc)
pred.lda.2 <- predict(lda_75perc, test.75perc)
summary(lda_75perc)
lda_75perc$terms
table(test.75perc$nstate, pred.lda.2$class, dnn=c("Observed","Predicted")) ## $class 
mean(ifelse(test.75perc$nstate != pred.lda.2$class, 1, 0)) # misclassification rate
library(AUC)
pc <- predict(lda_75perc, na.roughfix(test.75perc))
pb <- pred.lda.2$posterior
pb <- as.data.frame(pb)
pred.LDA <- data.frame(test.75perc$nstate, pb$'1')
colnames(pred.LDA) <- c("target","score")
labels <- as.factor(ifelse(pred.LDA$target=="1", 1, 0))
predictions <- pred.LDA$score
auc(roc(predictions, labels), min = 0, max = 1)
plot(roc(predictions, labels), min=0, max=1, type="l", main="LDA - ROC Chart")


#model all glmnet
#x <- as.matrix(train[,-1])
#y <- as.factor(train$nstate)
#glm <- glmnet(x, y, family = "binomial")
#print(glm)
#coef(glm, s = c(0.1, 0.05, 0.01))

#xt <- as.matrix(test[,-1])
#glm_result <-predict(glm, newx = xt, s=0.01, x, type="response")


#model all glm
glm_glm <- glm(nstate ~ ., family = "binomial", data = lrdata)
summary(glm_glm)


#model glm gmaes
xmg <- subset(train, main_category.Games==1)
xmg <- xmg[,c(1,17,18,19,20)]
xmg_x <- as.matrix(xmg[,-1])
xmg_y <- as.factor(xmg$nstate)
glm_xmg <- glmnet(xmg_x, xmg_y, family = "binomial")
print(glm_xmg)
coef(glm_xmg, s = c(0.1, 0.05, 0.01))

#model glm cluster=5
xc5 <- subset(train, cluster5==5 | cluster5==4)
xc5 <- as.matrix(xc5[,-1])
glm_xc5 <- glm(nstate ~ ., family = "binomial", data=xc5)
summary(glm_xc5)

#model just use 4 variables (Error)
glm_4v <- glm(nstate ~ backers+usd_pledged_real+usd_goal_real+days, family = "binomial", data = train)

#svm
svm <- svm(nstate ~ backers+usd_pledged_real+usd_goal_real+days, data=train)

#Random forests all
set.seed(2019)
randomforestM <- randomForest(nstate ~ ., data = train, importane = T, proximity = T, do.trace = 100)
randomforestM
plot(randomforestM)
round(importance(randomforestM), 2)

result <- predict(randomforestM, newdata = test)
cm <- table(test$nstate, result, dnn = c("Real", "Predict"))
cm
accuracy <- sum(diag(cm)) / sum(cm)
accuracy

library(AUC)
pc <- predict(model.LDA, na.roughfix(test))
pb <- pred.lda.2$posterior
pb <- as.data.frame(pb)
pred.LDA <- data.frame(test.75perc$nstate, pb$'1')
colnames(pred.LDA) <- c("target","score")
labels <- as.factor(ifelse(pred.LDA$target=="1", 1, 0))
predictions <- pred.LDA$score
auc(roc(predictions, labels), min = 0, max = 1)
plot(roc(predictions, labels), min=0, max=1, type="l", main="LDA - ROC Chart")



#memory limit
memory.limit()
memory.size(F)
gc()


#word cloud
library(tm)
library(SnowballC)
library(wordcloud)
library(tidyverse)

sucdata <- subset(crowd, nstate=="1")
summary(sucdata$main_category)

#Music
sucdata_music <- subset(sucdata, main_category=="Music")
review_music <- Corpus(VectorSource(sucdata_music$name))
review_music <- tm_map(review_music, content_transformer(tolower))
review_music <- tm_map(review_music, removeNumbers)
review_music <- tm_map(review_music, removePunctuation)
review_music <- tm_map(review_music, removeWords, c("music", "duh", "whatever", stopwords("english")))
review_music <- tm_map(review_music, stripWhitespace)
review_music <- tm_map(review_music, stemDocument)

dtm_music <- DocumentTermMatrix(review_music, control = list(bounds = list(global = c(1, Inf))))
dtm_music
findFreqTerms(dtm_music, 1000)
freq <- data.frame(sort(colSums(as.matrix(dtm_music)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(10, "Dark2"), scale=c(2,0.5),random.order=F)

#Film & Video
sucdata_FV <- subset(sucdata, main_category=="Film & Video")
review_FV <- Corpus(VectorSource(sucdata_FV$name))
review_FV <- tm_map(review_FV, content_transformer(tolower))
review_FV <- tm_map(review_FV, removeNumbers)
review_FV <- tm_map(review_FV, removePunctuation)
review_FV <- tm_map(review_FV, removeWords, c("film", "duh", "whatever", stopwords("english")))
review_FV <- tm_map(review_FV, stripWhitespace)
review_FV <- tm_map(review_FV, stemDocument)

dtm_FV <- DocumentTermMatrix(review_FV, control = list(bounds = list(global = c(1, Inf))))
dtm_FV
findFreqTerms(dtm_FV, 1000)
freq <- data.frame(sort(colSums(as.matrix(dtm_FV)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(10, "Dark2"), scale=c(2,0.5),random.order=F)

#Games
sucdata_games <- subset(sucdata, main_category=="Games")
review_games <- Corpus(VectorSource(sucdata_games$name))
review_games <- tm_map(review_games, content_transformer(tolower))
review_games <- tm_map(review_games, removeNumbers)
review_games <- tm_map(review_games, removePunctuation)
review_games <- tm_map(review_games, removeWords, c("game", "duh", "whatever", stopwords("english")))
review_games <- tm_map(review_games, stripWhitespace)
review_games <- tm_map(review_games, stemDocument)

dtm_games <- DocumentTermMatrix(review_games, control = list(bounds = list(global = c(1, Inf))))
dtm_games
findFreqTerms(dtm_games, 1000)
freq <- data.frame(sort(colSums(as.matrix(dtm_games)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(10, "Dark2"), scale=c(2,0.5),random.order=F)

#Publishing
sucdata_pub <- subset(sucdata, main_category=="Publishing")
review_pub <- Corpus(VectorSource(sucdata_pub$name))
review_pub <- tm_map(review_pub, content_transformer(tolower))
review_pub <- tm_map(review_pub, removeNumbers)
review_pub <- tm_map(review_pub, removePunctuation)
review_pub <- tm_map(review_pub, removeWords, c("book", "duh", "whatever", stopwords("english")))
review_pub <- tm_map(review_pub, stripWhitespace)
review_pub <- tm_map(review_pub, stemDocument)

dtm_pub <- DocumentTermMatrix(review_pub, control = list(bounds = list(global = c(1, Inf))))
dtm_pub
findFreqTerms(dtm_pub, 1000)
freq <- data.frame(sort(colSums(as.matrix(dtm_pub)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(10, "Dark2"), scale=c(2,0.5),random.order=F)

#Art
sucdata_art <- subset(sucdata, main_category=="Art")
review_art <- Corpus(VectorSource(sucdata_art$name))
review_art <- tm_map(review_art, content_transformer(tolower))
review_art <- tm_map(review_art, removeNumbers)
review_art <- tm_map(review_art, removePunctuation)
review_art <- tm_map(review_art, removeWords, c("art", "duh", "whatever", stopwords("english")))
review_art <- tm_map(review_art, stripWhitespace)
review_art <- tm_map(review_art, stemDocument)

dtm_art <- DocumentTermMatrix(review_art, control = list(bounds = list(global = c(1, Inf))))
dtm_art
findFreqTerms(dtm_art, 1000)
freq <- data.frame(sort(colSums(as.matrix(dtm_art)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(10, "Dark2"), scale=c(2,0.5),random.order=F)

#Design
sucdata_des <- subset(sucdata, main_category=="Design")
review_des <- Corpus(VectorSource(sucdata_des$name))
review_des <- tm_map(review_des, content_transformer(tolower))
review_des <- tm_map(review_des, removeNumbers)
review_des <- tm_map(review_des, removePunctuation)
review_des <- tm_map(review_des, removeWords, c("design", "duh", "whatever", stopwords("english")))
review_des <- tm_map(review_des, stripWhitespace)
review_des <- tm_map(review_des, stemDocument)

dtm_des <- DocumentTermMatrix(review_des, control = list(bounds = list(global = c(1, Inf))))
dtm_des
findFreqTerms(dtm_des, 1000)
freq <- data.frame(sort(colSums(as.matrix(dtm_des)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(10, "Dark2"), scale=c(2,0.5),random.order=F)
