library(stringr)
library(tidyquant)
library(tm)
library(SnowballC)
library(broom)
library(pdftools)
library(tau)
library(wordcloud)
library(maxent)
library(RTextTools)

train <- read.csv("rcv1_test.csv", header = F)
test <- read.csv("rcv1_train.csv", header = F)

col_names = c("id", "text", "CAT", "h1","h2", "h3", "h4", "h5")
colnames(train) <- col_names
colnames(test) <- col_names

train_sub <- sample_n(train, 10000)
train_sub <- train_sub[which(train_sub$h1 != "NULL"),c(1,2,3)]
n = nrow(train_sub)

doc_matrix <- create_matrix(train_sub$text,
                            language="english",
                            removeNumbers=TRUE,
                            removeSparseTerms=.998)

t <- as.integer(0.8*n)
container <- create_container(doc_matrix,
                              as.numeric(factor(train_sub$CAT)),
                              trainSize=1:t,
                              testSize=t:n,
                              virgin=FALSE)

#Container created. Model 1 SVM:
SVM <- train_model(container,"SVM")
SVM_CLASSIFY <- classify_model(container, SVM)
SVM_ANALYTICS <- create_analytics(container,
                                  cbind(SVM_CLASSIFY))
SVM_NUMBER_INCORRECT <- sum(SVM_ANALYTICS@document_summary$PROBABILITY_INCORRECT)
SVM_TOTAL_NUMBER <- length(SVM_ANALYTICS@document_summary$PROBABILITY_INCORRECT)
SVM_ACCURACY <- (SVM_TOTAL_NUMBER-SVM_NUMBER_INCORRECT)/SVM_TOTAL_NUMBER

BOOST <- train_model(container,"BOOSTING")
BOOST_CLASSIFY <- classify_model(container, BOOST)
BOOST_ANALYTICS <- create_analytics(container,
                                    cbind(BOOST_CLASSIFY))
BOOST_NUMBER_INCORRECT <- sum(BOOST_ANALYTICS@document_summary$PROBABILITY_INCORRECT)
BOOST_TOTAL_NUMBER <- length(BOOST_ANALYTICS@document_summary$PROBABILITY_INCORRECT)
BOOST_ACCURACY <- (BOOST_TOTAL_NUMBER-BOOST_NUMBER_INCORRECT)/BOOST_TOTAL_NUMBER

GLM <- train_model(container,"GLMNET")
GLM_CLASSIFY <- classify_model(container, GLM)
GLM_ANALYTICS <- create_analytics(container,
                                  cbind(GLM_CLASSIFY))
GLM_NUMBER_INCORRECT <- sum(GLM_ANALYTICS@document_summary$PROBABILITY_INCORRECT)
GLM_TOTAL_NUMBER <- length(GLM_ANALYTICS@document_summary$PROBABILITY_INCORRECT)
GLM_ACCURACY <- (GLM_TOTAL_NUMBER-GLM_NUMBER_INCORRECT)/GLM_TOTAL_NUMBER

BAG <- train_model(container,"BAGGING")
BAG_CLASSIFY <- classify_model(container, BAG)
BAG_ANALYTICS <- create_analytics(container,
                                  cbind(BAG_CLASSIFY))
BAG_NUMBER_INCORRECT <- sum(BAG_ANALYTICS@document_summary$PROBABILITY_INCORRECT)
BAG_TOTAL_NUMBER <- length(BAG_ANALYTICS@document_summary$PROBABILITY_INCORRECT)
BAG_ACCURACY <- (BAG_TOTAL_NUMBER-BAG_NUMBER_INCORRECT)/BAG_TOTAL_NUMBER

TREE <- train_model(container,"TREE")
TREE_CLASSIFY <- classify_model(container, TREE)
TREE_ANALYTICS <- create_analytics(container,
                                   cbind(TREE_CLASSIFY))
TREE_NUMBER_INCORRECT <- sum(TREE_ANALYTICS@document_summary$PROBABILITY_INCORRECT)
TREE_TOTAL_NUMBER <- length(TREE_ANALYTICS@document_summary$PROBABILITY_INCORRECT)
TREE_ACCURACY <- (TREE_TOTAL_NUMBER-TREE_NUMBER_INCORRECT)/TREE_TOTAL_NUMBER

models_used <- c("SVM", "BOOSTING", "GLMNET", "BAGGING", "TREE")
accuracy <- c(SVM_ACCURACY, BOOST_ACCURACY, GLM_ACCURACY, BAG_ACCURACY, TREE_ACCURACY)
results <- cbind(models_used, accuracy)

SVM <- cross_validate(container, 4, "SVM")
GLM <- cross_validate(container, 4, "GLMNET")
BOOST <- cross_validate(container, 4, "BOOSTING")

#write.csv(SVM_ANALYTICS@document_summary, "SVM_Summary.csv")
#write.csv(BOOST_ANALYTICS@document_summary, "BOOST_Summary.csv")
#write.csv(GLM_ANALYTICS@document_summary, "GLM_Summary.csv")

#Types Available - Taken from Documentation
#SVM <- train_model(container,"SVM")
#GLMNET <- train_model(container,"GLMNET")
#MAXENT <- train_model(container,"MAXENT")
#SLDA <- train_model(container,"SLDA")
#BOOSTING <- train_model(container,"BOOSTING")
#BAGGING <- train_model(container,"BAGGING")
#RF <- train_model(container,"RF")
#NNET <- train_model(container,"NNET")
#TREE <- train_model(container,"TREE")

#Trying to Model with narrower (unequal level) topics
train_sub_h <- sample_n(train, 10000)
train_sub_h5 <- train_sub_h[which(train_sub_h$h5 != "NULL"),c(1,2,3)]
train_sub_h4 <- train_sub_h[which(train_sub_h$h4 != "NULL"),c(1,2,3)]
train_sub_h3 <- train_sub_h[which(train_sub_h$h3 != "NULL"),c(1,2,3)]
train_sub_h2 <- train_sub_h[which(train_sub_h$h2 != "NULL"),c(1,2,3)]
train_sub_h1 <- train_sub_h[which(train_sub_h$h1 != "NULL"),c(1,2,3)]

n1 = nrow(train_sub_h1)
n2 = nrow(train_sub_h2)
n3 = nrow(train_sub_h3)
n4 = nrow(train_sub_h4)
n5 = nrow(train_sub_h5)

#LEVEL 1 MODEL
doc_matrix1 <- create_matrix(train_sub_h1$text,
                             language="english",
                             removeNumbers=TRUE,
                             removeSparseTerms=.998)

t1 <- as.integer(0.8*n1)
container_higher1 <- create_container(doc_matrix1,
                                      as.numeric(factor(train_sub_h1$CAT)),
                                      trainSize=1:t1,
                                      testSize=t1:n1,
                                      virgin=FALSE)

SVM_H1 <- train_model(container_higher1,"SVM")
SVM_CLASSIFY_H1 <- classify_model(container_higher1, SVM_H1)
SVM_ANALYTICS_H1 <- create_analytics(container_higher1,
                                     cbind(SVM_CLASSIFY_H1))
#LEVEL 2 MODEL
doc_matrix2 <- create_matrix(train_sub_h2$text,
                             language="english",
                             removeNumbers=TRUE,
                             removeSparseTerms=.998)

t2 <- as.integer(0.8*n2)
container_higher2 <- create_container(doc_matrix2,
                                      as.numeric(factor(train_sub_h2$CAT)),
                                      trainSize=1:t2,
                                      testSize=t2:n2,
                                      virgin=FALSE)

SVM_H2 <- train_model(container_higher2,"SVM")
SVM_CLASSIFY_H2 <- classify_model(container_higher2, SVM_H2)
SVM_ANALYTICS_H2 <- create_analytics(container_higher2,
                                     cbind(SVM_CLASSIFY_H2))

#LEVEL 3 MODEL
doc_matrix3 <- create_matrix(train_sub_h3$text,
                             language="english",
                             removeNumbers=TRUE,
                             removeSparseTerms=.998)

t3 <- as.integer(0.8*n3)
container_higher3 <- create_container(doc_matrix3,
                                      as.numeric(factor(train_sub_h3$CAT)),
                                      trainSize=1:t3,
                                      testSize=t3:n3,
                                      virgin=FALSE)

SVM_H3 <- train_model(container_higher3,"SVM")
SVM_CLASSIFY_H3 <- classify_model(container_higher3, SVM_H3)
SVM_ANALYTICS_H3 <- create_analytics(container_higher3,
                                     cbind(SVM_CLASSIFY_H3))

#LEVEL 4 MODEL
doc_matrix4 <- create_matrix(train_sub_h4[,2],
                             language="english",
                             removeNumbers=TRUE,
                             removeSparseTerms=.998)

t4 <- as.integer(0.8*n4)
container_higher4 <- create_container(doc_matrix4,
                                      as.numeric(factor(train_sub_h4$CAT)),
                                      trainSize=1:t4,
                                      testSize=t4:n4,
                                      virgin=FALSE)

SVM_H4 <- train_model(container_higher4,"SVM")
SVM_CLASSIFY_H4 <- classify_model(container_higher4, SVM_H4)
SVM_ANALYTICS_H4 <- create_analytics(container_higher4,
                                     cbind(SVM_CLASSIFY_H4))

#LEVEL 5 MODEL IS USELESS AS THERE IS ONLY ONE CATEGORY IN H5

unique_codes <- unique(test$CAT)
rep101 <- seq(1, 101, 1)
numeric_matchings <- as.data.frame(cbind(as.character(unique_codes), as.character(rep101)))
unique_numeric <- unique(as.numeric(factor(test$CAT)))
numeric_matchings$V2 <- unique_numeric

test$CAT <- as.numeric(factor(test$CAT))
head(test$CAT)

test_sub <- sample_n(test, 5000)
article_ids <- unique(test_sub$id)

test_sub_h5 <- test_sub[which(test_sub$h5 != "NULL"),c(1,2,3)]
test_sub_h4 <- test_sub[which(test_sub$h4 != "NULL"),c(1,2,3)]
test_sub_h3 <- test_sub[which(test_sub$h3 != "NULL"),c(1,2,3)]
test_sub_h2 <- test_sub[which(test_sub$h2 != "NULL"),c(1,2,3)]
test_sub_h1 <- test_sub[which(test_sub$h1 != "NULL"),c(1,2,3)]

doc_matrix_test <- create_matrix(test_sub_h4[,2],
                                 language="english",
                                 removeNumbers=TRUE,
                                 removeSparseTerms=.998,
                                 originalMatrix = doc_matrix4)

testing_container <- create_container(doc_matrix_test,
                                      as.numeric(factor(test_sub_h4$CAT)),
                                      trainSize = 1:1,
                                      testSize=2:nrow(test_sub_h4),
                                      virgin=FALSE)

res <- classify_model(testing_container, SVM_H4)
res_id <- test_sub_h4[-1,]$id
res <- cbind(res, res_id)

#Isolating only the confident results
res_confident_h4 <- res[which(res$SVM_PROB > 0.8),]
colnames(res_confident_h4)[3] <- "id"

classifications <- test_sub[,1:3]
classifications$CAT <- as.numeric(factor(classifications$CAT))

df <- merge(x=classifications,y=res_confident_h4,by="id", all = T)



oos_analytics <- create_analytics(testing_container,
                                  cbind(resss))

#preditions <- predict(SVM_H4, as.compressed.matrix(doc_matrix_test))

tested <- test_sub_h4[-1,]
idx_confident <- which(oos_analytics@document_summary$SVM_PROB > 0.9)
confident_preds <- oos_analytics@document_summary$SVM_LABEL[idx_confident]

number <- length(oos_analytics@document_summary$SVM_PROB)
correct <- oos_analytics@document_summary[idx_confident,]
bad <- which(correct$SVM_LABEL != correct$MANUAL_CODE)


comp_data <- read.csv("articles_test.csv", header = F)
colnames(comp_data) <- c("id", "text")



train_sub <- sample_n(train, 30000)
train_sub <- train_sub[which(train_sub$h1 != "NULL"),c(1,2,3)]

comp_data <- cbind(comp_data, rep(0, nrow(comp_data)))
colnames(comp_data)[3] <- "CAT"
comp_data_train <- rbind(train_sub, comp_data)

doc_matrix_comp <- create_matrix(comp_data_train$text,
                                 language="english",
                                 removeNumbers=F,
                                 stemWords = T, 
                                 removeSparseTerms=.99)

container_comp <- create_container(doc_matrix_comp,
                              as.numeric(factor(comp_data_train$CAT)),
                              trainSize = 1:nrow(train_sub),
                             testSize=nrow(train_sub):nrow(comp_data_train),
                             virgin=TRUE)

SVM <- train_model(container_comp,"SVM")
SVM_CLASSIFY_COMP <- classify_model(container_comp, SVM)
SVM_CLASSIFY_COMP$SVM_LABEL

comp_cats <- read.csv("cat_test.csv", header = F)
comp_cats$V2 <- as.numeric(factor(comp_cats$V2))

results <- cbind(SVM_CLASSIFY_COMP$SVM_LABEL, comp_cats$V2)
colnames(results) <- c("Pred", "Actual")
results <- as.data.frame(results)
length(which(results$Pred == results$Actual)) / length(results$Pred)
