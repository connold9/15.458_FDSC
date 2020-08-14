#Loading required packages
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

#Reading in training and test data (swapped due to size difference in database
train <- read.csv("rcv1_test.csv", header = F)
test <- read.csv("rcv1_train.csv", header = F)

col_names = c("id", "text", "CAT", "h1","h2", "h3", "h4", "h5")
colnames(train) <- col_names
colnames(test) <- col_names

train_sub <- sample_n(train, 28000)
train_sub <- train_sub[which(train_sub$h1 != "NULL"),c(1,2,3)]
n = nrow(train_sub)

#Used starttime and endtime to time the running of our models
start_time <- Sys.time()
doc_matrix <- create_matrix(train_sub$text,
                            language="english",
                            removeNumbers=TRUE,
                            removeSparseTerms=0.998,
                            weighting = weightTfIdf)

doc_matrix_boost <- create_matrix(train_sub$text,
                            language="english",
                            removeNumbers=TRUE,
                            removeSparseTerms=0.998)

t <- as.integer(0.7*n)
container <- create_container(doc_matrix,
                              as.numeric(factor(train_sub$CAT)),
                              trainSize=1:t,
                              testSize=(t+1):n,
                              virgin=FALSE)

container_boost <- create_container(doc_matrix_boost,
                              as.numeric(factor(train_sub$CAT)),
                              trainSize=1:t,
                              testSize=(t+1):n,
                              virgin=FALSE)

test_labels <- as.numeric(factor(train_sub[(t+1):n,]$CAT))

#Container created. Model construction for the ensemble:
SVM <- train_model(container,"SVM", cost = 1)
SVM_CLASSIFY <- classify_model(container, SVM)
SVM_ANALYTICS <- create_analytics(container,
                              cbind(SVM_CLASSIFY))

BOOST <- train_model(container_boost,"BOOSTING")
BOOST_CLASSIFY <- classify_model(container_boost, BOOST)
BOOST_ANALYTICS <- create_analytics(container_boost,
                              cbind(BOOST_CLASSIFY))

GLM <- train_model(container,"GLMNET", family = "multinomial", alpha = 1)
GLM_CLASSIFY <- classify_model(container, GLM)
GLM_ANALYTICS <- create_analytics(container,
                                   cbind(GLM_CLASSIFY))

BAG <- train_model(container,"BAGGING", method = "double", nbagg=10)
BAG_CLASSIFY <- classify_model(container, BAG)
BAG_ANALYTICS <- create_analytics(container,
                                  cbind(BAG_CLASSIFY))

#a function to return summary statistics for each model analytics
analyse_pred <- function(analytics) {
  precision <- mean(analytics@algorithm_summary[1:4,1])
  recall <- mean(analytics@algorithm_summary[1:4,2])
  fscore <- mean(analytics@algorithm_summary[1:4,3])
  return(c(precision, recall, fscore))
}

#Confusion matrices
bag_conf <- confusionMatrix(BAG_CLASSIFY$BAGGING_LABEL, factor(test_labels))
boost_conf <- confusionMatrix(BOOST_CLASSIFY$LOGITBOOST_LABEL, factor(test_labels))
glm_conf <- confusionMatrix(GLM_CLASSIFY$GLMNET_LABEL, factor(test_labels))
svm_conf <- confusionMatrix(SVM_CLASSIFY$SVM_LABEL, factor(test_labels))

accuracies <- c(glm_conf$overall[1], svm_conf$overall[1],boost_conf$overall[1],bag_conf$overall[1])

model_analysis <- rbind(analyse_pred(GLM_ANALYTICS), analyse_pred(SVM_ANALYTICS), 
                 analyse_pred(BOOST_ANALYTICS), analyse_pred(BAG_ANALYTICS))
model_analysis <- cbind(model_analysis, accuracies)
colnames(model_analysis) <- c("Precision", "Recall", "FScore", "Accuracy")
rownames(model_analysis) <- c("GLM", "SVM", "BOOSTING", "BAGGING")


##### FORMING AN ENSEMBLE FROM OUR MODELS #####

results_ensemble <- cbind(SVM_CLASSIFY$SVM_LABEL,
                          BAG_CLASSIFY$BAGGING_LABEL, 
                          GLM_CLASSIFY$GLMNET_LABEL,
                          BOOST_CLASSIFY$LOGITBOOST_LABEL)
results_ensemble <- as.data.frame(results_ensemble)
colnames(results_ensemble) <- c("SVM", "BAG", "GLM", "BOOST")

results_ensemble$GLM <- as.numeric(as.character(results_ensemble$GLM))
results_ensemble$SVM <- as.numeric(as.character(results_ensemble$SVM))
results_ensemble$BAG <- as.numeric(as.character(results_ensemble$BAG))
results_ensemble$BOOST <- as.numeric(as.character(results_ensemble$BOOST))

#Function to return mode of ensemble predictions
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

results_ensemble <- cbind(results_ensemble, apply(results_ensemble, 1, Mode))
colnames(results_ensemble)[5] <- "ENSEMBLE"

results_ensemble <- cbind(results_ensemble, test_labels)
colnames(results_ensemble)[6] <- "ACTUAL"

length(which(results_ensemble$ENSEMBLE == results_ensemble$ACTUAL)) / nrow(results_ensemble)

#Creating summary statistics
ensemble_conf <- confusionMatrix(factor(results_ensemble$ENSEMBLE), factor(results_ensemble$ACTUAL))
ensemble_sens <- mean(ensemble_conf$byClass[1:4,1])
ensemble_spec <- mean(ensemble_conf$byClass[1:4,2])
ensemble_prec <- mean(ensemble_conf$byClass[1:4,5])
ensemble_recall <- mean(ensemble_conf$byClass[1:4,6])
ensemble_bal <- mean(ensemble_conf$byClass[1:4,11])

end_time <- Sys.time()

######## UNEQUAL CLASSIFICATION - Q1b FROM HERE BELOW #######
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

#LEVEL 5 MODEL - Not possible with this formulation

test_sub <- sample_n(test, 5000)

test_sub_h5 <- test_sub[which(test_sub$h5 != "NULL"),c(1,2,3)]
test_sub_h4 <- test_sub[which(test_sub$h4 != "NULL"),c(1,2,3)]
test_sub_h3 <- test_sub[which(test_sub$h3 != "NULL"),c(1,2,3)]
test_sub_h2 <- test_sub[which(test_sub$h2 != "NULL"),c(1,2,3)]
test_sub_h1 <- test_sub[which(test_sub$h1 != "NULL"),c(1,2,3)]
######

### FOURTH LEVEL CLASSIFICATION

### trace("create_matrix", edit=T) and change A to a!
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

vec <- unique(as.character(test_sub_h4$CAT))
h4_matchings <- as.data.frame(cbind(vec, unique(as.numeric(factor(test_sub_h4$CAT)))))

res <- classify_model(testing_container, SVM_H4)
res_id <- test_sub_h4[-1,]$id
res <- cbind(res, res_id)

##ISSUE IS M143 - reclassify it (40 is being used as proxy for 39)
res$SVM_LABEL[which(res$SVM_LABEL == 40)] <- 39

res <- res %>%
  left_join(h4_matchings, by = c("SVM_LABEL" = "V2"))
res <- na.omit(res)

#Isolating only the confident results
res_confident_h4 <- res[which(res$SVM_PROB > 0.5),]
colnames(res_confident_h4)[3] <- "id"

classifications <- test_sub[,1:3]

df <- merge(x=classifications,y=res_confident_h4,by="id", all = T)

oos_analytics <- create_analytics(testing_container,
                                     cbind(resss))
######

#### THIRD LEVEL CLASSIFICATION
doc_matrix_test3 <- create_matrix(test_sub_h3[,2],
                                 language="english",
                                 removeNumbers=TRUE,
                                 removeSparseTerms=.998,
                                 originalMatrix = doc_matrix3)

testing_container3 <- create_container(doc_matrix_test3,
                                      as.numeric(factor(test_sub_h3$CAT)),
                                      trainSize = 1:1,
                                      testSize=2:nrow(test_sub_h3),
                                      virgin=FALSE)

vec3 <- unique(as.character(test_sub_h3$CAT))
h3_matchings <- as.data.frame(cbind(vec3, unique(as.numeric(factor(test_sub_h3$CAT)))))


res3 <- classify_model(testing_container3, SVM_H3)
res_id3 <- test_sub_h3[-1,]$id
res3 <- cbind(res3, res_id3)
res3$SVM_LABEL[which(res3$SVM_LABEL == 33)] <- 32

res3 <- res3 %>%
  left_join(h3_matchings, by = c("SVM_LABEL" = "V2"))
res3 <- na.omit(res3)

res_confident_h3 <- res3[which(res3$SVM_PROB > 0.5),]
colnames(res_confident_h3)[3] <- "id"

df_test <- merge(x=df,y=res_confident_h3,by="id", all = T)

na_idx <- which(is.na(df_test$vec))
df_test[,6] <- as.character(df_test[,6])

df_test[na_idx,4] <- df_test[na_idx,7]
df_test[na_idx,5] <- df_test[na_idx,8]
df_test[na_idx,6] <- as.character(df_test[na_idx,9])
#########


##### SECOND LEVEL CLASSIFICATION ####
doc_matrix_test2 <- create_matrix(test_sub_h2[,2],
                                  language="english",
                                  removeNumbers=TRUE,
                                  removeSparseTerms=.998,
                                  originalMatrix = doc_matrix2)

testing_container2 <- create_container(doc_matrix_test2,
                                       as.numeric(factor(test_sub_h2$CAT)),
                                       trainSize = 1:1,
                                       testSize=2:nrow(test_sub_h2),
                                       virgin=FALSE)

vec2 <- unique(as.character(test_sub_h2$CAT))
h2_matchings <- as.data.frame(cbind(vec2, unique(as.numeric(factor(test_sub_h2$CAT)))))


res2 <- classify_model(testing_container2, SVM_H2)
res_id2 <- test_sub_h2[-1,]$id
res2 <- cbind(res2, res_id2)

res2 <- res2 %>%
  left_join(h2_matchings, by = c("SVM_LABEL" = "V2"))
res2 <- na.omit(res2)

res_confident_h2 <- res2[which(res2$SVM_PROB > 0.5),]
colnames(res_confident_h2)[3] <- "id"

df_test <- df_test[,-c(7,8,9)]
df_test <- merge(x=df_test,y=res_confident_h2,by="id", all = T)

na_idx <- which(is.na(df_test$vec))
df_test[,6] <- as.character(df_test[,6])

df_test[na_idx,4] <- df_test[na_idx,7]
df_test[na_idx,5] <- df_test[na_idx,8]
df_test[na_idx,6] <- as.character(df_test[na_idx,9])
df_test <- df_test[,-c(7,8,9)]
#########

##### FIRST LEVEL CLASSIFICATION ####
doc_matrix_test1 <- create_matrix(test_sub_h1[,2],
                                  language="english",
                                  removeNumbers=TRUE,
                                  removeSparseTerms=.998,
                                  originalMatrix = doc_matrix1)

testing_container1 <- create_container(doc_matrix_test1,
                                       as.numeric(factor(test_sub_h1$CAT)),
                                       trainSize = 1:1,
                                       testSize=2:nrow(test_sub_h1),
                                       virgin=FALSE)

vec1 <- unique(as.character(test_sub_h1$CAT))
h1_matchings <- as.data.frame(cbind(vec1, unique(as.numeric(factor(test_sub_h1$CAT)))))


res1 <- classify_model(testing_container1, SVM_H1)
res_id1 <- test_sub_h1[-1,]$id
res1 <- cbind(res1, res_id1)

res1 <- res1 %>%
  left_join(h1_matchings, by = c("SVM_LABEL" = "V2"))
res1 <- na.omit(res1)

colnames(res1)[3] <- "id"

df_test <- merge(x=df_test,y=res1,by="id", all = T)

na_idx <- which(is.na(df_test$vec))
df_test[,6] <- as.character(df_test[,6])

df_test[na_idx,4] <- df_test[na_idx,7]
df_test[na_idx,5] <- df_test[na_idx,8]
df_test[na_idx,6] <- as.character(df_test[na_idx,9])
df_test <- df_test[,-c(7,8,9)]


#Now we have joined the accurate ones, go back and rejoin whats left. 
#Using just res for h4:

colnames(res)[3] <- "id"

df_test <- merge(x=df_test,y=res,by="id", all = T)

na_idx <- which(is.na(df_test$vec.x))
df_test[,6] <- as.character(df_test[,6])

df_test[na_idx,4] <- df_test[na_idx,7]
df_test[na_idx,5] <- df_test[na_idx,8]
df_test[na_idx,6] <- as.character(df_test[na_idx,9])
df_test <- df_test[,-c(7,8,9)]


colnames(res3)[3] <- "id"

df_test <- merge(x=df_test,y=res3,by="id", all = T)

na_idx <- which(is.na(df_test$vec.x))
df_test[,6] <- as.character(df_test[,6])

df_test[na_idx,4] <- df_test[na_idx,7]
df_test[na_idx,5] <- df_test[na_idx,8]
df_test[na_idx,6] <- as.character(df_test[na_idx,9])
df_test <- df_test[,-c(7,8,9)]

####


colnames(res2)[3] <- "id"

df_test <- merge(x=df_test,y=res2,by="id", all = T)

na_idx <- which(is.na(df_test$vec.x))
df_test[,6] <- as.character(df_test[,6])

df_test[na_idx,4] <- df_test[na_idx,7]
df_test[na_idx,5] <- df_test[na_idx,8]
df_test[na_idx,6] <- as.character(df_test[na_idx,9])
df_test <- df_test[,-c(7,8,9)]


colnames(res3)[3] <- "id"

df_test <- merge(x=df_test,y=res3,by="id", all = T)

na_idx <- which(is.na(df_test$vec.x))
df_test[,6] <- as.character(df_test[,6])

df_test[na_idx,4] <- df_test[na_idx,7]
df_test[na_idx,5] <- df_test[na_idx,8]
df_test[na_idx,6] <- as.character(df_test[na_idx,9])
df_test <- df_test[,-c(7,8,9)]

final_classifications <- df_test[!duplicated(df_test), ]

string_cat <- toString(final_classifications$CAT)
string_guess <- toString(final_classifications$vec.x)
(c(string_cat) == c(string_guess))

matches <- ifelse(as.character(final_classifications$CAT) == as.character(final_classifications$vec.x), 1, 0)
matches[which(is.na(matches))] <- 0

multi_accuracy <- sum(matches)/length(matches)

conf <- table(final_classifications$CAT, final_classifications$vec.x)
conf_matrix <- as.data.frame(conf)
conf_matrix_sparse <- conf_matrix[which(conf_matrix$Freq != 0), ]
colnames(conf_matrix_sparse) <- c("Actual", "Predicted", "Frequency")

comp_data <- read.csv("articles")

duplicated_idx <- duplicated(df_test$id)
lead_dup <- lead(duplicated_idx, 1)

dup <- duplicated_idx + lead_dup
dup[which(dup == 2)] <- 1
dup <- as.logical(dup)

dup_test <- df_test[dup, c(1,3,6)]

## An issued arised with articles that had more than one classification at different levels.
## Our algorithm predicted correctly almost all the time for at leasrt one level, and that
## and that prediction was added for every entry with that article id. 

correct = rep(0, length(unique_id))
for(i in 1:length(unique_id)) {
  id = unique_id[i]
  matches = df_test[which(df_test$id == id), c(1,3,6)]
  for(j in 1:nrow(matches)) {
    if(is.na(matches$vec.x)){
      break
    }
    indicator <- ifelse(as.character(matches$CAT) == as.character(matches$vec.x), 1, 0)
    if(indicator == 1) {
      correct[i] <- 1
    }
  }
}

finals <- as.data.frame(cbind(unique_id, correct))
