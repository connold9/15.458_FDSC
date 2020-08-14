train <- read.csv("rcv1_test.csv", header = F)

col_names = c("id", "text", "CAT", "h1","h2", "h3", "h4", "h5")
colnames(train) <- col_names

##Choose a Sample to create a container
train_sub <- sample_n(train, 11000)
train_sub <- train_sub[which(train_sub$h1 != "NULL"),c(1,2,3)]
n = nrow(train_sub)
train_sub$X <- as.numeric(factor(train_sub$CAT))

test <- reshape(train_sub, direction = "wide", idvar = c("id") , timevar = "CAT")
#colnames(test) <- c("id", "text", "MCAT", "CCAT", "GCAT", "ECAT")

hot_vec <- function(vec) {
  vec <- as.integer(vec)
  vec[!is.na(vec)] <- 1
  vec[is.na(vec)] <- 0
  return(vec)
}

#test$MCAT <- hot_vec(test$MCAT)
#test$CCAT <- hot_vec(test$CCAT)
#test$GCAT <- hot_vec(test$GCAT)
#test$ECAT <- hot_vec(test$ECAT)

test$MCAT <- hot_vec(test$`X.MCAT      `)
test$CCAT <- hot_vec(test$`X.CCAT      `)
test$GCAT <- hot_vec(test$`X.GCAT      `)
test$ECAT <- hot_vec(test$`X.ECAT      `)

test <- test[,c(1,10,11,12,13)]

test <- merge(train_sub[,c(1,2)], test, by = "id")
test <- test[!(duplicated(test)),]

#Read in the competition data
comp_data <- read.csv("ProjectD_Dataset1.csv", header = T)
colnames(comp_data) <- c("id", "text")

## Do not remove this, just replace the sample with all the data ##
comp_data_samp <- comp_data
sample_ids <- comp_data_samp$id

## READ ABOVE ##

comp_data_samp <- cbind(comp_data_samp, rep(NA, nrow(comp_data_samp)), rep(NA, nrow(comp_data_samp)),
                   rep(NA, nrow(comp_data_samp)), rep(NA, nrow(comp_data_samp)))
colnames(comp_data_samp) <- c("id", "text","MCAT", "CCAT", "GCAT", "ECAT")

#Bind them to form overall competition data
comp_data_train <- rbind(test, comp_data_samp)

overall_n <- nrow(comp_data_train)
n <- nrow(test)

#Changed comp_data_train to train_sub
doc_matrix_comp <- create_matrix(comp_data_train$text,
                                 language="english",
                                 removeNumbers=F,
                                 stemWords = T, 
                                 removeSparseTerms=.998)

# container_comp <- create_container(doc_matrix_comp,
#                                    as.numeric(factor(comp_data_train$CAT)),
#                                    trainSize = 1:n,
#                                    testSize=(n+1):overall_n,
#                                    virgin=TRUE)

container1 <- create_container(doc_matrix_comp,
                               comp_data_train$MCAT,
                               trainSize=1:n,
                               testSize=(n+1):overall_n,
                               virgin=FALSE)
container2 <- create_container(doc_matrix_comp,
                               test$CCAT,
                               trainSize=1:n,
                               testSize=(n+1):overall_n,
                               virgin=FALSE)
container3 <- create_container(doc_matrix_comp,
                               test$GCAT,
                               trainSize=1:n,
                               testSize=(n+1):overall_n,
                               virgin=FALSE)
container4 <- create_container(doc_matrix_comp,
                               test$ECAT,
                               trainSize=1:n,
                               testSize=(n+1):overall_n,
                               virgin=FALSE)

kernel_name = 'sigmoid'
SVM1 <- train_model(container1,"SVM", kernel = kernel_name, cost=1)
SVM2 <- train_model(container2,"SVM", kernel = kernel_name, cost=1)
SVM3 <- train_model(container3,"SVM", kernel = kernel_name, cost=1)
SVM4 <- train_model(container4,"SVM", kernel = kernel_name, cost=1)

SVM_CLASSIFY1 <- classify_model(container1, SVM1)
SVM_CLASSIFY2 <- classify_model(container2, SVM2)
SVM_CLASSIFY3 <- classify_model(container3, SVM3)
SVM_CLASSIFY4 <- classify_model(container4, SVM4)

GLM1 <- train_model(container1,"GLMNET", family = "binomial", alpha=1)
GLM2 <- train_model(container2,"GLMNET", family = "binomial", alpha=1)
GLM3 <- train_model(container3,"GLMNET", family = "binomial", alpha=1)
GLM4 <- train_model(container4,"GLMNET", family = "binomial", alpha=1)

GLM_CLASSIFY1 <- classify_model(container1, GLM1)
GLM_CLASSIFY2 <- classify_model(container2, GLM2)
GLM_CLASSIFY3 <- classify_model(container3, GLM3)
GLM_CLASSIFY4 <- classify_model(container4, GLM4)

BOOST1 <- train_model(container1,"BOOSTING")
BOOST2 <- train_model(container2,"BOOSTING")
BOOST3 <- train_model(container3,"BOOSTING")
BOOST4 <- train_model(container4,"BOOSTING")

BOOST_CLASSIFY1 <- classify_model(container1, BOOST1)
BOOST_CLASSIFY2 <- classify_model(container2, BOOST2)
BOOST_CLASSIFY3 <- classify_model(container3, BOOST3)
BOOST_CLASSIFY4 <- classify_model(container4, BOOST4)

BAG1 <- train_model(container1,"BAGGING", method = "double", nbagg=10)
BAG2 <- train_model(container2,"BAGGING", method = "double", nbagg=10)
BAG3 <- train_model(container3,"BAGGING", method = "double", nbagg=10)
BAG4 <- train_model(container4,"BAGGING", method = "double", nbagg=10)

BAG_CLASSIFY1 <- classify_model(container1, BAG1)
BAG_CLASSIFY2 <- classify_model(container2, BAG2)
BAG_CLASSIFY3 <- classify_model(container3, BAG3)
BAG_CLASSIFY4 <- classify_model(container4, BAG4)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

MCAT_RESULTS <- cbind(as.data.frame(SVM_CLASSIFY1$SVM_LABEL), as.data.frame(GLM_CLASSIFY1$GLMNET_LABEL),
                     as.data.frame(BOOST_CLASSIFY1$LOGITBOOST_LABEL), as.data.frame(BAG_CLASSIFY1$BAGGING_LABEL))
colnames(MCAT_RESULTS) <- c("SVM", "GLM", "BOOST", "BAG")
MCAT_RESULTS <- cbind(MCAT_RESULTS, apply(MCAT_RESULTS, 1, Mode))
colnames(MCAT_RESULTS)[5] <- "ENSEMBLE"

CCAT_RESULTS <- cbind(as.data.frame(SVM_CLASSIFY2$SVM_LABEL), as.data.frame(GLM_CLASSIFY2$GLMNET_LABEL),
                      as.data.frame(BOOST_CLASSIFY2$LOGITBOOST_LABEL), as.data.frame(BAG_CLASSIFY2$BAGGING_LABEL))
colnames(CCAT_RESULTS) <- c("SVM", "GLM", "BOOST", "BAG")
CCAT_RESULTS <- cbind(CCAT_RESULTS, apply(CCAT_RESULTS, 1, Mode))
colnames(CCAT_RESULTS)[5] <- "ENSEMBLE"

GCAT_RESULTS <- cbind(as.data.frame(SVM_CLASSIFY3$SVM_LABEL), as.data.frame(GLM_CLASSIFY3$GLMNET_LABEL),
                      as.data.frame(BOOST_CLASSIFY3$LOGITBOOST_LABEL), as.data.frame(BAG_CLASSIFY3$BAGGING_LABEL))
colnames(GCAT_RESULTS) <- c("SVM", "GLM", "BOOST", "BAG")
GCAT_RESULTS <- cbind(GCAT_RESULTS, apply(GCAT_RESULTS, 1, Mode))
colnames(GCAT_RESULTS)[5] <- "ENSEMBLE"

ECAT_RESULTS <- cbind(as.data.frame(SVM_CLASSIFY4$SVM_LABEL), as.data.frame(GLM_CLASSIFY4$GLMNET_LABEL),
                      as.data.frame(BOOST_CLASSIFY4$LOGITBOOST_LABEL), as.data.frame(BAG_CLASSIFY4$BAGGING_LABEL))
colnames(ECAT_RESULTS) <- c("SVM", "GLM", "BOOST", "BAG")
ECAT_RESULTS <- cbind(ECAT_RESULTS, apply(ECAT_RESULTS, 1, Mode))
colnames(ECAT_RESULTS)[5] <- "ENSEMBLE"

ENSEMBLE_RESULTS <- cbind(sample_ids, as.data.frame(CCAT_RESULTS$ENSEMBLE), as.data.frame(ECAT_RESULTS$ENSEMBLE),
                          as.data.frame(GCAT_RESULTS$ENSEMBLE), as.data.frame(MCAT_RESULTS$ENSEMBLE))
colnames(ENSEMBLE_RESULTS) <- c("id", "CCAT", "ECAT", "GCAT", "MCAT")

write.csv(ENSEMBLE_RESULTS, file = "dataset1_result.csv")
