train <- read.csv("rcv1_test.csv", header = F)

col_names = c("id", "text", "CAT", "h1","h2", "h3", "h4", "h5")
colnames(train) <- col_names

train_sub <- sample_n(train, 10000)
train_sub <- train_sub[which(train_sub$h1 != "NULL"),c(1,2,3)]
n = nrow(train_sub)

comp_data <- read.csv("articles_test.csv", header = F)
colnames(comp_data) <- c("id", "text")

comp_data <- cbind(comp_data, rep(NA, nrow(comp_data)))
colnames(comp_data)[3] <- "CAT"
comp_data_train <- rbind(train_sub, comp_data)

overall_n <- nrow(comp_data_train)

doc_matrix_comp <- create_matrix(comp_data_train$text,
                                 language="english",
                                 removeNumbers=F,
                                 stemWords = T, 
                                 removeSparseTerms=.998)

container_comp <- create_container(doc_matrix_comp,
                                   as.numeric(factor(comp_data_train$CAT)),
                                   trainSize = 1:n,
                                   testSize=(n+1):overall_n,
                                   virgin=TRUE)

GLMNET <- train_model(container_comp,"GLMNET")
SVM <- train_model(container_comp,"SVM")
BOOST <- train_model(container_comp,"BOOSTING")
BAGGING <- train_model(container_comp,"BAGGING")

SVM_CLASSIFY_COMP <- classify_model(container_comp, SVM)
GLMNET_CLASSIFY_COMP <- classify_model(container_comp, GLMNET)
BOOST_CLASSIFY_COMP <- classify_model(container_comp, BOOST)
BAGGING_CLASSIFY_COMP <- classify_model(container_comp, BAGGING)

## Can create new container with any train data and new test data, and pass it to model

comp_cats <- read.csv("cat_test.csv", header = F)
comp_cats$V2 <- as.numeric(factor(comp_cats$V2))

result_generator <- function(labels) {
  res <- cbind(labels, comp_cats$V2)
  colnames(res) <- c("Pred", "Actual")
  res <- as.data.frame(res)
  return(length(which(res$Pred == res$Actual)) / nrow(res))
}

result_generator(GLMNET_CLASSIFY_COMP$GLMNET_LABEL)
result_generator(BOOST_CLASSIFY_COMP$LOGITBOOST_LABEL)
result_generator(SVM_CLASSIFY_COMP$SVM_LABEL)
result_generator(BAGGING_CLASSIFY_COMP$BAGGING_LABEL)

results <- cbind(GLMNET_CLASSIFY_COMP$GLMNET_LABEL,SVM_CLASSIFY_COMP$SVM_LABEL,BAGGING_CLASSIFY_COMP$BAGGING_LABEL  )
results <- as.data.frame(results)
colnames(results) <- c("GLM", "SVM", "BAG")

results$GLM <- as.numeric(as.character(results$GLM))
results$SVM <- as.numeric(as.character(results$SVM))
results$BAG <- as.numeric(as.character(results$BAG))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

results <- cbind(results, apply(results, 1, Mode))
colnames(results)[4] <- "ENSEMBLE"

result_generator(results$ENSEMBLE)

res <- cbind(SVM_CLASSIFY_COMP$SVM_LABEL, comp_cats$V2)
colnames(res) <- c("Pred", "Actual")
res <- as.data.frame(res)

accuracy = length(which(res$Pred == res$Actual)) / nrow(res)
accuracy
