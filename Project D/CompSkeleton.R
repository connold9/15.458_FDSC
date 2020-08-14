train <- read.csv("rcv1_test.csv", header = F)
col_names = c("id", "text", "CAT", "h1","h2", "h3", "h4", "h5")
colnames(train) <- col_names

##Choose a Sample to create a container
train_sub <- sample_n(train, 14000)
train_sub <- train_sub[which(train_sub$h1 != "NULL"),c(1,2,3)]
n = nrow(train_sub)

#Read in the competition data
comp_data <- read.csv("articles_test.csv", header = F)
colnames(comp_data) <- c("id", "text")

comp_data <- cbind(comp_data, rep(NA, nrow(comp_data)))
colnames(comp_data)[3] <- "CAT"

#Bind them to form overall competition data
comp_data_train <- rbind(train_sub, comp_data)

overall_n <- nrow(comp_data_train)

#Changed comp_data_train to train_sub
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

## DO NOT NEED TO TRAIN MODEL
## CLASSIFY ON EACH OF THE TRAINED AND TUNED MODELS
GLM_COMP <- train_model(container_comp,"GLMNET", family = "multinomial", alpha = 1)
SVM_COMP <- train_model(container_comp,"SVM", cost = 1)
BOOST_COMP <- train_model(container_comp,"BOOSTING")
BAG_COMP <- train_model(container_comp,"BAGGING", method = "double", nbagg=10)

SVM_CLASSIFY_COMP <- classify_model(container_comp, SVM_COMP)
GLMNET_CLASSIFY_COMP <- classify_model(container_comp, GLM_COMP)
BAGGING_CLASSIFY_COMP <- classify_model(container_comp, BAG_COMP)
BOOSTING_CLASSIFY_COMP <- classify_model(container_comp, BOOST_COMP)

comp_cats <- read.csv("cat_test.csv", header = F)
comp_cats$V2 <- as.numeric(factor(comp_cats$V2))

result_generator <- function(labels) {
  res <- cbind(labels, comp_cats$V2)
  colnames(res) <- c("Pred", "Actual")
  res <- as.data.frame(res)
  return(length(which(res$Pred == res$Actual)) / nrow(res))
}

result_generator(GLMNET_CLASSIFY_COMP$GLMNET_LABEL)
result_generator(SVM_CLASSIFY_COMP$SVM_LABEL)
result_generator(BAGGING_CLASSIFY_COMP$BAGGING_LABEL)
result_generator(BOOSTING_CLASSIFY_COMP$LOGITBOOST_LABEL)

results_comp <- cbind(GLMNET_CLASSIFY_COMP$GLMNET_LABEL,
                      SVM_CLASSIFY_COMP$SVM_LABEL,
                      BAGGING_CLASSIFY_COMP$BAGGING_LABEL,
                      BOOSTING_CLASSIFY_COMP$LOGITBOOST_LABEL)
results_comp <- as.data.frame(results_comp)
colnames(results_comp) <- c("GLM", "SVM", "BAG", "BOOST")

#Change to numeric
results_comp$GLM <- as.numeric(as.character(results_comp$GLM))
results_comp$SVM <- as.numeric(as.character(results_comp$SVM))
results_comp$BAG <- as.numeric(as.character(results_comp$BAG))
results_comp$BOOST <- as.numeric(as.character(results_comp$BOOST))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Create the final results via mode of three models
results_comp <- cbind(results_comp, apply(results_comp, 1, Mode))
colnames(results_comp)[5] <- "ENSEMBLE"

#Calculating accuracy of ensemble
result_generator(results_comp$ENSEMBLE)
