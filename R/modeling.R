
#' extract all performance metrics for a binomial classification model
#'
#' @param mod a general linear model of family binomial
#' @param pi_0 the threshold of determination used in classifaction
#' @return A list of classification performance metrics including the vector of predictions y.hat and the confusion matrix
#' @examples
#' X <- rnorm(100)
#' Y <- ifelse(X < 0.5, 0, 1 - sample(0:1, prob = c(0.9, .1)))
#' f <- glm(Y ~ X, family='binomial')
#' classification_metrics(f)
classification_metrics <- function(mod, pi_0 = 0.5){
  y.hat <- factor(ifelse(fitted.values(mod) >= pi_0, 1, 0), levels = c(1,0), labels = c('1','0'), ordered = T)
  y <- factor(mod$y, levels = c(1,0), labels = c('1','0'), ordered = T)
  confm <- table(y.hat, y)
  tp <- confm[1]
  fn <- confm[2]
  fp <- confm[3]
  tn <- confm[4]
  recall <- (tp / (tp + fn))
  precision <- (tp / (tp + fp))
  return(
    list(
      'y.hat' = y.hat,
      'confmatrix' = confm,
      'tpos' = tp,
      'tneg' = tn,
      'fneg' = fn,
      'fpos' = fp,
      'acc' = (tn + tp) / sum(confm),
      'recall' = recall,
      'precision' = precision,
      'tpr' = recall,
      'fpr' = (fp / (tn + fp)),
      'fdcr' = (fp / (tp + fp)),
      'fnr' = (fn / (tp + fn)),
      'fomr' = (fn / (tn + fn)),
      'tnr' = (tn / (tn + fp)),
      'npv' = (tn / (tn + fn)),
      'F1' = 2 * ((precision * recall) / (precision + recall))
    )
  )
}

#' create training, testing and validation datasets in specified proportion
#'
#' @param data a data.frame type object to split
#' @param prop_train proportion of data in training set
#' @param prop_test proportion of data in test set
#' @param prop_validation proportion of data in validation set (optional)
#' @return a list containing the train and test data (and validation data if proportion > 0)
#' @examples
#' data.ex <- data.frame('A' = sample(1:100,100), 'B' = sample(101:200,100))
#' train_test_val(data.ex, 0.6)
#' train_test_val(data.ex, 0.6, 0.3, 0.1)
train_test_val <- function(data, prop_train = 0.8, prop_test = 0, prop_validation = 0){
  prop_test <- ifelse(prop_test == 0, (1 - prop_train), prop_test)
  assertthat::assert_that(is.data.frame(data))
  assertthat::are_equal(1 , (prop_train + prop_test + prop_validation))

  # split train and test set
  train_test <- train_test_val._split(data, prop_train)

  if(prop_validation > 0){
    # get proportion split between test and validation
    test_split <- prop_test / (prop_test + prop_validation)

    # split out test and validation set from test set
    test_validation <- train_test_val._split(train_test[[2]], test_split)
    return(
      list(
        'train' = train_test[[1]],
        'test' = test_validation[[1]],
        'validation' = test_validation[[2]]
      )
    )
  } else {
    return(
      list(
        'train' = train_test[[1]],
        'test' = train_test[[2]]
      )
    )
  }
}



train_test_val._split <- function(data, prop){
  n <- dim(data)[1]
  indices <- sample(1:n, floor((1 - prop) * n))
  return(
    list(
      data[-indices,],
      data[indices,]
    )
  )
}

