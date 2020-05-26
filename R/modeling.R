
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
  recall <- (tp / ( tp + fn ))
  precision <- (tp / (tp + fp))
  return( list( 'y.hat' = y.hat,
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
  ))
}
