#-------------------------------------------------------------------------------
#' Present a model: summary, confusion matrix, and ROC plot
#'
#' @param model   A fitted glm (binomial) model
#' @param train   Training data frame (must include Target)
#' @param test    Test data frame (must include Target)
#' @param thresh  Optional probability threshold; if NULL, uses Youden’s J
#' @export
present_model <- function(model, train, test, thresh = NULL) {
  # 1) Model summary
  cat("\n=== Model Summary ===\n")
  print(summary(model))

  # 2) Predict & ROC
  suppressPackageStartupMessages(library(pROC))
  probs   <- predict(model, newdata = test, type = "response")
  roc_obj <- suppressWarnings(roc(test$Target, probs))

  # 3) Threshold
  if (is.null(thresh)) {
    thr_info <- coords(
      roc_obj,
      x           = "best",
      best.method = "youden",
      ret         = c("threshold", "sensitivity", "specificity")
    )
    thresh <- as.numeric(thr_info["threshold"])
    cat(sprintf("\nOptimal threshold (Youden's J): %.3f\n",    thresh))
    cat(sprintf("  Sensitivity: %.3f\n",   thr_info["sensitivity"]))
    cat(sprintf("  Specificity: %.3f\n",   thr_info["specificity"]))
  } else {
    cat(sprintf("\nUsing provided threshold: %.3f\n", thresh))
  }

  # 4) Confusion matrix
  suppressPackageStartupMessages(library(caret))
  preds <- factor(ifelse(probs > thresh, 1, 0), levels = c(0,1))
  cm    <- confusionMatrix(
    preds,
    factor(test$Target, levels = c(0,1))
  )
  cat("\n=== Confusion Matrix ===\n")
  print(cm$table)
  cat(sprintf("\nAccuracy:    %.3f\n", cm$overall["Accuracy"]))
  cat(sprintf("Sensitivity: %.3f\n", cm$byClass["Sensitivity"]))
  cat(sprintf("Specificity: %.3f\n", cm$byClass["Specificity"]))

  # 5) ROC plot
  cat("\n=== ROC Curve ===\n")
  suppressPackageStartupMessages(library(ggplot2))
  auc_val <- auc(roc_obj)
  roc_df  <- data.frame(
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities
  )
  p <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
    geom_line(size = 1) +
    geom_abline(linetype = "dashed", color = "grey30") +
    labs(
      title = sprintf("ROC Curve (AUC = %.3f)", auc_val),
      x     = "False Positive Rate",
      y     = "True Positive Rate"
    ) +
    theme_minimal()
  print(p)
}


#' Load the pre-trained NBA parlay model
#'
#' @return A `glm` object trained on Elo + AllTimeElo
#' @export
load_model <- function() {
  # This will load final_model from data/final_model.rda into the env
  if (!exists("final_model", envir = asNamespace("NBAMODEL"), inherits = FALSE)) {
    data("final_model", package = "NBAMODEL", envir = environment())
  }
  final_model
}
