#' Get metrics of heterogeneity
#'
#' \code{get_heterogen} computes the heterogeneity metrics for meta-analysis.
#'
#' This function computes the heterogeneity metrics for the meta-analysis,
#' including I2, H2, and Q
#'
#' @inheritParams parameters_definition
#'
#' @export
#' @return A vector of the calculated heterogeneity metrics:

# ToDo 1: explain more about all the heterogeneity metrics - usig Koricheva's et al. book

get_heterogen <- function(model, digit = 2) {
  if (length(model$lambda) > 1) {
    stop("only one lambda is allowed")
  }

  vi <- model$phi
  wi <- 1/vi  ## weights = inverse of sampling variance
  k <- length(model$ranef)  ## number of groups in random term (here number of studies) ## ToDo: replace with extractor if possible (Alex)
  p <- length(spaMM::fixef(model))  ## number of fixed effect parameters
  W <- diag(wi, nrow = length(wi), ncol = length(wi))  ## matrix of weights
  X <- as.matrix(as.data.frame(model$X.pv))  ## design matrix ## ToDo: replace with extractor if possible (Alex)
  stXWX <- solve(t(X) %*% W %*% X)  ## weighted vcov of estimates
  P <- W - W %*% X %*% stXWX %*% crossprod(X, W)  ## weighted vcov of ?? ## ToDo: clarify? (Alex)
  vi.avg <- (k - p) / sum(diag(P))
  I2 <- as.numeric(model$lambda / (vi.avg + model$lambda))
  H2 <- as.numeric((vi.avg + model$lambda) / vi.avg)
  QE <- max(0, c(crossprod(model$y, P) %*% model$y))  ## Q statistics (assuming lambda = 0)
                                                      ## same as usual Q formula applied on model where ID is fixed and not random
  Qp <- stats::pchisq(QE, df = (k - p), lower.tail = FALSE)  ## Test if Q = 0
  res <- c(I2 = I2, H2 = H2, invH2 = 1/H2, Q = QE, Qp = Qp, vi.avg = vi.avg, lambda = model$lambda[[1]])
  print(signif(res, digit))
  cat("\n # I2 is the proportion of variance between studies (the one caused by the random effect)\n",
      "# H2 is the inverse of the proportion of variance within studies\n",
      "# invH2 is the proportion of variance within studies\n",
      "# Q is the total heterogeneity (in a fixed effect model)\n",
      "# Qp is the p-value for the test Q = 0 (Q follows a chi-square distribution under the null hypothesis)\n",
      "# vi.avg is the average variance within study\n",
      "# lambda (also called tau^2) is the variance between studies\n"
  )
  return(invisible(res))
}
