library(MASS)
library(quadprog)
source("R/kernel_methods.R")
source("R/misc.R")

#create s3 object that is a model.
svm <- function(X, classes, C, margin_type, kernel_function, feature_map) {
  if(margin_type == "hard" &&
     ((!missing(kernel_function))
      || (!missing(feature_map))
      || (!missing(C)))) {
    stop("Hard Margin SVM does not support kernel functions")
  }

  if(margin_type == "soft") {
    a <- train_kernel_soft_margin_svm(X, classes, C, kernel_function)$sol
    params <- recover_kernel_params(X, classes, a, C, feature_map)
    prediction_fun <- predict_kernel_svm(params$w, params$b, feature_map)
  }
  else {
    params <- train_primal_hard_margin_svm(X, classes)
    prediction_fun <- predict_hard_margin_svm(params$w, params$b)
  }
  model <- list(prediction_fun, params)
  names(model) <- c("prediction_function", "params")
  return(prediction_fun)
}



train_primal_hard_margin_svm <- function(data, classes) {
  feature_num <- ncol(data)
  num_observations <- nrow(data)

  Dm <- diag(feature_num)
  Dm <- cbind(Dm, rep(0, feature_num))
  Dm <- rbind(Dm, rep(0, feature_num + 1))
  #peturb diagonal of D to make it positive definite
  diag(Dm) <- diag(Dm) + 1e-6

  dv <- rep(0, feature_num + 1)

  bv <- rep(1, num_observations)

  A <- cbind(data, rep(-1, num_observations))
  A <- diag(classes) %*% A

  params <- solve.QP(Dm, dv, t(A), bv)$solution

  return_params <- list(params[1:ncol(data)], params[[length(params)]])
  names(return_params) <- c("w", "b")

  return(return_params)
}

primal_hard_margin_2d_plotter <- function(w,b){
  hard_svm_plotter <- function(x) {
    1/w[2] * (b -(w[1] * x ))
  }
  return(hard_svm_plotter)
}


train_kernel_soft_margin_svm <- function(X, y, C, kernel_function) {

  num_observation <- nrow(X)
  dim_num <- ncol(X)
  K <- kernel_function(X)
  Dmat2 <- diag(y) * K %*% diag(y)
  diag(Dmat2) <- diag(Dmat2) + 1e-11
  dv2 <- rep(1, num_observation)

  #constraints: 1 \leq \lambda \geq 0, \sum \lambda_i y_u = 0

  A2 <- rbind( y,diag(num_observation))
  A2 <- rbind(A2, -1*diag(num_observation))

  bv2 <- c(c(0), rep(0, num_observation), rep(-C, num_observation) )
  model <- solve.QP(Dmat2, dv2, t(A2), bv2, meq = 1)
}

predict_kernel_svm <- function(w, b, feature_map) {
  kernel_predictor <- function(x) {
    return(sign(w %*% feature_map(x) + b))
  }
  return(kernel_predictor)
}

predict_hard_margin_svm <- function(w, b) {
  kernel_predictor <- function(x) {
    return(sign((w %*% x) - b))
  }
  return(kernel_predictor)
}


calculate_kernel_b <- function(w, X, y, a, C, feature_map) {
  ks <- sapply(a, function(x){return(x > 0 && x < C)})
  indices <- which(ks)
  sum_bs <- 0
  for(i in indices) {
    sum_bs <- sum_bs + (y[i] - w %*% feature_map(X[i,]))
  }
  return(sum_bs / length(indices))
}

recover_kernel_params <- function(X,y, a, C, feature_map) {
  phiX <- t(apply(X, 1, feature_map))
  w <- colSums(diag(a) %*% (diag(y) %*% phiX))
  b <- calculate_kernel_b(w, X, y, a, C, feature_map)
  params <- list(w, b)
  names(params) <- c("w", "b")
  return(params)
}

