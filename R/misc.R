library(MASS)


accuracy_calc <- function(gt, predict){
    ((sum(abs(gt + predict))/2) / length(gt)) * 100
}


generate_separable_data <- function(class_1_num, class_2_num) {
  class_1 <- mvrnorm(class_1_num, c(-1, 6), diag(c(0.2, 0.2)))
  class_1 <- cbind(class_1, rep(1, class_1_num))

  class_2 <- mvrnorm(class_2_num, c(2, 5.9), diag(c(0.2, 0.2)))
  class_2 <- cbind(class_2, rep(-1, class_2_num))

  combined_class <- rbind(class_1, class_2)
  return(combined_class)
}


generate_non_separable_data <- function(class_1_num, class_2_num) {
  class_1 <- mvrnorm(class_1_num, c(1, 3), diag(c(1, 1)))
  class_1 <- cbind(class_1, rep(1, class_2_num))


  class_2 <- mvrnorm(class_1_num, c(-1, 5), diag(c(1, 1)))
  class_2 <- cbind(class_2, rep(-1, class_2_num))

  combined_class <- rbind(class_1, class_2)
  return(combined_class)
}


generate_separable_circular_data <- function() {
  x  <- replicate(10000, runif(2, min=-20, max=20), simplify = FALSE)

  in_circle <- function(a,b) {
    circle_checker <- function(z)  {
      return(((z[[1]]^2 + z[[2]]^2) > a^2) && ((z[[1]]^2 + z[[2]]^2) < b^2))
    }
  }

  checker2_3 <- in_circle(2,3)
  checker10_11 <- in_circle(10,11)

  t <- Filter(checker2_3, x)
  t <- do.call(rbind, t)
  t <- cbind(t, rep(1, dim(t)[[1]]))

  s <- Filter(checker10_11, x)
  s <- do.call(rbind, s)
  s <- cbind(s, rep(-1,dim(s)[[1]]))

  ds <- rbind(t, s)
  classes <- ds[,3]
  ds <- ds[,1:2]
  data <- list(data=ds, classes=classes)
}
