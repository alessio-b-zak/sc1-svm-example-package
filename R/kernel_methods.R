
create_kernel_matrix <- function(X, f) {
  num_observations <- nrow(X)
  K <- matrix(nrow=num_observations, ncol=num_observations)
  for(i in 1:num_observations) {
    for (j in 1:num_observations) {
      K[i, j] <- f(X[i,], X[j,])
    }
  }
  return(K)
}


linear_kernel <- function(X){
  create_kernel_matrix(X, function(x, y){x %*% y})
}

linear_basis_function <- identity


polynomial_kernel <- function(d) {
  f <- function(x, y) {
    return(((t(x) %*% y) + 1 )^d)
  }
  poly_kernel <- function(X) {
    t(create_kernel_matrix(X, f))
  }
  return(poly_kernel)
}


quadratic_basis_function <- function(x) {
  size <- length(x)
  vec_size <- ((size+1)*(size+2))/2
  phi_x <-  vector(mode="numeric", vec_size)
  phi_x[1] <- 1
  last_index <- 0
  start_point <- (2 * size) + 2
  for(i in 1:size){
    phi_x[i+1] <- sqrt(2) * x[i]
    phi_x[i + 1 + size] <- x[i]*x[i]
    for(j in i:size) {
      phi_x[start_point + last_index] <- sqrt(2)*x[i]*x[j]
      last_index <- last_index + 1
    }
  }
  return(phi_x)
}



