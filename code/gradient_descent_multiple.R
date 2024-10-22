#' Gradient descent for multiple linear regression
#'
#' Perform gradient descent for linear regression with multiple predictors
#' @param X the predictors (an n*p matrix). X should be standardized first.
#' @param y the response variable (a vector)
#' @param learning_rate the step size for gradient descent
#' @param max_itr the maximum number of iterations to run gradient descent
#' @param tol the tolearnace between current and best cost
#' @returns Theta estimated parameters
#' @returns cost cost history
#' @returns best_cost best cost
#' @returns itr number of iterations conducted
#' @examples
#' # Generate X with n=100, p=5
#' X <- matrix(rnorm(100*5, mean = 0, sd = 1), 100, 5)
#' Theta = c(1, 0.5, -5, 2, 3)
#' y <- X%*%Theta + rnorm(100, mean = 0, sd = 1)

#' # Perform gradient descent
#' result <- gradient_descent_multiple(X, y, learning_rate = 0.01, max_itr = 10000, tol = 1e-5)

#' Thetahat = result$Theta
#' result$itr
#' result$best_cost
#' cost = result$cost
#' # plot the cost history over iterations
#' plot(1:length(cost), cost, xlab = "iterations", main = "Cost over iterations")

#' @export
gradient_descent_multiple <- function(X, y,  learning_rate = 0.01, max_itr = 1000, tol = 1e-5) {
  # Initialize parameters
  n <- length(y)  # number of data points
  p <- ncol(X) # number of predictors
  Theta <- rep(0, p)     # parameters


  # Store the cost at each iteration
  cost <- numeric(max_itr)
  best_cost = 100000

  # Gradient descent algorithm
  itr = 1
  repeat{
    cat(itr)
    # Compute predictions
    y_pred <- X%*%Theta

    # Compute the error
    error <- y - y_pred

    # Compute the current cost (mean squared error)
    current_cost <- (1/(2*n)) * sum(error^2)

    cost[itr] = current_cost

    if (abs(best_cost - current_cost) < tol || itr == max_itr) {
      cost = cost[1:(itr-1)]
      break}
    else{
      best_cost = current_cost

      itr = itr + 1

      # Compute gradients
      gradient = -(1/n) * t(X)%*%(error)

      # Update parameters
      Theta <- Theta - learning_rate * gradient

    }

  }

  # Return the optimized parameters and the cost history
  return(list(Theta = Theta, cost = cost, best_cost = best_cost, itr = itr))
}
