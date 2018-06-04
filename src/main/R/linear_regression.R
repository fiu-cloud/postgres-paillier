# Title     : Linear regression example - gradient descent
# Created by: KeeSiong Ng

n = 50
x1 = 11 : (10 + n)
x2 = runif(n, 5, 95)
x3 = rbinom(n, 1, 0.5)
x = data.frame(x1, x2, x3)
x = scale(x)
x = data.frame(1, x)
sigma = 1.4
eps = rnorm(x1, 0, sigma) #noise vector
b = c(17, - 2.5, 0.5, - 5.2) #real coefficients
y = b[1] + b[2] * x[, 2] + b[3] * x[, 3] + b[4] * x[, 4] + scale(eps) #target variable


lm_sgd <- function(iter, rate) {
    theta = c(0, 0, 0, 0)
    alpha = rate
    for (iter in 1 : iter) {
        adj = c(0, 0, 0, 0)
        for (i in 1 : 4) {
            for (j in 1 : n) {
                adj[i] = adj[i] + (sum(x[j,] * theta) - y[j]) * x[j, i]
            }
        }
        for (i in 1 : 4)theta[i] = theta[i] - (alpha / n) * adj[i]
        print(adj)
    }
    print(theta)
}

lm_sgd(50, 0.1)
