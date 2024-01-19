library(glmnet) 


n <- 1000  # Number of observations
p <- 5000  # Number of predictors included in model
real_p <- 15  # Number of true predictors

## Generate the data
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

## data will be used for Testing.
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]


### RIDGE REGRESSION ###
m_ridge <- glmnet(x.train, y.train, alpha = 0)

# plot model coefficients vs. shrinkage parameter lambda
plot(m_ridge, xvar = "lambda", label = TRUE, xlab = expression(paste("Log(", lambda, ")")))
