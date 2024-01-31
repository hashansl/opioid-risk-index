# HepVu overdose data

library(sf)
library(mgcv)
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)
library(GGally) 
library(gratia)
library(caret)
library(Metrics)
library(glmnet)

#https://www.geo.fu-berlin.de/en/v/soga-r/Advances-statistics/Multivariate-approaches/Multiple-linear-regression/Regularization-methods/Regularization-methods-in-R/index.html

# washington dc/data/processed data/SVI2020 WashingtonDC counties with death rate HepVu/SVI2020_WashingtonDC_counties_with_death_rate_HepVu.shp

# Importing the dataset
dcRawHepvu <- st_read("/Users/h6x/ORNL/git/opioid-risk-index/washington dc/data/processed data/SVI2020 WashingtonDC counties with death rate HepVu/SVI2020_WashingtonDC_counties_with_death_rate_HepVu.shp")

# Getting the column names
column_names <- names(dcRawHepvu)
print(column_names)

# Filtering the data set
desired_columns <- c("EP_POV150","EP_UNEMP","EP_HBURD","EP_NOHSDP","EP_UNINSUR","EP_AGE65","EP_AGE17","EP_DISABL","EP_SNGPNT","EP_LIMENG","EP_MINRTY","EP_MUNIT","EP_MOBILE","EP_CROWD","EP_NOVEH","EP_GROUPQ","OP_Rate_20","NOD_Rate_2")
filtered_data <- dcRawHepvu %>%
  select(one_of(desired_columns))

filtered_data <- st_drop_geometry(filtered_data)

# Splitting the dataset into features (X) and target variable (y)
X <- filtered_data[, !names(filtered_data) %in% "NOD_Rate_2"]  # Features
y <- filtered_data$NOD_Rate_2  # Target variable

# Set seed for reproducibility
set.seed(123)

# Create an index for splitting the data
index <- createDataPartition(filtered_data$NOD_Rate_2, p = 0.8, list = FALSE)


# Create training sets for features and target
X_train <- X[index, ]
y_train <- y[index]

X_train_matrix <- as.matrix(X_train)


# Create test sets for features and target
X_test <- X[-index, ]
y_test <- y[-index]


X_test_matrix <- as.matrix(X_test)


### RIDGE REGRESSION ###
m_ridge <- glmnet(X_train, y_train, alpha = 0)

# plot model coefficients vs. shrinkage parameter lambda
plot_ridge <- plot(m_ridge, xvar = "lambda", label = TRUE, xlab = expression(paste("Log(", lambda, ")")))


# /Users/h6x/ORNL/git/opioid-risk-index/washington dc/rcode/results/regularization dc HepVu
# Save the plot using ggsave
ggsave("/Users/h6x/ORNL/git/opioid-risk-index/washington dc/rcode/results/regularization dc HepVu/ridge_coefficients_plot.png", plot_ridge)

grid <- 10^seq(10, -1, length = 100) # set lambda sequence
m_ridge_cv <- cv.glmnet(X_train_matrix,
                        y_train,
                        alpha = 0,
                        lambda = grid)

plot(m_ridge_cv)

m_ridge_cv$lambda.min

m_ridge_cv$lambda.1se

#my
fitR <- glmnet(X_train, y_train, alpha = 0,lambda = m_ridge_cv$lambda.1se)
fitR$beta[,1]

# prediction for the training set
m_ridge_cv_pred_train <- predict(m_ridge_cv,
                                 X_train_matrix, s = "lambda.min")

# calculate RMSE on training set
print(paste("RMSE on training set:", rmse(m_ridge_cv_pred_train,
                                          y_train)
)
)

# prediction for the test set
m_ridge_cv_pred_test <- predict(m_ridge_cv,
                                X_test_matrix, s = "lambda.min")

# calculate RMSE for the test data set
print(paste("RMSE on test set:", rmse(m_ridge_cv_pred_test,
                                      y_test)
)
)

# Initialize model_outcome as an empty list if it doesn't exist
if (!exists("model_outcome")) {
  model_outcome <- list()
}


# store model object and results of RMSE in the `list` object named `model_outcome`
model_outcome[["m_ridge_cv"]] <- list("model" = m_ridge_cv,
                                      "rmse" =  data.frame("name" = "ridge regression",
                                                           "train_RMSE" = rmse(m_ridge_cv_pred_train,
                                                                               y_train),
                                                           "test_RMSE" = rmse(m_ridge_cv_pred_test,
                                                                              y_test)
                                      )
)


# LASSO regression in R

### LASSO REGRESSION ###
m_lasso <- glmnet(X_train, y_train, alpha = 1)

# plot model coefficients vs. shrinkage parameter lambda
plot(m_lasso, xvar = "lambda", label = TRUE, xlab = expression(paste("Log(", lambda, ")")))

m_lasso_cv <- cv.glmnet(X_train_matrix, y_train, alpha = 1)

plot(m_lasso_cv)

m_lasso_cv$lambda.min

m_lasso_cv$lambda.1se

#my
fit <- glmnet(X_train, y_train, alpha = 1,lambda = m_lasso_cv$lambda.1se)

fit$beta[,1]


# prediction for the training set
m_lasso_cv_pred_train <- predict(m_lasso_cv, X_train_matrix, s = "lambda.min")

# calculate RMSE on training set
print(paste("RMSE on training set:",
            rmse(m_lasso_cv_pred_train,
                 y_train)
)
)

# prediction for the test set
m_lasso_cv_pred_test <- predict(m_lasso_cv, X_test_matrix, s = "lambda.min")

# calculate RMSE for the test data set
print(paste("RMSE on test set:",
            rmse(m_lasso_cv_pred_test,
                 y_test)
)
)


# store model object and results of RMSE in the `list` object named `model_outcome`
model_outcome[["m_lasso_cv"]] <- list("model" = m_lasso_cv,
                                      "rmse" =  data.frame("name" = "LASSO regression",
                                                           "train_RMSE" = rmse(m_lasso_cv_pred_train, y_train),
                                                           "test_RMSE" = rmse(m_lasso_cv_pred_test, y_test)
                                      )
)
















