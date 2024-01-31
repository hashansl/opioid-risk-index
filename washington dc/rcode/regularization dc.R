# This was using overdose data with missing values

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


#/Users/h6x/ORNL/git/opioid-risk-index

# Importing the dataset
dcRaw <- st_read("/Users/h6x/ORNL/git/opioid-risk-index/washington dc/data/processed data/SVI2020 WashingtonDC counties with death rate/SVI2020_WashingtonDC_counties_with_death_rate.shp")

# Getting the column names
column_names <- names(dcRaw)
print(column_names)

# Filtering the data set
desired_columns <- c("EP_POV150","EP_UNEMP","EP_HBURD","EP_NOHSDP","EP_UNINSUR","EP_AGE65","EP_AGE17","EP_DISABL","EP_SNGPNT","EP_LIMENG","EP_MINRTY","EP_MUNIT","EP_MOBILE","EP_CROWD","EP_NOVEH","EP_GROUPQ","od_deaths_")
filtered_data <- dcRaw %>%
  select(one_of(desired_columns))

filtered_data2 <- dcRaw %>%
  select(one_of(desired_columns))

filtered_data3 <- dcRaw %>%
  select(one_of(desired_columns))

filtered_data <- st_drop_geometry(filtered_data)

# Splitting the dataset into features (X) and target variable (y)
X <- filtered_data[, !names(filtered_data) %in% "od_deaths_"]  # Features
y <- filtered_data$od_deaths_  # Target variable

# Set seed for reproducibility
set.seed(123)

# Create an index for splitting the data
index <- createDataPartition(filtered_data$od_deaths_, p = 0.8, list = FALSE)


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

# Save the plot using ggsave
ggsave("/home/hashan/MacBook/git/opioid-risk-index/ridge_coefficients_plot.png", plot_ridge)

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




#  Final view

# extract matrix object
res_matrix <- do.call(rbind, model_outcome)

# extract data frame object
res_df <- do.call(rbind, res_matrix[, 2])

library(reshape2)

# melt to tidy data frame
df <- melt(res_df,
           id.vars = "name",
           measure.vars = c("train_RMSE",
                            "test_RMSE")
)

# plot
ggplot(data = df, aes(x = name, y = value, fill = variable)) +
  geom_bar(stat = "identity",
           position = position_dodge()
  ) +
  scale_fill_hue(name = "") +
  xlab("") + ylab("RMSE") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)
  )



#plotting 
dwd_data_plot <- filtered_data

X <- as.matrix(
  dwd_data_plot[, colnames(
    dwd_data_plot)[!colnames(
      dwd_data_plot) %in% c("od_deaths_")]])

# extract response vector
y <- dwd_data_plot$od_deaths_
y

# calculate RMSE vector based on L2-regularized regression model (ridge regression)
preds <- predict(m_ridge_cv, X, s = "lambda.min")
rmse_vector <- (preds[, ] - y)^2 / length(y)

preds2 <- predict(m_lasso_cv, X, s = "lambda.min")
rmse_vector2 <- (preds2[, ] - y)^2 / length(y)

#filtered_data2["RMSE_Ridge"] <- round(rmse_vector, 2)
filtered_data2["RMSE_Ridge"] <- rmse_vector
filtered_data2["RMSE_Lasso"] <- rmse_vector2

library(mapview)
library(sp)

# Convert the "geometry" column to a sf object
df_sf <- st_as_sf(filtered_data2, wkt = "geometry")

# Extract the centroid coordinates
centroids <- st_centroid(df_sf["geometry"])

# Extract x and y coordinates
filtered_data2$xcol <- st_coordinates(centroids)[, 1]  # Assuming x-coordinates are in the first column
filtered_data2$ycol <- st_coordinates(centroids)[, 2]  # Assuming y-coordinates are in the second column

filtered_data2 <- st_drop_geometry(filtered_data2)

# Create an sf object
sf_object <- st_as_sf(filtered_data2, coords = c("xcol", "ycol"))

# plot data

# Create the first map
map1 <- mapView(filtered_data3)

# Create the second map and overlay it on the first map
map2 <- mapView(filtered_data2,
                xcol = "xcol",
                ycol = "ycol",
                zcol = "RMSE_Ridge",
                cex = "RMSE_Ridge",
                legend = TRUE,
                layer.name = "RMSE Ridge",
                add = FALSE  # Set to FALSE to create a new map
)

# Create the second map and overlay it on the first map
map3 <- mapView(filtered_data2,
                xcol = "xcol",
                ycol = "ycol",
                zcol = "RMSE_Lasso",
                cex = "RMSE_Lasso",
                legend = TRUE,
                layer.name = "RMSE Lasso",
                add = FALSE  # Set to FALSE to create a new map
)

# Overlay the second map on the first map
map1 + map2


map1 + map3


map4 <- mapView(filtered_data2
)

map3
map1
map2

map5 <- mapView(filtered_data3,ledgend='False',col.regions = c("gray") )
map5
map5+map2
map5+map3
