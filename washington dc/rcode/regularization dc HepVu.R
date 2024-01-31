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

# Importing the dataset
dcRawHepvu <- st_read("/Users/h6x/ORNL/git/opioid-risk-index/data/SVI 2020 with HepVu/SVI2020_US_county_with_opioid_indicators.shp")


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
y <- filtered_data$od_deaths_  # Target variable

# Set seed for reproducibility
set.seed(123)