library(sf)
library(dplyr)

# Importing the dataset
dcRaw <- st_read("~/MacBook/git/opioid-risk-index/washington dc/data/processed data/SVI2020 WashingtonDC counties with death rate/SVI2020_WashingtonDC_counties_with_death_rate.shp")

# Getting the column names
column_names <- names(dcRaw)
print(column_names)

# Filtering the data set
desired_columns <- c("EP_POV150","EP_UNEMP","EP_HBURD","EP_NOHSDP","EP_UNINSUR","EP_AGE65","EP_AGE17","EP_DISABL","EP_SNGPNT","EP_LIMENG","EP_MINRTY","EP_MUNIT","EP_MOBILE","EP_CROWD","EP_NOVEH","EP_GROUPQ","od_deaths_")

filtered_data <- dcRaw %>%
  select(one_of(desired_columns)) 

filtered_data <- st_drop_geometry(filtered_data)  

plot(filtered_data$od_deaths_)

hist(filtered_data$od_deaths_,breaks=20,col="lightblue",main="Histogram of Data",xlab="data",ylab="Frequency")


# Get the number of columns in the filtered_data
num_columns <- ncol(filtered_data)

# Set up the layout for multiple histograms
par(mfrow = c(ceiling(sqrt(num_columns)), ceiling(sqrt(num_columns))))

# Plot histograms for each column
for (i in 1:num_columns) {
  hist(filtered_data[[i]], breaks=20, main = names(filtered_data)[i], xlab = "", col = "skyblue", border = "black")
}

# Reset the plotting layout
par(mfrow = c(1, 1)) # Reset to single plot