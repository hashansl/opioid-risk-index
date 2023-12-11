library(sf)
library(mgcv)

dcRaw <- st_read("~/MacBook/git/opioid-risk-index/washington dc/data/processed data/SVI2020 WashingtonDC counties with death rate/SVI2020_WashingtonDC_counties_with_death_rate.shp")


# Using names() function
column_names <- names(dcRaw)
print(column_names)

#trying the linear model first
mod_lm1 = gam(od_deaths_ ~ Income + Edu + Health, data = dcRaw)
