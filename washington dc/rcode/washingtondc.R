library(sf)
library(mgcv)
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)
library(GGally) 

# Importing the dataset
dcRaw <- st_read("~/MacBook/git/opioid-risk-index/washington dc/data/processed data/SVI2020 WashingtonDC counties with death rate/SVI2020_WashingtonDC_counties_with_death_rate.shp")

# Getting the column names
column_names <- names(dcRaw)
print(column_names)

# Filtering the data set
desired_columns <- c("EP_POV150","EP_UNEMP","EP_HBURD","EP_NOHSDP","EP_UNINSUR","EP_AGE65","EP_AGE17","EP_DISABL","EP_SNGPNT","EP_LIMENG","EP_MINRTY","EP_MUNIT","EP_MOBILE","EP_CROWD","EP_NOVEH","EP_GROUPQ","od_deaths_")
filtered_data <- my_data %>%
  select(one_of(desired_columns))

filtered_data <- st_drop_geometry(filtered_data)

# Pearson's correlation plot
chart.Correlation(filtered_data, histogram = TRUE, method = "pearson")

#trying the linear regression model first
linear_model = lm(od_deaths_ ~EP_POV150+EP_UNEMP+EP_HBURD+EP_NOHSDP+EP_UNINSUR+EP_AGE65+EP_AGE17+EP_DISABL+EP_SNGPNT+EP_LIMENG+EP_MINRTY+EP_MUNIT+EP_MOBILE+EP_CROWD+EP_NOVEH+EP_GROUPQ , data = dcRaw)
summary(linear_model)

# Same linear model with GAM library
mod_lm1 = gam(od_deaths_ ~EP_POV150+EP_UNEMP+EP_HBURD+EP_NOHSDP+EP_UNINSUR+EP_AGE65+EP_AGE17+EP_DISABL+EP_SNGPNT+EP_LIMENG+EP_MINRTY+EP_MUNIT+EP_MOBILE+EP_CROWD+EP_NOVEH+EP_GROUPQ , data = dcRaw)
summary(mod_lm1)

# Basic GAM model
# Look for nonlinear effects for each feature
mod_gam1 = gam(od_deaths_ ~s(EP_POV150)+s(EP_UNEMP)+s(EP_HBURD)+s(EP_NOHSDP)+s(EP_UNINSUR)+s(EP_AGE65)+s(EP_AGE17)+s(EP_DISABL)+s(EP_SNGPNT)+s(EP_LIMENG)+s(EP_MINRTY)+s(EP_MUNIT)+s(EP_MOBILE)+s(EP_CROWD)+s(EP_NOVEH)+s(EP_GROUPQ) , data = dcRaw)
summary(mod_gam1)

#plotting
plot(ggeffects::ggpredict(mod_gam2), facets = TRUE)
gratia::draw(mod_gam2)

# Model comparison
anova(mod_lm1, mod_gam2, test = "Chisq")

# GAM model 2
mod_gam2 = gam(od_deaths_ ~s(EP_POV150, bs = 'tp', k = 10)+
                 s(EP_UNEMP, bs = 'tp', k = 10)+
                 s(EP_HBURD, bs = 'tp', k = 10)+
                 s(EP_NOHSDP, bs = 'tp', k = 10)+
                 s(EP_UNINSUR, bs = 'tp', k = 10)+
                 s(EP_AGE65, bs = 'tp', k = 10)+
                 s(EP_AGE17, bs = 'tp', k = 10)+
                 s(EP_DISABL, bs = 'tp', k = 10)+
                 s(EP_SNGPNT, bs = 'tp', k = 10)+
                 s(EP_LIMENG, bs = 'tp', k = 10)+
                 s(EP_MINRTY, bs = 'tp', k = 10)+
                 s(EP_MUNIT, bs = 'tp', k = 10)+
                 s(EP_MOBILE, bs = 'tp', k = 10)+
                 s(EP_CROWD, bs = 'tp', k = 10)+
                 s(EP_NOVEH, bs = 'tp', k = 10)+
                 s(EP_GROUPQ, bs = 'tp', k = 10) , data = dcRaw)
summary(mod_gam2)
gam.check(mod_gam2)


# GAM model 3
mod_gam3 = gam(od_deaths_ ~s(EP_POV150, bs = 'tp', k = 10)+
                 s(EP_UNEMP, bs = 'tp', k = 10)+
                 s(EP_HBURD, bs = 'tp', k = 10)+
                 s(EP_NOHSDP, bs = 'tp', k = 10)+
                 s(EP_UNINSUR, bs = 'tp', k = 10)+
                 s(EP_AGE65, bs = 'tp', k = 10)+
                 s(EP_AGE17, bs = 'tp', k = 10)+
                 s(EP_DISABL, bs = 'tp', k = 10)+
                 s(EP_SNGPNT, bs = 'tp', k = 10)+
                 s(EP_LIMENG, bs = 'tp', k = 10)+
                 s(EP_MINRTY, bs = 'tp', k = 10)+
                 s(EP_MUNIT, bs = 'tp', k = 10)+
                 s(EP_MOBILE, bs = 'tp', k = 10)+
                 s(EP_CROWD, bs = 'tp', k = 10)+
                 s(EP_NOVEH, bs = 'tp', k = 10)+
                 s(EP_GROUPQ, bs = 'tp', k = 10) , data = dcRaw, family=poisson)
summary(mod_gam3)
gam.check(mod_gam3)

# GAM model 4
mod_gam4 = gam(od_deaths_ ~s(EP_POV150, bs = 'tp', k = 10)+
                 s(EP_UNEMP, bs = 'tp', k = 10)+
                 s(EP_HBURD, bs = 'tp', k = 10)+
                 s(EP_NOHSDP, bs = 'tp', k = 10)+
                 s(EP_UNINSUR, bs = 'tp', k = 10)+
                 s(EP_AGE65, bs = 'tp', k = 10)+
                 s(EP_AGE17, bs = 'tp', k = 10)+
                 s(EP_DISABL, bs = 'tp', k = 10)+
                 s(EP_SNGPNT, bs = 'tp', k = 10)+
                 s(EP_LIMENG, bs = 'tp', k = 10)+
                 s(EP_MINRTY, bs = 'tp', k = 10)+
                 s(EP_MUNIT, bs = 'tp', k = 10)+
                 s(EP_MOBILE, bs = 'tp', k = 10)+
                 s(EP_CROWD, bs = 'tp', k = 10)+
                 s(EP_NOVEH, bs = 'tp', k = 10)+
                 s(EP_GROUPQ, bs = 'tp', k = 10) , data = dcRaw, family=nb, method = 'REML')
summary(mod_gam4)
gam.check(mod_gam4)


# GAM model 5
mod_gam5 = gam(od_deaths_ ~s(EP_POV150, bs = 'tp', k = 10)+
                 s(EP_UNEMP, bs = 'tp', k = 10)+
                 s(EP_HBURD, bs = 'tp', k = 10)+
                 s(EP_NOHSDP, bs = 'tp', k = 10)+
                 s(EP_UNINSUR, bs = 'tp', k = 10)+
                 s(EP_AGE65, bs = 'tp', k = 10)+
                 s(EP_AGE17, bs = 'tp', k = 10)+
                 s(EP_DISABL, bs = 'tp', k = 10)+
                 s(EP_SNGPNT, bs = 'tp', k = 10)+
                 s(EP_LIMENG, bs = 'tp', k = 10)+
                 s(EP_MINRTY, bs = 'tp', k = 10)+
                 s(EP_MUNIT, bs = 'tp', k = 10)+
                 s(EP_MOBILE, bs = 'tp', k = 10)+
                 s(EP_CROWD, bs = 'tp', k = 10)+
                 s(EP_NOVEH, bs = 'tp', k = 10)+
                 s(EP_GROUPQ, bs = 'tp', k = 10) , data = dcRaw, family=gaussian, method = 'REML')
summary(mod_gam5)
gam.check(mod_gam5)
draw(mod_gam5, scales = "fixed")

#, bs = "cr"
# bs = 'gp', k = 100, m = 2

