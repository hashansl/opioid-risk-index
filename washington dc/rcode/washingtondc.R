library(sf)
library(mgcv)
library("PerformanceAnalytics")


dcRaw <- st_read("~/MacBook/git/opioid-risk-index/washington dc/data/processed data/SVI2020 WashingtonDC counties with death rate/SVI2020_WashingtonDC_counties_with_death_rate.shp")


# Using names() function
column_names <- names(dcRaw)
print(column_names)


my_data <- dcRaw[, c("EP_POV150","EP_UNEMP","EP_HBURD","EP_NOHSDP","EP_UNINSUR","EP_AGE65","EP_AGE17","EP_DISABL","EP_SNGPNT","EP_LIMENG","EP_MINRTY","EP_MUNIT","EP_MOBILE","EP_CROWD","EP_NOVEH","EP_GROUPQ","od_deaths_")]
my_data <- subset(my_data, select = -geometry)

column_names <- names(my_data)
print(column_names)

chart.Correlation(my_data, histogram=TRUE, pch=19)





#EP_POV150+EP_UNEMP+EP_HBURD+EP_NOHSDP+EP_UNINSUR+EP_AGE65+EP_AGE17+EP_DISABL+EP_SNGPNT+EP_LIMENG+EP_MINRTY+EP_MUNIT+EP_MOBILE+EP_CROWD+EP_NOVEH+EP_GROUPQ
#trying the linear model first

mymod = lm(od_deaths_ ~EP_POV150+EP_UNEMP+EP_HBURD+EP_NOHSDP+EP_UNINSUR+EP_AGE65+EP_AGE17+EP_DISABL+EP_SNGPNT+EP_LIMENG+EP_MINRTY+EP_MUNIT+EP_MOBILE+EP_CROWD+EP_NOVEH+EP_GROUPQ , data = dcRaw)
summary(mymod)

mod_lm1 = gam(od_deaths_ ~EP_POV150+EP_UNEMP+EP_HBURD+EP_NOHSDP+EP_UNINSUR+EP_AGE65+EP_AGE17+EP_DISABL+EP_SNGPNT+EP_LIMENG+EP_MINRTY+EP_MUNIT+EP_MOBILE+EP_CROWD+EP_NOVEH+EP_GROUPQ , data = dcRaw)
summary(mod_lm1)

#As far as the generalized additive model goes, we can approach things in a similar manner as before, and now and look for nonlinear effects for each feature24.

mod_gam2 = gam(od_deaths_ ~s(EP_POV150)+s(EP_UNEMP)+s(EP_HBURD)+s(EP_NOHSDP)+s(EP_UNINSUR)+s(EP_AGE65)+s(EP_AGE17)+s(EP_DISABL)+s(EP_SNGPNT)+s(EP_LIMENG)+s(EP_MINRTY)+s(EP_MUNIT)+s(EP_MOBILE)+s(EP_CROWD)+s(EP_NOVEH)+s(EP_GROUPQ) , data = dcRaw)
summary(mod_gam2)

#plotting
plot(ggeffects::ggpredict(mod_gam2), facets = TRUE)
gratia::draw(mod_gam2)
