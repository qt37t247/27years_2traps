rm(list=ls())
pkgs <- c("mgcv", "knitr", "multcomp", "coin", "colorspace", "ggplot2", "performance",
          "data.table", "tidyverse", "vegan") 

inst <- pkgs %in% installed.packages()
if (any(inst)) install.packages(pkgs[!inst])
pkg_out <- lapply(pkgs, require, character.only = TRUE)

date()

sessionInfo()

METHOD <- "REML"  ### or "GCV.Cp"

data_org <-read.csv("master.csv")

# Centering the sampling weather variables on the mean value to allow easier interpretation in the presence of interaction terms
data_org$prec_c <- data_org$precipitation-mean(data_org$precipitation)
data_org$tmax_c <- data_org$temp_max-mean(data_org$temp_max)
data_org$tmean_c <- data_org$temp_mean-mean(data_org$temp_mean)
data_org$tmin_c <- data_org$temp_min-mean(data_org$temp_min)
data_org$trang_c <- data_org$temp_range-mean(data_org$temp_range)

# Test for collinearity 

mx <- glm(count ~ EVI + tmax_c + tmin_c + tmean_c + trang_c + precipitation +
            precipitation_anomaly + precipitation_anomaly_april +
            temp_max_anomaly  + temp_max_anomaly_april + 
            temp_min_anomaly  + temp_min_anomaly_april + 
            temp_mean_anomaly  + temp_mean_anomaly_april + 
            temp_range_anomaly  + temp_range_anomaly_april + light + 
            PM25 + NO2 + O3,
          family = gaussian, 
          data = data_org)

check_collinearity(mx)

# Check for Multicollinearity

# Low Correlation
# 
# Term  VIF           VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
# EVI 1.00 [    1.00,         ]     1.00      1.00     [    , 1.00]
# tmean_c 1.00 [    1.00,      Inf]     1.00      1.00     [0.00, 1.00]
# precipitation 1.00 [    1.00,         ]     1.00      1.00     [    , 1.00]
# precipitation_anomaly 1.00 [    1.00,         ]     1.00      1.00     [    , 1.00]
# precipitation_anomaly_april 1.00 [    1.00,         ]     1.00      1.00     [    , 1.00]
# temp_mean_anomaly 1.00 [    1.00,      Inf]     1.00      1.00     [0.00, 1.00]
# temp_mean_anomaly_april 1.00 [    1.00,      Inf]     1.00      1.00     [0.00, 1.00]
# light 1.00 [    1.00,         ]     1.00      1.00     [    , 1.00]
# PM25 1.00 [    1.00,      Inf]     1.00      1.00     [0.00, 1.00]
# NO2 1.00 [    1.00,      Inf]     1.00      1.00     [0.00, 1.00]
# O3 1.00 [    1.00,         ]     1.00      1.00     [    , 1.00]
# 
# High Correlation
# 
# Term      VIF           VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
# tmax_c 1.12e+05 [1.05e+05, 1.20e+05]   334.59  8.93e-06     [0.00, 0.00]
# tmin_c 1.12e+05 [1.05e+05, 1.20e+05]   334.59  8.93e-06     [0.00, 0.00]
# trang_c 1.12e+05 [1.05e+05, 1.20e+05]   334.59  8.93e-06     [0.00, 0.00]
# temp_max_anomaly 2.05e+07 [1.92e+07, 2.19e+07]  4524.95  4.88e-08     [0.00, 0.00]
# temp_max_anomaly_april 2.04e+07 [1.91e+07, 2.17e+07]  4512.16  4.91e-08     [0.00, 0.00]
# temp_min_anomaly 2.05e+07 [1.92e+07, 2.19e+07]  4524.95  4.88e-08     [0.00, 0.00]
# temp_min_anomaly_april 2.04e+07 [1.91e+07, 2.17e+07]  4512.16  4.91e-08     [0.00, 0.00]
# temp_range_anomaly 2.05e+07 [1.92e+07, 2.19e+07]  4524.95  4.88e-08     [0.00, 0.00]
# temp_range_anomaly_april 2.04e+07 [1.91e+07, 2.17e+07]  4512.16  4.91e-08     [0.00, 0.00]

# remove tamx, tmin, and associated anomaly


#################################################################################################
## Let's combine two traps and set types of traps as a variable!

dim(training <- subset(data_org, State == "DE"))
## [1] 2372   37
training$year <- as.double(training$year)

dim(validation <- subset(data_org, State == "NJ"))
## [1] 1170  37

data_org$lighttrap <- as.factor(data_org$lighttrap)

data_org$pherotrap <- as.factor(data_org$pherotrap)


# Model 1a, consider season (month) and space
summary(m1a <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp"),
                  family = gaussian(link = "log"), 
                  method = METHOD, 
                  data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(month, k = 5) + s(Longitude, 
#                                                       Latitude, k = 11, bs = "tp")
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.78212    0.08169   21.82   <2e-16 ***
#   lighttrapY  -1.10999    0.06375  -17.41   <2e-16 ***
#   pherotrapY   0.00000    0.00000     NaN      NaN    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.866  3.987 119.12  <2e-16 ***
#   s(Longitude,Latitude) 7.818  8.949  13.75  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 16/17
# R-sq.(adj) =  0.375   Deviance explained = 37.8%
# -REML = 8292.6  Scale est. = 62.56     n = 2372

AIC(m1a)
## [1] 16559.05


# Model 1b, consider season (vegetation) and space
summary(m1b <- gam(count ~ lighttrap + pherotrap + s(Longitude, Latitude, k=11, bs = "tp") + EVI,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(Longitude, Latitude, k = 11, 
#                                     bs = "tp") + EVI
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 7.437e-03  2.120e-01   0.035    0.972    
# lighttrapY  0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY  1.261e+00  1.055e-01  11.955  < 2e-16 ***
#   EVI         1.850e-04  3.666e-05   5.046 4.86e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df    F p-value    
# s(Longitude,Latitude) 7.327  8.618 7.63  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 13/14
# R-sq.(adj) =  0.127   Deviance explained = 13.1%
# -REML = 8685.4  Scale est. = 87.304    n = 2372

AIC(m1b)
## [1] 17346.23


# Model 1c, consider season (sampling weather) and space
summary(m1c <- gam(count ~ lighttrap + pherotrap + s(Longitude, Latitude, k=11, bs = "tp")  + 
                     prec_c*tmean_c*trang_c,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))


# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(Longitude, Latitude, k = 11, 
#                                     bs = "tp") + prec_c * tmean_c * trang_c
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             8.139e-01  9.858e-02   8.257 2.46e-16 ***
#   lighttrapY              0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY              1.302e+00  9.780e-02  13.314  < 2e-16 ***
#   prec_c                  4.062e-03  3.982e-04  10.201  < 2e-16 ***
#   tmean_c                 1.193e-01  1.297e-02   9.195  < 2e-16 ***
#   trang_c                 2.188e-01  3.333e-02   6.567 6.30e-11 ***
#   prec_c:tmean_c         -2.729e-04  2.026e-04  -1.347   0.1780    
# prec_c:trang_c          7.667e-04  3.956e-04   1.938   0.0527 .  
# tmean_c:trang_c         1.670e-02  1.158e-02   1.442   0.1495    
# prec_c:tmean_c:trang_c -1.772e-05  2.011e-04  -0.088   0.9298    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value    
# s(Longitude,Latitude) 8.009  9.135 11.88  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 19/20
# R-sq.(adj) =  0.198   Deviance explained = 20.4%
# -REML = 8614.5  Scale est. = 80.224    n = 2372

AIC(m1c)
# [1] 17152.24

# Model 2a, consider month, space, and vegetation
summary(m2a <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") + EVI,
                  family = gaussian(link = "log"), 
                  method = METHOD, 
                  data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(month, k = 5) + s(Longitude, 
#                                                       Latitude, k = 11, bs = "tp") + EVI
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.147e+00  1.917e-01   5.982 2.54e-09 ***
#   lighttrapY   0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY   1.095e+00  6.286e-02  17.413  < 2e-16 ***
#   EVI         -9.583e-05  3.424e-05  -2.799  0.00517 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.871  3.988 119.81  <2e-16 ***
#   s(Longitude,Latitude) 7.819  8.938  13.05  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 17/18
# R-sq.(adj) =  0.377   Deviance explained =   38%
# -REML = 8297.8  Scale est. = 62.356    n = 2372

AIC(m2a)
## [1] 16552.09


# Model 2b, consider month, space and sampling weather
summary(m2b <- gam(count ~ lighttrap + pherotrap + s(Longitude, Latitude, k=11, bs = "tp")  + 
                   s(month, k=5) + prec_c*tmean_c*trang_c,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(Longitude, Latitude, k = 11, 
#                                     bs = "tp") + s(month, k = 5) + prec_c * tmean_c * trang_c
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             6.349e-01  9.813e-02   6.470 1.19e-10 ***
#   lighttrapY              0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY              1.173e+00  6.425e-02  18.254  < 2e-16 ***
#   prec_c                  2.444e-03  3.286e-04   7.438 1.43e-13 ***
#   tmean_c                 8.077e-02  2.083e-02   3.877 0.000109 ***
#   trang_c                 1.046e-01  2.494e-02   4.193 2.86e-05 ***
#   prec_c:tmean_c          2.602e-05  1.872e-04   0.139 0.889473    
# prec_c:trang_c          1.038e-03  2.931e-04   3.542 0.000404 ***
#   tmean_c:trang_c         2.395e-02  1.088e-02   2.201 0.027828 *  
#   prec_c:tmean_c:trang_c -2.676e-04  1.613e-04  -1.659 0.097267 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(Longitude,Latitude) 8.209  9.245  18.87  <2e-16 ***
#   s(month)              3.857  3.986 113.94  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 23/24
# R-sq.(adj) =  0.403   Deviance explained = 40.8%
# -REML = 8276.4  Scale est. = 59.744    n = 2372

AIC(m2b)
# [1] 16457.21


# Model 2c, consider month, space, vegetation, and sampling weather
summary(m2c <- gam(count ~ lighttrap + pherotrap + s(Longitude, Latitude, k=11, bs = "tp")  + EVI +
                     s(month, k=5) + prec_c*tmean_c*trang_c,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(Longitude, Latitude, k = 11, 
#                                     bs = "tp") + EVI + s(month, k = 5) + prec_c * tmean_c * trang_c
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.112e+00  2.005e-01   5.547 3.23e-08 ***
#   lighttrapY              0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY              1.164e+00  6.371e-02  18.273  < 2e-16 ***
#   EVI                    -9.722e-05  3.627e-05  -2.681 0.007394 ** 
#   prec_c                  2.428e-03  3.268e-04   7.430 1.51e-13 ***
#   tmean_c                 7.550e-02  2.096e-02   3.602 0.000322 ***
#   trang_c                 8.978e-02  2.570e-02   3.493 0.000487 ***
#   prec_c:tmean_c          4.084e-05  1.871e-04   0.218 0.827181    
# prec_c:trang_c          1.061e-03  2.919e-04   3.635 0.000284 ***
#   tmean_c:trang_c         2.110e-02  1.094e-02   1.928 0.053934 .  
# prec_c:tmean_c:trang_c -2.776e-04  1.610e-04  -1.725 0.084647 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(Longitude,Latitude) 8.102  9.149  16.63  <2e-16 ***
#   s(month)              3.861  3.987 116.05  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 24/25
# R-sq.(adj) =  0.405   Deviance explained =   41%
# -REML = 8281.7  Scale est. = 59.576    n = 2372

AIC(m2c)
# [1] 16451.25


# Model 3a, consider month, space, sampling weather, vegetation, and year linear
summary(m3a <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + EVI + s(Longitude, Latitude, k=11, bs = "tp") + 
                     prec_c*tmean_c*trang_c + year,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(month, k = 5) + EVI + s(Longitude, 
#                                                             Latitude, k = 11, bs = "tp") + prec_c * tmean_c * trang_c + 
#   year
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            -8.965e+00  6.022e+00  -1.489 0.136680    
# lighttrapY              0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY              1.182e+00  6.472e-02  18.268  < 2e-16 ***
#   EVI                    -1.074e-04  3.670e-05  -2.926 0.003466 ** 
#   prec_c                  2.511e-03  3.289e-04   7.633  3.3e-14 ***
#   tmean_c                 7.425e-02  2.106e-02   3.526 0.000430 ***
#   trang_c                 1.004e-01  2.631e-02   3.817 0.000139 ***
#   year                    5.024e-03  3.006e-03   1.671 0.094834 .  
# prec_c:tmean_c          1.197e-05  1.881e-04   0.064 0.949254    
# prec_c:trang_c          1.045e-03  2.921e-04   3.577 0.000354 ***
#   tmean_c:trang_c         2.194e-02  1.103e-02   1.989 0.046852 *  
#   prec_c:tmean_c:trang_c -2.674e-04  1.613e-04  -1.658 0.097516 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.864  3.987 116.98  <2e-16 ***
#   s(Longitude,Latitude) 8.092  9.147  16.56  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 25/26
# R-sq.(adj) =  0.405   Deviance explained = 41.1%
# -REML = 8285.2  Scale est. = 59.528    n = 2372

AIC(m3a)
## [1] 16450.27


# Model 3b, consider month, space, sampling weather, vegetation, and year smoothed non-linear
summary(m3b <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + EVI + s(Longitude, Latitude, k=11, bs = "tp") +
                     prec_c*tmean_c*trang_c + s(year),
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))


# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(month, k = 5) + EVI + s(Longitude, 
#                                                             Latitude, k = 11, bs = "tp") + prec_c * tmean_c * trang_c + 
#   s(year)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.000e+00  0.000e+00     NaN      NaN    
# lighttrapY              2.419e-01  1.836e-01   1.318 0.187725    
# pherotrapY              1.390e+00  1.773e-01   7.843 6.61e-15 ***
#   EVI                     5.031e-05  3.321e-05   1.515 0.129962    
# prec_c                  1.658e-03  3.167e-04   5.236 1.79e-07 ***
#   tmean_c                 4.193e-02  2.293e-02   1.829 0.067561 .  
# trang_c                 1.004e-01  2.019e-02   4.972 7.10e-07 ***
#   prec_c:tmean_c         -1.053e-03  1.796e-04  -5.865 5.13e-09 ***
#   prec_c:trang_c          4.237e-04  2.847e-04   1.488 0.136912    
# tmean_c:trang_c         9.747e-03  8.764e-03   1.112 0.266190    
# prec_c:tmean_c:trang_c -4.994e-04  1.503e-04  -3.323 0.000905 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.907  3.994 179.90  <2e-16 ***
#   s(Longitude,Latitude) 8.433  9.375  22.85  <2e-16 ***
#   s(year)               8.784  8.984  72.91  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 33/34
# R-sq.(adj) =  0.529   Deviance explained = 53.4%
# -REML = 8031.9  Scale est. = 47.186    n = 2372

AIC(m3b)
## [1] 15907.24


# Model 3c, consider month, space, sampling weather, vegetation, and annual dynamics (winter anomaly, April anomaly, light, and air quality)
summary(m3c <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + EVI + s(Longitude, Latitude, k=11, bs = "tp") +
                     prec_c*tmean_c*trang_c +
                     precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     light + PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(month, k = 5) + EVI + s(Longitude, 
#                                                             Latitude, k = 11, bs = "tp") + prec_c * tmean_c * trang_c + 
#   precipitation_anomaly * temp_mean_anomaly * temp_range_anomaly + 
#   precipitation_anomaly_april * temp_mean_anomaly_april * temp_range_anomaly_april + 
#   light + PM25 * NO2 * O3
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                   0.000e+00  0.000e+00     NaN      NaN    
# lighttrapY                                                                    1.059e+01  9.143e+00   1.158 0.246861    
# pherotrapY                                                                    1.169e+01  9.143e+00   1.279 0.201027    
# EVI                                                                           1.373e-04  4.065e-05   3.377 0.000745 ***
#   prec_c                                                                        1.288e-03  3.382e-04   3.809 0.000143 ***
#   tmean_c                                                                       8.619e-02  2.421e-02   3.561 0.000378 ***
#   trang_c                                                                       9.954e-03  2.293e-02   0.434 0.664201    
# precipitation_anomaly                                                        -5.864e-03  1.609e-03  -3.645 0.000273 ***
#   temp_mean_anomaly                                                             1.110e-01  1.934e-02   5.738 1.08e-08 ***
#   temp_range_anomaly                                                           -3.749e-02  5.271e-02  -0.711 0.477063    
# precipitation_anomaly_april                                                   5.464e-03  1.319e-03   4.141 3.58e-05 ***
#   temp_mean_anomaly_april                                                       1.090e-01  2.022e-02   5.392 7.69e-08 ***
#   temp_range_anomaly_april                                                     -3.629e-01  4.516e-02  -8.037 1.45e-15 ***
#   light                                                                         5.129e-02  8.815e-03   5.819 6.74e-09 ***
#   PM25                                                                         -1.684e+00  9.748e-01  -1.727 0.084298 .  
# NO2                                                                          -7.151e+00  1.465e+00  -4.881 1.13e-06 ***
#   O3                                                                           -3.285e-01  2.212e-01  -1.485 0.137693    
# prec_c:tmean_c                                                               -4.579e-04  2.023e-04  -2.264 0.023688 *  
#   prec_c:trang_c                                                                2.941e-04  2.859e-04   1.028 0.303870    
# tmean_c:trang_c                                                               1.862e-03  1.040e-02   0.179 0.857898    
# precipitation_anomaly:temp_mean_anomaly                                       2.578e-03  7.400e-04   3.484 0.000502 ***
#   precipitation_anomaly:temp_range_anomaly                                     -6.408e-03  1.406e-03  -4.557 5.46e-06 ***
#   temp_mean_anomaly:temp_range_anomaly                                          1.113e-01  2.015e-02   5.525 3.67e-08 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april                          -2.051e-03  5.957e-04  -3.443 0.000584 ***
#   precipitation_anomaly_april:temp_range_anomaly_april                          4.491e-03  1.080e-03   4.159 3.31e-05 ***
#   temp_mean_anomaly_april:temp_range_anomaly_april                              3.939e-02  1.642e-02   2.399 0.016512 *  
#   PM25:NO2                                                                      6.468e-01  1.313e-01   4.925 9.01e-07 ***
#   PM25:O3                                                                       4.602e-02  2.383e-02   1.931 0.053543 .  
# NO2:O3                                                                        1.843e-01  3.577e-02   5.151 2.81e-07 ***
#   prec_c:tmean_c:trang_c                                                       -1.759e-04  1.588e-04  -1.108 0.267994    
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                    2.850e-03  4.378e-04   6.508 9.28e-11 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -2.409e-03  4.324e-04  -5.571 2.82e-08 ***
#   PM25:NO2:O3                                                                  -1.664e-02  3.227e-03  -5.155 2.75e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.883  3.991 174.99  <2e-16 ***
#   s(Longitude,Latitude) 8.053  9.137  16.79  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 46/47
# R-sq.(adj) =  0.545   Deviance explained = 55.3%
# -REML = 8056.9  Scale est. = 45.525    n = 2372

AIC(m3c)
## [1] 15834.35

# Model 3d, consider month, space, sampling weather, vegetation, and annual dynamics (winter anomaly, april anomaly, light/PM2.5, and air quality)
summary(m3d <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + EVI + s(Longitude, Latitude, k=11, bs = "tp") +
                     prec_c*tmean_c*trang_c +
                     precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     PM25*light + NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(month, k = 5) + EVI + s(Longitude, 
#                                                             Latitude, k = 11, bs = "tp") + prec_c * tmean_c * trang_c + 
#   precipitation_anomaly * temp_mean_anomaly * temp_range_anomaly + 
#   precipitation_anomaly_april * temp_mean_anomaly_april * temp_range_anomaly_april + 
#   PM25 * light + NO2 * O3
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                  -4.229e+00  2.203e+00  -1.920 0.055022 .  
# lighttrapY                                                                   -1.129e+00  5.100e-02 -22.136  < 2e-16 ***
#   pherotrapY                                                                    0.000e+00  0.000e+00     NaN      NaN    
# EVI                                                                           1.081e-04  4.291e-05   2.520 0.011800 *  
#   prec_c                                                                        1.003e-03  3.359e-04   2.986 0.002857 ** 
#   tmean_c                                                                       5.059e-02  2.421e-02   2.089 0.036805 *  
#   trang_c                                                                       4.346e-03  2.336e-02   0.186 0.852410    
# precipitation_anomaly                                                        -8.236e-03  1.617e-03  -5.092 3.83e-07 ***
#   temp_mean_anomaly                                                             6.067e-02  1.881e-02   3.225 0.001277 ** 
#   temp_range_anomaly                                                           -2.023e-01  5.053e-02  -4.004 6.42e-05 ***
#   precipitation_anomaly_april                                                   3.581e-03  1.222e-03   2.929 0.003433 ** 
#   temp_mean_anomaly_april                                                       7.101e-02  2.006e-02   3.539 0.000409 ***
#   temp_range_anomaly_april                                                     -3.128e-01  4.322e-02  -7.236 6.22e-13 ***
#   PM25                                                                          5.364e-02  2.374e-02   2.259 0.023964 *  
#   light                                                                         4.049e-02  2.381e-02   1.700 0.089201 .  
# NO2                                                                          -7.547e-01  2.854e-01  -2.645 0.008235 ** 
#   O3                                                                            1.109e-01  5.279e-02   2.101 0.035706 *  
#   prec_c:tmean_c                                                               -2.985e-04  1.986e-04  -1.503 0.133005    
# prec_c:trang_c                                                                2.120e-04  2.790e-04   0.760 0.447469    
# tmean_c:trang_c                                                               6.011e-03  1.022e-02   0.588 0.556407    
# precipitation_anomaly:temp_mean_anomaly                                       1.941e-03  6.927e-04   2.802 0.005119 ** 
#   precipitation_anomaly:temp_range_anomaly                                     -4.612e-03  1.389e-03  -3.320 0.000913 ***
#   temp_mean_anomaly:temp_range_anomaly                                          9.903e-02  2.011e-02   4.924 9.09e-07 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april                          -1.517e-03  5.596e-04  -2.711 0.006766 ** 
#   precipitation_anomaly_april:temp_range_anomaly_april                          3.856e-03  1.004e-03   3.842 0.000125 ***
#   temp_mean_anomaly_april:temp_range_anomaly_april                              4.514e-02  1.618e-02   2.790 0.005306 ** 
#   PM25:light                                                                    3.748e-04  3.423e-03   0.110 0.912802    
# NO2:O3                                                                        1.839e-02  6.960e-03   2.642 0.008285 ** 
#   prec_c:tmean_c:trang_c                                                       -6.219e-05  1.562e-04  -0.398 0.690635    
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                    2.861e-03  4.310e-04   6.638 3.95e-11 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -2.444e-03  4.057e-04  -6.024 1.97e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.878  3.990 167.53  <2e-16 ***
#   s(Longitude,Latitude) 8.270  9.312  15.51  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 44/45
# R-sq.(adj) =  0.531   Deviance explained = 53.9%
# -REML =   8086  Scale est. = 46.924    n = 2372

AIC(m3d)
## [1] 15908.54


# Model 4a, consider month, space, sampling weather, vegetation and annual dynamics (trap type interact with winter anomaly)
summary(m4a <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") + EVI +
                     prec_c*tmean_c*trang_c +
                     lighttrap*pherotrap*precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     light + PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

AIC(m4a)
## [1] 15762.12

# Model 4b, consider month, space, sampling weather, vegetation, and annual dynamics (trap type interact with april anomaly)
summary(m4b <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") + EVI +
                     prec_c*tmean_c*trang_c +
                     precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     lighttrap*pherotrap*precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     light + PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

AIC(m4b)
## [1] 15716.73

# Model 4c, consider month, space, sampling weather, vegetation, and annual dynamics (trap type interact with light)
summary(m4c <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") + EVI +
                     prec_c*tmean_c*trang_c +
                     precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     lighttrap*pherotrap*light + PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

AIC(m4c)
## [1] 15807.18

# Model 4d, consider month, space, sampling weather, vegetation, and annual dynamics (trap type interact with air quality)
summary(m4d <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") + EVI +
                     prec_c*tmean_c*trang_c +
                     precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     light + lighttrap*pherotrap*PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ s(month, k = 5) + s(Longitude, Latitude, k = 11, bs = "tp") + 
#   EVI + prec_c * tmean_c * trang_c + precipitation_anomaly * 
#   temp_mean_anomaly * temp_range_anomaly + precipitation_anomaly_april * 
#   temp_mean_anomaly_april * temp_range_anomaly_april + light + 
#   lighttrap * pherotrap * PM25 * NO2 * O3
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                   3.492e+01  3.310e+01   1.055 0.291577    
# EVI                                                                           1.280e-04  3.673e-05   3.485 0.000501 ***
#   prec_c                                                                        1.381e-03  3.018e-04   4.575 5.02e-06 ***
#   tmean_c                                                                       5.332e-02  2.145e-02   2.486 0.013006 *  
#   trang_c                                                                       1.518e-02  2.037e-02   0.745 0.456122    
# precipitation_anomaly                                                        -8.050e-03  1.435e-03  -5.611 2.25e-08 ***
#   temp_mean_anomaly                                                             1.058e-01  1.784e-02   5.931 3.47e-09 ***
#   temp_range_anomaly                                                           -1.624e-03  4.733e-02  -0.034 0.972627    
# precipitation_anomaly_april                                                   4.603e-03  1.187e-03   3.877 0.000109 ***
#   temp_mean_anomaly_april                                                       1.170e-01  1.839e-02   6.362 2.40e-10 ***
#   temp_range_anomaly_april                                                     -4.309e-01  3.963e-02 -10.875  < 2e-16 ***
#   light                                                                         5.414e-02  8.016e-03   6.755 1.81e-11 ***
#   lighttrapY                                                                    0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY                                                                   -3.597e+01  3.511e+01  -1.024 0.305735    
# PM25                                                                         -1.570e-01  1.050e+00  -0.150 0.881162    
# NO2                                                                          -1.136e+01  3.344e+00  -3.398 0.000691 ***
#   O3                                                                           -1.670e-02  2.350e-01  -0.071 0.943354    
# prec_c:tmean_c                                                               -6.990e-04  1.798e-04  -3.887 0.000104 ***
#   prec_c:trang_c                                                                4.855e-04  2.536e-04   1.915 0.055666 .  
# tmean_c:trang_c                                                              -4.964e-03  9.430e-03  -0.526 0.598649    
# precipitation_anomaly:temp_mean_anomaly                                       2.853e-03  6.734e-04   4.237 2.36e-05 ***
#   precipitation_anomaly:temp_range_anomaly                                     -7.019e-03  1.286e-03  -5.456 5.38e-08 ***
#   temp_mean_anomaly:temp_range_anomaly                                          9.880e-02  1.853e-02   5.332 1.06e-07 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april                          -1.871e-03  5.399e-04  -3.466 0.000538 ***
#   precipitation_anomaly_april:temp_range_anomaly_april                          4.335e-03  9.447e-04   4.588 4.70e-06 ***
#   temp_mean_anomaly_april:temp_range_anomaly_april                              5.997e-02  1.427e-02   4.201 2.76e-05 ***
#   lighttrapY:pherotrapY                                                         0.000e+00  0.000e+00     NaN      NaN    
# lighttrapY:PM25                                                              -7.739e+00  3.493e+00  -2.216 0.026819 *  
#   pherotrapY:PM25                                                               0.000e+00  0.000e+00     NaN      NaN    
# lighttrapY:NO2                                                                0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY:NO2                                                                6.674e+00  3.800e+00   1.756 0.079162 .  
# PM25:NO2                                                                      1.485e+00  3.201e-01   4.640 3.68e-06 ***
#   lighttrapY:O3                                                                -1.211e+00  8.626e-01  -1.404 0.160374    
# pherotrapY:O3                                                                 0.000e+00  0.000e+00     NaN      NaN    
# PM25:O3                                                                       2.225e-01  8.118e-02   2.741 0.006174 ** 
#   NO2:O3                                                                        1.244e-01  3.938e-02   3.158 0.001608 ** 
#   prec_c:tmean_c:trang_c                                                       -3.811e-04  1.393e-04  -2.735 0.006285 ** 
#   precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                    3.553e-03  4.034e-04   8.807  < 2e-16 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -2.405e-03  3.800e-04  -6.327 2.99e-10 ***
#   lighttrapY:pherotrapY:PM25                                                    0.000e+00  0.000e+00     NaN      NaN    
# lighttrapY:pherotrapY:NO2                                                     0.000e+00  0.000e+00     NaN      NaN    
# lighttrapY:PM25:NO2                                                           0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY:PM25:NO2                                                          -1.137e+00  3.635e-01  -3.126 0.001792 ** 
#   lighttrapY:pherotrapY:O3                                                      0.000e+00  0.000e+00     NaN      NaN    
# lighttrapY:PM25:O3                                                            0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY:PM25:O3                                                           -2.136e-01  8.621e-02  -2.478 0.013279 *  
#   lighttrapY:NO2:O3                                                             1.994e-01  9.390e-02   2.124 0.033800 *  
#   pherotrapY:NO2:O3                                                             0.000e+00  0.000e+00     NaN      NaN    
# PM25:NO2:O3                                                                  -9.407e-03  3.696e-03  -2.545 0.010985 *  
#   lighttrapY:pherotrapY:PM25:NO2                                                0.000e+00  0.000e+00     NaN      NaN    
# lighttrapY:pherotrapY:PM25:O3                                                 0.000e+00  0.000e+00     NaN      NaN    
# lighttrapY:pherotrapY:NO2:O3                                                  0.000e+00  0.000e+00     NaN      NaN    
# lighttrapY:PM25:NO2:O3                                                       -3.057e-02  9.037e-03  -3.383 0.000728 ***
#   pherotrapY:PM25:NO2:O3                                                        0.000e+00  0.000e+00     NaN      NaN    
# lighttrapY:pherotrapY:PM25:NO2:O3                                             0.000e+00  0.000e+00     NaN      NaN    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.903  3.994 219.46  <2e-16 ***
#   s(Longitude,Latitude) 8.600  9.535  24.52  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 53/69
# R-sq.(adj) =  0.612   Deviance explained = 61.9%
# -REML = 7853.8  Scale est. = 38.936    n = 2372

AIC(m4d)
## [1] 15470.96


# Model 4e, consider month, space, sampling weather*traps, vegetation, and annual dynamics
summary(m4e <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") + EVI +
                     lighttrap*pherotrap*prec_c*tmean_c*trang_c +
                     precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     light + PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

AIC(m4e)
# [1] 15732.81

# Model 4f, consider month, space, sampling weather, vegetation, and year smoothed non-linear*traps
summary(m4f <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") + EVI +
                     lighttrap*pherotrap*year + prec_c*tmean_c*trang_c,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

AIC(m4f)
# [1] 16340.44


# Model 5, consider month, space, sampling weather, and annual dynamics 
# (light trap interact with light, and pheromone trap interact with air quality)
summary(m5 <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") + EVI +
                     prec_c*tmean_c*trang_c +
                     precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     lighttrap*light + pherotrap*PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ s(month, k = 5) + s(Longitude, Latitude, k = 11, bs = "tp") + 
#   EVI + prec_c * tmean_c * trang_c + precipitation_anomaly * 
#   temp_mean_anomaly * temp_range_anomaly + precipitation_anomaly_april * 
#   temp_mean_anomaly_april * temp_range_anomaly_april + lighttrap * 
#   light + pherotrap * PM25 * NO2 * O3
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                  -1.632e+00  9.740e+00  -0.168 0.866978    
# EVI                                                                           1.217e-04  3.685e-05   3.301 0.000979 ***
#   prec_c                                                                        1.384e-03  3.016e-04   4.589 4.69e-06 ***
#   tmean_c                                                                       5.504e-02  2.146e-02   2.565 0.010394 *  
#   trang_c                                                                       1.329e-02  2.039e-02   0.652 0.514686    
# precipitation_anomaly                                                        -8.154e-03  1.437e-03  -5.676 1.55e-08 ***
#   temp_mean_anomaly                                                             1.039e-01  1.786e-02   5.819 6.74e-09 ***
#   temp_range_anomaly                                                           -3.573e-03  4.737e-02  -0.075 0.939889    
# precipitation_anomaly_april                                                   4.846e-03  1.185e-03   4.091 4.45e-05 ***
#   temp_mean_anomaly_april                                                       1.198e-01  1.841e-02   6.509 9.22e-11 ***
#   temp_range_anomaly_april                                                     -4.257e-01  3.967e-02 -10.732  < 2e-16 ***
#   lighttrapY                                                                    4.249e+01  3.445e+01   1.233 0.217631    
# light                                                                         5.395e-02  8.007e-03   6.738 2.02e-11 ***
#   pherotrapY                                                                    0.000e+00  0.000e+00     NaN      NaN    
# PM25                                                                         -8.196e+00  3.206e+00  -2.556 0.010644 *  
#   NO2                                                                          -1.224e+01  3.287e+00  -3.723 0.000202 ***
#   O3                                                                           -1.357e+00  7.954e-01  -1.706 0.088098 .  
# prec_c:tmean_c                                                               -7.018e-04  1.799e-04  -3.902 9.80e-05 ***
#   prec_c:trang_c                                                                4.770e-04  2.534e-04   1.882 0.059914 .  
# tmean_c:trang_c                                                              -5.488e-03  9.435e-03  -0.582 0.560845    
# precipitation_anomaly:temp_mean_anomaly                                       2.881e-03  6.735e-04   4.278 1.97e-05 ***
#   precipitation_anomaly:temp_range_anomaly                                     -6.988e-03  1.286e-03  -5.435 6.06e-08 ***
#   temp_mean_anomaly:temp_range_anomaly                                          1.013e-01  1.855e-02   5.461 5.23e-08 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april                          -2.001e-03  5.401e-04  -3.706 0.000216 ***
#   precipitation_anomaly_april:temp_range_anomaly_april                          4.518e-03  9.492e-04   4.760 2.05e-06 ***
#   temp_mean_anomaly_april:temp_range_anomaly_april                              6.078e-02  1.425e-02   4.265 2.08e-05 ***
#   lighttrapY:light                                                             -5.416e-02  2.732e-02  -1.983 0.047537 *  
#   pherotrapY:PM25                                                               8.141e+00  3.425e+00   2.377 0.017541 *  
#   pherotrapY:NO2                                                                7.581e+00  3.758e+00   2.017 0.043784 *  
#   PM25:NO2                                                                      1.539e+00  3.136e-01   4.908 9.86e-07 ***
#   pherotrapY:O3                                                                 1.356e+00  8.457e-01   1.603 0.109120    
# PM25:O3                                                                       2.288e-01  7.927e-02   2.886 0.003934 ** 
#   NO2:O3                                                                        3.445e-01  8.155e-02   4.224 2.49e-05 ***
#   prec_c:tmean_c:trang_c                                                       -3.785e-04  1.392e-04  -2.720 0.006586 ** 
#   precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                    3.606e-03  4.038e-04   8.929  < 2e-16 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -2.412e-03  3.819e-04  -6.316 3.20e-10 ***
#   pherotrapY:PM25:NO2                                                          -1.197e+00  3.586e-01  -3.339 0.000854 ***
#   pherotrapY:PM25:O3                                                           -2.225e-01  8.448e-02  -2.633 0.008514 ** 
#   pherotrapY:NO2:O3                                                            -2.209e-01  9.280e-02  -2.380 0.017379 *  
#   PM25:NO2:O3                                                                  -4.124e-02  7.827e-03  -5.269 1.50e-07 ***
#   pherotrapY:PM25:NO2:O3                                                        3.199e-02  8.906e-03   3.592 0.000335 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.904  3.994 220.22  <2e-16 ***
#   s(Longitude,Latitude) 8.640  9.557  24.81  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 54/55
# R-sq.(adj) =  0.612   Deviance explained =   62%
# -REML = 7895.8  Scale est. = 38.875    n = 2372

AIC(m5)
## [1] 15468.27


gam.check(m5)
# Method: REML   Optimizer: outer newton
# full convergence after 10 iterations.
# Gradient range [-2.193083e-05,2.026216e-05]
# (score 7895.771 & scale 38.87482).
# Hessian positive definite, eigenvalue range [1.399176,1164.011].
# Model rank =  54 / 55 
# 
# Basis dimension (k) checking results. Low p-value (k-index<1) may
# indicate that k is too low, especially if edf is close to k'.
# 
#                          k'   edf k-index p-value    
# s(month)               4.00  3.90    0.93  <2e-16 ***
#   s(Longitude,Latitude) 10.00  8.64    0.96   0.075 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

concurvity(m5)
# para  s(month) s(Longitude,Latitude)
# worst       1 0.9042641             0.8199393
# observed    1 0.2895030             0.1354724
# estimate    1 0.2816761             0.3508649

# Results of the final generalized additive mixed model with annual dynamics
cf <- coef(m5)
sd <- sqrt(diag(vcov(m5)))
pval <- pnorm(-abs(cf / sd)) * 2
M <- matrix(c(-1, 1), byrow = TRUE, nrow = length(cf), ncol = 2)
ci <- cf + M * qnorm(.975) * sd
tb <- cbind(cf * 100, sd * 100, cf / sd, pval, exp(cf), exp(ci))
tb <- tb[names(cf) %in% attr(m5$pterms, "term.labels"),]
colnames(tb) <- c("Est*100", "SD*100", "z-Val", "p-Val", "Exp(Est)", "Lwr", "Upr")
formatC(tb, digits = 3, format = "f")

# Est*100     SD*100    z-Val     p-Val  
# EVI                                                                          "0.012"     "0.004"   "3.301"   "0.001"
# prec_c                                                                       "0.138"     "0.030"   "4.589"   "0.000"
# tmean_c                                                                      "5.504"     "2.146"   "2.565"   "0.010"
# trang_c                                                                      "1.329"     "2.039"   "0.652"   "0.515"
# precipitation_anomaly                                                        "-0.815"    "0.144"   "-5.676"  "0.000"
# temp_mean_anomaly                                                            "10.391"    "1.786"   "5.819"   "0.000"
# temp_range_anomaly                                                           "-0.357"    "4.737"   "-0.075"  "0.940"
# precipitation_anomaly_april                                                  "0.485"     "0.118"   "4.091"   "0.000"
# temp_mean_anomaly_april                                                      "11.982"    "1.841"   "6.509"   "0.000"
# temp_range_anomaly_april                                                     "-42.570"   "3.967"   "-10.732" "0.000"
# light                                                                        "5.395"     "0.801"   "6.738"   "0.000"
# PM25                                                                         "-819.638"  "320.642" "-2.556"  "0.011"
# NO2                                                                          "-1223.821" "328.746" "-3.723"  "0.000"
# O3                                                                           "-135.716"  "79.541"  "-1.706"  "0.088"
# prec_c:tmean_c                                                               "-0.070"    "0.018"   "-3.902"  "0.000"
# prec_c:trang_c                                                               "0.048"     "0.025"   "1.882"   "0.060"
# tmean_c:trang_c                                                              "-0.549"    "0.943"   "-0.582"  "0.561"
# precipitation_anomaly:temp_mean_anomaly                                      "0.288"     "0.067"   "4.278"   "0.000"
# precipitation_anomaly:temp_range_anomaly                                     "-0.699"    "0.129"   "-5.435"  "0.000"
# temp_mean_anomaly:temp_range_anomaly                                         "10.128"    "1.855"   "5.461"   "0.000"
# precipitation_anomaly_april:temp_mean_anomaly_april                          "-0.200"    "0.054"   "-3.706"  "0.000"
# precipitation_anomaly_april:temp_range_anomaly_april                         "0.452"     "0.095"   "4.760"   "0.000"
# temp_mean_anomaly_april:temp_range_anomaly_april                             "6.078"     "1.425"   "4.265"   "0.000"
# PM25:NO2                                                                     "153.926"   "31.364"  "4.908"   "0.000"
# PM25:O3                                                                      "22.881"    "7.927"   "2.886"   "0.004"
# NO2:O3                                                                       "34.449"    "8.155"   "4.224"   "0.000"
# prec_c:tmean_c:trang_c                                                       "-0.038"    "0.014"   "-2.720"  "0.007"
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   "0.361"     "0.040"   "8.929"   "0.000"
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april "-0.241"    "0.038"   "-6.316"  "0.000"
# PM25:NO2:O3                                                                  "-4.124"    "0.783"   "-5.269"  "0.000"
# Exp(Est) Lwr     Upr    
# EVI                                                                          "1.000"  "1.000" "1.000"
# prec_c                                                                       "1.001"  "1.001" "1.002"
# tmean_c                                                                      "1.057"  "1.013" "1.102"
# trang_c                                                                      "1.013"  "0.974" "1.055"
# precipitation_anomaly                                                        "0.992"  "0.989" "0.995"
# temp_mean_anomaly                                                            "1.110"  "1.071" "1.149"
# temp_range_anomaly                                                           "0.996"  "0.908" "1.093"
# precipitation_anomaly_april                                                  "1.005"  "1.003" "1.007"
# temp_mean_anomaly_april                                                      "1.127"  "1.087" "1.169"
# temp_range_anomaly_april                                                     "0.653"  "0.604" "0.706"
# light                                                                        "1.055"  "1.039" "1.072"
# PM25                                                                         "0.000"  "0.000" "0.148"
# NO2                                                                          "0.000"  "0.000" "0.003"
# O3                                                                           "0.257"  "0.054" "1.224"
# prec_c:tmean_c                                                               "0.999"  "0.999" "1.000"
# prec_c:trang_c                                                               "1.000"  "1.000" "1.001"
# tmean_c:trang_c                                                              "0.995"  "0.976" "1.013"
# precipitation_anomaly:temp_mean_anomaly                                      "1.003"  "1.002" "1.004"
# precipitation_anomaly:temp_range_anomaly                                     "0.993"  "0.991" "0.996"
# temp_mean_anomaly:temp_range_anomaly                                         "1.107"  "1.067" "1.148"
# precipitation_anomaly_april:temp_mean_anomaly_april                          "0.998"  "0.997" "0.999"
# precipitation_anomaly_april:temp_range_anomaly_april                         "1.005"  "1.003" "1.006"
# temp_mean_anomaly_april:temp_range_anomaly_april                             "1.063"  "1.033" "1.093"
# PM25:NO2                                                                     "4.661"  "2.521" "8.619"
# PM25:O3                                                                      "1.257"  "1.076" "1.468"
# NO2:O3                                                                       "1.411"  "1.203" "1.656"
# prec_c:tmean_c:trang_c                                                       "1.000"  "0.999" "1.000"
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   "1.004"  "1.003" "1.004"
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april "0.998"  "0.997" "0.998"
# PM25:NO2:O3                                                                  "0.960"  "0.945" "0.974"


# Assessing if residuals show still a temporal pattern and estimating residual time effect
training$pred <- predict(m5, type = "link")
m <- gam(count ~ offset(pred) + s(year),
         family = gaussian(link = "log"), 
         method = METHOD,
         data = training)

summary(m)
# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ offset(pred) + s(year)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -0.004068   0.013602  -0.299    0.765
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value
# s(year) 1.848  2.273 0.522   0.547
# 
# R-sq.(adj) =  0.621   Deviance explained = 0.112%
# -REML = 7686.7  Scale est. = 38.017    n = 2372

# Multiplicative change of the effects of year, confidence interval of the temporal effect
plot(m, trans = exp, xlab = "Year", ylab = "Multiplicative change")
abline(h = 1, lty = 3, lwd = 2, col = "lightgray")

# Assessing stability of results: Add year as random effect
m5_RE <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") + EVI +
               s(year, bs = "re") + ### add temporal random effect
                prec_c*tmean_c*trang_c +
                precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                lighttrap*light + pherotrap*PM25*NO2*O3,
             family = gaussian(link = "log"), 
             method = METHOD, 
             data = training)

AIC(m5_RE)
## [1] 15468.34

summary(m5_RE)

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ s(month, k = 5) + s(Longitude, Latitude, k = 11, bs = "tp") + 
#   EVI + s(year, bs = "re") + prec_c * tmean_c * trang_c + precipitation_anomaly * 
#   temp_mean_anomaly * temp_range_anomaly + precipitation_anomaly_april * 
#   temp_mean_anomaly_april * temp_range_anomaly_april + lighttrap * 
#   light + pherotrap * PM25 * NO2 * O3
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                  -1.632e+00  9.740e+00  -0.168 0.866976    
# EVI                                                                           1.217e-04  3.685e-05   3.301 0.000979 ***
#   prec_c                                                                        1.384e-03  3.016e-04   4.589 4.69e-06 ***
#   tmean_c                                                                       5.504e-02  2.146e-02   2.565 0.010394 *  
#   trang_c                                                                       1.329e-02  2.039e-02   0.652 0.514686    
# precipitation_anomaly                                                        -8.154e-03  1.437e-03  -5.676 1.55e-08 ***
#   temp_mean_anomaly                                                             1.039e-01  1.786e-02   5.819 6.74e-09 ***
#   temp_range_anomaly                                                           -3.573e-03  4.737e-02  -0.075 0.939889    
# precipitation_anomaly_april                                                   4.846e-03  1.185e-03   4.091 4.45e-05 ***
#   temp_mean_anomaly_april                                                       1.198e-01  1.841e-02   6.509 9.22e-11 ***
#   temp_range_anomaly_april                                                     -4.257e-01  3.967e-02 -10.732  < 2e-16 ***
#   lighttrapY                                                                    4.249e+01  3.445e+01   1.233 0.217631    
# light                                                                         5.395e-02  8.007e-03   6.738 2.02e-11 ***
#   pherotrapY                                                                    0.000e+00  0.000e+00     NaN      NaN    
# PM25                                                                         -8.196e+00  3.206e+00  -2.556 0.010644 *  
#   NO2                                                                          -1.224e+01  3.287e+00  -3.723 0.000202 ***
#   O3                                                                           -1.357e+00  7.954e-01  -1.706 0.088098 .  
# prec_c:tmean_c                                                               -7.018e-04  1.799e-04  -3.902 9.80e-05 ***
#   prec_c:trang_c                                                                4.770e-04  2.534e-04   1.882 0.059914 .  
# tmean_c:trang_c                                                              -5.488e-03  9.435e-03  -0.582 0.560845    
# precipitation_anomaly:temp_mean_anomaly                                       2.881e-03  6.735e-04   4.278 1.97e-05 ***
#   precipitation_anomaly:temp_range_anomaly                                     -6.988e-03  1.286e-03  -5.435 6.06e-08 ***
#   temp_mean_anomaly:temp_range_anomaly                                          1.013e-01  1.855e-02   5.461 5.23e-08 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april                          -2.001e-03  5.401e-04  -3.706 0.000216 ***
#   precipitation_anomaly_april:temp_range_anomaly_april                          4.518e-03  9.492e-04   4.760 2.05e-06 ***
#   temp_mean_anomaly_april:temp_range_anomaly_april                              6.078e-02  1.425e-02   4.265 2.08e-05 ***
#   lighttrapY:light                                                             -5.416e-02  2.732e-02  -1.983 0.047537 *  
#   pherotrapY:PM25                                                               8.141e+00  3.425e+00   2.377 0.017541 *  
#   pherotrapY:NO2                                                                7.581e+00  3.758e+00   2.017 0.043784 *  
#   PM25:NO2                                                                      1.539e+00  3.136e-01   4.908 9.86e-07 ***
#   pherotrapY:O3                                                                 1.356e+00  8.457e-01   1.603 0.109120    
# PM25:O3                                                                       2.288e-01  7.927e-02   2.886 0.003934 ** 
#   NO2:O3                                                                        3.445e-01  8.155e-02   4.224 2.49e-05 ***
#   prec_c:tmean_c:trang_c                                                       -3.785e-04  1.392e-04  -2.720 0.006586 ** 
#   precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                    3.606e-03  4.038e-04   8.929  < 2e-16 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -2.412e-03  3.819e-04  -6.316 3.20e-10 ***
#   pherotrapY:PM25:NO2                                                          -1.197e+00  3.586e-01  -3.339 0.000854 ***
#   pherotrapY:PM25:O3                                                           -2.225e-01  8.448e-02  -2.633 0.008514 ** 
#   pherotrapY:NO2:O3                                                            -2.209e-01  9.280e-02  -2.380 0.017379 *  
#   PM25:NO2:O3                                                                  -4.124e-02  7.827e-03  -5.269 1.50e-07 ***
#   pherotrapY:PM25:NO2:O3                                                        3.199e-02  8.906e-03   3.592 0.000335 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F  p-value    
# s(month)              3.904e+00  3.994 220.22  < 2e-16 ***
#   s(Longitude,Latitude) 8.640e+00  9.557  24.81  < 2e-16 ***
#   s(year)               2.610e-07  1.000   0.00 3.87e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 55/56
# R-sq.(adj) =  0.612   Deviance explained =   62%
# -REML = 7895.8  Scale est. = 38.875    n = 2372

# Compute linear predictor describes log(count)
lp <- predict(m5, newdata = training, type = "link")
plot(lp, log(training$count), xlab = "Linear predictor",
     ylab = "log(count)")
abline(a = 0, b = 1)

## Validation

# Replacing year with annual environmental dynamics 
X <- predict(m5, newdata = validation, type = "terms")
colnames(X)

# [1] "EVI"                                                                         
# [2] "prec_c"                                                                      
# [3] "tmean_c"                                                                     
# [4] "trang_c"                                                                     
# [5] "precipitation_anomaly"                                                       
# [6] "temp_mean_anomaly"                                                           
# [7] "temp_range_anomaly"                                                          
# [8] "precipitation_anomaly_april"                                                 
# [9] "temp_mean_anomaly_april"                                                     
# [10] "temp_range_anomaly_april"                                                    
# [11] "lighttrap"                                                                   
# [12] "light"                                                                       
# [13] "pherotrap"                                                                   
# [14] "PM25"                                                                        
# [15] "NO2"                                                                         
# [16] "O3"                                                                          
# [17] "prec_c:tmean_c"                                                              
# [18] "prec_c:trang_c"                                                              
# [19] "tmean_c:trang_c"                                                             
# [20] "precipitation_anomaly:temp_mean_anomaly"                                     
# [21] "precipitation_anomaly:temp_range_anomaly"                                    
# [22] "temp_mean_anomaly:temp_range_anomaly"                                        
# [23] "precipitation_anomaly_april:temp_mean_anomaly_april"                         
# [24] "precipitation_anomaly_april:temp_range_anomaly_april"                        
# [25] "temp_mean_anomaly_april:temp_range_anomaly_april"                            
# [26] "lighttrap:light"                                                             
# [27] "pherotrap:PM25"                                                              
# [28] "pherotrap:NO2"                                                               
# [29] "PM25:NO2"                                                                    
# [30] "pherotrap:O3"                                                                
# [31] "PM25:O3"                                                                     
# [32] "NO2:O3"                                                                      
# [33] "prec_c:tmean_c:trang_c"                                                      
# [34] "precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly"                  
# [35] "precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april"
# [36] "pherotrap:PM25:NO2"                                                          
# [37] "pherotrap:PM25:O3"                                                           
# [38] "pherotrap:NO2:O3"                                                            
# [39] "PM25:NO2:O3"                                                                 
# [40] "pherotrap:PM25:NO2:O3"                                                       
# [41] "s(month)"                                                                    
# [42] "s(Longitude,Latitude)"     

# Annual dynamics predictors
fvar <- c(1:42)
validation$Farm <- factor(validation$Farm)
validation$logct <- log(validation$count)
validation$ad <- rowSums(X[, fvar])
pvalue(coin::spearman_test(ad ~ logct | Farm, data = validation))
# [1] <2.220446e-16

colfunc <- colorRampPalette(c("blue", "yellow", "brown4"))
sp <- cor.test(~ ad + logct, data = validation, method = "spearman")$estimate

pdf("pl_both_annual_validate.pdf", height=7, width=7)
plot(count ~ ad, log = "y", ylab = "log(Count per day)",
     xlab = "Linear combination annual conditions",
     data = validation, col = colfunc(27)[validation$year-1997], pch = 19)

legend(-3, 200, legend = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"), box.col = "white",
       col = colfunc(27)[16:27], pch = 19)
text(0, 150, as.expression(bquote(rho == .(round(sp, 3)))))
text(0, 100, "p < 0.001")
dev.off()
