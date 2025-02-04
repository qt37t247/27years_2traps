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
# Term  VIF           VIF 95% CI Increased SE Tolerance Tolerance 95% CI
# EVI 1.00 [    1.00,         ]         1.00      1.00     [    , 1.00]
# tmean_c 1.00 [    1.00,      Inf]         1.00      1.00     [0.00, 1.00]
# precipitation 1.00 [    1.00,         ]         1.00      1.00     [    , 1.00]
# precipitation_anomaly 1.00                              1.00      1.00                 
# precipitation_anomaly_april 1.00 [    1.00,      Inf]         1.00      1.00     [0.00, 1.00]
# temp_mean_anomaly 1.00 [    1.00,      Inf]         1.00      1.00     [0.00, 1.00]
# temp_mean_anomaly_april 1.00 [    1.00,      Inf]         1.00      1.00     [0.00, 1.00]
# light 1.00 [    1.00,      Inf]         1.00      1.00     [0.00, 1.00]
# PM25 1.00 [    1.00,         ]         1.00      1.00     [    , 1.00]
# NO2 1.00 [    1.00,         ]         1.00      1.00     [    , 1.00]
# O3 1.00 [    1.00,      Inf]         1.00      1.00     [0.00, 1.00]
# 
# High Correlation
# 
# Term      VIF           VIF 95% CI Increased SE Tolerance Tolerance 95% CI
# tmax_c 2.19e+05 [2.05e+05, 2.34e+05]       468.37  4.56e-06     [0.00, 0.00]
# tmin_c 2.19e+05 [2.05e+05, 2.34e+05]       468.37  4.56e-06     [0.00, 0.00]
# trang_c 2.19e+05 [2.05e+05, 2.34e+05]       468.37  4.56e-06     [0.00, 0.00]
# temp_max_anomaly 2.22e+07 [2.08e+07, 2.37e+07]      4708.85  4.51e-08     [0.00, 0.00]
# temp_max_anomaly_april 2.08e+07 [1.95e+07, 2.22e+07]      4558.49  4.81e-08     [0.00, 0.00]
# temp_min_anomaly 2.22e+07 [2.08e+07, 2.37e+07]      4708.85  4.51e-08     [0.00, 0.00]
# temp_min_anomaly_april 2.08e+07 [1.95e+07, 2.22e+07]      4558.49  4.81e-08     [0.00, 0.00]
# temp_range_anomaly 2.22e+07 [2.08e+07, 2.37e+07]      4708.85  4.51e-08     [0.00, 0.00]
# temp_range_anomaly_april 2.08e+07 [1.95e+07, 2.22e+07]      4558.49  4.81e-08     [0.00, 0.00]

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


# Model 1, consider season and space
summary(m1 <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp"),
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

AIC(m1)
## [1] 16559.05

# Model 2, consider season, space, and habitat
summary(m2 <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") + EVI,
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
# (Intercept)  0.000e+00  0.000e+00     NaN      NaN    
# lighttrapY   7.248e-01  1.168e-01   6.205 6.43e-10 ***
#   pherotrapY   1.831e+00  1.017e-01  18.005  < 2e-16 ***
#   EVI         -1.178e-05  1.455e-05  -0.809    0.418    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.866  3.987 116.06  <2e-16 ***
#   s(Longitude,Latitude) 7.875  8.995  13.63  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 17/18
# R-sq.(adj) =  0.375   Deviance explained = 37.8%
# -REML = 8302.5  Scale est. = 62.56     n = 2372

AIC(m2)
## [1] 16560.12

# Model 3a, consider season, space, habitat, and year linear
summary(m3a <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     EVI + year,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(month, k = 5) + s(Longitude, 
#                                                       Latitude, k = 11, bs = "tp") + EVI + year
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.569e+00  5.737e+00   0.273    0.785    
# lighttrapY   0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY   1.104e+00  6.350e-02  17.388   <2e-16 ***
#   EVI         -1.120e-05  1.504e-05  -0.745    0.457    
# year        -4.199e-04  2.859e-03  -0.147    0.883    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.866  3.987 115.69  <2e-16 ***
#   s(Longitude,Latitude) 7.866  8.987  13.54  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 18/19
# R-sq.(adj) =  0.375   Deviance explained = 37.8%
# -REML = 8307.4  Scale est. = 62.585    n = 2372

AIC(m3a)
## [1] 16561.91


# Model 3b, consider season, space, habitat, and year smooth
summary(m3b <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     EVI + s(year),
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(month, k = 5) + s(Longitude, 
#                                                       Latitude, k = 11, bs = "tp") + EVI + s(year)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.000e+00  0.000e+00     NaN      NaN    
# lighttrapY   5.550e-01  9.807e-02   5.660  1.7e-08 ***
#   pherotrapY   1.721e+00  8.489e-02  20.268  < 2e-16 ***
#   EVI         -1.378e-05  1.240e-05  -1.111    0.267    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.909  3.994 194.15  <2e-16 ***
#   s(Longitude,Latitude) 8.253  9.242  19.36  <2e-16 ***
#   s(year)               8.805  8.988  86.53  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 26/27
# R-sq.(adj) =  0.516   Deviance explained = 52.1%
# -REML = 8025.2  Scale est. = 48.433    n = 2372

AIC(m3b)
## [1] 15961.9

concurvity(m3b)
# para   s(month) s(Longitude,Latitude)     s(year)
# worst       1 0.30069459            0.15136484 0.153900071
# observed  NaN 0.16428651            0.01472172 0.002683057
# estimate    1 0.06982027            0.02136999 0.105611259


# Model 4, consider season, space, habitat, year, and sampling weather
summary(m4 <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     EVI + s(year) + prec_c*tmean_c*trang_c,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(month, k = 5) + s(Longitude, 
#                                                       Latitude, k = 11, bs = "tp") + EVI + s(year) + prec_c * tmean_c * 
#   trang_c
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             5.036e-01  9.930e-02   5.072 4.25e-07 ***
#   lighttrapY              0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY              1.150e+00  5.229e-02  21.990  < 2e-16 ***
#   EVI                    -3.157e-06  1.252e-05  -0.252  0.80097    
# prec_c                  1.673e-03  3.157e-04   5.299 1.27e-07 ***
#   tmean_c                 4.194e-02  2.288e-02   1.833  0.06692 .  
# trang_c                 8.865e-02  1.902e-02   4.661 3.33e-06 ***
#   prec_c:tmean_c         -1.029e-03  1.790e-04  -5.749 1.01e-08 ***
#   prec_c:trang_c          4.101e-04  2.843e-04   1.442  0.14931    
# tmean_c:trang_c         9.641e-03  8.811e-03   1.094  0.27396    
# prec_c:tmean_c:trang_c -4.914e-04  1.502e-04  -3.273  0.00108 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.909  3.994 179.97  <2e-16 ***
#   s(Longitude,Latitude) 8.385  9.336  22.64  <2e-16 ***
#   s(year)               8.770  8.982  72.00  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 33/34
# R-sq.(adj) =  0.528   Deviance explained = 53.4%
# -REML =   8034  Scale est. = 47.247    n = 2372

AIC(m4)
## [1] 15910.09

# Model 5a, consider season, space, habitat, annual dynamics (winter anomaly, april anomaly, light, and air quality), and sampling weather
summary(m5a <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                    EVI + prec_c*tmean_c*trang_c +
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
#   count ~ lighttrap + pherotrap + s(month, k = 5) + s(Longitude, 
#                                                       Latitude, k = 11, bs = "tp") + EVI + prec_c * tmean_c * trang_c + 
#   precipitation_anomaly * temp_mean_anomaly * temp_range_anomaly + 
#   precipitation_anomaly_april * temp_mean_anomaly_april * temp_range_anomaly_april + 
#   light + PM25 * NO2 * O3
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                                                   0.000e+00  0.000e+00     NaN      NaN
# lighttrapY                                                                   -4.802e+00  1.142e+01  -0.421  0.67409
# pherotrapY                                                                   -3.684e+00  1.142e+01  -0.323  0.74696
# EVI                                                                          -6.316e-06  1.301e-05  -0.485  0.62743
# prec_c                                                                        8.375e-04  3.367e-04   2.488  0.01293
# tmean_c                                                                       1.751e-02  2.356e-02   0.743  0.45733
# trang_c                                                                      -9.561e-03  2.178e-02  -0.439  0.66068
# precipitation_anomaly                                                        -8.570e-03  1.633e-03  -5.247 1.68e-07
# temp_mean_anomaly                                                             5.202e-02  1.899e-02   2.739  0.00621
# temp_range_anomaly                                                           -1.391e-01  5.026e-02  -2.768  0.00568
# precipitation_anomaly_april                                                   3.898e-03  1.296e-03   3.008  0.00266
# temp_mean_anomaly_april                                                       5.609e-02  2.052e-02   2.734  0.00631
# temp_range_anomaly_april                                                     -1.648e-01  4.408e-02  -3.738  0.00019
# light                                                                         3.069e-02  2.099e-02   1.462  0.14378
# PM25                                                                         -6.970e-01  1.134e+00  -0.615  0.53892
# NO2                                                                          -2.881e+00  1.485e+00  -1.940  0.05252
# O3                                                                            7.692e-02  2.743e-01   0.280  0.77915
# prec_c:tmean_c                                                               -2.170e-04  2.010e-04  -1.080  0.28043
# prec_c:trang_c                                                                4.402e-04  2.790e-04   1.578  0.11466
# tmean_c:trang_c                                                               1.298e-02  1.038e-02   1.251  0.21088
# precipitation_anomaly:temp_mean_anomaly                                       4.154e-03  7.435e-04   5.587 2.58e-08
# precipitation_anomaly:temp_range_anomaly                                     -6.826e-03  1.449e-03  -4.711 2.61e-06
# temp_mean_anomaly:temp_range_anomaly                                          1.377e-01  1.976e-02   6.968 4.17e-12
# precipitation_anomaly_april:temp_mean_anomaly_april                          -1.132e-03  5.955e-04  -1.901  0.05740
# precipitation_anomaly_april:temp_range_anomaly_april                          2.702e-03  1.028e-03   2.627  0.00867
# temp_mean_anomaly_april:temp_range_anomaly_april                              9.576e-03  1.653e-02   0.579  0.56240
# PM25:NO2                                                                      3.225e-01  1.321e-01   2.441  0.01471
# PM25:O3                                                                       2.176e-02  2.746e-02   0.792  0.42817
# NO2:O3                                                                        7.693e-02  3.582e-02   2.147  0.03186
# prec_c:tmean_c:trang_c                                                       -2.032e-04  1.581e-04  -1.285  0.19884
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                    2.272e-03  4.537e-04   5.008 5.92e-07
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -1.854e-03  4.245e-04  -4.368 1.31e-05
# PM25:NO2:O3                                                                  -8.510e-03  3.206e-03  -2.654  0.00800
# 
# (Intercept)                                                                     
# lighttrapY                                                                      
# pherotrapY                                                                      
# EVI                                                                             
# prec_c                                                                       *  
#   tmean_c                                                                         
# trang_c                                                                         
# precipitation_anomaly                                                        ***
#   temp_mean_anomaly                                                            ** 
#   temp_range_anomaly                                                           ** 
#   precipitation_anomaly_april                                                  ** 
#   temp_mean_anomaly_april                                                      ** 
#   temp_range_anomaly_april                                                     ***
#   light                                                                           
# PM25                                                                            
# NO2                                                                          .  
# O3                                                                              
# prec_c:tmean_c                                                                  
# prec_c:trang_c                                                                  
# tmean_c:trang_c                                                                 
# precipitation_anomaly:temp_mean_anomaly                                      ***
#   precipitation_anomaly:temp_range_anomaly                                     ***
#   temp_mean_anomaly:temp_range_anomaly                                         ***
#   precipitation_anomaly_april:temp_mean_anomaly_april                          .  
# precipitation_anomaly_april:temp_range_anomaly_april                         ** 
#   temp_mean_anomaly_april:temp_range_anomaly_april                                
# PM25:NO2                                                                     *  
#   PM25:O3                                                                         
# NO2:O3                                                                       *  
#   prec_c:tmean_c:trang_c                                                          
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april ***
#   PM25:NO2:O3                                                                  ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.891  3.992 169.97  <2e-16 ***
#   s(Longitude,Latitude) 8.559  9.477  17.86  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 46/47
# R-sq.(adj) =  0.534   Deviance explained = 54.2%
# -REML = 8087.7  Scale est. = 46.655    n = 2372

AIC(m5a)
## [1] 15893.16

# Model 5b, consider season, space, habitat, annual dynamics (winter anomaly, april anomaly, light/PM2.5, and air quality), and sampling weather
summary(m5b <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     EVI + prec_c*tmean_c*trang_c +
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
#   count ~ lighttrap + pherotrap + s(month, k = 5) + s(Longitude, 
#                                                       Latitude, k = 11, bs = "tp") + EVI + prec_c * tmean_c * trang_c + 
#   precipitation_anomaly * temp_mean_anomaly * temp_range_anomaly + 
#   precipitation_anomaly_april * temp_mean_anomaly_april * temp_range_anomaly_april + 
#   PM25 * light + NO2 * O3
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                                                   0.000e+00  0.000e+00     NaN      NaN
# lighttrapY                                                                   -8.834e+00  2.342e+00  -3.771 0.000166
# pherotrapY                                                                   -7.707e+00  2.342e+00  -3.291 0.001013
# EVI                                                                          -1.599e-05  1.330e-05  -1.202 0.229526
# prec_c                                                                        9.026e-04  3.407e-04   2.649 0.008124
# tmean_c                                                                       3.188e-02  2.451e-02   1.301 0.193510
# trang_c                                                                      -2.690e-03  2.224e-02  -0.121 0.903748
# precipitation_anomaly                                                        -7.520e-03  1.628e-03  -4.619 4.08e-06
# temp_mean_anomaly                                                             1.477e-02  1.863e-02   0.793 0.427871
# temp_range_anomaly                                                           -2.077e-01  4.829e-02  -4.301 1.77e-05
# precipitation_anomaly_april                                                   3.980e-03  1.238e-03   3.214 0.001327
# temp_mean_anomaly_april                                                       5.108e-02  2.094e-02   2.440 0.014776
# temp_range_anomaly_april                                                     -1.874e-01  4.289e-02  -4.370 1.30e-05
# PM25                                                                          9.994e-02  2.151e-02   4.646 3.57e-06
# light                                                                         3.365e-01  7.352e-02   4.577 4.98e-06
# NO2                                                                          -8.113e-03  2.773e-01  -0.029 0.976663
# O3                                                                            2.021e-01  5.551e-02   3.641 0.000278
# prec_c:tmean_c                                                               -2.030e-04  2.004e-04  -1.013 0.311131
# prec_c:trang_c                                                                2.859e-04  2.761e-04   1.035 0.300651
# tmean_c:trang_c                                                               1.225e-02  1.025e-02   1.195 0.232080
# precipitation_anomaly:temp_mean_anomaly                                       2.049e-03  6.860e-04   2.987 0.002850
# precipitation_anomaly:temp_range_anomaly                                     -3.808e-03  1.434e-03  -2.656 0.007956
# temp_mean_anomaly:temp_range_anomaly                                          1.138e-01  1.881e-02   6.052 1.66e-09
# precipitation_anomaly_april:temp_mean_anomaly_april                          -1.658e-03  5.610e-04  -2.954 0.003163
# precipitation_anomaly_april:temp_range_anomaly_april                          3.173e-03  1.027e-03   3.089 0.002030
# temp_mean_anomaly_april:temp_range_anomaly_april                              1.946e-02  1.645e-02   1.183 0.237102
# PM25:light                                                                   -2.701e-02  6.936e-03  -3.894 0.000101
# NO2:O3                                                                       -2.938e-04  6.759e-03  -0.043 0.965334
# prec_c:tmean_c:trang_c                                                       -9.427e-05  1.572e-04  -0.600 0.548747
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                    2.007e-03  4.481e-04   4.478 7.91e-06
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -2.140e-03  4.305e-04  -4.971 7.14e-07
# 
# (Intercept)                                                                     
# lighttrapY                                                                   ***
#   pherotrapY                                                                   ** 
#   EVI                                                                             
# prec_c                                                                       ** 
#   tmean_c                                                                         
# trang_c                                                                         
# precipitation_anomaly                                                        ***
#   temp_mean_anomaly                                                               
# temp_range_anomaly                                                           ***
#   precipitation_anomaly_april                                                  ** 
#   temp_mean_anomaly_april                                                      *  
#   temp_range_anomaly_april                                                     ***
#   PM25                                                                         ***
#   light                                                                        ***
#   NO2                                                                             
# O3                                                                           ***
#   prec_c:tmean_c                                                                  
# prec_c:trang_c                                                                  
# tmean_c:trang_c                                                                 
# precipitation_anomaly:temp_mean_anomaly                                      ** 
#   precipitation_anomaly:temp_range_anomaly                                     ** 
#   temp_mean_anomaly:temp_range_anomaly                                         ***
#   precipitation_anomaly_april:temp_mean_anomaly_april                          ** 
#   precipitation_anomaly_april:temp_range_anomaly_april                         ** 
#   temp_mean_anomaly_april:temp_range_anomaly_april                                
# PM25:light                                                                   ***
#   NO2:O3                                                                          
# prec_c:tmean_c:trang_c                                                          
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.887  3.991 167.78  <2e-16 ***
#   s(Longitude,Latitude) 8.654  9.533  17.46  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 44/45
# R-sq.(adj) =  0.527   Deviance explained = 53.5%
# -REML = 8097.8  Scale est. = 47.36     n = 2372

AIC(m5b)
## [1] 15926.85


# Model 6a, consider season, space, habitat, annual dynamics (trap type interact with winter anomaly, april anomaly, light, and air quality), and sampling weather
summary(m6a <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     EVI + prec_c*tmean_c*trang_c +
                     lighttrap*pherotrap*precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     light + PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

AIC(m6a)
## [1] 15821.93

# Model 6b, consider season, space, habitat, annual dynamics (winter anomaly, trap type interact with april anomaly, light, and air quality), and sampling weather
summary(m6b <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     EVI + prec_c*tmean_c*trang_c +
                     precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     lighttrap*pherotrap*precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     light + PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

AIC(m6b)
## [1] 15784.71

# Model 6c, consider season, space, habitat, annual dynamics (winter anomaly, april anomaly, trap type interact with light, and air quality), and sampling weather
summary(m6c <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     EVI + prec_c*tmean_c*trang_c +
                     precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     lighttrap*pherotrap*light + PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

AIC(m6c)
## [1] 15894.6

# Model 6d, consider season, space, habitat, annual dynamics (winter anomaly, april anomaly, light, and trap type interact with air quality), and sampling weather
summary(m6d <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     EVI + prec_c*tmean_c*trang_c +
                     precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     light + lighttrap*pherotrap*PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

AIC(m6d)
## [1] 15569.04

# Model 6e, consider season, space, habitat, annual dynamics (winter anomaly, april anomaly, light trap interact with light, and pheromone trap interact with air quality), and sampling weather
summary(m6e <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     EVI + prec_c*tmean_c*trang_c +
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
# (Intercept)                                                                  -7.824e+01  2.832e+01  -2.763 0.005772
# EVI                                                                          -3.382e-06  1.147e-05  -0.295 0.768066
# prec_c                                                                        1.025e-03  3.021e-04   3.395 0.000699
# tmean_c                                                                      -3.684e-03  2.118e-02  -0.174 0.861887
# trang_c                                                                       1.954e-03  1.953e-02   0.100 0.920303
# precipitation_anomaly                                                        -9.086e-03  1.455e-03  -6.245 5.03e-10
# temp_mean_anomaly                                                             3.805e-02  1.731e-02   2.199 0.027987
# temp_range_anomaly                                                           -1.149e-01  4.527e-02  -2.539 0.011189
# precipitation_anomaly_april                                                   3.552e-03  1.212e-03   2.931 0.003407
# temp_mean_anomaly_april                                                       6.025e-02  1.874e-02   3.215 0.001322
# temp_range_anomaly_april                                                     -1.914e-01  3.913e-02  -4.891 1.07e-06
# lighttrapY                                                                    0.000e+00  0.000e+00     NaN      NaN
# light                                                                         4.903e-02  2.010e-02   2.439 0.014822
# pherotrapY                                                                    7.359e+01  3.120e+01   2.359 0.018403
# PM25                                                                          4.052e+00  2.697e+00   1.502 0.133170
# NO2                                                                          -1.642e+00  3.102e+00  -0.529 0.596622
# O3                                                                            1.676e+00  6.891e-01   2.432 0.015075
# prec_c:tmean_c                                                               -4.397e-04  1.801e-04  -2.442 0.014700
# prec_c:trang_c                                                                6.250e-04  2.523e-04   2.478 0.013294
# tmean_c:trang_c                                                               1.297e-02  9.459e-03   1.372 0.170299
# precipitation_anomaly:temp_mean_anomaly                                       3.734e-03  6.863e-04   5.441 5.85e-08
# precipitation_anomaly:temp_range_anomaly                                     -5.896e-03  1.335e-03  -4.416 1.05e-05
# temp_mean_anomaly:temp_range_anomaly                                          1.385e-01  1.813e-02   7.637 3.22e-14
# precipitation_anomaly_april:temp_mean_anomaly_april                          -1.178e-03  5.516e-04  -2.137 0.032733
# precipitation_anomaly_april:temp_range_anomaly_april                          2.710e-03  9.358e-04   2.897 0.003809
# temp_mean_anomaly_april:temp_range_anomaly_april                              2.196e-02  1.462e-02   1.502 0.133320
# lighttrapY:light                                                             -5.248e-02  3.064e-02  -1.713 0.086857
# pherotrapY:PM25                                                              -4.572e+00  2.994e+00  -1.527 0.126863
# pherotrapY:NO2                                                               -3.142e-01  3.578e+00  -0.088 0.930025
# PM25:NO2                                                                      4.806e-01  2.862e-01   1.679 0.093314
# pherotrapY:O3                                                                -1.571e+00  7.573e-01  -2.075 0.038134
# PM25:O3                                                                      -7.813e-02  6.616e-02  -1.181 0.237727
# NO2:O3                                                                        6.781e-02  7.605e-02   0.892 0.372652
# prec_c:tmean_c:trang_c                                                       -4.110e-04  1.409e-04  -2.917 0.003571
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                    2.497e-03  4.164e-04   5.997 2.32e-09
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -1.730e-03  3.834e-04  -4.513 6.72e-06
# pherotrapY:PM25:NO2                                                          -2.722e-01  3.303e-01  -0.824 0.410087
# pherotrapY:PM25:O3                                                            9.527e-02  7.316e-02   1.302 0.192972
# pherotrapY:NO2:O3                                                            -1.387e-02  8.733e-02  -0.159 0.873831
# PM25:NO2:O3                                                                  -1.405e-02  7.062e-03  -1.990 0.046735
# pherotrapY:PM25:NO2:O3                                                        8.313e-03  8.108e-03   1.025 0.305327
# 
# (Intercept)                                                                  ** 
#   EVI                                                                             
# prec_c                                                                       ***
#   tmean_c                                                                         
# trang_c                                                                         
# precipitation_anomaly                                                        ***
#   temp_mean_anomaly                                                            *  
#   temp_range_anomaly                                                           *  
#   precipitation_anomaly_april                                                  ** 
#   temp_mean_anomaly_april                                                      ** 
#   temp_range_anomaly_april                                                     ***
#   lighttrapY                                                                      
# light                                                                        *  
#   pherotrapY                                                                   *  
#   PM25                                                                            
# NO2                                                                             
# O3                                                                           *  
#   prec_c:tmean_c                                                               *  
#   prec_c:trang_c                                                               *  
#   tmean_c:trang_c                                                                 
# precipitation_anomaly:temp_mean_anomaly                                      ***
#   precipitation_anomaly:temp_range_anomaly                                     ***
#   temp_mean_anomaly:temp_range_anomaly                                         ***
#   precipitation_anomaly_april:temp_mean_anomaly_april                          *  
#   precipitation_anomaly_april:temp_range_anomaly_april                         ** 
#   temp_mean_anomaly_april:temp_range_anomaly_april                                
# lighttrapY:light                                                             .  
# pherotrapY:PM25                                                                 
# pherotrapY:NO2                                                                  
# PM25:NO2                                                                     .  
# pherotrapY:O3                                                                *  
#   PM25:O3                                                                         
# NO2:O3                                                                          
# prec_c:tmean_c:trang_c                                                       ** 
#   precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april ***
#   pherotrapY:PM25:NO2                                                             
# pherotrapY:PM25:O3                                                              
# pherotrapY:NO2:O3                                                               
# PM25:NO2:O3                                                                  *  
#   pherotrapY:PM25:NO2:O3                                                          
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.907  3.994 210.15  <2e-16 ***
#   s(Longitude,Latitude) 9.129  9.770  25.92  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 54/55
# R-sq.(adj) =  0.595   Deviance explained = 60.4%
# -REML = 7947.8  Scale est. = 40.514    n = 2372

AIC(m6e)
## [1] 15567.35

# Model 6f, consider season, space, habitat, annual dynamics (winter anomaly, light trap interact with April anomaly, light, and pheromone trap interact with air quality), and sampling weather
summary(m6f <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     EVI + prec_c*tmean_c*trang_c +
                     precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     lighttrap*precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     light + pherotrap*PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ s(month, k = 5) + s(Longitude, Latitude, k = 11, bs = "tp") + 
#   EVI + prec_c * tmean_c * trang_c + precipitation_anomaly * 
#   temp_mean_anomaly * temp_range_anomaly + lighttrap * precipitation_anomaly_april * 
#   temp_mean_anomaly_april * temp_range_anomaly_april + light + 
#   pherotrap * PM25 * NO2 * O3
# 
# Parametric coefficients:
#   Estimate Std. Error t value
# (Intercept)                                                                             -6.112e+00  1.224e+01  -0.499
# EVI                                                                                     -2.221e-06  1.120e-05  -0.198
# prec_c                                                                                   9.139e-04  2.952e-04   3.096
# tmean_c                                                                                  3.456e-03  2.056e-02   0.168
# trang_c                                                                                 -9.723e-03  1.907e-02  -0.510
# precipitation_anomaly                                                                   -9.951e-03  1.442e-03  -6.900
# temp_mean_anomaly                                                                        3.865e-02  1.706e-02   2.265
# temp_range_anomaly                                                                      -6.944e-02  4.464e-02  -1.556
# lighttrapY                                                                               1.266e+01  2.991e+01   0.423
# precipitation_anomaly_april                                                              1.861e-03  1.294e-03   1.438
# temp_mean_anomaly_april                                                                  4.347e-02  1.950e-02   2.230
# temp_range_anomaly_april                                                                -1.398e-01  4.304e-02  -3.248
# light                                                                                    3.312e-02  1.851e-02   1.789
# pherotrapY                                                                               0.000e+00  0.000e+00     NaN
# PM25                                                                                    -2.237e+00  2.534e+00  -0.883
# NO2                                                                                     -8.841e+00  2.916e+00  -3.032
# O3                                                                                      -3.616e-01  6.644e-01  -0.544
# prec_c:tmean_c                                                                          -2.684e-04  1.758e-04  -1.527
# prec_c:trang_c                                                                           4.151e-04  2.468e-04   1.682
# tmean_c:trang_c                                                                          1.643e-02  9.291e-03   1.769
# precipitation_anomaly:temp_mean_anomaly                                                  4.515e-03  6.774e-04   6.665
# precipitation_anomaly:temp_range_anomaly                                                -7.677e-03  1.313e-03  -5.847
# temp_mean_anomaly:temp_range_anomaly                                                     1.324e-01  1.780e-02   7.438
# lighttrapY:precipitation_anomaly_april                                                   6.764e-03  2.533e-03   2.670
# lighttrapY:temp_mean_anomaly_april                                                       1.386e-01  6.081e-02   2.279
# precipitation_anomaly_april:temp_mean_anomaly_april                                     -3.514e-04  5.687e-04  -0.618
# lighttrapY:temp_range_anomaly_april                                                     -2.886e-01  8.699e-02  -3.318
# precipitation_anomaly_april:temp_range_anomaly_april                                     2.158e-03  1.081e-03   1.997
# temp_mean_anomaly_april:temp_range_anomaly_april                                        -1.291e-02  1.570e-02  -0.822
# pherotrapY:PM25                                                                          1.525e+00  2.791e+00   0.546
# pherotrapY:NO2                                                                           6.986e+00  3.366e+00   2.075
# PM25:NO2                                                                                 1.040e+00  2.644e-01   3.935
# pherotrapY:O3                                                                            4.989e-01  7.259e-01   0.687
# PM25:O3                                                                                  7.259e-02  6.212e-02   1.169
# NO2:O3                                                                                   2.414e-01  7.155e-02   3.374
# prec_c:tmean_c:trang_c                                                                  -3.413e-04  1.386e-04  -2.462
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                               2.922e-03  4.064e-04   7.189
# lighttrapY:precipitation_anomaly_april:temp_mean_anomaly_april                          -2.833e-03  1.536e-03  -1.844
# lighttrapY:precipitation_anomaly_april:temp_range_anomaly_april                          3.702e-03  2.022e-03   1.831
# lighttrapY:temp_mean_anomaly_april:temp_range_anomaly_april                              2.800e-01  3.822e-02   7.326
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april            -1.347e-03  4.404e-04  -3.059
# pherotrapY:PM25:NO2                                                                     -8.193e-01  3.043e-01  -2.692
# pherotrapY:PM25:O3                                                                      -5.072e-02  6.813e-02  -0.745
# pherotrapY:NO2:O3                                                                       -1.892e-01  8.214e-02  -2.304
# PM25:NO2:O3                                                                             -2.760e-02  6.531e-03  -4.227
# lighttrapY:precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -1.598e-03  7.857e-04  -2.034
# pherotrapY:PM25:NO2:O3                                                                   2.154e-02  7.474e-03   2.883
# Pr(>|t|)    
# (Intercept)                                                                             0.617551    
# EVI                                                                                     0.842842    
# prec_c                                                                                  0.001984 ** 
#   tmean_c                                                                                 0.866555    
# trang_c                                                                                 0.610251    
# precipitation_anomaly                                                                   6.67e-12 ***
#   temp_mean_anomaly                                                                       0.023588 *  
#   temp_range_anomaly                                                                      0.119948    
# lighttrapY                                                                              0.672168    
# precipitation_anomaly_april                                                             0.150478    
# temp_mean_anomaly_april                                                                 0.025862 *  
#   temp_range_anomaly_april                                                                0.001179 ** 
#   light                                                                                   0.073700 .  
# pherotrapY                                                                                   NaN    
# PM25                                                                                    0.377443    
# NO2                                                                                     0.002458 ** 
#   O3                                                                                      0.586273    
# prec_c:tmean_c                                                                          0.126832    
# prec_c:trang_c                                                                          0.092774 .  
# tmean_c:trang_c                                                                         0.077065 .  
# precipitation_anomaly:temp_mean_anomaly                                                 3.30e-11 ***
#   precipitation_anomaly:temp_range_anomaly                                                5.70e-09 ***
#   temp_mean_anomaly:temp_range_anomaly                                                    1.43e-13 ***
#   lighttrapY:precipitation_anomaly_april                                                  0.007630 ** 
#   lighttrapY:temp_mean_anomaly_april                                                      0.022759 *  
#   precipitation_anomaly_april:temp_mean_anomaly_april                                     0.536676    
# lighttrapY:temp_range_anomaly_april                                                     0.000921 ***
#   precipitation_anomaly_april:temp_range_anomaly_april                                    0.045974 *  
#   temp_mean_anomaly_april:temp_range_anomaly_april                                        0.410951    
# pherotrapY:PM25                                                                         0.584877    
# pherotrapY:NO2                                                                          0.038055 *  
#   PM25:NO2                                                                                8.58e-05 ***
#   pherotrapY:O3                                                                           0.491972    
# PM25:O3                                                                                 0.242688    
# NO2:O3                                                                                  0.000752 ***
#   prec_c:tmean_c:trang_c                                                                  0.013884 *  
#   precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                              8.78e-13 ***
#   lighttrapY:precipitation_anomaly_april:temp_mean_anomaly_april                          0.065321 .  
# lighttrapY:precipitation_anomaly_april:temp_range_anomaly_april                         0.067252 .  
# lighttrapY:temp_mean_anomaly_april:temp_range_anomaly_april                             3.25e-13 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april            0.002245 ** 
#   pherotrapY:PM25:NO2                                                                     0.007152 ** 
#   pherotrapY:PM25:O3                                                                      0.456617    
# pherotrapY:NO2:O3                                                                       0.021330 *  
#   PM25:NO2:O3                                                                             2.46e-05 ***
#   lighttrapY:precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april 0.042021 *  
#   pherotrapY:PM25:NO2:O3                                                                  0.003980 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value    
# s(month)              3.91  3.994 223.6  <2e-16 ***
#   s(Longitude,Latitude) 9.13  9.778  23.5  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 60/61
# R-sq.(adj) =  0.612   Deviance explained = 62.2%
# -REML = 7922.7  Scale est. = 38.807    n = 2372

AIC(m6f)
## [1] 15471.05



gam.check(m6e)
# Method: REML   Optimizer: outer newton
# full convergence after 10 iterations.
# Gradient range [-7.084506e-05,6.449209e-05]
# (score 7947.805 & scale 40.51441).
# Hessian positive definite, eigenvalue range [1.386586,1164.011].
# Model rank =  54 / 55 
# 
# Basis dimension (k) checking results. Low p-value (k-index<1) may
# indicate that k is too low, especially if edf is close to k'.
# 
#                          k'   edf k-index p-value    
# s(month)               4.00  3.91    0.92  <2e-16 ***
#   s(Longitude,Latitude) 10.00  9.13    0.97    0.12    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

concurvity(m6e)
# para  s(month) s(Longitude,Latitude)
# worst       1 0.8884223             0.7986553
# observed    1 0.4216920             0.1679514
# estimate    1 0.1996115             0.3991925

# Results of the final generalized additive mixed model with annual dynamics
cf <- coef(m6e)
sd <- sqrt(diag(vcov(m6e)))
pval <- pnorm(-abs(cf / sd)) * 2
M <- matrix(c(-1, 1), byrow = TRUE, nrow = length(cf), ncol = 2)
ci <- cf + M * qnorm(.975) * sd
tb <- cbind(cf * 100, sd * 100, cf / sd, pval, exp(cf), exp(ci))
tb <- tb[names(cf) %in% c("typePheromone", attr(m6d$pterms, "term.labels")),]
colnames(tb) <- c("Est*100", "SD*100", "z-Val", "p-Val", "Exp(Est)", "Lwr", "Upr")
formatC(tb, digits = 3, format = "f")

# Est*100    SD*100    z-Val    p-Val  
# EVI                                                                          "-0.000"   "0.001"   "-0.295" "0.768"
# prec_c                                                                       "0.103"    "0.030"   "3.395"  "0.001"
# tmean_c                                                                      "-0.368"   "2.118"   "-0.174" "0.862"
# trang_c                                                                      "0.195"    "1.953"   "0.100"  "0.920"
# precipitation_anomaly                                                        "-0.909"   "0.145"   "-6.245" "0.000"
# temp_mean_anomaly                                                            "3.805"    "1.731"   "2.199"  "0.028"
# temp_range_anomaly                                                           "-11.494"  "4.527"   "-2.539" "0.011"
# precipitation_anomaly_april                                                  "0.355"    "0.121"   "2.931"  "0.003"
# temp_mean_anomaly_april                                                      "6.025"    "1.874"   "3.215"  "0.001"
# temp_range_anomaly_april                                                     "-19.136"  "3.913"   "-4.891" "0.000"
# light                                                                        "4.903"    "2.010"   "2.439"  "0.015"
# PM25                                                                         "405.185"  "269.719" "1.502"  "0.133"
# NO2                                                                          "-164.187" "310.174" "-0.529" "0.597"
# O3                                                                           "167.605"  "68.906"  "2.432"  "0.015"
# prec_c:tmean_c                                                               "-0.044"   "0.018"   "-2.442" "0.015"
# prec_c:trang_c                                                               "0.063"    "0.025"   "2.478"  "0.013"
# tmean_c:trang_c                                                              "1.297"    "0.946"   "1.372"  "0.170"
# precipitation_anomaly:temp_mean_anomaly                                      "0.373"    "0.069"   "5.441"  "0.000"
# precipitation_anomaly:temp_range_anomaly                                     "-0.590"   "0.134"   "-4.416" "0.000"
# temp_mean_anomaly:temp_range_anomaly                                         "13.845"   "1.813"   "7.637"  "0.000"
# precipitation_anomaly_april:temp_mean_anomaly_april                          "-0.118"   "0.055"   "-2.137" "0.033"
# precipitation_anomaly_april:temp_range_anomaly_april                         "0.271"    "0.094"   "2.897"  "0.004"
# temp_mean_anomaly_april:temp_range_anomaly_april                             "2.196"    "1.462"   "1.502"  "0.133"
# PM25:NO2                                                                     "48.057"   "28.625"  "1.679"  "0.093"
# PM25:O3                                                                      "-7.813"   "6.616"   "-1.181" "0.238"
# NO2:O3                                                                       "6.781"    "7.605"   "0.892"  "0.373"
# prec_c:tmean_c:trang_c                                                       "-0.041"   "0.014"   "-2.917" "0.004"
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   "0.250"    "0.042"   "5.997"  "0.000"
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april "-0.173"   "0.038"   "-4.513" "0.000"
# PM25:NO2:O3                                                                  "-1.405"   "0.706"   "-1.990" "0.047"
# Exp(Est) Lwr     Upr        
# EVI                                                                          "1.000"  "1.000" "1.000"    
# prec_c                                                                       "1.001"  "1.000" "1.002"    
# tmean_c                                                                      "0.996"  "0.956" "1.039"    
# trang_c                                                                      "1.002"  "0.964" "1.041"    
# precipitation_anomaly                                                        "0.991"  "0.988" "0.994"    
# temp_mean_anomaly                                                            "1.039"  "1.004" "1.075"    
# temp_range_anomaly                                                           "0.891"  "0.816" "0.974"    
# precipitation_anomaly_april                                                  "1.004"  "1.001" "1.006"    
# temp_mean_anomaly_april                                                      "1.062"  "1.024" "1.102"    
# temp_range_anomaly_april                                                     "0.826"  "0.765" "0.892"    
# light                                                                        "1.050"  "1.010" "1.092"    
# PM25                                                                         "57.504" "0.291" "11364.577"
# NO2                                                                          "0.194"  "0.000" "84.558"   
# O3                                                                           "5.344"  "1.385" "20.626"   
# prec_c:tmean_c                                                               "1.000"  "0.999" "1.000"    
# prec_c:trang_c                                                               "1.001"  "1.000" "1.001"    
# tmean_c:trang_c                                                              "1.013"  "0.994" "1.032"    
# precipitation_anomaly:temp_mean_anomaly                                      "1.004"  "1.002" "1.005"    
# precipitation_anomaly:temp_range_anomaly                                     "0.994"  "0.992" "0.997"    
# temp_mean_anomaly:temp_range_anomaly                                         "1.148"  "1.108" "1.190"    
# precipitation_anomaly_april:temp_mean_anomaly_april                          "0.999"  "0.998" "1.000"    
# precipitation_anomaly_april:temp_range_anomaly_april                         "1.003"  "1.001" "1.005"    
# temp_mean_anomaly_april:temp_range_anomaly_april                             "1.022"  "0.993" "1.052"    
# PM25:NO2                                                                     "1.617"  "0.923" "2.834"    
# PM25:O3                                                                      "0.925"  "0.812" "1.053"    
# NO2:O3                                                                       "1.070"  "0.922" "1.242"    
# prec_c:tmean_c:trang_c                                                       "1.000"  "0.999" "1.000"    
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   "1.003"  "1.002" "1.003"    
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april "0.998"  "0.998" "0.999"    
# PM25:NO2:O3                                                                  "0.986"  "0.972" "1.000"    


# Assessing if residuals show still a temporal pattern and estimating residual time effect
training$pred <- predict(m6e, type = "link")
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
# (Intercept) -0.03741    0.01717  -2.179   0.0294 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df    F  p-value    
# s(year) 8.434   8.91 5.38 6.88e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.613   Deviance explained = 2.55%
# -REML = 7724.8  Scale est. = 38.754    n = 2372

# Multiplicative change of the effects of year, confidence interval of the temporal effect
plot(m, trans = exp, xlab = "Year", ylab = "Multiplicative change")
abline(h = 1, lty = 3, lwd = 2, col = "lightgray")

# Assessing stability of results: Add year as random effect
m6e_RE <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
               s(year, bs = "re") + ### add temporal random effect
                EVI + prec_c*tmean_c*trang_c +
                precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                lighttrap*light + pherotrap*PM25*NO2*O3,
             family = gaussian(link = "log"), 
             method = METHOD, 
             data = training)

AIC(m6e_RE)
## [1] 15567.41

summary(m6e_RE)

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ s(month, k = 5) + s(Longitude, Latitude, k = 11, bs = "tp") + 
#   s(year, bs = "re") + EVI + prec_c * tmean_c * trang_c + precipitation_anomaly * 
#   temp_mean_anomaly * temp_range_anomaly + precipitation_anomaly_april * 
#   temp_mean_anomaly_april * temp_range_anomaly_april + lighttrap * 
#   light + pherotrap * PM25 * NO2 * O3
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                                                  -4.652e+00  1.271e+01  -0.366 0.714388
# EVI                                                                          -3.382e-06  1.147e-05  -0.295 0.768066
# prec_c                                                                        1.025e-03  3.021e-04   3.395 0.000699
# tmean_c                                                                      -3.684e-03  2.118e-02  -0.174 0.861887
# trang_c                                                                       1.954e-03  1.953e-02   0.100 0.920304
# precipitation_anomaly                                                        -9.086e-03  1.455e-03  -6.245 5.03e-10
# temp_mean_anomaly                                                             3.805e-02  1.731e-02   2.199 0.027987
# temp_range_anomaly                                                           -1.149e-01  4.527e-02  -2.539 0.011189
# precipitation_anomaly_april                                                   3.552e-03  1.212e-03   2.931 0.003407
# temp_mean_anomaly_april                                                       6.025e-02  1.874e-02   3.215 0.001322
# temp_range_anomaly_april                                                     -1.914e-01  3.913e-02  -4.891 1.07e-06
# lighttrapY                                                                   -7.359e+01  3.120e+01  -2.359 0.018403
# light                                                                         4.903e-02  2.010e-02   2.439 0.014822
# pherotrapY                                                                    0.000e+00  0.000e+00     NaN      NaN
# PM25                                                                          4.052e+00  2.697e+00   1.502 0.133170
# NO2                                                                          -1.642e+00  3.102e+00  -0.529 0.596622
# O3                                                                            1.676e+00  6.891e-01   2.432 0.015075
# prec_c:tmean_c                                                               -4.397e-04  1.801e-04  -2.442 0.014700
# prec_c:trang_c                                                                6.250e-04  2.523e-04   2.478 0.013294
# tmean_c:trang_c                                                               1.297e-02  9.459e-03   1.372 0.170299
# precipitation_anomaly:temp_mean_anomaly                                       3.734e-03  6.863e-04   5.441 5.85e-08
# precipitation_anomaly:temp_range_anomaly                                     -5.896e-03  1.335e-03  -4.416 1.05e-05
# temp_mean_anomaly:temp_range_anomaly                                          1.385e-01  1.813e-02   7.637 3.22e-14
# precipitation_anomaly_april:temp_mean_anomaly_april                          -1.178e-03  5.516e-04  -2.137 0.032733
# precipitation_anomaly_april:temp_range_anomaly_april                          2.710e-03  9.358e-04   2.897 0.003809
# temp_mean_anomaly_april:temp_range_anomaly_april                              2.196e-02  1.462e-02   1.502 0.133320
# lighttrapY:light                                                             -5.248e-02  3.064e-02  -1.713 0.086857
# pherotrapY:PM25                                                              -4.572e+00  2.994e+00  -1.527 0.126863
# pherotrapY:NO2                                                               -3.142e-01  3.578e+00  -0.088 0.930025
# PM25:NO2                                                                      4.806e-01  2.862e-01   1.679 0.093314
# pherotrapY:O3                                                                -1.571e+00  7.573e-01  -2.075 0.038134
# PM25:O3                                                                      -7.813e-02  6.616e-02  -1.181 0.237727
# NO2:O3                                                                        6.781e-02  7.605e-02   0.892 0.372651
# prec_c:tmean_c:trang_c                                                       -4.110e-04  1.409e-04  -2.917 0.003571
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                    2.497e-03  4.164e-04   5.997 2.32e-09
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -1.730e-03  3.834e-04  -4.513 6.72e-06
# pherotrapY:PM25:NO2                                                          -2.722e-01  3.303e-01  -0.824 0.410087
# pherotrapY:PM25:O3                                                            9.527e-02  7.316e-02   1.302 0.192972
# pherotrapY:NO2:O3                                                            -1.387e-02  8.733e-02  -0.159 0.873831
# PM25:NO2:O3                                                                  -1.405e-02  7.062e-03  -1.990 0.046735
# pherotrapY:PM25:NO2:O3                                                        8.313e-03  8.108e-03   1.025 0.305327
# 
# (Intercept)                                                                     
# EVI                                                                             
# prec_c                                                                       ***
#   tmean_c                                                                         
# trang_c                                                                         
# precipitation_anomaly                                                        ***
#   temp_mean_anomaly                                                            *  
#   temp_range_anomaly                                                           *  
#   precipitation_anomaly_april                                                  ** 
#   temp_mean_anomaly_april                                                      ** 
#   temp_range_anomaly_april                                                     ***
#   lighttrapY                                                                   *  
#   light                                                                        *  
#   pherotrapY                                                                      
# PM25                                                                            
# NO2                                                                             
# O3                                                                           *  
#   prec_c:tmean_c                                                               *  
#   prec_c:trang_c                                                               *  
#   tmean_c:trang_c                                                                 
# precipitation_anomaly:temp_mean_anomaly                                      ***
#   precipitation_anomaly:temp_range_anomaly                                     ***
#   temp_mean_anomaly:temp_range_anomaly                                         ***
#   precipitation_anomaly_april:temp_mean_anomaly_april                          *  
#   precipitation_anomaly_april:temp_range_anomaly_april                         ** 
#   temp_mean_anomaly_april:temp_range_anomaly_april                                
# lighttrapY:light                                                             .  
# pherotrapY:PM25                                                                 
# pherotrapY:NO2                                                                  
# PM25:NO2                                                                     .  
# pherotrapY:O3                                                                *  
#   PM25:O3                                                                         
# NO2:O3                                                                          
# prec_c:tmean_c:trang_c                                                       ** 
#   precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april ***
#   pherotrapY:PM25:NO2                                                             
# pherotrapY:PM25:O3                                                              
# pherotrapY:NO2:O3                                                               
# PM25:NO2:O3                                                                  *  
#   pherotrapY:PM25:NO2:O3                                                          
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.907e+00  3.994 210.15  <2e-16 ***
#   s(Longitude,Latitude) 9.129e+00  9.770  25.92  <2e-16 ***
#   s(year)               4.313e-07  1.000   0.00  0.0115 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 55/56
# R-sq.(adj) =  0.595   Deviance explained = 60.4%
# -REML = 7947.8  Scale est. = 40.514    n = 2372


# Compute linear predictor describes log(count)
lp <- predict(m6e, newdata = training, type = "link")
plot(lp, log(training$count), xlab = "Linear predictor",
     ylab = "log(count)")
abline(a = 0, b = 1)

## Validation
# with year 

# Model 4b, consider season, space, habitat, trap type interact with year, and sampling weather
summary(m4b <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                    EVI + lighttrap*pherotrap*year + prec_c*tmean_c*trang_c,
                  family = gaussian(link = "log"), 
                  method = METHOD, 
                  data = training))


# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ s(month, k = 5) + s(Longitude, Latitude, k = 11, bs = "tp") + 
#   EVI + lighttrap * pherotrap * year + prec_c * tmean_c * trang_c
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 1.298e+02  1.681e+01   7.721 1.69e-14 ***
#   EVI                        -2.505e-05  1.402e-05  -1.786 0.074200 .  
# lighttrapY                  0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY                 -1.536e+02  1.754e+01  -8.756  < 2e-16 ***
#   year                       -6.418e-02  8.381e-03  -7.658 2.74e-14 ***
#   prec_c                      2.578e-03  3.144e-04   8.199 3.94e-16 ***
#   tmean_c                     6.365e-02  1.998e-02   3.186 0.001460 ** 
#   trang_c                     1.080e-01  2.471e-02   4.370 1.30e-05 ***
#   lighttrapY:pherotrapY       0.000e+00  0.000e+00     NaN      NaN    
# lighttrapY:year             0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY:year             7.695e-02  8.743e-03   8.802  < 2e-16 ***
#   prec_c:tmean_c             -8.799e-05  1.786e-04  -0.493 0.622315    
# prec_c:trang_c              1.031e-03  2.772e-04   3.719 0.000205 ***
#   tmean_c:trang_c             1.918e-02  1.049e-02   1.828 0.067697 .  
# lighttrapY:pherotrapY:year  0.000e+00  0.000e+00     NaN      NaN    
# prec_c:tmean_c:trang_c     -3.277e-04  1.522e-04  -2.153 0.031395 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.868  3.988 124.55  <2e-16 ***
#   s(Longitude,Latitude) 8.283  9.312  19.91  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 26/30
# R-sq.(adj) =   0.43   Deviance explained = 43.6%
# -REML = 8231.1  Scale est. = 57.03     n = 2372

AIC(m4b)
## [1] 16349.73

Y <- predict(m4, newdata = validation, type = "terms")
colnames(Y)

yvar <- c(1:13)
validation$Farm <- factor(validation$Farm)
validation$logct <- log(validation$count)
validation$ad <- rowSums(Y[, yvar])
pvalue(coin::spearman_test(ad ~ logct | Farm, data = validation))
# [1] <2.220446e-16

colfunc <- colorRampPalette(c("blue", "yellow", "brown4"))
sp <- cor.test(~ ad + logct, data = validation, method = "spearman")$estimate

pdf("both_year_validate.pdf", height=7, width=7)
plot(count ~ ad, log = "y", ylab = "log(Count per day)",
     xlab = "Linear combination annual conditions",
     data = validation, col = colfunc(27)[validation$year-1997], pch = 19)

legend(-2, 200, legend = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"), box.col = "white",
       col = colfunc(27)[16:27], pch = 19)
text(0, 150, as.expression(bquote(rho == .(round(sp, 3)))))
text(0, 100, "p < 0.001")
dev.off()

# Replacing year with annual environmental dynamics 
X <- predict(m6e, newdata = validation, type = "terms")
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

legend(70, 200, legend = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"), box.col = "white",
       col = colfunc(27)[16:27], pch = 19)
text(73, 150, as.expression(bquote(rho == .(round(sp, 3)))))
text(73, 100, "p < 0.001")
dev.off()

# Winter anomaly only predictors
wvar <- c(1:4, 11:19, 26:33, 36:42)
validation$weather <- rowSums(X[, wvar])
pvalue(coin::spearman_test(weather ~ logct | Farm, data = validation))
# [1] <2.220446e-16

sp <- cor.test(~ weather + logct, data = validation, method = "spearman")$estimate

pdf("pl_both_weather_validate.pdf", height=7, width=7)
plot(count ~ weather, log = "y", ylab = "log(Count per day)",
     xlab = "Linear combination weather conditions",
     data = validation,col = colfunc(27)[validation$year-1997], pch = 19)

#legend(338, 8, legend = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"), box.col = "white",
#       col = colfunc(27)[16:27], pch = 19)
text(73, 150, as.expression(bquote(rho == .(round(sp, 3)))))
text(73, 100, "p < 0.001")
dev.off()


# Replacing year with annual environmental dynamics 
X <- predict(m6f, newdata = validation, type = "terms")
colnames(X)
# 
# [1] "EVI"                                                                                   
# [2] "prec_c"                                                                                
# [3] "tmean_c"                                                                               
# [4] "trang_c"                                                                               
# [5] "precipitation_anomaly"                                                                 
# [6] "temp_mean_anomaly"                                                                     
# [7] "temp_range_anomaly"                                                                    
# [8] "lighttrap"                                                                             
# [9] "precipitation_anomaly_april"                                                           
# [10] "temp_mean_anomaly_april"                                                               
# [11] "temp_range_anomaly_april"                                                              
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
# [23] "lighttrap:precipitation_anomaly_april"                                                 
# [24] "lighttrap:temp_mean_anomaly_april"                                                     
# [25] "precipitation_anomaly_april:temp_mean_anomaly_april"                                   
# [26] "lighttrap:temp_range_anomaly_april"                                                    
# [27] "precipitation_anomaly_april:temp_range_anomaly_april"                                  
# [28] "temp_mean_anomaly_april:temp_range_anomaly_april"                                      
# [29] "pherotrap:PM25"                                                                        
# [30] "pherotrap:NO2"                                                                         
# [31] "PM25:NO2"                                                                              
# [32] "pherotrap:O3"                                                                          
# [33] "PM25:O3"                                                                               
# [34] "NO2:O3"                                                                                
# [35] "prec_c:tmean_c:trang_c"                                                                
# [36] "precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly"                            
# [37] "lighttrap:precipitation_anomaly_april:temp_mean_anomaly_april"                         
# [38] "lighttrap:precipitation_anomaly_april:temp_range_anomaly_april"                        
# [39] "lighttrap:temp_mean_anomaly_april:temp_range_anomaly_april"                            
# [40] "precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april"          
# [41] "pherotrap:PM25:NO2"                                                                    
# [42] "pherotrap:PM25:O3"                                                                     
# [43] "pherotrap:NO2:O3"                                                                      
# [44] "PM25:NO2:O3"                                                                           
# [45] "lighttrap:precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april"
# [46] "pherotrap:PM25:NO2:O3"                                                                 
# [47] "s(month)"                                                                              
# [48] "s(Longitude,Latitude)" 

# Annual dynamics predictors
fvar <- c(1:48)
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

legend(1.7, 200, legend = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"), box.col = "white",
       col = colfunc(27)[16:27], pch = 19)
text(4.5, 150, as.expression(bquote(rho == .(round(sp, 3)))))
text(4.5, 100, "p < 0.001")
dev.off()

# Winter anomaly only predictors
wvar <- c(1:4, 8:11, 13:19, 23:35, 37:48)
validation$weather <- rowSums(X[, wvar])
pvalue(coin::spearman_test(weather ~ logct | Farm, data = validation))
# [1] <2.220446e-16

sp <- cor.test(~ weather + logct, data = validation, method = "spearman")$estimate

pdf("pl_both_weather_validate.pdf", height=7, width=7)
plot(count ~ weather, log = "y", ylab = "log(Count per day)",
     xlab = "Linear combination weather conditions",
     data = validation,col = colfunc(27)[validation$year-1997], pch = 19)

#legend(338, 8, legend = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"), box.col = "white",
#       col = colfunc(27)[16:27], pch = 19)
text(4.5, 150, as.expression(bquote(rho == .(round(sp, 3)))))
text(4.5, 100, "p < 0.001")
dev.off()

