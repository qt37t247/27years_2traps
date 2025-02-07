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
#   count ~ lighttrap + pherotrap + EVI + s(Longitude, Latitude, 
#                                           k = 11, bs = "tp") + EVI
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.949e-01  1.361e-01   1.431    0.152    
# lighttrapY  0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY  1.276e+00  1.040e-01  12.279   <2e-16 ***
#   EVI         1.615e-04  1.877e-05   8.605   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value    
# s(Longitude,Latitude) 6.858  8.191 7.151  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 13/14
# R-sq.(adj) =  0.144   Deviance explained = 14.7%
# -REML =   8663  Scale est. = 85.681    n = 2372

AIC(m1b)
## [1] 17300.83


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

AIC(m2a)
## [1] 16560.12


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
# (Intercept)             7.093e-01  1.156e-01   6.136 9.91e-10 ***
#   lighttrapY              0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY              1.169e+00  6.407e-02  18.243  < 2e-16 ***
#   EVI                    -1.689e-05  1.450e-05  -1.164 0.244442    
# prec_c                  2.453e-03  3.287e-04   7.465 1.17e-13 ***
#   tmean_c                 8.239e-02  2.088e-02   3.947 8.16e-05 ***
#   trang_c                 1.017e-01  2.508e-02   4.055 5.17e-05 ***
#   prec_c:tmean_c          1.367e-05  1.870e-04   0.073 0.941742    
# prec_c:trang_c          1.064e-03  2.931e-04   3.631 0.000289 ***
#   tmean_c:trang_c         2.292e-02  1.090e-02   2.102 0.035696 *  
#   prec_c:tmean_c:trang_c -2.861e-04  1.614e-04  -1.773 0.076373 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(Longitude,Latitude) 8.251  9.273  18.49  <2e-16 ***
#   s(month)              3.857  3.986 114.24  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 24/25
# R-sq.(adj) =  0.403   Deviance explained = 40.8%
# -REML = 8285.9  Scale est. = 59.727    n = 2372

AIC(m2c)
# [1] 16457.58


# Model 3a, consider month, space, sampling weather, and year linear
summary(m3a <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") + 
                     prec_c*tmean_c*trang_c + year,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(month, k = 5) + s(Longitude, 
#                                                       Latitude, k = 11, bs = "tp") + prec_c * tmean_c * trang_c + 
#   year
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            -6.269e+00  5.906e+00  -1.062 0.288541    
# lighttrapY              0.000e+00  0.000e+00     NaN      NaN    
# pherotrapY              1.186e+00  6.501e-02  18.242  < 2e-16 ***
#   prec_c                  2.498e-03  3.306e-04   7.557 5.87e-14 ***
#   tmean_c                 8.030e-02  2.090e-02   3.842 0.000125 ***
#   trang_c                 1.124e-01  2.574e-02   4.367 1.31e-05 ***
#   year                    3.425e-03  2.934e-03   1.167 0.243262    
# prec_c:tmean_c          9.954e-06  1.880e-04   0.053 0.957773    
# prec_c:trang_c          1.025e-03  2.936e-04   3.493 0.000486 ***
#   tmean_c:trang_c         2.474e-02  1.097e-02   2.256 0.024186 *  
#   prec_c:tmean_c:trang_c -2.588e-04  1.616e-04  -1.602 0.109297    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.858  3.986 114.28  <2e-16 ***
#   s(Longitude,Latitude) 8.199  9.241  18.98  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 24/25
# R-sq.(adj) =  0.403   Deviance explained = 40.8%
# -REML = 8280.6  Scale est. = 59.734    n = 2372

AIC(m3a)
## [1] 16457.65


# Model 3b, consider month, space, sampling weather, and year smoothed non-linear
summary(m3b <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     prec_c*tmean_c*trang_c + s(year),
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))


# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ lighttrap + pherotrap + s(month, k = 5) + s(Longitude, 
#                                                       Latitude, k = 11, bs = "tp") + prec_c * tmean_c * trang_c + 
#   s(year)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.0000000  0.0000000     NaN      NaN    
# lighttrapY              0.4902706  0.0830687   5.902 4.11e-09 ***
#   pherotrapY              1.6400910  0.0684808  23.950  < 2e-16 ***
#   prec_c                  0.0016716  0.0003156   5.296 1.30e-07 ***
#   tmean_c                 0.0418618  0.0228772   1.830  0.06740 .  
# trang_c                 0.0894153  0.0188465   4.744 2.22e-06 ***
#   prec_c:tmean_c         -0.0010288  0.0001790  -5.746 1.03e-08 ***
#   prec_c:trang_c          0.0004097  0.0002840   1.442  0.14934    
# tmean_c:trang_c         0.0097799  0.0087885   1.113  0.26590    
# prec_c:tmean_c:trang_c -0.0004912  0.0001500  -3.274  0.00108 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.909  3.994 180.33  <2e-16 ***
#   s(Longitude,Latitude) 8.386  9.336  22.65  <2e-16 ***
#   s(year)               8.773  8.983  73.35  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 32/33
# R-sq.(adj) =  0.528   Deviance explained = 53.4%
# -REML = 8023.7  Scale est. = 47.228    n = 2372

AIC(m3b)
## [1] 15908.14


# Model 3c, consider month, space, sampling weather, and annual dynamics (winter anomaly, april anomaly, light, and air quality)
summary(m3c <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
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
#   count ~ lighttrap + pherotrap + s(month, k = 5) + s(Longitude, 
#                                                       Latitude, k = 11, bs = "tp") + prec_c * tmean_c * trang_c + 
#   precipitation_anomaly * temp_mean_anomaly * temp_range_anomaly + 
#   precipitation_anomaly_april * temp_mean_anomaly_april * temp_range_anomaly_april + 
#   light + PM25 * NO2 * O3
# 
# Parametric coefficients:
#   Estimate Std. Error t value
# (Intercept)                                                                   0.0000000  0.0000000     NaN
# lighttrapY                                                                   -5.2157350 11.3764506  -0.458
# pherotrapY                                                                   -4.0981485 11.3762684  -0.360
# prec_c                                                                        0.0008338  0.0003363   2.479
# tmean_c                                                                       0.0179361  0.0234920   0.763
# trang_c                                                                      -0.0077801  0.0215902  -0.360
# precipitation_anomaly                                                        -0.0085029  0.0016279  -5.223
# temp_mean_anomaly                                                             0.0527218  0.0189637   2.780
# temp_range_anomaly                                                           -0.1381755  0.0501675  -2.754
# precipitation_anomaly_april                                                   0.0039113  0.0012964   3.017
# temp_mean_anomaly_april                                                       0.0560312  0.0204903   2.735
# temp_range_anomaly_april                                                     -0.1667584  0.0439339  -3.796
# light                                                                         0.0311263  0.0209760   1.484
# PM25                                                                         -0.6615291  1.1303739  -0.585
# NO2                                                                          -2.9041268  1.4837750  -1.957
# O3                                                                            0.0858168  0.2734126   0.314
# prec_c:tmean_c                                                               -0.0002125  0.0002008  -1.058
# prec_c:trang_c                                                                0.0004458  0.0002785   1.601
# tmean_c:trang_c                                                               0.0134740  0.0103231   1.305
# precipitation_anomaly:temp_mean_anomaly                                       0.0041180  0.0007418   5.551
# precipitation_anomaly:temp_range_anomaly                                     -0.0067369  0.0014455  -4.661
# temp_mean_anomaly:temp_range_anomaly                                          0.1374205  0.0197525   6.957
# precipitation_anomaly_april:temp_mean_anomaly_april                          -0.0011598  0.0005945  -1.951
# precipitation_anomaly_april:temp_range_anomaly_april                          0.0027496  0.0010242   2.685
# temp_mean_anomaly_april:temp_range_anomaly_april                              0.0097984  0.0165221   0.593
# PM25:NO2                                                                      0.3249909  0.1319418   2.463
# PM25:O3                                                                       0.0209147  0.0273706   0.764
# NO2:O3                                                                        0.0776024  0.0357894   2.168
# prec_c:tmean_c:trang_c                                                       -0.0002027  0.0001578  -1.284
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                    0.0022646  0.0004539   4.989
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -0.0018732  0.0004239  -4.419
# PM25:NO2:O3                                                                  -0.0085776  0.0032025  -2.678
# Pr(>|t|)    
# (Intercept)                                                                       NaN    
# lighttrapY                                                                   0.646659    
# pherotrapY                                                                   0.718703    
# prec_c                                                                       0.013234 *  
#   tmean_c                                                                      0.445243    
# trang_c                                                                      0.718616    
# precipitation_anomaly                                                        1.92e-07 ***
#   temp_mean_anomaly                                                            0.005477 ** 
#   temp_range_anomaly                                                           0.005928 ** 
#   precipitation_anomaly_april                                                  0.002581 ** 
#   temp_mean_anomaly_april                                                      0.006294 ** 
#   temp_range_anomaly_april                                                     0.000151 ***
#   light                                                                        0.137971    
# PM25                                                                         0.558450    
# NO2                                                                          0.050437 .  
# O3                                                                           0.753646    
# prec_c:tmean_c                                                               0.290090    
# prec_c:trang_c                                                               0.109536    
# tmean_c:trang_c                                                              0.191946    
# precipitation_anomaly:temp_mean_anomaly                                      3.15e-08 ***
#   precipitation_anomaly:temp_range_anomaly                                     3.33e-06 ***
#   temp_mean_anomaly:temp_range_anomaly                                         4.50e-12 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april                          0.051207 .  
# precipitation_anomaly_april:temp_range_anomaly_april                         0.007312 ** 
#   temp_mean_anomaly_april:temp_range_anomaly_april                             0.553206    
# PM25:NO2                                                                     0.013845 *  
#   PM25:O3                                                                      0.444869    
# NO2:O3                                                                       0.030236 *  
#   prec_c:tmean_c:trang_c                                                       0.199281    
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   6.51e-07 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april 1.04e-05 ***
#   PM25:NO2:O3                                                                  0.007448 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.891  3.992 170.09  <2e-16 ***
#   s(Longitude,Latitude) 8.572  9.484  17.82  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 45/46
# R-sq.(adj) =  0.534   Deviance explained = 54.2%
# -REML = 8077.5  Scale est. = 46.638    n = 2372

AIC(m3c)
## [1] 15891.47

# Model 3d, consider month, space, sampling weather, and annual dynamics (winter anomaly, april anomaly, light/PM2.5, and air quality)
summary(m3d <- gam(count ~ lighttrap + pherotrap + s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
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
#   count ~ lighttrap + pherotrap + s(month, k = 5) + s(Longitude, 
#                                                       Latitude, k = 11, bs = "tp") + prec_c * tmean_c * trang_c + 
#   precipitation_anomaly * temp_mean_anomaly * temp_range_anomaly + 
#   precipitation_anomaly_april * temp_mean_anomaly_april * temp_range_anomaly_april + 
#   PM25 * light + NO2 * O3
# 
# Parametric coefficients:
#   Estimate Std. Error t value
# (Intercept)                                                                  -7.902e+00  2.339e+00  -3.378
# lighttrapY                                                                   -1.127e+00  5.131e-02 -21.958
# pherotrapY                                                                    0.000e+00  0.000e+00     NaN
# prec_c                                                                        8.946e-04  3.399e-04   2.632
# tmean_c                                                                       3.353e-02  2.442e-02   1.373
# trang_c                                                                       1.460e-03  2.208e-02   0.066
# precipitation_anomaly                                                        -7.283e-03  1.616e-03  -4.506
# temp_mean_anomaly                                                             1.620e-02  1.856e-02   0.873
# temp_range_anomaly                                                           -2.047e-01  4.812e-02  -4.254
# precipitation_anomaly_april                                                   4.001e-03  1.241e-03   3.223
# temp_mean_anomaly_april                                                       5.164e-02  2.090e-02   2.471
# temp_range_anomaly_april                                                     -1.937e-01  4.266e-02  -4.540
# PM25                                                                          9.978e-02  2.148e-02   4.645
# light                                                                         3.426e-01  7.289e-02   4.700
# NO2                                                                           9.470e-03  2.767e-01   0.034
# O3                                                                            2.046e-01  5.551e-02   3.686
# prec_c:tmean_c                                                               -1.937e-04  2.001e-04  -0.968
# prec_c:trang_c                                                                2.937e-04  2.752e-04   1.067
# tmean_c:trang_c                                                               1.322e-02  1.021e-02   1.295
# precipitation_anomaly:temp_mean_anomaly                                       1.922e-03  6.801e-04   2.825
# precipitation_anomaly:temp_range_anomaly                                     -3.505e-03  1.424e-03  -2.461
# temp_mean_anomaly:temp_range_anomaly                                          1.121e-01  1.881e-02   5.960
# precipitation_anomaly_april:temp_mean_anomaly_april                          -1.732e-03  5.605e-04  -3.089
# precipitation_anomaly_april:temp_range_anomaly_april                          3.293e-03  1.022e-03   3.222
# temp_mean_anomaly_april:temp_range_anomaly_april                              2.018e-02  1.647e-02   1.225
# PM25:light                                                                   -2.729e-02  6.883e-03  -3.965
# NO2:O3                                                                       -6.503e-04  6.748e-03  -0.096
# prec_c:tmean_c:trang_c                                                       -9.049e-05  1.566e-04  -0.578
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                    1.988e-03  4.486e-04   4.431
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -2.197e-03  4.299e-04  -5.110
# Pr(>|t|)    
# (Intercept)                                                                  0.000742 ***
#   lighttrapY                                                                    < 2e-16 ***
#   pherotrapY                                                                        NaN    
# prec_c                                                                       0.008557 ** 
#   tmean_c                                                                      0.169890    
# trang_c                                                                      0.947260    
# precipitation_anomaly                                                        6.92e-06 ***
#   temp_mean_anomaly                                                            0.382991    
# temp_range_anomaly                                                           2.18e-05 ***
#   precipitation_anomaly_april                                                  0.001285 ** 
#   temp_mean_anomaly_april                                                      0.013554 *  
#   temp_range_anomaly_april                                                     5.90e-06 ***
#   PM25                                                                         3.58e-06 ***
#   light                                                                        2.75e-06 ***
#   NO2                                                                          0.972703    
# O3                                                                           0.000233 ***
#   prec_c:tmean_c                                                               0.333037    
# prec_c:trang_c                                                               0.285907    
# tmean_c:trang_c                                                              0.195459    
# precipitation_anomaly:temp_mean_anomaly                                      0.004765 ** 
#   precipitation_anomaly:temp_range_anomaly                                     0.013908 *  
#   temp_mean_anomaly:temp_range_anomaly                                         2.90e-09 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april                          0.002029 ** 
#   precipitation_anomaly_april:temp_range_anomaly_april                         0.001292 ** 
#   temp_mean_anomaly_april:temp_range_anomaly_april                             0.220644    
# PM25:light                                                                   7.56e-05 ***
#   NO2:O3                                                                       0.923237    
# prec_c:tmean_c:trang_c                                                       0.563355    
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   9.83e-06 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april 3.49e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.886  3.991 167.03  <2e-16 ***
#   s(Longitude,Latitude) 8.669  9.541  17.15  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 43/44
# R-sq.(adj) =  0.527   Deviance explained = 53.5%
# -REML = 8088.2  Scale est. = 47.366    n = 2372

AIC(m3d)
## [1] 15926.28


# Model 4a, consider month, space, sampling weather, and annual dynamics (trap type interact with winter anomaly)
summary(m4a <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     prec_c*tmean_c*trang_c +
                     lighttrap*pherotrap*precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     light + PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

AIC(m4a)
## [1] 15820.02

# Model 4b, consider month, space, sampling weather, and annual dynamics (trap type interact with april anomaly)
summary(m4b <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     prec_c*tmean_c*trang_c +
                     precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     lighttrap*pherotrap*precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     light + PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

AIC(m4b)
## [1] 15782.76

# Model 4c, consider month, space, sampling weather, and annual dynamics (trap type interact with light)
summary(m4c <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     prec_c*tmean_c*trang_c +
                     precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     lighttrap*pherotrap*light + PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

AIC(m4c)
## [1] 15892.81

# Model 4d, consider month, space, sampling weather, and annual dynamics (trap type interact with air quality)
summary(m4d <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
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
#   prec_c * tmean_c * trang_c + precipitation_anomaly * temp_mean_anomaly * 
#   temp_range_anomaly + precipitation_anomaly_april * temp_mean_anomaly_april * 
#   temp_range_anomaly_april + light + lighttrap * pherotrap * 
#   PM25 * NO2 * O3
# 
# Parametric coefficients:
#   Estimate Std. Error t value
# (Intercept)                                                                  -4.558e+00  1.257e+01  -0.362
# prec_c                                                                        1.024e-03  3.022e-04   3.388
# tmean_c                                                                      -5.073e-03  2.113e-02  -0.240
# trang_c                                                                       2.183e-03  1.938e-02   0.113
# precipitation_anomaly                                                        -9.051e-03  1.453e-03  -6.228
# temp_mean_anomaly                                                             3.804e-02  1.727e-02   2.202
# temp_range_anomaly                                                           -1.133e-01  4.520e-02  -2.508
# precipitation_anomaly_april                                                   3.367e-03  1.215e-03   2.772
# temp_mean_anomaly_april                                                       5.913e-02  1.877e-02   3.151
# temp_range_anomaly_april                                                     -1.933e-01  3.909e-02  -4.944
# light                                                                         3.999e-02  1.920e-02   2.082
# lighttrapY                                                                   -7.730e+01  3.128e+01  -2.471
# pherotrapY                                                                    0.000e+00  0.000e+00     NaN
# PM25                                                                         -5.465e-01  1.274e+00  -0.429
# NO2                                                                          -1.946e+00  1.693e+00  -1.150
# O3                                                                            1.764e+00  6.943e-01   2.541
# prec_c:tmean_c                                                               -4.367e-04  1.800e-04  -2.426
# prec_c:trang_c                                                                6.379e-04  2.521e-04   2.530
# tmean_c:trang_c                                                               1.305e-02  9.421e-03   1.385
# precipitation_anomaly:temp_mean_anomaly                                       3.760e-03  6.852e-04   5.488
# precipitation_anomaly:temp_range_anomaly                                     -6.061e-03  1.329e-03  -4.561
# temp_mean_anomaly:temp_range_anomaly                                          1.376e-01  1.811e-02   7.600
# precipitation_anomaly_april:temp_mean_anomaly_april                          -1.087e-03  5.513e-04  -1.972
# precipitation_anomaly_april:temp_range_anomaly_april                          2.512e-03  9.249e-04   2.716
# temp_mean_anomaly_april:temp_range_anomaly_april                              2.099e-02  1.465e-02   1.433
# lighttrapY:pherotrapY                                                         0.000e+00  0.000e+00     NaN
# lighttrapY:PM25                                                               4.862e+00  3.006e+00   1.617
# pherotrapY:PM25                                                               0.000e+00  0.000e+00     NaN
# lighttrapY:NO2                                                                7.173e-01  3.565e+00   0.201
# pherotrapY:NO2                                                                0.000e+00  0.000e+00     NaN
# PM25:NO2                                                                      4.577e-01  2.884e-01   1.587
# lighttrapY:O3                                                                 0.000e+00  0.000e+00     NaN
# pherotrapY:O3                                                                -1.662e+00  7.593e-01  -2.189
# PM25:O3                                                                      -8.458e-02  6.683e-02  -1.266
# NO2:O3                                                                        5.693e-02  7.591e-02   0.750
# prec_c:tmean_c:trang_c                                                       -4.129e-04  1.408e-04  -2.932
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                    2.524e-03  4.154e-04   6.077
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -1.689e-03  3.804e-04  -4.440
# lighttrapY:pherotrapY:PM25                                                    0.000e+00  0.000e+00     NaN
# lighttrapY:pherotrapY:NO2                                                     0.000e+00  0.000e+00     NaN
# lighttrapY:PM25:NO2                                                           0.000e+00  0.000e+00     NaN
# pherotrapY:PM25:NO2                                                          -2.497e-01  3.311e-01  -0.754
# lighttrapY:pherotrapY:O3                                                      0.000e+00  0.000e+00     NaN
# lighttrapY:PM25:O3                                                            0.000e+00  0.000e+00     NaN
# pherotrapY:PM25:O3                                                            1.023e-01  7.347e-02   1.392
# lighttrapY:NO2:O3                                                             0.000e+00  0.000e+00     NaN
# pherotrapY:NO2:O3                                                            -3.081e-03  8.697e-02  -0.035
# PM25:NO2:O3                                                                  -1.346e-02  7.114e-03  -1.892
# lighttrapY:pherotrapY:PM25:NO2                                                0.000e+00  0.000e+00     NaN
# lighttrapY:pherotrapY:PM25:O3                                                 0.000e+00  0.000e+00     NaN
# lighttrapY:pherotrapY:NO2:O3                                                  0.000e+00  0.000e+00     NaN
# lighttrapY:PM25:NO2:O3                                                        0.000e+00  0.000e+00     NaN
# pherotrapY:PM25:NO2:O3                                                        7.730e-03  8.129e-03   0.951
# lighttrapY:pherotrapY:PM25:NO2:O3                                             0.000e+00  0.000e+00     NaN
# Pr(>|t|)    
# (Intercept)                                                                  0.717039    
# prec_c                                                                       0.000715 ***
#   tmean_c                                                                      0.810279    
# trang_c                                                                      0.910328    
# precipitation_anomaly                                                        5.60e-10 ***
#   temp_mean_anomaly                                                            0.027744 *  
#   temp_range_anomaly                                                           0.012223 *  
#   precipitation_anomaly_april                                                  0.005614 ** 
#   temp_mean_anomaly_april                                                      0.001646 ** 
#   temp_range_anomaly_april                                                     8.19e-07 ***
#   light                                                                        0.037408 *  
#   lighttrapY                                                                   0.013536 *  
#   pherotrapY                                                                        NaN    
# PM25                                                                         0.668095    
# NO2                                                                          0.250321    
# O3                                                                           0.011117 *  
#   prec_c:tmean_c                                                               0.015353 *  
#   prec_c:trang_c                                                               0.011459 *  
#   tmean_c:trang_c                                                              0.166055    
# precipitation_anomaly:temp_mean_anomaly                                      4.51e-08 ***
#   precipitation_anomaly:temp_range_anomaly                                     5.36e-06 ***
#   temp_mean_anomaly:temp_range_anomaly                                         4.27e-14 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april                          0.048735 *  
#   precipitation_anomaly_april:temp_range_anomaly_april                         0.006665 ** 
#   temp_mean_anomaly_april:temp_range_anomaly_april                             0.152122    
# lighttrapY:pherotrapY                                                             NaN    
# lighttrapY:PM25                                                              0.105955    
# pherotrapY:PM25                                                                   NaN    
# lighttrapY:NO2                                                               0.840569    
# pherotrapY:NO2                                                                    NaN    
# PM25:NO2                                                                     0.112660    
# lighttrapY:O3                                                                     NaN    
# pherotrapY:O3                                                                0.028727 *  
#   PM25:O3                                                                      0.205803    
# NO2:O3                                                                       0.453375    
# prec_c:tmean_c:trang_c                                                       0.003406 ** 
#   precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   1.43e-09 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april 9.40e-06 ***
#   lighttrapY:pherotrapY:PM25                                                        NaN    
# lighttrapY:pherotrapY:NO2                                                         NaN    
# lighttrapY:PM25:NO2                                                               NaN    
# pherotrapY:PM25:NO2                                                          0.450772    
# lighttrapY:pherotrapY:O3                                                          NaN    
# lighttrapY:PM25:O3                                                                NaN    
# pherotrapY:PM25:O3                                                           0.163958    
# lighttrapY:NO2:O3                                                                 NaN    
# pherotrapY:NO2:O3                                                            0.971738    
# PM25:NO2:O3                                                                  0.058679 .  
# lighttrapY:pherotrapY:PM25:NO2                                                    NaN    
# lighttrapY:pherotrapY:PM25:O3                                                     NaN    
# lighttrapY:pherotrapY:NO2:O3                                                      NaN    
# lighttrapY:PM25:NO2:O3                                                            NaN    
# pherotrapY:PM25:NO2:O3                                                       0.341715    
# lighttrapY:pherotrapY:PM25:NO2:O3                                                 NaN    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value    
# s(month)              3.906  3.994 210.3  <2e-16 ***
#   s(Longitude,Latitude) 9.081  9.751  25.6  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 52/68
# R-sq.(adj) =  0.595   Deviance explained = 60.3%
# -REML = 7894.8  Scale est. = 40.546    n = 2372

AIC(m4d)
## [1] 15567.07


# Model 4e, consider month, space, sampling weather*traps, and annual dynamics
summary(m4e <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                     lighttrap*pherotrap*prec_c*tmean_c*trang_c +
                     precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                     precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                     light + PM25*NO2*O3,
                   family = gaussian(link = "log"), 
                   method = METHOD, 
                   data = training))

AIC(m4e)
# [1] 15567.07


# Model 5, consider month, space, sampling weather, and annual dynamics 
# (light trap interact with light, and pheromone trap interact with air quality)
summary(m5 <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
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
#   prec_c * tmean_c * trang_c + precipitation_anomaly * temp_mean_anomaly * 
#   temp_range_anomaly + precipitation_anomaly_april * temp_mean_anomaly_april * 
#   temp_range_anomaly_april + lighttrap * light + pherotrap * 
#   PM25 * NO2 * O3
# 
# Parametric coefficients:
#   Estimate Std. Error t value
# (Intercept)                                                                  -4.938e+00  1.266e+01  -0.390
# prec_c                                                                        1.024e-03  3.018e-04   3.394
# tmean_c                                                                      -3.416e-03  2.113e-02  -0.162
# trang_c                                                                       2.890e-03  1.937e-02   0.149
# precipitation_anomaly                                                        -9.051e-03  1.451e-03  -6.237
# temp_mean_anomaly                                                             3.837e-02  1.729e-02   2.220
# temp_range_anomaly                                                           -1.140e-01  4.520e-02  -2.522
# precipitation_anomaly_april                                                   3.557e-03  1.212e-03   2.934
# temp_mean_anomaly_april                                                       6.021e-02  1.873e-02   3.215
# temp_range_anomaly_april                                                     -1.923e-01  3.902e-02  -4.927
# lighttrapY                                                                   -7.283e+01  3.109e+01  -2.342
# light                                                                         4.944e-02  2.007e-02   2.463
# pherotrapY                                                                    0.000e+00  0.000e+00     NaN
# PM25                                                                          4.005e+00  2.694e+00   1.487
# NO2                                                                          -1.720e+00  3.097e+00  -0.555
# O3                                                                            1.664e+00  6.881e-01   2.418
# prec_c:tmean_c                                                               -4.378e-04  1.800e-04  -2.432
# prec_c:trang_c                                                                6.276e-04  2.520e-04   2.491
# tmean_c:trang_c                                                               1.321e-02  9.419e-03   1.402
# precipitation_anomaly:temp_mean_anomaly                                       3.717e-03  6.852e-04   5.424
# precipitation_anomaly:temp_range_anomaly                                     -5.850e-03  1.333e-03  -4.388
# temp_mean_anomaly:temp_range_anomaly                                          1.382e-01  1.812e-02   7.627
# precipitation_anomaly_april:temp_mean_anomaly_april                          -1.192e-03  5.510e-04  -2.163
# precipitation_anomaly_april:temp_range_anomaly_april                          2.733e-03  9.331e-04   2.929
# temp_mean_anomaly_april:temp_range_anomaly_april                              2.200e-02  1.462e-02   1.504
# lighttrapY:light                                                             -5.259e-02  3.063e-02  -1.717
# pherotrapY:PM25                                                              -4.502e+00  2.985e+00  -1.508
# pherotrapY:NO2                                                               -2.355e-01  3.573e+00  -0.066
# PM25:NO2                                                                      4.876e-01  2.859e-01   1.705
# pherotrapY:O3                                                                -1.553e+00  7.549e-01  -2.057
# PM25:O3                                                                      -7.699e-02  6.608e-02  -1.165
# NO2:O3                                                                        6.974e-02  7.595e-02   0.918
# prec_c:tmean_c:trang_c                                                       -4.112e-04  1.407e-04  -2.922
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                    2.497e-03  4.164e-04   5.995
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -1.738e-03  3.831e-04  -4.537
# pherotrapY:PM25:NO2                                                          -2.791e-01  3.300e-01  -0.846
# pherotrapY:PM25:O3                                                            9.355e-02  7.296e-02   1.282
# pherotrapY:NO2:O3                                                            -1.576e-02  8.722e-02  -0.181
# PM25:NO2:O3                                                                  -1.422e-02  7.054e-03  -2.016
# pherotrapY:PM25:NO2:O3                                                        8.480e-03  8.101e-03   1.047
# Pr(>|t|)    
# (Intercept)                                                                  0.696590    
# prec_c                                                                       0.000701 ***
#   tmean_c                                                                      0.871595    
# trang_c                                                                      0.881440    
# precipitation_anomaly                                                        5.30e-10 ***
#   temp_mean_anomaly                                                            0.026537 *  
#   temp_range_anomaly                                                           0.011749 *  
#   precipitation_anomaly_april                                                  0.003377 ** 
#   temp_mean_anomaly_april                                                      0.001322 ** 
#   temp_range_anomaly_april                                                     8.94e-07 ***
#   lighttrapY                                                                   0.019255 *  
#   light                                                                        0.013857 *  
#   pherotrapY                                                                        NaN    
# PM25                                                                         0.137251    
# NO2                                                                          0.578843    
# O3                                                                           0.015683 *  
#   prec_c:tmean_c                                                               0.015080 *  
#   prec_c:trang_c                                                               0.012814 *  
#   tmean_c:trang_c                                                              0.160954    
# precipitation_anomaly:temp_mean_anomaly                                      6.42e-08 ***
#   precipitation_anomaly:temp_range_anomaly                                     1.20e-05 ***
#   temp_mean_anomaly:temp_range_anomaly                                         3.48e-14 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april                          0.030653 *  
#   precipitation_anomaly_april:temp_range_anomaly_april                         0.003430 ** 
#   temp_mean_anomaly_april:temp_range_anomaly_april                             0.132614    
# lighttrapY:light                                                             0.086105 .  
# pherotrapY:PM25                                                              0.131725    
# pherotrapY:NO2                                                               0.947450    
# PM25:NO2                                                                     0.088273 .  
# pherotrapY:O3                                                                0.039831 *  
#   PM25:O3                                                                      0.244113    
# NO2:O3                                                                       0.358553    
# prec_c:tmean_c:trang_c                                                       0.003507 ** 
#   precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   2.35e-09 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april 6.00e-06 ***
#   pherotrapY:PM25:NO2                                                          0.397861    
# pherotrapY:PM25:O3                                                           0.199858    
# pherotrapY:NO2:O3                                                            0.856635    
# PM25:NO2:O3                                                                  0.043875 *  
#   pherotrapY:PM25:NO2:O3                                                       0.295328    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.907  3.994 210.81  <2e-16 ***
#   s(Longitude,Latitude) 9.144  9.776  25.91  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 53/54
# R-sq.(adj) =  0.596   Deviance explained = 60.4%
# -REML = 7937.4  Scale est. = 40.497    n = 2372

AIC(m5)
## [1] 15565.36


gam.check(m5)
# Method: REML   Optimizer: outer newton
# full convergence after 10 iterations.
# Gradient range [-0.0003340295,0.0003035679]
# (score 7937.438 & scale 40.49679).
# Hessian positive definite, eigenvalue range [1.386455,1164.511].
# Model rank =  53 / 54 
# 
# Basis dimension (k) checking results. Low p-value (k-index<1) may
# indicate that k is too low, especially if edf is close to k'.
# 
#                          k'   edf k-index p-value    
# s(month)               4.00  3.91    0.92  <2e-16 ***
#   s(Longitude,Latitude) 10.00  9.14    0.97    0.12    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

concurvity(m5)
# para  s(month) s(Longitude,Latitude)
# worst       1 0.8854359             0.7937777
# observed    1 0.4130324             0.1660273
# estimate    1 0.1942691             0.3983358

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

# Est*100    SD*100    z-Val   
# prec_c                                                                       "0.102"    "0.030"   "3.394" 
# tmean_c                                                                      "-0.342"   "2.113"   "-0.162"
# trang_c                                                                      "0.289"    "1.937"   "0.149" 
# precipitation_anomaly                                                        "-0.905"   "0.145"   "-6.237"
# temp_mean_anomaly                                                            "3.837"    "1.729"   "2.220" 
# temp_range_anomaly                                                           "-11.398"  "4.520"   "-2.522"
# precipitation_anomaly_april                                                  "0.356"    "0.121"   "2.934" 
# temp_mean_anomaly_april                                                      "6.021"    "1.873"   "3.215" 
# temp_range_anomaly_april                                                     "-19.227"  "3.902"   "-4.927"
# light                                                                        "4.944"    "2.007"   "2.463" 
# PM25                                                                         "400.493"  "269.398" "1.487" 
# NO2                                                                          "-171.953" "309.739" "-0.555"
# O3                                                                           "166.394"  "68.815"  "2.418" 
# prec_c:tmean_c                                                               "-0.044"   "0.018"   "-2.432"
# prec_c:trang_c                                                               "0.063"    "0.025"   "2.491" 
# tmean_c:trang_c                                                              "1.321"    "0.942"   "1.402" 
# precipitation_anomaly:temp_mean_anomaly                                      "0.372"    "0.069"   "5.424" 
# precipitation_anomaly:temp_range_anomaly                                     "-0.585"   "0.133"   "-4.388"
# temp_mean_anomaly:temp_range_anomaly                                         "13.823"   "1.812"   "7.627" 
# precipitation_anomaly_april:temp_mean_anomaly_april                          "-0.119"   "0.055"   "-2.163"
# precipitation_anomaly_april:temp_range_anomaly_april                         "0.273"    "0.093"   "2.929" 
# temp_mean_anomaly_april:temp_range_anomaly_april                             "2.200"    "1.462"   "1.504" 
# PM25:NO2                                                                     "48.758"   "28.592"  "1.705" 
# PM25:O3                                                                      "-7.699"   "6.608"   "-1.165"
# NO2:O3                                                                       "6.974"    "7.595"   "0.918" 
# prec_c:tmean_c:trang_c                                                       "-0.041"   "0.014"   "-2.922"
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   "0.250"    "0.042"   "5.995" 
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april "-0.174"   "0.038"   "-4.537"
# PM25:NO2:O3                                                                  "-1.422"   "0.705"   "-2.016"
# p-Val   Exp(Est) Lwr    
# prec_c                                                                       "0.001" "1.001"  "1.000"
# tmean_c                                                                      "0.872" "0.997"  "0.956"
# trang_c                                                                      "0.881" "1.003"  "0.966"
# precipitation_anomaly                                                        "0.000" "0.991"  "0.988"
# temp_mean_anomaly                                                            "0.026" "1.039"  "1.005"
# temp_range_anomaly                                                           "0.012" "0.892"  "0.817"
# precipitation_anomaly_april                                                  "0.003" "1.004"  "1.001"
# temp_mean_anomaly_april                                                      "0.001" "1.062"  "1.024"
# temp_range_anomaly_april                                                     "0.000" "0.825"  "0.764"
# light                                                                        "0.014" "1.051"  "1.010"
# PM25                                                                         "0.137" "54.868" "0.279"
# NO2                                                                          "0.579" "0.179"  "0.000"
# O3                                                                           "0.016" "5.280"  "1.371"
# prec_c:tmean_c                                                               "0.015" "1.000"  "0.999"
# prec_c:trang_c                                                               "0.013" "1.001"  "1.000"
# tmean_c:trang_c                                                              "0.161" "1.013"  "0.995"
# precipitation_anomaly:temp_mean_anomaly                                      "0.000" "1.004"  "1.002"
# precipitation_anomaly:temp_range_anomaly                                     "0.000" "0.994"  "0.992"
# temp_mean_anomaly:temp_range_anomaly                                         "0.000" "1.148"  "1.108"
# precipitation_anomaly_april:temp_mean_anomaly_april                          "0.031" "0.999"  "0.998"
# precipitation_anomaly_april:temp_range_anomaly_april                         "0.003" "1.003"  "1.001"
# temp_mean_anomaly_april:temp_range_anomaly_april                             "0.132" "1.022"  "0.993"
# PM25:NO2                                                                     "0.088" "1.628"  "0.930"
# PM25:O3                                                                      "0.244" "0.926"  "0.813"
# NO2:O3                                                                       "0.358" "1.072"  "0.924"
# prec_c:tmean_c:trang_c                                                       "0.003" "1.000"  "0.999"
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   "0.000" "1.002"  "1.002"
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april "0.000" "0.998"  "0.998"
# PM25:NO2:O3                                                                  "0.044" "0.986"  "0.972"
# Upr        
# prec_c                                                                       "1.002"    
# tmean_c                                                                      "1.039"    
# trang_c                                                                      "1.042"    
# precipitation_anomaly                                                        "0.994"    
# temp_mean_anomaly                                                            "1.075"    
# temp_range_anomaly                                                           "0.975"    
# precipitation_anomaly_april                                                  "1.006"    
# temp_mean_anomaly_april                                                      "1.102"    
# temp_range_anomaly_april                                                     "0.891"    
# light                                                                        "1.093"    
# PM25                                                                         "10775.631"
# NO2                                                                          "77.574"   
# O3                                                                           "20.342"   
# prec_c:tmean_c                                                               "1.000"    
# prec_c:trang_c                                                               "1.001"    
# tmean_c:trang_c                                                              "1.032"    
# precipitation_anomaly:temp_mean_anomaly                                      "1.005"    
# precipitation_anomaly:temp_range_anomaly                                     "0.997"    
# temp_mean_anomaly:temp_range_anomaly                                         "1.190"    
# precipitation_anomaly_april:temp_mean_anomaly_april                          "1.000"    
# precipitation_anomaly_april:temp_range_anomaly_april                         "1.005"    
# temp_mean_anomaly_april:temp_range_anomaly_april                             "1.052"    
# PM25:NO2                                                                     "2.852"    
# PM25:O3                                                                      "1.054"    
# NO2:O3                                                                       "1.244"    
# prec_c:tmean_c:trang_c                                                       "1.000"    
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   "1.003"    
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april "0.999"    
# PM25:NO2:O3                                                                  "1.000"  

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
# (Intercept) -0.03757    0.01717  -2.188   0.0288 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F  p-value    
# s(year) 8.439  8.911 5.415 8.05e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.613   Deviance explained = 2.56%
# -REML = 7724.6  Scale est. = 38.748    n = 2372

# Multiplicative change of the effects of year, confidence interval of the temporal effect
plot(m, trans = exp, xlab = "Year", ylab = "Multiplicative change")
abline(h = 1, lty = 3, lwd = 2, col = "lightgray")

# Assessing stability of results: Add year as random effect
m5_RE <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
               s(year, bs = "re") + ### add temporal random effect
                prec_c*tmean_c*trang_c +
                precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                lighttrap*light + pherotrap*PM25*NO2*O3,
             family = gaussian(link = "log"), 
             method = METHOD, 
             data = training)

AIC(m5_RE)
## [1] 15565.42

summary(m5_RE)
# Family: gaussian 
# Link function: log 
# 
# Formula:
#   count ~ s(month, k = 5) + s(Longitude, Latitude, k = 11, bs = "tp") + 
#   s(year, bs = "re") + prec_c * tmean_c * trang_c + precipitation_anomaly * 
#   temp_mean_anomaly * temp_range_anomaly + precipitation_anomaly_april * 
#   temp_mean_anomaly_april * temp_range_anomaly_april + lighttrap * 
#   light + pherotrap * PM25 * NO2 * O3
# 
# Parametric coefficients:
#   Estimate Std. Error t value
# (Intercept)                                                                  -4.938e+00  1.266e+01  -0.390
# prec_c                                                                        1.024e-03  3.018e-04   3.394
# tmean_c                                                                      -3.416e-03  2.113e-02  -0.162
# trang_c                                                                       2.890e-03  1.937e-02   0.149
# precipitation_anomaly                                                        -9.051e-03  1.451e-03  -6.237
# temp_mean_anomaly                                                             3.837e-02  1.729e-02   2.220
# temp_range_anomaly                                                           -1.140e-01  4.520e-02  -2.522
# precipitation_anomaly_april                                                   3.557e-03  1.212e-03   2.934
# temp_mean_anomaly_april                                                       6.021e-02  1.873e-02   3.215
# temp_range_anomaly_april                                                     -1.923e-01  3.902e-02  -4.927
# lighttrapY                                                                   -7.283e+01  3.109e+01  -2.342
# light                                                                         4.944e-02  2.007e-02   2.463
# pherotrapY                                                                    0.000e+00  0.000e+00     NaN
# PM25                                                                          4.005e+00  2.694e+00   1.487
# NO2                                                                          -1.720e+00  3.097e+00  -0.555
# O3                                                                            1.664e+00  6.881e-01   2.418
# prec_c:tmean_c                                                               -4.378e-04  1.800e-04  -2.432
# prec_c:trang_c                                                                6.276e-04  2.520e-04   2.491
# tmean_c:trang_c                                                               1.321e-02  9.419e-03   1.402
# precipitation_anomaly:temp_mean_anomaly                                       3.717e-03  6.852e-04   5.424
# precipitation_anomaly:temp_range_anomaly                                     -5.850e-03  1.333e-03  -4.388
# temp_mean_anomaly:temp_range_anomaly                                          1.382e-01  1.812e-02   7.627
# precipitation_anomaly_april:temp_mean_anomaly_april                          -1.192e-03  5.510e-04  -2.163
# precipitation_anomaly_april:temp_range_anomaly_april                          2.733e-03  9.331e-04   2.929
# temp_mean_anomaly_april:temp_range_anomaly_april                              2.200e-02  1.462e-02   1.504
# lighttrapY:light                                                             -5.259e-02  3.063e-02  -1.717
# pherotrapY:PM25                                                              -4.502e+00  2.985e+00  -1.508
# pherotrapY:NO2                                                               -2.355e-01  3.573e+00  -0.066
# PM25:NO2                                                                      4.876e-01  2.859e-01   1.705
# pherotrapY:O3                                                                -1.553e+00  7.549e-01  -2.057
# PM25:O3                                                                      -7.699e-02  6.608e-02  -1.165
# NO2:O3                                                                        6.974e-02  7.595e-02   0.918
# prec_c:tmean_c:trang_c                                                       -4.112e-04  1.407e-04  -2.922
# precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                    2.497e-03  4.164e-04   5.995
# precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april -1.738e-03  3.831e-04  -4.537
# pherotrapY:PM25:NO2                                                          -2.791e-01  3.300e-01  -0.846
# pherotrapY:PM25:O3                                                            9.355e-02  7.296e-02   1.282
# pherotrapY:NO2:O3                                                            -1.576e-02  8.722e-02  -0.181
# PM25:NO2:O3                                                                  -1.422e-02  7.054e-03  -2.016
# pherotrapY:PM25:NO2:O3                                                        8.480e-03  8.101e-03   1.047
# Pr(>|t|)    
# (Intercept)                                                                  0.696588    
# prec_c                                                                       0.000701 ***
#   tmean_c                                                                      0.871595    
# trang_c                                                                      0.881441    
# precipitation_anomaly                                                        5.30e-10 ***
#   temp_mean_anomaly                                                            0.026537 *  
#   temp_range_anomaly                                                           0.011749 *  
#   precipitation_anomaly_april                                                  0.003377 ** 
#   temp_mean_anomaly_april                                                      0.001322 ** 
#   temp_range_anomaly_april                                                     8.94e-07 ***
#   lighttrapY                                                                   0.019255 *  
#   light                                                                        0.013857 *  
#   pherotrapY                                                                        NaN    
# PM25                                                                         0.137251    
# NO2                                                                          0.578842    
# O3                                                                           0.015683 *  
#   prec_c:tmean_c                                                               0.015080 *  
#   prec_c:trang_c                                                               0.012814 *  
#   tmean_c:trang_c                                                              0.160954    
# precipitation_anomaly:temp_mean_anomaly                                      6.42e-08 ***
#   precipitation_anomaly:temp_range_anomaly                                     1.20e-05 ***
#   temp_mean_anomaly:temp_range_anomaly                                         3.48e-14 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april                          0.030653 *  
#   precipitation_anomaly_april:temp_range_anomaly_april                         0.003430 ** 
#   temp_mean_anomaly_april:temp_range_anomaly_april                             0.132614    
# lighttrapY:light                                                             0.086105 .  
# pherotrapY:PM25                                                              0.131725    
# pherotrapY:NO2                                                               0.947451    
# PM25:NO2                                                                     0.088273 .  
# pherotrapY:O3                                                                0.039831 *  
#   PM25:O3                                                                      0.244113    
# NO2:O3                                                                       0.358553    
# prec_c:tmean_c:trang_c                                                       0.003507 ** 
#   precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly                   2.35e-09 ***
#   precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april 6.00e-06 ***
#   pherotrapY:PM25:NO2                                                          0.397861    
# pherotrapY:PM25:O3                                                           0.199858    
# pherotrapY:NO2:O3                                                            0.856635    
# PM25:NO2:O3                                                                  0.043875 *  
#   pherotrapY:PM25:NO2:O3                                                       0.295328    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(month)              3.907e+00  3.994 210.81  <2e-16 ***
#   s(Longitude,Latitude) 9.144e+00  9.776  25.91  <2e-16 ***
#   s(year)               4.311e-07  1.000   0.00  0.0113 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 54/55
# R-sq.(adj) =  0.596   Deviance explained = 60.4%
# -REML = 7937.4  Scale est. = 40.497    n = 2372


# Compute linear predictor describes log(count)
lp <- predict(m5, newdata = training, type = "link")
plot(lp, log(training$count), xlab = "Linear predictor",
     ylab = "log(count)")
abline(a = 0, b = 1)

## Validation
# with year 

# Model 4f, consider month, space, sampling weather, and trap type interact with year
summary(m4f <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
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

AIC(m4f)
## [1] 16349.73

Y <- predict(m3b, newdata = validation, type = "terms")
colnames(Y)

yvar <- c(1:12)
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

legend(-1, 200, legend = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"), box.col = "white",
       col = colfunc(27)[16:27], pch = 19)
text(0.5, 150, as.expression(bquote(rho == .(round(sp, 3)))))
text(0.5, 100, "p < 0.001")
dev.off()

# Replacing year with annual environmental dynamics 
X <- predict(m5, newdata = validation, type = "terms")
colnames(X)

# [1] "prec_c"                                                                      
# [2] "tmean_c"                                                                     
# [3] "trang_c"                                                                     
# [4] "precipitation_anomaly"                                                       
# [5] "temp_mean_anomaly"                                                           
# [6] "temp_range_anomaly"                                                          
# [7] "precipitation_anomaly_april"                                                 
# [8] "temp_mean_anomaly_april"                                                     
# [9] "temp_range_anomaly_april"                                                    
# [10] "lighttrap"                                                                   
# [11] "light"                                                                       
# [12] "pherotrap"                                                                   
# [13] "PM25"                                                                        
# [14] "NO2"                                                                         
# [15] "O3"                                                                          
# [16] "prec_c:tmean_c"                                                              
# [17] "prec_c:trang_c"                                                              
# [18] "tmean_c:trang_c"                                                             
# [19] "precipitation_anomaly:temp_mean_anomaly"                                     
# [20] "precipitation_anomaly:temp_range_anomaly"                                    
# [21] "temp_mean_anomaly:temp_range_anomaly"                                        
# [22] "precipitation_anomaly_april:temp_mean_anomaly_april"                         
# [23] "precipitation_anomaly_april:temp_range_anomaly_april"                        
# [24] "temp_mean_anomaly_april:temp_range_anomaly_april"                            
# [25] "lighttrap:light"                                                             
# [26] "pherotrap:PM25"                                                              
# [27] "pherotrap:NO2"                                                               
# [28] "PM25:NO2"                                                                    
# [29] "pherotrap:O3"                                                                
# [30] "PM25:O3"                                                                     
# [31] "NO2:O3"                                                                      
# [32] "prec_c:tmean_c:trang_c"                                                      
# [33] "precipitation_anomaly:temp_mean_anomaly:temp_range_anomaly"                  
# [34] "precipitation_anomaly_april:temp_mean_anomaly_april:temp_range_anomaly_april"
# [35] "pherotrap:PM25:NO2"                                                          
# [36] "pherotrap:PM25:O3"                                                           
# [37] "pherotrap:NO2:O3"                                                            
# [38] "PM25:NO2:O3"                                                                 
# [39] "pherotrap:PM25:NO2:O3"                                                       
# [40] "s(month)"                                                                    
# [41] "s(Longitude,Latitude)"    

# Annual dynamics predictors
fvar <- c(1:41)
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

# light and air quality only predictors
wvar <- c(1:4, 10:18, 25:32, 35:41)
validation$weather <- rowSums(X[, wvar])
pvalue(coin::spearman_test(weather ~ logct | Farm, data = validation))
# [1] <2.220446e-16

sp <- cor.test(~ weather + logct, data = validation, method = "spearman")$estimate

pdf("pl_both_weather_excl_validate.pdf", height=7, width=7)
plot(count ~ weather, log = "y", ylab = "log(Count per day)",
     xlab = "Linear combination weather conditions",
     data = validation,col = colfunc(27)[validation$year-1997], pch = 19)

legend(-3, 150, legend = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"), box.col = "white",
       col = colfunc(27)[16:27], pch = 19)
text(0, 150, as.expression(bquote(rho == .(round(sp, 3)))))
text(0, 100, "p < 0.001")
dev.off()



# Model 5x, consider month, space, sampling weather, and annual dynamics 
# (light trap interact with April anomaly, and pheromone trap interact with air quality)
# The best model is consider lowest AIC, however, not outperform m5 in validation (see below)
summary(m5x <- gam(count ~ s(month, k=5) + s(Longitude, Latitude, k=11, bs = "tp") +
                    prec_c*tmean_c*trang_c +
                    precipitation_anomaly*temp_mean_anomaly*temp_range_anomaly + 
                    lighttrap*precipitation_anomaly_april*temp_mean_anomaly_april*temp_range_anomaly_april +
                    light + pherotrap*PM25*NO2*O3,
                  family = gaussian(link = "log"), 
                  method = METHOD, 
                  data = training))

AIC(m5x)

# Replacing year with annual environmental dynamics 
X <- predict(m5x, newdata = validation, type = "terms")
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
fvar <- c(1:47)
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


