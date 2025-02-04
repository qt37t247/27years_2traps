## Working directory should be the same of "GEE_collect.ipynb", where all the environmental variables were extracted with Google Earth Engine.

######################### Testing conversion between DMSP and VIIRS ###########################
# # Light data check
# DMSP <- read.csv("DMSP_edit.csv")
# VIIRS <- read.csv("VIIRS_edit.csv")
# 
# DMSPx <- DMSP[,3:4]
# VIIRSx <- VIIRS[,2:3]
# colnames(DMSPx) <- colnames(VIIRSx)
# check <- rbind(DMSPx, VIIRSx)
# 
# plot(check, col="white")
# points(DMSPx, col="red")
# abline(lm(Annual ~ Time, data = DMSPx), col="red")
# # y = 0.2409x – 461.9802 
# 
# points(VIIRSx, col="blue")
# abline(lm(Annual ~ Time, data = VIIRSx), col="blue") 
# # y = 0.0483x – 94.9962 
# 
# # Decide to use VIIRS with a DMSP conversion (Chen et al., 2019)
######################### End of the test ####################################################

###################################### Light #################################################
VIIRS <- read.csv("VIIRS.csv")
DMSP <- read.csv("DMSP.csv")
row.names(VIIRS) <- VIIRS$Farm
row.names(DMSP) <- DMSP$Farm
VIIRS <- VIIRS[, -c(130:131)]
DMSP <- DMSP[, -c(18:19)]

colfunc <- colorRampPalette(c("red", "black"))

plot(-1, xlim=c(0, 129), ylim=c(min(VIIRS), max(VIIRS)), col="white")
for (i in 1:129) {
  points(rep(i, 46), VIIRS[, i])
  points(rep(i, 46), VIIRS[, i], col=colfunc(12)[i%%12])
}


VIIRS_annual <- data.frame(matrix(nrow = 11, ncol = 46))
row.names(VIIRS_annual) <- 2014:2024
colnames(VIIRS_annual) <- row.names(VIIRS)

for (i in 1:46) {
  
  for (j in 1:10) {
    
    VIIRS_annual[j, i] <- mean(as.numeric(VIIRS[i, (12*j-11):(12*j)]))  
    
  }
  
}

for (i in 1:46) {
    
    VIIRS_annual[11, i] <- mean(as.numeric(VIIRS[i, 120:129]))  
    
}

VIIRS_annual$year <- 2014:2024

# tried to extrapolate with linear regression
#VIIRS_predict <- data.frame(matrix(nrow = 17, ncol = 47))
#row.names(VIIRS_predict) <- 1997:2013
#colnames(VIIRS_predict) <- colnames(VIIRS_annual)
#VIIRS_predict$year <- 1997:2013

#for (i in 1:46){
  
#  VIIRS_predict[, i] <- lm(VIIRS_annual[, i] ~ VIIRS_annual$year)$coefficients[1] + lm(VIIRS_annual[, i] ~ VIIRS_annual$year)$coefficients[2]*VIIRS_predict$year
  
#}

# Also used equations to convert DMSP to VIIRS

DMSP_VIIRS <- as.data.frame(t(DMSP*0.1249074+0.157486))
DMSP_VIIRS$year <- 1997:2013

colfunc <- colorRampPalette(c("red", "black", "green"))

plot("Light", xlim=c(1997, 2025), ylim=c(min(DMSP_VIIRS[,-47]), max(VIIRS_annual[,-47])))

for (i in 1:46){
  
  points(VIIRS_annual$year, VIIRS_annual[,i], cex=1, pch=19, col=colfunc(46)[i])
  #points(VIIRS_predict$year, VIIRS_predict[,i], cex=1, pch=21, col=colfunc(18)[i])
  points(DMSP_VIIRS$year, DMSP_VIIRS[,i], cex=1, pch=17, col=colfunc(46)[i])
  #abline(lm(VIIRS_annual[,i] ~ VIIRS_annual$year), col=colfunc(18)[i]) 
  
}

legend("topleft", legend=colnames(VIIRS_annual)[1:46], col=colfunc(46), pch=19, bty="n",ncol=3,cex=0.7,pt.cex=0.7)

write.csv(VIIRS_annual, "VIIRS_annual.csv")

#write.csv(light_predict, "light_predict.csv")

write.csv(DMSP_VIIRS, "DMSP_VIIRS.csv")
########################################## Light ends here #######################################

########################################## Habitat (EVI) ########################################
EVI <- read.csv("EVI.csv")
row.names(EVI) <- EVI$Farm
EVI <- EVI[, -c(573:574)]

colfunc <- colorRampPalette(c("red", "black"))

plot(-2000, xlim=c(0, 572), ylim=c(min(EVI, na.rm = T), max(EVI, na.rm = T)))
for (i in 21:572) {
  points(rep(i, 46), EVI[, i])
  points(rep(i, 46), EVI[, i], col=colfunc(23)[i%%23])
}
for (i in 1:20) {
  points(rep(i, 46), EVI[, i])
  points(rep(i, 46), EVI[, i], col=colfunc(23)[i%%23])
}

# To make it 25 data points per year for 25 years 
EVI_makeup <- data.frame(matrix(nrow = 46, ncol = 3))
row.names(EVI_makeup) <- row.names(EVI)
colnames(EVI_makeup) <- c("empty2000_0101", "empty2000_0115", "empty2000_0201")
EVI_complete <- cbind(EVI_makeup, EVI)

EVI_month <- data.frame(matrix(nrow = 25*5, ncol = 48))
colnames(EVI_month) <- c("Month", "Year", row.names(EVI))
EVI_month[, 1:2] <- expand.grid(c("May", "Jun", "Jul", "Aug", "Sep"), 2000:2024)
 
for (i in 1:46) {
   
  for (j in 1:25) {
    
   EVI_month[5*j-4, i+2] <- mean(as.numeric(EVI_complete[i, (23*j-14):(23*j-13)]), na.rm = TRUE)  
   EVI_month[5*j-3, i+2] <- mean(as.numeric(EVI_complete[i, (23*j-12):(23*j-11)]), na.rm = TRUE)
   EVI_month[5*j-2, i+2] <- mean(as.numeric(EVI_complete[i, (23*j-10):(23*j-9)]), na.rm = TRUE)
   EVI_month[5*j-1, i+2] <- mean(as.numeric(EVI_complete[i, (23*j-8):(23*j-7)]), na.rm = TRUE)
   EVI_month[5*j, i+2] <- mean(as.numeric(EVI_complete[i, (23*j-6):(23*j-5)]), na.rm = TRUE)

     }
   
 }

colfunc <- colorRampPalette(c("red", "green", "blue"))

plot(-2000, xlim=c(0, 125), ylim=c(min(EVI, na.rm = T), max(EVI, na.rm = T)))
for (i in 1:125) {
  points(rep(i, 46), EVI_month[i, -(1:2)])
  points(rep(i, 46), EVI_month[i, -(1:2)], col=colfunc(5)[i%%5])
}

write.csv(EVI_month, "EVI_month.csv")

# extrapolate for year 1998 and 1999
EVI_month_predict <- EVI_month[1:10, ]
EVI_month_predict$Year <- c(rep(1998, 5), rep(1999, 5))

EVI_May <- EVI_month[which(EVI_month$Month=="May"),]
EVI_Jun <- EVI_month[which(EVI_month$Month=="Jun"),]
EVI_Jul <- EVI_month[which(EVI_month$Month=="Jul"),]
EVI_Aug <- EVI_month[which(EVI_month$Month=="Aug"),]
EVI_Sep <- EVI_month[which(EVI_month$Month=="Sep"),]

for (i in 1:46){
   
  EVI_month_predict[1, i+2] <- lm(EVI_May[, i+2] ~ EVI_May$Year)$coefficients[1] + lm(EVI_May[, i+2] ~ EVI_May$Year)$coefficients[2]*1998
  EVI_month_predict[2, i+2] <- lm(EVI_Jun[, i+2] ~ EVI_Jun$Year)$coefficients[1] + lm(EVI_Jun[, i+2] ~ EVI_Jun$Year)$coefficients[2]*1998
  EVI_month_predict[3, i+2] <- lm(EVI_Jul[, i+2] ~ EVI_Jul$Year)$coefficients[1] + lm(EVI_Jul[, i+2] ~ EVI_Jul$Year)$coefficients[2]*1998
  EVI_month_predict[4, i+2] <- lm(EVI_Aug[, i+2] ~ EVI_Aug$Year)$coefficients[1] + lm(EVI_Aug[, i+2] ~ EVI_Aug$Year)$coefficients[2]*1998
  EVI_month_predict[5, i+2] <- lm(EVI_Sep[, i+2] ~ EVI_Sep$Year)$coefficients[1] + lm(EVI_Sep[, i+2] ~ EVI_Sep$Year)$coefficients[2]*1998
  EVI_month_predict[6, i+2] <- lm(EVI_May[, i+2] ~ EVI_May$Year)$coefficients[1] + lm(EVI_May[, i+2] ~ EVI_May$Year)$coefficients[2]*1999
  EVI_month_predict[7, i+2] <- lm(EVI_Jun[, i+2] ~ EVI_Jun$Year)$coefficients[1] + lm(EVI_Jun[, i+2] ~ EVI_Jun$Year)$coefficients[2]*1999
  EVI_month_predict[8, i+2] <- lm(EVI_Jul[, i+2] ~ EVI_Jul$Year)$coefficients[1] + lm(EVI_Jul[, i+2] ~ EVI_Jul$Year)$coefficients[2]*1999
  EVI_month_predict[9, i+2] <- lm(EVI_Aug[, i+2] ~ EVI_Aug$Year)$coefficients[1] + lm(EVI_Aug[, i+2] ~ EVI_Aug$Year)$coefficients[2]*1999
  EVI_month_predict[10, i+2] <- lm(EVI_Sep[, i+2] ~ EVI_Sep$Year)$coefficients[1] + lm(EVI_Sep[, i+2] ~ EVI_Sep$Year)$coefficients[2]*1999
  
}

plot(-2000, xlim=c(0, 135), ylim=c(min(EVI, na.rm = T), max(EVI, na.rm = T)))
for (i in 1:10) {
  points(rep(i, 46), EVI_month_predict[i, -(1:2)])
  points(rep(i, 46), EVI_month_predict[i, -(1:2)], col=colfunc(5)[i%%5])
}
for (i in 1:125) {
  points(rep(i+10, 46), EVI_month[i, -(1:2)])
  points(rep(i+10, 46), EVI_month[i, -(1:2)], col=colfunc(5)[i%%5])
}

write.csv(EVI_month_predict, "EVI_month_predict.csv")

## Deprecated, calculate monthly data instead.
# EVI_median <- data.frame(matrix(nrow = 23, ncol = 18))
# row.names(EVI_median) <- 2000:2022
# colnames(EVI_median) <- row.names(EVI)
# 
# EVI_interquartile <- EVI_median
# EVI_mean <- EVI_median
# EVI_deviation <- EVI_median
# 
# for (i in 1:18) {
#   
#  for (j in 1:23) {
#     
#    EVI_median[j, i] <- median(as.numeric(EVI_complete[i, (23*j-22):(23*j)]), na.rm = TRUE)  
#     
#  }
#   
# }
# 
# for (i in 1:18) {
#   
#  for (j in 1:23) {
#     
#    EVI_mean[j, i] <- mean(as.numeric(EVI_complete[i, (23*j-22):(23*j)]), na.rm = TRUE)  
#     
#  }
#   
# }
# 
# for (i in 1:18) {
#   
#  for (j in 1:23) {
#     
#     EVI_interquartile[j, i] <- IQR(as.numeric(EVI_complete[i, (23*j-22):(23*j)]), na.rm = TRUE)  
#     
#   }
#   
# }
# 
# for (i in 1:18) {
#   
#   for (j in 1:23) {
#     
#     EVI_deviation[j, i] <- sd(as.numeric(EVI_complete[i, (23*j-22):(23*j)]), na.rm = TRUE)  
#     
#   }
#   
# }
# 
# EVI_mean$year <- 2000:2022
# EVI_median$year <- 2000:2022
# EVI_interquartile$year <- 2000:2022
# EVI_deviation$year <- 2000:2022
# 
# EVI_median_predict <- data.frame(matrix(nrow = 3, ncol = 19))
# row.names(EVI_median_predict) <- 1997:1999
# colnames(EVI_median_predict) <- colnames(EVI_median)
# EVI_median_predict$year <- 1997:1999
# 
# EVI_mean_predict <- data.frame(matrix(nrow = 3, ncol = 19))
# row.names(EVI_mean_predict) <- 1997:1999
# colnames(EVI_mean_predict) <- colnames(EVI_mean)
# EVI_mean_predict$year <- 1997:1999
# 
# EVI_interquartile_predict <- data.frame(matrix(nrow = 3, ncol = 19))
# row.names(EVI_interquartile_predict) <- 1997:1999
# colnames(EVI_interquartile_predict) <- colnames(EVI_interquartile)
# EVI_interquartile_predict$year <- 1997:1999
# 
# EVI_deviation_predict <- data.frame(matrix(nrow = 3, ncol = 19))
# row.names(EVI_deviation_predict) <- 1997:1999
# colnames(EVI_deviation_predict) <- colnames(EVI_deviation)
# EVI_deviation_predict$year <- 1997:1999
# 
# for (i in 1:18){
#   
#   EVI_median_predict[, i] <- lm(EVI_median[, i] ~ EVI_median$year)$coefficients[1] + lm(EVI_median[, i] ~ EVI_median$year)$coefficients[2]*EVI_median_predict$year
#   
# }
# 
# colfunc <- colorRampPalette(c("red", "black", "green"))
# 
# plot("EVI_median", xlim=c(1997, 2023), ylim=c(min(EVI_median[,-19]), max(EVI_median[,-19])))
# 
# for (i in 1:18){
#   
#   points(EVI_median$year, EVI_median[,i], cex=0.7, pch=19, col=colfunc(18)[i])
#   points(EVI_median_predict$year, EVI_median_predict[,i], cex=0.7, pch=21, col=colfunc(18)[i])
#   abline(lm(EVI_median[,i] ~ EVI_median$year), col=colfunc(18)[i]) 
#   
# }
# 
# write.csv(EVI_median, "EVI_median.csv")
# 
# write.csv(EVI_median_predict, "EVI_median_predict.csv")
# 
# 
# for (i in 1:18){
#   
#   EVI_mean_predict[, i] <- lm(EVI_mean[, i] ~ EVI_mean$year)$coefficients[1] + lm(EVI_mean[, i] ~ EVI_mean$year)$coefficients[2]*EVI_mean_predict$year
#   
# }
# 
# colfunc <- colorRampPalette(c("red", "black", "green"))
# 
# plot("EVI_mean", xlim=c(1997, 2023), ylim=c(min(EVI_mean[,-19]), max(EVI_mean[,-19])))
# 
# for (i in 1:18){
#   
#   points(EVI_mean$year, EVI_mean[,i], cex=0.7, pch=19, col=colfunc(18)[i])
#   points(EVI_mean_predict$year, EVI_mean_predict[,i], cex=0.7, pch=21, col=colfunc(18)[i])
#   abline(lm(EVI_mean[,i] ~ EVI_mean$year), col=colfunc(18)[i]) 
#   
# }
# 
# write.csv(EVI_mean, "EVI_mean.csv")
# 
# write.csv(EVI_mean_predict, "EVI_mean_predict.csv")
# 
# 
# for (i in 1:18){
#   
#   EVI_interquartile_predict[, i] <- lm(EVI_interquartile[, i] ~ EVI_interquartile$year)$coefficients[1] + lm(EVI_interquartile[, i] ~ EVI_interquartile$year)$coefficients[2]*EVI_interquartile_predict$year
#   
# }
# 
# colfunc <- colorRampPalette(c("red", "black", "green"))
# 
# plot("EVI_interquartile", xlim=c(1997, 2023), ylim=c(min(EVI_interquartile[,-19]), max(EVI_interquartile[,-19])))
# 
# for (i in 1:18){
#   
#   points(EVI_interquartile$year, EVI_interquartile[,i], cex=0.7, pch=19, col=colfunc(18)[i])
#   points(EVI_interquartile_predict$year, EVI_interquartile_predict[,i], cex=0.7, pch=21, col=colfunc(18)[i])
#   abline(lm(EVI_interquartile[,i] ~ EVI_interquartile$year), col=colfunc(18)[i]) 
#   
# }
# 
# write.csv(EVI_interquartile, "EVI_interquartile.csv")
# 
# write.csv(EVI_interquartile_predict, "EVI_interquartile_predict.csv")
# 
# 
# for (i in 1:18){
#   
#   EVI_deviation_predict[, i] <- lm(EVI_deviation[, i] ~ EVI_deviation$year)$coefficients[1] + lm(EVI_deviation[, i] ~ EVI_deviation$year)$coefficients[2]*EVI_deviation_predict$year
#   
# }
# 
# colfunc <- colorRampPalette(c("red", "black", "green"))
# 
# plot("EVI_deviation", xlim=c(1997, 2023), ylim=c(min(EVI_deviation[,-19]), max(EVI_deviation[,-19])))
# 
# for (i in 1:18){
#   
#   points(EVI_deviation$year, EVI_deviation[,i], cex=0.7, pch=19, col=colfunc(18)[i])
#   points(EVI_deviation_predict$year, EVI_deviation_predict[,i], cex=0.7, pch=21, col=colfunc(18)[i])
#   abline(lm(EVI_deviation[,i] ~ EVI_deviation$year), col=colfunc(18)[i]) 
#   
# }
# 
# write.csv(EVI_deviation, "EVI_deviation.csv")
# 
# write.csv(EVI_deviation_predict, "EVI_deviation_predict.csv")

################################### Habitat ends here ##################################################

############################################ Climate ###################################################
## first check the 30-year normal of each month at sites
list.filenames <- list.files(pattern="\\_normal.csv$")
clim_long <- data.frame(year=rep(1960:2020, each=12), month=rep(1:12, times=61))

for (i in 1:length(list.filenames)){
  
  clim_long[, 2+i] <- colMeans(read.csv(list.filenames[i])[, c(1:732)])
  
} 

colnames(clim_long) <- c("year", "month", list.filenames)

clim_long$temp_range_normal <- clim_long$temp_max_normal.csv - clim_long$temp_min_normal.csv

clim_6090 <- data.frame()

for (i in 3:7) {
  
  for (j in 1:12) {
    
    clim_6090[j ,i-2] <- mean(clim_long[1:(12*31), i][clim_long$month==j], na.rm=TRUE)
    
  }
  
}
colnames(clim_6090) <- list.filenames

clim_7000 <- data.frame()

for (i in 3:7) {
  
  for (j in 1:12) {
    
    clim_7000[j ,i-2] <- mean(clim_long[121:(12*31+120), i][clim_long$month==j], na.rm=TRUE)
    
  }
  
}
colnames(clim_7000) <- list.filenames

clim_8010 <- data.frame()

for (i in 3:7) {
  
  for (j in 1:12) {
    
    clim_8010[j ,i-2] <- mean(clim_long[241:(12*31+240), i][clim_long$month==j], na.rm=TRUE)
    
  }
  
}
colnames(clim_8010) <- list.filenames

clim_9020 <- data.frame()

for (i in 3:7) {
  
  for (j in 1:12) {
    
    clim_9020[j ,i-2] <- mean(clim_long[361:(12*31+360), i][clim_long$month==j], na.rm=TRUE)
    
  }
  
}
colnames(clim_9020) <- list.filenames

df_6090 <- clim_6090 - clim_6090
df_7000 <- clim_7000 - clim_6090
df_8010 <- clim_8010 - clim_6090
df_9020 <- clim_9020 - clim_6090

df_all <- rbind(df_6090, df_7000, df_8010, df_9020)

colfunc <- colorRampPalette(c("blue", "red", "green"))

par(mar=c(0.1,0.1,0.1,0.1))
par(mfrow=c(5,12))

for (i in 1:5) {
  
  for (j in 1:12) {
    
    plot(1:4, c(0, df_7000[j,i], df_8010[j,i], df_9020[j,i]), xlim=c(1,4), ylim=c(min(df_all[,i]), max(df_all[,i])), type="b", pch=19, col=colfunc(5)[i], axes=FALSE, ylab="", xlab="")
    abline(h=0, lty=2)
    box()
    
  }
  
}

# Calculate winter (December, January, Feburary) anomaly comparing sampling period with 30-year normal (1990 - 2020)  

for (i in 1:5) {
  
  clim_9020[13, i] <- mean(clim_9020[c(1, 2, 12), i]) 
  
}

clim_9020$month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "winter")

list.filenamex <- sub("_normal", "", list.filenames)
clim <- lapply(list.filenamex, read.csv)

## Compute winter anomaly

for (n in 1:4) {
  
  for (i in 1:46) {
    
    for (j in 1:27) {
      
      clim[[n]][i, 338+j] <- mean(as.numeric(clim[[n]][i, (12*j):(12*j+2)])) - clim_9020[13, n]
      
    }
    
  }
  
}

list.filenamea <- sub("_normal", "_anomaly", list.filenames)

colfunc <- colorRampPalette(c("blue", "yellow", "brown4"))

par(mar=c(2,2,2,2))
par(mfrow=c(2,2))

for (i in 1:4) {
  
  xx <- as.matrix(clim[[i]][1:46, 339:365])
  colnames(xx) <- 1998:2024
  row.names(xx) <- clim[[i]]$Farm
  yy <- data.frame(Farm=rep(row.names(xx),ncol(xx)),
                   Year=rep(colnames(xx),each=nrow(xx)),
                   Anomaly=as.vector(xx))
  
  bo <- boxplot(Anomaly ~ Year, 
                xlab = "Year", ylab = "Anomaly", data = yy, 
                axes = TRUE, col = colfunc(27))
  abline(h=0, lty=2)
  write.csv(yy, list.filenamea[i])
  
}

temp_wa_range <- cbind(read.csv("temp_max_anomaly.csv"), read.csv("temp_min_anomaly.csv"))

temp_wa_range[, 5] <- temp_wa_range[, 4] - temp_wa_range[, 8]

temp_wa_range <- temp_wa_range[, c(2, 3, 5)]

colnames(temp_wa_range) <- c("Farm", "Year", "Anomaly")

bo <- boxplot(Anomaly ~ Year, 
              xlab = "Year", ylab = "Anomaly", data = temp_wa_range, 
              axes = TRUE, col = colfunc(27))
abline(h=0, lty=2)

write.csv(temp_wa_range, "temp_range_anomaly.csv")

## Compute April anomaly

for (n in 1:4) {
  
  for (i in 1:46) {
    
    for (j in 1:27) {
      
      clim[[n]][i, 365+j] <- as.numeric(clim[[n]][i, 4+(12*j)]) - clim_9020[4, n]
      
    }
    
  }
  
}

list.filenameaa <- sub("_normal", "_anomaly_april", list.filenames)

colfunc <- colorRampPalette(c("blue", "yellow", "brown4"))

par(mar=c(2,2,2,2))
par(mfrow=c(2,2))

for (i in 1:4) {
  
  xx <- as.matrix(clim[[i]][1:46, 366:392])
  colnames(xx) <- 1998:2024
  row.names(xx) <- clim[[i]]$Farm
  yy <- data.frame(Farm=rep(row.names(xx),ncol(xx)),
                   Year=rep(colnames(xx),each=nrow(xx)),
                   Anomaly=as.vector(xx))
  
  bo <- boxplot(Anomaly ~ Year, 
                xlab = "Year", ylab = "Anomaly", data = yy, 
                axes = TRUE, col = colfunc(27))
  abline(h=0, lty=2)
  write.csv(yy, list.filenameaa[i])
  
}

temp_aa_range <- cbind(read.csv("temp_max_anomaly_april.csv"), read.csv("temp_min_anomaly_april.csv"))

temp_aa_range[, 5] <- temp_aa_range[, 4] - temp_aa_range[, 8]

temp_aa_range <- temp_aa_range[, c(2, 3, 5)]

colnames(temp_aa_range) <- c("Farm", "Year", "Anomaly")

bo <- boxplot(Anomaly ~ Year, 
              xlab = "Year", ylab = "Anomaly", data = temp_aa_range, 
              axes = TRUE, col = colfunc(27))
abline(h=0, lty=2)

write.csv(temp_aa_range, "temp_range_anomaly_april.csv")

# Output sampling climate (May, Jun, Jul, Aug, Sep)

list.filenamef <- sub("_normal", "_final", list.filenames)

for (i in 1:4) {
  
  xx <- as.matrix(clim[[i]][1:46, 1:336])
  colnames(xx) <- format(seq(as.Date("1997-01-01"), as.Date("2024-12-31"), 
                             by = 'month'), '%Y-%m')
  row.names(xx) <- clim[[i]]$Farm
  yy <- data.frame(Farm=rep(row.names(xx),ncol(xx)),
                   Year_month=rep(colnames(xx),each=nrow(xx)),
                   climate=as.vector(xx))
  
  write.csv(yy, list.filenamef[i])
  
}

temp_range <- cbind(read.csv("temp_max_final.csv"), read.csv("temp_min_final.csv"))

temp_range[, 5] <- temp_range[, 4] - temp_range[, 8]

temp_range <- temp_range[, c(2, 3, 5)]

colnames(temp_range) <- c("Farm", "Year_month", "Climate")

write.csv(temp_range, "temp_range_final.csv")

################################################ Climate ends here ########################################

