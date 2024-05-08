library(raster)

list.filenames <- list.files(pattern="\\.tif _cropped.tif$")

allrasters <- stack(list.filenames)

loc <- read.csv("Sites.csv")
xy <- loc[, c(3,2)]
spdf <- SpatialPointsDataFrame(coords = xy, data = loc, proj4string = crs(allrasters))

site_var <- as.data.frame(t(extract(allrasters, spdf)))

colnames(site_var) <- loc$Farm

row.names(site_var) <- 2000:2016

site_var$year <- 2000:2016

site_predict <- site_var[1:9, 1:19]

site_predict$year <- c(1997:1999, 2017:2022)

for (i in 1:18){
  
  site_predict[, i] <- lm(site_var[, i] ~ site_var$year)$coefficients[1] + lm(site_var[, i] ~ site_var$year)$coefficients[2]*site_predict$year
  
}

colfunc <- colorRampPalette(c("red", "black", "green"))

plot("PM2.5", xlim=c(1997, 2023), ylim=c(min(site_predict[,-19]), max(site_predict[,-19])))

for (i in 1:18){
  
  points(site_var$year, site_var[,i], cex=0.7, pch=19, col=colfunc(18)[i])
  points(site_predict$year, site_predict[,i], cex=0.7, pch=21, col=colfunc(18)[i])
  abline(lm(site_var[,i] ~ site_var$year), col=colfunc(18)[i]) 

}

legend("topright",legend=colnames(site_var)[1:18], col=colfunc(18), pch=19, bty="n",ncol=2,cex=0.7,pt.cex=0.7)

write.csv(site_var, "site_var.csv")

write.csv(site_predict, "site_predict.csv")
