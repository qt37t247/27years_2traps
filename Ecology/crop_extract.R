library(raster)

list.filenames <- list.files(pattern="\\ _cropped.tif$")

allrasters <- stack(list.filenames)

# rerasterize

ext <- extent(-76, -74, 38, 42)

empty <- raster("prism_ppt_2020.tif")
empty <- crop(projectRaster(empty, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), ext)
empty <- raster(empty) 

newrasters <- resample(allrasters, empty, method='bilinear')

# Read in geo-coordinates of the traps
loc <- read.csv("Sites.csv")
xy <- loc[, c(3,2)]
spdf <- SpatialPointsDataFrame(coords = xy, data = loc, proj4string = crs(allrasters))

site_var <- as.data.frame(t(extract(newrasters, spdf)))

colnames(site_var) <- loc$Farm

site_var$year <- c(2000, 2005:2020)

site_predict <- site_var[1:11, 1:46]

site_predict$year <- c(1997:1999, 2001:2004, 2021:2024)

# interpolate/extrapolate values in years that not available in the dataset
for (i in 1:46){
  
  site_predict[, i] <- lm(site_var[, i] ~ site_var$year)$coefficients[1] + lm(site_var[, i] ~ site_var$year)$coefficients[2]*site_predict$year
  
}

colnames(site_predict) <- colnames(site_var)

site_var <- rbind(site_var, site_predict)

colfunc <- colorRampPalette(c("red", "black", "green"))

plot("O3", xlim=c(1997, 2024), ylim=c(min(site_var[,-47]), max(site_var[,-47])))

for (i in 1:47){
  
  points(site_var$year, site_var[,i], cex=0.7, pch=19, col=colfunc(47)[i])
  abline(lm(site_var[,i] ~ site_var$year), col=colfunc(47)[i]) 
  
}

#legend("topright",legend=colnames(site_var)[1:47], col=colfunc(47), pch=19, bty="n",ncol=4,cex=0.7,pt.cex=0.7)



write.csv(site_var, "O3_site_var.csv")

