library(raster)

## Working directory is a folder contains all the rasters of one environmental variable

# Read all the rasters downloaded
list.filenames <- list.files(pattern="\\.tif$")

# set extent to focal region
ext <- extent(-76, -74, 38, 42)

for (i in list.filenames){
  
  writeRaster(crop(projectRaster(raster(i), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), ext), paste(i,"_cropped"), format="GTiff", overwrite=TRUE)

} 

check <- raster(list.files(pattern="\\.tif _cropped.tif$")[1])

plot(check)

# Read cropped rasters
list.filenames <- list.files(pattern="\\ _cropped.tif$")

allrasters <- stack(list.filenames)

# Read in geo-coordinates of the traps
loc <- read.csv("Sites.csv")
xy <- loc[, c(3,2)]
spdf <- SpatialPointsDataFrame(coords = xy, data = loc, proj4string = crs(allrasters))

site_var <- as.data.frame(t(extract(allrasters, spdf)))

colnames(site_var) <- loc$Farm

row.names(site_var) <- c(2000, 2005:2020)

site_var$year <- c(2000, 2005:2020)

site_predict <- site_var[1:11, 1:47]

site_predict$year <- c(1997:1999, 2001:2004, 2021:2024)

# interpolate/extrapolate values in years that not available in the dataset
for (i in 1:47){
  
  site_predict[, i] <- lm(site_var[, i] ~ site_var$year)$coefficients[1] + lm(site_var[, i] ~ site_var$year)$coefficients[2]*site_predict$year
  
}

colfunc <- colorRampPalette(c("red", "black", "green"))

plot("NO2", xlim=c(1997, 2024), ylim=c(min(site_predict[,-47]), max(site_predict[,-47])))

for (i in 1:46){
  
  points(site_var$year, site_var[,i], cex=0.7, pch=19, col=colfunc(18)[i])
  points(site_predict$year, site_predict[,i], cex=0.7, pch=21, col=colfunc(18)[i])
  abline(lm(site_var[,i] ~ site_var$year), col=colfunc(18)[i]) 
  
}

legend("topright",legend=colnames(site_var)[1:46], col=colfunc(46), pch=19, bty="n",ncol=4,cex=0.7,pt.cex=0.7)

site_var <- rbind(site_var, site_predict)

write.csv(site_var, "NO2_site_var.csv")

