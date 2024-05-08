library(raster)

list.filenames <- list.files(pattern="\\.tif$")

ext <- extent(-76, -74, 38, 40)

for (i in list.filenames){
  
  writeRaster(crop(projectRaster(raster(i), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), ext), paste(i,"_cropped"), format="GTiff", overwrite=TRUE)

} 

check <- raster(list.files(pattern="\\.tif _cropped.tif$")[1])

plot(check)
