# Start with count data
counts <- read.csv("Monthly_mean.csv")

# Make trap types two varibales
counts <- cbind(counts$type, counts)
colnames(counts) <- c("lighttrap", "pherotrap", "State", "Farm", "year", "month", "count")

counts$lighttrap <- as.factor(counts$lighttrap)
levels(counts$lighttrap) <- c("Y", "N")

counts$pherotrap <- as.factor(counts$pherotrap)
levels(counts$pherotrap) <- c("N", "Y")

# Add Space
coordinates <- read.csv("Sites.csv")

identical(sort(unique(counts$Farm)), sort(unique(coordinates$Farm)))
## True

master <- merge(counts, coordinates, by = "Farm")
master$plot_year <- paste(master$Farm, master$year, sep="_")
master$plot_year_month <- paste(master$plot_year, master$month, sep="_")

# Add Habitat
EVI_month <- read.csv("EVI_month.csv")
EVI_month_predict <- read.csv("EVI_month_predict.csv")
EVI <- rbind(EVI_month[,-1], EVI_month_predict[,-1])

colnames(EVI)[-c(1:2)]
paste(EVI[,1], EVI[,2], sep="_")

xx <- as.matrix(EVI[1:135, 3:48])
colnames(xx) <- colnames(EVI)[-c(1:2)]
row.names(xx) <- paste(EVI[,2], EVI[,1], sep="_")
habitat <- data.frame(year_month=rep(row.names(xx),ncol(xx)),
                 Farm=rep(colnames(xx),each=nrow(xx)),
                 EVI=as.vector(xx))

habitat$plot_year_month <- paste(habitat$Farm, habitat$year_month, sep="_")

master <- merge(master, habitat[, 3:4], by="plot_year_month")

# Add sampling weather
list.filenames <- list.files(pattern="\\_final.csv$")
sampling_weather <- do.call(cbind, lapply(list.filenames, read.csv))
sampling_weather <- sampling_weather[, c(2, 3, 4, 5, 6*(1:5))]
sampling_weather$plot_year_month <- paste(sampling_weather$State, sampling_weather$Farm, sampling_weather$Year, sampling_weather$month, sep="_")
master <- merge(master, sampling_weather[, -(1:4)], by="plot_year_month")

# Add light annual data
VIIRS <- read.csv("VIIRS_annual.csv")
DMSP <- read.csv("DMSP_VIIRS.csv")
light <- rbind(DMSP[,-1], VIIRS[,-1])

xx <- as.matrix(light[1:28, 1:46])
colnames(xx) <- colnames(light)[1:46]
row.names(xx) <- light$year
lightx <- data.frame(year=rep(row.names(xx),ncol(xx)),
                      plot=rep(colnames(xx),each=nrow(xx)),
                      light=as.vector(xx))

lightx$plot_year <- paste(lightx$plot, lightx$year, sep="_")
master <- merge(master, lightx[, -(1:2)], by="plot_year")

# Add air quality
PM25 <- read.csv("PM25.csv")
PM25 <- PM25[,-1]
xx <- as.matrix(PM25[1:28, 1:46])
colnames(xx) <- colnames(PM25)[1:46]
row.names(xx) <- PM25$year
PM25x <- data.frame(year=rep(row.names(xx),ncol(xx)),
                     plot=rep(colnames(xx),each=nrow(xx)),
                     PM25=as.vector(xx))

PM25x$plot_year <- paste(PM25x$plot, PM25x$year, sep="_")
master <- merge(master, PM25x[, -(1:2)], by="plot_year")


O3 <- read.csv("O3.csv")
O3 <- O3[, -1]

xx <- as.matrix(O3[1:28, 1:46])
colnames(xx) <- colnames(O3)[1:46]
row.names(xx) <- O3$year
O3x <- data.frame(year=rep(row.names(xx),ncol(xx)),
                    plot=rep(colnames(xx),each=nrow(xx)),
                    O3=as.vector(xx))

O3x$plot_year <- paste(O3x$plot, O3x$year, sep="_")
master <- merge(master, O3x[, -(1:2)], by="plot_year")


NO2 <- read.csv("NO2.csv")
NO2 <- NO2[, -1]

xx <- as.matrix(NO2[1:28, 1:46])
colnames(xx) <- colnames(NO2)[1:46]
row.names(xx) <- NO2$year
NO2x <- data.frame(year=rep(row.names(xx),ncol(xx)),
                    plot=rep(colnames(xx),each=nrow(xx)),
                    NO2=as.vector(xx))

NO2x$plot_year <- paste(NO2x$plot, NO2x$year, sep="_")
master <- merge(master, NO2x[, -(1:2)], by="plot_year")

# Winter anomaly
list.filenamez <- list.files(pattern="\\_anomaly.csv$")
weather_anomaly <- do.call(cbind, lapply(list.filenamez, read.csv))
weather_anomaly <- weather_anomaly[, c(2, 3, 4*(1:5))]
weather_anomaly$plot_year <- paste(weather_anomaly$Farm, weather_anomaly$Year, sep="_")
master <- merge(master, weather_anomaly[, -(1:2)], by="plot_year")

# April anomaly
list.filenamezz <- list.files(pattern="\\_anomaly_april.csv$")
weather_anomaly <- do.call(cbind, lapply(list.filenamezz, read.csv))
weather_anomaly <- weather_anomaly[, c(2, 3, 4*(1:5))]
weather_anomaly$plot_year <- paste(weather_anomaly$Farm, weather_anomaly$Year, sep="_")
master <- merge(master, weather_anomaly[, -(1:2)], by="plot_year")

# Convert month from names (characters) to the days in year (numeric).
master$month <- as.factor(master$month)
levels(master$month) <- c(228, 197, 167, 136, 258)
master$month <- as.numeric(as.character(master$month))

# Output final build
write.csv(master, "master.csv")
