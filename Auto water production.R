library(lubridate)
library(dplyr)
data_record <- as.Date("2020-09-23")

setwd("D:/R/Solar Still")
WaterWeight <- read.table("Foam Bottom Interfacial test.txt", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
names(WaterWeight) <- c("TIMESTAMP", "Weight")
WaterWeight$TIMESTAMP <- round_date(ymd_hms(WaterWeight$TIMESTAMP), "minute")

Solar_climate <- read.csv(file = "CR1000_BSRN1000_Min200925.csv", skip = 1, stringsAsFactors = FALSE)
Solar_climate <- Solar_climate[c(-1, -2), -c(2, 14:19)] ##Delete two rows of unit, useless columns
Solar_climate$TIMESTAMP <- ymd_hms(Solar_climate$TIMESTAMP)
Solar_climate <- Solar_climate[date(Solar_climate$TIMESTAMP) >= data_record, ]
Solar_climate[, 2:length(Solar_climate)] <- lapply(Solar_climate[, 2:length(Solar_climate)], as.numeric)

WaterProduction <- data.frame(TIMESTAMP = WaterWeight$TIMESTAMP, Date = date(WaterWeight$TIMESTAMP), 
                         Time = as.numeric(WaterWeight$TIMESTAMP - as.POSIXct(date(WaterWeight$TIMESTAMP)))/3600, 
                         Weight = WaterWeight$Weight)
WaterProduction$Solar_Energy <- Solar_climate$Global_Energy_Day[Solar_climate$TIMESTAMP %in% WaterProduction$TIMESTAMP]
write.csv(WaterProduction, "auto water production.csv")

library(ggplot2)
g <- ggplot(WaterProduction, aes(TIMESTAMP, Solar_Energy))
g + geom_point()
p <- ggplot(WaterProduction, aes(TIMESTAMP, Weight))
p + geom_point()