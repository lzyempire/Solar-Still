##Get solar still water hourly production data
setwd("D:/R/Solar Still")
library(lubridate)
library(dplyr)
data_record <- as.Date("2020-06-05")
energy_record <- 5
power_record <- 0

SolarEnv_Day <- read.csv(file = "CR1000_BSRN1000_Day201116.csv", skip = 1, stringsAsFactors = FALSE)
SolarEnv_Day <- SolarEnv_Day[c(-1, -2), -(24:39)] ##Delete two rows of unit, 24-39 colunm
SolarEnv_Day$TIMESTAMP <- as.Date(ymd_hms(SolarEnv_Day$TIMESTAMP))
SolarEnv_Day <- SolarEnv_Day[SolarEnv_Day$TIMESTAMP >= data_record, ]
SolarEnv_Day$Date <- SolarEnv_Day$TIMESTAMP - ddays(1)
SolarEnv_Day[, 2:23] <- lapply(SolarEnv_Day[, 2:23], as.numeric)
Solar_climate <- read.csv(file = "CR1000_BSRN1000_Min201116.csv", skip = 1, stringsAsFactors = FALSE)
Solar_climate <- Solar_climate[c(-1, -2), -c(2, 14:19)] ##Delete two rows of unit, useless columns
Solar_climate$TIMESTAMP <- ymd_hms(Solar_climate$TIMESTAMP)
Solar_climate <- Solar_climate[date(Solar_climate$TIMESTAMP) >= data_record, ]
Solar_climate[, 2:length(Solar_climate)] <- lapply(Solar_climate[, 2:length(Solar_climate)], as.numeric)

cleandata <- function(SolarWater){
    hourlywater <- rbind(na.omit(SolarWater[2:length(SolarWater[, 1]), 1:2]), c("23:59", SolarWater[1, 2]))
    names(hourlywater) <- c("Time", "WaterProduction")
    hourlywater <- hourlywater[hourlywater$WaterProduction != 0, ]
    hourlywater <- rbind(c("0:00", 0), hourlywater)
    hourlywater$WaterProduction <- as.numeric(hourlywater$WaterProduction)
    hourlywater$Date <- ymd(SolarWater[1, 1])
    hourlywater$Percentage <- hourlywater$WaterProduction*100/hourlywater$WaterProduction[length(hourlywater$WaterProduction)]
    for(i in 2:(length(SolarWater)/2)){
    hourdata <- rbind(na.omit(SolarWater[2:length(SolarWater[, (2*i-1)]), (2*i-1):(2*i)]), c("23:59", SolarWater[1, (2*i)]))
    names(hourdata) <- c("Time", "WaterProduction")
    hourdata <- hourdata[hourdata$WaterProduction != 0, ]
    hourdata <- rbind(c("0:00", 0), hourdata)
    hourdata$WaterProduction <- as.numeric(hourdata$WaterProduction)
    hourdata$Date <- ymd(SolarWater[1, (2*i-1)])
    hourdata$Percentage <- hourdata$WaterProduction*100/hourdata$WaterProduction[length(hourdata$WaterProduction)]
    hourlywater <- rbind(hourlywater, hourdata)
  }
  hourlywater <- hourlywater[hourlywater$Date >= data_record, ]
  hourlywater$Time <- hm(hourlywater$Time)
  hourlywater$WaterProduction <- as.numeric(hourlywater$WaterProduction)*4/1000
  hourlywater$WaterEnergy <- hourlywater$WaterProduction/1.5
  hourlywater <- hourlywater[(hourlywater$Date + hourlywater$Time) %in% Solar_climate$TIMESTAMP, ]
  hourlywater$SolarEnergy <- as.numeric(Solar_climate$Global_Energy_Day[Solar_climate$TIMESTAMP %in% (hourlywater$Date + hourlywater$Time)])
  hourlywater$DeltaTime <- c(0, time_length(int_diff(hourlywater$Date + hourlywater$Time), "hours"))
  hourlywater$DeltaSolarEnergy <- c(0, diff(hourlywater$SolarEnergy))
  ##Set delta solar energy < 0 data = 0
  hourlywater$DeltaSolarEnergy[hourlywater$DeltaSolarEnergy < 0] <- 0
  hourlywater$DeltaWaterEnergy <- c(0, diff(hourlywater$WaterEnergy))
  ##Set delta water energy < 0 data = 0
  hourlywater$DeltaWaterEnergy[hourlywater$DeltaWaterEnergy < 0] <- 0
  #hourlywater$TransPercentage <- hourlywater$DeltaWaterEnergy/hourlywater$WaterEnergy
  hourlywater$AvgEfficiency <- hourlywater$WaterEnergy/hourlywater$SolarEnergy*100
  hourlywater$TransSolarPower <- hourlywater$DeltaSolarEnergy/hourlywater$DeltaTime
  hourlywater$DirectEnergy <- as.numeric(Solar_climate$Direct_Energy_Day[Solar_climate$TIMESTAMP %in% (hourlywater$Date + hourlywater$Time)])
  hourlywater$DiffuseEnergy <- as.numeric(Solar_climate$Diffuse_Energy_Day[Solar_climate$TIMESTAMP %in% (hourlywater$Date + hourlywater$Time)])
  hourlywater$DeltaDirectEnergy <- c(0, diff(hourlywater$DirectEnergy))
  hourlywater$DeltaDirectEnergy[hourlywater$DeltaDirectEnergy < 0] <- 0
  hourlywater$DeltaDiffuseEnergy <- c(0, diff(hourlywater$DiffuseEnergy))
  hourlywater$DeltaDiffuseEnergy[hourlywater$DeltaDiffuseEnergy < 0] <- 0
  hourlywater$TransDirectPower <- hourlywater$DeltaDirectEnergy/hourlywater$DeltaTime
  hourlywater$TransDiffusePower <- hourlywater$DeltaDiffuseEnergy/hourlywater$DeltaTime
  hourlywater$TransEfficiency <- hourlywater$DeltaWaterEnergy/hourlywater$DeltaSolarEnergy*100
  hourlywater$DirDiffRatio <- hourlywater$TransDirectPower/hourlywater$TransDiffusePower
  hourlywater$DirDiff <- cut(hourlywater$DirDiffRatio, breaks = c(min(na.omit(hourlywater$DirDiffRatio)), 0.1, 1, max(na.omit(hourlywater$DirDiffRatio))), labels = c("Cloudy", "Between", "Clear"))
  ##fit Efficiency from 10% to 75%
  fit <- lm(TransEfficiency ~ TransDirectPower + TransDiffusePower, data = na.omit(hourlywater[(hourlywater$TransEfficiency < 75)&(hourlywater$TransEfficiency > 10), ]))
  hourlywater$FitEfficiency <- coefficients(fit)[1] + coefficients(fit)[2]*hourlywater$TransDirectPower + coefficients(fit)[3]*hourlywater$TransDiffusePower
  ##hourlywater$TransEfficiency[(hourlywater$TransEfficiency > 100)|(hourlywater$TransEfficiency == 0)] <- 100 ##Set unusual transient efficiency as 100
  ##hourlywater <- hourlywater[-which((hourlywater$TransEfficiency > 100)|(hourlywater$TransEfficiency == 0)), ] ##rule out data with unusual transient efficiency
  ##print(summary(fit))
  return(hourlywater)
}

SolarWater_FoamSidewallInter <- read.csv(file = "Foam Sidewall Interfacial Solar Still Water Production.csv", header = FALSE, stringsAsFactors = FALSE)
hourlywater_FoamSidewallInter <- cleandata(SolarWater_FoamSidewallInter)

SolarWater_FoamBottomInter <- read.csv(file = "Foam Bottom Interfacial Solar Still Water Production.csv", header = FALSE, stringsAsFactors = FALSE)
hourlywater_FoamBottomInter <- cleandata(SolarWater_FoamBottomInter)

SolarWater_SidewallInter <- read.csv(file = "Sidewall Interfacial Solar Still Water Production.csv", header = FALSE, stringsAsFactors = FALSE)
hourlywater_SidewallInter <- cleandata(SolarWater_SidewallInter)

SolarWater_BottomInter <- read.csv(file = "Bottom Interfacial Solar Still Water Production.csv", header = FALSE, stringsAsFactors = FALSE)
hourlywater_BottomInter <- cleandata(SolarWater_BottomInter)

SolarWater_SidewallHeat <- read.csv(file = "Sidewall Bottom Heating Solar Still Water Production.csv", header = FALSE, stringsAsFactors = FALSE)
hourlywater_SidewallHeat <- cleandata(SolarWater_SidewallHeat)

SolarWater_BottomHeat <- read.csv(file = "Bottom Heating Solar Still Water Production.csv", header = FALSE, stringsAsFactors = FALSE)
hourlywater_BottomHeat <- cleandata(SolarWater_BottomHeat)

meltcol <- function(colnames){
  mergecolselect <- rbind(rbind(rbind(rbind(rbind(
    cbind(hourlywater_BottomHeat[, colnames], Still = "Bottom_Heating"),
    cbind(hourlywater_SidewallHeat[, colnames], Still = "Sidewall_Bottom_Heating")), 
    cbind(hourlywater_BottomInter[, colnames], Still = "Bottom_Interfacial")), 
    cbind(hourlywater_FoamBottomInter[, colnames], Still = "Foam_Bottom_Interfacial")), 
    cbind(hourlywater_SidewallInter[, colnames], Still = "Sidewall_Interfacial")), 
    cbind(hourlywater_FoamSidewallInter[, colnames], Still = "Foam_Sidewall_Interfacial")) 
  return(mergecolselect)
}

hourlywater_Percentage <- meltcol(c("Time", "Percentage"))
# hourlywater_TransPercentage <- meltcol(c("Time", "TransPercentage"))
hourlywater_Production <- meltcol(c("Time", "WaterProduction"))
hourlywater_AvgEfficiency <- meltcol(c("Time", "AvgEfficiency"))
hourlywater_TransEfficiency <- meltcol(c("Time", "TransEfficiency"))
hourlywater_PowerEfficiency <- meltcol(c("TransSolarPower", "TransEfficiency"))
hourlywater_DirectEfficiency <- meltcol(c("TransDirectPower", "TransEfficiency"))
hourlywater_DiffuseEfficiency <- meltcol(c("TransDiffusePower", "TransEfficiency"))
hourlywater_FitEfficiency <- meltcol(c("TransSolarPower", "FitEfficiency"))
 
hourlysolar <- Solar_climate[, c("TIMESTAMP", "Global_Energy_Day", "Direct_Energy_Day", "Diffuse_Energy_Day")]
hourlysolar$Date <- as.Date(date(hourlysolar$TIMESTAMP))
hourlysolar$Time <- as.numeric(hourlysolar$TIMESTAMP - as.POSIXct(hourlysolar$Date))/3600
hourlysolar <- hourlysolar[hourlysolar$Time %in% (hourlywater_Percentage$Time/hm("1:00")), ]
hourlysolar <- hourlysolar %>% left_join(SolarEnv_Day[, c("Date", "Global_Energy_Tot", "Direct_Energy_Tot", "Diffuse_Energy_Tot")], by = "Date")
hourlysolar <- hourlysolar[hourlysolar$Date != as.Date("2020-09-22"), ]
hourlysolar$GlobalPercentage <- hourlysolar$Global_Energy_Day/hourlysolar$Global_Energy_Tot
hourlysolar$DirectPercentage <- hourlysolar$Direct_Energy_Day/hourlysolar$Direct_Energy_Tot
hourlysolar$DiffusePercentage <- hourlysolar$Diffuse_Energy_Day/hourlysolar$Diffuse_Energy_Tot
hourlysolar$DeltaPercentage <- c(0, diff(hourlysolar$GlobalPercentage))
##Set delta solar energy < 0 data = 0
hourlysolar$DeltaPercentage[hourlysolar$DeltaPercentage < 0] <- 0
Percentage_solar <- na.omit(hourlysolar[, c("Time", "GlobalPercentage")])
Percentage_solar <- with(Percentage_solar, tapply(GlobalPercentage, as.factor(Time), mean))
Percentage_solar <- cbind(as.data.frame.table(Percentage_solar), Still = "Average_Solar_Radiation")
names(Percentage_solar) <- names(hourlywater_Percentage)
Percentage_solar$Percentage <- as.numeric(Percentage_solar$Percentage) * 100
Percentage_solar$Time <- as.numeric(as.character(Percentage_solar$Time))
hourlywater_Percentage$Time <- hourlywater_Percentage$Time/hm("1:00")
hourlywater_Percentage <- rbind(hourlywater_Percentage, Percentage_solar)
# DeltaPercentage_solar <- na.omit(hourlysolar[, c("Time", "GlobalPercentage")])
# DeltaPercentage_solar <- with(DeltaPercentage_solar, tapply(GlobalPercentage, as.factor(Time), mean))
# DeltaPercentage_solar <- cbind(as.data.frame.table(DeltaPercentage_solar), Still = "Average_Solar_Radiation")
# names(DeltaPercentage_solar) <- names(hourlywater_TransPercentage)
# DeltaPercentage_solar$TransPercentage <- as.numeric(DeltaPercentage_solar$TransPercentage)
# DeltaPercentage_solar$Time <- as.numeric(as.character(DeltaPercentage_solar$Time))
# hourlywater_TransPercentage$Time <- hourlywater_TransPercentage$Time/hm("1:00")
# hourlywater_TransPercentage <- rbind(hourlywater_TransPercentage, DeltaPercentage_solar)

library(ggplot2)
##g <- ggplot(hourlywater_BottomInter, aes(Time/hm("1:00"), Percentage, color = WaterProduction))
##g + geom_point() + labs(x = "DayTime", y = "Normalized Production Percentage") 
##p <- ggplot(hourlywater_SidewallInter, aes(Time/hm("1:00"), Percentage, color = WaterProduction))
##p + geom_point() + labs(x = "DayTime", y = "Normalized Production Percentage") 
##q <- ggplot(hourlywater_FoamBottomInter, aes(Time/hm("1:00"), Percentage, color = WaterProduction))
##q + geom_point() + labs(x = "DayTime", y = "Normalized Production Percentage") 
##GlobalPercentageBinomial <- ggplot(hourlysolar[(hourlysolar$GlobalPercentage <= 1) & (hourlysolar$GlobalPercentage >= 0), ], aes(Time, GlobalPercentage, color = as.factor(month(Date))))
##GlobalPercentageBinomial + geom_point(alpha = 0.01) + stat_smooth(method = "glm", method.args = list(family = binomial), se=FALSE) + labs(x = "DayTime", y = "Normalized Solar Radiation Percentage")## + scale_x_continuous(limits = c(6,24))
##DirectPercentageBinomial <- ggplot(hourlysolar[(hourlysolar$DirectPercentage <= 1) & (hourlysolar$DirectPercentage >= 0), ], aes(Time, DirectPercentage))
##DirectPercentageBinomial + geom_point(alpha = 0.2) + stat_smooth(method = "glm", method.args = list(family = binomial), se=FALSE) + labs(x = "DayTime", y = "Normalized Solar Radiation Percentage")## + scale_x_continuous(limits = c(6,24))
##DiffusePercentageBinomial <- ggplot(hourlysolar[(hourlysolar$DiffusePercentage <= 1) & (hourlysolar$DiffusePercentage >= 0), ], aes(Time, DiffusePercentage))
##DiffusePercentageBinomial + geom_point(alpha = 0.2) + stat_smooth(method = "glm", method.args = list(family = binomial), se=FALSE) + labs(x = "DayTime", y = "Normalized Solar Radiation Percentage")## + scale_x_continuous(limits = c(6,24))

StillName_Interfacial <- c("Bottom_Interfacial", "Foam_Bottom_Interfacial", "Sidewall_Interfacial", "Foam_Sidewall_Interfacial")

PercentageBinomial <- ggplot(hourlywater_Percentage, aes(Time, Percentage/100, color = Still))
PercentageBinomial + geom_point(alpha = 0.1) + 
  stat_smooth(method = "glm", method.args = list(family = binomial), se=FALSE) + 
  labs(x = "DayTime", y = "Normalized Production Percentage")## + scale_x_continuous(limits = c(6,24))
# TransPercentage <- ggplot(hourlywater_TransPercentage, aes(Time, TransPercentage, color = Still))
# TransPercentage + geom_point(alpha = 0.1) + geom_smooth(se = FALSE)
##Percentageloess <- ggplot(hourlywater_Percentage, aes(Time/hm("1:00"), Percentage, color = Still))
##Percentageloess + geom_point(alpha = 0.2) + stat_smooth(se=FALSE) + labs(x = "DayTime", y = "Normalized Production Percentage") + scale_x_continuous(limits = c(6,20))
##h <- ggplot(hourlywater_Production, aes(Time/hm("1:00"), WaterProduction, color = Still))
##h + geom_point(alpha = 0.5) + geom_smooth(se = FALSE) + labs(x = "Time", y = "Production") ##+ scale_x_continuous(limits = c(6,18))
##AvgEff <- ggplot(hourlywater_AvgEfficiency, aes(Time/hm("1:00"), AvgEfficiency, color = Still))
##AvgEff + geom_point(alpha = 0.5) + geom_smooth(se = FALSE) + labs(x = "Time", y = "Daily Average Efficiency/%") + scale_y_continuous(limits = c(0,70)) + scale_x_continuous(limits = c(7,24))
##TransEff <- ggplot(hourlywater_TransEfficiency[(hourlywater_TransEfficiency$Time/hm("1:00")<18)&(hourlywater_TransEfficiency$Time/hm("1:00")>6), ], aes(Time/hm("1:00"), TransEfficiency, color = Still))
##TransEff + geom_point(alpha = 0.5) + geom_smooth(se = FALSE) + labs(x = "Time", y = "Hourly Transient Efficiency/%") + scale_y_continuous(limits = c(0,100))## + scale_x_continuous(limits = c(7,20))
TransEff <- ggplot(hourlywater_TransEfficiency, aes(Time/hm("1:00"), TransEfficiency, color = Still))
TransEff + geom_point(alpha = 0.2) + geom_smooth(se = FALSE) + labs(x = "Time", y = "Hourly Transient Efficiency/%") + scale_y_continuous(limits = c(10,70), breaks=seq(0,100,10)) + scale_x_continuous(limits = c(8,20), breaks=seq(0,24,2))

StillName_Interfacial <- c("Bottom_Interfacial", "Foam_Bottom_Interfacial", "Sidewall_Interfacial", "Foam_Sidewall_Interfacial")
PowerEff <- ggplot(hourlywater_PowerEfficiency[hourlywater_PowerEfficiency$Still %in% StillName_Interfacial, ], aes(TransSolarPower*1000, TransEfficiency, color = Still))
PowerEff + geom_point(alpha = 0.2) + geom_smooth(se = FALSE) + labs(x = "Solar Power/W", y = "Hourly Transient Efficiency/%") + scale_y_continuous(limits = c(10,80), breaks=seq(0,100,10)) + scale_x_continuous(limits = c(100,1000), breaks=seq(0,1000,200))

DirectEff <- ggplot(hourlywater_DirectEfficiency[hourlywater_DirectEfficiency$Still %in% StillName_Interfacial, ], aes(TransDirectPower*1000, TransEfficiency, color = Still))
DirectEff + geom_point(alpha = 0.2) + geom_smooth(se = FALSE) + labs(x = "Solar Direct Power/W", y = "Hourly Transient Efficiency/%") + scale_y_continuous(limits = c(10,75)) + scale_x_continuous(limits = c(50,900))
DiffuseEff <- ggplot(hourlywater_DiffuseEfficiency[hourlywater_DiffuseEfficiency$Still %in% StillName_Interfacial, ], aes(TransDiffusePower*1000, TransEfficiency, color = Still))
DiffuseEff + geom_point(alpha = 0.2) + geom_smooth(se = FALSE) + labs(x = "Solar Diffuse Power/W", y = "Hourly Transient Efficiency/%") + scale_y_continuous(limits = c(10,75)) + scale_x_continuous(limits = c(50,600))
FitEff <- ggplot(hourlywater_FitEfficiency[hourlywater_FitEfficiency$Still %in% StillName_Interfacial, ], aes(TransSolarPower*1000, FitEfficiency, color = Still))
FitEff + geom_point(alpha = 0.2) + geom_smooth(se = FALSE) + labs(x = "Solar Power/W", y = "Hourly Fit Transient Efficiency/%") + scale_y_continuous(limits = c(10,75)) + scale_x_continuous(limits = c(100,1000))


library(plotly)
PercentageBinomial <- ggplot(hourlywater_Percentage, aes(Time, Percentage/100, color = Still)) +
  geom_point(alpha = 0.1) + stat_smooth(method = "glm", method.args = list(family = binomial), se=FALSE) + 
  labs(x = "DayTime/0:00", y = "Normalized Percentage/100%") + scale_x_continuous(breaks=seq(0,24,2))
ggplotly(PercentageBinomial)



TransEff <- ggplot(hourlywater_TransEfficiency, aes(Time/hm("1:00"), TransEfficiency, color = Still)) + 
  geom_point(alpha = 0.2) + geom_smooth(se = FALSE) + labs(x = "Time", y = "Hourly Transient Efficiency/%") + 
  scale_y_continuous(limits = c(10,70), breaks=seq(0,100,10)) + scale_x_continuous(limits = c(8,20), breaks=seq(0,24,2))
ggplotly(TransEff)
