##Get solar still water hourly production data
setwd("D:/R/Solar Still")
library(lubridate)
data_record <- as.Date("2020-07-03")
energy_record <- 5
power_record <- 0

Solar_climate <- read.csv(file = "CR1000_BSRN1000_Min200921.csv", skip = 1, stringsAsFactors = FALSE)
Solar_climateUnit <- Solar_climate[1, -c(2, 14:19)]
Solar_climate <- Solar_climate[c(-1, -2), -c(2, 14:19)] ##Delete two rows of unit, useless columns
Solar_climate$TIMESTAMP <- ymd_hms(Solar_climate$TIMESTAMP)
Solar_climate <- Solar_climate[date(Solar_climate$TIMESTAMP) >= data_record, ]

SolarWater_FoamBottomInter <- read.csv(file = "Foam Bottom Interfacial Solar Still Water Production.csv", header = FALSE, stringsAsFactors = FALSE)
hourlywater_FoamBottomInter <- rbind(rbind(c("0:00", 0), na.omit(SolarWater_FoamBottomInter[2:length(SolarWater_FoamBottomInter[, 1]), 1:2]), c("23:59", SolarWater_FoamBottomInter[1, 2])))
names(hourlywater_FoamBottomInter) <- c("Time", "WaterProduction")
hourlywater_FoamBottomInter <- cbind(Date = SolarWater_FoamBottomInter[1, 1], hourlywater_FoamBottomInter)
hourlywater_FoamBottomInter$Percentage <- as.numeric(hourlywater_FoamBottomInter$WaterProduction)*100/as.numeric(SolarWater_FoamBottomInter[1, 2])
for(i in 2:(length(SolarWater_FoamBottomInter)/2)){
  hourdata_FoamBottomInter <- rbind(rbind(c("0:00", 0), na.omit(SolarWater_FoamBottomInter[2:length(SolarWater_FoamBottomInter[, (2*i-1)]), (2*i-1):(2*i)]), c("23:59", SolarWater_FoamBottomInter[1, (2*i)])))
  names(hourdata_FoamBottomInter) <- c("Time", "WaterProduction")
  hourdata_FoamBottomInter <- cbind(Date = SolarWater_FoamBottomInter[1, (2*i-1)], hourdata_FoamBottomInter)
  hourdata_FoamBottomInter$Percentage <- as.numeric(hourdata_FoamBottomInter$WaterProduction)*100/as.numeric(SolarWater_FoamBottomInter[1, (2*i)])
  hourlywater_FoamBottomInter <- rbind(hourlywater_FoamBottomInter, hourdata_FoamBottomInter)
}
hourlywater_FoamBottomInter <- hourlywater_FoamBottomInter[hourlywater_FoamBottomInter$Date >= data_record, ]
hourlywater_FoamBottomInter$Date <- ymd(hourlywater_FoamBottomInter$Date)
hourlywater_FoamBottomInter$Time <- hm(hourlywater_FoamBottomInter$Time)
hourlywater_FoamBottomInter$WaterProduction <- as.numeric(hourlywater_FoamBottomInter$WaterProduction)*4/1000
hourlywater_FoamBottomInter$WaterEnergy <- hourlywater_FoamBottomInter$WaterProduction/1.5
hourlywater_FoamBottomInter <- hourlywater_FoamBottomInter[(hourlywater_FoamBottomInter$Date + hourlywater_FoamBottomInter$Time) %in% Solar_climate$TIMESTAMP, ]
hourlywater_FoamBottomInter$SolarEnergy <- as.numeric(Solar_climate$Global_Energy_Day[Solar_climate$TIMESTAMP %in% (hourlywater_FoamBottomInter$Date + hourlywater_FoamBottomInter$Time)])
hourlywater_FoamBottomInter$DeltaTime <- c(0, time_length(int_diff(hourlywater_FoamBottomInter$Date + hourlywater_FoamBottomInter$Time), "hours"))
hourlywater_FoamBottomInter$DeltaSolarEnergy <- c(0, diff(hourlywater_FoamBottomInter$SolarEnergy))
hourlywater_FoamBottomInter$DeltaSolarEnergy[hourlywater_FoamBottomInter$DeltaSolarEnergy < 0] <- 0
hourlywater_FoamBottomInter$DeltaWaterEnergy <- c(0, diff(hourlywater_FoamBottomInter$WaterEnergy))
hourlywater_FoamBottomInter$DeltaWaterEnergy[hourlywater_FoamBottomInter$DeltaWaterEnergy < 0] <- 0
hourlywater_FoamBottomInter$AvgEfficiency <- hourlywater_FoamBottomInter$WaterEnergy/hourlywater_FoamBottomInter$SolarEnergy*100
hourlywater_FoamBottomInter$TransSolarPower <- hourlywater_FoamBottomInter$DeltaSolarEnergy/hourlywater_FoamBottomInter$DeltaTime
hourlywater_FoamBottomInter$TransEfficiency <- hourlywater_FoamBottomInter$DeltaWaterEnergy/hourlywater_FoamBottomInter$DeltaSolarEnergy*100
##hourlywater_FoamBottomInter$TransEfficiency[(hourlywater_FoamBottomInter$TransEfficiency > 100)|(hourlywater_FoamBottomInter$TransEfficiency == 0)] <- 100 ##Set unusual transient efficiency as 100
##hourlywater_FoamBottomInter <- hourlywater_FoamBottomInter[-which((hourlywater_FoamBottomInter$TransEfficiency > 100)|(hourlywater_FoamBottomInter$TransEfficiency == 0)), ] ##rule out data with unusual transient efficiency

SolarWater_SidewallInter <- read.csv(file = "Sidewall Interfacial Solar Still Water Production.csv", header = FALSE, stringsAsFactors = FALSE)
hourlywater_SidewallInter <- rbind(rbind(c("0:00", 0), na.omit(SolarWater_SidewallInter[2:length(SolarWater_SidewallInter[, 1]), 1:2]), c("23:59", SolarWater_SidewallInter[1, 2])))
names(hourlywater_SidewallInter) <- c("Time", "WaterProduction")
hourlywater_SidewallInter <- cbind(Date = SolarWater_SidewallInter[1, 1], hourlywater_SidewallInter)
hourlywater_SidewallInter$Percentage <- as.numeric(hourlywater_SidewallInter$WaterProduction)*100/as.numeric(SolarWater_SidewallInter[1, 2])
for(i in 2:(length(SolarWater_SidewallInter)/2)){
  hourdata_SidewallInter <- rbind(rbind(c("0:00", 0), na.omit(SolarWater_SidewallInter[2:length(SolarWater_SidewallInter[, (2*i-1)]), (2*i-1):(2*i)]), c("23:59", SolarWater_SidewallInter[1, (2*i)])))
  names(hourdata_SidewallInter) <- c("Time", "WaterProduction")
  hourdata_SidewallInter <- cbind(Date = SolarWater_SidewallInter[1, (2*i-1)], hourdata_SidewallInter)
  hourdata_SidewallInter$Percentage <- as.numeric(hourdata_SidewallInter$WaterProduction)*100/as.numeric(SolarWater_SidewallInter[1, (2*i)])
  hourlywater_SidewallInter <- rbind(hourlywater_SidewallInter, hourdata_SidewallInter)
}
hourlywater_SidewallInter <- hourlywater_SidewallInter[hourlywater_SidewallInter$Date >= data_record, ]
hourlywater_SidewallInter$Date <- ymd(hourlywater_SidewallInter$Date)
hourlywater_SidewallInter$Time <- hm(hourlywater_SidewallInter$Time)
hourlywater_SidewallInter$WaterProduction <- as.numeric(hourlywater_SidewallInter$WaterProduction)*4/1000
hourlywater_SidewallInter$WaterEnergy <- hourlywater_SidewallInter$WaterProduction/1.5
hourlywater_SidewallInter <- hourlywater_SidewallInter[(hourlywater_SidewallInter$Date + hourlywater_SidewallInter$Time) %in% Solar_climate$TIMESTAMP, ]
hourlywater_SidewallInter$SolarEnergy <- as.numeric(Solar_climate$Global_Energy_Day[Solar_climate$TIMESTAMP %in% (hourlywater_SidewallInter$Date + hourlywater_SidewallInter$Time)])
hourlywater_SidewallInter$DeltaTime <- c(0, time_length(int_diff(hourlywater_SidewallInter$Date + hourlywater_SidewallInter$Time), "hours"))
hourlywater_SidewallInter$DeltaSolarEnergy <- c(0, diff(hourlywater_SidewallInter$SolarEnergy))
hourlywater_SidewallInter$DeltaSolarEnergy[hourlywater_SidewallInter$DeltaSolarEnergy < 0] <- 0
hourlywater_SidewallInter$DeltaWaterEnergy <- c(0, diff(hourlywater_SidewallInter$WaterEnergy))
hourlywater_SidewallInter$DeltaWaterEnergy[hourlywater_SidewallInter$DeltaWaterEnergy < 0] <- 0
hourlywater_SidewallInter$AvgEfficiency <- hourlywater_SidewallInter$WaterEnergy/hourlywater_SidewallInter$SolarEnergy*100
hourlywater_SidewallInter$TransSolarPower <- hourlywater_SidewallInter$DeltaSolarEnergy/hourlywater_SidewallInter$DeltaTime
hourlywater_SidewallInter$TransEfficiency <- hourlywater_SidewallInter$DeltaWaterEnergy/hourlywater_SidewallInter$DeltaSolarEnergy*100
##hourlywater_SidewallInter$TransEfficiency[(hourlywater_SidewallInter$TransEfficiency > 100)|(hourlywater_SidewallInter$TransEfficiency == 0)] <- 100 ##Set unusual transient efficiency as 100
##hourlywater_SidewallInter <- hourlywater_SidewallInter[-which((hourlywater_SidewallInter$TransEfficiency > 100)|(hourlywater_SidewallInter$TransEfficiency == 0)), ] ##rule out data with unusual transient efficiency


SolarWater_BottomInter <- read.csv(file = "Bottom Interfacial Solar Still Water Production.csv", header = FALSE, stringsAsFactors = FALSE)
hourlywater_BottomInter <- rbind(rbind(c("0:00", 0), na.omit(SolarWater_BottomInter[2:length(SolarWater_BottomInter[, 1]), 1:2]), c("23:59", SolarWater_BottomInter[1, 2])))
names(hourlywater_BottomInter) <- c("Time", "WaterProduction")
hourlywater_BottomInter <- cbind(Date = SolarWater_BottomInter[1, 1], hourlywater_BottomInter)
hourlywater_BottomInter$Percentage <- as.numeric(hourlywater_BottomInter$WaterProduction)*100/as.numeric(SolarWater_BottomInter[1, 2])
for(i in 2:(length(SolarWater_BottomInter)/2)){
  hourdata_BottomInter <- rbind(rbind(c("0:00", 0), na.omit(SolarWater_BottomInter[2:length(SolarWater_BottomInter[, (2*i-1)]), (2*i-1):(2*i)]), c("23:59", SolarWater_BottomInter[1, (2*i)])))
  names(hourdata_BottomInter) <- c("Time", "WaterProduction")
  hourdata_BottomInter <- cbind(Date = SolarWater_BottomInter[1, (2*i-1)], hourdata_BottomInter)
  hourdata_BottomInter$Percentage <- as.numeric(hourdata_BottomInter$WaterProduction)*100/as.numeric(SolarWater_BottomInter[1, (2*i)])
  hourlywater_BottomInter <- rbind(hourlywater_BottomInter, hourdata_BottomInter)
}
hourlywater_BottomInter <- hourlywater_BottomInter[hourlywater_BottomInter$Date >= data_record, ]
hourlywater_BottomInter$Date <- ymd(hourlywater_BottomInter$Date)
hourlywater_BottomInter$Time <- hm(hourlywater_BottomInter$Time)
hourlywater_BottomInter$WaterProduction <- as.numeric(hourlywater_BottomInter$WaterProduction)*4/1000
hourlywater_BottomInter$WaterEnergy <- hourlywater_BottomInter$WaterProduction/1.5
hourlywater_BottomInter <- hourlywater_BottomInter[(hourlywater_BottomInter$Date + hourlywater_BottomInter$Time) %in% Solar_climate$TIMESTAMP, ]
hourlywater_BottomInter$SolarEnergy <- as.numeric(Solar_climate$Global_Energy_Day[Solar_climate$TIMESTAMP %in% (hourlywater_BottomInter$Date + hourlywater_BottomInter$Time)])
hourlywater_BottomInter$DeltaTime <- c(0, time_length(int_diff(hourlywater_BottomInter$Date + hourlywater_BottomInter$Time), "hours"))
hourlywater_BottomInter$DeltaSolarEnergy <- c(0, diff(hourlywater_BottomInter$SolarEnergy))
hourlywater_BottomInter$DeltaSolarEnergy[hourlywater_BottomInter$DeltaSolarEnergy < 0] <- 0
hourlywater_BottomInter$DeltaWaterEnergy <- c(0, diff(hourlywater_BottomInter$WaterEnergy))
hourlywater_BottomInter$DeltaWaterEnergy[hourlywater_BottomInter$DeltaWaterEnergy < 0] <- 0
hourlywater_BottomInter$AvgEfficiency <- hourlywater_BottomInter$WaterEnergy/hourlywater_BottomInter$SolarEnergy*100
hourlywater_BottomInter$TransSolarPower <- hourlywater_BottomInter$DeltaSolarEnergy/hourlywater_BottomInter$DeltaTime
hourlywater_BottomInter$TransEfficiency <- hourlywater_BottomInter$DeltaWaterEnergy/hourlywater_BottomInter$DeltaSolarEnergy*100
##hourlywater_BottomInter$TransEfficiency[(hourlywater_BottomInter$TransEfficiency > 100)|(hourlywater_BottomInter$TransEfficiency == 0)] <- 100 ##Set unusual transient efficiency as 100
##hourlywater_BottomInter <- hourlywater_BottomInter[-which((hourlywater_BottomInter$TransEfficiency > 80)|(hourlywater_BottomInter$TransEfficiency == 0)), ] ##rule out data with unusual transient efficiency


SolarWater_SidewallHeat <- read.csv(file = "Sidewall Bottom Heating Solar Still Water Production.csv", header = FALSE, stringsAsFactors = FALSE)
hourlywater_SidewallHeat <- rbind(rbind(c("0:00", 0), na.omit(SolarWater_SidewallHeat[2:length(SolarWater_SidewallHeat[, 1]), 1:2]), c("23:59", SolarWater_SidewallHeat[1, 2])))
names(hourlywater_SidewallHeat) <- c("Time", "WaterProduction")
hourlywater_SidewallHeat <- cbind(Date = SolarWater_SidewallHeat[1, 1], hourlywater_SidewallHeat)
hourlywater_SidewallHeat$Percentage <- as.numeric(hourlywater_SidewallHeat$WaterProduction)*100/as.numeric(SolarWater_SidewallHeat[1, 2])
for(i in 2:(length(SolarWater_SidewallHeat)/2)){
  hourdata_SidewallHeat <- rbind(rbind(c("0:00", 0), na.omit(SolarWater_SidewallHeat[2:length(SolarWater_SidewallHeat[, (2*i-1)]), (2*i-1):(2*i)]), c("23:59", SolarWater_SidewallHeat[1, (2*i)])))
  names(hourdata_SidewallHeat) <- c("Time", "WaterProduction")
  hourdata_SidewallHeat <- cbind(Date = SolarWater_SidewallHeat[1, (2*i-1)], hourdata_SidewallHeat)
  hourdata_SidewallHeat$Percentage <- as.numeric(hourdata_SidewallHeat$WaterProduction)*100/as.numeric(SolarWater_SidewallHeat[1, (2*i)])
  hourlywater_SidewallHeat <- rbind(hourlywater_SidewallHeat, hourdata_SidewallHeat)
}
hourlywater_SidewallHeat <- hourlywater_SidewallHeat[hourlywater_SidewallHeat$Date >= data_record, ]
hourlywater_SidewallHeat$Date <- ymd(hourlywater_SidewallHeat$Date)
hourlywater_SidewallHeat$Time <- hm(hourlywater_SidewallHeat$Time)
hourlywater_SidewallHeat$WaterProduction <- as.numeric(hourlywater_SidewallHeat$WaterProduction)*4/1000
hourlywater_SidewallHeat$WaterEnergy <- hourlywater_SidewallHeat$WaterProduction/1.5
hourlywater_SidewallHeat <- hourlywater_SidewallHeat[(hourlywater_SidewallHeat$Date + hourlywater_SidewallHeat$Time) %in% Solar_climate$TIMESTAMP, ]
hourlywater_SidewallHeat$SolarEnergy <- as.numeric(Solar_climate$Global_Energy_Day[Solar_climate$TIMESTAMP %in% (hourlywater_SidewallHeat$Date + hourlywater_SidewallHeat$Time)])
hourlywater_SidewallHeat$DeltaTime <- c(0, time_length(int_diff(hourlywater_SidewallHeat$Date + hourlywater_SidewallHeat$Time), "hours"))
hourlywater_SidewallHeat$DeltaSolarEnergy <- c(0, diff(hourlywater_SidewallHeat$SolarEnergy))
hourlywater_SidewallHeat$DeltaSolarEnergy[hourlywater_SidewallHeat$DeltaSolarEnergy < 0] <- 0
hourlywater_SidewallHeat$DeltaWaterEnergy <- c(0, diff(hourlywater_SidewallHeat$WaterEnergy))
hourlywater_SidewallHeat$DeltaWaterEnergy[hourlywater_SidewallHeat$DeltaWaterEnergy < 0] <- 0
hourlywater_SidewallHeat$AvgEfficiency <- hourlywater_SidewallHeat$WaterEnergy/hourlywater_SidewallHeat$SolarEnergy*100
hourlywater_SidewallHeat$TransSolarPower <- hourlywater_SidewallHeat$DeltaSolarEnergy/hourlywater_SidewallHeat$DeltaTime
hourlywater_SidewallHeat$TransEfficiency <- hourlywater_SidewallHeat$DeltaWaterEnergy/hourlywater_SidewallHeat$DeltaSolarEnergy*100
##hourlywater_SidewallHeat$TransEfficiency[(hourlywater_SidewallHeat$TransEfficiency > 100)|(hourlywater_SidewallHeat$TransEfficiency == 0)] <- 100 ##Set unusual transient efficiency as 100
##hourlywater_SidewallHeat <- hourlywater_SidewallHeat[-which((hourlywater_SidewallHeat$TransEfficiency > 80)|(hourlywater_SidewallHeat$TransEfficiency == 0)), ] ##rule out data with unusual transient efficiency


SolarWater_BottomHeat <- read.csv(file = "Bottom Heating Solar Still Water Production.csv", header = FALSE, stringsAsFactors = FALSE)
hourlywater_BottomHeat <- rbind(rbind(c("0:00", 0), na.omit(SolarWater_BottomHeat[2:length(SolarWater_BottomHeat[, 1]), 1:2]), c("23:59", SolarWater_BottomHeat[1, 2])))
names(hourlywater_BottomHeat) <- c("Time", "WaterProduction")
hourlywater_BottomHeat <- cbind(Date = SolarWater_BottomHeat[1, 1], hourlywater_BottomHeat)
hourlywater_BottomHeat$Percentage <- as.numeric(hourlywater_BottomHeat$WaterProduction)*100/as.numeric(SolarWater_BottomHeat[1, 2])
for(i in 2:(length(SolarWater_BottomHeat)/2)){
  hourdata_BottomHeat <- rbind(rbind(c("0:00", 0), na.omit(SolarWater_BottomHeat[2:length(SolarWater_BottomHeat[, (2*i-1)]), (2*i-1):(2*i)]), c("23:59", SolarWater_BottomHeat[1, (2*i)])))
  names(hourdata_BottomHeat) <- c("Time", "WaterProduction")
  hourdata_BottomHeat <- cbind(Date = SolarWater_BottomHeat[1, (2*i-1)], hourdata_BottomHeat)
  hourdata_BottomHeat$Percentage <- as.numeric(hourdata_BottomHeat$WaterProduction)*100/as.numeric(SolarWater_BottomHeat[1, (2*i)])
  hourlywater_BottomHeat <- rbind(hourlywater_BottomHeat, hourdata_BottomHeat)
}
hourlywater_BottomHeat <- hourlywater_BottomHeat[hourlywater_BottomHeat$Date >= data_record, ]
hourlywater_BottomHeat$Date <- ymd(hourlywater_BottomHeat$Date)
hourlywater_BottomHeat$Time <- hm(hourlywater_BottomHeat$Time)
hourlywater_BottomHeat$WaterProduction <- as.numeric(hourlywater_BottomHeat$WaterProduction)*4/1000
hourlywater_BottomHeat$WaterEnergy <- hourlywater_BottomHeat$WaterProduction/1.5
hourlywater_BottomHeat <- hourlywater_BottomHeat[(hourlywater_BottomHeat$Date + hourlywater_BottomHeat$Time) %in% Solar_climate$TIMESTAMP, ]
hourlywater_BottomHeat$SolarEnergy <- as.numeric(Solar_climate$Global_Energy_Day[Solar_climate$TIMESTAMP %in% (hourlywater_BottomHeat$Date + hourlywater_BottomHeat$Time)])
hourlywater_BottomHeat$DeltaTime <- c(0, time_length(int_diff(hourlywater_BottomHeat$Date + hourlywater_BottomHeat$Time), "hours"))
hourlywater_BottomHeat$DeltaSolarEnergy <- c(0, diff(hourlywater_BottomHeat$SolarEnergy))
hourlywater_BottomHeat$DeltaSolarEnergy[hourlywater_BottomHeat$DeltaSolarEnergy < 0] <- 0
hourlywater_BottomHeat$DeltaWaterEnergy <- c(0, diff(hourlywater_BottomHeat$WaterEnergy))
hourlywater_BottomHeat$DeltaWaterEnergy[hourlywater_BottomHeat$DeltaWaterEnergy < 0] <- 0
hourlywater_BottomHeat$AvgEfficiency <- hourlywater_BottomHeat$WaterEnergy/hourlywater_BottomHeat$SolarEnergy*100
hourlywater_BottomHeat$TransSolarPower <- hourlywater_BottomHeat$DeltaSolarEnergy/hourlywater_BottomHeat$DeltaTime
hourlywater_BottomHeat$TransEfficiency <- hourlywater_BottomHeat$DeltaWaterEnergy/hourlywater_BottomHeat$DeltaSolarEnergy*100
##hourlywater_BottomHeat$TransEfficiency[(hourlywater_BottomHeat$TransEfficiency > 100)|(hourlywater_BottomHeat$TransEfficiency == 0)] <- 100 ##Set unusual transient efficiency as 100
##hourlywater_BottomHeat <- hourlywater_BottomHeat[-which((hourlywater_BottomHeat$TransEfficiency > 80)|(hourlywater_BottomHeat$TransEfficiency == 0)), ] ##rule out data with unusual transient efficiency

hourlywater_Percentage <- rbind(rbind(rbind(rbind(cbind(hourlywater_BottomInter[, c("Time", "Percentage")], Still = "Bottom_Interfacial"), cbind(hourlywater_SidewallInter[, c("Time", "Percentage")], Still = "Sidewall_Interfacial")), cbind(hourlywater_FoamBottomInter[, c("Time", "Percentage")], Still = "Foam_Bottom_Interfacial")), cbind(hourlywater_SidewallHeat[, c("Time", "Percentage")], Still = "Sidewall_Bottom_Heating")), cbind(hourlywater_BottomHeat[, c("Time", "Percentage")], Still = "Bottom_Heating"))
hourlywater_Production <- rbind(rbind(rbind(rbind(cbind(hourlywater_BottomInter[, c("Time", "WaterProduction")], Still = "Bottom_Interfacial"), cbind(hourlywater_SidewallInter[, c("Time", "WaterProduction")], Still = "Sidewall_Interfacial")), cbind(hourlywater_FoamBottomInter[, c("Time", "WaterProduction")], Still = "Foam_Bottom_Interfacial")), cbind(hourlywater_SidewallHeat[, c("Time", "WaterProduction")], Still = "Sidewall_Bottom_Heating")), cbind(hourlywater_BottomHeat[, c("Time", "WaterProduction")], Still = "Bottom_Heating"))
hourlywater_AvgEfficiency <- rbind(rbind(rbind(rbind(cbind(hourlywater_BottomInter[, c("Time", "AvgEfficiency")], Still = "Bottom_Interfacial"), cbind(hourlywater_SidewallInter[, c("Time", "AvgEfficiency")], Still = "Sidewall_Interfacial")), cbind(hourlywater_FoamBottomInter[, c("Time", "AvgEfficiency")], Still = "Foam_Bottom_Interfacial")), cbind(hourlywater_SidewallHeat[, c("Time", "AvgEfficiency")], Still = "Sidewall_Bottom_Heating")), cbind(hourlywater_BottomHeat[, c("Time", "AvgEfficiency")], Still = "Bottom_Heating"))
hourlywater_TransEfficiency <- rbind(rbind(rbind(rbind(cbind(hourlywater_BottomInter[, c("Time", "TransEfficiency")], Still = "Bottom_Interfacial"), cbind(hourlywater_SidewallInter[, c("Time", "TransEfficiency")], Still = "Sidewall_Interfacial")), cbind(hourlywater_FoamBottomInter[, c("Time", "TransEfficiency")], Still = "Foam_Bottom_Interfacial")), cbind(hourlywater_SidewallHeat[, c("Time", "TransEfficiency")], Still = "Sidewall_Bottom_Heating")), cbind(hourlywater_BottomHeat[, c("Time", "TransEfficiency")], Still = "Bottom_Heating"))
hourlywater_PowerEfficiency <- rbind(rbind(rbind(rbind(cbind(hourlywater_BottomInter[, c("TransSolarPower", "TransEfficiency")], Still = "Bottom_Interfacial"), cbind(hourlywater_SidewallInter[, c("TransSolarPower", "TransEfficiency")], Still = "Sidewall_Interfacial")), cbind(hourlywater_FoamBottomInter[, c("TransSolarPower", "TransEfficiency")], Still = "Foam_Bottom_Interfacial")), cbind(hourlywater_SidewallHeat[, c("TransSolarPower", "TransEfficiency")], Still = "Sidewall_Bottom_Heating")), cbind(hourlywater_BottomHeat[, c("TransSolarPower", "TransEfficiency")], Still = "Bottom_Heating"))


library(ggplot2)
##g <- ggplot(hourlywater_BottomInter, aes(Time/hm("1:00"), Percentage, color = WaterProduction))
##g + geom_point() + labs(x = "DayTime", y = "Normalized Production Percentage") 
##p <- ggplot(hourlywater_SidewallInter, aes(Time/hm("1:00"), Percentage, color = WaterProduction))
##p + geom_point() + labs(x = "DayTime", y = "Normalized Production Percentage") 
##q <- ggplot(hourlywater_FoamBottomInter, aes(Time/hm("1:00"), Percentage, color = WaterProduction))
##q + geom_point() + labs(x = "DayTime", y = "Normalized Production Percentage") 

PercentageBinomial <- ggplot(hourlywater_Percentage, aes(Time/hm("1:00"), Percentage/100, color = Still))
PercentageBinomial + geom_point(alpha = 0.2) + stat_smooth(method = "glm", method.args = list(family = binomial), se=FALSE) + labs(x = "DayTime", y = "Normalized Production Percentage")## + scale_x_continuous(limits = c(6,24))
##Percentageloess <- ggplot(hourlywater_Percentage, aes(Time/hm("1:00"), Percentage, color = Still))
##Percentageloess + geom_point(alpha = 0.2) + stat_smooth(se=FALSE) + labs(x = "DayTime", y = "Normalized Production Percentage") + scale_x_continuous(limits = c(6,20))
##h <- ggplot(hourlywater_Production, aes(Time/hm("1:00"), WaterProduction, color = Still))
##h + geom_point(alpha = 0.5) + geom_smooth(se = FALSE) + labs(x = "Time", y = "Production") ##+ scale_x_continuous(limits = c(6,18))
AvgEff <- ggplot(hourlywater_AvgEfficiency, aes(Time/hm("1:00"), AvgEfficiency, color = Still))
AvgEff + geom_point(alpha = 0.5) + geom_smooth(se = FALSE) + labs(x = "Time", y = "Daily Average Efficiency/%") + scale_y_continuous(limits = c(0,70)) + scale_x_continuous(limits = c(7,24))
##TransEff <- ggplot(hourlywater_TransEfficiency[(hourlywater_TransEfficiency$Time/hm("1:00")<18)&(hourlywater_TransEfficiency$Time/hm("1:00")>6), ], aes(Time/hm("1:00"), TransEfficiency, color = Still))
##TransEff + geom_point(alpha = 0.5) + geom_smooth(se = FALSE) + labs(x = "Time", y = "Hourly Transient Efficiency/%") + scale_y_continuous(limits = c(0,100))## + scale_x_continuous(limits = c(7,20))
TransEff <- ggplot(hourlywater_TransEfficiency, aes(Time/hm("1:00"), TransEfficiency, color = Still))
TransEff + geom_point(alpha = 0.2) + geom_smooth(se = FALSE) + labs(x = "Time", y = "Hourly Transient Efficiency/%") + scale_y_continuous(limits = c(10,90)) + scale_x_continuous(limits = c(7,20))
PowerEff <- ggplot(hourlywater_PowerEfficiency, aes(TransSolarPower*1000, TransEfficiency, color = Still))
PowerEff + geom_point(alpha = 0.2) + geom_smooth(se = FALSE) + labs(x = "Solar Power/W", y = "Hourly Transient Efficiency/%") + scale_y_continuous(limits = c(10,100)) + scale_x_continuous(limits = c(100,1000))

