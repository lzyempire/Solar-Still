setwd("D:/R/Solar Still")
library(lubridate)
Still_BottomInter <- read.csv(file = "Bottom Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_SidewallInter <- read.csv(file = "Sidewall Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_FoamBottomInter <- read.csv(file = "Foam Bottom Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
##Still_FoamSidewallInter <- read.csv(file = "Foam Sidewall Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_SidewallHeat <- read.csv(file = "Sidewall Bottom Heating Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)

WaterProduction_Energy <- merge(merge(merge(Still_BottomInter[, c("TIMESTAMP", "Global_Energy_Tot", "Water_Energy")], Still_FoamBottomInter[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP", all = TRUE), Still_SidewallHeat[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP", all = TRUE), Still_SidewallInter[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP", all = TRUE)
names(WaterProduction_Energy) <- c("Date", "Bottom_Interfacial", "Sidewall_Interfacial", "Foam_Bottom_Interfacial", "Sidewall_Bottom_Heating")
WaterProduction_Efficiency <- merge(merge(merge(Still_BottomInter[, c("TIMESTAMP", "Global_Efficiency")], Still_FoamBottomInter[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP", all = TRUE), Still_SidewallHeat[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP", all = TRUE), Still_SidewallInter[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP", all = TRUE)
names(WaterProduction_Efficiency) <- c("Date", "Bottom_Interfacial", "Sidewall_Interfacial", "Foam_Bottom_Interfacial", "Sidewall_Bottom_Heating")
write.csv(WaterProduction_Efficiency, file = "Water Efficiency.csv")
Solar_Efficiency <- merge(merge(merge(Still_BottomInter[, c("Global_Energy_Tot", "Global_Efficiency")], Still_FoamBottomInter[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot", all = TRUE), Still_SidewallHeat[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot", all = TRUE), Still_SidewallInter[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot", all = TRUE)
names(Solar_Efficiency) <- c("Solar_Energy", "Bottom_Interfacial", "Foam_Bottom_Interfacial", "Sidewall_Bottom_Heating", "Sidewall_Interfacial")

library(reshape2)
Water_Energy <- melt(WaterProduction_Energy, id = "Date")
Water_Energy$Date <- ymd(Water_Energy$Date)
Water_Eff <- melt(WaterProduction_Efficiency, id = "Date")
Water_Eff$Date <- ymd(Water_Eff$Date)
Solar_Eff <- melt(Solar_Efficiency, id = "Solar_Energy")



library(ggplot2)
##g <- ggplot(Water_Energy, aes(Date, value, fill = variable))
##g + geom_bar(stat = 'identity', position='dodge') + labs(x = "Date", y = "Energy/kWh") 
p <- ggplot(Solar_Eff, aes(Solar_Energy, value*100, color = variable))
p + geom_point() + labs(x = "Solar Energy/kWh", y = "Efficiency/%") + geom_smooth(method="loess",se=FALSE)
##q <- ggplot(Water_Eff, aes(Date, value*100, fill = variable))
##q + geom_bar(stat = 'identity', position='dodge') + labs(x = "Date", y = "Efficiency/%") 









##With SidewallInter



setwd("D:/R/Solar Still")
library(lubridate)
Still_BottomInter <- read.csv(file = "Bottom Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_SidewallInter <- read.csv(file = "Sidewall Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_FoamBottomInter <- read.csv(file = "Foam Bottom Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_FoamSidewallInter <- read.csv(file = "Foam Sidewall Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_SidewallHeat <- read.csv(file = "Sidewall Bottom Heating Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)

WaterProduction_Energy <- merge(merge(merge(merge(Still_BottomInter[, c("TIMESTAMP", "Global_Energy_Tot", "Water_Energy")], Still_SidewallInter[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP", all = TRUE), Still_FoamBottomInter[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP", all = TRUE), Still_FoamSidewallInter[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP", all = TRUE), Still_SidewallHeat[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP", all = TRUE)
names(WaterProduction_Energy) <- c("Date", "Bottom_Interfacial", "Sidewall_Interfacial", "Foam_Bottom_Interfacial", "Foam_Siderwall_Interfacial", "Sidewall_Bottom_Heating")
WaterProduction_Efficiency <- merge(merge(merge(merge(Still_BottomInter[, c("TIMESTAMP", "Global_Efficiency")], Still_SidewallInter[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP", all = TRUE), Still_FoamBottomInter[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP", all = TRUE), Still_FoamSidewallInter[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP", all = TRUE), Still_SidewallHeat[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP", all = TRUE)
names(WaterProduction_Efficiency) <- c("Date", "Bottom_Interfacial", "Sidewall_Interfacial", "Foam_Bottom_Interfacial", "Foam_Siderwall_Interfacial", "Sidewall_Bottom_Heating")
write.csv(WaterProduction_Efficiency, file = "Water Efficiency.csv")
Solar_Efficiency <- merge(merge(merge(merge(Still_BottomInter[, c("Global_Energy_Tot", "Global_Efficiency")], Still_SidewallInter[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot", all = TRUE), Still_FoamBottomInter[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot", all = TRUE), Still_FoamSidewallInter[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot", all = TRUE), Still_SidewallHeat[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot", all = TRUE)
names(Solar_Efficiency) <- c("Solar_Energy", "Bottom_Interfacial", "Sidewall_Interfacial", "Foam_Bottom_Interfacial", "Foam_Siderwall_Interfacial", "Sidewall_Bottom_Heating")

library(reshape2)
Water_Energy <- melt(WaterProduction_Energy, id = "Date")
Water_Energy$Date <- ymd(Water_Energy$Date)
Water_Eff <- melt(WaterProduction_Efficiency, id = "Date")
Water_Eff$Date <- ymd(Water_Eff$Date)
Solar_Eff <- melt(Solar_Efficiency, id = "Solar_Energy")



library(ggplot2)
g <- ggplot(Water_Energy, aes(Date, value, fill = variable))
g + geom_bar(stat = 'identity', position='dodge') + labs(x = "Date", y = "Energy/kWh") 
p <- ggplot(Solar_Eff, aes(Solar_Energy, value*100, color = variable))
p + geom_point() + labs(x = "Solar Energy/kWh", y = "Efficiency/%") + geom_smooth(method="loess",se=FALSE)
q <- ggplot(Water_Eff, aes(Date, value*100, fill = variable))
q + geom_bar(stat = 'identity', position='dodge') + labs(x = "Date", y = "Efficiency/%") 
