setwd("D:/R/Solar Still")
library(lubridate)
Still_BottomInter <- read.csv(file = "Bottom Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_SidewallInter <- read.csv(file = "Sidewall Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_FoamBottomInter <- read.csv(file = "Foam Bottom Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_BottomHeat <- read.csv(file = "Bottom Heating Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_SidewallHeat <- read.csv(file = "Sidewall Bottom Heating Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)

library(dplyr)
WaterProduction_Energy <- Still_BottomHeat[, c("TIMESTAMP", "Global_Energy_Tot", "Water_Energy")] %>% 
  full_join(Still_SidewallHeat[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP") %>% 
  full_join(Still_BottomInter[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP") %>% 
  full_join(Still_FoamBottomInter[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP") %>% 
  full_join(Still_SidewallInter[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP")
names(WaterProduction_Energy) <- c("Date", "SolarEnergy", "Bottom_Heating", "Sidewall_Bottom_Heating", "Bottom_Interfacial", "Foam_Bottom_Interfacial", "Sidewall_Interfacial")
WaterProduction_Efficiency <- Still_BottomHeat[, c("TIMESTAMP", "Global_Efficiency")] %>% 
  full_join(Still_SidewallHeat[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP") %>% 
  full_join(Still_BottomInter[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP") %>% 
  full_join(Still_FoamBottomInter[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP") %>% 
  full_join(Still_SidewallInter[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP")
names(WaterProduction_Efficiency) <- c("Date", "Bottom_Heating", "Sidewall_Bottom_Heating", "Bottom_Interfacial", "Foam_Bottom_Interfacial", "Sidewall_Interfacial")
write.csv(WaterProduction_Efficiency, file = "Water Efficiency.csv")
Solar_Efficiency <- Still_BottomHeat[, c("Global_Energy_Tot", "Global_Efficiency")] %>% 
  full_join(Still_SidewallHeat[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot") %>% 
  full_join(Still_BottomInter[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot") %>% 
  full_join(Still_FoamBottomInter[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot") %>% 
  full_join(Still_SidewallInter[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot")
names(Solar_Efficiency) <- c("Solar_Energy", "Bottom_Heating", "Sidewall_Bottom_Heating", "Bottom_Interfacial", "Foam_Bottom_Interfacial", "Sidewall_Interfacial")

Direct_Efficiency <- Still_BottomHeat[, c("Direct_Energy_Tot", "Global_Efficiency")] %>% 
  full_join(Still_SidewallHeat[, c("Direct_Energy_Tot", "Global_Efficiency")], by = "Direct_Energy_Tot") %>% 
  full_join(Still_BottomInter[, c("Direct_Energy_Tot", "Global_Efficiency")], by = "Direct_Energy_Tot") %>% 
  full_join(Still_FoamBottomInter[, c("Direct_Energy_Tot", "Global_Efficiency")], by = "Direct_Energy_Tot") %>% 
  full_join(Still_SidewallInter[, c("Direct_Energy_Tot", "Global_Efficiency")], by = "Direct_Energy_Tot")
names(Direct_Efficiency) <- c("Direct_Energy", "Bottom_Heating", "Sidewall_Bottom_Heating", "Bottom_Interfacial", "Foam_Bottom_Interfacial", "Sidewall_Interfacial")
Diffuse_Efficiency <- Still_BottomHeat[, c("Diffuse_Energy_Tot", "Global_Efficiency")] %>% 
  full_join(Still_SidewallHeat[, c("Diffuse_Energy_Tot", "Global_Efficiency")], by = "Diffuse_Energy_Tot") %>% 
  full_join(Still_BottomInter[, c("Diffuse_Energy_Tot", "Global_Efficiency")], by = "Diffuse_Energy_Tot") %>% 
  full_join(Still_FoamBottomInter[, c("Diffuse_Energy_Tot", "Global_Efficiency")], by = "Diffuse_Energy_Tot") %>% 
  full_join(Still_SidewallInter[, c("Diffuse_Energy_Tot", "Global_Efficiency")], by = "Diffuse_Energy_Tot")
names(Diffuse_Efficiency) <- c("Diffuse_Energy", "Bottom_Heating", "Sidewall_Bottom_Heating", "Bottom_Interfacial", "Foam_Bottom_Interfacial", "Sidewall_Interfacial")

library(reshape2)
Water_Energy <- melt(WaterProduction_Energy, id = "Date")
Water_Energy$Date <- ymd(Water_Energy$Date)
Water_Eff <- melt(WaterProduction_Efficiency, id = "Date")
Water_Eff$Date <- ymd(Water_Eff$Date)
Solar_Eff <- melt(Solar_Efficiency, id = "Solar_Energy")
Direct_Eff <- melt(Direct_Efficiency, id = "Direct_Energy")
Diffuse_Eff <- melt(Diffuse_Efficiency, id = "Diffuse_Energy")


library(ggplot2)
##g <- ggplot(Water_Energy, aes(Date, value, fill = variable))
##g + geom_bar(stat = 'identity', position='dodge') + labs(x = "Date", y = "Energy/kWh") 
SolarEff <- ggplot(Solar_Eff, aes(Solar_Energy, value*100, color = variable))
SolarEff + geom_point() + labs(x = "Solar Energy/kWh", y = "Efficiency/%") + geom_smooth(method="loess",se=FALSE)
DirectEff <- ggplot(Direct_Eff, aes(Direct_Energy, value*100, color = variable))
DirectEff + geom_point() + labs(x = "Direct Energy/kWh", y = "Efficiency/%") + geom_smooth(method="loess",se=FALSE)
DiffuseEff <- ggplot(Diffuse_Eff, aes(Diffuse_Energy, value*100, color = variable))
DiffuseEff + geom_point() + labs(x = "Diffuse Energy/kWh", y = "Efficiency/%") + geom_smooth(method="loess",se=FALSE)
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
