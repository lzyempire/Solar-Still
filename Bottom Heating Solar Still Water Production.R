##Get solar still water daily production data
setwd("D:/R/Solar Still")
library(lubridate)
data_record <- as.Date("2020-06-05")
SolarWater <- read.csv(file = "Bottom Heating Solar Still Water Production.csv", header = FALSE, stringsAsFactors = FALSE)
SolarWater_Day <- c(SolarWater[1, 1], SolarWater[1, 2])
for(i in 2:(length(SolarWater)/2)){
  daydata <- c(SolarWater[1, 2*i-1], SolarWater[1, 2*i])
  SolarWater_Day <- rbind(SolarWater_Day, daydata)
}
SolarWater_Day <- as.data.frame(SolarWater_Day, stringsAsFactors = FALSE)
names(SolarWater_Day) <- c("Date", "Water_Production")
row.names(SolarWater_Day) <- SolarWater_Day[, 1]
SolarWater_Day$Date <- ymd(SolarWater_Day$Date)
SolarWater_Day <- SolarWater_Day[SolarWater_Day$Date >= data_record, ]
SolarWater_Day$Water_Production <- as.numeric(SolarWater_Day$Water_Production)*4/1000
SolarWater_Day$Water_Energy <- SolarWater_Day$Water_Production/1.5

##Get solar environment daily data
setwd("D:/R/Solar Still")
library(lubridate)
SolarEnv_Day <- read.csv(file = "CR1000_BSRN1000_Day201022.csv", skip = 1, stringsAsFactors = FALSE)
SolarEnvUnit_Day <- SolarEnv_Day[1, ]
SolarEnv_Day <- SolarEnv_Day[c(-1, -2), ] ##Delete two rows of unit
SolarEnv_Day$TIMESTAMP <- as.Date(ymd_hms(SolarEnv_Day$TIMESTAMP))
SolarEnv_Day$TIMESTAMP <- SolarEnv_Day$TIMESTAMP - ddays(1)
SolarEnv_Day[, 2:39] <- lapply(SolarEnv_Day[, 2:39], as.numeric)
##Select data column and analysis
datacol <- c("TIMESTAMP", "Global_Energy_Tot", "Direct_Energy_Tot", "Diffuse_Energy_Tot")
SolarData_Day <- SolarEnv_Day[c(SolarEnv_Day$TIMESTAMP >= data_record), datacol]
row.names(SolarData_Day) <- SolarData_Day[, 1]
SolarDataUnit_Day <- SolarEnvUnit_Day[datacol]
SolarData_Day <- merge(SolarData_Day, SolarWater_Day[, c("Date", "Water_Energy")], by.x = "TIMESTAMP", by.y = "Date", all = TRUE)
library(reshape2)
SolarEnergy_Day <- melt(SolarData_Day, id = "TIMESTAMP")
SolarData_Day$Global_Efficiency <- SolarData_Day$Water_Energy/SolarData_Day$Global_Energy_Tot
##SolarData_Day$Direct_Efficiency <- SolarData_Day$Water_Energy/SolarData_Day$Direct_Energy_Tot

SolarData_Day$DirDiffRatio <- SolarData_Day$Direct_Energy_Tot/SolarData_Day$Diffuse_Energy_Tot
SolarData_Day$DirDiff <- cut(SolarData_Day$DirDiffRatio, breaks = c(min(na.omit(SolarData_Day$DirDiffRatio)), 0.1, 1, max(na.omit(SolarData_Day$DirDiffRatio))), labels = c("Cloudy", "Between", "Clear"))

library(ggplot2)
##fit1 <- lm(Water_Energy ~ Global_Energy_Tot + Direct_Energy_Tot, data = na.omit(SolarData_Day))
##fit2 <- lm(Water_Energy ~ Global_Energy_Tot, data = na.omit(SolarData_Day))
##fit3 <- lm(Water_Energy ~ poly(Global_Energy_Tot, 2), data = na.omit(SolarData_Day))
##fit4 <- lm(Water_Energy ~ Global_Energy_Tot + I(Global_Energy_Tot^2), data = na.omit(SolarData_Day))
fit1 <- lm(Global_Efficiency ~ Direct_Energy_Tot + Diffuse_Energy_Tot, data = na.omit(SolarData_Day))
fit2 <- lm(Global_Efficiency ~ Direct_Energy_Tot + Diffuse_Energy_Tot, data = na.omit(SolarData_Day[-c(105, 106, 122), ]))
SolarData_Day$fit_Efficiency <- coefficients(fit2)[1] + coefficients(fit2)[2]*SolarData_Day$Direct_Energy_Tot + coefficients(fit2)[3]*SolarData_Day$Diffuse_Energy_Tot
lmfit2 <- ggplot(na.omit(melt(SolarData_Day[, c("Global_Energy_Tot", "Global_Efficiency", "fit_Efficiency")], id = "Global_Energy_Tot")), 
                 aes(Global_Energy_Tot, value*100, color = variable))
lmfit2 + geom_point()

write.csv(SolarData_Day, file = "Bottom Heating Solar Still Daily Water Production.csv")



g <- ggplot(na.omit(SolarData_Day), aes(x = Global_Energy_Tot, y = Water_Energy))
g + geom_point() + geom_smooth(method = "lm") + geom_text(data = na.omit(SolarData_Day), aes(label = TIMESTAMP), check_overlap = TRUE)
q <- ggplot(data = SolarData_Day, aes(TIMESTAMP, Global_Efficiency))
q + geom_point()
o <- ggplot(data = na.omit(SolarData_Day), aes(Global_Energy_Tot, Global_Efficiency, color = DirDiff))
o + geom_point() + geom_smooth(method = "lm")
##r <- ggplot(data = SolarData_Day, aes(Direct_Energy_Tot, Global_Efficiency))
##r + geom_point()
p <- ggplot(SolarEnergy_Day, aes(TIMESTAMP, value, fill = variable))
p + geom_bar(stat = 'identity', position='dodge') + labs(x = "Date", y = "Energy/kWh") 