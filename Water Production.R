setwd("D:/R/Solar Still")
library(lubridate)
Still_BottomInter <- read.csv(file = "Bottom Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_SidewallInter <- read.csv(file = "Sidewall Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_FoamBottomInter <- read.csv(file = "Foam Bottom Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_BottomHeat <- read.csv(file = "Bottom Heating Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_SidewallHeat <- read.csv(file = "Sidewall Bottom Heating Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_FoamSidewallInter <- read.csv(file = "Foam Sidewall Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
StillNames <- c("Bottom_Heating", "Sidewall_Bottom_Heating", "Bottom_Interfacial", "Foam_Bottom_Interfacial", "Sidewall_Interfacial", "Foam_Sidewall_Interfacial")

SolarEnv_Day <- read.csv(file = "CR1000_BSRN1000_Day201022.csv", skip = 1, stringsAsFactors = FALSE)
SolarEnv_Day <- SolarEnv_Day[c(-1, -2), ] ##Delete two rows of unit
SolarEnv_Day$TIMESTAMP <- as.Date(ymd_hms(SolarEnv_Day$TIMESTAMP))
SolarEnv_Day$TIMESTAMP <- SolarEnv_Day$TIMESTAMP - ddays(1)
SolarEnv_Day$TIMESTAMP <- as.character(SolarEnv_Day$TIMESTAMP)
SolarEnv_Day[, 2:39] <- lapply(SolarEnv_Day[, 2:39], as.numeric)

library(dplyr)
WaterProduction_Energy <- Still_BottomHeat[, c("TIMESTAMP", "Global_Energy_Tot", "Water_Energy")] %>% 
  full_join(Still_SidewallHeat[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP") %>% 
  full_join(Still_BottomInter[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP") %>% 
  full_join(Still_FoamBottomInter[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP") %>% 
  full_join(Still_SidewallInter[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP") %>%
  full_join(Still_FoamSidewallInter[, c("TIMESTAMP", "Water_Energy")], by = "TIMESTAMP")
names(WaterProduction_Energy) <- c("Date", "Solar_Radiation", StillNames)
WaterProduction_Efficiency <- Still_BottomHeat[, c("TIMESTAMP", "Global_Efficiency")] %>% 
  full_join(Still_SidewallHeat[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP") %>% 
  full_join(Still_BottomInter[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP") %>% 
  full_join(Still_FoamBottomInter[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP") %>% 
  full_join(Still_SidewallInter[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP") %>% 
  full_join(Still_FoamSidewallInter[, c("TIMESTAMP", "Global_Efficiency")], by = "TIMESTAMP") %>% 
  left_join(SolarEnv_Day[, c("TIMESTAMP", "Global_Energy_Tot", "Direct_Energy_Tot", "Diffuse_Energy_Tot")], by = "TIMESTAMP")
names(WaterProduction_Efficiency) <- c("Date", StillNames, "Solar_Energy", "Direct_Energy", "Diffuse_Energy")
write.csv(WaterProduction_Efficiency, file = "Water Efficiency.csv")
Direct_Efficiency <- WaterProduction_Efficiency[WaterProduction_Efficiency$Direct_Energy >= 1, ]
Diffuse_Efficiency <- WaterProduction_Efficiency[WaterProduction_Efficiency$Direct_Energy < 1, ]
fit_Efficiency <- Still_BottomHeat[, c("TIMESTAMP", "fit_Efficiency")] %>% 
  full_join(Still_SidewallHeat[, c("TIMESTAMP", "fit_Efficiency")], by = "TIMESTAMP") %>% 
  full_join(Still_BottomInter[, c("TIMESTAMP", "fit_Efficiency")], by = "TIMESTAMP") %>% 
  full_join(Still_FoamBottomInter[, c("TIMESTAMP", "fit_Efficiency")], by = "TIMESTAMP") %>% 
  full_join(Still_SidewallInter[, c("TIMESTAMP", "fit_Efficiency")], by = "TIMESTAMP") %>% 
  full_join(Still_FoamSidewallInter[, c("TIMESTAMP", "fit_Efficiency")], by = "TIMESTAMP") %>% 
  left_join(SolarEnv_Day[, c("TIMESTAMP", "Global_Energy_Tot", "Direct_Energy_Tot", "Diffuse_Energy_Tot")], by = "TIMESTAMP")
names(fit_Efficiency) <- c("Date", StillNames, "Solar_Energy", "Direct_Energy", "Diffuse_Energy")

library(reshape2)
Water_Energy <- melt(WaterProduction_Energy, id = "Date")
Water_Energy$Date <- ymd(Water_Energy$Date)
Water_Eff <- melt(WaterProduction_Efficiency, id = "Date")
Water_Eff$Date <- ymd(Water_Eff$Date)
Solar_Eff <- melt(WaterProduction_Efficiency[, c("Solar_Energy", StillNames)], id = "Solar_Energy")
Direct_Eff <- melt(Direct_Efficiency[, c("Direct_Energy", StillNames)], id = "Direct_Energy")
Diffuse_Eff <- melt(Diffuse_Efficiency[, c("Diffuse_Energy", StillNames)], id = "Diffuse_Energy")
Total_eff <- melt(WaterProduction_Efficiency[, c("Solar_Energy", "Direct_Energy", "Diffuse_Energy", StillNames)], id = c("Solar_Energy", "Direct_Energy", "Diffuse_Energy"))
names(Total_eff) <- c("Solar_Energy", "Direct_Energy", "Diffuse_Energy", "Still_Type", "Efficiency")
fit_Eff <- melt(fit_Efficiency[, c("Solar_Energy", StillNames)], id = "Solar_Energy")

library(ggplot2)
StillEnergy <- ggplot(Water_Energy, aes(Date, value, fill = variable))
StillEnergy + geom_bar(stat = 'identity', position='dodge') + labs(x = "Date", y = "Energy/kWh") 
SolarEff <- ggplot(Solar_Eff, aes(Solar_Energy, value*100, color = variable))
SolarEff + geom_point() + labs(x = "Solar Energy/kWh", y = "Efficiency/%") + geom_smooth(method="loess",se=FALSE)
fitEff <- ggplot(fit_Eff, aes(Solar_Energy, value*100, color = variable))
fitEff + geom_point() + labs(x = "Solar Energy/kWh", y = "Efficiency/%") + geom_smooth(method="loess",se=FALSE)

##DirectEff <- ggplot(Direct_Eff[Direct_Eff$variable %in% c("Bottom_Heating", "Foam_Bottom_Interfacial"), ], aes(Direct_Energy, value*100, color = variable))
##DirectEff + geom_point() + labs(x = "Direct Energy/kWh", y = "Efficiency/%") + geom_smooth(method="lm")
##DiffuseEff <- ggplot(Diffuse_Eff[Diffuse_Eff$variable %in% c("Bottom_Heating", "Foam_Bottom_Interfacial"), ], aes(Diffuse_Energy, value*100, color = variable))
##DiffuseEff + geom_point() + labs(x = "Diffuse Energy/kWh", y = "Efficiency/%") + geom_smooth(method="lm")
DirectEff <- ggplot(Direct_Eff, aes(Direct_Energy, value*100, color = variable))
DirectEff + geom_point() + labs(x = "Direct Energy/kWh", y = "Efficiency/%") + geom_smooth(method="loess",se=FALSE)
DiffuseEff <- ggplot(Diffuse_Eff, aes(Diffuse_Energy, value*100, color = variable))
DiffuseEff + geom_point() + labs(x = "Diffuse Energy/kWh", y = "Efficiency/%") + geom_smooth(method="loess",se=FALSE)
##q <- ggplot(Water_Eff, aes(Date, value*100, fill = variable))
##q + geom_bar(stat = 'identity', position='dodge') + labs(x = "Date", y = "Efficiency/%") 


library(plotly)
StillEnergy_plotly <- plot_ly(WaterProduction_Energy, x = ~Date, y = ~Solar_Radiation, type = 'bar', name = 'Solar Radiation') %>%
  add_trace(y = ~Bottom_Heating, name = 'Bottom Heating') %>%
  add_trace(y = ~Sidewall_Bottom_Heating, name = 'Sidewall Bottom Heating') %>%
  add_trace(y = ~Bottom_Interfacial, name = 'Bottom Interfacial') %>%
  add_trace(y = ~Foam_Bottom_Interfacial, name = 'Foam Bottom Interfacial') %>%
  add_trace(y = ~Sidewall_Interfacial, name = 'Sidewall Interfacial') %>%
  add_trace(y = ~Foam_Sidewall_Interfacial, name = 'Foam Sidewall Interfacial') %>%
  layout(
    title = "Solar Stills Daily Water Production Energy vs Solar Radiation Energy",
    yaxis = list(title = 'Energy/kWh'), 
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(
            count = 3,
            label = "3 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 6,
            label = "6 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 1,
            label = "1 yr",
            step = "year",
            stepmode = "backward"),
          list(
            count = 1,
            label = "YTD",
            step = "year",
            stepmode = "todate"),
          list(step = "all"))),
      rangeslider = list(type = "date")),
    barmode = 'group')
StillEnergy_plotly

SolarEff_plotly <- plot_ly(Solar_Eff, x = ~Solar_Energy, y = ~value, color = ~variable, trendline="lowess")
SolarEff_plotly

DirDiffEff_plotly <- plot_ly(Total_eff, x = ~Direct_Energy, y = ~Diffuse_Energy, z = ~Efficiency*100, color = ~Still_Type, type = 'scatter3d')
DirDiffEff_plotly

library(shiny)
library(ggplot2)
shinyApp(
  ui = fluidPage(
    titlePanel("Solar Still Daily Desalination Energy Efficiency"),
    sliderInput("slider_Date", label = h3("Date Range Slider"), min = min(Water_Energy$Date, na.rm = TRUE), 
                max = max(Water_Energy$Date, na.rm = TRUE), value = c(as.Date('2020-08-01'), as.Date('2020-09-01')), width='100%'),
    plotOutput("StillEnergy_Shiny")
  ),
  
  server = function(input, output) {
    output$StillEnergy_Shiny = renderPlot({
      StillEnergy <- ggplot(na.omit(Water_Energy[(Water_Energy$Date >= input$slider_Date[1])&(Water_Energy$Date <= input$slider_Date[2]), ]), aes(Date, value, fill = variable))
      ##StillEnergy <- ggplot(Water_Energy, aes(Date, value, fill = variable))
      StillEnergy + geom_bar(stat = 'identity', position='dodge') + labs(x = "Date", y = "Energy/kWh")
    })
  },
  options = list(height = 600)
)