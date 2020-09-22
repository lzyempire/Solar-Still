#
library(shiny)
setwd("D:/R/Solar Still")
library(lubridate)
Still_BottomInter <- read.csv(file = "Bottom Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_SidewallInter <- read.csv(file = "Sidewall Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_FoamBottomInter <- read.csv(file = "Foam Bottom Interfacial Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)
Still_SidewallHeat <- read.csv(file = "Sidewall Bottom Heating Solar Still Daily Water Production.csv", header = TRUE, stringsAsFactors = FALSE)

Solar_Efficiency <- merge(merge(merge(Still_BottomInter[, c("Global_Energy_Tot", "Global_Efficiency")], Still_FoamBottomInter[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot", all = TRUE), Still_SidewallHeat[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot", all = TRUE), Still_SidewallInter[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot", all = TRUE)
names(Solar_Efficiency) <- c("Solar_Energy", "Bottom_Interfacial", "Foam_Bottom_Interfacial", "Sidewall_Bottom_Heating", "Sidewall_Interfacial")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({
        xmin <- input$sliderX[1]
        xmax <- input$sliderX[2]
        ymin <- input$sliderY[1]
        ymax <- input$sliderY[2]
        library(ggplot2)
        Solar_Eff <- melt(Solar_Efficiency[, c(TRUE, input$show_1, input$show_2, input$show_3, input$show_4)], id = "Solar_Energy")
        p <- ggplot(Solar_Eff, aes(Solar_Energy, value*100, color = variable))
        p + geom_point() + labs(x = "Solar Energy/kWh", y = "Efficiency/%") + geom_smooth(method="loess",se=FALSE) + scale_x_continuous(limits = c(xmin,xmax)) + scale_y_continuous(limits = c(ymin,ymax))

    })

})
