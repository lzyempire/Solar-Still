#
library(shiny)
library(RCurl)
Still_BottomInter <- read.csv(text = getURL("https://raw.githubusercontent.com/lzyempire/Solar-Still/master/Bottom%20Interfacial%20Solar%20Still%20Daily%20Water%20Production.csv"), header = TRUE, stringsAsFactors = FALSE)
Still_SidewallInter <- read.csv(text = getURL("https://raw.githubusercontent.com/lzyempire/Solar-Still/master/Sidewall%20Interfacial%20Solar%20Still%20Daily%20Water%20Production.csv"), header = TRUE, stringsAsFactors = FALSE)
Still_FoamBottomInter <- read.csv(text = getURL("https://raw.githubusercontent.com/lzyempire/Solar-Still/master/Foam%20Bottom%20Interfacial%20Solar%20Still%20Daily%20Water%20Production.csv"), header = TRUE, stringsAsFactors = FALSE)
Still_BottomHeat <- read.csv(text = getURL("https://raw.githubusercontent.com/lzyempire/Solar-Still/master/Bottom%20Heating%20Solar%20Still%20Daily%20Water%20Production.csv"), header = TRUE, stringsAsFactors = FALSE)
Still_SidewallHeat <- read.csv(text = getURL("https://raw.githubusercontent.com/lzyempire/Solar-Still/master/Sidewall%20Bottom%20Heating%20Solar%20Still%20Daily%20Water%20Production.csv"), header = TRUE, stringsAsFactors = FALSE)

library(dplyr)
Solar_Efficiency <- Still_BottomHeat[, c("Global_Energy_Tot", "Global_Efficiency")] %>% 
    full_join(Still_SidewallHeat[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot") %>% 
    full_join(Still_BottomInter[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot") %>% 
    full_join(Still_FoamBottomInter[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot") %>% 
    full_join(Still_SidewallInter[, c("Global_Energy_Tot", "Global_Efficiency")], by = "Global_Energy_Tot")
names(Solar_Efficiency) <- c("Solar_Energy", "Bottom_Heating", "Sidewall_Bottom_Heating", "Bottom_Interfacial", "Foam_Bottom_Interfacial", "Sidewall_Interfacial")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({
        xmin <- input$sliderX[1]
        xmax <- input$sliderX[2]
        ymin <- input$sliderY[1]
        ymax <- input$sliderY[2]
        library(ggplot2)
        library(reshape2)
        Solar_Eff <- melt(Solar_Efficiency[, c(TRUE, input$show_1, input$show_2, input$show_3, input$show_4, input$show_5)], id = "Solar_Energy")
        p <- ggplot(Solar_Eff, aes(Solar_Energy, value*100, color = variable))
        p + geom_point() + labs(x = "Solar Energy/kWh", y = "Efficiency/%") + geom_smooth(method="loess",se=FALSE) + scale_x_continuous(limits = c(xmin,xmax)) + scale_y_continuous(limits = c(ymin,ymax))

    })

})
