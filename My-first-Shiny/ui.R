library(shiny)

shinyUI(fluidPage(
    titlePanel("Different Solar Still Water Production Efficiency vs Solar Energy"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("sliderX", "Pick Minimum and Maximum of Solar Energy Values(kWh)",
                        0, 10, value = c(0, 8)),
            sliderInput("sliderY", "Pick Minimum and Maximum of Efficiency Values(%)",
                        0, 100, value = c(0, 60)),
            checkboxInput("show_1", "Show/Hide Bottom Interfacial Solar Still", value = TRUE),
            checkboxInput("show_2", "Show/Hide Foam Bottom Interfacial Solar Still", value = TRUE),
            checkboxInput("show_3", "Show/Hide Sidewall Bottom Heating Solar Still", value = TRUE),
            checkboxInput("show_4", "Show/Hide Sidewall Interfacial Solar Still", value = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
