#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
#reading and merging data files for geography names with FIPS
CCFIPS <- read.csv(file="citycountyfips.csv")
SASMCD <- read.csv(file="SAS_County_Data.csv")
CountyDataWNames <- merge(CCFIPS, 
                          SASMCD, 
                          by.x = "CityCountyFIPS", 
                          by.y= "FIPS")

#LAOPR80 scatterplot
ggplot(CountyDataWNames, aes(x=Long_Acting_Opioid_Pres2, 
                             y=Opioid_Prescribing_Rate2,
                             color=target_LAOPR_80)) + geom_point()
#LAOPR90 scatterplot
ggplot(CountyDataWNames, aes(x=Long_Acting_Opioid_Pres2, 
                             y=Opioid_Prescribing_Rate2,
                             color=target_LAOPR_90)) + geom_point()


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            plotOutput("testplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    output$testplot <- renderPlot({
        ggplot(CountyDataWNames, aes(x=Long_Acting_Opioid_Pres2, 
                                 y=Opioid_Prescribing_Rate2,
                                 color=target_LAOPR_80)) + geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
