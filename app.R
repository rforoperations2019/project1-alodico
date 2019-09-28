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
library(dplyr)
#reading and merging data files for geography names with FIPS
CCFIPS <- read.csv(file="citycountyfips.csv")
SASMCD <- read.csv(file="SAS_County_Data.csv")
CountyData <- merge(CCFIPS,
                    SASMCD, 
                    by.x = "CityCountyFIPS", 
                    by.y= "FIPS")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("County Level Opioid Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #This is a range slider that allows you to choose which year(s) of data you are looking at
            sliderInput("years",
                        label = "Include data from year(s):",
                        min = 2013,
                        max = 2017,
                        value = c(2013,2017)),
            #and this select box lets you choose the dependent variable
            selectInput("DepVar", label = h3("Choose Dependent Variable"), 
                        choices = list("Opioid Prescription Rate" = "Opioid_Prescribing_Rate2", 
                                       "Long Acting Opioid Prescription Rate" = "Long_Acting_Opioid_Pres2",
                                       "Binomial Target Variable 80" = "target_LAOPR_80", 
                                       "Binomial Target Variable 90" = "target_LAOPR_90")
                        ),
            #and this select box lets you choose the independent variable
            selectInput("IndVar", label = h3("Choose Independent Variable"), 
                        choices = list("Opioid Prescription Rate" = "Opioid_Prescribing_Rate2", 
                                       "Long Acting Opioid Prescription Rate" = "Long_Acting_Opioid_Pres2",
                                       "Binomial Target Variable 80" = "target_LAOPR_80", 
                                       "Binomial Target Variable 90" = "target_LAOPR_90")
                        )
            ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("testplot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    newdata <- reactive({CountyData %>% filter(year_num %in% (input$years[1]:input$years[2]))
        })
    # newy <- observe({
    #     print(input$DepVar)
    # })
    # obsB <- observe({
    #     print(input$years[1])
    #     print(input$years[2])
    # })
    output$testplot <- renderPlot({
        ggplot(newdata(), aes_string(x=input$IndVar,
                                 y=input$DepVar,
                                 color="target_LAOPR_80")) + geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
