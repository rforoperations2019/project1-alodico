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
library(shinydashboard)
library(flexdashboard)
#reading and merging data files for geography names with FIPS
CCFIPS <- read.csv(file="citycountyfips.csv")
SASMCD <- read.csv(file="SAS_County_Data.csv")
CountyData <- merge(CCFIPS,
                    SASMCD, 
                    by.x = "CityCountyFIPS", 
                    by.y= "FIPS")

# Define UI for my dashboard
ui <- dashboardPage(
        dashboardHeader(title = "County Level Opioid Data"),
        # Sidebar layout default for now 
        dashboardSidebar(
            #This is a range slider that allows you to choose which year(s) of data you are looking at
            sliderInput("years",
                        label = "Include data from year(s):",
                        min = 2013,
                        max = 2017,
                        value = c(2013,2017)
                        ),
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
                                       "Overall_Claims",
                                       "Part_D_Opioid_Prescribers",
                                       "Part_D_Prescribers",
                                       "FFS_Beneficiaries",
                                       "Hospital_Readmission_Rate2")
                        ),
            #and clicking this button lets you see a table of the omitted results
            actionButton("omits", "See table of omitted results")
            ),
        # Show a plot of the generated distribution
        dashboardBody(
            #This is the output for the scatterplot
            box(plotOutput("testplot")),
            #and the data table  
            box(dataTableOutput('table'))
        )
    )

# Define server logic
server <- function(input, output) {
    #This is filtering the data by the year slider
    newdata <- reactive({CountyData %>% filter(year_num %in% (input$years[1]:input$years[2]))
        })
    
    #This creates a new data table of omitted rows once omits action button is clicked
    #I couldnt figure out how to make this work without putting the newdata line from above in here
    omitted <-eventReactive(input$omits,{
        newdata <- CountyData %>% filter(year_num %in% (input$years[1]:input$years[2]))
        omitbetween <- newdata[rowSums(is.na(newdata))>0,]
        })      
    #GGplot of the variables 
    output$testplot <- renderPlot({
        ggplot(newdata(), aes_string(x=input$IndVar,
                                 y=input$DepVar,
                                 color="target_LAOPR_80")) + geom_point()
        })
    #the output for the optional data table above.
    output$table <- renderDataTable({
        omitted()
        })
}

# Run the application 
shinyApp(ui=ui, server=server)
