#
# punchlist 
# 
# add two graphs to dashboard
# better way to do datatable
# data validity based on nulls throughout DF, not only for variables viewed
# format omit tab
# fill out summary tab
# state filter
# checkbox to cha


library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(flexdashboard)
#reading and merging data files for geography names with FIPS
CCFIPS <- read.csv(file="citycountyfips.csv")
SASMCD <- read.csv(file="SAS_County_Data.csv")
CountyData <- merge(CCFIPS, SASMCD, by.x = "CityCountyFIPS", by.y= "FIPS")

# Define UI for my dashboard
ui <- dashboardPage(
  dashboardHeader(title = "County Level Opioid Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashTab", icon = icon("dashboard"))
    ),
    
    #This is a range slider that allows you to choose which year(s) of data you are looking at
    sliderInput("years",
                label = "Year range for data:",
                min =2013, max = 2017, value = c(min,max)
                # min side of slider below only works if its on a whole year integer this way
                #                        min(as.integer(CountyData$year_num)), max = max(as.integer(CountyData$year_num)), value = c(min ,max)
    ),
    #and this select box lets you choose the dependent variable
    selectInput("DepVar", label = h3("Dependent Variable"), 
                choices = list("Opioid Prescription Rate" = "Opioid_Prescribing_Rate2", 
                               "Long Acting Opioid Prescription Rate" = "Long_Acting_Opioid_Pres2",
                               "Binomial Target Variable 80" = "target_LAOPR_80", 
                               "Binomial Target Variable 90" = "target_LAOPR_90")
    ),
    #and this select box lets you choose the independent variable
    selectInput("IndVar", label = h3("Independent Variable"), 
                choices = list("Total Claims" = "Overall_Claims",
                               "Opioid Prescription Rate" = "Opioid_Prescribing_Rate2", 
                               "Long Acting Opioid Prescription Rate" = "Long_Acting_Opioid_Pres2",
                               "Part_D_Opioid_Prescribers",
                               "Part_D_Prescribers",
                               "FFS_Beneficiaries",
                               "Hospital_Readmission_Rate2")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashTab", h2("Main Dashboard - scatterplot"),
              #This is the output for the scatterplot
              box(plotOutput("testplot")),
              box(plotOutput("boxplotDV")),
              box(plotOutput("boxplotIV"))
              )
      )
    )
  ) 


# Define server logic
server <- function(input, output) {
  #This is filtering the data by the year slider
  newdata <- reactive({CountyData %>% filter(year_num %in% (input$years[1]:input$years[2]))
  })
  #GGplot of the variables 
  # output$testplot <- renderPlot({
  #   ggplot(newdata(), aes_string(x=input$IndVar,
  #                                y=input$DepVar,
  #                                color="target_LAOPR_80")) + geom_point()
  # })
  output$boxplotDV <- renderPlot({
    ggplot(newdata(), aes_string(x=as.factor(newdata()$target_LAOPR_80),
                                 y=input$DepVar,
                                 color="target_LAOPR_80")) + geom_boxplot()
  })
  output$boxplotIV <- renderPlot({
    ggplot(newdata(), aes_string(x=as.factor(newdata()$target_LAOPR_80),
                                 y=input$IndVar,
                                 color="target_LAOPR_80")) + geom_boxplot()
  })

}

# Run the application 
shinyApp(ui=ui, server=server)