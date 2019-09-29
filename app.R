#
# punchlist 
# 
# better way to do datatable
# data validity based on nulls throughout DF, not only for variables viewed
# break into tabs - scatterplot, validity, summary
# state filter


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
            sidebarMenu(
                menuItem("Dashboard", tabName = "dashTab", icon = icon("dashboard")),
                menuItem("Omitted info", icon = icon("th"), tabName = "omitTab",
                         badgeLabel = "watch", badgeColor = "yellow"),
                menuItem("Summary info", icon = icon("th"), tabName = "summTab",
                         badgeLabel = "Coming Soon", badgeColor = "red")
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
                        box(plotOutput("testplot"))                        
                        ),
                tabItem(tabName = "omitTab", h2("Null omitted tab content"),
                        #clicking this button lets you see a table of the omitted results
                        box(actionButton("omits", "See table of omitted results")),
                        box(dataTableOutput('table')),
                        box(gaugeOutput("DVGauge"),
                            width=4,title="Data validity gauge",background ="green"
                            ),
                        box(valueBoxOutput("NArows"),
                            width=4, title = "Number of rows with null value", background = "yellow"
                            ),
                        box(valueBoxOutput("OKrows"),
                            width=4, title = "Okay rows", background = "blue"
                            )
                        ),
                tabItem(tabName = "summTab", h2("Summary info variable tab content")
                        )
                )
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
    #data validity gauge
    output$DVGauge <- renderGauge({
        percok2 <- as.integer(100*nrow(na.omit(newdata()))/nrow(newdata()))
        gauge(percok2, min = 0, max = 100, symbol = '%',
              label = paste("% data w/o nulls in query"),
              gaugeSectors(success = c(90, 100), warning = c(60,90), danger = c(0, 60))
        )
    })
    #number of null rows
    output$NArows <- renderValueBox({
        partna <- nrow(newdata()[rowSums(is.na(newdata()))>0,])
        valueBox(partna)
    })
    output$OKrows <- renderValueBox({
        partok <- nrow(na.omit(newdata()))
        valueBox(partok)
    })
}

# Run the application 
shinyApp(ui=ui, server=server)
