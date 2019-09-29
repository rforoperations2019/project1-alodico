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

CCFIPS <- read.csv(file="citycountyfips.csv")
SASMCD <- read.csv(file="SAS_County_Data.csv")
CountyData <- merge(CCFIPS,
                    SASMCD, 
                    by.x = "CityCountyFIPS", 
                    by.y= "FIPS")

ui <- fluidPage(
    dashboardPage(
        dashboardHeader(title = "Box testing ground"),
        dashboardSidebar(helpText("Test Sidebar")),
        dashboardBody(
            fluidRow(
                box(title = "textoutput",
                    textOutput("text_calc")
                    ),
                box(title = "gauge",
                    gaugeOutput("plt1")
                    ),
                box(title = "null rows",
                    valueBoxOutput("NArows")
                    )
            )
        )
    )
    )
    

server <- function(input, output) {
    newdata <- reactive({CountyData %>% filter(year_num %in% (2014:2016))
            })
    output$text_calc <- renderText({
#       calculation for number of rows
        partok <- nrow(na.omit(newdata()))
        partna <- nrow(newdata()[rowSums(is.na(newdata()))>0,])
        totrow <- nrow(newdata())
        percok <- 100*partok/totrow
        paste("This query has", partna, "rows with nulls, or ", percok,
              "and this many", partok, "good roows",
              "for a total of", totrow, "rows")
    })

    output$plt1 <- renderGauge({
        # partok <- nrow(na.omit(newdata()))
        # partna <- nrow(newdata()[rowSums(is.na(newdata()))>0,])
        # totrow <- nrow(newdata())
        # percok <- as.integer(100*partok/totrow)
        percok2 <- as.integer(100*nrow(na.omit(newdata()))/nrow(newdata()))
        gauge(percok2, min = 0, max = 100, symbol = '%',
              label = paste("% data valid in query"),
              gaugeSectors(success = c(90, 100), warning = c(60,90), danger = c(0, 60))
        )
    })
    
    output$NArows <- renderValueBox({
        partna <- nrow(newdata()[rowSums(is.na(newdata()))>0,])
        valueBox(partna)
    })
}

shinyApp(ui, server)