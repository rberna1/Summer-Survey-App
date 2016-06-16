rm(list=ls())
setwd("A:/Summer Survey Data")
survey <- read.csv("2016-06-14_CPIC_JOINED_DeID_for_REU.csv")

#Establish Libraries
library(shiny)
library(shinyAce)
library(shinyBS)
library(shinythemes)
library(RCurl)
library(networkD3)
library(igraph)
library(ggplot2)
library(plyr)
library(shinydashboard)
library(corrplot)

#Define Gender Column
survey$gender <- as.factor(survey$gender_male_t1 + 2*survey$gender_female_t1 + 3*survey$gender_slfdsc_t1
                           + 4*survey$gender_prfr_not_answr_t1)
survey$gender <- mapvalues(survey$gender, from = c("1", "2","3","4"), to = c("Male","Female","Other",
                                                                             "Preferred not to answer"))


ui <- fluidPage(
      dashboardPage(
  
        skin = "blue",
        dashboardHeader(title = "CPIC Summer Fellowship Pre-Program Survey Data"),
  
        dashboardSidebar(
          sidebarMenu(
            menuItem("Basic Demographics", tabName = "basic", icon = icon("th")),
            
            selectInput("funding","Funding Source", choices = c("All",levels(survey$fund_source))),
            selectInput("gender", "Gender", choices= c("All",levels(survey$gender)))
      
          )
        ),
        
        dashboardBody(
          
          tabItems(
            tabItem(tabName = "basic",              
                    tabBox(
                      title = "Basic Demographics",
                      id = "tabset1",
                      tabPanel("Age", plotOutput('age')),
                      tabPanel("Gender", plotOutput('gender')),
                      tabPanel("Parents' Education", plotOutput('education')),
                      tabPanel("Parents' Income", plotOutput('education')),
                      tabPanel("Ethnicity", plotOutput('ethnicity')),
                      width = 11
                    )
            )
             
          )
        
        )
      )
)


server <- function(input,output,session)  { 
  
  observe({
    updateSelectInput(session, inputId = "funding", 
                      label = "Funding Source", choices = c("All",unique(survey$fund_source )),
                      selected=c("All"))
  })
  
  file0 = reactive({
    if (input$funding != "All"){
      survey[survey$fund_source == input$funding,]
    } else {
      survey
    } 
  })
  
  observe({
    updateSelectInput(session, inputId = "gender", 
                      label = "Gender", choices = c("All",unique(file0()$gender)),selected=c("All"))
  })
  
  file = reactive({
    if (input$gender != "All"){
      file0()[file0()$gender == input$gender,]
    } else {
      file0()
    } 
  })
  
  }

shinyApp(ui=ui,server=server)
  
