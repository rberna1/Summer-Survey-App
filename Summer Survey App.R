rm(list=ls())
setwd("A:/Summer Survey App/")
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
survey$demog_age_t1 <- as.factor(survey$demog_age_t1)


ui <- fluidPage(
  dashboardPage(
    
    skin = "blue",
    dashboardHeader(title = "CPIC Summer Fellowship Pre-Program Survey Data"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Basic Demographics", tabName = "basic", icon = icon("th")),
        menuItem("Future Plans", tabName = "future", icon = icon("th")),
        
        selectInput(inputId="funding",label="Funding Source", choices = c("All",levels(survey$fund_source))),
        selectInput(inputId="gender",label="Gender", choices= c("All",levels(survey$gender)))
        
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
                  tabPanel("Parents' Income", plotOutput('income')),
                  tabPanel("Ethnicity", plotOutput('ethnicity')),
                  width = 11
                )
        ),
        
        tabItem(tabName = "future",              
                tabBox(
                  title = "Future Plans",
                  id = "tabset2",
                  tabPanel("Interests Correlation", plotOutput('int_corr')),
                  tabPanel("Same Sector", plotOutput('gender')),
                  tabPanel("Different Sector", plotOutput('education')),
                  width = 11
                )
        )
      )
    )
  )
)





server <- function(input,output,session)  { 
  
  ###Establishes file() as the reactive subset of survey desired
  ##This definitely could be cleaner...
  
  observe({
    updateSelectInput(session, inputId = "funding", 
                      label = "Funding Source", choices = c("All",levels(survey$fund_source )),
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
                      label = "Gender", choices = c("All",levels(file0()$gender)),
                      selected=c("All"))
  })
  
  file = reactive({
    if (input$gender != "All"){
      file0()[file0()$gender == input$gender,]
    } else {
      file0()
    } 
  })
  
  ###Age Plot 
  output$age <- renderPlot({
    numeric.age <- as.numeric(file()$demog_age_t1)
    numeric.age <- mapvalues(numeric.age, from = c("1", "2","3","4","5"), to = c(18,19,20,21,22))
    mean_age <- round( mean(numeric.age,na.rm=TRUE), 2)
    plot(file()$demog_age_t1,main="Distribution of Age",xlab="Age of Participants",
         ylab="Frequency")
    text(1.5,25,paste("Mean Age is", mean_age))
    
  })
  
  ##Interests Correlation
  output$int_corr <- renderPlot({
    file<-file()
    print(class(file))
    
    ### labels for question about interest questions (Question 11)
    interest.labels = c("CourseIssues","CourseCommunity","CoursePublicService",
                        "UndergradIssues", "SeniorThesis","GradDegreeIssues",
                        "GradDegreecommunity")
    
    # keep<-c("acad_fut_courses_issues_org_t1", "acad_fut_courses_community_org_t1",
    #          "acad_fut_courses_eng_sch_t1", "acad_fut_conc_issues_org_t1",
    #          "acad_fut_sen_thesis_issues_org_t1", "acad_fut_grad_deg_issues_org_t1",
    #          "acad_fut_grad_deg_community_org_t1")

    validate(
       need(nrow(na.omit(file[33:39])) != 0, "No data available for this course")
    )
    #corrplot(cor(file[,motiv.name],use="complete.obs"))
    
    #plotting on survey works outside of ui
    #corrplot(cor(survey[33:39],use="complete.obs"),method = "square")
    #colnames(survey)[33:39] = interest.labels
    
    #plotting on file does not work within ui
    corrplot(cor(file[33:39],use="complete.obs"),method = "square")
    
    
  })
  
  
}

shinyApp(ui=ui,server=server)
