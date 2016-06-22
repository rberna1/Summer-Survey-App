rm(list=ls())
##RON
#setwd("A:/Summer Survey Data")
#survey <- read.csv("2016-06-14_CPIC_JOINED_DeID_for_REU.csv")
##Dustin
#setwd("C:/Users/dtingley/Dropbox")
##Irving
setwd("A:/Summer Survey App/")
survey <- read.csv("2016-06-14_CPIC_JOINED_DeID_for_REU.csv")
#Everyone put their local path here. Don't delete anyone elses
#setwd("TRICAM2016/publicservice")
#survey <- read.csv("data/2016-06-14_CPIC_JOINED_DeID_for_REU.csv")

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
                                                                             "No Answer"))
survey$demog_age_t1 <- as.factor(survey$demog_age_t1)


ui <- fluidPage(
  dashboardPage(
    
    skin = "blue",
    dashboardHeader(title = "CPIC Summer Fellowship Pre-Program Survey Data"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Basic Demographics", tabName = "basic", icon = icon("th")),
        menuItem("Goals and Skills", tabName = "goals", icon = icon("th")), #Section 1 of 7
        menuItem("Self Awareness and Reflection", tabName = "selfAwareness", icon = icon("th")), #Section 4 of 7
        menuItem("Future Plans", tabName = "future", icon = icon("th")),
        menuItem("Community Engagement", tabName = "community", icon = icon("th")),
        
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
        
        tabItem(tabName = "goals",          #Section 1 of 7
                tabBox(
                  title = "Goals and Skills",
                  id = "tabset2",
                  tabPanel("Goals", plotOutput("plot.goals", height = 800)), #Question 1
                  tabPanel("Skills", plotOutput("plot.skills", height = 800)), #Question 4
                  tabPanel("Experience", plotOutput("plot.exp", height = 800)), #Question 5
                  tabPanel("Competence", plotOutput("plot.competence", height = 800)), #Question 6
                  tabPanel("Work Environment", plotOutput("plot.work", height = 800)), #Question 7
                  width = 11
                )
                
        ),
        
        tabItem(tabName = "selfAwareness",          #Section 4 of 7
                tabBox(
                  title = "Self Awareness and Reflection",
                  id = "tabset4",
                  tabPanel("Aspects of Self", plotOutput("plot.self", height = 800)), #Question 20
                  tabPanel("Thoughts of Self", plotOutput("plot.thoughts", height = 800)), #Question 21
                  tabPanel("Thoughts of Community", plotOutput("plot.community", height = 800)), #Question 22
                  tabPanel("Project Engagement", plotOutput("plot.engagement", height = 800)), #Question 23
                  width = 11
                )
        ),

        tabItem(tabName = "future",              
                tabBox(
                  title = "Future Plans",
                  id = "tabset5",
                  tabPanel("Interests Correlation", plotOutput('int_corr')),
                  tabPanel("Same Sector", plotOutput('same_sec')),
                  tabPanel("Different Sector", plotOutput('diff_sec')),
                  width = 11
                )
        ),
        
        tabItem(tabName = "community",              
                tabBox(
                  title = "Community Engagement",
                  id = "tabset3",
                  tabPanel("Contribute", plotOutput('contribute', height = 800)),
                  tabPanel("Services", plotOutput('services', height = 800)),
                  tabPanel("Knowledge", plotOutput('knowledge', height = 800)),
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
  
  ####Demographics Tab  
  ###Age Plot 
  output$age <- renderPlot({
    numeric.age <- as.numeric(file()$demog_age_t1)
    numeric.age <- mapvalues(numeric.age, from = c("1", "2","3","4","5"), to = c(18,19,20,21,22))
    mean_age <- round( mean(numeric.age,na.rm=TRUE), 2)
    plot(file()$demog_age_t1,main="Distribution of Age",xlab="Age of Participants",
         ylab="Frequency")
    legend("topleft",paste("Mean Age is", mean_age))
    
  })
  
  ###Gender Plot
  output$gender <- renderPlot({
    barplot(table(file()$gender)/sum(table(file()$gender)),main="Distribution of Gender",
            xlab="Proportion of Students",horiz=TRUE)
  })
  
  ###Parents' Education (Where does the ' go in "parents"??????)
  output$education <- renderPlot({
    factor.education <- mapvalues(file()$demog_COARSE_CONT_parent_highest_ed_t1,
                                  from = c("1","2","3","4","5","6","7"), 
                                  to = c("Did not finish high school",
                                         "High school diploma or G.E.D.",
                                         "Didn't complete college degree",
                                         "Associate's degree","Bachelor's degree",
                                         "Master's degree","Doctoral or professional degree"))
    const.education <- mapvalues(survey$demog_COARSE_CONT_parent_highest_ed_t1,
                                 from = c("1","2","3","4","5","6","7"), 
                                 to = c("Did not finish high school",
                                        "High school diploma or G.E.D.",
                                        "Didn't complete college degree",
                                        "Associate's degree","Bachelor's degree",
                                        "Master's degree","Doctoral or professional degree"))
    par(mar = c(3, 9, 2, 2) + 4.8) 
    barplot(table(const.education),main = "Parents' Highest Degree",
            xlab="Frequency",las=1,horiz=TRUE,font.lab=1)
    
  })
  
  ###Parents' Income
  output$income <- renderPlot({
    factor.income <- as.factor(file()$demog_fam_ann_inc_t1)
    factor.income <- mapvalues(factor.income, from = c("1","2","3","4","5","6","7"),
                               to = c("$0-$49,999","$50,000-$99,999","$100,000-$149,999",
                                      "$150,000-$249,999","$250,000-$499,999","$500,000 or more",
                                      "No Answer"))
    par(mar = c(3, 9, 2, 2) + 0.2) 
    barplot(table(factor.income),main = "Parents' Annual Income",
            xlab="Frequency",las=1,horiz=TRUE,font.lab=1)
    
  })
  
  ###Ethnicity
  output$ethnicity <- renderPlot({
    eths <- rep(0,721)
    eths[1:103] <- mapvalues(file()$demog_race_white_t,
                             from = c("0","1"), 
                             to = c(NA,"White"))
    eths[104:206] <- mapvalues(file()$demog_race_amer_ind_alsk_nat_t1,
                               from = c("0","1"), 
                               to = c(NA,"American Indian"))
    eths[207:309] <- mapvalues(file()$demog_race_black_af_am_t1,
                               from = c("0","1"),
                               to = c(NA,"Black"))
    eths[310:412] <- mapvalues(file()$demog_race_nat_hwii_pcfc_isl_t1,
                               from = c("0","1"),
                               to = c(NA,"Native Hawaiian/Pacific Islander"))
    eths[413:515] <- mapvalues(file()$demog_race_asian_asian_amer_t1,
                               from = c("0","1"),
                               to = c(NA,"Asian"))
    eths[516:618] <- mapvalues(file()$demog_race_latino_latina_t1,
                               from = c("0","1"),
                               to = c(NA,"Latino"))
    eths[619:721] <- mapvalues(file()$demog_race_other_t1,
                               from = c("0","1"),
                               to = c(NA,"Other"))
    eths <- as.factor(eths)
    par(mar = c(3, 9, 2, 2) + 0.2)
    plot(eths,main="Ethnicities",xlab="Frequency",las=1,horiz=TRUE,font.lab=1)
    
  })
  
  
  ####The following function will create a plot (similar to a scatterplot) for a set of
  ####survey questions. Give it the indices of the columns for your questions (e.g. 7:13),
  ####a vector of question labels for the y-axis
  
  surveyPlot <- function(columns, title, questionLabels){
    par(mfrow = c(2, 1), mar=c(4,15,10,12), xpd=TRUE)
    plot(NULL, xlim=c(0.5,5.5), ylim=c(0.5,length(columns)), xlab = "", ylab = "", axes = FALSE, #Generate an empty plot
         main = title)
    axis(side = 1, at = c(1:5), #add an x-axis
         labels = c("Not at all", "To a small extent", "To a moderate Extent", "To a large extent", "To a great extent"))
    axis(side = 2, at = c(1:length(columns)), las = 2, #add a y-axis
         labels = rev(questionLabels))
    
    for(i in 1:length(columns)){ #Iterate through the columns, adding a row of points for each subquestion
      points(x = jitter(file()[,columns[1]-1+i], 1),
             y = rep(length(columns)+1-i, times = nrow(file())), 
             pch = 19, col = rgb(0, 0, 0, 0.15), cex = 2)
      points(x = mean(file()[,columns[1]-1+i], na.rm = TRUE), y = length(columns)+1-i, #Mean value for each subquestion response
             pch = 19, col = rgb(1, 0, 0, 0.85), cex = 2)
      points(x = median(file()[,columns[1]-1+i], na.rm = TRUE), y = length(columns)+1-i, #Median value for each subquestion response
             pch = 19, col = rgb(0, 0, 1, 0.85), cex = 2)
      legend("right", c("Individual Response", "Mean Response", "Median Response"), 
             col = c(rgb(0, 0, 0, 0.15), rgb(1, 0, 0, 0.85), rgb(0, 0, 1, 0.85)),
             pch = c(19, 19, 19), inset=c(-0.07,0))
    }
    
    #Generate boxplots for the different columns
    plot(NULL, xlim=c(0.5,5.5), ylim=c(0.5,length(columns)), xlab = "", ylab = "", axes = FALSE, #Generate an empty plot
         main = title)
    axis(side = 1, at = c(1:5), #add an x-axis
         labels = c("Not at all", "To a small extent", "To a moderate Extent", "To a large extent", "To a great extent"))
    axis(side = 2, at = c(1:length(columns)), las = 2, #add a y-axis
         labels = rev(questionLabels))
    #Add boxplots:
    boxplot(rev(file()[,columns]), names = rev(questionLabels), horizontal = TRUE, add = TRUE, axes = FALSE)
  }
  
  #Section 1 of 7
  output$plot.goals <- renderPlot({ #Plot for Question 1, columns 7 to 13
    title = "Q1: To what extent do you want to accomplish the following goals through participation in your fellowship?"
    questionLabels = c("Make contacts", "Resume", "Community involvement", "Career-related",
                       "Social justice", "Leadership", "Helpful to others")
    surveyPlot(7:13, title, questionLabels)
  })
  
  output$plot.skills <- renderPlot({ #Plot for Question 4, columns 14 to 18
    title = "Q4: To what extent do you..."
    questionLabels = c("Listen to ideas", "Understand perspectives", "Lead groups", "Delegate", 
                       "Devise solutions")
    surveyPlot(14:18, title, questionLabels)
  })
  
  output$plot.exp <- renderPlot({ #Plot for Question 5, columns 19 to 22
    title = "Q5: To what extent do you have experience writing the following?"
    questionLabels = c("Executive summaries", "Press releases", "Advocacy statements", "Blog posts")
    surveyPlot(19:22, title, questionLabels)
  })
  
  output$plot.competence <- renderPlot({ #Plot for Question 6, columns 23 to 27
    title = "Q6: In the context of working on a research project, to what extent do you believe that you are competent at the following?"
    questionLabels = c("Research questions", "Evaluating sources", "Consistent citation",
                       "Written communication", "Presenting research")
    surveyPlot(23:27, title, questionLabels)
  })
  
  output$plot.work <- renderPlot({ #Plot for Question 6, columns 23 to 27
    title = "Q7:In a work environment, to what extent do you believe that you can do the following?"
    questionLabels = c("Personal responsibility", "Work independently", "Conform to attire",
                       "Be on time", "Meet deadlines")
    surveyPlot(28:32, title, questionLabels)
  })
  
  #Section 4 of 7
  output$plot.self <- renderPlot({ #Plot for Question 20, columns 60 to 62
    title = "Q20: To what extent would you say that you consciously think about the following aspects of yourself?"
    questionLabels = c("Your strengths", "Your Weaknesses", "Your values and beliefs")
    surveyPlot(60:62, title, questionLabels)
  })
  
  output$plot.thoughts <- renderPlot({ #Plot for Question 21, columns 63 to 65
    title = "Q21: To what extent would you say that you consciously think about the following?"
    questionLabels = c("Influencing your behavior", "Improving yourself", "Influence of experiences")
    surveyPlot(63:65, title, questionLabels)
  })
  
  output$plot.community <- renderPlot({ #Plot for Question 22, columns 66 to 67
    title = "Q22: To what extent would you say that you consciously think about the following? (In this question, community is defined as an identity group, civic, or cultural group.)"
    questionLabels = c("Make a difference", "Issues facing community")
    surveyPlot(66:67, title, questionLabels)
  })
  
  output$plot.engagement <- renderPlot({ #Plot for Question 23, columns 68 to 71
    title = "Q23: Now, reflect on the extent of your engagement in the following activities while working on this project."
    questionLabels = c("Usefulness of strategies", "Requested feedback", "Used feedback",
                       "Communicated concerns")
    surveyPlot(68:71, title, questionLabels)
  })
  
###Irving's Section  
  ##Interests Correlation
  output$int_corr <- renderPlot({
    file<-file()
    
    ### labels for question about interest questions (Question 11)
    interest.labels = c("CourseIssues","CourseCommunity","CoursePublicService",
                        "UndergradIssues", "SeniorThesis","GradDegreeIssues",
                        "GradDegreecommunity")
    
    validate(
      need(nrow(na.omit(file[33:39])) != 0, "No data available for this course")
    )
    
    # creates labels to be used in plot, then plots the corrPlot
    colnames(file)[33:39] = interest.labels
    corrplot(cor(file[33:39],use="complete.obs"),method = "square")
    
  })
  
  ## Bar Plots for same sector
  output$same_sec = renderPlot({
    # Set the params to allow 2x2 graphs on one tab
    par(mar = c(3, 7, 2, 2) + 0.2) 
    par(mfrow=c(2,2))
    
    # eliminate NA values
    file = subset(file(), subset = 
                    career_int_same_sector_vol_t1 != "" &
                    career_int_same_sector_pt_coll_t1 != "" &
                    career_int_same_sector_pt_post_t1 != "" &
                    career_int_same_sector_ft_post_t1 != ""
    )
    
    # ensure there is data to plot
    validate(
      need(nrow(file) != 0, "No data available for this course")
    )
    
    question = "Do you think you will engage in public service in the same sector as your fellowship?"
    xlabels = c("No", "Possibly", "Maybe", "Probably", "Definitely")
    graphs = c("Volunteer", "Intern/PT (during college)", "Intern/PT (after college)", "FT After College", "Definitely")
    var = c("career_int_same_sector_vol_t1", "career_int_same_sector_pt_coll_t1",
            "career_int_same_sector_pt_post_t1", "career_int_same_sector_ft_post_t1")
    
    #cols 40:43
    
    #for %, divide first arg by /sum(table(file[,var[i]])
    for (i in 1:4){
      fine <- mapvalues(file[,var[i]],from=c(1,2,3,4,5),to=xlabels, warn_missing = FALSE)
    
      barplot(table(factor(fine, levels = xlabels)),las=1, main = paste(graphs[i]),
                    cex.names=1.15, xlab="Response", font.lab=2, ylab = "# of Responses")
    }
    
  })
  
  ## Bar Plots for different sector
  output$diff_sec = renderPlot({
    # Set the params to allow 2x2 graphs on one tab
    par(mar = c(3, 7, 2, 2) + 0.2) 
    par(mfrow=c(2,2))
    
    # eliminate NA values
    file = subset(file(), subset = 
                    career_int_diff_sector_vol_t1 != "" &
                    career_int_diff_sector_pt_coll_t1 != "" &
                    career_int_diff_sector_pt_post_t1 != "" &
                    career_int_diff_sector_ft_post_t1 != ""
    )
    
    # ensure there is data to plot
    validate(
      need(nrow(file) != 0, "No data available for this course")
    )
    
    question = "Do you think you will engage in public service in a different sector as your fellowship?"
    xlabels = c("1:No", "2:Possibly", "3:Maybe", "4:Probably", "5:Definitely")
    graphs = c("Volunteer", "Intern/PT (during college)", "Intern/PT (after college)", "FT After College", "Definitely")
    var = names(file[44:47])
    
    #mapvalues(file[44:47], from = c(1,2,3,4,5), xlabels, warn_missing = FALSE)
    #var = c("career_int_diff_sector_vol_t1", "career_int_diff_sector_pt_coll_t1",
    #        "career_int_diff_sector_pt_post_t1", "career_int_diff_sector_ft_post_t1")
    
    #for %, divide first arg by /sum(table(file[,var[i]])
    for (i in 1:4){
      finer <- mapvalues(file[,var[i]], from=c(1,2,3,4,5), to=xlabels)
      barplot(table(finer), las=1, main=paste(graphs[i]), cex.names=1.15, xlab="Response", font.lab=2, ylab = "# of Responses")
    }
    
  })
  
  # TODO: Flesh this out in order to modularize bar plots (like in diff_sec/same_sec)
  # TODO: Figure out how to label bars on x-axis correctly
  make_a_bar_plot <- function(title, colStart, colEnd, xlabels, ylabel)({
    
    relevant_df = file()[colStart:colEnd]
    
    var = names(relevant_df)
    
    file = subset(file(), subset = 
                    career_int_diff_sector_vol_t1 != "" &
                    career_int_diff_sector_pt_coll_t1 != "" &
                    career_int_diff_sector_pt_post_t1 != "" &
                    career_int_diff_sector_ft_post_t1 != ""
    )
    
    
    
  })
  
  # TODO: Flesh this out in order to modularize corr plots (like in int_corr)
  # TODO: Should be funcitoning, but needs more testing
  make_a_corr_plot <- function(title, colStart, colEnd, labels)({
    
    file<-file()
    
    ### labels for question about interest questions (Question 11)
    # labels 
    
    validate(
      need(nrow(na.omit(file[colStart:colEnd])) != 0, "No data available for this course")
    )
    
    # creates labels to be used in plot, then plots the corrPlot
    colnames(file)[colStart:colEnd] = labels
    corrplot(cor(file[colStart:colEnd],use="complete.obs"),method = "square")
  })
  surveyPlot <- function(columns, title, questionLabels){
    par(mfrow = c(2, 1), mar=c(4,15,10,12), xpd=TRUE)
    plot(NULL, xlim=c(0.5,5.5), ylim=c(0.5,length(columns)), xlab = "", ylab = "", axes = FALSE, #Generate an empty plot
         main = title)
    axis(side = 1, at = c(1:5), #add an x-axis
         labels = c("Not at all", "To a small extent", "To a moderate Extent", "To a large extent", "To a great extent"))
    axis(side = 2, at = c(1:length(columns)), las = 2, #add a y-axis
         labels = rev(questionLabels))
    
    for(i in 1:length(columns)){ #Iterate through the columns, adding a row of points for each subquestion
      points(x = jitter(file()[,columns[1]-1+i], 1),
             y = rep(length(columns)+1-i, times = nrow(file())), 
             pch = 19, col = rgb(0, 0, 0, 0.15), cex = 2)
      points(x = mean(file()[,columns[1]-1+i], na.rm = TRUE), y = length(columns)+1-i, #Mean value for each subquestion response
             pch = 19, col = rgb(1, 0, 0, 0.85), cex = 2)
      points(x = median(file()[,columns[1]-1+i], na.rm = TRUE), y = length(columns)+1-i, #Median value for each subquestion response
             pch = 19, col = rgb(0, 0, 1, 0.85), cex = 2)
      legend("right", c("Individual Response", "Mean Response", "Median Response"), 
             col = c(rgb(0, 0, 0, 0.15), rgb(1, 0, 0, 0.85), rgb(0, 0, 1, 0.85)),
             pch = c(19, 19, 19), inset=c(-0.07,0))
    }
    
    #Generate boxplots for the different columns
    plot(NULL, xlim=c(0.5,5.5), ylim=c(0.5,length(columns)), xlab = "", ylab = "", axes = FALSE, #Generate an empty plot
         main = title)
    axis(side = 1, at = c(1:5), #add an x-axis
         labels = c("Not at all", "To a small extent", "To a moderate Extent", "To a large extent", "To a great extent"))
    axis(side = 2, at = c(1:length(columns)), las = 2, #add a y-axis
         labels = rev(questionLabels))
    #Add boxplots:
    boxplot(rev(file()[,columns]), names = rev(questionLabels), horizontal = TRUE, add = TRUE, axes = FALSE)
  }
  
  output$services <- renderPlot({
    title = "Q15. To what extent do you believe that...?"
    questionnames = c("Your community service", "Everyone's community", "Public service at Harvard", 
                      "Society fairer", "Community engagement everyone", "Community engagement themselves")
    surveyPlot(52:57, title, questionnames)
  })  
  
  output$knowledge <- renderPlot({
    title = "Q16. To what extent do you believe that...?"
    questionnames = c("Community challenges", "Address these challenges")
    surveyPlot(58:59, title, questionnames)
  })
  surveyPlot2 <- function(columns, title, questionLabels){
    par(mfrow = c(2, 1), mar=c(4,15,10,12), xpd=TRUE)
    plot(NULL, xlim=c(0.5,5.5), ylim=c(0.5,length(columns)), xlab = "", ylab = "", axes = FALSE, #Generate an empty plot
         main = title)
    axis(side = 1, at = c(1:5), #add an x-axis
         labels = c("Very poor", "Poor", "Average", "Good", "Excellent"))
    axis(side = 2, at = c(1:length(columns)), las = 2, #add a y-axis
         labels = rev(questionLabels))
    
    for(i in 1:length(columns)){ #Iterate through the columns, adding a row of points for each subquestion
      points(x = jitter(file()[,columns[1]-1+i], 1),
             y = rep(length(columns)+1-i, times = nrow(file())), 
             pch = 19, col = rgb(0, 0, 0, 0.15), cex = 2)
      points(x = mean(file()[,columns[1]-1+i], na.rm = TRUE), y = length(columns)+1-i, #Mean value for each subquestion response
             pch = 19, col = rgb(1, 0, 0, 0.85), cex = 2)
      points(x = median(file()[,columns[1]-1+i], na.rm = TRUE), y = length(columns)+1-i, #Median value for each subquestion response
             pch = 19, col = rgb(0, 0, 1, 0.85), cex = 2)
      legend("right", c("Individual Response", "Mean Response", "Median Response"), 
             col = c(rgb(0, 0, 0, 0.15), rgb(1, 0, 0, 0.85), rgb(0, 0, 1, 0.85)),
             pch = c(19, 19, 19), inset=c(-0.07,0))
    }
    
    #Generate boxplots for the different columns
    plot(NULL, xlim=c(0.5,5.5), ylim=c(0.5,length(columns)), xlab = "", ylab = "", axes = FALSE, #Generate an empty plot
         main = title)
    axis(side = 1, at = c(1:5), #add an x-axis
         labels = c("Very poor", "Poor", "Average", "Good", "Excellent"))
    axis(side = 2, at = c(1:length(columns)), las = 2, #add a y-axis
         labels = rev(questionLabels))
    #Add boxplots:
    boxplot(rev(file()[,columns]), names = rev(questionLabels), horizontal = TRUE, add = TRUE, axes = FALSE)
  }
  
  output$contribute <- renderPlot({
    title = "Q14. Please select the response that best represents your ability to do the following."
    questionnames = c("Well-being", "Resolve disagreements", "Resolve bias", "Different background")
    surveyPlot2(48:51, title, questionnames)
  }) 
  
}

shinyApp(ui=ui,server=server)