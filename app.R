rm(list=ls())

library(openxlsx)
library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(scales)
library(plotly)
library(shinythemes)

##LOAD DATA
a <- read.xlsx("C:/Users/timmy.karlsson/Documents/runR/data.xlsx")
a[is.na(a)] <- 0

##FIX DATE
a$date2 <- as.Date(a$date,origin = "1899-12-30")
a$yearvar <- year(a$date2)
a$monthvar <- month(a$date2)
a$weekvar <- week(a$date2)
a$month <- as.Date(cut(a$date2,
                       breaks = "month"))
a$week <- as.Date(cut(a$date2,
                      breaks = "week",
                      start.on.monday = TRUE))
a_2015 <- subset(a, yearvar == 2015) 
a_2016 <- subset(a, yearvar == 2016) 
a_2017 <- subset(a, yearvar == 2017)

##KPIs
antalkm <- sum(a$km,  na.rm=TRUE)
antalpass <- sum(a$no_of_runs, na.rm=TRUE)
km2015 <- sum(a_2015$km,  na.rm=TRUE)
km2016 <- sum(a_2016$km,  na.rm=TRUE)
km2017 <- sum(a_2017$km,  na.rm=TRUE)

##CREATE KM PER WEEK PLOT
kmweek <- a %>% group_by(week) %>% summarise(kmweek=sum(km))
kmweekplot <- ggplot(kmweek, aes(as.Date(week), kmweek)) + geom_point() + scale_x_date() +
  ylab("Aggregated by Week") + xlab("Week") + geom_line()

##CREATE KM PER MONTH PLOT
kmmonth <- a %>% group_by(month) %>% summarise(kmmonth=sum(km))
kmmonthplot <- ggplot(kmmonth, aes(as.Date(month), kmmonth)) + geom_point() + scale_x_date() +
  ylab("Aggregated by Month") + xlab("Month") + geom_line()

##CREATE TYPE OF RUN PER WEEK PLOT
typeweek <- a %>% 
  select(run1, run2, week) %>% 
  gather(runtype, value, -week, na.rm = TRUE) %>% 
  group_by(week, value) %>% 
  summarise(n = n()) 

typeweek<-typeweek[!(typeweek$value==0),]

typeweek$value <- factor(typeweek$value,
                    levels = c("D","DL","F","I","T"),
                    labels = c("Easy run", "Long run", "Tempo run", "Interval", "Race"))

typeweekplot <- ggplot(typeweek, aes( x = week, y = n, fill = value)) + 
  geom_bar(stat="identity", position = "stack") + labs(x = "Week", y = "", fill = "Type of run")
   
##CREATE TYPE OF RUN PER MONTH PLOT
typemonth <- a %>% 
  select(run1, run2, month) %>% 
  gather(runtype, value, -month, na.rm = TRUE) %>% 
  group_by(month, value) %>% 
  summarise(n = n()) 

typemonth<-typemonth[!(typemonth$value==0),]

typemonth$value <- factor(typemonth$value,
                         levels = c("D","DL","F","I","T"),
                         labels = c("Easy run", "Long run", "Tempo run", "Interval", "Race"))

typemonthplot <- ggplot(typemonth, aes( x = month, y = n, fill = value)) + 
  geom_bar(stat="identity", position = "stack") + labs(x = "Month", y = "", fill = "Type of run")


            
###APP BELOW

# BODY
body <- dashboardBody(
  tags$head(tags$style(HTML('.content-wrapper, right-side {
  background-color: #222;
                            }
  .small-box, h3 {
  font-size: 20px;
  }'))),
  fluidRow(
    box(
      title = "# of runs", width = 3, height = 50, background = "green",
      column(width=1, valueBoxOutput("run2015")),
      column(width=1, valueBoxOutput("run2016")),
      column(width=1, valueBoxOutput("run2017"))
    ),
    box(
      title = "# of km", width = 3, background = "light-blue",
      "kpi2"
    ),
    box(
      title = "Average # of runs/week",width = 3, background = "maroon",
      "kpi3"
    ),
    box(
      title = "Average # of km/week",width = 3, background = "yellow",
      "kpi4"
    ),
  fluidRow(
    box(title = "Km per week",plotOutput("plot1")),
    box(title = "KM per month", plotOutput("plot2"))
  ),
  fluidRow(
    box(title = "Type of run, per week",plotOutput("plot3")),
    box(title = "Type of run, per month", plotOutput("plot4"))
  )
)
)
  


# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "My running diary"),
  dashboardSidebar(),
  body
)

# Preview the UI in the console
shinyApp(ui = ui,
         server = function(input, output) { 
  
  output$plot1 <- renderPlot({plot(kmweekplot)})
  output$plot2 <- renderPlot({plot(kmmonthplot)})
  output$plot3 <- renderPlot({plot(typeweekplot)})
  output$plot4 <- renderPlot({plot(typemonthplot)})
  output$run2015 <- renderValueBox({valueBox(km2015, subtitle = "")})
  output$run2016 <- renderValueBox({valueBox(km2016, subtitle = "")})
  output$run2017 <- renderValueBox({valueBox(km2017, subtitle = "")})
  }
  )



  