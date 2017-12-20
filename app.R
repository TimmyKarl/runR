rm(list=ls())

library(rstudioapi)
library(openxlsx)
library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(scales)
library(plotly)
library(shinythemes)
library(Rcpp)
library(rlang)
library(lazyeval)



  ##LOAD DATA
a <- read.xlsx("data/data.xlsx")
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

##CREATE SUB-DATA PER YEAR
a_2015 <- subset(a, yearvar == 2015) 
a_2016 <- subset(a, yearvar == 2016) 
a_2017 <- subset(a, yearvar == 2017)

##########
##YEARLY##
##########
##CREATE KM PER WEEK PLOT
kmweek <- a %>% group_by(week) %>% summarise(kmweek=sum(km))
kmweekplot <- ggplot(kmweek, aes(as.Date(week), kmweek)) + geom_point() + scale_x_date() +
  ylab("Aggregated by Week") + xlab("Week") + geom_line()

##CREATE KM PER MONTH PLOT
kmmonth <- a %>% group_by(month) %>% summarise(kmmonth=sum(km))
kmmonthplot <- ggplot(kmmonth, aes(as.Date(month), kmmonth)) + geom_point() + scale_x_date() +
  ylab("Aggregated by Month") + xlab("Month") + geom_line()

##KPIs
passtot <- sum(a$no_of_runs, na.rm=TRUE)
kmtot <- sum(a$km, na.rm=TRUE)
passweek <- a %>% group_by(week) %>% summarise(passweek=sum(no_of_runs))
avgpasstot <- round(mean(passweek$passweek),digits = 1)
avgkmtot <- round(mean(kmweek$kmweek),digits = 1)

km2015 <- sum(a_2015$km,  na.rm=TRUE)
km2016 <- sum(a_2016$km,  na.rm=TRUE)
km2017 <- sum(a_2017$km,  na.rm=TRUE)




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


##########
###2015###
##########
kmweekplot2015 <- subset(kmweek,week < "2015-12-31") %>% 
  ggplot(., aes(as.Date(week), kmweek)) + geom_point() + scale_x_date() +
  ylab("Aggregated by Week") + xlab("Week") + geom_line()

kmmonthplot2015 <- subset(kmmonth,month < "2015-12-31") %>% 
  ggplot(., aes(as.Date(month), kmmonth)) + geom_point() + scale_x_date() +
  ylab("Aggregated by Month") + xlab("Month") + geom_line()

typeweekplot2015 <- subset(typeweek, week < "2015-12-31") %>%
  ggplot(., aes( x = week, y = n, fill = value)) + 
  geom_bar(stat="identity", position = "stack") + labs(x = "Week", y = "", fill = "Type of run")

typemonthplot2015 <- subset(typemonth, month < "2015-12-31") %>%
  ggplot(., aes( x = month, y = n, fill = value)) + 
  geom_bar(stat="identity", position = "stack") + labs(x = "Month", y = "", fill = "Type of run")

passtot15 <- sum(a_2015$no_of_runs, na.rm=TRUE)
kmtot15 <- sum(a_2015$km, na.rm=TRUE)
passweek15 <- a_2015 %>% group_by(week) %>% summarise(passweek=sum(no_of_runs))
avgpasstot15 <- round(mean(passweek15$passweek),digits = 1)
avgkmtot15 <- subset(kmweek,week < "2016-01-01")
avgkmtot15 <- round(mean(avgkmtot15$kmweek), digits = 1)


##########
###2016###
##########
kmweekplot2016 <- subset(kmweek,week > "2015-12-31" & week < "2017-01-01") %>% 
  ggplot(., aes(as.Date(week), kmweek)) + geom_point() + scale_x_date() +
  ylab("Aggregated by Week") + xlab("Week") + geom_line()

kmmonthplot2016 <- subset(kmmonth,month > "2015-12-31" & month < "2017-01-01") %>% 
  ggplot(., aes(as.Date(month), kmmonth)) + geom_point() + scale_x_date() +
  ylab("Aggregated by Month") + xlab("Month") + geom_line()

typeweekplot2016 <- subset(typeweek, week > "2015-12-31" & week < "2017-01-01") %>%
  ggplot(., aes( x = week, y = n, fill = value)) + 
  geom_bar(stat="identity", position = "stack") + labs(x = "Week", y = "", fill = "Type of run")

typemonthplot2016 <- subset(typemonth, month > "2015-12-31" & month < "2017-01-01") %>%
  ggplot(., aes( x = month, y = n, fill = value)) + 
  geom_bar(stat="identity", position = "stack") + labs(x = "Month", y = "", fill = "Type of run")

passtot16 <- sum(a_2016$no_of_runs, na.rm=TRUE)
kmtot16 <- sum(a_2016$km, na.rm=TRUE)
passweek16 <- a_2016 %>% group_by(week) %>% summarise(passweek=sum(no_of_runs))
avgpasstot16 <- round(mean(passweek16$passweek),digits = 1)
avgkmtot16 <- subset(kmweek, week > "2015-12-31" & week < "2017-01-01")
avgkmtot16 <- round(mean(avgkmtot16$kmweek), digits = 1)


##########
###2017###
##########
kmweekplot2017 <- subset(kmweek,week > "2016-12-31" & week < "2018-01-01") %>% 
  ggplot(., aes(as.Date(week), kmweek)) + geom_point() + scale_x_date() +
  ylab("Aggregated by Week") + xlab("Week") + geom_line()

kmmonthplot2017 <- subset(kmmonth,month > "2016-12-31" & month < "2018-01-01") %>% 
  ggplot(., aes(as.Date(month), kmmonth)) + geom_point() + scale_x_date() +
  ylab("Aggregated by Month") + xlab("Month") + geom_line()

typeweekplot2017 <- subset(typeweek, week > "2016-12-31" & week < "2018-01-01") %>%
  ggplot(., aes( x = week, y = n, fill = value)) + 
  geom_bar(stat="identity", position = "stack") + labs(x = "Week", y = "", fill = "Type of run")

typemonthplot2017 <- subset(typemonth, month > "2016-12-31" & month < "2018-01-01") %>%
  ggplot(., aes( x = month, y = n, fill = value)) + 
  geom_bar(stat="identity", position = "stack") + labs(x = "Month", y = "", fill = "Type of run")

passtot17 <- sum(a_2017$no_of_runs, na.rm=TRUE)
kmtot17 <- sum(a_2017$km, na.rm=TRUE)
passweek17 <- a_2017 %>% group_by(week) %>% summarise(passweek=sum(no_of_runs))
avgpasstot17 <- round(mean(passweek17$passweek),digits = 1)
avgkmtot17 <- subset(kmweek, week > "2016-12-31" & week < "2018-01-01")
avgkmtot17 <- round(mean(avgkmtot17$kmweek), digits = 1)

###APP BELOW

# BODY
body <- dashboardBody(
  tags$head(tags$style(HTML('.content-wrapper, right-side {
  background-color: #222d32;
                      }
'))),
  tabItems(
    # Total tab
    tabItem(tabName = "total",
  fluidRow(
      valueBoxOutput("runtot", width=3),
      valueBoxOutput("avgruntot", width = 3),
      valueBoxOutput("passtot", width = 3),
      valueBoxOutput("avgpasstot", width = 3)
  ),
        fluidRow(
    box(title = "# of km per week",plotOutput("plot1")),
    box(title = "# of km per month", plotOutput("plot2"))
  ),
  fluidRow(
    box(title = "Type of run, per week",plotOutput("plot3")),
    box(title = "Type of run, per month", plotOutput("plot4"))
  )),
    # 2015 Tab
tabItem(tabName = "fifteen",
fluidRow(
  valueBoxOutput("runtot15", width=3),
  valueBoxOutput("avgruntot15", width = 3),
  valueBoxOutput("passtot15", width = 3),
  valueBoxOutput("avgpasstot15", width = 3)
),
fluidRow(
  box(title = "# of km per week",plotOutput("plot115")),
  box(title = "# of km per month", plotOutput("plot215"))
),
fluidRow(
  box(title = "Type of run, per week",plotOutput("plot315")),
  box(title = "Type of run, per month", plotOutput("plot415"))
)
),
# 2016 Tab
tabItem(tabName = "sixteen",
        fluidRow(
          valueBoxOutput("runtot16", width=3),
          valueBoxOutput("avgruntot16", width = 3),
          valueBoxOutput("passtot16", width = 3),
          valueBoxOutput("avgpasstot16", width = 3)
        ),
        fluidRow(
          box(title = "# of km per week",plotOutput("plot116")),
          box(title = "# of km per month", plotOutput("plot216"))
        ),
        fluidRow(
          box(title = "Type of run, per week",plotOutput("plot316")),
          box(title = "Type of run, per month", plotOutput("plot416"))
        )
),
# 2017 Tab
tabItem(tabName = "seventeen",
        fluidRow(
          valueBoxOutput("runtot17", width=3),
          valueBoxOutput("avgruntot17", width = 3),
          valueBoxOutput("passtot17", width = 3),
          valueBoxOutput("avgpasstot17", width = 3)
        ),
        fluidRow(
          box(title = "# of km per week",plotOutput("plot117")),
          box(title = "# of km per month", plotOutput("plot217"))
        ),
        fluidRow(
          box(title = "Type of run, per week",plotOutput("plot317")),
          box(title = "Type of run, per month", plotOutput("plot417"))
        )
)
)
)
  


# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "My running diary"),
  # Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Total 2015-2017", tabName = "total"),
      menuItem("2015", tabName = "fifteen"),
      menuItem("2016", tabName = "sixteen"),
      menuItem("2017", tabName = "seventeen")
    )
  ),
  body)

# Preview the UI in the console
shinyApp(ui = ui,
         server = function(input, output) { 
  
  output$plot1 <- renderPlot({plot(kmweekplot)})
  output$plot2 <- renderPlot({plot(kmmonthplot)})
  output$plot3 <- renderPlot({plot(typeweekplot)})
  output$plot4 <- renderPlot({plot(typemonthplot)})
  output$plot115 <- renderPlot({plot(kmweekplot2015)})
  output$plot215 <- renderPlot({plot(kmmonthplot2015)})
  output$plot315 <- renderPlot({plot(typeweekplot2015)})
  output$plot415 <- renderPlot({plot(typemonthplot2015)})
  output$plot116 <- renderPlot({plot(kmweekplot2016)})
  output$plot216 <- renderPlot({plot(kmmonthplot2016)})
  output$plot316 <- renderPlot({plot(typeweekplot2016)})
  output$plot416 <- renderPlot({plot(typemonthplot2016)})
  output$plot117 <- renderPlot({plot(kmweekplot2017)})
  output$plot217 <- renderPlot({plot(kmmonthplot2017)})
  output$plot317 <- renderPlot({plot(typeweekplot2017)})
  output$plot417 <- renderPlot({plot(typemonthplot2017)})
  output$runtot <- renderValueBox({valueBox(value = tags$p(kmtot, style = "font-size: 20px;"),subtitle = "# of km", color = "yellow")})
  output$avgruntot <- renderValueBox({valueBox(value = tags$p(avgkmtot, style = "font-size: 20px;"), subtitle = "Average # of km/week")})
  output$passtot <- renderValueBox({valueBox(value = tags$p(passtot, style = "font-size: 20px;"),subtitle = "# of runs", color= "green")})
  output$avgpasstot <- renderValueBox({valueBox(value = tags$p(avgpasstot, style = "font-size: 20px;"),subtitle = "Average # of runs/week", color="red")})
  
  output$runtot15 <- renderValueBox({valueBox(value = tags$p(kmtot15, style = "font-size: 20px;"),subtitle = "# of km", color = "yellow")})
  output$avgruntot15 <- renderValueBox({valueBox(value = tags$p(avgkmtot15, style = "font-size: 20px;"), subtitle = "Average # of km/week")})
  output$passtot15 <- renderValueBox({valueBox(value = tags$p(passtot15, style = "font-size: 20px;"),subtitle = "# of runs", color= "green")})
  output$avgpasstot15 <- renderValueBox({valueBox(value = tags$p(avgpasstot15, style = "font-size: 20px;"),subtitle = "Average # of runs/week", color="red")})
  
  output$runtot16 <- renderValueBox({valueBox(value = tags$p(kmtot16, style = "font-size: 20px;"),subtitle = "# of km", color = "yellow")})
  output$avgruntot16 <- renderValueBox({valueBox(value = tags$p(avgkmtot16, style = "font-size: 20px;"), subtitle = "Average # of km/week")})
  output$passtot16 <- renderValueBox({valueBox(value = tags$p(passtot16, style = "font-size: 20px;"),subtitle = "# of runs", color= "green")})
  output$avgpasstot16 <- renderValueBox({valueBox(value = tags$p(avgpasstot16, style = "font-size: 20px;"),subtitle = "Average # of runs/week", color="red")})
  
  output$runtot17 <- renderValueBox({valueBox(value = tags$p(kmtot17, style = "font-size: 20px;"),subtitle = "# of km", color = "yellow")})
  output$avgruntot17 <- renderValueBox({valueBox(value = tags$p(avgkmtot17, style = "font-size: 20px;"), subtitle = "Average # of km/week")})
  output$passtot17 <- renderValueBox({valueBox(value = tags$p(passtot17, style = "font-size: 20px;"),subtitle = "# of runs", color= "green")})
  output$avgpasstot17 <- renderValueBox({valueBox(value = tags$p(avgpasstot17, style = "font-size: 20px;"),subtitle = "Average # of runs/week", color="red")})
  }
  )

