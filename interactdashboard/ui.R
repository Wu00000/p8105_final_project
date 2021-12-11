
library(shiny)
library(highcharter)
library(ggplot2)
library(tidyverse)
library(shinydashboard)
library(viridis)
library(plotly)
library(leaflet)
library(treemap)
library(viridisLite)
library(forecast)
library(readxl)
library(rsconnect)



plot =
  read_csv("./data/maindata.csv") %>%
  separate(date_of_death, c("year", "month", "day")) %>%
  filter(year != 2021)

Year_choices =
  plot %>%
  mutate(year = as.integer(year)) %>% 
  distinct(year) %>%
  arrange(year) %>% 
  pull()
  
State_choices =
  plot %>%
  distinct(state) %>%
  pull()

Group_choices =
  c("age_group", "gender", "race")

dashboardPage(
  skin = "black",
  dashboardHeader(title = "Innocent death"),
  dashboardSidebar(sidebarMenu(menuItem(
    selectInput('Year',
                'Select year',
                choices = Year_choices,
                selected = "2020")
  ))),
  dashboardBody(
    fluidRow(
      tabBox(id = "tab", width = 12, 
             tabPanel("State interactive map",
                       fluidRow(
                         column(12, highchartOutput("distPlot", height = 800)))
                    ),
             tabPanel("US interactive map",
                      fluidRow(
                        box(width = 5, 
                            selectInput('State',
                                        'Select state',
                                        choices = State_choices,
                                        selected = "NY"))
                                              
                            ),
                      fluidRow(
                        column(12, leafletOutput("leafplot", height = 800)), 
                        )
                      ),
             tabPanel("TOP death reason",
                      fluidRow(
                        box(width = 12, 
                            selectInput('State_1',
                                        'Select state',
                                        choices = State_choices,
                                        selected = "NY"),
                            selectInput('Group',
                                        'Group by',
                                        choices = Group_choices,
                                        selected = "race")
                            )
                        
                      ),
                      fluidRow(
                        column(7, imageOutput("circleplot", height = 800)), 
                        column(5, 
                               box(title = "In selected state, the top death reason are:",
                                   solidHeader = T,
                                   width = 12,
                                   collapsible = T,
                                   div(DT::DTOutput("Circle"), style = "font-size: 80%;")))
                      )
                      
             )
                      
             )
             ))
)
  

