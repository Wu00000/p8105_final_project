

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


# plot setting

theme_set(theme_minimal() + theme(legend.position = "bottom"))
options(ggplot2.continuous.colour = "viridis",
        ggplot2.continuous.fill = "viridis")
scale_colour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d

# load data
plot =
  read_csv("./data/maindata.csv") %>%
  separate(date_of_death, c("year", "month", "day")) %>%
  filter(year != 2021) %>% 
  mutate(age_bin = as.factor(age_bin),
         gender = as.factor(gender),
         race = as.factor(race))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # click map
  output$distPlot <- renderHighchart({
    data("usgeojson")
    
    pop_stat <-
      readxl::read_excel("./data/nst-est2019-01.xlsx",
                         range = "A9:M60") %>%
      select("state" = "West", "population" = "78347268") %>%
      mutate(state = str_match(state, "[a-zA-Z ]+"))
    
    plotly =
      plot %>%
      filter(year == input$Year) %>%
      group_by(state) %>%
      summarize(count = n()) %>%
      mutate(state = state.name[match(state, state.abb)]) %>%
      drop_na() %>%
      left_join(pop_stat, by = "state") %>%
      mutate(death_rate_per10e6 = round(count / population * 1000000, digits = 2))
    
    ClickFunction <-
      JS(
        "function(event) {Shiny.onInputChange('canvasClicked', event.point.properties.postalcode);}"
      )
    
    highchart() %>%
      hc_add_series_map(
        usgeojson,
        plotly,
        name = "Innocent Death number",
        value = "death_rate_per10e6",
        joinBy = c("woename", "state"),
        dataLabels = list(enabled = TRUE,
                          format = '{point.properties.postalcode}')
      ) %>%
      hc_plotOptions(series = list(
        stacking = FALSE,
        events = list(click = ClickFunction)
      ))
    
  })
  
  observeEvent(input$canvasClicked, {
    # code for trend plot
    output$trendPlot <-
      renderHighchart({
        plot %>%
          filter(year == input$Year,
                 state == input$canvasClicked) %>%
          group_by(month) %>%
          summarize(count = n()) %>%
          hchart("line", hcaes(x = month, y = count))
      })
    
    showModal(modalDialog(
      fluidRow(column(12,
                      h3(
                        paste0(input$Year, " ~ ", input$canvasClicked)
                      ))),
      highchartOutput("trendPlot"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  # interactive leaflet
  
  output$leafplot =
    renderLeaflet({
      plot %>%
        filter(year == input$Year,
               state == input$State) %>%
        mutate(
          lan = as.numeric((str_sub(latitude, end = -2))),
          lon = as.numeric(str_sub(longitude, end = -2)),
          label = str_c(
            "<b>City: ",
            city,
            "<br>Age: ",
            age,
            "<br>Race: ",
            race,
            "<br>Gender: ",
            gender,
            "<br>Cause of death: ",
            highest_force,
            sep = ""
          )
        ) %>%
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(~ lon, ~ lan, popup = ~ label, radius = 1)
    })
  
  # circle plot
  
  
  
  output$circleplot = renderPlot({
    
    plot_circle =
      plot %>%
      rename(age_group = age_bin) %>%
      filter(state == input$State_1) %>%
      group_by(across(input$Group), highest_force) %>%
      summarize(count = n()) %>%
      drop_na() %>%
      arrange(desc(count)) %>% 
      ungroup(input$Group) 
    
    empty_bar <- 3
    
    to_add <-
      data.frame(matrix(
        NA,
        empty_bar * nlevels(plot_circle %>% pull(input$Group)),
        ncol(plot_circle)
      ))
    colnames(to_add) <- colnames(plot_circle)
    
    data = reactive({
      plot_circle
    })
    
    var <- data()[[input$Group]]
    lvl <- levels(as.factor(var)) 
    
    
    if (input$Group == 'race') {
      to_add$race <- rep(lvl, each = empty_bar)
    } else {
      if (input$Group == 'gender') {
        to_add$gender <- rep(lvl, each = empty_bar)
      } else
      {
        to_add$age_group <- rep(lvl, each = empty_bar)
      }
    }
    
                                                 
    
    

    
    plot_circle <- rbind(plot_circle, to_add)
    plot_circle <- plot_circle %>% arrange(!!rlang::sym(input$Group))
    plot_circle$id <- seq(1, nrow(plot_circle))
    
    number_of_bar = nrow(plot_circle)
    
    angle = 90 - 360 * (plot_circle$id - 0.5) / number_of_bar
    
    labeldata <-
      plot_circle %>%
      mutate(
        hjust = ifelse(angle < -90, 1 - 0.15, 0 + 0.15),
        angle = ifelse(angle < -90, angle + 180, angle)
      )
    
    maxcount = log(max(plot_circle$count + 1)) * 100000
    
    plot_circle %>%
      ggplot(aes_string(
        x = "as.factor(id)",
        y = "log(count + 1) * 100000", 
        fill =input$Group)) +
      geom_bar(
        aes_string(
          x = "as.factor(id)",
          y = "log(count + 1) * 100000",
          fill =input$Group),
        stat = "identity",
        alpha = 0.7
      ) +
      ylim(-1000000, maxcount) +
      theme_minimal() +
      theme(
        legend.position = c(0.8, 0.05),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1, 4), "cm")
      ) +
      coord_polar() +
      geom_text(
        data = labeldata,
        aes(
          x = as.factor(id),
          y = count,
          label = highest_force,
          hjust =  hjust
        ),
        color = "black",
        fontface = "bold",
        alpha = 0.6,
        size = 3,
        angle = angle ,
        inherit.aes = FALSE
      )
    
    

  })
  
  output$Circle <- DT::renderDataTable({
    circle =
      plot %>%
      rename(age_group = age_bin) %>%
      filter(state == input$State_1) %>%
      group_by(across(input$Group), highest_force) %>%
      summarize(number = n()) %>%
      drop_na() %>%
      arrange(desc(number))
    
    circle
    })
})