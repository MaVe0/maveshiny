# Check S2/S3 Item Browser (Test version)

# LIBRARIES & OPTIONS ####
library(shiny)
library(shinythemes)
library(tidyverse)
library(Cairo)
library(tibbletime)
library(lubridate)
library(DT)

options(shiny.maxRequestSize=100*1024^2)
options(stringsAsFactors = FALSE)
options(shiny.usecairo = TRUE)
options(scipen = 999)

# LOAD DATA FRAMES ####
rm <- rollify(mean, window = 7)

cnt.data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                     na.strings = "", 
                     fileEncoding = "UTF-8-BOM")

deaths <- cnt.data %>%
  select(date = dateRep, con = continentExp, deaths, cnt = countriesAndTerritories) %>%
  mutate(date = lubridate::dmy(date),
         cnt = str_replace_all(cnt, "_", " ")) %>%
  arrange(con, date)

tot.deaths.cnt <- deaths %>%
  group_by(cnt) %>%
  summarise(totdeaths = sum(deaths))

deaths.cnt <- deaths %>%
  group_by(con, date) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)) %>%
  mutate(cumdeaths = cumsum(deaths)) %>%
  filter(con != "Other")
  
cnt.data <- cnt.data %>%
  rename(date = dateRep, cnt = countriesAndTerritories, pop = popData2019) %>%
  mutate(date = lubridate::dmy(date),
         relcases = cases/pop*100000) %>%
  arrange(cnt, date) %>%
  add_count(cnt, name = "n") %>%
  filter(n >= 7,
         date >= "2020-03-01",
         !(cnt %in% c("Cases_on_an_international_conveyance_Japan", "Holy_See"))) %>%
  group_by(cnt) %>%
  mutate(cnt = str_replace_all(cnt, "_", " "),
         relcases_7 = rm(relcases),
         cumcases = cumsum(cases),
         cumdeaths = cumsum(deaths),
         mortality = cumdeaths/cumcases)

contpop <- cnt.data %>%
  select(cnt, con = continentExp, pop) %>%
  distinct() %>%
  group_by(con) %>%
  summarise(pop = sum(pop)) %>%
  filter(!is.na(pop))

cont.data <- cnt.data %>%
  rename(con = continentExp) %>%
  filter(con != "Other",
         date >= "2020-03-01") %>%
  group_by(date, con) %>%
  summarise(cases = sum(cases),
            deaths = sum(deaths)) %>%
  left_join(contpop, by = "con") %>%
  arrange(con, date) %>%
  mutate(relcases = cases/pop*100000,
         reldeaths = deaths/pop*100000) %>%
  group_by(con) %>%
  mutate(cases_7 = rm(cases),
         relcases_7 = rm(relcases),
         deaths_7 = rm(deaths),
         reldeaths_7 = rm(reldeaths),
         cumcases = cumsum(cases),
         cumdeaths = cumsum(deaths),
         mortality = cumdeaths/cumcases)

cnt.table <- cnt.data %>%
  select(date, cases, pop) %>%
  group_by(cnt) %>%
  summarise(totcases = sum(cases, na.rm = TRUE),
            pop = first(pop)) %>%
  left_join(tot.deaths.cnt, by = 'cnt') %>%
  mutate(reldeaths = totdeaths/pop*100000,
         relcases = totcases/pop*100000,
         cfr = totdeaths/totcases)

# COLORS #####################################################################################################

pink <- c('#91008b', '#bd00b4', '#e624da', '#fc5dfa', '#ff99ff')
cyan <- c('#00363b', '#006367', '#009496', '#00c8c9', '#00ffff')
purple <- c('#550058', '#900a8f', '#bc45b9', '#ea71e6', '#ffb0ff')
yellow <- c('#eb7c00', '#f29900', '#f7b400', '#fccf00', '#ffea00')
blue <- c('#002f7f', '#065cb5', '#5687e5', '#8fb8ff', '#c8edff')
backg <- c('#2a2139', '#3d334c', '#504560', '#645975', '#796d8a', '#8f82a0', '#a598b6', '#bbaece', '#d2c5e5', '#eadcfd') # background colors are in bootstrap.css

colors.lines <- c(
  "#AAFF00", # Africa
  "#FFAA00", # America
  "#FF00AA",  # Asia
  "#AA00FF", # Europe
  "#00AAFF" # Oceania
)

names(colors.lines) <- c("Africa", "America", "Asia", "Europe", "Oceania")
colscale.lines <- scale_color_manual(name = "value", values = colors.lines)
colscale.areas <- scale_fill_manual(name = "value", values = colors.lines)

# .####
# UI PORTION ####

ui <- shinyUI(
  
  fluidPage(
    
    theme = "bootstrap.css",
    
    tags$head(
      tags$style(type='text/css', 
                 ".nav-tabs {font-size: 10px} ")),
    
    tabsetPanel(
      
      # TAB 1 ####
      
      tabPanel("Infections by country",
               
               # Country Input ####
               
               br(),
               
               fluidRow(
                 
                 align = "center",
                 
                 selectInput("i.country", 
                             "search and add countries", 
                             choices = cnt.data %>% 
                               arrange(cnt) %>% 
                               select(cnt) %>% 
                               unique() %>% 
                               pull(), 
                             selected = c("Switzerland", "Czechia"), 
                             multiple = TRUE)
               ),
               
               # Plot Output 1 ####
               
               plotOutput("plot.1",
                          # height = "600px",
                          # width = "800px"
                          )
      ),
      
      #.####
      # TAB 2 ####
      
      tabPanel("Infections by continent",
               
               br(),
               
               # Plot Output 2 ####
               
               plotOutput("plot.2")
               ),
      
      tabPanel("CFR by country",
               
               # Country Input 2 ####
               
               br(),
               
               fluidRow(
                 
                 align = "center",
                 
                 selectInput("i.country2", 
                             "search and add countries", 
                             choices = cnt.data %>% 
                               arrange(cnt) %>% 
                               select(cnt) %>% 
                               unique() %>% 
                               pull(), 
                             selected = c("Switzerland", "Czechia"), 
                             multiple = TRUE)
               ),
               
               # Plot Output 3 ####
               
               plotOutput("plot.3",
                          # height = "600px",
                          # width = "800px"
               )
      ),
      
      #.####
      # TAB 4 ####
      
      tabPanel("Deaths by continent",
               
               br(),
               
               # Plot Output 2 ####
               
               plotOutput("plot.4")
               ),
      
      # TAB 4 ####
      
      tabPanel("Country Table",
               
               br(),
               
               tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                  background-color: #9c4242 !important;
                                  }
                                  "))),
               tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
            color: #EE72F1 !important;
        }")),
               
               # Plot Output 2 ####
               
               DT::dataTableOutput("table.1")
      )
      )
    )
  )
  

  
#.####
# SERVER PORTION ############################################################################################

server <- shinyServer(function(input, output, session)
  {
  
  # SERV: RENDER PLOT ##############################################################################
  output$plot.1 <- renderPlot({
    cnt.data %>%
      filter(cnt %in% input$i.country) %>%
      ggplot(aes(x = date, y = relcases_7, color = cnt)) +
      geom_line(size = 1, alpha = 0.75, show.legend = TRUE) +
      scale_x_date(name = NULL, date_labels = '%d-%m-%Y', date_breaks = "1 month") +
      scale_y_continuous(name = "Confirmed infections per 100'000 inhabitants\n") +
      theme(
        text = element_text(color = "#EE72F1", size = 14),
        axis.text = element_text(color = "#EE72F1", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "#21202C", color = NA),
        panel.background = element_rect(fill = "#21202C"),
        panel.grid.major = element_line(color = '#61606e', size = 0.05),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = 'top',
        legend.background = element_rect(fill = "#21202C", color = NA),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "#21202C", color = NA),
        legend.text = element_text(size = 12)
      )
  })  
  
  
  output$plot.2 <- renderPlot({
      ggplot(cont.data, aes(x = date, y = relcases_7, color = con)) +
        geom_line(size = 1, alpha = 0.75, show.legend = TRUE) +
        colscale.lines +
        scale_x_date(name = NULL, date_labels = '%d-%m-%Y', date_breaks = "1 month") +
        scale_y_continuous(name = "Confirmed infections per 100'000 inhabitants\n") +
        theme(
          text = element_text(color = "#EE72F1", size = 14),
          axis.text = element_text(color = "#EE72F1", size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "#21202C", color = NA),
          panel.background = element_rect(fill = "#21202C"),
          panel.grid.major = element_line(color = '#61606e', size = 0.05),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.position = 'top',
          legend.background = element_rect(fill = "#21202C", color = NA),
          legend.title = element_blank(),
          legend.key = element_rect(fill = "#21202C", color = NA),
          legend.text = element_text(size = 12)
        )
      })
  
  output$plot.3 <- renderPlot({
    cnt.data %>%
      filter(cnt %in% input$i.country2) %>%
      ggplot(aes(x = date, y = mortality*100, color = cnt)) +
      geom_line(size = 1, alpha = 0.75, show.legend = TRUE) +
      scale_x_date(name = NULL, date_labels = '%d-%m-%Y', date_breaks = "1 month") +
      scale_y_continuous(name = "Case fatality ratio in %\n") +
      theme(
        text = element_text(color = "#EE72F1", size = 14),
        axis.text = element_text(color = "#EE72F1", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "#21202C", color = NA),
        panel.background = element_rect(fill = "#21202C"),
        panel.grid.major = element_line(color = '#61606e', size = 0.05),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = 'top',
        legend.background = element_rect(fill = "#21202C", color = NA),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "#21202C", color = NA),
        legend.text = element_text(size = 12)
      )
  })  
  
  output$plot.4 <- renderPlot({
    ggplot(deaths.cnt %>% filter(date >= "2020-03-01"), aes(x = date, y = cumdeaths/1000000, fill = con)) +
      geom_area() +
      colscale.areas +
      scale_x_date(name = NULL, date_labels = '%d-%m-%Y', date_breaks = "1 month") +
      scale_y_continuous(name = "Total confirmed deaths in millions\n", labels = function(x) format(x, scientific = FALSE)) +
      theme(
        text = element_text(color = "#EE72F1", size = 14),
        axis.text = element_text(color = "#EE72F1", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "#21202C", color = NA),
        panel.background = element_rect(fill = "#21202C"),
        panel.grid.major = element_line(color = '#61606e', size = 0.05),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = 'top',
        legend.background = element_rect(fill = "#21202C", color = NA),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "#21202C", color = NA),
        legend.text = element_text(size = 12)
      )
  })
  
  output$table.1 <- DT::renderDataTable({
    
    datatable(
      cnt.table,
      rownames = FALSE,
      options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'color': '#EE72F1'});",
          "}"))
    ) %>%
      formatStyle(names(cnt.table),
                  backgroundColor = "#8D8C93")
  })
    
  })
    
shinyApp(ui, server)





















