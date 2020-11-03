# Check S2/S3 Item Browser (Test version)

# LIBRARIES & OPTIONS ####
library(shiny)
library(shinythemes)
library(tidyverse)
library(Cairo)
library(tibbletime)
library(lubridate)

options(shiny.maxRequestSize=100*1024^2)
options(stringsAsFactors = FALSE)
options(shiny.usecairo = TRUE)

# LOAD DATA FRAMES ####
rm <- rollify(mean, window = 7)

data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                 na.strings = "", 
                 fileEncoding = "UTF-8-BOM") %>%
  rename(date = dateRep, cnt = countriesAndTerritories, pop = popData2019) %>%
  mutate(date = lubridate::dmy(date),
         relcases = cases/pop*100000) %>%
  arrange(cnt, date) %>%
  group_by(cnt) %>%
  mutate(relcases_7 = rm(relcases),
         cumcases = cumsum(cases),
         cumdeaths = cumsum(deaths),
         mortality = cumdeaths/cumcases)

contpop <- data %>%
  select(cnt, con = continentExp, pop) %>%
  distinct() %>%
  group_by(con) %>%
  summarise(pop = sum(pop)) %>%
  filter(!is.na(pop))

cont <- data %>%
  rename(con = continentExp) %>%
  filter(con != "Other",
         date >= "2020-03-01") %>%
  group_by(date, con) %>%
  summarise(cases = sum(cases),
            deaths = sum(deaths)) %>%
  left_join(contpop, by = "con") %>%
  arrange(con, date) %>%
  mutate(relcases = cases/pop*1000000,
         reldeaths = deaths/pop*1000000) %>%
  group_by(con) %>%
  mutate(cases_7 = rm(cases),
         relcases_7 = rm(relcases),
         deaths_7 = rm(deaths),
         reldeaths_7 = rm(reldeaths),
         cumcases = cumsum(cases),
         cumdeaths = cumsum(deaths),
         mortality = cumdeaths/cumcases)

# COLORS #####################################################################################################

pink <- c('#91008b', '#bd00b4', '#e624da', '#fc5dfa', '#ff99ff')
cyan <- c('#00363b', '#006367', '#009496', '#00c8c9', '#00ffff')
purple <- c('#550058', '#900a8f', '#bc45b9', '#ea71e6', '#ffb0ff')
yellow <- c('#eb7c00', '#f29900', '#f7b400', '#fccf00', '#ffea00')
blue <- c('#002f7f', '#065cb5', '#5687e5', '#8fb8ff', '#c8edff')
backg <- c('#2a2139', '#3d334c', '#504560', '#645975', '#796d8a', '#8f82a0', '#a598b6', '#bbaece', '#d2c5e5', '#eadcfd') # background colors are in bootstrap.css

colors.bars <- c(
  cyan[2:3], # HOMELANG
  purple[2:3], # IMMIG
  blue[1:4]  # ESCS
)

names(colors.bars) <- c("Africa", "America", "Asia", "Europe", "Oceania")
colscale.bars <- scale_fill_manual(name = "value", values = colors.bars)

# .####
# UI PORTION #################################################################################################

ui <- shinyUI(
  
  fluidPage(
    
  # PLOT OUTPUT ############################################################################################
    
    plotOutput("plot_1",
               height = "250px",
               width = "500px"
    )
  )
)
  
#.####
# SERVER PORTION ############################################################################################

server <- shinyServer(function(input, output, session)
  {
  
  # SERV: RENDER PLOT ##############################################################################
    output$plot_1 <- renderPlot({
      
      ggplot(cont, aes(x = date, y = relcases_7, color = con)) +
        geom_line(size = 1.5, alpha = 0.75) +
        scale_x_date(date_labels = '%m.%y', date_minor_breaks = "1 month") +
        theme(
          text = element_text(color = "#EE72F1"),
          axis.text = element_text(color = "#EE72F1", size = 12),
          # axis.title = element_blank(),
          # axis.line = element_blank(),
          plot.background = element_rect(fill = "#21202C", color = NA),
          panel.background = element_rect(fill = "#21202C"),
          panel.grid.major = element_line(color = '#353441', size = 0.05),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
        )
      
      })
    
  })
    
    
     
   
shinyApp(ui, server)





















