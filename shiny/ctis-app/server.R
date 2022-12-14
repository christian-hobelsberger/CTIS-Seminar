#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# install.packages(c("timetk", "shinythemes", "plotly", "shinycssloaders"))
library(shiny)
library(tmap)
library(dplyr)
library(tmap)
library(plotly)


variable_choices <- c("anxious_7d", "finance","depressed_7d", "food_security", "worried_become_ill")
data("World")
world <- World[c("iso_a3", "sovereignt", "geometry")]
colnames(world) <- c("data.iso_code", "data.country", "geometry")
data_CTIS_map <- readRDS("app-data/data_CTIS_map.RDS")
data_CTIS_policy <- readRDS("app-data/data_CTIS_policy.RDS")
table_D1_E4_WO_NA <- readRDS("app-data/protected_data/table_D1_E4_WO_NA.RDS")
table_D1_E8_WO_NA <- readRDS("app-data/protected_data/table_D1_E8_WO_NA.RDS")
table_D1_E3_WO_NA <- readRDS("app-data/protected_data/table_D1_E3_WO_NA.RDS")
table_D1_D7a_WO_NA <- readRDS("app-data/protected_data/table_D1_D7a_WO_NA.RDS")
 # Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Maps tab -----
  ## Global ----
    create_mapdata <- reactive({
      mapdata <- data_CTIS_map %>% 
        select(continent, variable_choices, geometry, data.country, data.iso_code, data.survey_date, school_closures, stay_home_requirements) %>% 
        filter(data.survey_date %in% as.Date(input$date))
      missing_countries <- setdiff(world$data.country, mapdata$data.country)
      missing_data <- world %>% filter(data.country %in% missing_countries)
      missing_columns <- setdiff(colnames(mapdata), colnames(missing_data))
      for (column in 1:length(missing_columns)) {
        missing_data[missing_columns[column]] <- NA
      }
      mapdata <- rbind(mapdata, missing_data)
    })

    fill <- reactive({
      tm_fill(input$variable, id = "data.country", 
                              popup.vars = c(input$variable, "Stay home req. level" = "stay_home_requirements", "School closure level" = "school_closures"))})
    output$global_map <- renderTmap(tm_shape(create_mapdata())+
                                        fill()+
                                        tm_borders())
    ## Continent ----
    create_cont_mapdata <- reactive({
      cont_mapdata <- data_CTIS_map %>% 
        select(continent, variable_choices, geometry, data.country, data.iso_code, data.survey_date, school_closures, stay_home_requirements) %>% 
        filter(continent %in% input$continent) %>% 
        filter(data.survey_date %in% as.Date(input$cont_date))
    })
    fill_cont <- reactive({
      tm_fill(input$cont_variable, id = "data.country", 
              popup.vars = c(input$cont_variable, "Stay home req. level" = "stay_home_requirements", "School closure level" = "school_closures"))})
    
    output$cont_map <- renderTmap(tm_shape(create_cont_mapdata())+
                                    fill_cont()+
                                    tm_borders())
    
    # Global Analysis tab ---- 
    # create_D1_E4_WO_NA <- reactive({
    #   readRDS("app-data/protected_data/table_D1_E4_WO_NA.RDS")
    # })
    output$global_micro_ana_bar <- renderPlotly({
      ggplotly(ggplot(table_D1_E4_WO_NA, aes(x = D1, y = perc)) + 
                 geom_bar(mapping = aes(fill = E4), position = "dodge", stat = "identity") +
                 labs(title = "D1 (nervous) vs E4 (age group)", 
                      y = "relative frequencies",
                      x = "D1 (nervous)",) + 
                 scale_fill_brewer(palette = "Blues", name = "E4 (age group)"))})
    
    
    # Continent Analysis tab----
    create_continent_policy <- reactive({
      data_CTIS_policy %>% 
        filter(continent %in% input$cont_ana_continent) %>% 
        group_by(continent, data.survey_date) %>% 
        summarise(continent_mean_var = mean(get(input$cont_ana_variable), na.rm = TRUE))
    })
      output$cont_ana_line <- renderPlotly({
        ggplotly(create_continent_policy() %>%
                   ggplot(aes(x = data.survey_date, y = continent_mean_var))+
                   facet_wrap(vars(continent))+
                   geom_line(size = 0.1))})
  
      
      
      # Country Analysis tab ----
      create_country_policy <- reactive({
        data_CTIS_policy %>% 
          filter(data.country %in% input$country_ana_country) %>% 
          group_by(data.country, data.survey_date) %>% 
          summarise(country_mean_var = mean(get(input$country_ana_variable), na.rm = TRUE))
      })
      
      output$country_ana_line <- renderPlotly({ggplotly(create_country_policy() %>% 
                                           ggplot(aes(x = data.survey_date, y = country_mean_var))+
                                           facet_wrap(vars(input$country_ana_country))+
                                           geom_line(size = 0.1))})
    
    
})
