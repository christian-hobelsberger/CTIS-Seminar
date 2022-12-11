#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tmap)
library(dplyr)
library(tmap)

#data_CTIS_map <- readRDS("shiny/ctis-app/app-data/data_CTIS_map.RDS")
variable_choices <- c("anxious_7d", "finance","depressed_7d", "food_security", "worried_become_ill")
data("World")
world <- World[c("iso_a3", "sovereignt", "geometry")]
colnames(world) <- c("data.iso_code", "data.country", "geometry")

 # Define server logic required to draw a histogram
shinyServer(function(input, output) {
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
})



# switch(input$variable, "anxious_7d" = "Anxiety last 7 days:")
# plot_global_map(data_CTIS_map, variable = "anxious_7d",survey_date = "2021-01-28", mode = "view", id = "data.country", popup.vars = c("Anxiety last 7 days:" = "anxious_7d", "Stay home req. level" = "stay_home_requirements", "School closure level" = "school_closures"))
# mapdata <- reactive({data_CTIS_map %>% 
#     select(continent, input$variable, geometry, data.country, data.iso_code, data.survey_date, school_closures, stay_home_requirements) %>% 
#     filter(data.survey_date %in% as.Date(input$date))
# # If we filter for specific data we lose geometries for countries with no obs at that data therefore   we need to add the geo data + NA for survey data
# missing_countries <- setdiff(world$data.country, mapdata$data.country)
# missing_data <- world %>% filter(data.country %in% missing_countries)
# missing_columns <- setdiff(colnames(mapdata), colnames(missing_data))
# for (column in 1:length(missing_columns)) {
#     missing_data[missing_columns[column]] <- NA
# }
# mapdata <- rbind(mapdata, missing_data)})
# 
# tmap_mode("view")
# temp_map <- reactive({tm_shape(mapdata)+
#     tm_fill(reactive({input$variable}))+
#     tm_borders()})
# 
# observe({
#     global_map <- temp_map()
# })
# 
# global_map <- observe({plot_global_map(data_CTIS_map,
#                               variable = reactive(input$variable),
#                               survey_date = reactive(input$date),
#                               mode = "view", id = "data.country", popup.vars = c(input$variable, "Stay home req. level" = "stay_home_requirements", "School closure level" = "school_closures"))})