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
table_D1_E4_Date <- readRDS("app-data/protected_data/table_D1_E4_Date.RDS")
table_D1_E8_Date <- readRDS("app-data/protected_data/table_D1_E8_Date.RDS")
table_D1_E3_Date <- readRDS("app-data/protected_data/table_D1_E3_Date.RDS")
table_D1_D7a_Date <- readRDS("app-data/protected_data/table_D1_D7a_Date.RDS")
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
    create_bar_data <- reactive({
       switch (input$global_ana_variable,
         "E4" = return(table_D1_E4_WO_NA),
         "E8" = return(table_D1_E8_WO_NA),
         "E3" = return(table_D1_E3_WO_NA),
         "D7a" = return(table_D1_D7a_WO_NA)
       )
    })
    bar_fill <- reactive({
      switch (input$global_ana_variable,
              "E4" = return(table_D1_E4_WO_NA[["E4"]]),
              "E8" = return(table_D1_E8_WO_NA[["E8"]]),
              "E3" = return(table_D1_E3_WO_NA[["E3"]]),
              "D7a" = return(table_D1_D7a_WO_NA[["D7a"]])
      )
    })
    bar_name <- reactive({
      switch (input$global_ana_variable,
              "E4" = return("E4"),
              "E8" = return("E8"),
              "E3" = return("E3"),
              "D7a" = return("D7a")
      )
    })
    create_line_data <- reactive({
      switch (input$global_ana_variable,
              "E4" = return(table_D1_E4_Date),
              "E8" = return(table_D1_E8_Date),
              "E3" = return(table_D1_E3_Date),
              "D7a" = return(table_D1_D7a_Date)
      )
    })
    facet_wrap_var <- reactive({
      switch (input$global_ana_variable,
              "E4" = return("E4"),
              "E8" = return("E8"),
              "E3" = return("E3"),
              "D7a" = return("D7a")
      )
    })
    
    pick_plot <- reactive({
      if(!input$global_ana_checkbox) {
        if (input$global_ana_variable == "E4") {
          return(
            ggplotly(ggplot(data = table_D1_E4_Date, aes(x = D1, y = perc)) + 
                       geom_bar(mapping = aes(fill = E4), position = "dodge", stat = "identity") +
                       labs(title = "D1 (nervous) vs E4 (age group)", 
                            y = "relative frequencies",
                            x = "D1 (nervous)",) + 
                       scale_fill_brewer(palette = "Blues", name = "")+
                       theme(legend.position = "bottom"))%>%
              layout(legend = list(orientation = "h", x = 0.4, y = -0.2), width = "800px", heigth = "500px")
          )
        } else if (input$global_ana_variable == "E8") {
          return(
            ggplotly(ggplot(data = table_D1_E8_Date, aes(x = D1, y = perc)) + 
                       geom_bar(mapping = aes(fill = E8), position = "dodge", stat = "identity") +
                       labs(title = "D1 (nervous) vs E8 (highest education-level)", 
                            y = "relative frequencies",
                            x = "D1 (nervous)",) + 
                       scale_fill_brewer(palette = "Blues", name = "")+
                       theme(legend.position = "bottom"))%>%
              layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
          )
        } else if (input$global_ana_variable == "E3") {
          return(
            ggplotly(ggplot(data = table_D1_E3_Date, aes(x = D1, y = perc)) + 
                       geom_bar(mapping = aes(fill = E3), position = "dodge", stat = "identity") +
                       labs(title = "D1 (nervous) vs E3 (gender)", 
                            y = "relative frequencies",
                            x = "D1 (nervous)",) + 
                       scale_fill_brewer(palette = "Blues", name = "")+
                       theme(legend.position = "bottom"))%>%
              layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
          )
        } else {
          return(
            ggplotly(ggplot(data = table_D1_D7a_Date, aes(x = D1, y = perc)) + 
                       geom_bar(mapping = aes(fill = D7a), position = "dodge", stat = "identity") +
                       labs(title = "D1 (nervous) vs D7a (work last 4 weeks)", 
                            y = "relative frequencies",
                            x = "D1 (nervous)",) + 
                       scale_fill_brewer(palette = "Blues", name = "")+
                       theme(legend.position = "bottom"))%>%
              layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
          )
        }
        # return(
        #   ggplotly(ggplot(data = create_bar_data(), aes(x = D1, y = perc)) + 
        #                            geom_bar(mapping = aes(fill = bar_fill()), position = "dodge", stat = "identity") +
        #                            labs(y = "relative frequencies",
        #                                 x = "D1 (nervous)",) + 
        #                            scale_fill_brewer(palette = "Blues", name = "")+
        #                            theme(legend.position = "bottom"))%>%
        #       layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
        # )
      } else {
        return(
          ggplot(data = create_line_data(), mapping = aes(x = RecordedDate, y = perc)) +
              geom_line(mapping = aes(color = D1)) +
              facet_wrap(facets = facet_wrap_var()) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              labs(y = "relative frequencies",
                   color = "D1 (nervous)")
        )
      }
    })
    
    output$global_micro_ana_bar <- renderPlotly({
      return(pick_plot())
    })
    
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
