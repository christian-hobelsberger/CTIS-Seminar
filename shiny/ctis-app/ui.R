library(shiny)
library(shinythemes)
library(shinycssloaders)
library(tmap)
library(tmaptools)
button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"

shinyUI(fluidPage(
    navbarPage("CTIS Mental Health", theme = shinytheme("lumen"),
             navbarMenu("Maps", icon = icon("map"),
               tabPanel("World maps", fluid = TRUE, icon = icon("globe"),
               tags$style(button_color_css),
               sidebarLayout(
                   sidebarPanel(
                       titlePanel("Map characteristics"),
                       selectInput(inputId = "variable",
                                    label = "Select variable",
                                    choices = colnames(data_CTIS_map)[5:9],
                                    selected = "anxious_7d",
                                    width = "220px"
                       ),
                       dateInput(inputId = "date",
                                 label = "Select date",
                                 value = "2021-08-21",
                                 min = min(data_CTIS_map$data.survey_date, na.rm = T),
                                 max = max(data_CTIS_map$data.survey_date, na.rm = T)),
                   ),
                   mainPanel(
                     titlePanel("Global map"),
                     tmapOutput("global_map")
                   )
           )
        ),
        tabPanel("Continent maps", fluid = TRUE, icon = icon("globe-americas"),
                 tags$style(button_color_css),
                 sidebarLayout(
                   sidebarPanel(
                     titlePanel("Map characteristics"),
                     selectInput(inputId = "continent",
                                 label = "Select continent",
                                 choices = unique(data_CTIS_map$continent),
                                 selected = "Europe",
                                 width = "220px"),
                     selectInput(inputId = "cont_variable",
                                 label = "Select variable",
                                 choices = colnames(data_CTIS_map)[5:9],
                                 selected = "anxious_7d",
                                 width = "220px"
                     ),
                     dateInput(inputId = "cont_date",
                               label = "Select date",
                               value = "2021-08-21",
                               min = min(data_CTIS_map$data.survey_date, na.rm = T),
                               max = max(data_CTIS_map$data.survey_date, na.rm = T)),
                   ),
                   mainPanel(
                     titlePanel("Continental map"),
                     tmapOutput("cont_map")
                   )
                 )
        )
             ),
        tabPanel("Continent Analysis", icon = icon("earth-europe"))
    )
    
)
    
)
