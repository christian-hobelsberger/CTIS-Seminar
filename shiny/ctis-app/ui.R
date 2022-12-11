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
                     tmapOutput("global_map")
                   )
           )
        )
    )
)
    
)
