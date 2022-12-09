plot_global_map <- function(data, variable, survey_date, mode = "plot", ...){
  mapdata <- data %>% 
    select(continent, variable, geometry, data.country, data.iso_code, data.survey_date, school_closures, stay_home_requirements) %>% 
    filter(data.survey_date %in% as.Date(reactive(survey_date)))
  # If we filter for specific data we lose geometries for countries with no obs at that data therefore   we need to add the geo data + NA for survey data
  missing_countries <- setdiff(world$data.country, mapdata$data.country)
  missing_data <- world %>% filter(data.country %in% missing_countries)
  missing_columns <- setdiff(colnames(mapdata), colnames(missing_data))
  for (column in 1:length(missing_columns)) {
    missing_data[missing_columns[column]] <- NA
  }
  mapdata <- rbind(mapdata, missing_data)
  tmap_mode(mode)
  tm_shape(mapdata)+
    #tm_fill(variable, ...)+
    tm_borders()
}

plot_continent_map <- function(data, variable, area, survey_date, mode = "plot", ...){
  mapdata <- data %>% 
    select(continent, variable, geometry, data.country, data.iso_code, data.survey_date, school_closures, stay_home_requirements) %>% 
    filter(data.survey_date %in% as.Date(survey_date)) %>% 
    filter(continent %in% area)
  
  tmap_mode(mode)
  tm_shape(mapdata)+
    tm_borders()+
    tm_polygons(variable, ...)
}