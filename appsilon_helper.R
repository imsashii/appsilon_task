# ---------- LIBRARIES ---------#
# Installation & Loading of packages
if (!require("tidyverse")){
  install.packages("tidyverse",dependencies = T)
  library(tidyverse)
}

if (!require("shiny")){
  install.packages("shiny",dependencies = T)
  library(shiny)
}

if (!require("shiny.semantic")){
  install.packages("shiny.semantic",dependencies = T)
  library(shiny.semantic)
}

if (!require("shinyjs")){
  install.packages("shinyjs",dependencies = T)
  library(shinyjs)
}

if (!require("data.table")){
  install.packages("data.table",dependencies = T)
  library(data.table)
}

if (!require("leaflet")){
  install.packages("leaflet",dependencies = T)
  library(leaflet)
}

# ---------- LOADING DATASET ---------#

#loaded from the /data folder
map_data <- fread("data/map_data.csv")
map_data$ship_name<-ifelse(map_data$ship_name %in% c("SAT AIS","[SAT-AIS]"),
                           map_data$super_unique_id,map_data$ship_name)

#making a list of unique vessel type and vessel names for dropdowns
ship_type_and_name <- map_data %>% select(super_unique_id,ship_type_name,ship_name) %>% distinct()

#to capitalize the first letter of a word
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

#create a function with text to explain app info
explain_all_func <- function() {
  list(
  p("Distancia displays the maximum distance between two observations for 
    the selected vessel type and vessel."),
  p("The first box displays the total distance travelled by the selected vessel in meters."),
  p("The second box displays the corresponding date and time of the End point  on 
    the map (Please place your pointer on the ship icons to view the points.)"),
  p("The third box displays the port reported by the vessel at the End point ."),
  div(class = "ui horizontal divider", icon("ship")),
  p("Method to find the maximum distance:"),
  p("Since the data contained latitude and longitude information,the haversine formula 
    was used to calculate the distance between two consecutive lat-long pairs.
    The haversine formula determines the great-circle distance between two points 
    on a sphere given their longitudes and latitudes."),
  p("Once the distance was calculated,the sum of the distance gives the total distance
    travelled by the vessel.The date and time corresponds to the End point .")
  )
}
