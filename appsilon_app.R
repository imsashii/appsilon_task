##### SOURCING THE HELPER SCRIPT & SHINY MODULE SCRIPT #####
source("appsilon_helper.R")
source("dropdown_shiny_module.R")

##### GRID LAYOUT #####
overall_Grid <- grid_template(default = list(
  areas = rbind(
    c("selections","info1","info2","info3"),
    c("selections","map","map","table")
  ),
  rows_height = c("125px","700px"),
  cols_width = c("20%","26.6%","26.6%","26.6%")
))

##### UI #####
ui <- semanticPage(
      includeCSS('appsilon.css'),
      title = "Appsilon | Task",
      grid(overall_Grid,
           container_style = "background: whitesmoke;",
           area_styles = list(selections = "background: #232E32",
                              info1 = "margin: 5px;background: #0EBFED",
                              info2 = "margin: 5px;background: #0074C1",
                              info3 = "margin: 5px;background: #36CCD4",
                              map = "margin: 8px;background: whitesmoke",
                              table = "margin: 5px;background: white"),
           selections = div(
             tags$br(),
             h1("distancia", style = "text-align: center;color: whitesmoke"),
             tags$br(),
             tags$hr(),
             tags$br(),
             #shiny module ui
             dropdown_ui("shiny_mod")
           ),
           info1 = card(div(class = "content", div(class = "header", 
                                                   textOutput("dist_travelled")), 
                            div(class = "description", "Total distance travelled (meters)"))),
           info2 = card(div(class = "content", div(class = "header", 
                                                   textOutput("date_time")), 
                            div(class = "description", "Observation of longest distance (as shown in map)"))),
           info3 = card(div(class = "content", div(class = "header", 
                                                   textOutput("port_report")), 
                            div(class = "description", "Port reported at time of observation"))),
           map = leafletOutput("distmap",height = "100%"),
           table = div(class = "intro",
                       tags$br(),
                       tags$b(textOutput("max_dist")),
                       tags$br(),
                       explain_all_func())
           ) #grid end bracket
     ) #page end bracket


##### SERVER #####
server <- function(input,output,session){
    #shiny module server 
    vesselType<-dropdown_server("shiny_mod")
      
    observeEvent(input$vessel, {
      #creating a reactive element to be used in other places
      one_ship <- reactive({
        #creating a subset for selected ship
        one_ship<-map_data %>% filter(ship_name == input$vessel,ship_type_name == vesselType()) %>% arrange(datetime) 
      })
      
      output$dist_travelled <- renderText({
        req(one_ship())
        ship <- one_ship()
        tot <- prettyNum(unique(ship$tot),big.mark=",",scientific=FALSE)
        return(tot)
      })
      
      output$date_time <- renderText({
        req(one_ship())
        ship <- one_ship()
        dt <- as.character(format(max(ship$datetime,na.rm = TRUE),'%d %b %Y %H:%M:%S'))
        return(dt)
      })
      
      output$port_report <- renderText({
        req(one_ship())
        ship <- one_ship()
        return(.simpleCap(ship$port_reported[which.max(ship$haversine_dist)]))
      })
      
      output$distmap <- renderLeaflet({
        req(one_ship())
        ship <- one_ship()
        
        #map icon
        icons <- awesomeIcons(
          icon = 'ship',
          iconColor = 'black',
          library = 'fa',
          markerColor = "white"
        )
        
        #render map
        map <- leaflet(data = ship,options = leafletOptions(minZoom = 0, maxZoom = 11)) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addAwesomeMarkers(~lon,~lat,icon = icons,label = c("Beginning","End")) %>%
          addPolygons(lng = ~lon, lat = ~lat,
                      fill = F, weight = 4, color = "#000000", group = "Outline")
        return(map)
      })
    
      output$max_dist <- renderText({
        req(one_ship())
        ship <- one_ship()
        max_dist <- prettyNum(round(max(ship$haversine_dist,na.rm = TRUE),2),big.mark = ",",scientific=FALSE)
        paste0("Maximum Distance for ",input$vessel," is ",max_dist," meters.")
      })
      
    }) #observe event end bracket
} #server end bracket
    
shinyApp(ui,server)

