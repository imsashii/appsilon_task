dropdown_ui <- function(id) {
  tagList(
    #for vessel type
    selectInput(NS(id,"vessel_type"),label = "Select Vessel Type:",
      choices = str_sort(unique(ship_type_and_name$ship_type_name)),
      selected = "Tanker",
      width = "90%"
    ),
    tags$br(),
  #for vessel name
    uiOutput(NS(id,"select_vessel_output"))
  )
}

dropdown_server <- function(id) {
  moduleServer(id,function(input,output,session) {
    vessels <- reactive(ship_type_and_name$ship_name[ship_type_and_name$ship_type_name 
                                           == input$vessel_type])
    output$select_vessel_output <- renderUI({
      selectInput(
        inputId = "vessel",
        label = "Select Vessel:",
        choices = str_sort(vessels()),
        #selected = "ARK FORWARDER",
        width = "90%")
    })
    return(vessel_type = reactive({input$vessel_type}))
  })
}
  



  
