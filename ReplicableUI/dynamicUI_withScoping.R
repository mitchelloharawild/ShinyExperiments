library(tidyverse)
# UI part of the module
ab_moduleUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      id = ns("self"),
      br(),
      textInput(ns("txt_input"), "Insert some text"),
      actionButton(ns("btn"), "Display text"),
      actionButton(ns("btn_rm"), "Delete self"),
      textOutput(ns("txt_output"))
    )
  )
}

# Server part of the module
ab_module <- function(input, output, session){
  observeEvent(input$btn,{
    output$txt_output <- renderText(input$txt_input)
  })
  observeEvent(input$btn_rm,{
    removeUI(selector = paste0("#", session$ns("self")))
  })
  
  return(list(val = reactive({input$txt_input}),
              rm = reactive({input$btn_rm})))
}


# UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("add", "Add option", icon=icon("plus"), class = "btn btn-primary"),
      div(id="options"),
      actionButton("debug", "Debug")
    ),
    mainPanel(
      htmlOutput("scope")
    )
  )
)

# Server side
server <- function(input, output, session){
  vals <- reactiveValues(moduleReturn = list())
  observeEvent(input$add, {
    #browser()
    
    insertUI("#options", ui = ab_moduleUI(input$add))
    vals$moduleReturn[[input$add]] <- callModule(ab_module, input$add)
    
    showNotification(paste("input$add:", input$add),
                     type = "message")
  })
  
  output$scope <- renderUI({
    vals$moduleReturn %>% 
      map(~ if(!is.null(.x$val())){.x} else{NULL}) %>%
      compact %>% # Remove NULLs from first creation
      map(~ if(.x$rm() == 0 & .x$val() != ""){.x$val()} else {NULL}) %>% 
      compact %>% # Remove empty or removed values
      map_chr(~paste0("Value: ", .x)) %>%
      paste(collapse = "<br/>") %>%
      HTML
  })
  
  observeEvent(input$debug,{
    browser()
  })
}

shinyApp(ui, server)