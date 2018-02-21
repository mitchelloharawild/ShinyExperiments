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
}


# UI
ui <- fluidPage(
  actionButton("add", "Add option", icon=icon("plus"), class = "btn btn-primary"),
  div(id="options"),
  actionButton("debug", "Debug")
)

# Server side
server <- function(input, output, session){
  
  observeEvent(input$add, {
    #browser()
    
    insertUI("#options", ui = ab_moduleUI(input$add))
    callModule(ab_module, input$add)
    
    showNotification(paste("input$add:", input$add),
                     type = "message")
  })
  
  observeEvent(input$debug,{
    browser()
  })
}

shinyApp(ui, server)