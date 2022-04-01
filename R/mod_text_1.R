#' text_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_text_1_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      div(
        id = ns("main"),
        col_3(),
        shinydashboard::box(
          title = NULL,
          width = 6,
          p("Here we will put the stuff")
        )
      )
    )
  )
}

#' text_1 Server Functions
#'
#' @noRd 
mod_text_1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}

## To be copied in the UI
# mod_text_1_ui("text_1_ui_1")

## To be copied in the server
# mod_text_1_server("text_1_ui_1")
