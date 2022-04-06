#' text_browse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_text_browse_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      div(
        id = ns("main"),
        col_3(),
        shinydashboard::box(
          width = 6,
          title = NULL,
          p("This is the browse page")
        )
      )
    )
  )
}
    
#' text_browse Server Functions
#'
#' @noRd 
mod_text_browse_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_text_browse_ui("text_browse_ui_1")
    
## To be copied in the server
# mod_text_browse_server("text_browse_ui_1")
