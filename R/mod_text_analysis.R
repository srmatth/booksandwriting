#' text_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_text_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      div(
        id = ns("main")
      )
    )
  )
}
    
#' text_analysis Server Functions
#'
#' @noRd 
mod_text_analysis_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_text_analysis_ui("text_analysis_ui_1")
    
## To be copied in the server
# mod_text_analysis_server("text_analysis_ui_1")
