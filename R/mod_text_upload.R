#' text_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_text_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      div(
        id = ns("main"),
        col_3(),
        shinydashboard::box(
          width = 6,
          title = NULL,
          p("This is the upload page"),
          fileInput(
            inputId = ns("file"),
            label = "Drag and drop your text file here or select from your computer.",
            multiple = FALSE,
            accept = c(".pdf", ".doc", ".docx", ".rtf", ".txt")
          )
        )
      )
    )
 
  )
}
    
#' text_upload Server Functions
#'
#' @noRd 
mod_text_upload_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_text_upload_ui("text_upload_ui_1")
    
## To be copied in the server
# mod_text_upload_server("text_upload_ui_1")
