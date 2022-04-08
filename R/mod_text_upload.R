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
          fluidRow(
            col_9(
              p("This is the upload page")
            ),
            col_3(
              actionLink(
                inputId = ns("back"),
                label = "Back to Previous Page"
              )
            )
          ),
          fileInput(
            inputId = ns("file"),
            label = "Drag and drop your text file here",
            multiple = FALSE,
            accept = c(".pdf", ".doc", ".docx", ".rtf", ".txt"),
            placeholder = " ",
            buttonLabel = "... or Select from Your Computer"
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
    
    ## Go back to the previous page
    observeEvent(input$back, ignoreInit = TRUE, {
      shinyjs::hide("main")
      shinyjs::show("mod_text_1_ui_1-main", asis = TRUE)
    })
 
  })
}
    
## To be copied in the UI
# mod_text_upload_ui("text_upload_ui_1")
    
## To be copied in the server
# mod_text_upload_server("text_upload_ui_1")
