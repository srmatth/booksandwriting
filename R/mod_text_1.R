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
        br(),
        br(),
        br(),
        col_3(),
        shinydashboard::box(
          title = NULL,
          width = 6,
          p(
            "To profile a text, you can either upload your own file by clicking on 'Upload a Document' (must ",
            "be in .pdf, .doc, .docx, .rft, or .txt format) or you can select ",
            "a work available on ",
            tags$a("Project Gutenberg", href = "https://www.gutenberg.org/"),
            " by clicking on 'Browse Project Gutenberg'."
          ),
          p(
            "Please note that as of now, only texts in the English Language are supported.",
            "Furthermore, the model used to compute word similarity was trained on modern ",
            "text. This means that profiling of historical documents may be innaccurate ",
            "due to changes in semantic word meaning over time."
          ),
          br(),
          fluidRow(
            col_6(
              actionButton(
                inputId = ns("upload"),
                label = "Upload a Document",
                class = "text-btn"
              )
            ),
            col_6(
              actionButton(
                inputId = ns("browse"),
                label = "Browse Project Gutenberg",
                class = "text-btn"
              )
            )
          )
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
    
    ## Observers
    observeEvent(input$upload, ignoreInit = TRUE, {
      shinyjs::hide("main")
      shinyjs::show("text_upload_ui_1-main", asis = TRUE)
    })
    observeEvent(input$browse, ignoreInit = TRUE, {
      shinyjs::hide("main")
      shinyjs::show("text_browse_ui_1-main", asis = TRUE)
    })
  })
}

## To be copied in the UI
# mod_text_1_ui("text_1_ui_1")

## To be copied in the server
# mod_text_1_server("text_1_ui_1")
