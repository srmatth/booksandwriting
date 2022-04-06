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
          p("I want to..."),
          fluidRow(
            col_6(
              actionButton(
                inputId = ns("upload"),
                label = "Upload My Own File",
                class = "text-btn"
              )
            ),
            col_6(
              actionButton(
                inputId = ns("browse"),
                label = "Browse Built-in Texts",
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
