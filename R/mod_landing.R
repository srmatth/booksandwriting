#' landing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_landing_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      id = ns("main"),
      br(),
      br(),
      br(),
      fluidRow(
        col_3(),
        col_6(
          shinydashboard::box(
            title = NULL,
            width = 12,
            fluidRow(
              col_6(
                actionButton(
                  inputId = ns("tutorial"),
                  label = "See a Tutorial",
                  class = "tutorial-btn"
                )
              ),
              col_6(
                actionButton(
                  inputId = ns("text"),
                  label = "Analyze a Text",
                  class = "text-btn"
                )
              )
            ),
            fluidRow(
              col_3(),
              col_6(
                actionButton(
                  inputId = ns("learn"),
                  label = "Learn about Word2Vec",
                  class = "learn-btn"
                )
              )
            )
          )
        ),
        col_3()
      )
    )
  )
}

#' landing Server Functions
#'
#' @noRd 
mod_landing_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$tutorial, {
      ## Hide main tab and show tutorial page
      shinyjs::hide("landing_ui_1-main", asis = TRUE)
    })
    
    observeEvent(input$text, {
      ## Hide main tab and show the first text page
      shinyjs::hide("landing_ui_1-main", asis = TRUE)
      shinyjs::show("text_1_ui_1-main", asis = TRUE)
    })
    
    observeEvent(input$learn, {
      ## Hide main tab and show the tutorial page
      shinyjs::hide("landing_ui_1-main", asis = TRUE)
    })
    
  })
}

## To be copied in the UI
# mod_landing_ui("landing_ui_1")

## To be copied in the server
# mod_landing_server("landing_ui_1")
