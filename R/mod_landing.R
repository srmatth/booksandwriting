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
              col_12(
                p(
                  "Hello, and welcome to the Semantic Text Profiling tool.",
                  style = "font-weight: bold; font-size: 14pt;"
                ),
                p(
                  "This tool computes the semantic similarity of a document to a target concept, ",
                  "and then plots the resulting semantic similarity profile across the text. ",
                  style = "font-size: 12pt;"
                ),
                p(
                  "If this is your first time on the app, press the 'Learn More' button to ",
                  "see further examples and discover the methods behind the output. ",
                  "Otherwise, get started by clicking 'Profile a Text'!",
                  style = "font-size: 12pt;"
                )
              )
            ),
            br(),
            fluidRow(
              col_6(
                actionButton(
                  inputId = ns("tutorial"),
                  label = "Learn More",
                  class = "tutorial-btn"
                )
              ),
              col_6(
                actionButton(
                  inputId = ns("text"),
                  label = "Profile a Text",
                  class = "text-btn"
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
    
  })
}

## To be copied in the UI
# mod_landing_ui("landing_ui_1")

## To be copied in the server
# mod_landing_server("landing_ui_1")
