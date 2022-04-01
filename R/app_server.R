#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny gutenbergr
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  rv <- rv()
  rv$word2vec_mod <- word2vec::read.word2vec("data/models/word2vec_1/model.bin", normalize = TRUE)
  
  mod_landing_server("landing_ui_1")
  mod_text_1_server("text_1_ui_1")
  mod_word2vec_server("word2vec_ui_1", rv = rv)
  
  observeEvent(input$return_home, {
    ## Hide all other modules
    shinyjs::hide("text_1_ui_1-main", asis = TRUE)
    
    ## Show landing page module
    shinyjs::show("landing_ui_1-main", asis = TRUE)
  })
}
