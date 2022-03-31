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
  
  mod_word2vec_server("word2vec_ui_1", rv = rv)
}
