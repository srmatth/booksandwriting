#' word2vec UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_word2vec_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        width = 3,
        selectInput(
          inputId = ns("book"),
          label = "Select a Book",
          choices = gutenbergr::gutenberg_works(author == "Austen, Jane") %>%
            dplyr::pull(title),
          multiple = FALSE,
          selected = NULL
        ),
        textInput(
          inputId = ns("word"),
          label = "Enter word to compute similarity scores for (must be a single word)",
          value = "",
          placeholder = "E.g. confusion"
        ),
        numericInput(
          inputId = ns("smooth"),
          label = "Rolling average over ___ words",
          value = 200,
          min = 1,
          max = 1000
        ),
        actionButton(
          inputId = ns("go"),
          label = "View Plot"
        )
      ),
      mainPanel = mainPanel(
        width = 9,
        plotOutput(
          outputId = ns("plt")
        )
      )
    )
  )
}
    
#' word2vec Server Functions
#'
#' @noRd 
mod_word2vec_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(ignoreInit = TRUE, input$go, {
      book_id <- gutenbergr::gutenberg_works(title == input$book) %>%
        dplyr::pull(gutenberg_id)
      text <- gutenberg_download(book_id, mirror = "http://mirrors.xmission.com/gutenberg/")
      tokenized_words <- text %>%
        dplyr::mutate(line = 1:nrow(.)) %>%
        dplyr::select(-gutenberg_id) %>%
        dplyr::filter(text != "") %>%
        tidytext::unnest_tokens(word, text) %>%
        dplyr::anti_join(tidytext::stop_words, by = "word") %>%
        dplyr::mutate(
          word = tolower(word),
          word = stringr::str_remove_all(word, "'s"),
          word = stringr::str_extract(word, "[a-z]+" )
        ) %>%
        dplyr::filter(!is.na(word))
      
      preds_word <- predict(model, newdata = input$word, type = "nearest", top_n = 20000)[[1]] %>%
        dplyr::filter(!stringr::str_detect(term2, "::")) %>%
        dplyr::mutate(similarity = (similarity - min(similarity)) / (max(similarity) - min(similarity)))
      
      rv$joined <- dplyr::left_join(
        tokenized_words, 
        preds_word %>% 
          dplyr::select(term2, similarity), 
        by = c("word" = "term2")
      ) %>%
        dplyr::mutate(
          similarity = tidyr::replace_na(similarity, 0),
          roll_similarity = zoo::rollmean(similarity, k = input$smooth, fill = NA)
        )
    })
    
    output$plt <- renderPlot({
      req(rv$joined)
      rv$joined %>%
        ggplot2::ggplot() +
        ggplot2::aes(x = 1:nrow(rv$joined), y = roll_similarity) +
        ggplot2::geom_line() +
        ggplot2::geom_smooth(se = FALSE)
    })
 
  })
}
    
## To be copied in the UI
# mod_word2vec_ui("word2vec_ui_1")
    
## To be copied in the server
# mod_word2vec_server("word2vec_ui_1")
