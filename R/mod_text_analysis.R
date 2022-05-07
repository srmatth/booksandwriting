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
        id = ns("main"),
        col_1(),
        shinydashboard::box(
          title = NULL,
          width = 10,
          fluidRow(
            col_9(
              uiOutput(outputId = ns("title"))
            ),
            col_3(
              actionLink(
                inputId = ns("back"),
                label = "Select a Different Text"
              ),
              style = "text-align: right;"
            )
          ),
          sidebarLayout(
            sidebarPanel = sidebarPanel(
              p(
                "Welcome to the text analysis page! If you are here it means you are trying to",
                " analyze the text that you have stored in the app so far."
              ),
              textInput(
                inputId = ns("word"),
                label = "Find Parts Similar To...",
                placeholder = "E.g., Love, Battle, Sadness"
              ),
              numericInput(
                inputId = ns("smooth"),
                label = "Rolling average over ___ words",
                value = 200,
                min = 1,
                max = 10000
              ),
              br(),
              actionButton(
                inputId = ns("go"),
                label = "View Analysis",
                class = "text-btn"
              )
            ),
            mainPanel = mainPanel(
              div(
                id = ns("plt_div"),
                plotly::plotlyOutput(outputId = ns("plt")),
                htmlOutput(outputId = ns("clicks"))
              )
            )
          )
        )
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
    
    observeEvent(input$go, ignoreInit = TRUE, {
      req(input$word)
      waiter::waiter_show(
        id = ns("plt_div"),
        color = "#896C70",
        html = waiter::spin_4()
      )
      tokenized_words <- rv$text_dat %>%
        ## Need to create lines (or additional text) here
        #dplyr::mutate(context = get_context(word)) %>%
        dplyr::anti_join(tidytext::stop_words, by = "word") %>%
        dplyr::mutate(
          word = tolower(word),
          word = stringr::str_remove_all(word, "'s"),
          word = stringr::str_extract(word, "[a-z]+" )
        ) %>%
        dplyr::filter(!is.na(word))
      
      preds_word <- predict(rv$word2vec_mod, newdata = tolower(input$word), type = "nearest", top_n = 20000)[[1]] %>%
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
          roll_similarity = zoo::rollmean(
            similarity, 
            k = isolate(input$smooth), 
            fill = NA, 
            align = "center"
          )
        ) %>%
        dplyr::filter(!is.na(roll_similarity))
      
      rv$smoother <- FKSUM::fk_regression(
        y = rv$joined$roll_similarity,
        x = rv$joined$word_num,
        type = "NW",
        h = nrow(rv$joined) / 10
      )
      print(rv$smoother$h)
      waiter::waiter_hide(id = ns("plt_div"))
    }) 
    
    output$title <- renderUI({
      h1(HTML(rv$text_title))
    })
    output$plt <- plotly::renderPlotly({
      req(rv$joined)
      num_words <- isolate(max(rv$joined$word_num, na.rm = TRUE))
      num_roll <- isolate(input$smooth)
      max_sim <- isolate(max(rv$joined$roll_similarity, na.rm = TRUE))
      min_sim <- isolate(min(rv$joined$roll_similarity, na.rm = TRUE))
      rv$joined %>%
        dplyr::filter(!is.na(roll_similarity)) %>%
        plotly::plot_ly() %>%
        plotly::add_trace(
          x = ~word_num, 
          y = ~roll_similarity,
          line = list(
            color = "#53687E"
          ),
          type = "scatter",
          mode = "lines",
          showlegend = FALSE,
          hoverinfo = "text",
          text = ~word
        ) %>% 
        plotly::add_lines(
          x = rv$smoother$x_eval,
          y = rv$smoother$y_fitted,
          line = list(
            color = "#6B4E71",
            width = 4
          ),
          showlegend = FALSE,
          hoverinfo = "text"
        ) %>%
        plotly::config(displayModeBar = FALSE) %>%
        plotly::layout(
          #title = "Similarity Over the Text",
          showlegend = FALSE,
          xaxis = list(
            title = "Progression of Text",
            ticktext = c("Beginning", "Middle", "End"),
            tickvals = c(1 + 2*num_roll, num_words / 2, num_words - 2*num_roll)
          ),
          yaxis = list(
            title = stringr::str_c("Similarity to ", isolate(snakecase::to_title_case(input$word))),
            ticktext = c("Low", " ", "High"),
            tickvals = c(min_sim, (max_sim + min_sim) / 2, max_sim)
          ),
          plot_bgcolor = "#faefef",
          paper_bgcolor = "#faefef"
        )
    })
    
    output$clicks <- renderUI({
      req(rv$joined)
      s <- plotly::event_data("plotly_click")
      if (length(s) == 0) {
        context <- p("Click on a point on the graph to see the associated text")
      } else {
        raw_text <- stringr::str_split(rv$text_dat_raw$text, " |—|-") 
        raw_text <- raw_text %>%
          purrr::flatten_chr() %>%
          `[`(. != "")
        end_num <- nrow(rv$text_dat)
        word_num <- s$x
        
        win_l <- isolate(min(c(floor(input$smooth / 2), 300)))
        min_indx <- ifelse(word_num - win_l < 1, 1, word_num - win_l)
        max_indx <- ifelse(word_num + win_l > end_num, end_num, word_num + win_l)
        
        context <- raw_text[min_indx:max_indx] %>%
          stringr::str_c(collapse = " ") %>%
          p()
      }
      tagList(
        br(),
        h1("Context around Selected Point in Text"),
        context
      )
    })
    
    observeEvent(input$back, ignoreInit = TRUE, {
      shinyjs::hide("main")
      shinyjs::show("text_1_ui_1-main", asis = TRUE)
    })
    
    observeEvent(rv$text_title, ignoreInit = TRUE, {
      rv$joined <- NULL
      updateTextInput(
        inputId = "word",
        value = "",
        label = "Find Parts Similar To...",
        placeholder = "E.g., Love, Battle, Sadness"
      )
    })
  })
}
    
## To be copied in the UI
# mod_text_analysis_ui("text_analysis_ui_1")
    
## To be copied in the server
# mod_text_analysis_server("text_analysis_ui_1")
