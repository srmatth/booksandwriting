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
                max = 1000
              ),
              actionButton(
                inputId = ns("go"),
                label = "View Analysis"
              )
            ),
            mainPanel = mainPanel(
              div(
                id = ns("plt_div"),
                plotly::plotlyOutput(outputId = ns("plt")),
                #plotOutput(outputId = ns("plt"))
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
        )
      
      #rv$smoother <- loess(roll_similarity ~ word_num, data = rv$joined, span = 0.75)
      waiter::waiter_hide(id = ns("plt_div"))
    }) 
    
    # output$plt <- renderPlot({
    #   req(rv$joined)
    #   rv$joined %>%
    #     ggplot2::ggplot() +
    #     ggplot2::aes(x = word_num, y = roll_similarity) +
    #     ggplot2::geom_line(alpha = 0.5) +
    #     ggplot2::geom_smooth(se = FALSE, color = "#6B4E71", size = 2) +
    #     ggplot2::theme_classic() +
    #     ggplot2::xlab("Progression of Story") +
    #     ggplot2::ylab(stringr::str_c("Similarity to ", snakecase::to_title_case(input$word)))
    # })
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
        # plotly::add_lines(
        #   x = ~word_num,
        #   y = predict(rv$smoother),
        #   line = list(
        #     color = "#6B4E71",
        #     width = 4
        #   ),
        #   showlegend = FALSE,
        #   hoverinfo = "text",
        #   text = ~word
        # ) %>%
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
        p("Click on a point on the graph to see the associated text")
      } else {
        raw_text <- stringr::str_split(rv$text_dat_raw$text, " |—|-") 
        # breaks <- cumsum(
        #   purrr::map_int(
        #     raw_text, 
        #     ~{ifelse(all(.x == ""), 0L, length(.x))}
        #     )
        #   ) %>%
        #   unique()
        raw_text <- raw_text %>%
          purrr::flatten_chr() %>%
          `[`(. != "")
        print("Length of raw text:")
        print(length(raw_text))
        end_num <- nrow(rv$text_dat)
        word_num <- s$x
        print("word number")
        print(word_num)
        win_l <- isolate(floor(input$smooth / 2))
        min_indx <- ifelse(word_num - win_l < 1, 1, word_num - win_l)
        max_indx <- ifelse(word_num + win_l > end_num, end_num, word_num + win_l)
        # sub_breaks <- breaks[breaks > min_indx & breaks < max_indx]
        # out_words <- c(raw_text[min_indx:max_indx], rep("<br>", length(sub_breaks)))
        # ids <- c(seq_along(raw_text[min_indx:max_indx]), sub_breaks - min_indx + 0.5) 
        # 
        # text_out <- out_words[order(ids)] %>%
        #   stringr::str_c(collapse = " ") 
        # # print(text_out)
        # text_out %>%
        #   HTML() %>%
        #   p()
        # View(cbind(raw_text = raw_text, cleaned_text = c(rv$text_dat$word, rep(NA, length(raw_text) - nrow(rv$text_dat)))))
        raw_text[min_indx:max_indx] %>%
          stringr::str_c(collapse = " ") %>%
          p()
      }
    })
  })
}
    
## To be copied in the UI
# mod_text_analysis_ui("text_analysis_ui_1")
    
## To be copied in the server
# mod_text_analysis_server("text_analysis_ui_1")
