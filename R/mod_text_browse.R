#' text_browse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_text_browse_ui <- function(id){
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
          width = 6,
          title = NULL,
          fluidRow(
            col_9(),
            col_3(
              actionLink(
                inputId = ns("back"),
                label = "Back to Previous Page"
              )
            )
          ),
          fluidRow(
            col_12(
              p(
                "Here, you can browse the works available from ",
                tags$a("Project Gutenberg", href = "https://www.gutenberg.org/"),
                "to analyze them within the app.",
                "Use the search bar below to look for a text by title or author."
              )
            )
          ),
          fluidRow(
            col_12(
              shinyWidgets::searchInput(
                inputId = ns("search"),
                label = "Search Project Gutenberg",
                value = "",
                placeholder = "Enter Author Name or Book Title",
                width = "95%",
                btnSearch = icon("search")
              )
            )
          ),
          fluidRow(
            col_12(
              style = "text-align: right;",
              shinyjs::hidden(
                actionLink(
                  inputId = ns("go"),
                  label = "Analyze Selected Text"
                )
              )
            )
          ),
          fluidRow(
            col_12(
              DT::dataTableOutput(outputId = ns("search_results"))
            )
          ),
          fluidRow(
            col_12(
              style = "text-align: right;",
              shinyjs::hidden(
                actionLink(
                  inputId = ns("go1"),
                  label = "Analyze Selected Text"
                )
              )
            )
          )
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("main2"),
        col_3(),
        shinydashboard::box(
          id = ns("main_box"),
          width = 6,
          fluidRow(
            col_9(
              p("This is the upload page")
            ),
            col_3(
              actionLink(
                inputId = ns("back1"),
                label = "Back to Previous Page"
              )
            )
          ),
          fluidRow(
            col_12(
              p(
                "Here, you can browse the works available from ",
                tags$a("Project Gutenberg", href = "https://www.gutenberg.org/"),
                "to analyze them within the app.",
                "Use the search function below to perform a matching search, which you",
                " can perform by title or author."
              )
            )
          ),
          fluidRow(
            col_12(
              shinyWidgets::searchInput(
                inputId = ns("search1"),
                label = "Search Project Gutenberg",
                value = "",
                placeholder = "Enter Author Name or Book Title",
                width = "95%",
                btnSearch = icon("search")
              )
            )
          )
        )
      )
    )
  )
}
    
#' text_browse Server Functions
#'
#' @noRd 
mod_text_browse_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe(print(input$search))
    
    observeEvent(input$search, ignoreNULL = TRUE, ignoreInit = TRUE, {
      waiter::waiter_show(
        id = ns("search"),
        color = "#896C70",
        html = waiter::spin_4()
      )
      distance <- stringdist::stringdist(input$search, rv$g_books$title)
      distance_auth <- stringdist::stringdist(input$search, rv$g_books$author)
      search_words <- stringr::str_split(input$search, " ")[[1]]
      # print(search_words) #FIXME
      author_matches <- matrix(NA, nrow = nrow(rv$g_books), ncol = length(search_words))
      title_matches <- matrix(NA, nrow = nrow(rv$g_books), ncol = length(search_words))
      j <- 0
      for (i in tolower(search_words)) {
        j <- j + 1
        author_matches[,j] <- as.numeric(stringr::str_detect(tolower(rv$g_books$author), i))
        title_matches[,j] <- as.numeric(stringr::str_detect(tolower(rv$g_books$title), i))
      }
      # View(author_matches) #FIXME
      # View(title_matches) #FIXME
      author_matches <- rowSums(author_matches)
      title_matches <- rowSums(title_matches)
      
      rv$ordered_df <- rv$g_books %>%
        dplyr::select(gutenberg_id, title, author) %>%
        dplyr::mutate(
          author_words = purrr::map_dbl(stringr::str_split(author, " "), length),
          title_words = purrr::map_dbl(stringr::str_split(title, " "), length),
          author_matches = author_matches / author_words,
          title_matches = title_matches / title_words,
          string_dist_title = distance,
          string_dist_auth = distance_auth,
          exact_match_title = ifelse(stringr::str_detect(tolower(title), tolower(input$search)), 1, 0),
          exact_match_auth = ifelse(stringr::str_detect(tolower(author), tolower(input$search)), 1, 0),
          exact_match = ifelse(exact_match_title == 1, 1, exact_match_auth),
          string_dist = ifelse(string_dist_title < string_dist_auth, string_dist_title, string_dist_auth),
          word_match = ifelse(author_matches < title_matches, title_matches, author_matches),
          exact_match = ifelse(word_match == 1, 1, exact_match)
        ) %>%
        dplyr::arrange(desc(exact_match), desc(word_match), string_dist, title)
      
      waiter::waiter_hide(id = ns("search"))
    })
    
    observeEvent(input$back, ignoreInit = TRUE, {
      shinyjs::hide("main")
      shinyjs::show("text_1_ui_1-main", asis = TRUE)
    })
    
    observeEvent(input$search_results_rows_selected, {
      shinyjs::show("go")
      shinyjs::show("go1")
    })
    
    observeEvent(c(input$go, input$go1), ignoreInit = TRUE, {
      shinyjs::hide("main")
      shinyjs::show("main2")
      waiter::waiter_show(
        id = ns("main_box"),
        color = "#896C70",
        html = waiter::spin_4()
      )
      rv$text_title <- stringr::str_c(
        "<em>Selected Text:</em> ",
        rv$ordered_df %>% 
        dplyr::pull(title) %>% 
        `[`(input$search_results_rows_selected), "<br><em>Author:</em> ",
        rv$ordered_df %>% 
          dplyr::pull(author) %>% 
          `[`(input$search_results_rows_selected)
      )
      gutenberg_id <- rv$ordered_df %>% 
        dplyr::pull(gutenberg_id) %>% 
        `[`(input$search_results_rows_selected)
      rv$text_dat_raw <- gutenbergr::gutenberg_download(
        gutenberg_id, 
        mirror = "http://mirrors.xmission.com/gutenberg/"
      ) 
      rv$text_dat <- rv$text_dat_raw %>%
        tidytext::unnest_tokens(
          output = "word",
          input = "text"
        ) %>%
        dplyr::mutate(word_num = 1:nrow(.))
      shinyjs::hide("main2")
      waiter::waiter_hide(
        id = ns("main_box")
      )
      shinyjs::show("text_analysis_ui_1-main", asis = TRUE)
    })
    
    output$search_results <- DT::renderDataTable({
      req(rv$ordered_df)
      
      num_res <- sum(rv$ordered_df$exact_match, na.rm = TRUE)
      if (num_res < 20) num_res <- 20
      if (num_res > 50) num_res <- 50
      
      rv$ordered_df %>%
        dplyr::select(gutenberg_id, title, author) %>%
        dplyr::slice_head(n = num_res) %>%
        DT::datatable(
          colnames = c("ID", "Title", "Author"),
          options = list(
            pageLength = num_res,
            dom = "t"
          ),
          rownames = FALSE,
          selection = "single"
        )
    })
 
  })
}
    
## To be copied in the UI
# mod_text_browse_ui("text_browse_ui_1")
    
## To be copied in the server
# mod_text_browse_server("text_browse_ui_1")
