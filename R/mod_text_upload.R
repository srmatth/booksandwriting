#' text_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_text_upload_ui <- function(id){
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
                "Upload your own file on this page by dragging and dropping in the ",
                "upload box below or by selecting from your local computer. ",
                "Files must be in .pdf, .doc, .docx, .rft, or .txt format."
              )
            )
          ),
          fileInput(
            inputId = ns("file"),
            label = "Drag and drop your text file here",
            multiple = FALSE,
            accept = c(".pdf", ".doc", ".docx", ".rtf", ".txt"),
            placeholder = " ",
            buttonLabel = "... or Select from Your Computer"
          ),
          fluidRow(
            col_9(
              uiOutput(outputId = ns("file_name"))
            ),
            col_3(
              shinyjs::hidden(
                actionLink(
                  inputId = ns("go"),
                  label = "Analyze Uploaded Text"
                )
              )
            )
          )
        )
      )
    )
 
  )
}
    
#' text_upload Server Functions
#'
#' @noRd 
mod_text_upload_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ## Go back to the previous page
    observeEvent(input$back, ignoreInit = TRUE, {
      shinyjs::hide("main")
      shinyjs::show("text_1_ui_1-main", asis = TRUE)
    })
    observeEvent(input$go, ignoreInit = TRUE, {
      shinyjs::hide("main")
      shinyjs::show("text_analysis_ui_1-main", asis = TRUE)
    })
    observeEvent(input$file, ignoreInit = TRUE, {
      
      ## Check the file extension
      ext <- tolower(fs::path_ext(input$file$name))
      # print(ext) #FIXME
      accepted <- c("pdf", "doc", "docx", "rtf", "txt", "ppt", "pptx")
      if (ext %in% accepted) {
        rv$text_dat_raw <- data.frame(text = textreadr::read_document(input$file$datapath))
        rv$text_dat <- rv$text_dat_raw %>%
          tidytext::unnest_tokens(
            output = "word",
            input = "text"
          ) %>%
          dplyr::mutate(word_num = 1:nrow(.))
        rv$text_title <- stringr::str_c("<em>Uploaded File:</em> ", input$file$name)
        shinyjs::show("go")
      } else {
        ## Give error message here
        shinyalert::shinyalert(
          title = "Error: Incorrect File Type",
          text = "Please ensure that the file you upload is in a text file format: .pdf, .doc, .docx, .rtf, .txt, .ppt, .pptx",
          type = "error",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE
        )
      }
      
    })
    
    output$file_name <- renderUI({
      out_text <- ifelse(is.null(input$file), "None", input$file$name)
      p(
        "Uploaded File:",
        br(),
        tags$em(out_text)
      )
    })
 
  })
}
    
## To be copied in the UI
# mod_text_upload_ui("text_upload_ui_1")
    
## To be copied in the server
# mod_text_upload_server("text_upload_ui_1")
