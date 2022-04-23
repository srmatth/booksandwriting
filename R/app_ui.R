#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny gutenbergr
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(
        title = "Creative Title",
        titleWidth = 450,
        tags$li(
          actionLink(
            inputId = "return_home",
            label = "Return to Landing Page",
            class = "header-btn"
          ),
          class = "dropdown"
        )
      ),
      shinydashboard::dashboardSidebar(disable = TRUE),
      shinydashboard::dashboardBody(
        mod_landing_ui("landing_ui_1"),
        mod_text_1_ui("text_1_ui_1"),
        mod_text_browse_ui("text_browse_ui_1"),
        mod_text_upload_ui("text_upload_ui_1"),
        mod_text_analysis_ui("text_analysis_ui_1")
        #shinyWidgets::setBackgroundImage(src = "inst/app/www/books_bg.jpg", shinydashboard = TRUE)
      )
      #mod_word2vec_ui("word2vec_ui_1")
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny gutenbergr
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'booksandwriting'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    
    shinyWidgets::useShinydashboard(),
    shinyjs::useShinyjs(),
    waiter::useWaiter()
  )
}

