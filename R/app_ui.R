#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      br(),
      br(),
      sidebarLayout(
        sidebarPanel(
          br(),
          br(),
          wellPanel(fileInput(inputId = "file_state_1_params",
                              label = "Provide fit results for the first state:"),
                    fileInput(inputId = "file_state_2_params",
                              label = "Provide fit results for the second state:"))

        ),
        mainPanel(
          plotOutput("state_1_params_plot"),
          plotOutput("state_2_params_plot")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "compahradex"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
