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
          class = "HaDeX-tab-content-element",
          br(),
          img(src='./www/logo_2.png', width = "40%", align = "center"),
          br(),
          br(),
          wellPanel(fileInput(inputId = "file_state_1_params",
                              label = "Provide fit results for the first state:"),
                    fileInput(inputId = "file_state_2_params",
                              label = "Provide fit results for the second state:")),
          verbatimTextOutput("input_status")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Hires",
              plotOutput("states_params_plot", width = "95%"),
              fluidRow(
                column(width = 1),
                column(
                  width = 10,
                  plotOutput("distance_plot")
                )
              ),
              p("The residues with NA values for either state are not shown."),
              plotOutput("states_class_components")
            ),
            tabPanel(
              "Peptides",
              plotOutput("plot_peptides_coverage")
            )
          )

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
