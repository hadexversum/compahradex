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
          collapsible_card(
            title = "Input data",
            p("Tip: Fit results can be found in the `Params` tab in HRaDeX."),
            fileInput(inputId = "file_state_1_params",
                      label = "Provide fit results for the first state:"),
            fileInput(inputId = "file_state_2_params",
                      label = "Provide fit results for the second state:"),
            verbatimTextOutput("input_status"),
            p("Tip: UC data can be found in the `UC data` tab in HRaDeX."),
            fileInput(inputId = "file_state_1_uc",
                      label = "Provide uc data for the first state:"),
            fileInput(inputId = "file_state_2_uc",
                      label = "Provide uc data for the second state:")
          ),
          br(),
          collapsible_card(
            init_collapsed = T,
            title = "UC data",
            p("See the uptake curve for selected peptide in tab `Peptides`"),
            dataTableOutput_h("peptide_list_data"),
            actionButton(inputId = "reset_peptide_list",
                         label = "Reset chosen peptides")



          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Hires",
              plotOutput_h("states_params_plot"),
              plotOutput_h("distance_plot"),
              p("The residues with NA values for either state are not shown."),
              plotOutput_h("uc_diff_plot"),
              plotOutput_h("states_class_components")
            ),
            tabPanel(
              "Peptides",
              plotOutput_h("plot_peptides_coverage"),
              plotOutput_h("uc_plot")
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
  add_resource_path(
    "utils",
    app_sys("app/utils")
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
