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
                      label = "Provide uc data for the second state:"),
            fancy_icon = "cogs"
          ),
          br(),
          collapsible_card(
            init_collapsed = T,
            title = "UC data",
            p("See the uptake curve for selected peptide in tab `Uptake Curves`"),
            dataTableOutput_h("peptide_list_data"),
            actionButton(inputId = "reset_peptide_list",
                         label = "Reset chosen peptides"),
            br(),
            fancy_icon = "cogs"
            # sliderInput(inputId = "uc_plot_x_range",
            #             label = "Select x range: ",
            #             min = 0, max = 100,
            #             value = c(0, 100)),
            # sliderInput(inputId = "uc_plot_y_range",
            #             label = "Select y range: ",
            #             min = 0, max = 100,
            #             value = c(0, 100))
          ),
          br(),
          collapsible_card(
            init_collapsed = T,
            title = "Structure",
            p("If the structure of the protein is available, upload the pdb file:"),
            fileInput(inputId = "pdb_file",
                      label = "PDB file: ",
                      accept = ".pdb"),
            checkboxGroupInput(inputId = "values_structure",
                        label = "Select values to be shown on the structure:",
                        choices = c("color distance", "uc distance"),
                        selected = "color distance"
            ),
            numericInput(inputId = "threshold_color",
                         label = "Threshold for color distance:",
                         value = 0.1,
                         min = 0, max = 100),
            numericInput(inputId = "threshold_uc",
                         label = "Threshold for UC distance:",
                         value = 150,
                         min = 0, max = 100),
            fancy_icon = "cogs"
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Hires results",
              br(),
              tabsetPanel(
                tabPanel(
                  "Plot",
                  ggiraph::girafeOutput("states_params_plot"),
                ),
                tabPanel(
                  "Data"
                )
              ),
              br(),
              tabsetPanel(
                tabPanel(
                  "Plot",
                  ggiraph::girafeOutput("distance_plot"),
                  p("The residues with NA values for either state are not shown."),

                ),
                tabPanel(
                  "Data"
                )
              ),
              br(),
              tabsetPanel(
                tabPanel(
                  "Plot",
                  ggiraph::girafeOutput("uc_diff_plot")
                ),
                tabPanel(
                  "Data"
                )
              )
            ),
            tabPanel(
              "Uptake Curves",
              ggiraph::girafeOutput("uc_plot")
            ),
            tabPanel(
              "Classification",
              ggiraph::girafeOutput("plot_peptides_coverage_1"),
              ggiraph::girafeOutput("plot_peptides_coverage_2"),
              ggiraph::girafeOutput("state_1_class_components"),
              ggiraph::girafeOutput("state_2_class_components")
            ),
            tabPanel(
              "Structure",
              br(),
              r3dmol::r3dmolOutput("protein_structure", width = "100%", height = "1000px")
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
