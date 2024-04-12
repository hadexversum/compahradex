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
      tags$style(type='text/css', '#txt_out {white-space: pre-wrap;}'),
      br(),
      br(),
      sidebarLayout(
        sidebarPanel(
          class = "HaDeX-tab-content-element",
          br(),
          img(src='./www/logo_2.png', width = "40%", align = "center"),
          br(),
          br(),
          p("Compare fit results from HRaDeX for two biological states:"),
          collapsible_card(
            title = "Input data",
            p("Tip: Fit results can be found in the `Params` tab in HRaDeX."),
            fileInput(inputId = "file_state_1_params",
                      label = "Provide fit results for the first state:"),
            fileInput(inputId = "file_state_2_params",
                      label = "Provide fit results for the second state:"),
            verbatimTextOutput("fit_data_status"),
            p("Tip: UC data can be found in the `UC data` tab in HRaDeX."),
            fileInput(inputId = "file_state_1_uc",
                      label = "Provide uc data for the first state:"),
            fileInput(inputId = "file_state_2_uc",
                      label = "Provide uc data for the second state:"),
            verbatimTextOutput("uc_data_status"),
            fancy_icon = "cogs"
          ),
          br(),
          collapsible_card(
            init_collapsed = T,
            title = "UC data",
            p("See the uptake curve for selected peptide in tab `Uptake Curves`."),
            dataTableOutput_h("peptide_list_data"),
            actionButton(inputId = "reset_peptide_list",
                         label = "Reset chosen peptides"),
            br(),
            fancy_icon = "cogs"
          ),
          br(),
          collapsible_card(
            init_collapsed = T,
            title = "Structure",
            p("If the structure of the protein is available, upload the pdb file:"),
            fileInput(inputId = "pdb_file",
                      label = "PDB file: ",
                      accept = c(".pdb", ".cif")),
            p("The structure is to be found in the tab `Structure`."),
            h3("Which values should be presented on the structure?"),
            fluidRow(
              column(
                width = 6,
                checkboxInput(inputId = "str_show_color_dist",
                              label = "Color distance",
                              value = F),
                numericInput(inputId = "threshold_color",
                             label = "Threshold for color distance:",
                             value = 0.1,
                             min = 0, max = 100),
                p("Residues with color distance value above the threshold are presented in color aquamarine.")
              ),
              column(
                width = 6,
                checkboxInput(inputId = "str_show_uc_dist",
                              label = "UC distance",
                              value = F),
                numericInput(inputId = "threshold_uc",
                             label = "Threshold for UC distance:",
                             value = 150,
                             min = 0, max = 100),
                p("Residues with UC distance value above the threshold are presented in color pink.")

              )
            ),
            p("When both distances are selected, residueas meeting both criteria are presented in color orange. Those colors are beyond classification color code to avoid confusion."),
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
                  "Hires Plot",
                  girafeOutput_h("states_params_plot"),
                ),
                tabPanel(
                  "Data",
                  DT::dataTableOutput("states_params_plot_data")
                )
              ),
              br(),
              tabsetPanel(
                tabPanel(
                  "Color Dist Plot",
                  girafeOutput_h("distance_plot"),
                  p("The residues with NA values for either state are not shown."),

                ),
                tabPanel(
                  "K Dist Plot",
                  girafeOutput_h("k_distance_plot"),
                  p("The residues with NA values for either state are not shown."),

                ),
                tabPanel(
                  "Data",
                  DT::dataTableOutput("distance_plot_data")
                )
              ),
              br(),
              tabsetPanel(
                tabPanel(
                  "UC Dist Plot 2",
                  girafeOutput_h("uc_diff_plot_2"),
                  checkboxInput(inputId = "is_diff_plot_2_fractional",
                                label = "See fractional data?",
                                value = T),
                  checkboxInput(inputId = "is_diff_plot_2_squared",
                                label = "See squared data?",
                                value = F),
                  p("Method of calculation:"),
                  p("For every time point of uptake curve for two states it is recognized which state has higher value and which has lower. From the higher value is substracted the uncertainty value to have the lowest possible value. To the lower value the uncertainty value is added to get the highest possible value. Now, the difference is calculated. Values from all of the time points are summarised to get one value of difference between uptake curves for each peptide.")

                ),
                tabPanel(
                  "UC Dist Plot",
                  girafeOutput_h("uc_diff_plot"),
                  checkboxInput(inputId = "is_diff_fractional",
                                label = "See fractional data?",
                                value = T),
                  checkboxInput(inputId = "is_diff_plot_squared",
                                label = "See squared data?",
                                value = F),
                  p("Method of calculation:"),
                  p("For every time point of uptake curve for two states is calculated the value: difference between uptake in first state and second state, divided by the sum of uncertainty. Then, all the values from the time points are summarised to get one value for each peptide.")
                ),

                tabPanel(
                  "Data",
                  DT::dataTableOutput("uc_diff_plot_data")
                )
              )
            ),
            tabPanel(
              "Uptake Curves",
              br(),
              girafeOutput_h("uc_plot",
                            height = "500px")
            ),
            tabPanel(
              "Classification",
              br(),
              p("Classification results for the first state:"),
              girafeOutput_h("plot_peptides_coverage_1"),
              p("Classification results for the second state:"),
              girafeOutput_h("plot_peptides_coverage_2"),
              p("Classification components for the first state:"),
              girafeOutput_h("state_1_class_components"),
              p("Classification components for the second state:"),
              girafeOutput_h("state_2_class_components")
            ),
            tabPanel(
              "Structure",
              br(),
              r3dmol::r3dmolOutput("protein_structure", width = "100%", height = "1000px"),
              p("To make the image of the structure, set the protein in desired position, stop the spinning and make a screen shot. ")
            ),
            tabPanel(
              "About",
              br(),
              wellPanel(
                includeMarkdown(app_sys("app/man/about.md"))
              )
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
