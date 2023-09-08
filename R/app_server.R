#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  state_1_params <- reactive({
    data_file <- input[["file_state_1_params"]]
    if(is.null(data_file)){
      NULL
      } else {
        read.csv(data_file[["datapath"]])
        }

  })

  state_2_params <- reactive({
    data_file <- input[["file_state_2_params"]]
    if(is.null(data_file)){
      NULL
    } else {
      read.csv(data_file[["datapath"]])
    }

  })

  ## hires calc

  state_1_hires_params <- reactive({

    HRaDeX::calculate_hires(state_1_params(),
                            fractional = T)

  })

  state_2_hires_params <- reactive({

    HRaDeX::calculate_hires(state_2_params(),
                            fractional = T)

  })

  output[["state_1_params_plot"]] <- renderPlot({

    validate(need(!is.null(state_1_params()), "No data provided"))
    HRaDeX::plot_hires(state_1_hires_params())

  })

  output[["state_2_params_plot"]] <- renderPlot({

    validate(need(!is.null(state_2_params()), "No data provided"))
    HRaDeX::plot_hires(state_2_hires_params())

  })
}
