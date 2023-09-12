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

  ##

  check_protein <- reactive({

    state_1_params()[["Protein"]][1] == state_2_params()[["Protein"]][1]

  })

  output[["input_status"]] <- renderText({

    validate(need(!is.null(state_1_params()) & !is.null(state_2_params()), ""))

    if(check_protein()) {

      paste0("The uploaded files containt data for ", state_1_params()[["Protein"]][1], " protein \n in state ", state_1_params()[["State"]][1],
             " and state ", state_2_params()[["State"]][1], ".")

    } else {

      paste0("Uploaded files are for different proteins.")
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

  two_states_dataset <- reactive({

    HRaDeX::create_two_state_dataset(state_1_hires_params(),
                                     state_2_hires_params())

  })

  output[["states_params_plot"]] <- renderPlot({

    validate(need(!is.null(state_1_params()) & !is.null(state_2_params()), ""))
    HRaDeX::plot_two_states(state_1_hires_params(),
                            state_2_hires_params())
  })

  output[["distance_plot"]] <- renderPlot({

    validate(need(!is.null(state_1_params()) & !is.null(state_2_params()), ""))
    HRaDeX::plot_color_distance(two_states_dataset())

  })

}
