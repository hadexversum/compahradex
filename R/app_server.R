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

  state_1_uc <- reactive({
    data_file <- input[["file_state_1_uc"]]
    if(is.null(data_file)){
      NULL
    } else {
      read.csv(data_file[["datapath"]])
    }
  })

  state_2_uc <- reactive({
    data_file <- input[["file_state_2_uc"]]
    if(is.null(data_file)){
      NULL
    } else {
      read.csv(data_file[["datapath"]])
    }
  })

  ##

  fractional <- reactive({

    if(any(state_1_params()[["n_1"]] > 2, na.rm = TRUE) | any(state_1_params()[["n_2"]] > 2, na.rm = TRUE)){
      FALSE
    } else { TRUE }

  })

  ##

  observe({

    y_max <- if(fractional()){ 1.2 } else {max(state_1_uc()[["deut_uptake"]], state_2_uc()[["deut_uptake"]])}

    updateSliderInput(inputId = "uc_plot_y_range",
                      max = y_max,
                      value = c(0, y_max))

  })

  observe({

    x_max <- max(state_1_uc()[["End"]])

    updateSliderInput(inputId = "uc_plot_x_range",
                      max = x_max,
                      value = c(0, x_max))

  })

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
                            fractional = fractional())

  })

  state_2_hires_params <- reactive({

    HRaDeX::calculate_hires(state_2_params(),
                            fractional = fractional())

  })

  two_states_dataset <- reactive({

    HRaDeX::create_two_state_dataset(state_1_hires_params(),
                                     state_2_hires_params())

  })

  uc_diff_dataset <- reactive({

    HRaDeX::create_uc_distance_dataset(state_1_uc(),
                                       state_2_uc())
  })

  ## TAB: HIRES ##

  output[["states_params_plot"]] <- ggiraph::renderGirafe({

    validate(need(!is.null(state_1_params()) & !is.null(state_2_params()), "Please upload necessary files."))

    HRaDeX::plot_two_states(state_1_hires_params(),
                            state_2_hires_params(),
                            interactive = T)
  })

  output[["states_params_plot_data"]] <- DT::renderDataTable({

    validate(need(!is.null(state_1_params()) & !is.null(state_2_params()), "Please upload necessary files."))

    DT::datatable(rbind(dplyr::mutate(state_1_hires_params(),
                                      n_1 = formatC(n_1, 2),
                                      n_2 = formatC(n_2, 2),
                                      n_3 = formatC(n_3, 2),
                                      k_est = formatC(k_est, 4)),
                        dplyr::mutate(state_2_hires_params(),
                                      n_1 = formatC(n_1, 2),
                                      n_2 = formatC(n_2, 2),
                                      n_3 = formatC(n_3, 2),
                                      k_est = formatC(k_est, 4))))

  })

  ##

  output[["distance_plot"]] <- ggiraph::renderGirafe({

    validate(need(!is.null(state_1_params()) & !is.null(state_2_params()), "Please upload necessary files."))
    HRaDeX::plot_color_distance(two_states_dataset(),
                                interactive = T)

  })


  output[["k_distance_plot"]] <- ggiraph::renderGirafe({

    validate(need(!is.null(state_1_params()) & !is.null(state_2_params()), "Please upload necessary files."))

    HRaDeX::plot_k_distance(two_states_dataset(),
                            interactive = T)

  })


  output[["distance_plot_data"]] <- DT::renderDataTable({

    validate(need(!is.null(state_1_params()) & !is.null(state_2_params()), "Please upload necessary files."))

    DT::datatable(dplyr::mutate(two_states_dataset(),
                                dist = formatC(dist, 2),
                                k_diff = formatC(k_diff, 2)))

  })

  ##

  output[["uc_diff_plot"]] <- ggiraph::renderGirafe({

    validate(need(!is.null(state_1_uc()) & !is.null(state_2_uc()), "Please upload necessary files."))

    HRaDeX::plot_uc_distance(uc_diff_dataset(),
                             fractional = input[["is_diff_fractional"]],
                             squared = input[["is_diff_plot_squared"]],
                             interactive = T)

  })

  output[["uc_diff_plot_2"]] <- ggiraph::renderGirafe({

    validate(need(!is.null(state_1_uc()) & !is.null(state_2_uc()), "Please upload necessary files."))

    HRaDeX::plot_uc_real_dist(uc_diff_dataset(),
                              fractional = input[["is_diff_plot_2_fractional"]],
                              squared = input[["is_diff_plot_2_squared"]],
                              interactive = T)

  })

  ##

  output[["uc_diff_plot_data"]] <- DT::renderDataTable({

    validate(need(!is.null(state_1_uc()) & !is.null(state_2_uc()), "Please upload necessary files."))

    DT::datatable(dplyr::mutate(uc_diff_dataset(),
                                frac_uptake_diff = formatC(frac_uptake_diff, 4),
                                uptake_diff = formatC(uptake_diff, 4),
                                frac_uptake_dist = formatC(frac_uptake_dist, 4),
                                uptake_dist = formatC(uptake_dist, 4)))

  })


  ##

  ## TAB: CLASSIFICATION ##

  output[["state_1_class_components"]] <- ggiraph::renderGirafe({

    validate(need(!is.null(state_1_params()), "Please upload necessary files."))

    HRaDeX::plot_hires_components(state_1_hires_params(),
                                  interactive = T)

  })

  output[["state_2_class_components"]] <- ggiraph::renderGirafe({

    validate(need(!is.null(state_2_params()), "Please upload necessary files."))

    HRaDeX::plot_hires_components(state_2_hires_params(),
                                  interactive = T)


  })


  output[["plot_peptides_coverage_1"]] <- ggiraph::renderGirafe({

    validate(need(!is.null(state_1_params()), ""))

    HRaDeX::plot_cov_class(state_1_params(),
                           fractional = fractional(),
                           interactive = T)



  })

  output[["plot_peptides_coverage_2"]] <- ggiraph::renderGirafe({

    validate(need(!is.null(state_2_params()), ""))

    HRaDeX::plot_cov_class(state_2_params(),
                           fractional = fractional(),
                           interactive = T)

  })

  ## TAB: UPTAKE CURVES ##

  peptide_list <- reactive({

    HRaDeX::get_peptide_list(rbind(state_1_uc(), state_1_uc()))

  })

  output[["peptide_list_data"]] <- DT::renderDataTable({

    validate(need(!is.null(state_1_params()) & !is.null(state_2_params()), "Please upload necessary files."))

    DT::datatable(data = peptide_list(),
              class = "table-bordered table-condensed",
              extensions = "Buttons",
              selection = "single",
              options = list(pageLength = 10, dom = "tip", autoWidth = TRUE, target = 'cell'),
              filter = "bottom",
              rownames = FALSE)

  })

  peptide_list_proxy <- DT::dataTableProxy("peptide_list_data", session = session)

  ##

  observeEvent(input[["reset_peptide_list"]], {

    DT::selectRows(peptide_list_proxy, NULL)

  })


  output[["uc_plot"]] <- ggiraph::renderGirafe({

    validate(need(!is.null(input[["peptide_list_data_rows_selected"]]), "Please select a peptide on the left in the `UC data` section." ))

    tmp_fit_1 <- dplyr::filter(state_1_uc(),
                               Sequence == peptide_list()[[input[["peptide_list_data_rows_selected"]], "Sequence"]],
                               Start == peptide_list()[[input[["peptide_list_data_rows_selected"]], "Start"]],
                               End == peptide_list()[[input[["peptide_list_data_rows_selected"]], "End"]])

    tmp_fit_2 <- dplyr::filter(state_2_uc(),
                               Sequence == peptide_list()[[input[["peptide_list_data_rows_selected"]], "Sequence"]],
                               Start == peptide_list()[[input[["peptide_list_data_rows_selected"]], "Start"]],
                               End == peptide_list()[[input[["peptide_list_data_rows_selected"]], "End"]])

    plt <- HRaDeX::plot_uc(tmp_fit_1,
                    tmp_fit_2,
                    state_1_params(),
                    state_2_params(),
                    fractional = fractional(),
                    interactive = T)

    ggiraph::girafe_options(plt,
                            ggiraph::opts_zoom(min = .7, max = 2) )

  })

  ## TAB: STRUCTURE ##

  protein_structure <- reactive({

    validate(need(!is.null(input[["pdb_file"]]), "Please provide pdb file to see the 3D structure."))

    HRaDeX::plot_3d_structure_blank(pdb_file_path = input[["pdb_file"]][["datapath"]])

  })

  protein_structure_out <- reactive({

    # browser()

    color_positions <- c(0)
    if(input[["str_show_color_dist"]]){
      color_positions <- HRaDeX::prepare_diff_data(two_states_dataset(),
                                                   "dist",
                                                   input[["threshold_color"]])
    }

    uc_positions <- c(0)
    if(input[["str_show_uc_dist"]]){

      uc_value <- if(input[["is_diff_fractional"]]) {"frac_uptake_diff"} else {"uptake_diff"}
      uc_positions <- HRaDeX::prepare_diff_data(uc_diff_dataset(),
                                                uc_value,
                                                input[["threshold_uc"]])
    }

    r3dmol::m_set_style(protein_structure(),
                        sel = r3dmol::m_sel(resi = color_positions),
                        style = r3dmol::m_style_cartoon(color = "aquamarine")) |>
      r3dmol::m_set_style(sel = r3dmol::m_sel(resi = uc_positions),
                          style = r3dmol::m_style_cartoon(color = "deeppink")) |>
      r3dmol::m_set_style(sel = r3dmol::m_sel(resi = intersect(color_positions, uc_positions)),
                          style = r3dmol::m_style_cartoon(color = "darkorange"))


  })



  output[["protein_structure"]] <- r3dmol::renderR3dmol({


      r3dmol::m_button_spin(protein_structure_out())

  })


}
