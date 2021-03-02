#' @title  organize result
#' @description organize result
#' @author Xiaotao Shen
#' @param path path.
#' @param match_item_pos match_item_pos
#' @param match_item_neg match_item_neg
#' @return Peak plot for each internal standard.
#' @importFrom magrittr %>%
#' @export

organize_result <-
  function(path = ".",
           match_item_pos =
             list(
               "Cer" = "d18:1 (d7)-15:0 Cer",
               "ChE" = c("18:1(d7) Chol Ester", "Cholesterol (d7)"),
               "Chol" = "Cholesterol (d7)",
               "DG" = "15:0-18:1(d7) DAG",
               "LPC" = "18:1(d7) Lyso PC",
               "LPE" = "18:1(d7) Lyso PE",
               "MG" = "18:1 (d7) MG",
               "PA" = "15:0-18:1(d7) PA (Na Salt)",
               "PC" = "15:0-18:1(d7) PC",
               "PE" = "15:0-18:1(d7) PE",
               "PG" = "15:0-18:1(d7) PG (Na Salt)",
               "PI" = "15:0-18:1(d7) PI (NH4 Salt)",
               "PPE" = "C18(Plasm)-18:1(d9) PE",
               "PS" = "15:0-18:1(d7) PS (Na Salt)",
               "SM" = "d18:1-18:1(d9) SM",
               "TG" = "15:0-18:1(d7)-15:0 TAG"
             ),
           match_item_neg =
             list(
               "Cer" = "d18:1 (d7)-15:0 Cer",
               "Chol" = "Cholesterol (d7)",
               "ChE" = c("18:1(d7) Chol Ester", "Cholesterol (d7)"),
               "LPC" = "18:1(d7) Lyso PC",
               "LPE" = "18:1(d7) Lyso PE",
               "PC" = "15:0-18:1(d7) PC",
               "PE" = "15:0-18:1(d7) PE",
               "PG" = "15:0-18:1(d7) PG (Na Salt)",
               "PI" = "15:0-18:1(d7) PI (NH4 Salt)",
               "PPE" = "C18(Plasm)-18:1(d9) PE",
               "PS" = "15:0-18:1(d7) PS (Na Salt)",
               "SM" = "d18:1-18:1(d9) SM"
             )
           ) {
    
    ##check data
    ##POS and NEG folder
    if (all(dir(path) != "POS") & all(dir(path) != "NEG")) {
      stop("POS or NEG folder", " is not in directory ", path)
    }

    dir.create(file.path(path, "Result"))
    dir.create(file.path(path, "Result/intermediate_data"))
 
    ############################################################################
    #############################absolute quantification#######################
    ############################################################################
    cat(crayon::green("-------------------------------------------------------------------\n"))
    cat(crayon::green("Get absolute quantification tables...\n"))
    cat(crayon::green("-------------------------------------------------------------------\n"))
    
    ###positive mode
    is_quantification_table =
      readxl::read_xlsx(file.path(
        path,
        "POS/is_relative_quantification/is_quantification_table.xlsx"
      ))
    
    lipid_quantification_table =
      readxl::read_xlsx(
        file.path(
          path,
          "POS/lipid_relative_quantification/lipid_quantification_table.xlsx"
        )
      )
    
    cat(crayon::green("Positive mode.\n"))
    sample_info_pos = generate_sample_info(path = file.path(path, "POS"))
    absolute_data_pos = get_absolute_quantification(
      path = file.path(path, "POS"),
      is_quantification_table = is_quantification_table,
      lipid_quantification_table = lipid_quantification_table,
      sample_info = sample_info_pos,
      match_item = match_item_pos
    )
    
    ###negative mode
    is_quantification_table =
      readxl::read_xlsx(file.path(
        path,
        "NEG/is_relative_quantification/is_quantification_table.xlsx"
      ))
    
    lipid_quantification_table = readxl::read_xlsx(
      file.path(
        path,
        "NEG/lipid_relative_quantification/lipid_quantification_table.xlsx"
      )
    )
    
    cat(crayon::green("negative mode.\n"))
    
    sample_info_neg = generate_sample_info(path = file.path(path, "NEG"))
    
    absolute_data_neg = get_absolute_quantification(
      path = file.path(path, "NEG"),
      is_quantification_table = is_quantification_table,
      lipid_quantification_table = lipid_quantification_table,
      sample_info = sample_info_neg,
      match_item = match_item_neg
    )
      
  ##combine positive and negative
    combine_pos_neg_quantification(
      path = file.path(path, "Result"),
      express_data_abs_ug_ml_pos = absolute_data_pos$express_data_abs_ug_ml,
      express_data_abs_um_pos = absolute_data_pos$express_data_abs_um,
      variable_info_abs_pos = absolute_data_pos$variable_info_abs,
      express_data_abs_ug_ml_neg = absolute_data_neg$express_data_abs_ug_ml,
      express_data_abs_um_neg = absolute_data_neg$express_data_abs_um,
      variable_info_abs_neg = absolute_data_neg$variable_info_abs
    )
    cat("\n")
    
    
    ############################################################################
    #############################reorganize plot################################
    ############################################################################
    cat(crayon::green("-------------------------------------------------------------------\n"))
    cat(crayon::green("Generate the peak plots for lipids...\n"))
    cat(crayon::green("-------------------------------------------------------------------\n"))
    
    ###positive mode
    absolute_table_pos <-
      readxl::read_xlsx(file.path(path, "Result/lipid_data_um.xlsx"))
    cat("positive mode...\n")
    reorganize_peak_plot(path = file.path(path, "POS/lipid_relative_quantification/"),
                         plot_dir = "peak_shape",
                         absolute_table = absolute_table_pos,
                         match_item = match_item_pos) 
    
    ###negative mode
    absolute_table_neg <-
      readxl::read_xlsx(file.path(path, "Result/lipid_data_um.xlsx"))
    cat(crayon::green("negative mode...\n"))
    reorganize_peak_plot(path = file.path(path, "NEG/lipid_relative_quantification/"),
                         plot_dir = "peak_shape",
                         absolute_table = absolute_table_neg,
                         match_item = match_item_pos)
    cat("\n")
    
    
    ############################################################################
    #############################output results################################
    ############################################################################
    cat(crayon::green("-------------------------------------------------------------------\n"))
    cat(crayon::green("Output results...\n"))
    cat(crayon::green("-------------------------------------------------------------------\n"))
    
    output_result(path = path,
                  match_item_pos = match_item_pos,
                  match_item_neg = match_item_neg)
    cat("\n")
    cat(crayon::bgRed('All done.\n'))
  }
