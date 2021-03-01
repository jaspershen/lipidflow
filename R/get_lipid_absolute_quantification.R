# # library(tidyverse)
# # sxtTools::setwd_project()
# # setwd("demo_data/one_step/")
# 
# library(plyr)
# setwd("/Users/shenxt/projects/chuchu/chuchu/data/lipid20210116/absolute_quantification")
# 
# path = "."
# is_info_name_pos = "IS_information.xlsx"
# is_info_name_neg = "IS_information.xlsx"
# use_manual_is_info = FALSE
# lipid_annotation_table_pos = "020121_LP1_Align_HCD_Pos.xlsx"
# lipid_annotation_table_neg = "020121_LP1_Align_HCD_Neg.xlsx"
# chol_rt = 1169
# output_integrate = TRUE
# forced_targeted_peak_table_name = NULL
# fit.gaussian = TRUE
# integrate_xcms = TRUE
# output_eic = TRUE
# ppm = 40
# rt.tolerance = 180
# threads = 3
# rerun = FALSE
# which_group_for_rt_confirm = "QC"
# match_item_pos =
#   list(
#     "Cer" = "d18:1 (d7)-15:0 Cer",
#     "ChE" = c("18:1(d7) Chol Ester", "Cholesterol (d7)"),
#     "Chol" = "Cholesterol (d7)",
#     "DG" = "15:0-18:1(d7) DAG",
#     "LPC" = "18:1(d7) Lyso PC",
#     "LPE" = "18:1(d7) Lyso PE",
#     "MG" = "18:1 (d7) MG",
#     "PA" = "15:0-18:1(d7) PA (Na Salt)",
#     "PC" = "15:0-18:1(d7) PC",
#     "PE" = "15:0-18:1(d7) PE",
#     "PG" = "15:0-18:1(d7) PG (Na Salt)",
#     "PI" = "15:0-18:1(d7) PI (NH4 Salt)",
#     "PPE" = "C18(Plasm)-18:1(d9) PE",
#     "PS" = "15:0-18:1(d7) PS (Na Salt)",
#     "SM" = "d18:1-18:1(d9) SM",
#     "TG" = "15:0-18:1(d7)-15:0 TAG"
#   )
# match_item_neg =
#   list(
#     "Cer" = "d18:1 (d7)-15:0 Cer",
#     "Chol" = "Cholesterol (d7)",
#     "ChE" = c("18:1(d7) Chol Ester", "Cholesterol (d7)"),
#     "LPC" = "18:1(d7) Lyso PC",
#     "LPE" = "18:1(d7) Lyso PE",
#     "PC" = "15:0-18:1(d7) PC",
#     "PE" = "15:0-18:1(d7) PE",
#     "PG" = "15:0-18:1(d7) PG (Na Salt)",
#     "PI" = "15:0-18:1(d7) PI (NH4 Salt)",
#     "PPE" = "C18(Plasm)-18:1(d9) PE",
#     "PS" = "15:0-18:1(d7) PS (Na Salt)",
#     "SM" = "d18:1-18:1(d9) SM"
#   )
# 
# get_lipid_absolute_quantification(
#   path = path,
#   is_info_name_pos = is_info_name_pos,
#   is_info_name_neg = is_info_name_neg,
#   use_manual_is_info = use_manual_is_info,
#   lipid_annotation_table_pos = lipid_annotation_table_pos,
#   lipid_annotation_table_neg = lipid_annotation_table_neg,
#   chol_rt = chol_rt,
#   output_integrate = output_integrate,
#   forced_targeted_peak_table_name = forced_targeted_peak_table_name,
#   fit.gaussian = fit.gaussian,
#   integrate_xcms = integrate_xcms,
#   output_eic = FALSE,
#   ppm = ppm,
#   rt.tolerance = rt.tolerance,
#   threads = threads,
#   rerun = rerun,
#   which_group_for_rt_confirm = which_group_for_rt_confirm,
#   match_item_pos = match_item_pos,
#   match_item_neg = match_item_neg
# )


#' @title  get_lipid_absolute_quantification
#' @description get_lipid_absolute_quantification
#' @author Xiaotao Shen
#' @param path path.
#' @param is_info_name_pos is_info_name_pos
#' @param is_info_name_neg is_info_name_neg
#' @param use_manual_is_info use_manual_is_info
#' @param lipid_annotation_table_pos lipid_annotation_table_pos
#' @param lipid_annotation_table_neg lipid_annotation_table_neg
#' @param chol_rt chol_rt
#' @param output_integrate output_integrate
#' @param forced_targeted_peak_table_name forced_targeted_peak_table_name
#' @param fit.gaussian fit.gaussian
#' @param integrate_xcms integrate_xcms
#' @param output_eic output_eic
#' @param ppm ppm
#' @param rt.tolerance rt.tolerance
#' @param threads threads
#' @param rerun rerun
#' @param which_group_for_rt_confirm which_group_for_rt_confirm
#' @param match_item_pos match_item_pos
#' @param match_item_neg match_item_neg
#' @return Peak plot for each internal standard.
#' @importFrom magrittr %>%
#' @export


get_lipid_absolute_quantification <-
  function(path = ".",
           is_info_name_pos = "IS_information.xlsx",
           is_info_name_neg = "IS_information.xlsx",
           use_manual_is_info = FALSE,
           lipid_annotation_table_pos = "lipid_annotation_table_pos.xlsx",
           lipid_annotation_table_neg = "lipid_annotation_table_neg.xlsx",
           chol_rt = 1169,
           output_integrate = TRUE,
           forced_targeted_peak_table_name = NULL,
           fit.gaussian = TRUE,
           integrate_xcms = TRUE,
           output_eic = TRUE,
           ppm = 40,
           rt.tolerance = 180,
           threads = 3,
           rerun = FALSE,
           which_group_for_rt_confirm = "QC",
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
    
    ###is_info_name_pos and lipid_annotation_table_pos
    if (all(dir(file.path(path, "POS")) != is_info_name_pos)) {
      stop(is_info_name_pos, " is not in directory ", file.path(path, "POS"))
    }
    
    if (all(dir(file.path(path, "POS")) != lipid_annotation_table_pos)) {
      stop(lipid_annotation_table_pos, " is not in directory ", file.path(path, "POS"))
    }
    
    ###is_info_name_neg and lipid_annotation_table_neg
    if (all(dir(file.path(path, "NEG")) != is_info_name_neg)) {
      stop(is_info_name_neg, " is not in directory ", file.path(path, "NEG"))
    }
    
    if (all(dir(file.path(path, "NEG")) != lipid_annotation_table_neg)) {
      stop(lipid_annotation_table_neg, " is not in directory ", file.path(path, "NEG"))
    }
    
    ##mzXML data
    pos_mzxml = list.files(
      path = file.path(path, "POS"),
      pattern = "mzXML",
      all.files = TRUE,
      recursive = TRUE
    )
    
    neg_mzxml = list.files(
      path = file.path(path, "NEG"),
      pattern = "mzXML",
      all.files = TRUE,
      recursive = TRUE
    )
    
    if (length(grep("mzXML", pos_mzxml)) == 0 &
        length(grep("mzXML", neg_mzxml)) == 0) {
      stop('No mzXML data in POS or NEG folder.')
    }
    
    ##which_group_for_rt_confirm
    if(all(dir(file.path(path, "POS/")) != which_group_for_rt_confirm) |
       all(dir(file.path(path, "NEG/")) != which_group_for_rt_confirm)){
      stop(which_group_for_rt_confirm, " is not in POS or NEG folder.")
    }
    
    
    ############################################################################
    #############################get the RTs of each IS#########################
    ############################################################################
    cat(crayon::green("-------------------------------------------------------------------\n"))
    cat(crayon::green("Get retention times of all Internal standards...\n"))
    cat(crayon::green("-------------------------------------------------------------------\n"))
    
    ##get the RTs of internal standard in which_group_for_rt_confirm
    if (use_manual_is_info) {
      is_info_table_new_pos =
        readxl::read_xlsx(file.path(path, "POS", is_info_name_pos))
      
      is_info_table_new_neg =
        readxl::read_xlsx(file.path(path, "NEG", is_info_name_neg))
    } else{
      ###positive mode
      is_info_table <-
        readxl::read_xlsx(file.path(path, "POS", is_info_name_pos))
      
      is_info_table_new_pos =
        get_IS_RT(
          path = file.path(path, "POS", which_group_for_rt_confirm),
          is_info_table = is_info_table,
          polarity = "positive",
          threads = threads,
          rerun = rerun, 
          output_eic = output_eic
        )
      
      openxlsx::write.xlsx(
        is_info_table_new_pos,
        file = file.path(path, "POS/IS_info_new.xlsx"),
        asTable = TRUE
      )
      
      
      ###negative mode
      is_info_table <-
        readxl::read_xlsx(file.path(path, "NEG", is_info_name_neg))
      
      is_info_table_new_neg =
        get_IS_RT(
          path = file.path(path, "NEG", which_group_for_rt_confirm),
          is_info_table = is_info_table,
          polarity = "negative",
          threads = threads,
          rerun = rerun,
          output_eic = output_eic
        )
      
      openxlsx::write.xlsx(
        is_info_table_new_neg,
        file = file.path(path, "NEG/IS_info_new.xlsx"),
        asTable = TRUE
      )
    }
    
    dir.create(file.path(path, "Result"))
    dir.create(file.path(path, "Result/intermediate_data"))
    
    is_info_table_new =
      is_info_table_new_pos %>%
      dplyr::left_join(is_info_table_new_neg[, c("name", "rt_neg_second", "rt_neg_min", "adduct_neg", "mz_neg")],
                       by = "name")
    
    openxlsx::write.xlsx(is_info_table_new,
                         file = file.path(path, "Result", "IS_info_table.xlsx"))
    
    
    ###is there a chol in is_info_table
    if(any(is_info_table_new$name == "Cholesterol")){
      idx = which(is_info_table_new$name == "Cholesterol")
      chol_rt2 = c(is_info_table_new$rt_pos_second[idx], is_info_table_new$rt_neg_second[idx])
      chol_rt2 = chol_rt2[!is.na(chol_rt2)]
      if(length(chol_rt2) > 1){
        chol_rt = chol_rt2[1]
      }
    }
    
############################################################################
#############################relative quantification of IS##################
############################################################################
cat(crayon::green("-------------------------------------------------------------------\n"))
cat(crayon::green("Get relative quantification tables...\n"))
cat(crayon::green("-------------------------------------------------------------------\n"))
###positive mode
##get sample information

sample_info_pos =
      pos_mzxml %>%
      stringr::str_split(pattern = "\\/") %>%
      do.call(rbind, .) %>%
      as.data.frame()
    colnames(sample_info_pos) = c("group", "sample.name")
    sample_info_pos = 
      sample_info_pos[,c(2,1)]
    
    sample_info_pos$sample.name =
      stringr::str_replace(sample_info_pos$sample, "\\.mzXML", "")
    ###Internal standard
    cat(crayon::green("Internal standard positive mode...\n"))
    get_relative_quantification(
      path = file.path(path, "POS/"),
      output_path_name = "is_relative_quantification",
      targeted_table_name = "IS_info_new.xlsx",
      sample_info = sample_info_pos,
      targeted_table_type = "is",
      polarity = "positive",
      chol_rt = chol_rt,
      output_integrate = output_integrate,
      forced_targeted_peak_table_name = forced_targeted_peak_table_name,
      fit.gaussian = fit.gaussian,
      integrate_xcms = integrate_xcms,
      output_eic = output_eic,
      ppm = ppm,
      rt.tolerance = rt.tolerance,
      threads = threads,
      rerun = rerun
    )
    
    ###lipid
    cat(crayon::green("Lipid positive mode...\n"))
    get_relative_quantification(
      path = file.path(path, "POS"),
      output_path_name = "lipid_relative_quantification",
      targeted_table_name = lipid_annotation_table_pos,
      sample_info = sample_info_pos,
      targeted_table_type = "lipid",
      polarity = "positive",
      chol_rt = chol_rt,
      output_integrate = output_integrate,
      forced_targeted_peak_table_name = forced_targeted_peak_table_name,
      fit.gaussian = fit.gaussian,
      integrate_xcms = integrate_xcms,
      output_eic = output_eic,
      ppm = ppm,
      rt.tolerance = rt.tolerance,
      threads = threads,
      rerun = rerun
    )


    ###negative mode
    ##get sample information
    sample_info_neg =
      neg_mzxml %>%
      stringr::str_split(pattern = "\\/") %>%
      do.call(rbind, .) %>%
      as.data.frame()
    colnames(sample_info_neg) = c("group", "sample.name")
    sample_info_neg = 
      sample_info_neg[,c(2,1)]
    
    sample_info_neg$sample.name =
      stringr::str_replace(sample_info_neg$sample, "\\.mzXML", "")
    
    ###Internal standard
    cat(crayon::green("Internal standard negative mode...\n"))
    get_relative_quantification(
      path = file.path(path, "NEG/"),
      output_path_name = "is_relative_quantification",
      targeted_table_name = "IS_info_new.xlsx",
      sample_info = sample_info_neg,
      targeted_table_type = "is",
      polarity = "negative",
      chol_rt = chol_rt,
      output_integrate = output_integrate,
      forced_targeted_peak_table_name = forced_targeted_peak_table_name,
      fit.gaussian = fit.gaussian,
      integrate_xcms = integrate_xcms,
      output_eic = output_eic,
      ppm = ppm,
      rt.tolerance = rt.tolerance,
      threads = threads,
      rerun = rerun
    )
    
    ###lipid
    cat(crayon::green("Lipid negative mode...\n"))
    get_relative_quantification(
      path = file.path(path, "NEG"),
      output_path_name = "lipid_relative_quantification",
      targeted_table_name = lipid_annotation_table_neg,
      sample_info = sample_info_neg,
      targeted_table_type = "lipid",
      polarity = "negative",
      chol_rt = chol_rt,
      output_integrate = output_integrate,
      forced_targeted_peak_table_name = forced_targeted_peak_table_name,
      fit.gaussian = fit.gaussian,
      integrate_xcms = integrate_xcms,
      output_eic = output_eic,
      ppm = ppm,
      rt.tolerance = rt.tolerance,
      threads = threads,
      rerun = rerun
    )
    
  
    
    
    
    
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
    absolute_data_neg = get_absolute_quantification(
      path = file.path(path, "NEG"),
      is_quantification_table = is_quantification_table,
      lipid_quantification_table = lipid_quantification_table,
      sample_info = sample_info_neg,
      match_item = match_item_neg
    )
      
  ##combine positive and negative
    load(file.path(path, "POS/absolute_quantification/variable_info_abs"))
    variable_info_abs_pos <- variable_info_abs
    
    load(file.path(path, "POS/absolute_quantification/express_data_abs_ug_ml"))
    express_data_abs_ug_ml_pos <-
      express_data_abs_ug_ml
    
    load(file.path(path, "POS/absolute_quantification/express_data_abs_um"))
    express_data_abs_um_pos <-
      express_data_abs_um
    
    load(file.path(path, "NEG/absolute_quantification/variable_info_abs"))
    variable_info_abs_neg <- variable_info_abs
    
    load(file.path(path, "NEG/absolute_quantification/express_data_abs_ug_ml"))
    express_data_abs_ug_ml_neg <-
      express_data_abs_ug_ml
    
    load(file.path(path, "NEG/absolute_quantification/express_data_abs_um"))
    express_data_abs_um_neg <-
      express_data_abs_um

    combine_pos_neg_quantification(
      path = file.path(path, "Result"),
      express_data_abs_ug_ml_pos = express_data_abs_ug_ml_pos,
      express_data_abs_um_pos = express_data_abs_um_pos,
      variable_info_abs_pos = variable_info_abs_pos,
      express_data_abs_ug_ml_neg = express_data_abs_ug_ml_neg,
      express_data_abs_um_neg = express_data_abs_um_neg,
      variable_info_abs_neg = variable_info_abs_neg
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
    
    lipid_data <-
      readxl::read_xlsx(file.path(path, "Result/lipid_data_um.xlsx"))
    
    load(file.path(path, "POS/is_table"))
    is_table_pos <- is_table
    
    load(file.path(path, "POS/is_tag"))
    is_tag_pos <- is_tag
    
    load(file.path(path, "POS/lipid_table"))
    lipid_table_pos <- lipid_table
    
    load(file.path(path, "POS/lipid_tag"))
    lipid_tag_pos <- lipid_tag
    
    load(file.path(path, "POS/express_data_abs_ug_ml"))
    express_data_abs_ug_ml_pos <- express_data_abs_ug_ml
    
    load(file.path(path, "POS/express_data_abs_um"))
    express_data_abs_um_pos <- express_data_abs_um
    
    load(file.path(path, "POS/variable_info_abs"))
    variable_info_abs_pos <- variable_info_abs
    
    variable_info_abs_pos <-
      variable_info_abs_pos %>%
      dplyr::filter(peak_name %in% lipid_data$peak_name)
    
    express_data_abs_ug_ml_pos <-
      express_data_abs_ug_ml_pos[match(variable_info_abs_pos$peak_name,
                                       rownames(express_data_abs_ug_ml_pos)),]
    
    express_data_abs_um_pos <-
      express_data_abs_um_pos[match(variable_info_abs_pos$peak_name,
                                    rownames(express_data_abs_um_pos)),]
    
    rownames(lipid_table_pos) <- lipid_tag_pos$peak_name
    
    lipid_tag_pos <-
      lipid_tag_pos %>%
      dplyr::filter(peak_name %in% lipid_data$peak_name)
    
    lipid_table_pos <-
      lipid_table_pos[match(lipid_tag_pos$peak_name, rownames(lipid_table_pos)),]
    
    ###negative mode
    load(file.path(path, "NEG/is_table"))
    is_table_neg <- is_table
    
    load(file.path(path, "NEG/is_tag"))
    is_tag_neg <- is_tag
    
    load(file.path(path, "NEG/lipid_table"))
    lipid_table_neg <- lipid_table
    
    load(file.path(path, "NEG/lipid_tag"))
    lipid_tag_neg <- lipid_tag
    
    load(file.path(path, "NEG/express_data_abs_ug_ml"))
    express_data_abs_ug_ml_neg <- express_data_abs_ug_ml
    
    load(file.path(path, "NEG/express_data_abs_um"))
    express_data_abs_um_neg <- express_data_abs_um
    
    load(file.path(path, "NEG/variable_info_abs"))
    variable_info_abs_neg <- variable_info_abs
    
    variable_info_abs_neg <-
      variable_info_abs_neg %>%
      dplyr::filter(peak_name %in% lipid_data$peak_name)
    
    express_data_abs_ug_ml_neg <-
      express_data_abs_ug_ml_neg[match(variable_info_abs_neg$peak_name,
                                       rownames(express_data_abs_ug_ml_neg)),]
    
    express_data_abs_um_neg <-
      express_data_abs_um_neg[match(variable_info_abs_neg$peak_name,
                                    rownames(express_data_abs_um_neg)),]
    
    rownames(lipid_table_neg) <- lipid_tag_neg$peak_name
    
    lipid_tag_neg <-
      lipid_tag_neg %>%
      dplyr::filter(peak_name %in% lipid_data$peak_name)
    
    lipid_table_neg <-
      lipid_table_neg[match(lipid_tag_neg$peak_name, rownames(lipid_table_neg)),]
    
    ###output results
    dir.create(file.path(path, "Result/intensity_plot"),
               showWarnings = FALSE)
    
    ###positive
    for (i in names(match_item_pos)) {
      cat(i, " ")
      is_name <-
        match_item_pos[[i]]
      
      if (is.na(unique(is_name)[1])) {
        next()
      }
      
      dir.create(file.path(path, "Result/intensity_plot", i),
                 showWarnings = FALSE)
      
      temp_idx <-
        which(lipid_tag_pos$Class == i)
      
      if (length(temp_idx) > 0) {
        for (j in temp_idx) {
          temp_is_name <-
            abs(lipid_tag_pos$rt[j] -
                  is_tag_pos$rt[match(is_name, is_tag_pos$name)]) %>%
            which.min() %>%
            head(1) %>%
            `[`(is_name, .)
          
          temp_data1 <-
            is_table_pos[match(temp_is_name, is_tag_pos$name),] %>%
            t() %>%
            as.data.frame() %>%
            tibble::rownames_to_column(var = "sample_id")
          
          colnames(temp_data1)[2] <- "intensity"
          
          ###raw data
          temp_name <- lipid_tag_pos$name[j]
          temp_data2 <-
            lipid_table_pos[j, ] %>%
            t() %>%
            as.data.frame() %>%
            tibble::rownames_to_column(var = "sample_id")
          
          colnames(temp_data2)[2] <- "intensity"
          
          
          temp_data3 <-
            express_data_abs_um_pos[match(temp_name, variable_info_abs_pos$name), ] %>%
            t() %>%
            as.data.frame() %>%
            tibble::rownames_to_column(var = "sample_id")
          
          colnames(temp_data3)[2] <- "intensity"
          
          temp_data <-
            rbind(
              data.frame(temp_data1, from = "is"),
              data.frame(temp_data2, from = "raw"),
              data.frame(temp_data3, from = "final")
            ) %>%
            dplyr::mutate(from = factor(from, levels = c("is", "raw", "final")))
          
          temp_plot <-
            temp_data %>%
            ggplot2::ggplot(ggplot2::aes(sample_id, intensity)) +
            ggplot2::geom_point(size = 4, shape = 21, ggplot2::aes(fill = from)) +
            ggrepel::geom_text_repel(aes(label = round(intensity, 4))) +
            ggsci::scale_fill_d3() +
            ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.5, 0.5))) +
            ggplot2::theme_bw() +
            ggplot2::labs(x = "", y = "Intensity") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
              angle = 45,
              hjust = 1,
              vjust = 1
            )) +
            ggplot2::facet_wrap(facets = vars(from),
                       ncol = 1,
                       scales = "free_y")
          
          temp_name <- paste(temp_name, ".pdf", sep = "")
          temp_name <-
            temp_name %>% 
            stringr::str_replace_all("\\/", "_") %>% 
            stringr::str_replace_all("\\:", "_")
          ggplot2::ggsave(
            temp_plot,
            filename = file.path(path, "Result/intensity_plot", i, temp_name),
            width = 14,
            height = 14
          )
        }
      }
    }
    
    
    ###negative
    for (i in names(match_item_neg)) {
      cat(i, " ")
      is_name <-
        match_item_neg[[i]]
      
      if (is.na(unique(is_name)[1])) {
        next()
      }
      
      dir.create(file.path(path, "Result/intensity_plot", i),
                 showWarnings = FALSE)
      
      temp_idx <-
        which(lipid_tag_neg$Class == i)
      
      if (length(temp_idx) > 0) {
        for (j in temp_idx) {
          temp_is_name <-
            abs(lipid_tag_neg$rt[j] -
                  is_tag_neg$rt[match(is_name, is_tag_neg$name)]) %>%
            which.min() %>%
            head(1) %>%
            `[`(is_name, .)
          
          temp_data1 <-
            is_table_neg[match(temp_is_name, is_tag_neg$name),] %>%
            t() %>%
            as.data.frame() %>%
            tibble::rownames_to_column(var = "sample_id")
          
          colnames(temp_data1)[2] <- "intensity"
          
          ###raw data
          temp_name <- lipid_tag_neg$name[j]
          temp_data2 <-
            lipid_table_neg[j, ] %>%
            t() %>%
            as.data.frame() %>%
            tibble::rownames_to_column(var = "sample_id")
          
          colnames(temp_data2)[2] <- "intensity"
          
          
          temp_data3 <-
            express_data_abs_um_neg[match(temp_name, variable_info_abs_neg$name), ] %>%
            t() %>%
            as.data.frame() %>%
            tibble::rownames_to_column(var = "sample_id")
          
          colnames(temp_data3)[2] <- "intensity"
          
          temp_data <-
            rbind(
              data.frame(temp_data1, from = "is"),
              data.frame(temp_data2, from = "raw"),
              data.frame(temp_data3, from = "final")
            ) %>%
            dplyr::mutate(from = factor(from, levels = c("is", "raw", "final")))
          
          temp_plot <-
            temp_data %>%
            ggplot(aes(sample_id, intensity)) +
            geom_point(size = 4, shape = 21, aes(fill = from)) +
            ggrepel::geom_text_repel(aes(label = round(intensity, 4))) +
            ggsci::scale_fill_d3() +
            scale_y_continuous(expand = expansion(mult = c(0.5, 0.5))) +
            theme_bw() +
            labs(x = "", y = "Intensity") +
            theme(axis.text.x = element_text(
              angle = 45,
              hjust = 1,
              vjust = 1
            )) +
            facet_wrap(facets = vars(from),
                       ncol = 1,
                       scales = "free_y")
          
          temp_name <- paste(temp_name, ".pdf", sep = "")
          temp_name <-
            temp_name %>% 
            stringr::str_replace_all("\\/", "_") %>% 
            stringr::str_replace_all("\\:", "_")
          ggsave(
            temp_plot,
            filename = file.path(path, "Result/intensity_plot", i, temp_name),
            width = 14,
            height = 14
          )
        }
      }
    }
    
    #####Class plot
    dir.create(file.path(path, "Result/class_plot"))
    
    ####each class
    lipid_data_class <-
      readxl::read_xlsx(file.path(path, "Result/lipid_data_class_um.xlsx"))
    
    for (i in 1:nrow(lipid_data_class)) {
      cat(i, " ")
      temp_data <-
        lipid_data_class[i, -1] %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "sample_id")
      colnames(temp_data)[2] <- "intensity"
      
      temp_plot <-
        temp_data %>%
        ggplot(aes(sample_id, intensity)) +
        geom_point(size = 4,
                   shape = 21,
                   fill = "orange") +
        ggrepel::geom_text_repel(aes(label = round(intensity, 4))) +
        ggsci::scale_fill_d3() +
        scale_y_continuous(expand = expansion(mult = c(0.5, 0.5))) +
        theme_bw() +
        labs(x = "", y = "Intensity") +
        theme(axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 1
        ))
      
      ggsave(
        temp_plot,
        filename = file.path(path,
          "Result/class_plot",
          paste(lipid_data_class$Class[i] %>% 
                  stringr::str_replace_all("\\/", "_") %>% 
                  stringr::str_replace_all("\\:", "_"), 
                ".pdf",
                sep = "")
        ),
        width = 7,
        height = 7
      )
    }
    cat("\n")
    cat(crayon::bgRed('All done.\n'))
  }
