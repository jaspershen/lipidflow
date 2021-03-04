#' @title get_relative_quantification
#' @description get_IS_RT
#' @author Xiaotao Shen
#' @param path path.
#' @param output_path_name output_path_name
#' @param targeted_table_name targeted_table_name
#' @param sample_info sample_info
#' @param targeted_table_type targeted_table_type
#' @param polarity polarity
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
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export

# # tinyTools::setwd_project()
# get_relative_quantification(
#   path = "example/POS",
#   forced_targeted_peak_table_name = "forced_targeted_peak_table_temple_manual.xlsx",
#   output_path_name = "is_relative_quantification",
#   targeted_table_name = "IS_info_new.xlsx",
#   sample_info = sample_info_pos,
#   targeted_table_type = "is",
#   polarity = "positive",
#   chol_rt = chol_rt,
#   output_integrate = TRUE,
#   output_eic = TRUE,
#   ppm = 40,
#   rt.tolerance = 180,
#   threads = 5,
#   rerun = FALSE
# )

get_relative_quantification <-
  function(path = ".",
           output_path_name = "Result",
           targeted_table_name,
           sample_info,
           targeted_table_type = c("is", "lipid"),
           polarity = c("positive", "negative"),
           chol_rt = 1169,
           output_integrate = TRUE,
           forced_targeted_peak_table_name = NULL,
           fit.gaussian = TRUE,
           integrate_xcms = TRUE,
           output_eic = TRUE,
           ppm = 40,
           rt.tolerance = 180,
           threads = 3,
           rerun = FALSE) {
    polarity = match.arg(polarity)
    targeted_table_type = match.arg(targeted_table_type)
    ##check data
    if (all(dir(path) != targeted_table_name)) {
      stop(targeted_table_name, " is not in directory ", path)
    }
    
    mzxml_data = list.files(
      path = path,
      pattern = "mzXML",
      all.files = TRUE,
      recursive = TRUE,
      full.names = FALSE
    ) %>% basename()
    
    if (length(mzxml_data) == 0) {
      stop('No mzXML data in ', path)
    }
    
    output_path = file.path(path, output_path_name)
    dir.create(output_path, showWarnings = FALSE)
    dir.create(file.path(output_path, "intermediate_data"))
    
    if (rerun) {
      unlink(
        file.path(output_path, "intermediate_data"),
        recursive = TRUE,
        force = TRUE
      )
    }
    
    if (length(setdiff(
      mzxml_data,
      paste(sample_info$sample.name, "mzXML", sep = ".")
    )) > 0 |
    length(setdiff(
      paste(sample_info$sample.name, "mzXML", sep = "."),
      mzxml_data
    )) > 0) {
      stop("mzXML data in ",
           path,
           " and samples in ",
           sample_info,
           " are different.")
    }
    
    if (targeted_table_type == "is") {
      targeted_table = 
        suppressMessages(readxl::read_xlsx(file.path(path, targeted_table_name)))
      targeted_table = trans_is_table(is_table = targeted_table,
                                      polarity = polarity)
      openxlsx::write.xlsx(targeted_table,
                           file.path(path, "feature_table.xlsx"),
                           asTable = TRUE)
    } else{
      targeted_table =
        suppressMessages(readxl::read_xlsx(
          file.path(path, targeted_table_name),
          sheet = 1,
          col_names = FALSE
        ))
      
      lipid_table =
        tidy_lipidsearch_data(file = targeted_table,
                              polarity = polarity,
                              from = "lipidsearch")
      
      lipid_table <-
        clean_lipid_data(x = lipid_table)
      
      if (polarity == "positive") {
        ###add Chol only for positive mode
        chol_matrix <-
          data.frame(
            peak_name = "Cholesterol",
            mz = 404.3892,
            rt = chol_rt / 60,
            name = "Cholesterol",
            lipid_raw_name = NA,
            adduct = "NH4",
            polarity = "positive",
            Class = "Chol",
            FattyAcid = NA,
            IonFormula = NA,
            FA1 = NA,
            FA2 = NA,
            FA3 = NA,
            FA4 = NA,
            mean.int = 100000
          )
        
        sample_matrix <-
          matrix(NA,
                 nrow = 1,
                 ncol = lipid_table %>%
                   dplyr::select(-c(peak_name:mean.int)) %>%
                   ncol()) %>%
          as.data.frame()
        
        colnames(sample_matrix) <-
          lipid_table %>%
          dplyr::select(-c(peak_name:mean.int)) %>%
          colnames()
        
        lipid_table <-
          rbind(lipid_table,
                cbind(chol_matrix, sample_matrix))
      }
      
      feature_info = lipid_table
      
      lipid_table <-
        lipid_table %>%
        dplyr::select(name = peak_name,
                      mz,
                      rt,
                      adduct,
                      Class,
                      compound_name = name) %>%
        dplyr::mutate(rt = rt * 60)
      
      openxlsx::write.xlsx(lipid_table,
                           file.path(path, "feature_table.xlsx"),
                           asTable = TRUE)
      
    }
    
    # openxlsx::write.xlsx(targeted_table, file = file.path(path, "feature_table.xlsx"))
    
    extract_targeted_peaks(
      path = path,
      output_path_name = output_path_name,
      targeted_targeted_peak_table_name = "feature_table.xlsx",
      forced_targeted_peak_table_name = forced_targeted_peak_table_name,
      fit.gaussian = fit.gaussian,
      integrate_xcms = integrate_xcms,
      output_eic = output_eic,
      output_integrate = output_integrate,
      ppm = ppm,
      rt.tolerance = rt.tolerance,
      from_lipid_search = ifelse(targeted_table_type == "lipid", TRUE, FALSE),
      facet = TRUE
    )
    
    cat("\n")
    unlink(file.path(path, "lipid_table.xlsx"),
           recursive = TRUE,
           force = TRUE)
    
    unlink(file.path(path, "is_table.xlsx"),
           recursive = TRUE,
           force = TRUE)
    
    ##combine raw lipid table and our quantification table
    quantification_data <-
      readxl::read_xlsx(file.path(output_path, "quantification_table.xlsx"))
    
    if (targeted_table_type == "is") {
      feature_info <-
        readxl::read_xlsx(file.path(path, targeted_table_name))
    } else{
      feature_info =
        feature_info
    }
    
    quantification_data[is.na(quantification_data)] <- 0
    
    quantification_data =
      combine_quantification_data_and_feature_info(
        quantification_data = quantification_data,
        feature_info = feature_info,
        targeted_table_type = targeted_table_type
      )
    
    openxlsx::write.xlsx(quantification_data,
                         file = file.path(
                           output_path,
                           paste(targeted_table_type,
                                 "quantification_table.xlsx", sep = "_")
                         ),
                         asTable = TRUE)
    
    ###decide which peak should be checked
    check_sample_feature =
      unique(sample_info$group) %>%
      purrr::map(function(x) {
        temp =
          quantification_data[, sample_info$sample.name[sample_info$group ==
                                                          x]]
        
        if (ncol(temp) == 1) {
          return(
            data.frame(
              sample = paste(sample_info$sample.name[sample_info$group == x], collapse = "_"),
              feature = quantification_data$name,
              check = TRUE,
              stringsAsFactors = FALSE,
              check.names = FALSE
            ) %>%
              tidyr::separate_rows(sample)
          )
        }
        
        if (ncol(temp) == 2) {
          check_idx =
            apply(temp, 1, function(y) {
              y = as.numeric(y)
              difference = abs(y[1] - y[2]) / max(y)
              difference[is.na(difference)] = 1
              if (difference > 0.3) {
                return(TRUE)
              } else{
                return(FALSE)
              }
            }) %>%
            which()
          
          return(
            data.frame(
              sample = paste(sample_info$sample.name[sample_info$group == x], collapse = "_"),
              feature = quantification_data$name,
              check = TRUE,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )[check_idx, , drop = FALSE] %>%
              tidyr::separate_rows(sample)
          )
        }
        
        if (ncol(temp) > 2) {
          check_idx =
            apply(temp, 1, function(y) {
              y = as.numeric(y)
              rsd = sd(y) / mean(y)
              rsd[is.na(rsd)] = 1
              if (rsd > 0.3) {
                return(TRUE)
              } else{
                return(FALSE)
              }
            }) %>%
            which()
          
          return(
            data.frame(
              sample = paste(sample_info$sample.name[sample_info$group == x], collapse = "_"),
              feature = quantification_data$name,
              check = TRUE,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )[check_idx, , drop = FALSE] %>%
              tidyr::separate_rows(sample)
          )
        }
      }) %>%
      do.call(rbind, .) %>%
      as.data.frame()
    
    if (targeted_table_type == "lipid") {
      check_sample_feature$feature =
        feature_info$peak_name[match(check_sample_feature$feature, feature_info$name)]
    }
    
    forced_targeted_peak_table_temple =
      readxl::read_xlsx(file.path(output_path, "forced_targeted_peak_table_temple.xlsx"))
    
    forced_targeted_peak_table_temple =
      forced_targeted_peak_table_temple %>%
      dplyr::left_join(check_sample_feature,
                       by = c("sample" = "sample",
                              "name" = "feature"))
    
    openxlsx::write.xlsx(
      forced_targeted_peak_table_temple,
      file = file.path(output_path, "forced_targeted_peak_table_temple.xlsx"),
      asTable = TRUE
    )
    
    unlink(
      file.path(output_path, "quantification_table.xlsx"),
      recursive = TRUE,
      force = TRUE
    )
    
    unlink(file.path(path, "feature_table.xlsx"),
           recursive = TRUE,
           force = TRUE)
    
  }
