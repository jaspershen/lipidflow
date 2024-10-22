#' @title  get_IS_RT
#' @description Get the retention time of internal standards.
#' @author Xiaotao Shen
#' @param path path.
#' @param is_info_table is_info_table
#' @param polarity polarity
#' @param threads threads
#' @param rerun rerun
#' @param output_eic output_EIC or not.
#' @importFrom magrittr %>%
#' @export
# 
# tinyTools::setwd_project()
# setwd("vignettes/")
# 
# is_info_table_pos =
#   readxl::read_xlsx("example/POS/IS_information.xlsx")
# 
# is_info_table_new_pos =
# get_IS_RT(
#   path = "example/POS/D25",
#   is_info_table = is_info_table_pos,
#   polarity = "positive",
#   threads = 3,
#   rerun = TRUE,
#   output_eic = TRUE
# )


get_IS_RT <-
  function(path = ".",
           is_info_table,
           polarity = c("positive", "negative"),
           threads = 3,
           rerun = FALSE,
           output_eic = TRUE) {
    polarity = match.arg(polarity)
    ##check data
    if (length(grep("mzXML", dir(path))) == 0) {
      stop('No mzXML data in ', path)
    }
    
    mass <-
      is_info_table$exact.mass %>%
      as.character() %>%
      stringr::str_trim(side = "both") %>%
      as.numeric()
    
    is_info_table_raw = is_info_table
    
    if (polarity == "positive") {
      cat(crayon::green("Positive mode...\n"))
      mz_pos_h <-
        mass + Rdisop::getMass(molecule = Rdisop::getMolecule("H"))
      mz_pos_na <-
        mass + Rdisop::getMass(molecule = Rdisop::getMolecule("Na"))
      mz_pos_nh4 <-
        mass + Rdisop::getMass(molecule = Rdisop::getMolecule("NH4"))
      mz_pos_h_h20 <-
        mass - Rdisop::getMass(molecule = Rdisop::getMolecule("HO"))
      mz_pos_nh4_h20 <-
        mass + Rdisop::getMass(molecule = Rdisop::getMolecule("NH4")) -
        Rdisop::getMass(molecule = Rdisop::getMolecule("H2O"))
      
      is_info_table <- data.frame(
        is_info_table,
        mz_pos_h,
        mz_pos_na,
        mz_pos_nh4,
        mz_pos_h_h20,
        mz_pos_nh4_h20,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      is_info_table <-
        rbind(
          data.frame(
            name = is_info_table$name,
            mz = mz_pos_h,
            rt = 100,
            adduct = "M+H",
            stringsAsFactors = FALSE
          ),
          data.frame(
            name = is_info_table$name,
            mz = mz_pos_na,
            rt = 100,
            adduct = 'M+Na',
            stringsAsFactors = FALSE
          ),
          data.frame(
            name = is_info_table$name,
            mz = mz_pos_nh4,
            rt = 100,
            adduct = "M+NH4",
            stringsAsFactors = FALSE
          ),
          data.frame(
            name = is_info_table$name,
            mz = mz_pos_h_h20,
            rt = 100,
            adduct = "M+H-H2O",
            stringsAsFactors = FALSE
          ),
          data.frame(
            name = is_info_table$name,
            mz = mz_pos_nh4_h20,
            rt = 100,
            adduct = "M+NH4-H2O",
            stringsAsFactors = FALSE
          )
        ) %>%
        as.data.frame() %>%
        dplyr::arrange(name, adduct)
      
      is_info_table$name <-
        stringr::str_trim(is_info_table$name)
      
      is_info_table$mz <- as.numeric(is_info_table$mz)
      
      is_info_table$name = paste(is_info_table$name, is_info_table$adduct, sep = "_")
      
      openxlsx::write.xlsx(
        x = is_info_table,
        file = file.path(path, "is_info_table.xlsx"),
        asTable = TRUE
      )
      
    } else{
      cat(crayon::green("Negative mode...\n"))
      mz_neg_h <-
        mass - Rdisop::getMass(molecule = Rdisop::getMolecule("H"))
      mz_neg_ch3cOO <-
        mass + Rdisop::getMass(molecule = Rdisop::getMolecule("CH3COO"))
      mz_neg_hcooh <-
        mass + Rdisop::getMass(molecule = Rdisop::getMolecule("HCOOH"))
      
      is_info_table <- data.frame(
        is_info_table,
        mz_neg_h,
        mz_neg_ch3cOO,
        mz_neg_hcooh,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      is_info_table <-
        rbind(
          data.frame(
            name = is_info_table$name,
            mz = mz_neg_h,
            rt = 100,
            adduct = "M-H",
            stringsAsFactors = FALSE
          ),
          data.frame(
            name = is_info_table$name,
            mz = mz_neg_ch3cOO,
            rt = 100,
            adduct = 'M+CH3COO',
            stringsAsFactors = FALSE
          ),
          data.frame(
            name = is_info_table$name,
            mz = mz_neg_hcooh,
            rt = 100,
            adduct = "M+HCOOH",
            stringsAsFactors = FALSE
          )
        ) %>%
        as.data.frame() %>%
        dplyr::arrange(name, adduct)
      
      is_info_table$name <-
        stringr::str_trim(is_info_table$name)
      
      is_info_table$mz <- as.numeric(is_info_table$mz)
      
      is_info_table$name = paste(is_info_table$name, is_info_table$adduct, sep = "_")
      
      openxlsx::write.xlsx(
        x = is_info_table,
        file = file.path(path, "is_info_table.xlsx"),
        asTable = TRUE
      )
      
    }
    
    
    ###extract IS and output peak plot for each adduct
    if (rerun) {
      unlink(
        x = file.path(path, "Result/intermediate_data"),
        recursive = TRUE,
        force = TRUE
      )
    }
    
    extract_targeted_peaks(
      path = path,
      output_path_name = "Result",
      targeted_targeted_peak_table_name = "is_info_table.xlsx",
      forced_targeted_peak_table_name = NULL,
      from_lipid_search = FALSE,
      fit.gaussian = FALSE,
      integrate_xcms = TRUE,
      output_eic = output_eic,
      output_integrate = TRUE,
      ppm = 40,
      rt.tolerance = 100000,
      facet = FALSE
    )
    
    ###organize plots
    plot_name = dir(path = file.path(path, "Result/peak_shape"), pattern = "html")
    if(length(plot_name) > 0){
      plot_name = 
        data.frame(plot_name = plot_name) %>% 
        dplyr::mutate(lipid_name = stringr::str_split(plot_name, pattern = "_") %>% 
                        purrr::map(function(x){
                          x[-length(x)] %>% 
                            paste(collapse = "_")}) 
                      %>% unlist())
      
      purrr::walk(unique(plot_name$lipid_name), .f = function(x){
        dir.create(file.path(path, "Result/peak_shape", x))
        file.copy(file.path(path, "Result/peak_shape", plot_name$plot_name[plot_name$lipid_name == x]),
                  file.path(path, "Result/peak_shape", x))
      })
      unlink(file.path(path, "Result/peak_shape", plot_name$plot_name))
    }

    unlink(
      x = file.path(path, "Result/forced_targeted_peak_table_temple.xlsx"),
      recursive = TRUE,
      force = TRUE
    )
    
    # unlink(
    #   x = file.path(path, "is_info_table.xlsx"),
    #   recursive = TRUE,
    #   force = TRUE
    # )
    
    from_quantification_table_to_rt_table(path = file.path(path, "Result"))
    
    unlink(
      x = file.path(path, "Result/quantification_table.xlsx"),
      recursive = TRUE,
      force = TRUE
    )
    
    is_info_table_new =
      from_rt_table_to_is_info(
        polarity = polarity,
        is.info = is_info_table_raw,
        path = file.path(path, "Result")
      )
    
    is_info_table$name <-
      stringr::str_split(is_info_table$name, "_") %>%
      purrr::map(function(x) {
        x[1]
      }) %>%
      unlist()
    
    colnames(is_info_table)[colnames(is_info_table) == "mz"] =
      paste("mz", ifelse(polarity == "positive", "pos", "neg"), sep = "_")
    
    colnames(is_info_table)[colnames(is_info_table) == "adduct"] =
      paste("adduct", ifelse(polarity == "positive", "pos", "neg"), sep = "_")
    
    is_info_table_new =
      is_info_table_new %>%
      dplyr::left_join(is_info_table[, c("name",
                                         paste("adduct", ifelse(polarity == "positive", "pos", "neg"), sep = "_"),
                                         paste("mz", ifelse(polarity == "positive", "pos", "neg"), sep = "_"))],
                       by = c("name", paste(
                         "adduct", ifelse(polarity == "positive", "pos", "neg"), sep = "_"
                       )))
    
    return(is_info_table_new)
  }
