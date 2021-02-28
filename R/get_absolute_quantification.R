#' @title  get_absolute_quantification
#' @description get_absolute_quantification
#' @author Xiaotao Shen
#' @param path path.
#' @param is_quantification_table is_quantification_table
#' @param lipid_quantification_table lipid_quantification_table
#' @param sample_info sample_info
#' @param match_item match_item
#' @return Peak plot for each internal standard.
#' @importFrom magrittr %>%
#' @export


get_absolute_quantification <-
  function(path = ".",
           is_quantification_table,
           lipid_quantification_table,
           sample_info,
           match_item) {
    ##check data
    if (length(setdiff(
      sample_info$sample.name,
      colnames(is_quantification_table)
    )) > 0) {
      stop(
        paste(setdiff(
          sample_info$sample.name,
          colnames(is_quantification_table)
        ), collapse = ", "),
        " are in samples but are not in internal standard quantification table."
      )
    }
    
    if (length(setdiff(
      sample_info$sample.name,
      colnames(lipid_quantification_table)
    )) > 0) {
      stop(
        paste(setdiff(
          sample_info$sample.name,
          colnames(lipid_quantification_table)
        ), collapse = ", "),
        " are in samples but are not in lipid quantification table."
      )
    }
    
    is_tag =
      is_quantification_table %>%
      dplyr::select(-sample_info$sample.name)
    
    is_table =
      is_quantification_table %>%
      dplyr::select(sample_info$sample.name)
    
    rownames(is_table) = is_tag$name
    
    lipid_tag =
      lipid_quantification_table %>%
      dplyr::select(-sample_info$sample.name)
    
    if (max(lipid_tag$rt) < 60) {
      lipid_tag$rt = lipid_tag$rt * 60
    }
    
    lipid_table =
      lipid_quantification_table %>%
      dplyr::select(sample_info$sample.name)
    
    rownames(lipid_table) = lipid_tag$peak_name
    
    get_quantification_data2(
      path = path,
      is_tag = is_tag,
      is_table = is_table,
      lipid_tag = lipid_tag,
      lipid_table = lipid_table,
      match_item = match_item
    )
  }
