#' @title output_result
#' @description output_result
#' @author Xiaotao Shen
#' @param path work directory.
#' @param match_item_pos match_item_pos
#' @param match_item_neg match_item_neg
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export

output_result = function(path = ".",
                         match_item_pos,
                         match_item_neg) {
  
  lipid_data <-
    readxl::read_xlsx(file.path(path, "Result/lipid_data_um.xlsx"))
  
  load(file.path(path, "POS/absolute_quantification/is_table"))
  is_table_pos <- is_table
  
  load(file.path(path, "POS/absolute_quantification/is_tag"))
  is_tag_pos <- is_tag
  
  load(file.path(path, "POS/absolute_quantification/lipid_table"))
  lipid_table_pos <- lipid_table
  
  load(file.path(path, "POS/absolute_quantification/lipid_tag"))
  lipid_tag_pos <- lipid_tag
  
  load(file.path(path, "POS/absolute_quantification/express_data_abs_ug_ml"))
  express_data_abs_ug_ml_pos <- express_data_abs_ug_ml
  
  load(file.path(path, "POS/absolute_quantification/express_data_abs_um"))
  express_data_abs_um_pos <- express_data_abs_um
  
  load(file.path(path, "POS/absolute_quantification/variable_info_abs"))
  variable_info_abs_pos <- variable_info_abs
  
  variable_info_abs_pos <-
    variable_info_abs_pos %>%
    dplyr::filter(peak_name %in% lipid_data$peak_name)
  
  express_data_abs_ug_ml_pos <-
    express_data_abs_ug_ml_pos[match(variable_info_abs_pos$peak_name,
                                     rownames(express_data_abs_ug_ml_pos)), ]
  
  express_data_abs_um_pos <-
    express_data_abs_um_pos[match(variable_info_abs_pos$peak_name,
                                  rownames(express_data_abs_um_pos)), ]
  
  rownames(lipid_table_pos) <- lipid_tag_pos$peak_name
  
  lipid_tag_pos <-
    lipid_tag_pos %>%
    dplyr::filter(peak_name %in% lipid_data$peak_name)
  
  lipid_table_pos <-
    lipid_table_pos[match(lipid_tag_pos$peak_name, rownames(lipid_table_pos)), ]
  
  ###negative mode
  load(file.path(path, "NEG/absolute_quantification/is_table"))
  is_table_neg <- is_table
  
  load(file.path(path, "NEG/absolute_quantification/is_tag"))
  is_tag_neg <- is_tag
  
  load(file.path(path, "NEG/absolute_quantification/lipid_table"))
  lipid_table_neg <- lipid_table
  
  load(file.path(path, "NEG/absolute_quantification/lipid_tag"))
  lipid_tag_neg <- lipid_tag
  
  load(file.path(path, "NEG/absolute_quantification/express_data_abs_ug_ml"))
  express_data_abs_ug_ml_neg <- express_data_abs_ug_ml
  
  load(file.path(path, "NEG/absolute_quantification/express_data_abs_um"))
  express_data_abs_um_neg <- express_data_abs_um
  
  load(file.path(path, "NEG/absolute_quantification/variable_info_abs"))
  variable_info_abs_neg <- variable_info_abs
  
  variable_info_abs_neg <-
    variable_info_abs_neg %>%
    dplyr::filter(peak_name %in% lipid_data$peak_name)
  
  express_data_abs_ug_ml_neg <-
    express_data_abs_ug_ml_neg[match(variable_info_abs_neg$peak_name,
                                     rownames(express_data_abs_ug_ml_neg)), ]
  
  express_data_abs_um_neg <-
    express_data_abs_um_neg[match(variable_info_abs_neg$peak_name,
                                  rownames(express_data_abs_um_neg)), ]
  
  rownames(lipid_table_neg) <- lipid_tag_neg$peak_name
  
  lipid_tag_neg <-
    lipid_tag_neg %>%
    dplyr::filter(peak_name %in% lipid_data$peak_name)
  
  lipid_table_neg <-
    lipid_table_neg[match(lipid_tag_neg$peak_name, rownames(lipid_table_neg)), ]
  
  ###output results
  dir.create(file.path(path, "Result/intensity_plot"),
             showWarnings = FALSE)
  
  ###positive
  cat(crayon::green("Positive mode...\n"))
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
          is_table_pos[match(temp_is_name, is_tag_pos$name), ] %>%
          t() %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "sample_id")
        
        colnames(temp_data1)[2] <- "intensity"
        
        ###raw data
        temp_name <- lipid_tag_pos$name[j]
        temp_data2 <-
          lipid_table_pos[j,] %>%
          t() %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "sample_id")
        
        colnames(temp_data2)[2] <- "intensity"
        
        
        temp_data3 <-
          express_data_abs_um_pos[match(temp_name, variable_info_abs_pos$name),] %>%
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
          ggplot2::geom_point(size = 4,
                              shape = 21,
                              ggplot2::aes(fill = from)) +
          ggrepel::geom_text_repel(aes(label = round(intensity, 4))) +
          ggsci::scale_fill_d3() +
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.5, 0.5))) +
          ggplot2::theme_bw() +
          ggplot2::labs(x = "", y = "Intensity") +
          ggplot2::theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
            axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1,
            vjust = 1,
            size = 10
          )) +
          ggplot2::facet_wrap(facets = ggplot2::vars(from),
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
  cat("\n")
  
  ###negative
  cat(crayon::green("Negative mode...\n"))
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
          is_table_neg[match(temp_is_name, is_tag_neg$name), ] %>%
          t() %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "sample_id")
        
        colnames(temp_data1)[2] <- "intensity"
        
        ###raw data
        temp_name <- lipid_tag_neg$name[j]
        temp_data2 <-
          lipid_table_neg[j,] %>%
          t() %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "sample_id")
        
        colnames(temp_data2)[2] <- "intensity"
        
        
        temp_data3 <-
          express_data_abs_um_neg[match(temp_name, variable_info_abs_neg$name),] %>%
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
  cat("\n")
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
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.5, 0.5))) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", y = "Intensity") +
      ggplot2::theme(
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        size = 10
      ))
    
    ggsave(
      temp_plot,
      filename = file.path(
        path,
        "Result/class_plot",
        paste(
          lipid_data_class$Class[i] %>%
            stringr::str_replace_all("\\/", "_") %>%
            stringr::str_replace_all("\\:", "_"),
          ".pdf",
          sep = ""
        )
      ),
      width = 7,
      height = 7
    )
  }

  cat("\n")
  cat(crayon::bgRed("Done.\n"))
}