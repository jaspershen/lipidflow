#' @title generate_sample_info
#' @description cal_abs
#' @author Xiaotao Shen
#' @param path path
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export
generate_sample_info = function(path = ".") {
  mzxml = list.files(
    path = path,
    pattern = "mzXML",
    all.files = TRUE,
    recursive = TRUE
  )
  
  sample_info =
    mzxml %>%
    stringr::str_split(pattern = "\\/") %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    dplyr::rename(group = V1, sample.name = V2) %>%
    dplyr::select(sample.name, group) %>%
    dplyr::mutate(sample.name = stringr::str_replace(sample.name, "\\.mzXML", ""))
  sample_info
}


#' @title cal_abs
#' @description cal_abs
#' @author Xiaotao Shen
#' @param lipid_tag lipid_tag
#' @param lipid_table lipid_table
#' @param is_tag is_tag
#' @param is_table is_table
#' @param match_item match_item
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export

cal_abs <- function(lipid_tag,
                    lipid_table,
                    is_tag,
                    is_table,
                    match_item) {
  ##remove the class which can bot be use for absolute quantificaion
  remain_idx <- lapply(match_item, function(x) {
    !is.na(x)[1]
  }) %>%
    unlist() %>%
    which()
  
  match_item <- match_item[remain_idx]
  
  ####get the final match_item and final is_table and is_table
  intersect_name <-
    intersect(is_tag$name, unique(unlist(match_item)))
  
  remain_idx <- which(is_tag$name %in% intersect_name)
  
  is_tag <-
    is_tag[remain_idx, ]
  
  is_table <-
    is_table[remain_idx, ]
  
  match_item <-
    lapply(match_item, function(x) {
      x[(x %in% intersect_name)]
    })
  
  remain_idx <-
    lapply(match_item, length) %>%
    unlist() %>%
    `!=`(0)
  
  match_item <- match_item[remain_idx]
  
  ########--------------------------------------
  remain_idx <-
    which(lipid_tag$Class %in% names(match_item))
  
  lipid_table <- lipid_table[remain_idx,]
  lipid_tag <- lipid_tag[remain_idx, ]
  
  express_data_abs_ug_ml <-
    express_data_abs_um <-
    lipid_table %>%
    as.data.frame()
  
  for (i in 1:nrow(lipid_tag)) {
    if(i %in% c(1, seq(10, 10000, by = 10), nrow(lipid_tag))){
      cat(i, " ")  
    }
    temp_class <- lipid_tag$Class[i]
    temp_idx <- match_item[[temp_class]] %>%
      match(., is_tag$name)
    temp_rt <- lipid_tag$rt[i]
    
    temp_idx <-
      temp_idx[which.min(abs(temp_rt - is_tag$rt[temp_idx]))]
    
    ratio <-
      unlist(lipid_table[i, , drop = TRUE]) / unlist(is_table[temp_idx, , drop = TRUE])
    
    # ratio[is.na(ratio)] <- 0
    # ratio[is.infinite(ratio)] <- max(ratio[!is.infinite(ratio)])
    ratio[is.infinite(ratio)] <- 0
    ratio[is.na(ratio)] <- 0
    
    concentration1 <-
      is_tag$ug_ml[temp_idx] * ratio
    
    concentration2 <-
      is_tag$um[temp_idx] * ratio
    
    express_data_abs_ug_ml[i, ] <- concentration1
    express_data_abs_um[i, ] <- concentration2
  }
  
  cat("\n")
  cat(crayon::red(cli::symbol$tick), crayon::red("OK\n"))
  
  return_result <- list(
    express_data_abs_ug_ml = express_data_abs_ug_ml,
    express_data_abs_um = express_data_abs_um,
    variable_info_abs = lipid_tag
  )
  
  return(return_result)
  
}















###clean lipid data
#' @title clean_lipid_data
#' @description clean_lipid_data
#' @author Xiaotao Shen
#' @param x x
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export

clean_lipid_data <-
  function(x) {
    ##rename class
    x$Class[x$Class == "PC" &
              stringr::str_detect(x$name, "p")] <- "PPC"
    x$Class[x$Class == "PE" &
              stringr::str_detect(x$name, "p")] <- "PPE"
    
    ##remove some wrong annotation
    x <-
      x %>%
      dplyr::filter(rt > 1)
    
    dim(x)
    
    remain_idx <-
      as.data.frame(t(x)) %>%
      purrr::map(
        .f = function(x) {
          ##RT
          if (x[7] == "LPC" & as.numeric(x[4]) > 15) {
            return(FALSE)
          }
          
          if (x[7] == "LPE" & as.numeric(x[4]) > 15) {
            return(FALSE)
          }
          
          if (x[7] == "PC" & as.numeric(x[4]) < 15) {
            return(FALSE)
          }
          
          if (x[7] == "PE" & as.numeric(x[4]) < 15) {
            return(FALSE)
          }
          
          if (x[7] == "MG" & as.numeric(x[4]) > 10) {
            return(FALSE)
          }
          
          if (x[7] == "DG" & as.numeric(x[4]) < 10) {
            return(FALSE)
          }
          
          if (x[7] == "TG" & as.numeric(x[4]) < 15) {
            return(FALSE)
          }
          
          return(TRUE)
        }
      ) %>%
      unlist() %>%
      which()
    
    x <-
      x[remain_idx,]
    x
  }


#' @title clean_is_table
#' @description clean_is_table
#' @author Xiaotao Shen
#' @param x x
#' @param polarity polarity
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @importFrom  Rdisop getMass
#' @importFrom  Rdisop getMolecule
#' @export
clean_is_table <- function(x,
                           polarity = c("positive", "negative")) {
  polarity <- match.arg(polarity)
  
  x <-
    x %>%
    dplyr::arrange(Index) %>%
    dplyr::mutate(`Compound Name` = stringr::str_trim(`Compound Name`))
  
  if (any(colnames(x) == "RT POS (second)")) {
    x$`RT POS (min)` <- as.numeric(x$`RT POS (second)`) / 60
    x$`RT NEG (min)` <- as.numeric(x$`RT NEG (second)`) / 60
    x <-
      x %>%
      dplyr::select(-c(`RT POS (second)`, `RT NEG (second)`))
  }
  
  if (polarity == "positive") {
    x_pos <-
      x %>%
      dplyr::select(-c(`RT NEG (min)`, `Adduct NEG`)) %>%
      dplyr::mutate(`RT POS (min)` = as.numeric(`RT POS (min)`)) %>%
      dplyr::filter(!is.na(`RT POS (min)`)) %>%
      dplyr::rename(rt = `RT POS (min)`) %>%
      dplyr::mutate(rt = rt * 60)
    
    cat(nrow(x_pos),
        "out of",
        nrow(x),
        "internal standards have RT.\n")
    
    adduct <-
      x_pos$`Adduct POS`
    
    shift_mz <-
      purrr::map(
        .x = adduct,
        .f = function(x) {
          if (x == "M+H") {
            return(Rdisop::getMass(Rdisop::getMolecule(formula = "H")))
          }
          
          if (x == "M+H-H2O") {
            return(-Rdisop::getMass(Rdisop::getMolecule(formula = "HO")))
          }
          
          if (x == "M+NH4") {
            return(Rdisop::getMass(Rdisop::getMolecule(formula = "NH4")))
          }
          
          if (x == "M+NH4-H2O") {
            return(Rdisop::getMass(Rdisop::getMolecule(formula = "NH2")) - Rdisop::getMass(Rdisop::getMolecule(formula = "O")))
          }
          
          if (x == "M+Na") {
            return(Rdisop::getMass(Rdisop::getMolecule(formula = "Na")))
          }
          
          return(NA)
        }
      ) %>%
      unlist()
    
    mz <-
      purrr::map2(
        .x = as.numeric(x_pos$`Exact Mass`),
        .y = shift_mz,
        .f = function(x, y) {
          x + y
        }
      ) %>% unlist()
    
    x_pos$mz <- mz
    return(x_pos)
  }
  
  
  if (polarity == "negative") {
    x_neg <-
      x %>%
      dplyr::select(-c(`RT POS (min)`, `Adduct POS`)) %>%
      dplyr::mutate(`RT NEG (min)` = as.numeric(`RT NEG (min)`)) %>%
      dplyr::filter(!is.na(`RT NEG (min)`)) %>%
      dplyr::rename(rt = `RT NEG (min)`) %>%
      dplyr::mutate(rt = rt * 60)
    
    cat(nrow(x_neg),
        "out of",
        nrow(x),
        "internal standards have RT.\n")
    
    adduct <-
      x_neg$`Adduct NEG`
    
    shift_mz <-
      purrr::map(
        .x = adduct,
        .f = function(x) {
          if (x == "M-H") {
            return(-Rdisop::getMass(Rdisop::getMolecule(formula = "H")))
          }
          
          if (x == "M+CH3COO") {
            return(Rdisop::getMass(Rdisop::getMolecule(formula = "CH3COO")))
          }
          
          if (x == "M+HCOO") {
            return(Rdisop::getMass(Rdisop::getMolecule(formula = "HCOO")))
          }
          return(NA)
        }
      ) %>%
      unlist()
    
    mz <-
      purrr::map2(
        .x = as.numeric(x_neg$`Exact Mass`),
        .y = shift_mz,
        .f = function(x, y) {
          x + y
        }
      ) %>% unlist()
    
    x_neg$mz <- mz
    return(x_neg)
  }
  
}


#' @title match_sample_peak_table_lipid_search
#' @description match_sample_peak_table_lipid_search
#' @author Xiaotao Shen
#' @param peak_table peak_table
#' @param lipid_search lipid_search
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export

match_sample_peak_table_lipid_search <- function(peak_table,
                                                 lipid_search) {
  colnames(peak_table) <-
    stringr::str_replace(colnames(peak_table), "^X", "")
  
  last_idx_pos <- which(colnames(lipid_search) == "mean.int")
  
  name1 <- setdiff(colnames(lipid_search)[-c(1:last_idx_pos)],
                   colnames(peak_table)[-c(1:3)])
  
  name2 <- setdiff(colnames(peak_table)[-c(1:3)],
                   colnames(lipid_search)[-c(1:last_idx_pos)])
  
  if (length(name1) > 0) {
    lipid_search <-
      lipid_search %>%
      dplyr::select(-name1)
  }
  
  if (length(name2) > 0) {
    peak_table <-
      peak_table %>%
      dplyr::select(-name2)
  }
  
  return(list(peak_table, lipid_search))
}



#' @title get_is_quantify_table
#' @description get_is_quantify_table
#' @author Xiaotao Shen
#' @param is_table is_table
#' @param peak_table peak_table
#' @param mz.tol mz.tol
#' @param rt.tol rt.tol
#' @param figure figure
#' @param polarity polarity
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export
get_is_quantify_table <- function(is_table,
                                  peak_table,
                                  mz.tol = 25,
                                  rt.tol = 90,
                                  figure = FALSE,
                                  polarity = c("positive", "negative")) {
  polarity <- match.arg(polarity)
  data_is <- is_table[, c("mz", "rt")]
  
  is_data <-
    purrr::map(
      .x = peak_table,
      .f = function(x) {
        data_table <- x[, c("mz", "rt")]
        match_result <-
          sxtTools::sxtMTmatch(
            data1 = as.matrix(data_is),
            data2 = as.matrix(data_table),
            mz.tol = mz.tol,
            rt.tol = rt.tol,
            rt.error.type = "abs"
          )
        
        match_result <-
          match_result %>%
          as.data.frame() %>%
          dplyr::group_by(Index1) %>%
          dplyr::filter(`rt error` == min(`rt error`)) %>%
          dplyr::ungroup()
        
        cat(
          nrow(match_result),
          "out of",
          nrow(is_table),
          "internal standards are found in peak table.\n"
        )
        
        is_tag <-
          is_table[match_result$Index1,]
        
        is_data <- x[match_result$Index2, ]  %>%
          dplyr::select(-c(mz, rt))
        
        is_data <-
          cbind(is_tag, is_data)
        is_data
      }
    )
  
  is_data <-
    purrr::map2(
      .x = is_data,
      .y = 1:length(is_data),
      .f = function(x, y) {
        colnames(x)[11] <-
          paste("name", y, sep = "_")
        x
      }
    )
  
  if (length(is_data) == 1) {
    temp_data <- is_data[[1]]
  } else{
    temp_data <- is_data[[1]]
    for (i in 2:length(is_data)) {
      temp_data <-
        temp_data %>%
        dplyr::full_join(is_data[[i]],
                         by = intersect(colnames(temp_data), colnames(is_data[[i]])))
    }
  }
  
  write.csv(
    temp_data,
    file = paste(polarity, "is_data.csv", sep = "_"),
    row.names = FALSE
  )
  
  temp_data <-
    temp_data %>%
    dplyr::select(-contains("name_"))
  
  if (figure) {
    dir.create(path = "IS_figure", showWarnings = FALSE)
    name <- colnames(temp_data)[-c(1:10)]
    purrr::walk(
      as.data.frame(t(temp_data)),
      .f = function(x) {
        plot <-
          data.frame(name = name, value = as.numeric(x[-c(1:10)]))  %>%
          ggplot(aes(name, value)) +
          geom_point(shape = 21,
                     size = 4,
                     fill = "red") +
          labs(x = "", y = "Response") +
          theme_bw()
        ggsave(
          plot,
          filename = file.path("IS_figure", 
                               paste(x[2] %>% 
                                       stringr::str_replace_all("\\/", "_") %>% 
                                       stringr::str_replace_all("\\:", "_"), 
                                     ".pdf", sep = "")),
          width = 7,
          height = 7
        )
      }
    )
  }
  
  temp_data
  
}




#' @title combine_pos_neg_quantification
#' @description combine_pos_neg_quantification
#' @author Xiaotao Shen
#' @param path path
#' @param express_data_abs_ug_ml_pos express_data_abs_ug_ml_pos
#' @param express_data_abs_um_pos express_data_abs_um_pos
#' @param variable_info_abs_pos variable_info_abs_pos
#' @param express_data_abs_ug_ml_neg express_data_abs_ug_ml_neg
#' @param express_data_abs_um_neg express_data_abs_um_neg
#' @param variable_info_abs_neg variable_info_abs_neg
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export

combine_pos_neg_quantification <- function(path = ".",
                                           express_data_abs_ug_ml_pos,
                                           express_data_abs_um_pos,
                                           variable_info_abs_pos,
                                           express_data_abs_ug_ml_neg,
                                           express_data_abs_um_neg,
                                           variable_info_abs_neg) {
  dir.create(path, showWarnings = FALSE)
  
  ##combine positive and negative
  rownames(express_data_abs_ug_ml_pos) <-
    rownames(express_data_abs_um_pos) <-
    variable_info_abs_pos$peak_name
  
  rownames(express_data_abs_ug_ml_neg) <-
    rownames(express_data_abs_um_neg) <-
    variable_info_abs_neg$peak_name
  
  express_data_abs_ug_ml <-
    rbind(express_data_abs_ug_ml_pos,
          express_data_abs_ug_ml_neg)
  
  colnames(express_data_abs_um_pos) <-
    colnames(express_data_abs_um_neg)
  
  express_data_abs_um <-
    rbind(express_data_abs_um_pos,
          express_data_abs_um_neg)
  
  variable_info_abs <-
    variable_info_abs_pos %>%
    dplyr::full_join(variable_info_abs_neg, by = intersect(
      colnames(variable_info_abs_pos),
      colnames(variable_info_abs_neg)
    ))
  
  ####remove duplicated
  express_data_abs_ug_ml <-
    express_data_abs_ug_ml %>%
    data.frame(variable_info_abs, ., check.names = FALSE) %>%
    plyr::dlply(.variables = plyr::.(name)) %>%
    purrr::map(
      .f = function(x) {
        x <-
          x %>%
          dplyr::filter(mean.int == max(mean.int)) %>%
          head(1)
      }
    ) %>%
    do.call(rbind, .) %>%
    as.data.frame()
  
  rownames(express_data_abs_ug_ml)  <- NULL
  
  express_data_abs_ug_ml <-
    express_data_abs_ug_ml %>%
    tibble::column_to_rownames(var = "peak_name") %>%
    dplyr::select(-colnames(variable_info_abs)[-1])
  
  express_data_abs_um <-
    express_data_abs_um %>%
    data.frame(variable_info_abs, ., check.names = FALSE) %>%
    plyr::dlply(.variables = plyr::.(name)) %>%
    purrr::map(
      .f = function(x) {
        x <-
          x %>%
          dplyr::filter(mean.int == max(mean.int)) %>%
          head(1)
      }
    ) %>%
    do.call(rbind, .) %>%
    as.data.frame()
  
  rownames(express_data_abs_um)   <- NULL
  
  express_data_abs_um <-
    express_data_abs_um %>%
    tibble::column_to_rownames(var = "peak_name") %>%
    dplyr::select(-colnames(variable_info_abs)[-1])
  
  variable_info_abs <-
    variable_info_abs[match(rownames(express_data_abs_ug_ml),
                            variable_info_abs$peak_name),]
  
  class_data_ug_ml <-
    express_data_abs_ug_ml %>%
    data.frame(
      class = variable_info_abs$Class,
      .,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ) %>%
    plyr::dlply(.variables = plyr::.(class)) %>%
    purrr::map(
      .f = function(x) {
        apply(x[, -1], 2, function(y) {
          sum(y, na.rm = TRUE)
        })
      }
    ) %>%
    do.call(rbind, .) %>%
    as.data.frame()
  
  class_data_ug_ml = 
  class_data_ug_ml %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "Class")
  
  # openxlsx::write.xlsx(
  #   x = class_data_ug_ml,
  #   file = file.path(path, "class_data_ug_ml.xlsx"),
  #   asTable = TRUE
  # )
  
  class_data_um <-
    express_data_abs_um %>%
    data.frame(
      class = variable_info_abs$Class,
      .,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ) %>%
    plyr::dlply(.variables = plyr::.(class)) %>%
    purrr::map(
      .f = function(x) {
        apply(x[, -1], 2, function(y) {
          sum(y, na.rm = TRUE)
        })
      }
    ) %>%
    do.call(rbind, .) %>%
    as.data.frame()
  
  class_data_um = 
    class_data_um %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "Class")
  
  # openxlsx::write.xlsx(
  #   x = class_data_um,
  #   file = file.path(path, "class_data_um.xlsx"),
  #   asTable = TRUE
  # )
  
  plot1 <-
    class_data_ug_ml %>%
    tidyr::pivot_longer(cols = -Class,
                        names_to = "sample_id",
                        values_to = "value") %>%
    ggplot(aes(sample_id, value)) +
    geom_bar(stat = "identity", position = "fill", aes(fill = Class)) +
    theme_bw() +
    labs(x = "") +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    scale_x_discrete(expand = expansion(mult = c(0, 0))) +
    theme(axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ))
  
  ggsave(
    plot1,
    filename = file.path(path, "plot_ug.pdf"),
    width = 14,
    height = 7
  )
  
  
  plot2 <-
    class_data_um %>%
    tidyr::pivot_longer(cols = -Class,
                        names_to = "sample_id",
                        values_to = "value") %>%
    ggplot(aes(sample_id, value)) +
    geom_bar(stat = "identity",
             position = "fill",
             aes(fill = Class)) +
    theme_bw() +
    labs(x = "") +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    scale_x_discrete(expand = expansion(mult = c(0, 0))) +
    theme(axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ))
  
  ggsave(
    plot2,
    filename = file.path(path, "plot_um.pdf"),
    width = 14,
    height = 7
  )
  
  express_data_abs_um_per <-
    express_data_abs_um %>%
    apply(2, function(x) {
      x * 100 / sum(x)
    }) %>%
    as.data.frame()
  
  class_data_um_per <-
    class_data_um %>%
    tibble::column_to_rownames(var = "Class") %>% 
    apply(2, function(x) {
      x * 100 / sum(x)
    }) %>%
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "Class")
  
  openxlsx::write.xlsx(
    x = cbind(variable_info_abs, express_data_abs_ug_ml),
    file = file.path(path, "lipid_data_ug_ml.xlsx"),
    asTable = TRUE
  )
  
  openxlsx::write.xlsx(
    x = cbind(variable_info_abs, express_data_abs_um),
    file = file.path(path, "lipid_data_um.xlsx"),
    asTable = TRUE
  )
  
  openxlsx::write.xlsx(
    x = cbind(variable_info_abs, express_data_abs_um_per),
    file = file.path(path, "lipid_data_um_per.xlsx"),
    asTable = TRUE
  )
  
  openxlsx::write.xlsx(
    x = class_data_ug_ml,
    file = file.path(path, "lipid_data_class_ug_ml.xlsx"),
    asTable = TRUE
  )
  
  openxlsx::write.xlsx(
    x = class_data_um,
    file = file.path(path, "lipid_data_class_um.xlsx"),
    asTable = TRUE
  )
  
  openxlsx::write.xlsx(
    x = class_data_um_per,
    file = file.path(path, "lipid_data_class_um_per.xlsx"),
    asTable = TRUE
  )
  
}










#' @title tidy_lipidsearch_data
#' @description tidy_lipidsearch_data
#' @author Xiaotao Shen
#' @param file file
#' @param polarity polarity
#' @param from from
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export

tidy_lipidsearch_data <-
  function(file,
           polarity = c("positive", "negative"),
           from = "lipidsearch") {
    polarity <- match.arg(polarity)
    ##read lipid search result
    lipid_table <- file
    if (from == "lipidsearch") {
      idx <- which(is.na(lipid_table$...1))[1]
      rename <- lipid_table[1:(idx - 1), 1]
      
      rename <-
        rename$...1 %>%
        stringr::str_split("\\:") %>%
        do.call(rbind, .) %>%
        as.data.frame() %>%
        dplyr::rename(new_name = V1, old_name = V2) %>%
        dplyr::filter(stringr::str_detect(old_name, "raw"))
      
      rename$new_name <-
        rename$new_name %>%
        stringr::str_replace("#", "")
      
      rename$old_name <-
        rename$old_name %>%
        stringr::str_replace("\\.raw", "")
      
      lipid_table <-
        lipid_table[-c(1:idx), ]
      
      colnames(lipid_table) <- as.character(lipid_table[1, ])
      lipid_table <- lipid_table[-1, ]
      
      lipid_table <-
        lipid_table %>%
        dplyr::rename(rentention_time = `GroupTopPos[c]`)
      
      mz <-
        lipid_table %>%
        dplyr::select(contains("CalcMz")) %>%
        apply(1, function(x) {
          mean(as.numeric(x), na.rm = TRUE)
        })
      
      lipid_table <-
        lipid_table %>%
        dplyr::select(-contains("Height")) %>%
        dplyr::select(-contains("Score")) %>%
        dplyr::select(-contains("Norm")) %>%
        dplyr::select(-contains("Top")) %>%
        dplyr::select(-contains("Hwhm")) %>%
        dplyr::select(-contains("DataId")) %>%
        dplyr::select(-contains("Scan")) %>%
        dplyr::select(-contains("ObsMz")) %>%
        dplyr::select(-contains("Rt")) %>%
        dplyr::select(-contains("Delta")) %>%
        dplyr::select(-contains("z")) %>%
        dplyr::select(-contains("It"))
      
      colnames(lipid_table) <-
        colnames(lipid_table) %>%
        stringr::str_replace("Area", "")
      
      expression_data <-
        lipid_table[, rename$new_name] %>%
        apply(2, as.numeric) %>%
        as.data.frame()
      
      colnames(expression_data) <- rename$old_name
      
      variable_info <-
        lipid_table %>%
        dplyr::select(-rename$new_name)
      
      adduct <-
        variable_info$LipidIon %>%
        purrr::map(
          .f = function(x) {
            temp <- stringr::str_split(x, "\\)\\+", n = 2)[[1]]
            if (length(temp) == 1) {
              temp <- stringr::str_split(x, "\\)\\-", n = 2)[[1]]
            }
            temp
          }
        ) %>%
        do.call(rbind, .) %>%
        as.data.frame() %>%
        dplyr::rename(name = V1, adduct = V2) %>%
        dplyr::mutate(name = paste(name, ")", sep = ""))
      
      adduct <-
        data.frame(
          lipid_raw_name = variable_info$LipidIon,
          adduct,
          polarity,
          stringsAsFactors = FALSE
        )
      
      variable_info <-
        data.frame(adduct, variable_info, stringsAsFactors = FALSE)
      
      mean.int <- apply(expression_data, 1, median)
      
      variable_info <-
        variable_info %>%
        dplyr::select(-c(ARatio.0.:HDiff.0.)) %>%
        dplyr::select(
          name,
          lipid_raw_name,
          rt = rentention_time,
          adduct,
          polarity,
          Class,
          FattyAcid,
          IonFormula,
          contains("FA")
          # mean.int = Average.
        ) %>%
        data.frame(., mean.int, stringsAsFactors = FALSE)
      
      variable_info$rt <- as.numeric(variable_info$rt)
      data <- cbind(variable_info, expression_data)
      
      ##give unique name for each lipid
      peak_name = data$name
      for (x in unique(peak_name)) {
        if (length(peak_name[peak_name == x]) > 1) {
          peak_name[peak_name == x] =
            paste(x, seq_along(peak_name[peak_name == x]), sep = "_")
        }
      }
      
      data$peak_name <-
        peak_name
      
      rownames(data) <- data$peak_name
      
      data <-
        data %>%
        dplyr::mutate(mz = mz) %>%
        dplyr::select(peak_name, mz, rt, dplyr::everything())
      
      return(data)
    }
  }




#' @title trans_is_table
#' @description trans_is_table
#' @author Xiaotao Shen
#' @param is_table is_table
#' @param polarity polarity
#' @param path path
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export

trans_is_table <-
  function(is_table,
           polarity = c("positive", "negative"),
           path = ".") {
    polarity <- match.arg(polarity)
    
    is_table$name <-
      is_table$name %>%
      stringr::str_trim()
    
    is_table_old <- is_table
    
    if (polarity == "positive") {
      is_table =
        is_table_old[, c('name', "mz_pos", "rt_pos_second", "adduct_pos")]
      colnames(is_table) =
        c("name", "mz", "rt", "adduct")
      
      is_table$name <-
        stringr::str_trim(is_table$name)
      is_table$mz <- as.numeric(is_table$mz)
      is_table$rt <- as.numeric(is_table$rt)
      
      is_table <-
        is_table %>%
        dplyr::filter(!is.na(rt))
      
    } else {
      is_table =
        is_table_old[, c('name', "mz_neg", "rt_neg_second", "adduct_neg")]
      
      colnames(is_table) =
        c("name", "mz", "rt", "adduct")
      
      is_table$name <-
        stringr::str_trim(is_table$name)
      is_table$mz <- as.numeric(is_table$mz)
      is_table$rt <- as.numeric(is_table$rt)
      
      is_table <-
        is_table %>%
        dplyr::filter(!is.na(rt))
    }
    return(is_table)
  }


#' @title extract_targeted_peaks
#' @description extract_targeted_peaks
#' @author Xiaotao Shen
#' @param path path
#' @param output_path_name output_path_name
#' @param targeted_targeted_peak_table_name targeted_targeted_peak_table_name
#' @param forced_targeted_peak_table_name forced_targeted_peak_table_name
#' @param from_lipid_search from_lipid_search
#' @param sample_numer_show sample_numer_show
#' @param fit.gaussian fit.gaussian
#' @param integrate_xcms integrate_xcms
#' @param output_eic output_eic
#' @param output_integrate output_integrate
#' @param ppm ppm
#' @param rt.tolerance rt.tolerance
#' @param threads threads
#' @param facet facet
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export

extract_targeted_peaks <-
  function(path = ".",
           output_path_name = "Result",
           targeted_targeted_peak_table_name = "is_table.xlsx",
           forced_targeted_peak_table_name = "forced_table.xlsx",
           from_lipid_search = FALSE,
           sample_numer_show = 5,
           fit.gaussian = TRUE,
           integrate_xcms = FALSE,
           output_eic = TRUE,
           output_integrate = TRUE,
           ppm = 25,
           rt.tolerance = 90,
           threads = 3,
           facet = TRUE) {

    peak_table <-
      suppressMessages(readxl::read_xlsx(file.path(path , targeted_targeted_peak_table_name)))
    output_path = file.path(path, output_path_name)
    
    dir.create(output_path, showWarnings = FALSE)
    
    dir.create(file.path(output_path, "intermediate_data"), showWarnings = FALSE)
    
    if (any(dir(file.path(output_path, "intermediate_data")) == "result_raw")) {
      load(file.path(output_path, "intermediate_data/result_raw"))
    } else{
      result_raw <-
        extractPeaks2(
          path = path,
          output_path_name = output_path_name,
          ppm = ppm,
          threads = threads,
          is.table = targeted_targeted_peak_table_name,
          rt.tolerance = rt.tolerance,
          msLevel = 1L
        )
      
      save(result_raw,
           file = file.path(output_path, "intermediate_data/result_raw"))
    }
    
    dir.create(file.path(output_path, "peak_shape"), showWarnings = FALSE)
    
    # load("raw_data")
    #
    # raw_data %>%
    #   filterRt(rt = as.numeric(result_raw@featureData@data[1,3:4])) %>%
    #   filterMz(mz = as.numeric(result_raw@featureData@data[1,1:2])) %>%
    #   plot(type = "XIC")
    #
    # x <-
    #   raw_data@featureData@data
    #
    # x <-
    # x %>%
    #   dplyr::filter(fileIdx == 1)
    
    if (any(dir(file.path(output_path, "intermediate_data/")) == "result")) {
      load(file.path(output_path, "intermediate_data/result"))
    } else{
      result <- xcms::findChromPeaks(
        object = result_raw,
        param = xcms::CentWaveParam(
          peakwidth = c(5, 30),
          snthresh = 2,
          ppm = ppm,
          fitgauss = FALSE,
          noise = 500,
          prefilter = c(3, 500),
          integrate = 1
          # mzdiff = -0.01
        )
      )
      
      # plot(result, col = "black", type = "b")
      
      mpp <- xcms::MergeNeighboringPeaksParam(expandRt = 3)
      
      result <- xcms::refineChromPeaks(result, param = mpp)
      # plot(result, col = "black", type = "b")
      save(result, file = file.path(output_path, "intermediate_data/result"))
    }
    
    peak_value <-
      xcms::chromPeaks(result) %>%
      as.data.frame()
    
    peak_value <-
      peak_value %>%
      plyr::dlply(.variables = plyr::.(row, column)) %>%
      purrr::map(function(x) {
        x <-
          x %>%
          dplyr::filter(into == max(into))
        x
      }) %>%
      do.call(rbind, .) %>%
      as.data.frame()
    
    peak_value$name <-
      peak_table$name[peak_value$row]
    
    peak_value$sample <-
      result@phenoData@data$sample_group[peak_value$column]
    
    peak_value <-
      peak_value %>%
      dplyr::select(-c(row, column)) %>%
      dplyr::select(name, sample, dplyr::everything())
    
    raw_info <- result@.Data
    
    rownames(raw_info) <- peak_table$name
    colnames(raw_info) <- result@phenoData@data$sample_group
    # raw_info_old <- raw_info
    
    ##output quantification information
    output_quantification_table <-
      peak_value %>%
      dplyr::select(name, sample, into, rt) %>%
      dplyr::group_by(name) %>%
      dplyr::mutate(rt2 = median(rt)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-rt) %>%
      dplyr::rename(rt = rt2) %>%
      dplyr::distinct(name, sample, .keep_all = TRUE) %>%
      tidyr::pivot_wider(names_from = sample, values_from = "into")
    
    diff_name <-
      setdiff(result@phenoData@data$sample_group,
              colnames(output_quantification_table))
    
    if (length(diff_name) > 0) {
      diff_matrix <-
        matrix(NA,
               nrow = nrow(output_quantification_table),
               ncol = length(diff_name)) %>%
        as.data.frame()
      colnames(diff_matrix) <-
        diff_name
      output_quantification_table <-
        cbind(output_quantification_table, diff_matrix)
    }
    
    output_quantification_table <-
      as.data.frame(output_quantification_table)
    
    output_quantification_table_old <- output_quantification_table
    
    if (!integrate_xcms) {
      output_quantification_table[, -c(1:2)] <- NA
    }
    
    ###check missing value
    if (fit.gaussian) {
      for (temp_name in output_quantification_table$name) {
        # cat(temp_name, " ")
        for (temp_sample in colnames(output_quantification_table)[-c(1:2)]) {
          # cat(temp_sample, " ")
          value <-
            output_quantification_table[output_quantification_table$name == temp_name, temp_sample] %>%
            as.numeric()
          
          if (!is.na(value)) {
            next()
          } else{
            temp_raw_info <-
              raw_info[temp_name, temp_sample][[1]]
            
            xy <-
              data.frame(
                x = temp_raw_info@rtime,
                y = temp_raw_info@intensity,
                stringsAsFactors = FALSE
              ) %>%
              dplyr::filter(!is.na(y))
            
            if (sum(!is.na(xy$y)) <= 6) {
              value <- 0
            } else{
              
              fit_result <-
                try(expr = fit_gaussian(x = xy$x, y = xy$y),
                    silent = TRUE)
              
              if (class(fit_result)[1] == "try-error") {
                xy <-
                  data.frame(xy, z = 0)
              } else{
                xy <-
                  data.frame(xy, z = fit_result$y)
              }
              
              
              # plot(xy$x, xy$y, type = "b")
              # points(xy$x, xy$z, type = "b", col = "red")
              #
              # abline(v = fit_result$center)
              # abline(v = fit_result$center - fit_result$width * 2)
              # abline(v = fit_result$center + fit_result$width * 2)
              # abline(h = 0)
              
              l1 <-
                loess(y ~ x, xy, control = loess.control(surface = "direct"))
              # l2 <-
              #   loess(z ~ x, xy, control = loess.control(surface = "direct"))
              f1 <- function(x) {
                predict(l1, newdata = x)
              }
              # f1 <- approxfun(xy$x, xy$y)
              # f2 <- function(x)
              #   predict(l2, newdata = x)
              # f2 <- approxfun(xy$x, xy$z)
              
              value1 <- try(expr = integrate(
                f = f1,
                lower = min(xy$x),
                upper = max(xy$x)
              )$value,
              silent = TRUE)
              #
              # value2 <- try(integrate(f = f2,
              #                         lower = min(xy$x),
              #                         upper = max(xy$x))$value, silent = TRUE
              # )
              
              if (class(value1)[1] == "try-error") {
                value1 <- NA
              }
              #
              # if(class(value2) == "try-error"){
              #   value2 <- NA
              # }
              
              fit_error <-
                abs(as.numeric(fit_result$residual)) / xy$y
              
              max_idx <- which.max(xy$z)
              
              item1 <-
                try(expr = shapiro.test(x = xy$z)$p.value > 0.05,
                    silent = TRUE)
              
              if (class(item1)[1] == "try-error") {
                item1 <- FALSE
              }
              
              item2 <-
                (sum(fit_error < 0.3) >= nrow(xy) / 2) &
                (abs(max_idx - nrow(xy) / 2) < nrow(xy) * 0.3 / 2)
              
              if (item1 | item2) {
                value <- value1
                ##add information to raw_info
                raw_info[temp_name, temp_sample][[1]]@chromPeaks <-
                  data.frame(
                    rt = fit_result$center,
                    rtmin = min(xy$x),
                    rtmax = max(xy$x),
                    into = value,
                    maxo = max(xy$y),
                    sn = NA
                  ) %>%
                  as.matrix()
                
                raw_info[temp_name, temp_sample][[1]]@rtime <-
                  xy$x
                raw_info[temp_name, temp_sample][[1]]@intensity <-
                  xy$y
                
                raw_info[temp_name, temp_sample][[1]]@productMz <-
                  xy$z
              } else{
                value <- value
              }
            }
            output_quantification_table[output_quantification_table$name == temp_name, temp_sample] <-
              value
          }
        }
      }
    }
    
    ###add mz and adduct
    output_quantification_table <-
      output_quantification_table %>%
      dplyr::left_join(peak_table[, c("name", "mz", "adduct")], by = "name") %>%
      dplyr::select(name, mz, rt, adduct, dplyr::everything())
    
    if (from_lipid_search) {
      output_quantification_table <-
        output_quantification_table %>%
        dplyr::left_join(peak_table[, c("name", "compound_name")],
                         by = "name")
      
      ##remove duplicated
      output_quantification_table <-
        output_quantification_table %>%
        plyr::dlply(.variables = plyr::.(compound_name)) %>%
        purrr::map(function(y) {
          if (nrow(y) == 1) {
            return(y)
          }
          
          mean.int <-
            y %>%
            dplyr::select(-c(name:adduct, compound_name)) %>%
            apply(1, function(z) {
              mean(z, na.rm = TRUE)
            })
          y <- y %>%
            dplyr::mutate(mean.int = mean.int)
          
          ###remain the peaks with largest mean.int
          if (nrow(y) > 1) {
            y <-
              y %>%
              dplyr::filter(mean.int == max(mean.int)) %>%
              `[`(., 1,) %>%
              as.data.frame()
            
            return(y %>% dplyr::select(-mean.int))
          }
        }) %>%
        do.call(rbind, .) %>%
        as.data.frame()
      
      output_quantification_table <-
        output_quantification_table %>%
        dplyr::select(-compound_name)
      
    }
    
    ###manual check
    if (!is.null(forced_targeted_peak_table_name)) {
      cat(crayon::green("Manually check..\n"))
      forced_targeted_peak_table <-
        readxl::read_xlsx(file.path(output_path, forced_targeted_peak_table_name)) %>%
        dplyr::filter(!is.na(begin_rt)) %>%
        dplyr::filter(begin_rt != "")
      
      if (nrow(forced_targeted_peak_table) > 0) {
        for (i in 1:nrow(forced_targeted_peak_table)) {
          # cat(forced_targeted_peak_table$name[i], "\n")
          temp_name <- forced_targeted_peak_table$name[i]
          temp_sample <- forced_targeted_peak_table$sample[i]
          
          temp_raw_info <-
            raw_info[temp_name, temp_sample][[1]]
          
          xy <-
            data.frame(
              x = temp_raw_info@rtime,
              y = temp_raw_info@intensity,
              stringsAsFactors = FALSE
            ) %>%
            dplyr::filter(!is.na(y)) %>%
            dplyr::filter(
              x >= as.numeric(forced_targeted_peak_table$begin_rt[i]) &
                x <= as.numeric(forced_targeted_peak_table$end_rt[i])
            )
          
          # fit_result <-
          #   try(expr = fit_gaussian(x = xy$x, y = xy$y), silent = TRUE
          #   )
          #
          # xy <-
          #   data.frame(xy, z = fit_result$y)
          
          l1 <- loess(y ~ x, xy,
                      control = loess.control(surface = "direct"))
          
          f1 <- function(x) {
            predict(l1, newdata = x)
          }
          
          value1 <- try(expr = integrate(f = f1,
                                         lower = min(xy$x),
                                         upper = max(xy$x))$value,
                        silent = TRUE)
          
          if (class(value1) == "try-error") {
            value1 <- NA
          }
          
          value <- value1
          ##add information to raw_info
          raw_info[temp_name, temp_sample][[1]]@chromPeaks <-
            data.frame(
              rt = fit_result$center,
              rtmin = min(xy$x),
              rtmax = max(xy$x),
              into = value,
              maxo = max(xy$y),
              sn = NA
            ) %>%
            as.matrix()
          
          raw_info[temp_name, temp_sample][[1]]@rtime <-
            xy$x
          
          raw_info[temp_name, temp_sample][[1]]@intensity <-
            xy$y
          
          # raw_info[temp_name, temp_sample][[1]]@productMz <-
          #   xy$z
          
          output_quantification_table[output_quantification_table$name == temp_name, temp_sample] <-
            value
          
        }
      }
      
    }
    
    openxlsx::write.xlsx(
      x = output_quantification_table,
      file = file.path(output_path, "quantification_table.xlsx"),
      asTable = TRUE
    )
    
    example_temp <-
      matrix(nrow = 0, ncol = 8) %>%
      as.data.frame()
    
    colnames(example_temp)  <-
      c("name",
        "rt",
        "rtmin",
        "rtmax",
        "into",
        "intb",
        "maxo",
        "sn")
    
    peak_value_old <- peak_value
    
    peak_value <-
      purrr::map2(
        .x = as.data.frame(raw_info),
        .y = colnames(as.data.frame(raw_info)),
        .f = function(data, sample_name) {
          names(data) <- rownames(raw_info)
          temp <-
            purrr::map2(
              .x = data,
              .y = names(data),
              .f = function(data1, peak_name) {
                if (nrow(as.data.frame(data1@chromPeaks)) == 0) {
                  peak_name <- logical(0)
                }
                test <-
                  data.frame(
                    name = peak_name,
                    as.data.frame(data1@chromPeaks) %>%
                      dplyr::filter(into == max(into)),
                    stringsAsFactors = FALSE
                  )
                
                if (ncol(test) < ncol(example_temp)) {
                  diff_name <- setdiff(colnames(example_temp), colnames(test))
                  add_matrix <-
                    matrix(nrow = nrow(test), ncol = length(diff_name)) %>%
                    as.data.frame()
                  colnames(add_matrix) <- diff_name
                  test <-
                    data.frame(test, add_matrix)[, colnames(example_temp)]
                  test
                }
                test
              }
            )  %>%
            do.call(rbind, .) %>%
            as.data.frame()
          
          if (nrow(temp) == 0) {
            sample_name <- logical(0)
          }
          
          data.frame(sample = sample_name, temp, stringsAsFactors = FALSE)
        }
      ) %>%
      do.call(rbind, .) %>%
      as.data.frame()
    
    rownames(peak_value) <- NULL
    
    peak_table <-
      peak_table %>%
      dplyr::filter(name %in% output_quantification_table$name)
    
    forced_targeted_peak_table_temple <-
      matrix(NA, nrow = nrow(raw_info), ncol = ncol(raw_info)) %>%
      as.data.frame()
    
    colnames(forced_targeted_peak_table_temple) <-
      colnames(raw_info)
    rownames(forced_targeted_peak_table_temple) <-
      rownames(raw_info)
    
    forced_targeted_peak_table_temple <-
      forced_targeted_peak_table_temple %>%
      tibble::rownames_to_column(var = "name") %>%
      tidyr::pivot_longer(cols = -name,
                          names_to = "sample",
                          values_to = "value") %>%
      dplyr::mutate(begin_rt = NA, end_rt = NA) %>%
      dplyr::select(-value)
    
    forced_targeted_peak_table_temple <-
      forced_targeted_peak_table_temple %>%
      dplyr::filter(name %in% output_quantification_table$name)
    
    openxlsx::write.xlsx(
      forced_targeted_peak_table_temple,
      file.path(output_path, "forced_targeted_peak_table_temple.xlsx"),
      asTable = TRUE
    )
    
    if (output_eic) {
      cat("\n")
      cat(crayon::green("Output peak shapes...\n"))
      for (i in 1:nrow(peak_table)) {
        test <-
          try(expr = {
            if (!is.null(forced_targeted_peak_table_name)) {
              if (!peak_table$name[i] %in% forced_targeted_peak_table$name) {
                next()
              }
            }
            cat(i, " ")
            x <- as.character(peak_table[i,])
            name <- paste(x[1], sep = "_")
            
            plot <-
              showPeak2(
                object = result_raw,
                peak.index = match(x[1], rownames(raw_info)),
                title = name,
                interactive = TRUE,
                area_data = peak_value[peak_value$name == name, ],
                raw_info = raw_info,
                facet = facet
              )
            
            htmlwidgets::saveWidget(
              widget = plot,
              file = file.path(
                output_path,
                "peak_shape",
                paste(
                  name %>%
                    stringr::str_replace_all("\\:", "_") %>%
                    stringr::str_replace_all("\\/", "_") %>% 
                    stringr::str_replace_all("\\(", "_") %>% 
                    stringr::str_replace_all("\\)", "_"),
                  "html",
                  sep = "."
                )
              ),
              selfcontained = TRUE,
              title = name,
              libdir = "lib"
            )
          }, silent = TRUE)
      }
      cat("\n")
      cat(crayon::bgRed("Done\n"))
    }
  }

# x <- temp_raw_info@rtime
# y <- temp_raw_info@intensity
#
# xy <- data.frame(x, y, stringsAsFactors = FALSE) %>%
#   dplyr::filter(!is.na(y))
#
# x <- xy$x
# y <- xy$y
#
# plot(x,y, type = "b")
#
fit_gaussian <-
  function (x,
            y,
            start.center = NULL,
            start.width = NULL,
            start.height = NULL,
            start.floor = NULL,
            fit.floor = FALSE) {
    who.max <- which.max(y)
    if (is.null(start.center)) {
      start.center <- x[who.max]
    }
    
    if (is.null(start.height)) {
      start.height <- y[who.max]
    }
    
    if (is.null(start.width)) {
      start.width <- sum(y > (start.height / 2), na.rm = TRUE) / 2
    }
    
    controlList <- nls.control(maxiter = 100,
                               minFactor = 1 / 512,
                               warnOnly = TRUE)
    
    gaussian1 <-
      function(x,
               center = 0,
               width = 1,
               height = NULL,
               floor = 0) {
        # adapted from Earl F. Glynn;  Stowers Institute for Medical Research, 2007
        twoVar <- 2 * width * width
        sqrt2piVar <- sqrt(pi * twoVar)
        y <- exp(-(x - center) ^ 2 / twoVar) / sqrt2piVar
        
        # by default, the height is such that the curve has unit volume
        if (!is.null (height)) {
          scalefactor <- sqrt2piVar
          y <- y * scalefactor * height
        }
        y + floor
      }
    
    if (!fit.floor) {
      starts <- list(center = start.center,
                     width = start.width,
                     height = start.height)
      nlsAns <- try(nls(y ~ gaussian1(x, center, width, height),
                        start = starts,
                        control = controlList))
    } else {
      if (is.null(start.floor)) {
        start.floor <- quantile(y, seq(0, 1, 0.1))[2]
      }
      starts <- list(
        center = start.center,
        width = start.width,
        height = start.height,
        floor = start.floor
      )
      nlsAns <- try(nls(
        y ~ gaussian1(x, center, width, height,
                     floor),
        start = starts,
        control = controlList
      ))
    }
    if (class(nlsAns) == "try-error") {
      centerAns <- start.center
      widthAns <- start.width
      heightAns <- start.height
      floorAns <- if (fit.floor) {
        start.floor
      } else{
        0
      }
      yAns <-
        try(expr = gaussian1(x, centerAns, widthAns, heightAns, floorAns),
            silent = TRUE)
      if (class(yAns) == "try-error") {
        residualAns <- y - 0
      } else{
        residualAns <- y - yAns
      }
      
    } else {
      coefs <- coef(nlsAns)
      centerAns <- coefs[1]
      widthAns <- coefs[2]
      heightAns <- coefs[3]
      floorAns <- if (fit.floor) {
        coefs[4]
      } else{
        0
      }
      
      yAns <- fitted(nlsAns)
      residualAns <- residuals(nlsAns)
    }
    widthAns <- abs(widthAns)
    out <-
      list(
        center = centerAns,
        width = widthAns,
        height = heightAns,
        y = yAns,
        residual = residualAns
      )
    if (fit.floor) {
      out <- c(out, floor = floorAns)
    }
    return(out)
  }


find_local_minimal <- function(x, y) {
  i.mins  <- which(diff(sign(diff(c(
    Inf, y, Inf
  )))) == 2)
  i.mins
}







#' @title extractPeaks2
#' @description extractPeaks2
#' @author Xiaotao Shen
#' @param path path
#' @param output_path_name output_path_name
#' @param ppm ppm
#' @param threads threads
#' @param is.table is.table
#' @param mz mz
#' @param rt rt
#' @param rt.tolerance rt.tolerance
#' @param msLevel msLevel
#' @param filled filled
#' @param include include
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export

setGeneric(
  name = "extractPeaks2",
  def = function(path = ".",
                 output_path_name = "Result",
                 ppm = 15,
                 threads = 4,
                 is.table = "is.xlsx",
                 mz = NULL,
                 rt = NULL,
                 rt.tolerance = 40,
                 msLevel = 1,
                 filled = TRUE,
                 include = c("apex_within", "any", "none")) {
    options(warn = -1)
    output_path <- file.path(path, output_path_name)
    dir.create(output_path)
    dir.create(file.path(output_path, "intermediate_data"), showWarnings = FALSE)
    include <- match.arg(include)
    # dir.create(output_path)
    ##peak detection
    
    f.in <- list.files(
      path = path,
      pattern = '\\.(mz[X]{0,1}ML|cdf)',
      recursive = TRUE,
      full.names = TRUE
    )
    
    sample_group <-
      BiocGenerics::basename(f.in) %>%
      stringr::str_replace("\\.(mz[X]{0,1}ML|cdf)", "")
    
    pd <-
      data.frame(# sample_name = sub(
        basename(f.in),
        #   pattern = ".mzXML",
        #   replacement = "",
        #   fixed = TRUE
        # ),
        sample_group = sample_group,
        stringsAsFactors = FALSE)
    
    # requireNamespace("xcms")
    cat(crayon::green("Reading raw data, it will take a while...\n"))
    
    if (any(dir(file.path(output_path, "intermediate_data")) == "raw_data")) {
      cat(crayon::yellow("Use old data.\n"))
      load(file.path(output_path, "intermediate_data/raw_data"))
    } else{
      raw_data <- MSnbase::readMSData(
        files = f.in,
        pdata = new("NAnnotatedDataFrame", pd),
        mode = "onDisk",
        verbose = TRUE
      )
      
      save(
        raw_data,
        file = file.path(output_path, "intermediate_data/raw_data"),
        compress = "xz"
      )
    }
    
    cat(crayon::red(cli::symbol$tick, "OK\n"))
    
    is.table <-
      try(readxl::read_xlsx(file.path(path, is.table)), silent = TRUE)
    
    if (!is.null(mz) & !is.null(rt)) {
      if (length(mz) != length(rt)) {
        cat(crayon::yellow("Lenght of mz and rt you provied are different.\n"))
      }
      is.table <- data.frame(mz = as.numeric(mz),
                             rt = as.numeric(rt),
                             stringsAsFactors = FALSE)
      is.table$name <- paste("feature", 1:nrow(is.table), sep = "_")
      
      is.table <-
        is.table %>%
        dplyr::select(name, mz, rt)
    }
    
    if (class(is.table)[1] == "try-error") {
      stop(crayon::red('Please provide right is table or mz and rt.\n'))
    }
    
    mz <-
      is.table %>%
      dplyr::pull(2)
    
    mz <- as.numeric(mz)
    
    mz_range <-
      lapply(mz, function(x) {
        c(x - ppm * x / 10 ^ 6, ppm * x / 10 ^ 6 + x)
      })
    
    mz_range <- do.call(rbind, mz_range)
    
    if (rt.tolerance > 10000) {
      rt_range <- NA
    } else{
      if (any(colnames(is.table) == "rt")) {
        rt <-
          is.table %>%
          dplyr::pull(3) %>%
          as.numeric()
        
        rt_range <-
          lapply(rt, function(x) {
            c(x - rt.tolerance, x + rt.tolerance)
          }) %>%
          do.call(rbind, .)
        
        rt_range[, 1][which(rt_range[, 1] < 0)] <- 0
        
      } else{
        rt_range <- NA
      }
    }
    
    cat(crayon::green("Extracting peaks, it will take a while..."))
    if (!is.na(rt_range)) {
      # raw_data <- xcms::filterRt(raw_data, c(min(rt_range), max(rt_range)))
      peak_data <- xcms::chromatogram(
        object = raw_data,
        mz = mz_range,
        rt = rt_range,
        aggregationFun = "sum",
        msLevel = msLevel
        # filled = filled,
        # include = include
      )
      
    } else{
      peak_data <- xcms::chromatogram(
        object = raw_data,
        mz = mz_range,
        aggregationFun = "sum",
        msLevel = msLevel
        # filled = filled,
        # include = include
      )
    }
    cat(crayon::red(cli::symbol$tick, "OK\n"))
    
    save(peak_data,
         file = file.path(output_path, "intermediate_data/peak_data"))
    return(peak_data)
  }
)



#' @title showPeak2
#' @description showPeak2
#' @author Xiaotao Shen
#' @param object object
#' @param peak.index peak.index
#' @param title.size title.size
#' @param lab.size lab.size
#' @param axis.text.size axis.text.size
#' @param alpha alpha
#' @param title title
#' @param interactive interactive
#' @param area_data area_data
#' @param raw_info raw_info
#' @param facet facet
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export

setGeneric(
  name = "showPeak2",
  def = function(object,
                 peak.index = 1,
                 title.size = 12,
                 lab.size = 12,
                 axis.text.size = 12,
                 alpha = 0.5,
                 title = "",
                 interactive = FALSE,
                 area_data,
                 raw_info,
                 facet = TRUE) {
    options(warn = -1)
    info <- object@phenoData@data
    data <- object@.Data
    rm(list = c("object"))
    if (peak.index > nrow(data)) {
      peak.index <- nrow(data)
      cat("peak.index is ", nrow(data), '\n')
    }
    data <- apply(data, 2, function(x) {
      x <- x[[peak.index]]
      x <-
        data.frame(
          "rt" = x@rtime,
          "intensity" = x@intensity,
          stringsAsFactors = FALSE
        )
      list(x)
    })
    
    data <- lapply(data, function(x) {
      x[[1]]
    })
    
    data <- mapply(
      FUN = function(x, y, z) {
        x <- data.frame(
          x,
          "group" = y,
          "sample" = z,
          stringsAsFactors = FALSE
        )
        list(x)
      },
      x = data,
      y = info[, 2],
      z = info[, 1]
    )
    
    data <- do.call(rbind, args = data)
    
    ######
    ##check rt
    area_data <-
      area_data %>%
      apply(1, function(z) {
        if (as.numeric(z[3]) > as.numeric(z[5]) |
            as.numeric(z[3]) < as.numeric(z[4])) {
          z[3] <- mean(c(as.numeric(z[4]), as.numeric(z[5])))
        }
        z
      }) %>%
      t() %>%
      as.data.frame()
    
    area_data$rt <- as.numeric(area_data$rt)
    area_data$rtmin <- as.numeric(area_data$rtmin)
    area_data$rtmax <- as.numeric(area_data$rtmax)
    area_data$into <- as.numeric(area_data$into)
    area_data$intb <- as.numeric(area_data$intb)
    area_data$maxo <- as.numeric(area_data$maxo)
    area_data$sn <- as.numeric(area_data$sn)
    
    area_data <-
      area_data %>%
      dplyr::rename(rtmedian = rt) %>%
      dplyr::arrange(sample)
    
    area_data_raw_info <-
      purrr::map2(
        .x = area_data$sample,
        .y = raw_info[area_data$name[1], area_data$sample],
        .f = function(x, y) {
          if (length(y@productMz) > 2) {
            fit_int <- as.numeric(y@productMz)
          } else{
            fit_int <- NA
          }
          data.frame(
            rt = y@rtime,
            int = y@intensity,
            fit_int,
            sample = x,
            stringsAsFactors = FALSE
          )
        }
      ) %>%
      do.call(rbind, .) %>%
      as.data.frame()
    
    area_data_raw_info <-
      area_data_raw_info %>%
      dplyr::left_join(area_data, by = "sample") %>%
      dplyr::select(-c(rt:fit_int, name)) %>%
      dplyr::distinct(sample, .keep_all = TRUE)
    
    
    final_data <-
      data %>%
      dplyr::select(group, rt, intensity) %>%
      dplyr::left_join(area_data_raw_info, by = c("group" = "sample")) %>%
      dplyr::mutate(group = paste(group,
                                  format(into, scientific = TRUE),
                                  sep = ":"))
    
    plot <-
      ggplot(final_data) +
      geom_line(aes(rt, intensity, color = group, group = group)) +
      geom_area(
        aes(rt, intensity, fill = group, group = group),
        alpha = 0.3,
        data = final_data %>%
          dplyr::filter(rt > rtmin & rt < rtmax)
      ) +
      geom_rect(
        aes(
          xmin = rtmin,
          xmax = rtmax,
          ymin = 0,
          ymax = maxo,
          col = group
        ),
        fill = "transparent",
        data = final_data[, c("group", "rtmin", "rtmax", "maxo")] %>%
          dplyr::distinct(group, .keep_all = TRUE)
      ) +
      # geom_line(aes(rt, fit_int, group = sample), color = "black") +
      geom_point(aes(rt, intensity),
                 shape = 16,
                 size = 0.5) +
      labs(x = "", y = "") +
      theme_bw() +
      # facet_wrap(facets = vars(group)) +
      theme(
        panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 1
        )
      )
    
    if (facet) {
      plot =
        plot +
        facet_wrap(facets = vars(group))
    }
    
    if (interactive) {
      plot <- plotly::ggplotly(plot)
    }
    return(plot)
  }
)



#' @title get_quantification_data2
#' @description get_quantification_data2
#' @author Xiaotao Shen
#' @param path path
#' @param is_tag is_tag
#' @param is_table is_table
#' @param lipid_tag lipid_tag
#' @param lipid_table lipid_table
#' @param match_item match_item
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export


# get_quantification_data2(
#   path = path,
#   is_tag = is_tag,
#   is_table = is_table,
#   lipid_tag = lipid_tag,
#   lipid_table = lipid_table,
#   match_item = match_item
# )

get_quantification_data2 <-
  function(path = ".",
           is_tag,
           is_table,
           lipid_tag,
           lipid_table,
           match_item) {
    expression_data_abs <-
      cal_abs(
        lipid_tag = lipid_tag,
        lipid_table = lipid_table,
        is_tag = is_tag,
        is_table = is_table,
        match_item = match_item
      )
    
    express_data_abs_ug_ml <-
      expression_data_abs$express_data_abs_ug_ml
    express_data_abs_um <- expression_data_abs$express_data_abs_um
    variable_info_abs <- expression_data_abs$variable_info_abs
    
    rownames(express_data_abs_ug_ml) <-
      rownames(express_data_abs_um) <-
      variable_info_abs$peak_name
    
    new_path = file.path(path, "absolute_quantification")
    dir.create(new_path)
    save(variable_info_abs, file = file.path(new_path, "variable_info_abs"))
    save(express_data_abs_ug_ml,
         file = file.path(new_path, "express_data_abs_ug_ml"))
    save(express_data_abs_um, file = file.path(new_path, "express_data_abs_um"))
    save(lipid_tag, file = file.path(new_path, "lipid_tag"))
    save(lipid_table, file = file.path(new_path, "lipid_table"))
    save(is_tag, file = file.path(new_path, "is_tag"))
    save(is_table, file = file.path(new_path, "is_table"))
    return(list(variable_info_abs = variable_info_abs,
                express_data_abs_ug_ml = express_data_abs_ug_ml,
                express_data_abs_um = express_data_abs_um,
                lipid_tag = lipid_tag,
                lipid_table = lipid_table,
                is_tag = is_tag,
                is_table = is_table))
  }




#' @title process_mv
#' @description process_mv
#' @author Xiaotao Shen
#' @param quantification_data quantification_data
#' @param na.tolerance na.tolerance
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export
process_mv <- function(quantification_data,
                       na.tolerance = 0.8) {
  tag <-
    quantification_data %>%
    dplyr::select(c(name:adduct))
  
  data <-
    quantification_data %>%
    dplyr::select(-c(name:adduct))
  
  remain_idx <-
    apply(data, 1, function(x) {
      sum(is.na(x)) / ncol(data)
    }) %>%
    `<`(na.tolerance) %>%
    which()
  
  tag <- tag[remain_idx, ]
  
  data <- data[remain_idx, ]
  
  data <-
    impute::impute.knn(data = as.matrix(data), k = 10)$data %>%
    as.data.frame()
  return(cbind(tag, data))
}




#' @title from_quantification_table_to_rt_table
#' @description from_quantification_table_to_rt_table
#' @author Xiaotao Shen
#' @param path path
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export

from_quantification_table_to_rt_table <-
  function(path = ".") {
    table = readxl::read_xlsx(file.path(path, "quantification_table.xlsx"))
    table$name <-
      stringr::str_split(table$name, "_") %>%
      lapply(function(x) {
        x[1]
      }) %>%
      unlist()
    unique_name = unique(table$name)
    
    wb <- openxlsx::createWorkbook()
    openxlsx::modifyBaseFont(wb, fontSize = 14, fontName = "Times New Roma")
    ##add sheet to wb
    unique_name %>%
      purrr::walk(
        .f = function(x) {
          x =
            x %>%
            stringr::str_replace_all("\\:", "_")
          openxlsx::addWorksheet(wb, sheetName = x, gridLines = TRUE)
        }
      )
    
    ##write table to wb
    1:length(unique_name) %>%
      purrr::walk(function(idx) {
        openxlsx::freezePane(wb,
                   sheet = idx,
                   firstRow = TRUE,
                   firstCol = TRUE)
        temp =
          table[table$name == unique_name[idx], , drop = FALSE]
        
        mean_value =
          temp %>%
          dplyr::select(-c(name, mz, rt, adduct)) %>%
          apply(1, function(x) {
            mean(x, na.rm = TRUE)
          })
        
        temp =
          data.frame(temp, mean_value, stringsAsFactors = FALSE) %>%
          dplyr::arrange(dplyr::desc(mean_value)) %>%
          dplyr::select(name, mz, rt, adduct) %>%
          dplyr::mutate(check_priority = "Low")
        
        temp$check_priority[1] = "High"
        
        ###if the first adduct RT is very similar with other adducts, no need to check others
        if (nrow(temp) > 1) {
          rt_error = lapply(temp$rt[-1], function(x) {
            abs(x - temp$rt[1])
          }) %>%
            unlist()
          
          remove_idx = which(rt_error < 30) + 1
          if (length(remove_idx) > 0) {
            temp =
              temp[-remove_idx, , drop = FALSE]
          }
        }
        
        
        openxlsx::writeDataTable(
          wb,
          sheet = idx,
          x = temp,
          colNames = TRUE,
          rowNames = FALSE
        )
      })
    
    openxlsx::saveWorkbook(wb,
                 file.path(path, "IS_RT_table_for_check.xlsx"),
                 overwrite = TRUE)
    
  }


#' @title from_rt_table_to_is_info
#' @description from_rt_table_to_is_info
#' @author Xiaotao Shen
#' @param polarity polarity
#' @param is.info is.info
#' @param path path
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export
from_rt_table_to_is_info =
  function(polarity = c("positive", 'negative'),
           is.info,
           path = ".") {
    polarity = match.arg(polarity)
    is_table <- is.info
    
    is_table$name =
      stringr::str_trim(is_table$name)
    
    sheets = readxl::excel_sheets(file.path(path,
                                            "IS_RT_table_for_check.xlsx"))
    
    for (x in sheets) {
      temp = readxl::read_xlsx(file.path(path,
                                         "IS_RT_table_for_check.xlsx"),
                               sheet = x)
      
      if (polarity == 'positive') {
        is_table$rt_pos_second[is_table$name == temp$name[1]] =
          temp$rt[1]
        is_table$rt_pos_min[is_table$name == temp$name[1]] =
          temp$rt[1] / 60
        is_table$adduct_pos[is_table$name == temp$name[1]] =
          temp$adduct[1]
      } else{
        is_table$rt_neg_second[is_table$name == temp$name[1]] =
          temp$rt[1]
        is_table$rt_neg_min[is_table$name == temp$name[1]] =
          temp$rt[1] / 60
        is_table$adduct_neg[is_table$name == temp$name[1]] =
          temp$adduct[1]
      }
    }
    
    
    is_table$exact.mass  =
      is_table$exact.mass %>%
      stringr::str_trim(side = "both") %>%
      as.numeric()
    
    is_table$ug_ml =
      is_table$ug_ml %>%
      stringr::str_trim(side = "both") %>%
      as.numeric()
    
    is_table$um =
      is_table$um %>%
      stringr::str_trim(side = "both") %>%
      as.numeric()
    
    return(is_table)
  }


#' @title combine_quantification_data_and_feature_info
#' @description combine_quantification_data_and_feature_info
#' @author Xiaotao Shen
#' @param quantification_data quantification_data
#' @param feature_info feature_info
#' @param targeted_table_type targeted_table_type
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export
combine_quantification_data_and_feature_info =
  function(quantification_data,
           feature_info,
           targeted_table_type = c("is", "lipid")) {
    targeted_table_type = match.arg(targeted_table_type)
    
    if (targeted_table_type == "is") {
      feature_info =
        feature_info %>%
        dplyr::mutate(name = stringr::str_trim(name))
      
      quantification_data <-
        quantification_data %>%
        dplyr::left_join(feature_info %>%
                           dplyr::select(name:um), by = "name") %>%
        dplyr::select(name:adduct, exact.mass:um, everything())
      
      return(quantification_data)
      
    } else{
      colnames(quantification_data)[1] = "peak_name"
      quantification_data <-
        quantification_data %>%
        dplyr::select(-c(mz, rt, adduct)) %>%
        dplyr::left_join(feature_info %>%
                           dplyr::select(peak_name:mean.int), by = "peak_name") %>%
        dplyr::select(peak_name, mz:mean.int, everything())
      
      quantification_data$mean.int <-
        quantification_data %>%
        dplyr::select(-c(peak_name:mean.int)) %>%
        apply(1, function(x) {
          mean(x, na.rm = TRUE)
        })
      
      return(quantification_data)
    }
    
    
  }