#' @title  reorganize_peak_plot
#' @description reorganize_peak_plot
#' @author Xiaotao Shen
#' @param path path.
#' @param plot_dir plot_dir
#' @param absolute_table absolute_table
#' @param match_item match_item
#' @return Peak plot for each internal standard.
#' @importFrom magrittr %>%
#' @export

reorganize_peak_plot <-
  function(path = ".",
           plot_dir = "peak_shape",
           absolute_table,
           match_item) {
    for (i in 1:length(match_item)) {
      cat(i, " ")
      dir.create(file.path(path, plot_dir, names(match_item)[i]), showWarnings = FALSE)
      
      peak_name <-
        absolute_table %>%
        dplyr::filter(Class == names(match_item)[i]) %>%
        dplyr::pull(peak_name) %>%
        stringr::str_replace_all("\\/", "_") %>%
        stringr::str_replace_all("\\:", "_") %>% 
        stringr::str_replace_all("\\(", "_") %>% 
        stringr::str_replace_all("\\)", "_")
      
      if (length(peak_name) > 0) {
        file_name1 <- paste(peak_name, ".html", sep = "")
        
        file.copy(
          from = file.path(path, plot_dir, file_name1),
          to = file.path(path, plot_dir, names(match_item)[i]),
          overwrite = TRUE,
          recursive = TRUE
        )
        
        unlink(
          file.path(path, plot_dir, file_name1),
          recursive = TRUE,
          force = TRUE
        )
      }
    }
    
    ###other peaks are copy into 'other_lipids'
   other_plot = dir(path = file.path(path, plot_dir), pattern = "html")
   if(length(other_plot) > 1){
     dir.create(file.path(path, plot_dir, "other_lipids"))
     file.copy(
       from = file.path(path, plot_dir, other_plot),
       to = file.path(path, plot_dir, "other_lipids"),
       overwrite = TRUE,
       recursive = TRUE
     )
     
     unlink(
       file.path(path, plot_dir, other_plot),
       recursive = TRUE,
       force = TRUE
     )
   }  
  }
