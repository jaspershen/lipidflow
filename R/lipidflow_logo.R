#' @title Show the base information of lipidflow package
#' @description Show the base information of lipidflow package.
#' @author Xiaotao Shen
#' \email{shenxt@@stanford.edu}
#' @return A ASCII log of lipidflow
#' @importFrom magrittr %>%
#' @importFrom crayon red yellow green bgRed
#' @importFrom stringr str_detect str_extract str_extract_all 
#' @importFrom stringr str_replace_all str_replace str_trim str_c str_count
#' @importFrom readr cols read_csv
#' @importFrom pbapply pblapply pboptions
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom dplyr mutate filter everything select bind_rows left_join pull desc
#' @importFrom plyr dlply .
#' @importFrom readxl read_xlsx read_xls
#' @importFrom purrr map map2
#' @importFrom ggplot2 aes ggplot geom_point geom_line geom_smooth theme annotate
#' @importFrom ggplot2 geom_abline theme_bw ggsave geom_segment xlim ylim labs
#' @importFrom ggplot2 element_line element_text
#' @importFrom MSnbase readMSData
#' @importFrom ProtGenerics spectra
#' @importFrom stats lm loess predict coef fitted integrate loess.control median nls
#' @importFrom stats nls.control quantile residuals rt sd shapiro.test
#' @importFrom plotly ggplotly
#' @importFrom Rdisop getMass getMolecule
#' @importFrom openxlsx write.xlsx
#' @importFrom cli rule symbol
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggsci scale_fill_d3
#' @importFrom htmlwidgets saveWidget
#' @importFrom impute impute.knn
#' @importFrom rstudioapi hasFun getThemeInfo isAvailable
#' @importFrom sxtTools sxtMTmatch
#' @import lifecycle
#' @import utils
#' @import ggplot2
#' @import grDevices
#' @import methods
#' @export
#' @examples 
#' lipidflow_logo()

lipidflow_logo <- function() {
  cat(crayon::green("Thank you for using lipidflow!\n"))
  cat(crayon::green("Version 0.0.1 (20210227)\n"))
  cat(
    crayon::green(
      "More information can be found at https://jaspershen.github.io/lipidflow/\n"
    )
  )
  cat(crayon::green(
    c("  _ _       _     _  __ _               ", " | (_)     (_)   | |/ _| |              ", 
      " | |_ _ __  _  __| | |_| | _____      __", " | | | '_ \\| |/ _` |  _| |/ _ \\ \\ /\\ / /", 
      " | | | |_) | | (_| | | | | (_) \\ V  V / ", " |_|_| .__/|_|\\__,_|_| |_|\\___/ \\_/\\_/  ", 
      "     | |                                ", "     |_|                               "
    )
    
  ), sep = "\n")
}






# library(cowsay)
# # https://onlineasciitools.com/convert-text-to-ascii-art
# # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
# art <- readLines("logo.txt")
# dput(art)
# lipidflow_logo <-
#   c("  _ _       _     _  __ _               ", " | (_)     (_)   | |/ _| |              ", 
#     " | |_ _ __  _  __| | |_| | _____      __", " | | | '_ \\| |/ _` |  _| |/ _ \\ \\ /\\ / /", 
#     " | | | |_) | | (_| | | | | (_) \\ V  V / ", " |_|_| .__/|_|\\__,_|_| |_|\\___/ \\_/\\_/  ", 
#     "     | |                                ", "     |_|                               "
#   )
# cat(lipidflow_logo, sep = "\n")
