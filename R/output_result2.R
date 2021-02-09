# ##no source
# no_source()
# 
# sxtTools::setwd_project()
# 
# setwd("demo_data/absolute_quantification/")
# 
# lipid_data <-
#   readxl::read_xlsx("Result/lipid_data_um.xlsx")
# 
# ###positive mode
# load("POS/is_table")
# is_table_pos <-
#   is_table
# 
# load("POS/is_tag")
# is_tag_pos <-
#   is_tag
# 
# load("POS/lipid_table")
# lipid_table_pos <-
#   lipid_table
# 
# load("POS/lipid_tag")
# lipid_tag_pos <-
#   lipid_tag
# 
# load("POS/express_data_abs_ug_ml")
# express_data_abs_ug_ml_pos <-
#   express_data_abs_ug_ml
# 
# load("POS/express_data_abs_um")
# express_data_abs_um_pos <-
#   express_data_abs_um
# 
# load("POS/variable_info_abs")
# variable_info_abs_pos <-
#   variable_info_abs
# 
# variable_info_abs_pos <-
#   variable_info_abs_pos %>%
#   dplyr::filter(peak_name %in% lipid_data$peak_name)
# 
# express_data_abs_ug_ml_pos <-
#   express_data_abs_ug_ml_pos[match(variable_info_abs_pos$peak_name,
#                                    rownames(express_data_abs_ug_ml_pos)),]
# 
# express_data_abs_um_pos <-
#   express_data_abs_um_pos[match(variable_info_abs_pos$peak_name,
#                                 rownames(express_data_abs_um_pos)),]
# 
# rownames(lipid_table_pos) <- lipid_tag_pos$peak_name
# 
# lipid_tag_pos <-
#   lipid_tag_pos %>%
#   dplyr::filter(peak_name %in% lipid_data$peak_name)
# 
# lipid_table_pos <-
#   lipid_table_pos[match(lipid_tag_pos$peak_name, rownames(lipid_table_pos)),]
# 
# ###negative mode
# load("NEG/is_table")
# is_table_neg <-
#   is_table
# 
# load("NEG/is_tag")
# is_tag_neg <-
#   is_tag
# 
# load("NEG/lipid_table")
# lipid_table_neg <-
#   lipid_table
# 
# load("NEG/lipid_tag")
# lipid_tag_neg <-
#   lipid_tag
# 
# load("NEG/express_data_abs_ug_ml")
# express_data_abs_ug_ml_neg <-
#   express_data_abs_ug_ml
# 
# load("NEG/express_data_abs_um")
# express_data_abs_um_neg <-
#   express_data_abs_um
# 
# load("NEG/variable_info_abs")
# variable_info_abs_neg <-
#   variable_info_abs
# 
# variable_info_abs_neg <-
#   variable_info_abs_neg %>%
#   dplyr::filter(peak_name %in% lipid_data$peak_name)
# 
# express_data_abs_ug_ml_neg <-
#   express_data_abs_ug_ml_neg[match(variable_info_abs_neg$peak_name,
#                                    rownames(express_data_abs_ug_ml_neg)),]
# 
# express_data_abs_um_neg <-
#   express_data_abs_um_neg[match(variable_info_abs_neg$peak_name,
#                                 rownames(express_data_abs_um_neg)),]
# 
# rownames(lipid_table_neg) <- lipid_tag_neg$peak_name
# 
# lipid_tag_neg <-
#   lipid_tag_neg %>%
#   dplyr::filter(peak_name %in% lipid_data$peak_name)
# 
# lipid_table_neg <-
#   lipid_table_neg[match(lipid_tag_neg$peak_name, rownames(lipid_table_neg)),]
# 
# ###output
# dir.create(file.path("Result/intensity_plot"),
#            showWarnings = FALSE)
# 
# ###positive
# for (i in names(match_item_pos)) {
#   cat(i, "")
#   is_name <-
#     match_item_pos[[i]]
#   
#   if (is.na(unique(is_name)[1])) {
#     next()
#   }
#   
#   dir.create(file.path("Result/intensity_plot", i),
#              showWarnings = FALSE)
#   
#   temp_idx <-
#     which(lipid_tag_pos$Class == i)
#   
#   if (length(temp_idx) > 0) {
#     for (j in temp_idx) {
#       temp_is_name <-
#         abs(lipid_tag_pos$rt[j] -
#               is_tag_pos$rt[match(is_name, is_tag_pos$name)]) %>%
#         which.min() %>%
#         head(1) %>%
#         `[`(is_name, .)
#       
#       temp_data1 <-
#         is_table_pos[match(temp_is_name, is_tag_pos$name),] %>%
#         t() %>%
#         as.data.frame() %>%
#         tibble::rownames_to_column(var = "sample_id")
#       
#       colnames(temp_data1)[2] <- "intensity"
#       
#       ###raw data
#       temp_name <- lipid_tag_pos$name[j]
#       temp_data2 <-
#         lipid_table_pos[j, ] %>%
#         t() %>%
#         as.data.frame() %>%
#         tibble::rownames_to_column(var = "sample_id")
#       
#       colnames(temp_data2)[2] <- "intensity"
#       
#       
#       temp_data3 <-
#         express_data_abs_um_pos[match(temp_name, variable_info_abs_pos$name), ] %>%
#         t() %>%
#         as.data.frame() %>%
#         tibble::rownames_to_column(var = "sample_id")
#       
#       colnames(temp_data3)[2] <- "intensity"
#       
#       temp_data <-
#         rbind(
#           data.frame(temp_data1, from = "is"),
#           data.frame(temp_data2, from = "raw"),
#           data.frame(temp_data3, from = "final")
#         ) %>%
#         dplyr::mutate(from = factor(from, levels = c("is", "raw", "final")))
#       
#       temp_plot <-
#         temp_data %>%
#         ggplot(aes(sample_id, intensity)) +
#         geom_point(size = 4, shape = 21, aes(fill = from)) +
#         ggrepel::geom_text_repel(aes(label = round(intensity, 4))) +
#         ggsci::scale_fill_d3() +
#         scale_y_continuous(expand = expansion(mult = c(0.5, 0.5))) +
#         theme_bw() +
#         labs(x = "", y = "Intensity") +
#         theme(axis.text.x = element_text(
#           angle = 45,
#           hjust = 1,
#           vjust = 1
#         )) +
#         facet_wrap(facets = vars(from),
#                    ncol = 1,
#                    scales = "free_y")
#       
#       temp_name <- paste(temp_name, ".pdf", sep = "")
#       temp_name <-
#         stringr::str_replace_all(temp_name, "\\/", "_")
#       ggsave(
#         temp_plot,
#         filename = file.path("Result/intensity_plot", i, temp_name),
#         width = 14,
#         height = 14
#       )
#     }
#   }
# }
# 
# 
# ###negative
# for (i in names(match_item_neg)) {
#   cat(i, "")
#   is_name <-
#     match_item_neg[[i]]
#   
#   if (is.na(unique(is_name)[1])) {
#     next()
#   }
#   
#   dir.create(file.path("Result/intensity_plot", i),
#              showWarnings = FALSE)
#   
#   temp_idx <-
#     which(lipid_tag_neg$Class == i)
#   
#   if (length(temp_idx) > 0) {
#     for (j in temp_idx) {
#       temp_is_name <-
#         abs(lipid_tag_neg$rt[j] -
#               is_tag_neg$rt[match(is_name, is_tag_neg$name)]) %>%
#         which.min() %>%
#         head(1) %>%
#         `[`(is_name, .)
#       
#       temp_data1 <-
#         is_table_neg[match(temp_is_name, is_tag_neg$name),] %>%
#         t() %>%
#         as.data.frame() %>%
#         tibble::rownames_to_column(var = "sample_id")
#       
#       colnames(temp_data1)[2] <- "intensity"
#       
#       ###raw data
#       temp_name <- lipid_tag_neg$name[j]
#       temp_data2 <-
#         lipid_table_neg[j, ] %>%
#         t() %>%
#         as.data.frame() %>%
#         tibble::rownames_to_column(var = "sample_id")
#       
#       colnames(temp_data2)[2] <- "intensity"
#       
#       
#       temp_data3 <-
#         express_data_abs_um_neg[match(temp_name, variable_info_abs_neg$name), ] %>%
#         t() %>%
#         as.data.frame() %>%
#         tibble::rownames_to_column(var = "sample_id")
#       
#       colnames(temp_data3)[2] <- "intensity"
#       
#       temp_data <-
#         rbind(
#           data.frame(temp_data1, from = "is"),
#           data.frame(temp_data2, from = "raw"),
#           data.frame(temp_data3, from = "final")
#         ) %>%
#         dplyr::mutate(from = factor(from, levels = c("is", "raw", "final")))
#       
#       temp_plot <-
#         temp_data %>%
#         ggplot(aes(sample_id, intensity)) +
#         geom_point(size = 4, shape = 21, aes(fill = from)) +
#         ggrepel::geom_text_repel(aes(label = round(intensity, 4))) +
#         ggsci::scale_fill_d3() +
#         scale_y_continuous(expand = expansion(mult = c(0.5, 0.5))) +
#         theme_bw() +
#         labs(x = "", y = "Intensity") +
#         theme(axis.text.x = element_text(
#           angle = 45,
#           hjust = 1,
#           vjust = 1
#         )) +
#         facet_wrap(facets = vars(from),
#                    ncol = 1,
#                    scales = "free_y")
#       
#       temp_name <- paste(temp_name, ".pdf", sep = "")
#       temp_name <-
#         stringr::str_replace_all(temp_name, "\\/", "_")
#       ggsave(
#         temp_plot,
#         filename = file.path("Result/intensity_plot", i, temp_name),
#         width = 14,
#         height = 14
#       )
#     }
#   }
# }
# 
# 
# #####Class plot
# dir.create("Result/class_plot")
# 
# ####each class
# lipid_data_class <-
#   readxl::read_xlsx("Result/lipid_data_class_um.xlsx")
# 
# for (i in 1:nrow(lipid_data_class)) {
#   cat(i, " ")
#   temp_data <-
#     lipid_data_class[i, -1] %>%
#     t() %>%
#     as.data.frame() %>%
#     tibble::rownames_to_column(var = "sample_id")
#   colnames(temp_data)[2] <- "intensity"
#   
#   temp_plot <-
#     temp_data %>%
#     ggplot(aes(sample_id, intensity)) +
#     geom_point(size = 4,
#                shape = 21,
#                fill = "orange") +
#     ggrepel::geom_text_repel(aes(label = round(intensity, 4))) +
#     ggsci::scale_fill_d3() +
#     scale_y_continuous(expand = expansion(mult = c(0.5, 0.5))) +
#     theme_bw() +
#     labs(x = "", y = "Intensity") +
#     theme(axis.text.x = element_text(
#       angle = 45,
#       hjust = 1,
#       vjust = 1
#     ))
#   
#   ggsave(
#     temp_plot,
#     filename = file.path(
#       "Result/class_plot",
#       paste(lipid_data_class$X1[i], ".pdf",
#             sep = "")
#     ),
#     width = 7,
#     height = 7
#   )
#   
# }
# 
# 
# 
