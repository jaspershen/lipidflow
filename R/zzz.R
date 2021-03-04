.onAttach <- function(libname, pkgname) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()
  
  crayon::num_colors(TRUE)
  lipidflow_attach()
  
  packageStartupMessage(
    crayon::green(
      "lipidflow,
More information can be found at https://jaspershen.github.io/lipidflow/
If you use lipidflow in you publication, please cite this publication:
Metabolic reaction network-based recursive metabolite annotation for untargeted metabolomics.
Authors: Xiaotao Shen & Chuchu Wang
Maintainer: Xiaotao Shen (shenxt1990@163.com)."
    )
  )
}

globalVariables(
  names = c(
    "Index",
    "Compound Name",
    "RT POS (second)",
    "RT NEG (second)",
    "RT NEG (min)",
    "Adduct NEG",
    "RT POS (min)",
    "rt",
    "Adduct POS",
    "name",
    "mean.int",
    "Class",
    "sample_id",
    "value",
    "um",
    "adduct",
    "exact.mass",
    "mz",
    "peak_name",
    "column",
    "into",
    "median",
    "rt2",
    "y",
    "compound_name",
    "begin_rt",
    "Index1",
    "rt error",
    "contains",
    "variable_info_abs",
    "express_data_abs_ug_ml",
    "express_data_abs_um",
    "is_table",
    "is_tag",
    "lipid_table",
    "lipid_tag",
    "from",
    "sample_id",
    "intensity",
    "V1",
    "V2",
    "old_name",
    "GroupTopPos[c]",
    "ARatio.0.",
    "HDiff.0.",
    "lipid_raw_name",
    "rentention_time",
    "FattyAcid",
    "IonFormula",
    "sample.name",
    "group",
    "sample_info_pos",
    "sample_info_neg",
    "fit_result",
    ""
  )
)


is_attached <- function(x) {
  paste0("package:", x) %in% search()
}