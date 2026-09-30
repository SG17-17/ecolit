# Sensitivity analysis for the final 16-item PEB CFA.
# AIC/BIC and robust chi-square difference tests are comparable only within
# the same observed-item set. Across different item sets, fit indices are
# descriptive and do not establish that an item should be retained or removed.

uji_item_peb <- function(data, final_fit, out_file) {
  make_model <- function(soc = character(), env = character(), add18 = TRUE,
                         cov9_10 = FALSE, cov17_18 = FALSE) {
    soc_items <- c("PEB_6", "PEB_7", "PEB_8", soc)
    civic_items <- c("PEB_11", "PEB_12", "PEB_13", "PEB_14", "PEB_15", env)
    land_items <- c("PEB_16", "PEB_17", if (add18) "PEB_18", "PEB_19")
    paste0(
      "ConsLife =~ PEB_1 + PEB_3 + PEB_4 + PEB_5\n",
      "SocEnv =~ ", paste(soc_items, collapse = " + "), "\n",
      "EnvCit =~ ", paste(civic_items, collapse = " + "), "\n",
      "LandStew =~ ", paste(land_items, collapse = " + "), "\n",
      "PEB =~ ConsLife + SocEnv + EnvCit + LandStew\n",
      if (cov9_10) "PEB_9 ~~ PEB_10\n" else "",
      if (cov17_18) "PEB_17 ~~ PEB_18\n" else ""
    )
  }

  specs <- list(
    Tanpa_PEB18 = make_model(add18 = FALSE),
    Tambah_PEB9 = make_model(soc = "PEB_9"),
    PEB9_di_EnvCit = make_model(env = "PEB_9"),
    PEB9_dua_faktor = make_model(soc = "PEB_9", env = "PEB_9"),
    Tambah_PEB10 = make_model(soc = "PEB_10"),
    Tambah_PEB9_10 = make_model(soc = c("PEB_9", "PEB_10")),
    Tambah_PEB9_10_Kov = make_model(
      soc = c("PEB_9", "PEB_10"), cov9_10 = TRUE
    ),
    PEB17_18_Kov = make_model(cov17_18 = TRUE),
    Tambah_Semua_Kov = make_model(
      soc = c("PEB_9", "PEB_10"), add18 = TRUE,
      cov9_10 = TRUE, cov17_18 = TRUE
    )
  )
  fits <- lapply(
    specs,
    function(model) lavaan::cfa(
      model, data = data, estimator = "MLR", std.lv = TRUE
    )
  )
  fits <- c(list(Final_16 = final_fit), fits)

  admissible <- vapply(
    fits,
    function(fit) isTRUE(lavaan::lavInspect(fit, "converged")) &&
      isTRUE(lavaan::lavInspect(fit, "post.check")),
    logical(1)
  )
  if (!all(admissible)) warning(
    "Fit model sensitivitas PEB tidak ditafsirkan: ",
    paste(names(fits)[!admissible], collapse = ", ")
  )

  parameter <- function(fit, item, factor = NULL) {
    pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
    factor_ok <- if (is.null(factor)) rep(TRUE, nrow(pe)) else pe$lhs == factor
    x <- pe$std.all[
      pe$op == "=~" & pe$rhs == item & factor_ok
    ]
    if (length(x) == 1L) unname(x) else NA_real_
  }
  residual_mi <- function(fit, item1, item2) {
    mi <- lavaan::modificationIndices(fit)
    x <- mi$mi[
      mi$op == "~~" &
        ((mi$lhs == item1 & mi$rhs == item2) |
           (mi$lhs == item2 & mi$rhs == item1))
    ]
    if (length(x) == 1L) unname(x) else NA_real_
  }
  fit_row <- function(fit, name) {
    if (!admissible[[name]]) {
      return(data.frame(
        Model = name,
        N = lavaan::lavInspect(fit, "nobs"),
        Item_Count = length(lavaan::lavNames(fit, type = "ov")),
        Admissible = FALSE,
        Chisq_Scaled = NA_real_, df = NA_real_,
        CFI_Robust = NA_real_, TLI_Robust = NA_real_,
        RMSEA_Robust = NA_real_, SRMR = NA_real_,
        AIC = NA_real_, BIC = NA_real_,
        Loading_PEB9 = NA_real_, Loading_PEB10 = NA_real_,
        Loading_PEB9_EnvCit = NA_real_,
        Loading_PEB18 = NA_real_,
        MI_PEB9_PEB15 = NA_real_, MI_PEB10_PEB11 = NA_real_,
        MI_PEB10_PEB15 = NA_real_, MI_PEB17_PEB18 = NA_real_,
        MI_PEB12_PEB18 = NA_real_,
        Delta_Chisq_Robust = NA_real_, p_Delta_Robust = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    fm <- lavaan::fitMeasures(fit)
    data.frame(
      Model = name,
      N = lavaan::lavInspect(fit, "nobs"),
      Item_Count = length(lavaan::lavNames(fit, type = "ov")),
      Admissible = TRUE,
      Chisq_Scaled = fm[["chisq.scaled"]],
      df = fm[["df"]],
      CFI_Robust = fm[["cfi.robust"]],
      TLI_Robust = fm[["tli.robust"]],
      RMSEA_Robust = fm[["rmsea.robust"]],
      SRMR = fm[["srmr"]],
      AIC = fm[["aic"]],
      BIC = fm[["bic"]],
      Loading_PEB9 = parameter(fit, "PEB_9", "SocEnv"),
      Loading_PEB10 = parameter(fit, "PEB_10"),
      Loading_PEB9_EnvCit = parameter(fit, "PEB_9", "EnvCit"),
      Loading_PEB18 = parameter(fit, "PEB_18"),
      MI_PEB9_PEB15 = residual_mi(fit, "PEB_9", "PEB_15"),
      MI_PEB10_PEB11 = residual_mi(fit, "PEB_10", "PEB_11"),
      MI_PEB10_PEB15 = residual_mi(fit, "PEB_10", "PEB_15"),
      MI_PEB17_PEB18 = residual_mi(fit, "PEB_17", "PEB_18"),
      MI_PEB12_PEB18 = residual_mi(fit, "PEB_12", "PEB_18"),
      Delta_Chisq_Robust = NA_real_,
      p_Delta_Robust = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  table <- do.call(rbind, Map(fit_row, fits, names(fits)))
  rownames(table) <- NULL
  for (pair in list(
    c("Tambah_PEB9_10", "Tambah_PEB9_10_Kov"),
    c("Final_16", "PEB17_18_Kov")
  )) {
    if (!all(admissible[pair])) next
    test <- lavaan::lavTestLRT(fits[[pair[1]]], fits[[pair[2]]])
    target <- match(pair[2], table$Model)
    table$Delta_Chisq_Robust[target] <- test[["Chisq diff"]][2]
    table$p_Delta_Robust[target] <- test[["Pr(>Chisq)"]][2]
  }

  numeric_columns <- vapply(table, is.numeric, logical(1))
  numeric_columns[c("N", "Item_Count", "df", "p_Delta_Robust")] <- FALSE
  table[numeric_columns] <- lapply(table[numeric_columns], round, digits = 4)
  if (!is.null(out_file)) {
    dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(table, out_file, row.names = FALSE)
  }
  invisible(table)
}
