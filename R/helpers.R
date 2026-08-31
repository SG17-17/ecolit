library(knitr)
library(lavaan)
library(semTools)

# Helper function: Tabel Demografi
print_demo_table <- function(variabel, labels) {
    freq <- table(variabel, useNA = "ifany")
    pct  <- prop.table(freq) * 100
    display <- sapply(names(freq), function(x) {
        if (is.na(x)) return("Missing")
        if (x %in% names(labels)) return(labels[[x]])
        return(x)
    })
    data.frame(
        Kategori   = unname(display),
        Frekuensi  = as.numeric(freq),
        Persentase = paste0(round(pct, 2), "%"),
        check.names = FALSE
    )
}

# Helper function: Menghasilkan dataframe reliabilitas
get_rel_df <- function(fit) {
  if (is.null(fit)) return(NULL)
  a <- compRelSEM(fit, tau.eq = TRUE)
  o <- compRelSEM(fit, tau.eq = FALSE)
  ave <- AVE(fit)
  data.frame(
      Faktor = names(a),
      Alpha  = round(as.numeric(a), 3),
      Omega  = round(as.numeric(o[names(a)]), 3),
      AVE    = round(as.numeric(ave[names(a)]), 3),
      check.names = FALSE
  )
}

# Helper function: Tabel Reliabilitas (Markdown HTML)
print_rel_table <- function(fit) {
  res <- get_rel_df(fit)
  if (is.null(res)) return(cat("Tidak dapat menghitung reliabilitas karena model CFA tidak konvergen.\n"))
  return(knitr::kable(res))
}

# Helper function: Menghasilkan dataframe fit model CFA
get_cfa_fit_df <- function(fit, model_name) {
  if (is.null(fit)) return(NULL)
  
  # Deteksi estimator yang digunakan
  est <- lavInspect(fit, "options")$estimator.orig
  if (est == "MLR") {
    fits <- fitMeasures(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust", "tli.robust", "rmsea.robust", "srmr"))
    
    data.frame(
      Model = model_name,
      `chisq.scaled` = round(as.numeric(fits["chisq.scaled"]), 2),
      df = as.numeric(fits["df.scaled"]),
      pvalue = round(as.numeric(fits["pvalue.scaled"]), 3),
      `cfi.robust` = round(as.numeric(fits["cfi.robust"]), 3),
      `tli.robust` = round(as.numeric(fits["tli.robust"]), 3),
      `rmsea.robust` = round(as.numeric(fits["rmsea.robust"]), 3),
      srmr = round(as.numeric(fits["srmr"]), 3),
      check.names = FALSE,
      row.names = NULL
    )
  } else {
    fits <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
    
    data.frame(
      Model = model_name,
      `Chi-Square` = round(as.numeric(fits["chisq"]), 2),
      df = as.numeric(fits["df"]),
      p = round(as.numeric(fits["pvalue"]), 3),
      CFI = round(as.numeric(fits["cfi"]), 3),
      TLI = round(as.numeric(fits["tli"]), 3),
      RMSEA = round(as.numeric(fits["rmsea"]), 3),
      SRMR = round(as.numeric(fits["srmr"]), 3),
      check.names = FALSE,
      row.names = NULL
    )
  }
}

# Helper function: Tabel Fit Model CFA (Markdown HTML)
print_fit_table <- function(fit, model_name) {
  res <- get_cfa_fit_df(fit, model_name)
  if (is.null(res)) return(cat("Tidak dapat menghitung fit model karena model CFA tidak konvergen.\n"))
  return(knitr::kable(res))
}

library(semPlot)

# Helper function: Visualisasi Model CFA (semPlot)
plot_cfa <- function(fit, title_text) {
  # Ambil nilai MLR Robust secara otomatis
  est <- lavInspect(fit, "options")$estimator.orig
  if (est == "MLR") {
    fit_idx <- fitMeasures(fit, c("chisq.scaled", "cfi.robust", "tli.robust", "rmsea.robust", "srmr"))
  } else {
    fit_idx <- fitMeasures(fit, c("chisq", "cfi", "tli", "rmsea", "srmr"))
    names(fit_idx) <- c("chisq.scaled", "cfi.robust", "tli.robust", "rmsea.robust", "srmr")
  }
  
  # Render semPaths
  semPlot::semPaths(fit, title = FALSE, whatLabels = "std.all", edge.label.cex = 0.6,
         color = "white", edge.color = "black", sizeMan = 3.5, sizeLat = 8,
         layout = "tree2", rotation = 2, style = "lisrel",
         curve = 2.5, asize = 2, residuals = FALSE, mar = c(3,5,3,5))
  
  # Judul & Legend Fit Indices
  title(title_text, cex.main = 1.0, font.main = 2, adj = 0)
  op <- par(family = "mono")
  legend("bottomleft", inset = c(0.1, 0.15), legend = c(
      "Fit Indices (MLR):", 
      sprintf("Chi-Square: %.2f", fit_idx["chisq.scaled"]),
      sprintf("CFI: %.3f", fit_idx["cfi.robust"]), 
      sprintf("TLI: %.3f", fit_idx["tli.robust"]),
      sprintf("RMSEA: %.3f", fit_idx["rmsea.robust"]), 
      sprintf("SRMR: %.3f", fit_idx["srmr"])
  ), bty = "n", cex = 0.8)
  par(op)
}
