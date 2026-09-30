# Re-create the user's original semPlot design from the final CFA fit objects.
# Original style settings are archived in R/esi.r, R/psmls.r, R/peb.r,
# and R/PWB2ndOrder.r inside arsip_analisis_lama.zip.

buat_plot_cfa <- function(final_fits, run_dir, nama_file = "plot_model.png", judul = NULL) {
  if (!requireNamespace("semPlot", quietly = TRUE)) {
    stop("Package semPlot diperlukan untuk membuat plot CFA.")
  }
  models <- c("ESI", "PSMLS", "PEB", "PWB")
  if (!length(final_fits) || !all(names(final_fits) %in% models)) {
    stop("Nama model CFA harus ESI, PSMLS, PEB, atau PWB.")
  }
  models <- models[models %in% names(final_fits)]

  configs <- list(
    ESI = list(
      title = "CFA Model ESI (6 Item, 1 Faktor)",
      size_man = 5, size_man2 = 3, size_lat = 9,
      edge_cex = 0.6, curve = 0.8, legend_cex = 0.9,
      full_labels = TRUE
    ),
    PSMLS = list(
      title = "CFA Model PSMLS (14 Item, 4 Faktor + 1 Second-Order)",
      size_man = 7, size_man2 = 3, size_lat = 8,
      edge_cex = 0.6, curve = 2.5, legend_cex = 0.9,
      full_labels = FALSE
    ),
    PEB = list(
      title = "CFA Model PEB (16 Item, 4 Faktor + 1 Second-Order)",
      size_man = 5, size_man2 = 2.5, size_lat = 8,
      edge_cex = 0.5, curve = 2.5, legend_cex = 0.9,
      full_labels = TRUE
    ),
    PWB = list(
      title = "CFA Second-Order PWB (21 Item, 6 Faktor)",
      size_man = 6, size_man2 = 2.5, size_lat = 8,
      edge_cex = 0.5, curve = 2.5, legend_cex = 0.8,
      full_labels = TRUE
    )
  )

  plot_one <- function(model_name) {
    fit <- final_fits[[model_name]]
    config <- configs[[model_name]]
    plot_tahap <- nama_file != "plot_model.png"
    out_dir <- file.path(run_dir, "cfa", tolower(model_name))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    plot_file <- file.path(out_dir, nama_file)

    grDevices::png(plot_file, width = 3200, height = 2200, res = 300)
    on.exit(grDevices::dev.off(), add = TRUE)

    plot_args <- list(
      object = fit,
      title = FALSE,
      whatLabels = "std.all",
      edge.label.cex = config$edge_cex,
      color = "white",
      edge.color = "black",
      shapeMan = "rectangle",
      sizeMan = config$size_man,
      sizeMan2 = config$size_man2,
      sizeLat = config$size_lat,
      layout = "tree2",
      rotation = 2,
      style = "lisrel",
      curve = config$curve,
      asize = 2,
      residuals = TRUE,
      mar = c(3, 5, 3, 5)
    )
    if (plot_tahap && model_name == "PWB") {
      plot_args$sizeMan <- 3.5
      plot_args$sizeMan2 <- 2
      plot_args$sizeLat <- 6
      plot_args$edge.label.cex <- 0.4
      plot_args$label.cex <- 0.7
    }
    if (config$full_labels) {
      plot_args$nodeLabels <- c(
        lavaan::lavNames(fit, type = "ov"),
        lavaan::lavNames(fit, type = "lv")
      )
      plot_args$nCharNodes <- 0
    }
    if (plot_tahap && model_name == "PWB") {
      plot_args$nodeLabels <- c(
        sub("^PWB_", "", lavaan::lavNames(fit, type = "ov")),
        lavaan::lavNames(fit, type = "lv")
      )
    }
    do.call(semPlot::semPaths, plot_args)

    plot_title <- if (!is.null(judul) && !is.null(judul[[model_name]])) judul[[model_name]] else config$title
    graphics::mtext(
      plot_title, side = 3, line = 1.5,
      adj = 0.03, cex = 1.0, font = 2
    )
    fit_idx <- lavaan::fitMeasures(
      fit, c("chisq.scaled", "cfi.robust", "tli.robust", "rmsea.robust", "srmr")
    )
    legend_text <- c(
      "Fit Indices:",
      sprintf("Chi-Square: %.2f", fit_idx[["chisq.scaled"]]),
      sprintf("CFI       : %.3f", fit_idx[["cfi.robust"]]),
      sprintf("TLI       : %.3f", fit_idx[["tli.robust"]]),
      sprintf("RMSEA     : %.3f", fit_idx[["rmsea.robust"]]),
      sprintf("SRMR      : %.3f", fit_idx[["srmr"]])
    )
    if (!plot_tahap) {
      old_par <- graphics::par(family = "mono")
      graphics::legend(
        "bottomleft", inset = c(0.03, 0.15), legend = legend_text,
        bty = "n", cex = config$legend_cex, text.font = 1, xpd = TRUE
      )
      graphics::par(old_par)
    }
    invisible(plot_file)
  }

  files <- vapply(models, plot_one, character(1))
  invisible(files)
}
