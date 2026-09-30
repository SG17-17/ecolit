# ==============================================================================
# GENERAL HELPERS - ANECO
# ==============================================================================
# Fungsi umum project:
# - Tabel demografi
# - Reliability (Alpha, Omega, AVE)
# - Ringkasan fit CFA untuk tabel
# - Visualisasi model CFA
#
# Catatan:
# Output TXT CFA yang rinci berada di R/cfa_helpers.R
# ==============================================================================

library(knitr)
library(lavaan)
library(semTools)
library(semPlot)


# ==============================================================================
# 1. HELPER FUNCTION: TABEL DEMOGRAFI
# ==============================================================================

print_demo_table <- function(
  variabel,
  labels
) {

  freq <- table(
    variabel,
    useNA = "ifany"
  )

  pct <- prop.table(freq) * 100


  display <- sapply(
    names(freq),
    function(x) {

      if (is.na(x)) {
        return("Missing")
      }

      if (x %in% names(labels)) {
        return(labels[[x]])
      }

      return(x)
    }
  )


  data.frame(
    Category = unname(display),
    Frequency = as.numeric(freq),
    Percentage = paste0(
      round(pct, 2),
      "%"
    ),
    check.names = FALSE
  )
}


# ==============================================================================
# 2. HELPER FUNCTION: RELIABILITAS
# ==============================================================================

get_rel_df <- function(
  fit,
  exclude_factors = c("ReverseMethod")
) {

  if (is.null(fit)) {
    return(NULL)
  }


  # ---------------------------------------------------------------------------
  # Ambil nama latent factors
  # ---------------------------------------------------------------------------

  latent_factors <- lavNames(
    fit,
    type = "lv"
  )


  # Faktor yang benar-benar konstruk substantif
  substantive_factors <- setdiff(
    latent_factors,
    exclude_factors
  )


  # ---------------------------------------------------------------------------
  # Composite Reliability
  # ---------------------------------------------------------------------------

  alpha_all <- tryCatch(

    compRelSEM(
      fit,
      tau.eq = TRUE
    ),

    error = function(e) {
      NULL
    }
  )


  omega_all <- tryCatch(

    compRelSEM(
      fit,
      tau.eq = FALSE
    ),

    error = function(e) {
      NULL
    }
  )


  # ---------------------------------------------------------------------------
  # Ambil standardized loading
  # ---------------------------------------------------------------------------

  pe <- parameterEstimates(
    fit,
    standardized = TRUE
  )


  loadings <- pe[
    pe$op == "=~" &
      pe$lhs %in% substantive_factors,
    c(
      "lhs",
      "rhs",
      "std.all"
    )
  ]


  # ---------------------------------------------------------------------------
  # AVE manual
  #
  # AVE = mean(lambda^2)
  #
  # Hanya loading faktor substantif yang dihitung.
  # ReverseMethod tidak dimasukkan ke perhitungan AVE.
  # ---------------------------------------------------------------------------

  ave_values <- sapply(

    substantive_factors,

    function(factor_name) {

      x <- loadings$std.all[
        loadings$lhs == factor_name
      ]


      x <- x[
        is.finite(x)
      ]


      if (length(x) == 0) {
        return(NA_real_)
      }


      mean(
        x^2,
        na.rm = TRUE
      )
    }
  )


  # ---------------------------------------------------------------------------
  # Alpha
  # ---------------------------------------------------------------------------

  alpha_values <- sapply(

    substantive_factors,

    function(factor_name) {

      if (
        is.null(alpha_all) ||
        !(factor_name %in% names(alpha_all))
      ) {

        return(NA_real_)
      }


      as.numeric(
        alpha_all[factor_name]
      )
    }
  )


  # ---------------------------------------------------------------------------
  # Omega
  # ---------------------------------------------------------------------------

  omega_values <- sapply(

    substantive_factors,

    function(factor_name) {

      if (
        is.null(omega_all) ||
        !(factor_name %in% names(omega_all))
      ) {

        return(NA_real_)
      }


      as.numeric(
        omega_all[factor_name]
      )
    }
  )


  # ---------------------------------------------------------------------------
  # Output
  # ---------------------------------------------------------------------------

  data.frame(

    Factor = substantive_factors,

    Alpha = round(
      alpha_values,
      3
    ),

    Omega = round(
      omega_values,
      3
    ),

    AVE = round(
      ave_values,
      3
    ),

    check.names = FALSE,
    row.names = NULL
  )
}


# ==============================================================================
# 3. HELPER FUNCTION: TABEL RELIABILITAS
# ==============================================================================

print_rel_table <- function(fit) {

  res <- get_rel_df(
    fit
  )


  if (is.null(res)) {

    return(
      cat(
        "Unable to compute reliability because the CFA model did not converge.\n"
      )
    )
  }


  knitr::kable(
    res
  )
}


# ==============================================================================
# 4. HELPER FUNCTION: DATAFRAME FIT MODEL CFA
# ==============================================================================

get_cfa_fit_df <- function(
  fit,
  model_name
) {

  if (is.null(fit)) {
    return(NULL)
  }


  # ---------------------------------------------------------------------------
  # Deteksi estimator
  # ---------------------------------------------------------------------------

  options_fit <- lavInspect(
    fit,
    "options"
  )


  est <- options_fit$estimator.orig


  # ---------------------------------------------------------------------------
  # Jika estimator MLR
  # ---------------------------------------------------------------------------

  if (
    !is.null(est) &&
    est == "MLR"
  ) {

    fits <- fitMeasures(
      fit,
      c(
        "chisq.scaled",
        "df.scaled",
        "pvalue.scaled",
        "cfi.robust",
        "tli.robust",
        "rmsea.robust",
        "srmr"
      )
    )


    data.frame(

      Model = model_name,

      `chisq.scaled` = round(
        as.numeric(
          fits["chisq.scaled"]
        ),
        2
      ),

      df = as.numeric(
        fits["df.scaled"]
      ),

      pvalue = round(
        as.numeric(
          fits["pvalue.scaled"]
        ),
        3
      ),

      `cfi.robust` = round(
        as.numeric(
          fits["cfi.robust"]
        ),
        3
      ),

      `tli.robust` = round(
        as.numeric(
          fits["tli.robust"]
        ),
        3
      ),

      `rmsea.robust` = round(
        as.numeric(
          fits["rmsea.robust"]
        ),
        3
      ),

      srmr = round(
        as.numeric(
          fits["srmr"]
        ),
        3
      ),

      check.names = FALSE,
      row.names = NULL
    )

  } else {

    # -------------------------------------------------------------------------
    # Estimator non-MLR
    # -------------------------------------------------------------------------

    fits <- fitMeasures(
      fit,
      c(
        "chisq",
        "df",
        "pvalue",
        "cfi",
        "tli",
        "rmsea",
        "srmr"
      )
    )


    data.frame(

      Model = model_name,

      `Chi-Square` = round(
        as.numeric(
          fits["chisq"]
        ),
        2
      ),

      df = as.numeric(
        fits["df"]
      ),

      p = round(
        as.numeric(
          fits["pvalue"]
        ),
        3
      ),

      CFI = round(
        as.numeric(
          fits["cfi"]
        ),
        3
      ),

      TLI = round(
        as.numeric(
          fits["tli"]
        ),
        3
      ),

      RMSEA = round(
        as.numeric(
          fits["rmsea"]
        ),
        3
      ),

      SRMR = round(
        as.numeric(
          fits["srmr"]
        ),
        3
      ),

      check.names = FALSE,
      row.names = NULL
    )
  }
}


# ==============================================================================
# 5. HELPER FUNCTION: TABEL FIT MODEL CFA
# ==============================================================================

print_fit_table <- function(
  fit,
  model_name
) {

  res <- get_cfa_fit_df(
    fit,
    model_name
  )


  if (is.null(res)) {

    return(
      cat(
        "Unable to compute model fit because the CFA model did not converge.\n"
      )
    )
  }


  knitr::kable(
    res
  )
}


# ==============================================================================
# 6. HELPER FUNCTION: VISUALISASI MODEL CFA
# ==============================================================================

plot_cfa <- function(
  fit,
  title_text
) {

  # ---------------------------------------------------------------------------
  # Deteksi estimator
  # ---------------------------------------------------------------------------

  options_fit <- lavInspect(
    fit,
    "options"
  )


  est <- options_fit$estimator.orig


  # ---------------------------------------------------------------------------
  # Ambil fit indices
  # ---------------------------------------------------------------------------

  if (
    !is.null(est) &&
    est == "MLR"
  ) {

    fit_idx <- fitMeasures(
      fit,
      c(
        "chisq.scaled",
        "cfi.robust",
        "tli.robust",
        "rmsea.robust",
        "srmr"
      )
    )

  } else {

    fit_idx <- fitMeasures(
      fit,
      c(
        "chisq",
        "cfi",
        "tli",
        "rmsea",
        "srmr"
      )
    )


    names(fit_idx) <- c(
      "chisq.scaled",
      "cfi.robust",
      "tli.robust",
      "rmsea.robust",
      "srmr"
    )
  }


  # ---------------------------------------------------------------------------
  # Render semPaths
  # ---------------------------------------------------------------------------

  semPlot::semPaths(

    fit,

    title = FALSE,

    whatLabels = "std.all",

    edge.label.cex = 0.6,

    color = "white",

    edge.color = "black",

    sizeMan = 3.5,

    sizeLat = 8,

    layout = "tree2",

    rotation = 2,

    style = "lisrel",

    curve = 2.5,

    asize = 2,

    residuals = FALSE,

    mar = c(
      3,
      5,
      3,
      5
    )
  )


  # ---------------------------------------------------------------------------
  # Judul
  # ---------------------------------------------------------------------------

  title(
    title_text,
    cex.main = 1.0,
    font.main = 2,
    adj = 0
  )


  # ---------------------------------------------------------------------------
  # Legend fit indices
  # ---------------------------------------------------------------------------

  op <- par(
    family = "mono"
  )


  legend(

    "bottomleft",

    inset = c(
      0.1,
      0.15
    ),

    legend = c(

      "Fit Indices:",

      sprintf(
        "Chi-Square: %.2f",
        fit_idx["chisq.scaled"]
      ),

      sprintf(
        "CFI: %.3f",
        fit_idx["cfi.robust"]
      ),

      sprintf(
        "TLI: %.3f",
        fit_idx["tli.robust"]
      ),

      sprintf(
        "RMSEA: %.3f",
        fit_idx["rmsea.robust"]
      ),

      sprintf(
        "SRMR: %.3f",
        fit_idx["srmr"]
      )
    ),

    bty = "n",

    cex = 0.8
  )


  par(op)
}

