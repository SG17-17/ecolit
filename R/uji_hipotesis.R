# Deskriptif, asumsi regresi, Model 7, bootstrap, grafik, dan laporan.

# 1. Deskriptif dan korelasi ----

# A single score representation is needed for the descriptive/correlation table.
# These "primary TSFS scores" use:
#   ESI   = regression score from ESI+PSMLS predictor CFA
#   PSMLS = regression score from ESI+PSMLS predictor CFA
#   PEB   = Bartlett outcome score
#   PWB   = Bartlett outcome score
#
# Equation-specific variants remain available in the TSFS data file.

descriptive_scores <- data.frame(
  ESI = tsfs$ESI_R_M,
  PSMLS = tsfs$PSMLS_R_M,
  PEB = tsfs$PEB_B,
  PWB = tsfs$PWB_B
)

descriptives <- do.call(
  rbind,
  lapply(
    names(
      descriptive_scores
    ),
    function(v) {
      x <- descriptive_scores[[v]]

      data.frame(
        Variable = v,
        N = length(x),
        Mean = mean(x),
        SD = stats::sd(x),
        Min = min(x),
        Max = max(x),
        stringsAsFactors = FALSE
      )
    }
  )
)

cor_matrix <- stats::cor(
  descriptive_scores
)

cor_p <- matrix(
  NA_real_,
  nrow = ncol(descriptive_scores),
  ncol = ncol(descriptive_scores),
  dimnames = list(
    names(descriptive_scores),
    names(descriptive_scores)
  )
)

for (
  i in seq_len(
    ncol(
      descriptive_scores
    )
  )
) {
  for (
    j in seq_len(
      ncol(
        descriptive_scores
      )
    )
  ) {
    if (i == j) {
      cor_p[i, j] <- NA_real_
    } else {
      cor_p[i, j] <- stats::cor.test(
        descriptive_scores[[i]],
        descriptive_scores[[j]]
      )$p.value
    }
  }
}

utils::write.csv(
  round_numeric(descriptives),
  file.path(
    OUT,
    "descriptive_statistics_primary_TSFS_scores.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  round(
    cor_matrix,
    6
  ),
  file.path(
    OUT,
    "correlation_matrix_primary_TSFS_scores.csv"
  )
)

utils::write.csv(
  cor_p,
  file.path(
    OUT,
    "correlation_pvalues_primary_TSFS_scores.csv"
  )
)


# 2. Persamaan Model 7 ----

# Mediator equation:
# PEB Bartlett outcome ~ ESI regression + PSMLS regression + interaction
model_M <- stats::lm(
  PEB_B ~ ESI_c * PSMLS_c,
  data = tsfs
)

# Outcome equation:
# PWB Bartlett outcome ~ PEB regression + ESI regression
# PSMLS is intentionally NOT included as a direct predictor of PWB.
model_Y <- stats::lm(
  PWB_B ~ PEB_R_Y + ESI_R_Y,
  data = tsfs
)


# 3. Diagnostik asumsi regresi ----

bp_M <- lmtest::bptest(
  model_M
)

bp_Y <- lmtest::bptest(
  model_Y
)

if (
  !is.finite(
    bp_M$p.value
  ) ||
    !is.finite(
      bp_Y$p.value
    )
) {
  stop(
    "Breusch-Pagan menghasilkan p-value non-finite. ",
    "Periksa varians TSFS scores dan model regresi."
  )
}

use_hc3_M <- bp_M$p.value < .05
use_hc3_Y <- bp_Y$p.value < .05

# RESET memeriksa spesifikasi fungsi; VIF dan Cook's D membantu menilai
# multikolinearitas dan pengaruh observasi tanpa menghapus data otomatis.
vif_maksimum <- function(fit) {
  x <- stats::model.matrix(fit)
  x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
  if (ncol(x) == 1L) {
    return(1)
  }
  vif <- vapply(seq_len(ncol(x)), function(j) {
    y <- x[, j]
    other <- cbind(1, x[, -j, drop = FALSE])
    residual <- stats::lm.fit(other, y)$residuals
    sum((y - mean(y))^2) / sum(residual^2)
  }, numeric(1))
  max(vif)
}

reset_hc3 <- function(fit) {
  lmtest::resettest(
    fit,
    power = 2:3,
    type = "fitted",
    vcov = function(x) sandwich::vcovHC(x, type = "HC3")
  )
}

reset_M <- reset_hc3(model_M)
reset_Y <- reset_hc3(model_Y)
cook_M <- stats::cooks.distance(model_M)
cook_Y <- stats::cooks.distance(model_Y)

diagnostik_asumsi <- data.frame(
  Equation = c(
    "PEB mediator equation",
    "PWB outcome equation"
  ),
  N = c(stats::nobs(model_M), stats::nobs(model_Y)),
  BP = c(
    unname(
      bp_M$statistic
    ),
    unname(
      bp_Y$statistic
    )
  ),
  BP_df = c(
    unname(
      bp_M$parameter
    ),
    unname(
      bp_Y$parameter
    )
  ),
  BP_p = c(
    bp_M$p.value,
    bp_Y$p.value
  ),
  RESET_HC3_F = c(unname(reset_M$statistic), unname(reset_Y$statistic)),
  RESET_HC3_p = c(reset_M$p.value, reset_Y$p.value),
  Max_VIF = c(vif_maksimum(model_M), vif_maksimum(model_Y)),
  Max_Cooks_D = c(max(cook_M), max(cook_Y)),
  Cook_N_above_4_over_N = c(
    sum(cook_M > 4 / stats::nobs(model_M)),
    sum(cook_Y > 4 / stats::nobs(model_Y))
  ),
  Inference_SE = c(
    if (use_hc3_M) "HC3" else "OLS",
    if (use_hc3_Y) "HC3" else "OLS"
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  diagnostik_asumsi,
  file.path(
    OUT,
    "TSFS_assumption_diagnostics.csv"
  ),
  row.names = FALSE
)


# 4. Koefisien regresi ----

table_M <- if (use_hc3_M) {
  regression_table_hc3(
    model_M
  )
} else {
  regression_table_ols(
    model_M
  )
}

table_Y <- if (use_hc3_Y) {
  regression_table_hc3(
    model_Y
  )
} else {
  regression_table_ols(
    model_Y
  )
}

utils::write.csv(
  table_M,
  file.path(
    OUT,
    paste0(
      "TSFS_mediator_equation_",
      if (use_hc3_M) "HC3" else "OLS",
      ".csv"
    )
  ),
  row.names = FALSE
)

utils::write.csv(
  table_Y,
  file.path(
    OUT,
    paste0(
      "TSFS_outcome_equation_",
      if (use_hc3_Y) "HC3" else "OLS",
      ".csv"
    )
  ),
  row.names = FALSE
)

r2_table <- data.frame(
  Equation = c(
    "PEB mediator equation",
    "PWB outcome equation"
  ),
  R2 = c(
    summary(model_M)$r.squared,
    summary(model_Y)$r.squared
  ),
  Adjusted_R2 = c(
    summary(model_M)$adj.r.squared,
    summary(model_Y)$adj.r.squared
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  r2_table,
  file.path(
    OUT,
    "TSFS_R2.csv"
  ),
  row.names = FALSE
)


# 5. Simple slopes dan Johnson–Neyman ----

simple_slopes_selected <- function(
  model,
  w_values,
  use_hc3 = FALSE
) {
  V <- if (use_hc3) {
    sandwich::vcovHC(
      model,
      type = "HC3"
    )
  } else {
    stats::vcov(model)
  }

  co <- stats::coef(model)

  b1 <- co["ESI_c"]
  b3 <- co["ESI_c:PSMLS_c"]

  df_res <- stats::df.residual(model)
  crit <- stats::qt(
    .975,
    df = df_res
  )

  out <- lapply(
    names(
      w_values
    ),
    function(label) {
      w <- w_values[[label]]

      est <- b1 + b3 * w

      var_est <-
        V["ESI_c", "ESI_c"] +
        w^2 *
          V[
            "ESI_c:PSMLS_c",
            "ESI_c:PSMLS_c"
          ] +
        2 *
          w *
          V[
            "ESI_c",
            "ESI_c:PSMLS_c"
          ]

      se <- sqrt(
        var_est
      )

      tval <- est / se

      pval <- 2 *
        stats::pt(
          abs(tval),
          df = df_res,
          lower.tail = FALSE
        )

      data.frame(
        PSMLS_Level = label,
        PSMLS_c = w,
        Slope_ESI_to_PEB = est,
        SE = se,
        t = tval,
        p = pval,
        CI_L = est - crit * se,
        CI_U = est + crit * se,
        SE_Type = if (use_hc3) "HC3" else "OLS",
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(
    rbind,
    out
  )
}

moderator_values <- c(
  Low_minus_1SD = -PSMLS_SD,
  Mean = 0,
  High_plus_1SD = PSMLS_SD
)

simple_slopes <- simple_slopes_selected(
  model_M,
  moderator_values,
  use_hc3 = use_hc3_M
)

utils::write.csv(
  simple_slopes,
  file.path(
    OUT,
    "TSFS_simple_slopes.csv"
  ),
  row.names = FALSE
)

jn_output <- capture.output(
  interactions::sim_slopes(
    model_M,
    pred = ESI_c,
    modx = PSMLS_c,
    johnson_neyman = TRUE,
    robust = if (use_hc3_M) "HC3" else FALSE
  )
)

writeLines(
  jn_output,
  file.path(
    OUT,
    "TSFS_Johnson_Neyman.txt"
  )
)


# 6. Efek tidak langsung kondisional ----

a1 <- stats::coef(
  model_M
)["ESI_c"]

a3 <- stats::coef(
  model_M
)["ESI_c:PSMLS_c"]

b <- stats::coef(
  model_Y
)["PEB_R_Y"]

W_low <- -PSMLS_SD
W_mean <- 0
W_high <- PSMLS_SD

indirect_low <-
  (
    a1 +
      a3 *
        W_low
  ) *
    b

indirect_mean <-
  (
    a1 +
      a3 *
        W_mean
  ) *
    b

indirect_high <-
  (
    a1 +
      a3 *
        W_high
  ) *
    b

index_modmed <-
  a3 *
    b


# 7. Bootstrap efek tidak langsung ----
#
# The bootstrap resamples rows of the already-estimated TSFS score data and
# re-estimates both regression equations. The CFA measurement models are not
# re-estimated within each bootstrap sample.
# ==============================================================================

boot_summary <- data.frame()
boot_result <- NULL

if (RUN_BOOTSTRAP) {
  cat(
    "Menjalankan TSFS bootstrap ",
    BOOT_R,
    " resamples...\n",
    sep = ""
  )

  set.seed(
    BOOT_SEED
  )

  boot_result <- matrix(
    NA_real_,
    nrow = BOOT_R,
    ncol = 4
  )

  colnames(
    boot_result
  ) <- c(
    "Indirect_Low",
    "Indirect_Mean",
    "Indirect_High",
    "Index_ModMed"
  )

  for (
    i in seq_len(
      BOOT_R
    )
  ) {
    idx <- sample(
      seq_len(
        nrow(tsfs)
      ),
      size = nrow(tsfs),
      replace = TRUE
    )

    d <- tsfs[
      idx, ,
      drop = FALSE
    ]

    mM <- stats::lm(
      PEB_B ~ ESI_c * PSMLS_c,
      data = d
    )

    mY <- stats::lm(
      PWB_B ~ PEB_R_Y + ESI_R_Y,
      data = d
    )

    a1_i <- stats::coef(
      mM
    )["ESI_c"]

    a3_i <- stats::coef(
      mM
    )["ESI_c:PSMLS_c"]

    b_i <- stats::coef(
      mY
    )["PEB_R_Y"]

    boot_result[
      i,
    ] <- c(
      (
        a1_i +
          a3_i *
            W_low
      ) *
        b_i,
      (
        a1_i +
          a3_i *
            W_mean
      ) *
        b_i,
      (
        a1_i +
          a3_i *
            W_high
      ) *
        b_i,
      a3_i *
        b_i
    )
  }

  boot_summary <- data.frame(
    Effect = c(
      "Indirect Effect - Low PSMLS",
      "Indirect Effect - Mean PSMLS",
      "Indirect Effect - High PSMLS",
      "Index of Moderated Mediation"
    ),
    Estimate = c(
      indirect_low,
      indirect_mean,
      indirect_high,
      index_modmed
    ),
    Boot_SE = apply(
      boot_result,
      2,
      stats::sd,
      na.rm = TRUE
    ),
    Boot_LLCI = apply(
      boot_result,
      2,
      stats::quantile,
      probs = .025,
      na.rm = TRUE
    ),
    Boot_ULCI = apply(
      boot_result,
      2,
      stats::quantile,
      probs = .975,
      na.rm = TRUE
    ),
    stringsAsFactors = FALSE
  )

  utils::write.csv(
    boot_summary,
    file.path(
      OUT,
      "TSFS_bootstrap_indirect_effects.csv"
    ),
    row.names = FALSE
  )

  saveRDS(
    boot_result,
    file.path(
      OUT,
      "TSFS_bootstrap_raw.rds"
    )
  )
}

# 8. Grafik ----

if (RUN_PLOTS) {
  cat("Membuat TSFS moderation plots...\n")
  if (!file.exists("R/plot_model_7.R")) {
    stop("Helper plot tidak ditemukan: R/plot_model_7.R")
  }
  source("R/plot_model_7.R", local = TRUE)
  generate_tsfs_final_plots(model_M, tsfs, OUT, use_hc3_M)
  buat_gambar_model_7(
    model_M, model_Y, simple_slopes, boot_summary,
    file.path(OUT, "TSFS_Model_7_Estimated.png"),
    use_hc3_M = use_hc3_M
  )
}


# 9. Simpan objek analisis ----

saveRDS(
  list(
    data_md5 = unname(
      tools::md5sum(
        DATA_FILE
      )
    ),
    final_measurement_models = list(
      ESI = model_esi_final,
      PSMLS = model_psmls_final,
      PEB = model_peb_final,
      PWB = model_pwb_final
    ),
    final_fits = final_fits,
    tahap_fits = tahap_fits,
    peb_merge_lrt = if (RUN_MODEL_STAGES) peb_merge_lrt else NULL,
    peb_higher_order_lrt = if (RUN_MODEL_STAGES) peb_higher_order_lrt else NULL,
    peb_second_order_lrt = if (RUN_MODEL_STAGES) peb_second_order_lrt else NULL,
    peb_16_four_vs_three_lrt = if (RUN_MODEL_STAGES) peb_16_four_vs_three_lrt else NULL,
    tsfs_joint_fits = list(
      ESI_PSMLS_predictors = fit_esi_psmls_joint,
      PEB_ESI_predictors = fit_peb_esi_joint
    ),
    pemeriksaan_skor_bartlett = pemeriksaan_skor_bartlett,
    tsfs_data = tsfs,
    descriptive_scores = descriptive_scores,
    mediator_model = model_M,
    outcome_model = model_Y,
    bp_M = bp_M,
    bp_Y = bp_Y,
    simple_slopes = simple_slopes,
    bootstrap_summary = boot_summary
  ),
  file.path(
    OUT,
    "analysis_objects_final_TSFS.rds"
  )
)


# 10. Laporan lengkap ----

report_file <- file.path(
  OUT,
  "MASTER_ANALYSIS_REPORT_FINAL_TSFS.txt"
)

report <- capture.output({
  cat(
    strrep(
      "=",
      110
    ),
    "\n",
    sep = ""
  )

  cat(
    "MASTER ANALYSIS ARCHIVE - FINAL TSFS\n"
  )

  cat(
    strrep(
      "=",
      110
    ),
    "\n\n",
    sep = ""
  )

  cat(
    "Run time      : ",
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S %Z"
    ),
    "\n",
    sep = ""
  )

  cat(
    "Data file     : ",
    normalizePath(
      DATA_FILE
    ),
    "\n",
    sep = ""
  )

  cat(
    "Data MD5      : ",
    unname(
      tools::md5sum(
        DATA_FILE
      )
    ),
    "\n",
    sep = ""
  )

  cat(
    "N             : ",
    nrow(df),
    "\n",
    sep = ""
  )

  cat(
    "CFA estimator : MLR\n"
  )

  cat(
    "CFA scaling   : std.lv = TRUE\n"
  )

  cat(
    "Structural    : Two-Step Factor Scores (TSFS)\n\n"
  )


  cat(
    strrep(
      "=",
      110
    ),
    "\nFINAL CFA MODEL FIT\n",
    strrep(
      "=",
      110
    ),
    "\n\n",
    sep = ""
  )

  print(
    round_numeric(
      final_fit_table
    ),
    row.names = FALSE
  )


  cat(
    "\n\n",
    strrep(
      "=",
      110
    ),
    "\nFIRST-ORDER RELIABILITY / ITEM-LEVEL AVE SUMMARY\n",
    strrep(
      "=",
      110
    ),
    "\n\n",
    sep = ""
  )

  print(
    round_numeric(
      reliability_first
    ),
    row.names = FALSE
  )


  cat(
    "\n\n",
    strrep(
      "=",
      110
    ),
    "\nSECOND-ORDER RELIABILITY\n",
    strrep(
      "=",
      110
    ),
    "\n\n",
    sep = ""
  )

  print(
    round_numeric(
      reliability_second
    ),
    row.names = FALSE
  )

  cat(
    "\nNote: Mean_squared_second_order_loading is not item-level AVE.\n"
  )


  cat(
    "\n\n",
    strrep(
      "=",
      110
    ),
    "\nTSFS SCORING DESIGN\n",
    strrep(
      "=",
      110
    ),
    "\n\n",
    sep = ""
  )

  cat(
    "Mediator equation:\n",
    "  Outcome    = PEB Bartlett score\n",
    "  Predictors = ESI + PSMLS regression scores from joint CFA\n\n",
    sep = ""
  )

  cat(
    "Outcome equation:\n",
    "  Outcome    = PWB Bartlett score\n",
    "  Predictors = PEB + ESI regression scores from joint CFA\n\n",
    sep = ""
  )

  cat(
    "PEB Bartlett method used: ",
    score_peb_bartlett$method,
    "\n",
    sep = ""
  )

  cat(
    "PWB Bartlett method used: ",
    score_pwb_bartlett$method,
    "\n\n",
    sep = ""
  )

  print(
    pemeriksaan_skor_bartlett,
    row.names = FALSE
  )


  cat(
    "\n\n",
    strrep(
      "=",
      110
    ),
    "\nPRIMARY TSFS SCORE DESCRIPTIVES\n",
    strrep(
      "=",
      110
    ),
    "\n\n",
    sep = ""
  )

  print(
    round_numeric(
      descriptives
    ),
    row.names = FALSE
  )

  cat(
    "\n\nCORRELATION MATRIX\n\n"
  )

  print(
    round(
      cor_matrix,
      4
    )
  )


  cat(
    "\n\n",
    strrep(
      "=",
      110
    ),
    "\nMODEL 7 - DIAGNOSTIK ASUMSI REGRESI\n",
    strrep(
      "=",
      110
    ),
    "\n\n",
    sep = ""
  )

  print(
    diagnostik_asumsi,
    row.names = FALSE
  )


  cat(
    "\n\n",
    strrep(
      "=",
      110
    ),
    "\nMODEL 7 - MEDIATOR EQUATION\n",
    strrep(
      "=",
      110
    ),
    "\n\n",
    sep = ""
  )

  print(
    table_M,
    row.names = FALSE
  )


  cat(
    "\n\n",
    strrep(
      "=",
      110
    ),
    "\nMODEL 7 - OUTCOME EQUATION\n",
    strrep(
      "=",
      110
    ),
    "\n\n",
    sep = ""
  )

  print(
    table_Y,
    row.names = FALSE
  )


  cat(
    "\n\nMODEL R-SQUARED\n\n"
  )

  print(
    r2_table,
    row.names = FALSE
  )


  cat(
    "\n\n",
    strrep(
      "=",
      110
    ),
    "\nSIMPLE SLOPES\n",
    strrep(
      "=",
      110
    ),
    "\n\n",
    sep = ""
  )

  print(
    simple_slopes,
    row.names = FALSE
  )


  cat(
    "\n\nJOHNSON-NEYMAN\n\n"
  )

  cat(
    paste(
      jn_output,
      collapse = "\n"
    ),
    "\n"
  )


  if (RUN_BOOTSTRAP) {
    cat(
      "\n\n",
      strrep(
        "=",
        110
      ),
      "\nCONDITIONAL INDIRECT EFFECTS / INDEX OF MODERATED MEDIATION\n",
      strrep(
        "=",
        110
      ),
      "\n\n",
      sep = ""
    )

    print(
      boot_summary,
      row.names = FALSE
    )

    cat(
      "\nBootstrap resamples: ",
      BOOT_R,
      "\nSeed: ",
      BOOT_SEED,
      "\nCFA re-estimated inside bootstrap: NO\n",
      sep = ""
    )
  }


  if (RUN_MODEL_STAGES) {
    cat(
      "\n\n",
      strrep(
        "=",
        110
      ),
      "\nPENGUJIAN MODEL CFA BERTAHAP - MODEL FIT\n",
      strrep(
        "=",
        110
      ),
      "\n\n",
      sep = ""
    )

    print(
      round_numeric(
        tahap_fit_table
      ),
      row.names = FALSE
    )

    cat(
      "\nKey decision diagnostics:\n\n"
    )

    print(
      round_numeric(
        tahap_special
      ),
      row.names = FALSE
    )

    cat(
      "\nCatatan pengujian model bertahap:\n",
      "- ESI baseline has six items without correlated residuals; final adds ESI_2 ~~ ESI_3.\n",
      "- PEB baseline began with 19 items / 4 factors.\n",
      "- PEB final has 16 items / 4 first-order factors + PEB; the 15-item models are historical comparisons.\n",
      sprintf(
        "- Historical 15-item correlated four-factor EnvCit-LandStew correlation = %.3f.\n",
        lavaan::lavInspect(fit_peb_15_four_correlated, "cor.lv")["EnvCit", "LandStew"]
      ),
      sprintf(
        "- Final 16-item correlated four-factor EnvCit-LandStew correlation = %.3f.\n",
        lavaan::lavInspect(fit_peb_16_four_correlated, "cor.lv")["EnvCit", "LandStew"]
      ),
      sprintf(
        "- Four-versus-three-factor robust difference test on the same 15 items: delta chi-square(%d) = %.3f, p = %.4g.\n",
        peb_merge_lrt[["Df diff"]][2],
        peb_merge_lrt[["Chisq diff"]][2],
        peb_merge_lrt[["Pr(>Chisq)"]][2]
      ),
      sprintf(
        "- Historical 15-item four- versus three-factor second-order test: delta chi-square(%d) = %.3f, p = %.4g.\n",
        peb_higher_order_lrt[["Df diff"]][2],
        peb_higher_order_lrt[["Chisq diff"]][2],
        peb_higher_order_lrt[["Pr(>Chisq)"]][2]
      ),
      sprintf(
        "- Final 16-item four- versus three-factor second-order test: delta chi-square(%d) = %.3f, p = %.4g.\n",
        peb_16_four_vs_three_lrt[["Df diff"]][2],
        peb_16_four_vs_three_lrt[["Chisq diff"]][2],
        peb_16_four_vs_three_lrt[["Pr(>Chisq)"]][2]
      ),
      "- The significant difference establishes distinguishability, not whether a broader aggregate is substantively useful.\n",
      sprintf(
        "- On the same 16 items, correlated versus second-order robust difference test: delta chi-square(%d) = %.3f, p = %.4g.\n",
        peb_second_order_lrt[["Df diff"]][2],
        peb_second_order_lrt[["Chisq diff"]][2],
        peb_second_order_lrt[["Pr(>Chisq)"]][2]
      ),
      "- PEB final retains PEB_15 and PEB_18; only PEB_2, PEB_9, and PEB_10 are excluded.\n",
      "- Uji PEB_9/10, PEB_18, dan PEB_17 ~~ PEB_18 ada di cfa/peb/laporan_cfa.txt.\n",
      "- PWB reverse-wording factor is diagnostic only and is not retained in the final model.\n",
      "- PWB final has 21 favorable items / 6 first-order factors + second-order PWB.\n",
      "- AIC/BIC must not be interpreted across models with different observed-item sets.\n",
      "- Uji PWB enam faktor berkorelasi versus second-order ada di cfa/pwb/laporan_cfa.txt.\n",
      sep = ""
    )
  }


  cat(
    "\n\n",
    strrep(
      "=",
      110
    ),
    "\nREPRODUCIBILITY\n",
    strrep(
      "=",
      110
    ),
    "\n\n",
    sep = ""
  )

  cat(
    "R version       : ",
    R.version.string,
    "\n",
    sep = ""
  )

  for (
    pkg in required_packages
  ) {
    cat(
      sprintf(
        "%-15s: %s\n",
        pkg,
        as.character(
          utils::packageVersion(
            pkg
          )
        )
      )
    )
  }

  cat("\n")

  print(
    utils::sessionInfo()
  )
})

writeLines(
  report,
  report_file
)
