# Pembentukan factor scores TSFS dan pemeriksaan skor.

# 1. Model pengukuran TSFS ----
#
# Pieters, Pieters, & Lemmens (2022) recommend:
#   - Bartlett factor scores for the endogenous/outcome construct.
#   - Regression factor scores for predictors estimated jointly so their
#     covariance is represented in the predictor measurement model.
#
# Final measurement structures from Sections 4-9 are retained.
# ==============================================================================

cat("Menyiapkan TSFS measurement models...\n")

# PEB and PWB outcome models are the already-estimated final CFAs.
fit_peb_outcome <- fit_peb
fit_pwb_outcome <- fit_pwb

# Joint predictor model for mediator equation: ESI + PSMLS.
model_esi_psmls_joint <- paste0(
  model_esi_final,
  "\n",
  model_psmls_final,
  "\nESI ~~ PSMLS\n"
)

fit_esi_psmls_joint <- lavaan::cfa(
  model_esi_psmls_joint,
  data = df,
  estimator = "MLR",
  std.lv = TRUE
)

# Joint predictor model for outcome equation: PEB + ESI.
model_peb_esi_joint <- paste0(
  model_peb_final,
  "\n",
  model_esi_final,
  "\nPEB ~~ ESI\n"
)

fit_peb_esi_joint <- lavaan::cfa(
  model_peb_esi_joint,
  data = df,
  estimator = "MLR",
  std.lv = TRUE
)

fits_tsfs <- list(
  PEB_outcome = fit_peb_outcome,
  ESI_PSMLS_predictors = fit_esi_psmls_joint,
  PWB_outcome = fit_pwb_outcome,
  PEB_ESI_predictors = fit_peb_esi_joint
)

for (nm in names(fits_tsfs)) {
  if (!isTRUE(lavaan::lavInspect(fits_tsfs[[nm]], "converged"))) {
    stop("TSFS model tidak konvergen: ", nm)
  }

  if (!isTRUE(lavaan::lavInspect(fits_tsfs[[nm]], "post.check"))) {
    stop("TSFS model menghasilkan solusi inadmissible: ", nm)
  }
}

tsfs_measurement_fit <- do.call(
  rbind,
  lapply(
    names(fits_tsfs),
    function(nm) fit_row(fits_tsfs[[nm]], nm)
  )
)

utils::write.csv(
  round_numeric(tsfs_measurement_fit),
  file.path(OUT, "TSFS_measurement_model_fit.csv"),
  row.names = FALSE
)


# 2. Skor faktor final ----

cat("\nMengestimasi TSFS factor scores...\n")

cat("  1. PEB Bartlett outcome score\n")
score_peb_bartlett <- get_bartlett_outcome_score(
  fit_peb_outcome,
  df,
  "PEB"
)
PEB_B <- score_peb_bartlett$scores

cat("  2. ESI + PSMLS regression predictor scores\n")
score_esi_psmls_reg <- as.data.frame(
  lavaan::lavPredict(
    fit_esi_psmls_joint,
    type = "lv",
    method = "regression",
    transform = FALSE
  )
)

if (
  !all(
    c(
      "ESI",
      "PSMLS"
    ) %in%
      names(
        score_esi_psmls_reg
      )
  )
) {
  stop(
    "ESI/PSMLS regression scores tidak ditemukan."
  )
}

ESI_R_M <- score_esi_psmls_reg$ESI
PSMLS_R_M <- score_esi_psmls_reg$PSMLS

cat("  3. PWB Bartlett outcome score\n")
score_pwb_bartlett <- get_bartlett_outcome_score(
  fit_pwb_outcome,
  df,
  "PWB"
)
PWB_B <- score_pwb_bartlett$scores

cat("  4. PEB + ESI regression predictor scores\n")
score_peb_esi_reg <- as.data.frame(
  lavaan::lavPredict(
    fit_peb_esi_joint,
    type = "lv",
    method = "regression",
    transform = FALSE
  )
)

if (
  !all(
    c(
      "PEB",
      "ESI"
    ) %in%
      names(
        score_peb_esi_reg
      )
  )
) {
  stop(
    "PEB/ESI regression scores tidak ditemukan."
  )
}

PEB_R_Y <- score_peb_esi_reg$PEB
ESI_R_Y <- score_peb_esi_reg$ESI

pemeriksaan_skor_bartlett <- rbind(
  transform(
    score_peb_bartlett$pemeriksaan,
    Scoring_Method = score_peb_bartlett$method
  ),
  transform(
    score_pwb_bartlett$pemeriksaan,
    Scoring_Method = score_pwb_bartlett$method
  )
)

utils::write.csv(
  pemeriksaan_skor_bartlett,
  file.path(
    OUT,
    "TSFS_Bartlett_outcome_score_check.csv"
  ),
  row.names = FALSE
)


# 3. Data analisis struktural ----

tsfs <- data.frame(
  PEB_B = as.numeric(PEB_B),
  ESI_R_M = as.numeric(ESI_R_M),
  PSMLS_R_M = as.numeric(PSMLS_R_M),
  PWB_B = as.numeric(PWB_B),
  PEB_R_Y = as.numeric(PEB_R_Y),
  ESI_R_Y = as.numeric(ESI_R_Y)
)

# Mean-center only the moderation predictors before forming the product.
tsfs$ESI_c <- as.numeric(
  scale(
    tsfs$ESI_R_M,
    center = TRUE,
    scale = FALSE
  )
)

tsfs$PSMLS_c <- as.numeric(
  scale(
    tsfs$PSMLS_R_M,
    center = TRUE,
    scale = FALSE
  )
)

tsfs$ESI_x_PSMLS <-
  tsfs$ESI_c *
    tsfs$PSMLS_c

if (
  anyNA(tsfs) ||
    any(
      !is.finite(
        as.matrix(tsfs)
      )
    )
) {
  stop(
    "Ada missing/non-finite value pada TSFS structural data."
  )
}

tsfs_sd <- vapply(
  tsfs,
  stats::sd,
  numeric(1)
)

invalid_sd <-
  !is.finite(tsfs_sd) |
    tsfs_sd <= 1e-10

if (any(invalid_sd)) {
  stop(
    "SD nol/hampir nol pada TSFS variable: ",
    paste(
      names(tsfs_sd)[invalid_sd],
      collapse = ", "
    )
  )
}

PSMLS_SD <- stats::sd(
  tsfs$PSMLS_c
)

utils::write.csv(
  tsfs,
  file.path(
    OUT,
    "TSFS_factor_scores_and_structural_data.csv"
  ),
  row.names = FALSE
)

saveRDS(
  tsfs,
  file.path(
    OUT,
    "TSFS_factor_scores_and_structural_data.rds"
  )
)

# 4. Diagnostik skor TSFS ----

score_cor_all <- stats::cor(
  tsfs
)

score_sd_all <- data.frame(
  Variable = names(tsfs),
  Mean = vapply(
    tsfs,
    mean,
    numeric(1)
  ),
  SD = vapply(
    tsfs,
    stats::sd,
    numeric(1)
  ),
  Min = vapply(
    tsfs,
    min,
    numeric(1)
  ),
  Max = vapply(
    tsfs,
    max,
    numeric(1)
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  score_cor_all,
  file.path(
    OUT,
    "TSFS_all_score_correlations.csv"
  )
)

utils::write.csv(
  score_sd_all,
  file.path(
    OUT,
    "TSFS_all_score_descriptives.csv"
  ),
  row.names = FALSE
)

scoring_note <- c(
  "TWO-STEP FACTOR SCORES (TSFS)",
  "",
  "Mediator equation:",
  "  Outcome    = PEB Bartlett factor score",
  "  Predictors = ESI + PSMLS regression factor scores from a joint CFA",
  "",
  "Outcome equation:",
  "  Outcome    = PWB Bartlett factor score",
  "  Predictors = PEB + ESI regression factor scores from a joint CFA",
  "",
  paste0(
    "PEB Bartlett scoring actually used: ",
    score_peb_bartlett$method
  ),
  paste0(
    "PWB Bartlett scoring actually used: ",
    score_pwb_bartlett$method
  ),
  "",
  "Regression factor scores use:",
  "  lavaan::lavPredict(method = 'regression', transform = FALSE)",
  "",
  "For Bartlett outcomes, the script first attempts:",
  "  lavaan::lavPredict(method = 'Bartlett', transform = FALSE)",
  "and only uses the collapsed-loading fallback if the higher-order score",
  "is absent, non-finite, or essentially constant.",
  "",
  "CFA models are not re-estimated inside the indirect-effect bootstrap."
)

writeLines(
  scoring_note,
  file.path(
    OUT,
    "TSFS_SCORING_METHOD.txt"
  )
)
