# CFA ESI: jalankan source("R/cfa_esi.R") dari folder proyek.
# Model awal -> residual covariance ESI_2–ESI_3 -> model akhir.
CFA_MANDIRI_ESI <- !isTRUE(get0("CFA_DARI_ANALISIS_FINAL", ifnotfound = FALSE, inherits = FALSE))
if (CFA_MANDIRI_ESI) {
  source("R/fungsi_analisis.R", local = TRUE)
  source("R/helpers.R", local = TRUE)
  source("R/cfa_helpers.R", local = TRUE)
  source("R/plot_cfa.R", local = TRUE)
  df <- readRDS("data/data_bersih.rds")
  OUT <- "hasil_analisis"
  dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
  RUN_MODEL_STAGES <- TRUE
}

# Definisi model ----
model_esi_baseline <- "
ESI =~
  ESI_1 +
  ESI_2 +
  ESI_3 +
  ESI_4 +
  ESI_5 +
  ESI_6
"

model_esi_final <- paste0(model_esi_baseline, "
ESI_2 ~~ ESI_3
")

cek_item_cfa(df, model_esi_final)

# Pengujian model bertahap ----
if (RUN_MODEL_STAGES) {
  cat("Menguji model ESI bertahap...\n")
  fit_esi_baseline <- lavaan::cfa(
    model_esi_baseline,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_esi_final_tahap <- lavaan::cfa(
    model_esi_final,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )
}

# Model final ----
fit_esi <- if (RUN_MODEL_STAGES) {
  fit_esi_final_tahap
} else {
  lavaan::cfa(
    model_esi_final,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )
}

folder <- file.path(OUT, "cfa", "esi")
tahap_esi <- if (RUN_MODEL_STAGES) list(Awal_6_item = fit_esi_baseline, Akhir_residual_2_3 = fit_esi) else list(Akhir = fit_esi)
banding_esi <- if (RUN_MODEL_STAGES) list(Awal_vs_akhir = lavaan::lavTestLRT(fit_esi_baseline, fit_esi)) else list()
diagnostik_esi <- if (RUN_MODEL_STAGES) list(
  Loading_awal = loading_table(fit_esi_baseline, "ESI_awal"),
  MI_awal = mi_table(fit_esi_baseline, cutoff = 10, top_n = 10)
) else list()
simpan_laporan_cfa("ESI", tahap_esi, fit_esi, folder, faktor = "ESI",
                   perbandingan = banding_esi, diagnostik = diagnostik_esi)
if (isTRUE(get0("RUN_PLOTS", ifnotfound = TRUE))) {
  if (RUN_MODEL_STAGES) {
    buat_plot_cfa(list(ESI = fit_esi_baseline), OUT, "plot_model_awal.png",
                  list(ESI = "CFA ESI Awal (6 Item, 1 Faktor)"))
  }
  buat_plot_cfa(list(ESI = fit_esi), OUT)
}
if (CFA_MANDIRI_ESI) cat("Hasil CFA ESI:", normalizePath(folder), "\n")
