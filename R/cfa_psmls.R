# CFA PSMLS: jalankan source("R/cfa_psmls.R") dari folder proyek.
# Empat faktor berkorelasi -> satu faktor second-order; semua item dipertahankan.
CFA_MANDIRI_PSMLS <- !isTRUE(get0("CFA_DARI_ANALISIS_FINAL", ifnotfound = FALSE, inherits = FALSE))
if (CFA_MANDIRI_PSMLS) {
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
model_psmls_awal <- "
TechComp =~
  PSMLS_1 +
  PSMLS_2 +
  PSMLS_3 +
  PSMLS_4 +
  PSMLS_5

SocialRel =~
  PSMLS_6 +
  PSMLS_7 +
  PSMLS_8

Privacy =~
  PSMLS_9 +
  PSMLS_10 +
  PSMLS_11

InfoAware =~
  PSMLS_12 +
  PSMLS_13 +
  PSMLS_14
"

model_psmls_final <- paste0(model_psmls_awal, "
PSMLS =~
  TechComp +
  SocialRel +
  Privacy +
  InfoAware
")

cek_item_cfa(df, model_psmls_final)

# Pengujian struktur awal ----
if (RUN_MODEL_STAGES) {
  fit_psmls_awal <- lavaan::cfa(model_psmls_awal, data = df, estimator = "MLR", std.lv = TRUE)
}

# Model final ----
fit_psmls <- lavaan::cfa(
  model_psmls_final,
  data = df,
  estimator = "MLR",
  std.lv = TRUE
)

folder <- file.path(OUT, "cfa", "psmls")
tahap_psmls <- if (RUN_MODEL_STAGES) list(
  Awal_empat_faktor_berkorelasi = fit_psmls_awal, Akhir_second_order = fit_psmls
) else list(Akhir = fit_psmls)
banding_psmls <- if (RUN_MODEL_STAGES) list(Awal_vs_akhir = lavaan::lavTestLRT(fit_psmls_awal, fit_psmls)) else list()
diagnostik_psmls <- if (RUN_MODEL_STAGES) list(Loading_awal = loading_table(fit_psmls_awal, "PSMLS_awal")) else list()
simpan_laporan_cfa(
  "PSMLS", tahap_psmls, fit_psmls, folder,
  faktor = c("TechComp", "SocialRel", "Privacy", "InfoAware"),
  faktor_utama = "PSMLS", perbandingan = banding_psmls, diagnostik = diagnostik_psmls
)
if (isTRUE(get0("RUN_PLOTS", ifnotfound = TRUE))) {
  if (RUN_MODEL_STAGES) {
    buat_plot_cfa(list(PSMLS = fit_psmls_awal), OUT, "plot_model_awal.png",
                  list(PSMLS = "CFA PSMLS Awal (14 Item, 4 Faktor Berkorelasi)"))
  }
  buat_plot_cfa(list(PSMLS = fit_psmls), OUT)
}
if (CFA_MANDIRI_PSMLS) cat("Hasil CFA PSMLS:", normalizePath(folder), "\n")
