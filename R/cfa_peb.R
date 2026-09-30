# CFA PEB: jalankan source("R/cfa_peb.R") dari folder proyek.
# Model 19 item -> evaluasi item dan dimensi -> model akhir 16 item.
CFA_MANDIRI_PEB <- !isTRUE(get0("CFA_DARI_ANALISIS_FINAL", ifnotfound = FALSE, inherits = FALSE))
if (CFA_MANDIRI_PEB) {
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
model_peb_baseline19 <- "
ConsLife =~
  PEB_1 + PEB_2 + PEB_3 + PEB_4 + PEB_5

SocEnv =~
  PEB_6 + PEB_7 + PEB_8 + PEB_9 + PEB_10

EnvCit =~
  PEB_11 + PEB_12 + PEB_13 + PEB_14 + PEB_15

LandStew =~
  PEB_16 + PEB_17 + PEB_18 + PEB_19
"

# Kandidat lama: 14 item.

model_peb_14 <- "
ConsLife =~
  PEB_1 + PEB_3 + PEB_4 + PEB_5

SocEnv =~
  PEB_6 + PEB_7 + PEB_8

CivicStew =~
  PEB_11 + PEB_12 + PEB_13 + PEB_14 +
  PEB_16 + PEB_17 + PEB_19

PEB =~
  ConsLife +
  SocEnv +
  CivicStew
"

# Pembanding lama: 15 item, tiga dimensi, second-order.
# PEB_15 dipertahankan; PEB_2, PEB_9, PEB_10, dan PEB_18 belum digunakan.

model_peb_three_factor <- "
ConsLife =~
  PEB_1 + PEB_3 + PEB_4 + PEB_5

SocEnv =~
  PEB_6 + PEB_7 + PEB_8

CivicStew =~
  PEB_11 + PEB_12 + PEB_13 + PEB_14 +
  PEB_15 + PEB_16 + PEB_17 + PEB_19

PEB =~
  ConsLife +
  SocEnv +
  CivicStew
"

# Pembanding lama: 15 item, empat dimensi.

model_peb_15_four_correlated <- "
ConsLife =~ PEB_1 + PEB_3 + PEB_4 + PEB_5
SocEnv =~ PEB_6 + PEB_7 + PEB_8
EnvCit =~ PEB_11 + PEB_12 + PEB_13 + PEB_14 + PEB_15
LandStew =~ PEB_16 + PEB_17 + PEB_19
"

model_peb_15_four_second_order <- paste0(
  model_peb_15_four_correlated,
  "\nPEB =~ ConsLife + SocEnv + EnvCit + LandStew\n"
)

# Model akhir: 16 item, empat dimensi; PEB_18 dipertahankan.
model_peb_16_four_correlated <- "
ConsLife =~ PEB_1 + PEB_3 + PEB_4 + PEB_5
SocEnv =~ PEB_6 + PEB_7 + PEB_8
EnvCit =~ PEB_11 + PEB_12 + PEB_13 + PEB_14 + PEB_15
LandStew =~ PEB_16 + PEB_17 + PEB_18 + PEB_19
"

model_peb_16_four_second_order <- paste0(
  model_peb_16_four_correlated,
  "\nPEB =~ ConsLife + SocEnv + EnvCit + LandStew\n"
)

# Pembanding pada 16 item yang sama: EnvCit dan LandStew digabung.
model_peb_16_three_second_order <- "
ConsLife =~ PEB_1 + PEB_3 + PEB_4 + PEB_5
SocEnv =~ PEB_6 + PEB_7 + PEB_8
CivicStew =~ PEB_11 + PEB_12 + PEB_13 + PEB_14 + PEB_15 +
  PEB_16 + PEB_17 + PEB_18 + PEB_19
PEB =~ ConsLife + SocEnv + CivicStew
"

model_peb_final <- model_peb_16_four_second_order

cek_item_cfa(df, model_peb_baseline19)

# Pengujian model bertahap ----
if (RUN_MODEL_STAGES) {
  cat("Menguji model PEB bertahap...\n")
  fit_peb_baseline19 <- lavaan::cfa(
    model_peb_baseline19,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_peb_14 <- lavaan::cfa(
    model_peb_14,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_peb_15 <- lavaan::cfa(
    model_peb_three_factor,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_peb_15_four_correlated <- lavaan::cfa(
    model_peb_15_four_correlated,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_peb_15_four_second_order <- lavaan::cfa(
    model_peb_15_four_second_order,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_peb_16_four_correlated <- lavaan::cfa(
    model_peb_16_four_correlated,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_peb_16_four_second_order <- lavaan::cfa(
    model_peb_16_four_second_order,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_peb_16_three_second_order <- lavaan::cfa(
    model_peb_16_three_second_order,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  # Uji pembanding lama pada 15 item yang sama.
  peb_merge_lrt <- lavaan::lavTestLRT(
    fit_peb_15_four_correlated,
    fit_peb_15
  )
  peb_higher_order_lrt <- lavaan::lavTestLRT(
    fit_peb_15_four_second_order,
    fit_peb_15
  )

  # Uji empat faktor berkorelasi versus second-order pada 16 item.
  peb_second_order_lrt <- lavaan::lavTestLRT(
    fit_peb_16_four_correlated,
    fit_peb_16_four_second_order
  )

  peb_16_four_vs_three_lrt <- lavaan::lavTestLRT(
    fit_peb_16_four_second_order,
    fit_peb_16_three_second_order
  )

  tahap_peb_baseline_loadings <- loading_table(
    fit_peb_baseline19,
    "PEB_Baseline19"
  )

  tahap_peb_baseline_mi <- mi_table(
    fit_peb_baseline19,
    cutoff = 10,
    top_n = 100
  )

  # Bukti keputusan untuk item yang ditinjau.
  tahap_peb_item_decisions <- rbind(
    cbind(
      Model = "PEB_Baseline19",
      Decision = "REMOVE",
      item_diagnostic(
        fit_peb_baseline19,
        "PEB_2",
        "ConsLife"
      )
    ),
    cbind(
      Model = "PEB_Baseline19",
      Decision = "REMOVE",
      item_diagnostic(
        fit_peb_baseline19,
        "PEB_9",
        "SocEnv"
      )
    ),
    cbind(
      Model = "PEB_Baseline19",
      Decision = "REMOVE",
      item_diagnostic(
        fit_peb_baseline19,
        "PEB_10",
        "SocEnv"
      )
    ),
    cbind(
      Model = "PEB_Baseline19",
      Decision = "RETAIN_AFTER_REVIEW",
      item_diagnostic(
        fit_peb_baseline19,
        "PEB_15",
        "EnvCit"
      )
    ),
    cbind(
      Model = "PEB_Baseline19",
      Decision = "RETAIN_AFTER_REVIEW",
      item_diagnostic(
        fit_peb_baseline19,
        "PEB_18",
        "LandStew"
      )
    ),
    cbind(
      Model = "PEB_Final16_FourSecondOrder",
      Decision = "RETAIN_FINAL",
      item_diagnostic(
        fit_peb_16_four_second_order,
        "PEB_15",
        "EnvCit"
      )
    ),
    cbind(
      Model = "PEB_Final16_FourSecondOrder",
      Decision = "RETAIN_FINAL",
      item_diagnostic(
        fit_peb_16_four_second_order,
        "PEB_18",
        "LandStew"
      )
    )
  )

  tahap_peb_final_mi <- mi_table(
    fit_peb_16_four_second_order,
    cutoff = 10,
    top_n = 30
  )

  if (!file.exists("R/uji_item_peb.R")) stop("Helper uji item PEB tidak ditemukan")
  source("R/uji_item_peb.R", local = TRUE)
  tahap_peb_sensitivitas <- uji_item_peb(
    data = df, final_fit = fit_peb_16_four_second_order,
    out_file = NULL
  )
}

# Model final ----
fit_peb <- if (RUN_MODEL_STAGES) fit_peb_16_four_second_order else {
  lavaan::cfa(model_peb_final, data = df, estimator = "MLR", std.lv = TRUE)
}

folder <- file.path(OUT, "cfa", "peb")
tahap <- if (RUN_MODEL_STAGES) list(
    Awal_19_item = fit_peb_baseline19, Kandidat_14_item = fit_peb_14,
    Empat_faktor_15_item = fit_peb_15_four_correlated,
    Second_order_15_item = fit_peb_15_four_second_order,
    Tiga_faktor_15_item = fit_peb_15,
    Empat_faktor_16_item = fit_peb_16_four_correlated,
    Second_order_16_item = fit_peb,
    Tiga_faktor_16_item = fit_peb_16_three_second_order
) else list(Akhir = fit_peb)
banding_peb <- if (RUN_MODEL_STAGES) list(
  Empat_vs_tiga_dimensi_16_item = peb_16_four_vs_three_lrt,
  Berkorelasi_vs_second_order_16_item = peb_second_order_lrt
) else list()
diagnostik_peb <- if (RUN_MODEL_STAGES) list(
  Keputusan_item = tahap_peb_item_decisions,
  Loading_model_awal = tahap_peb_baseline_loadings,
  MI_model_awal = tahap_peb_baseline_mi,
  Uji_sensitivitas_item = tahap_peb_sensitivitas
) else list()
simpan_laporan_cfa(
    "PEB", tahap, fit_peb, folder,
    faktor = c("ConsLife", "SocEnv", "EnvCit", "LandStew"), faktor_utama = "PEB",
    perbandingan = banding_peb, diagnostik = diagnostik_peb
)
if (isTRUE(get0("RUN_PLOTS", ifnotfound = TRUE))) {
  if (RUN_MODEL_STAGES) {
    buat_plot_cfa(list(PEB = fit_peb_baseline19), OUT, "plot_model_awal.png",
                  list(PEB = "CFA PEB Awal (19 Item, 4 Faktor Berkorelasi)"))
  }
  buat_plot_cfa(list(PEB = fit_peb), OUT)
}
if (CFA_MANDIRI_PEB) cat("Hasil CFA PEB:", normalizePath(folder), "\n")
