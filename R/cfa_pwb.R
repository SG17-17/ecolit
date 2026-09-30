# CFA PWB: jalankan source("R/cfa_pwb.R") dari folder proyek.
# Model 42 item -> method factor -> model akhir 21 favorable items.
CFA_MANDIRI_PWB <- !isTRUE(get0("CFA_DARI_ANALISIS_FINAL", ifnotfound = FALSE, inherits = FALSE))
if (CFA_MANDIRI_PWB) {
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
model_pwb_baseline42 <- "
Autonomy =~
  PWB_1 + PWB_2 + PWB_3_R + PWB_4_R +
  PWB_5 + PWB_6_R + PWB_7

Mastery =~
  PWB_8 + PWB_9_R + PWB_10_R + PWB_11 +
  PWB_12_R + PWB_13_R + PWB_14

Growth =~
  PWB_15_R + PWB_16 + PWB_17_R + PWB_18 +
  PWB_19_R + PWB_20 + PWB_21_R

Relations =~
  PWB_22 + PWB_23_R + PWB_24_R + PWB_25 +
  PWB_26 + PWB_27_R + PWB_28

Purpose =~
  PWB_29_R + PWB_30 + PWB_31_R + PWB_32_R +
  PWB_33 + PWB_34 + PWB_35_R

Acceptance =~
  PWB_36 + PWB_37 + PWB_38_R + PWB_39 +
  PWB_40_R + PWB_41_R + PWB_42
"

# Model alternatif 42 item dengan reverse-wording method factor.

model_pwb_method42 <- paste0(
  model_pwb_baseline42,
  "
ReverseMethod =~
  PWB_3_R + PWB_4_R + PWB_6_R +
  PWB_9_R + PWB_10_R + PWB_12_R + PWB_13_R +
  PWB_15_R + PWB_17_R + PWB_19_R + PWB_21_R +
  PWB_23_R + PWB_24_R + PWB_27_R +
  PWB_29_R + PWB_31_R + PWB_32_R + PWB_35_R +
  PWB_38_R + PWB_40_R + PWB_41_R

ReverseMethod ~~ 0*Autonomy
ReverseMethod ~~ 0*Mastery
ReverseMethod ~~ 0*Growth
ReverseMethod ~~ 0*Relations
ReverseMethod ~~ 0*Purpose
ReverseMethod ~~ 0*Acceptance
"
)

# Kandidat lama: 22 item.

model_pwb_old22 <- "
Autonomy =~
  PWB_1 + PWB_2 + PWB_5 + PWB_7

Mastery =~
  PWB_8 + PWB_11 + PWB_14

Growth =~
  PWB_15_R + PWB_16 + PWB_20 + PWB_21_R

Relations =~
  PWB_22 + PWB_25 + PWB_26 + PWB_28

Purpose =~
  PWB_30 + PWB_33 + PWB_34

Acceptance =~
  PWB_36 + PWB_37 + PWB_39 + PWB_42

PWB =~
  Autonomy +
  Mastery +
  Growth +
  Relations +
  Purpose +
  Acceptance
"

# Model akhir: 21 favorable items, second-order.

model_pwb_final <- "
Autonomy =~
  PWB_1 + PWB_2 + PWB_5 + PWB_7

Mastery =~
  PWB_8 + PWB_11 + PWB_14

Growth =~
  PWB_16 + PWB_18 + PWB_20

Relations =~
  PWB_22 + PWB_25 + PWB_26 + PWB_28

Purpose =~
  PWB_30 + PWB_33 + PWB_34

Acceptance =~
  PWB_36 + PWB_37 + PWB_39 + PWB_42

PWB =~
  Autonomy +
  Mastery +
  Growth +
  Relations +
  Purpose +
  Acceptance
"

# Pembanding: 21 item yang sama, enam faktor berkorelasi.

model_pwb_final_correlated <- "
Autonomy =~
  PWB_1 + PWB_2 + PWB_5 + PWB_7

Mastery =~
  PWB_8 + PWB_11 + PWB_14

Growth =~
  PWB_16 + PWB_18 + PWB_20

Relations =~
  PWB_22 + PWB_25 + PWB_26 + PWB_28

Purpose =~
  PWB_30 + PWB_33 + PWB_34

Acceptance =~
  PWB_36 + PWB_37 + PWB_39 + PWB_42
"

# Uji penambahan PWB_21_R pada Growth.

model_pwb_m7 <- "
Autonomy =~
  PWB_1 + PWB_2 + PWB_5 + PWB_7

Mastery =~
  PWB_8 + PWB_11 + PWB_14

Growth =~
  PWB_16 + PWB_18 + PWB_20 + PWB_21_R

Relations =~
  PWB_22 + PWB_25 + PWB_26 + PWB_28

Purpose =~
  PWB_30 + PWB_33 + PWB_34

Acceptance =~
  PWB_36 + PWB_37 + PWB_39 + PWB_42

PWB =~
  Autonomy +
  Mastery +
  Growth +
  Relations +
  Purpose +
  Acceptance
"

# Uji penghapusan PWB_22 atau PWB_26 dari Relations.

model_pwb_drop22 <- "
Autonomy =~ PWB_1 + PWB_2 + PWB_5 + PWB_7
Mastery =~ PWB_8 + PWB_11 + PWB_14
Growth =~ PWB_16 + PWB_18 + PWB_20
Relations =~ PWB_25 + PWB_26 + PWB_28
Purpose =~ PWB_30 + PWB_33 + PWB_34
Acceptance =~ PWB_36 + PWB_37 + PWB_39 + PWB_42
PWB =~ Autonomy + Mastery + Growth + Relations + Purpose + Acceptance
"

model_pwb_drop26 <- "
Autonomy =~ PWB_1 + PWB_2 + PWB_5 + PWB_7
Mastery =~ PWB_8 + PWB_11 + PWB_14
Growth =~ PWB_16 + PWB_18 + PWB_20
Relations =~ PWB_22 + PWB_25 + PWB_28
Purpose =~ PWB_30 + PWB_33 + PWB_34
Acceptance =~ PWB_36 + PWB_37 + PWB_39 + PWB_42
PWB =~ Autonomy + Mastery + Growth + Relations + Purpose + Acceptance
"

cek_item_cfa(df, model_pwb_baseline42)

# Pengujian model bertahap ----
if (RUN_MODEL_STAGES) {
  cat("Menguji model PWB bertahap...\n")
  fit_pwb_baseline42 <- lavaan::cfa(
    model_pwb_baseline42,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_pwb_method42 <- lavaan::cfa(
    model_pwb_method42,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_pwb_old22 <- lavaan::cfa(
    model_pwb_old22,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_pwb_final21 <- lavaan::cfa(
    model_pwb_final,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_pwb_correlated21 <- lavaan::cfa(
    model_pwb_final_correlated,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_pwb_m7 <- lavaan::cfa(
    model_pwb_m7,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_pwb_drop22 <- lavaan::cfa(
    model_pwb_drop22,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  fit_pwb_drop26 <- lavaan::cfa(
    model_pwb_drop26,
    data = df,
    estimator = "MLR",
    std.lv = TRUE
  )

  tahap_pwb_baseline_loadings <- loading_table(
    fit_pwb_baseline42,
    "PWB_Baseline42"
  )

  tahap_pwb_baseline_mi <- mi_table(
    fit_pwb_baseline42,
    cutoff = 10,
    top_n = 100
  )

  # Factor loading untuk reverse-wording method factor.
  method_loadings <- loading_table(
    fit_pwb_method42,
    "PWB_ReverseMethod42"
  )

  method_loadings <- method_loadings[
    method_loadings$Factor == "ReverseMethod",
  ]

  tahap_pwb_final_mi <- mi_table(
    fit_pwb_final21,
    cutoff = 10,
    top_n = 30
  )

  # Perbandingan enam faktor berkorelasi dan second-order pada 21 item.
  pwb_second_order_lrt <- tryCatch(
    lavaan::lavTestLRT(
      fit_pwb_correlated21,
      fit_pwb_final21
    ),
    error = function(e) {
      data.frame(
        Error = conditionMessage(e)
      )
    }
  )

}

# Model final ----
fit_pwb <- if (RUN_MODEL_STAGES) fit_pwb_final21 else {
  lavaan::cfa(model_pwb_final, data = df, estimator = "MLR", std.lv = TRUE)
}

folder <- file.path(OUT, "cfa", "pwb")
tahap <- if (RUN_MODEL_STAGES) list(
    Awal_42_item = fit_pwb_baseline42,
    Method_factor_42_item = fit_pwb_method42,
    Kandidat_lama_22_item = fit_pwb_old22,
    Enam_faktor_21_item = fit_pwb_correlated21,
    Second_order_21_item = fit_pwb,
    Uji_tambah_PWB21R = fit_pwb_m7,
    Uji_hapus_PWB22 = fit_pwb_drop22,
    Uji_hapus_PWB26 = fit_pwb_drop26
) else list(Akhir = fit_pwb)
banding_pwb <- if (RUN_MODEL_STAGES) list(
  Berkorelasi_vs_second_order_21_item = pwb_second_order_lrt
) else list()
diagnostik_pwb <- if (RUN_MODEL_STAGES) list(
  Loading_model_awal = tahap_pwb_baseline_loadings,
  Loading_method_factor = method_loadings,
  MI_model_awal = tahap_pwb_baseline_mi
) else list()
simpan_laporan_cfa(
    "PWB", tahap, fit_pwb, folder,
    faktor = c("Autonomy", "Mastery", "Growth", "Relations", "Purpose", "Acceptance"),
    faktor_utama = "PWB",
    perbandingan = banding_pwb, diagnostik = diagnostik_pwb
)
if (isTRUE(get0("RUN_PLOTS", ifnotfound = TRUE))) {
  if (RUN_MODEL_STAGES) {
    buat_plot_cfa(list(PWB = fit_pwb_baseline42), OUT, "plot_model_awal.png",
                  list(PWB = "CFA PWB Awal (42 Item, 6 Faktor Berkorelasi)"))
    buat_plot_cfa(list(PWB = fit_pwb_method42), OUT, "plot_method_factor.png",
                  list(PWB = "CFA PWB Alternatif (42 Item + Method Factor)"))
  }
  buat_plot_cfa(list(PWB = fit_pwb), OUT)
}
if (CFA_MANDIRI_PWB) cat("Hasil CFA PWB:", normalizePath(folder), "\n")
