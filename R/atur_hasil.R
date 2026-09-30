# Tata letak hasil final. Jalankan dari folder proyek.
# Sumber file ditulis oleh analisis_final.R, lalu dipindahkan menurut isi.

.nama_hasil <- c(
  MASTER_ANALYSIS_REPORT_FINAL_TSFS.txt = "laporan_lengkap.txt",
  final_CFA_model_fit.csv = "cfa/ringkasan_model.csv",
  final_CFA_loadings.csv = "cfa/loading_item_final.csv",
  final_reliability_first_order.csv = "cfa/reliabilitas_dimensi.csv",
  final_reliability_second_order.csv = "cfa/reliabilitas_konstruk.csv",
  TSFS_measurement_model_fit.csv = "model_7/kecocokan_pengukuran.csv",
  tahap_model_fit.csv = "cfa/riwayat_model.csv",
  tahap_key_item_diagnostics.csv = "cfa/diagnostik_item_kunci.csv",
  tahap_PEB_item_decision_evidence.csv = "cfa/peb/keputusan_item.csv",
  tahap_PEB_baseline19_loadings.csv = "cfa/peb/loading_awal.csv",
  tahap_PEB_baseline19_MI_ge10.csv = "cfa/peb/mi_awal.csv",
  tahap_PEB_final_MI_ge10.csv = "cfa/peb/mi_final.csv",
  tahap_PWB_baseline42_loadings.csv = "cfa/pwb/loading_awal.csv",
  tahap_PWB_baseline42_MI_ge10.csv = "cfa/pwb/mi_awal.csv",
  tahap_PWB_final_MI_ge10.csv = "cfa/pwb/mi_final.csv",
  tahap_PWB_reverse_method_loadings.csv = "cfa/pwb/loading_metode_terbalik.csv",
  tahap_PWB_correlated_vs_secondorder_LRT.txt = "cfa/pwb/uji_struktur.txt",
  tahap_PEB_correlated_vs_secondorder_LRT.txt = "cfa/peb/uji_struktur_second_order.txt",
  tahap_PEB_16_four_vs_three_LRT.txt = "cfa/peb/uji_empat_vs_tiga_dimensi.txt",
  final_ESI_MI_ge10.csv = "cfa/esi/mi_final.csv",
  final_PSMLS_MI_ge10.csv = "cfa/psmls/mi_final.csv",
  final_PEB_MI_ge10.csv = "cfa/peb/mi_final.csv",
  final_PWB_MI_ge10.csv = "cfa/pwb/mi_final.csv",
  descriptive_statistics_primary_TSFS_scores.csv = "deskriptif/statistik_deskriptif.csv",
  correlation_matrix_primary_TSFS_scores.csv = "deskriptif/korelasi.csv",
  correlation_pvalues_primary_TSFS_scores.csv = "deskriptif/nilai_p_korelasi.csv",
  TSFS_all_score_descriptives.csv = "deskriptif/statistik_semua_skor.csv",
  TSFS_all_score_correlations.csv = "deskriptif/korelasi_semua_skor.csv",
  TSFS_mediator_equation_HC3.csv = "model_7/koefisien_mediator_hc3.csv",
  TSFS_mediator_equation_OLS.csv = "model_7/koefisien_mediator_ols.csv",
  TSFS_outcome_equation_HC3.csv = "model_7/koefisien_outcome_hc3.csv",
  TSFS_outcome_equation_OLS.csv = "model_7/koefisien_outcome_ols.csv",
  TSFS_assumption_diagnostics.csv = "model_7/diagnostik_asumsi.csv",
  TSFS_R2.csv = "model_7/r_kuadrat.csv",
  TSFS_simple_slopes.csv = "model_7/simple_slopes.csv",
  TSFS_Johnson_Neyman.txt = "model_7/johnson_neyman.txt",
  TSFS_bootstrap_indirect_effects.csv = "model_7/efek_tidak_langsung_bootstrap.csv",
  TSFS_bootstrap_raw.rds = "model_7/bootstrap_mentah.rds",
  TSFS_Bartlett_outcome_score_check.csv = "model_7/pemeriksaan_skor_bartlett.csv",
  TSFS_SCORING_METHOD.txt = "model_7/metode_skor_tsfs.txt",
  TSFS_Interaction_ESI_PSMLS_PEB.png = "model_7/grafik_interaksi.png",
  TSFS_Johnson_Neyman_ESI_PSMLS_PEB.png = "model_7/grafik_johnson_neyman.png",
  TSFS_Model_7_Estimated.png = "model_7/gambar_model_7.png",
  analysis_objects_final_TSFS.rds = "model_7/objek_analisis.rds"
)

rapikan_hasil_tsfs <- function(run_dir, data_dir = "data") {
  if (!dir.exists(run_dir)) stop("Folder hasil tidak ditemukan: ", run_dir)
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

  move_once <- function(source, destination) {
    if (!file.exists(source)) {
      return(invisible(FALSE))
    }
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(destination)) {
      if (!file.remove(destination)) stop("Gagal mengganti hasil lama: ", destination)
    }
    if (!file.rename(source, destination)) {
      stop("Gagal memindahkan: ", source, " -> ", destination)
    }
    invisible(TRUE)
  }

  for (extension in c("csv", "rds")) {
    source <- file.path(
      run_dir, paste0("TSFS_factor_scores_and_structural_data.", extension)
    )
    destination <- file.path(
      data_dir, paste0("skor_faktor_tsfs.", extension)
    )
    move_once(source, destination)
  }

  for (old_name in names(.nama_hasil)) {
    move_once(
      file.path(run_dir, old_name),
      file.path(run_dir, .nama_hasil[[old_name]])
    )
  }

  invisible(TRUE)
}

pisahkan_tabel_cfa <- function(run_dir) {
  cfa_dir <- file.path(run_dir, "cfa")
  models <- c("ESI", "PSMLS", "PEB", "PWB")

  riwayat_file <- file.path(cfa_dir, "riwayat_model.csv")
  if (file.exists(riwayat_file)) {
    riwayat <- utils::read.csv(riwayat_file)
    for (model in models) {
      rows <- riwayat[grepl(paste0("^", model, "_"), riwayat$Model), , drop = FALSE]
      if (!nrow(rows)) next
      model_dir <- file.path(cfa_dir, tolower(model))
      dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(rows, file.path(model_dir, "riwayat_model.csv"), row.names = FALSE)
    }
  }

  fit_file <- file.path(cfa_dir, "ringkasan_model.csv")
  if (file.exists(fit_file)) {
    fits <- utils::read.csv(fit_file)
    for (model in models) {
      rows <- fits[fits$Model == model, , drop = FALSE]
      if (nrow(rows) != 1L) stop("Baris fit CFA tidak unik: ", model)
      model_dir <- file.path(cfa_dir, tolower(model))
      dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(
        rows, file.path(model_dir, "kecocokan_model.csv"),
        row.names = FALSE
      )
    }
  }

  split_table <- function(source_name, target_name) {
    source <- file.path(cfa_dir, source_name)
    if (!file.exists(source)) {
      return(invisible(NULL))
    }
    table <- utils::read.csv(source)
    if (!"Construct" %in% names(table)) {
      stop("Kolom Construct tidak ditemukan: ", source)
    }
    for (model in models) {
      rows <- table[table$Construct == model, , drop = FALSE]
      if (!nrow(rows)) next
      model_dir <- file.path(cfa_dir, tolower(model))
      dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(
        rows, file.path(model_dir, target_name),
        row.names = FALSE
      )
    }
    unlink(source)
    invisible(NULL)
  }

  split_table("loading_item_final.csv", "loading_item.csv")
  split_table("reliabilitas_dimensi.csv", "reliabilitas_dimensi.csv")
  split_table("reliabilitas_konstruk.csv", "reliabilitas_konstruk.csv")
  invisible(TRUE)
}

buat_manifest_hasil <- function(run_dir, data_dir = "data") {
  result_files <- list.files(run_dir, recursive = TRUE, full.names = TRUE)
  result_files <- result_files[!file.info(result_files)$isdir]
  result_files <- result_files[
    basename(result_files) != "daftar_berkas.csv" &
      basename(result_files) != "MANIFEST.csv" &
      basename(result_files) != ".DS_Store"
  ]
  data_files <- file.path(
    data_dir, paste0("skor_faktor_tsfs", c(".csv", ".rds"))
  )
  data_files <- data_files[file.exists(data_files)]
  files <- c(result_files, data_files)
  manifest <- data.frame(
    Lokasi = files,
    MD5 = unname(tools::md5sum(files)),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    manifest, file.path(run_dir, "daftar_berkas.csv"),
    row.names = FALSE
  )
  invisible(manifest)
}
