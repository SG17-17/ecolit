# Jalankan dari folder proyek: Rscript R/buat_ulang_plot.R
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) {
  stop("Berikan paling banyak satu folder hasil sebagai argumen.")
}

run_dir <- if (length(args)) args[[1L]] else "hasil_analisis"
score_file <- file.path("data", "skor_faktor_tsfs.rds")
bp_file <- file.path(run_dir, "model_7", "diagnostik_asumsi.csv")
if (!file.exists(score_file) || !file.exists(bp_file)) {
  stop("Data skor TSFS atau diagnostik asumsi belum tersedia. Jalankan analisis_final.R terlebih dahulu.")
}

tsfs <- readRDS(score_file)
bp <- utils::read.csv(bp_file, check.names = FALSE)
bp_m <- bp[bp$Equation == "PEB mediator equation", , drop = FALSE]
if (nrow(bp_m) != 1L || !bp_m$Inference_SE %in% c("HC3", "OLS")) {
  stop("Jenis galat baku model mediator tidak ditemukan dalam diagnostik_asumsi.csv.")
}

model_M <- stats::lm(PEB_B ~ ESI_c * PSMLS_c, data = tsfs)
use_hc3_M <- bp_m$Inference_SE == "HC3"
source("R/plot_model_7.R", local = TRUE)

files <- generate_tsfs_final_plots(
  model_M, tsfs, file.path(run_dir, "model_7"), use_hc3_M,
  file_names = c(
    johnson_neyman = "grafik_johnson_neyman.png",
    interaction = "grafik_interaksi.png"
  )
)
model_Y <- stats::lm(PWB_B ~ PEB_R_Y + ESI_R_Y, data = tsfs)
slopes_file <- file.path(run_dir, "model_7", "simple_slopes.csv")
bootstrap_file <- file.path(run_dir, "model_7", "efek_tidak_langsung_bootstrap.csv")
if (!file.exists(slopes_file)) stop("Hasil simple slopes belum tersedia: ", slopes_file)
simple_slopes <- utils::read.csv(slopes_file)
bootstrap_summary <- if (file.exists(bootstrap_file)) {
  utils::read.csv(bootstrap_file)
} else data.frame()
model_file <- buat_gambar_model_7(
  model_M, model_Y, simple_slopes, bootstrap_summary,
  file.path(run_dir, "model_7", "gambar_model_7.png"),
  use_hc3_M = use_hc3_M
)
source("R/atur_hasil.R", local = TRUE)
buat_manifest_hasil(run_dir)
cat(paste(c(files, model = model_file), collapse = "\n"), "\n")
