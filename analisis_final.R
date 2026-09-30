# Analisis final ANECO ----
# Jalankan dari folder proyek: source("analisis_final.R")
# Input: data/data_bersih.rds (N = 704).
# Output tetap di hasil_analisis/ dan skor faktor di data/.
# Alur: pengujian CFA bertahap -> CFA final -> TSFS -> Model 7 -> laporan.
# Script ini dimulai dari data bersih; penyiapan data ada di R/siapkan_data.R.
# Rincian model, item, dan lokasi hasil dijelaskan di README.md.


# ==============================================================================
# 0. PENGATURAN
# ==============================================================================

DATA_FILE <- "data/data_bersih.rds"
OUTPUT_ROOT <- "hasil_analisis"
# Pilih "cfa", "skor", atau "hipotesis". Tiap pilihan menjalankan prasyaratnya.
TAHAP_TERAKHIR <- "hipotesis"
RUN_MODEL_STAGES <- TRUE
RUN_BOOTSTRAP <- TRUE
RUN_PLOTS <- TRUE
BOOT_R <- 5000L
BOOT_SEED <- 17012000L
EXPECTED_N <- 704L

if (!TAHAP_TERAKHIR %in% c("cfa", "skor", "hipotesis")) {
  stop('TAHAP_TERAKHIR harus "cfa", "skor", atau "hipotesis".')
}


# ==============================================================================
# 1. PAKET YANG DIPERLUKAN
# ==============================================================================

required_packages <- c(
  "lavaan",
  "semTools",
  "knitr",
  "sandwich",
  "lmtest",
  "interactions",
  "ggplot2",
  "scales",
  "semPlot"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages)) {
  stop(
    "Package belum tersedia: ",
    paste(missing_packages, collapse = ", "),
    "\nInstal terlebih dahulu dengan:\n",
    "install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}


# ==============================================================================
# 2. DATA DAN FOLDER HASIL
# ==============================================================================

if (!file.exists(DATA_FILE)) {
  stop(
    "File tidak ditemukan: ",
    DATA_FILE,
    "\nJalankan script dari root project atau ubah DATA_FILE."
  )
}

df <- readRDS(DATA_FILE)

if (!is.data.frame(df)) {
  stop("data_bersih.rds harus berupa data.frame.")
}

if (nrow(df) != EXPECTED_N) {
  stop(
    "N berbeda dari arsip analisis yang diharapkan. ",
    "Expected N = ", EXPECTED_N,
    "; current N = ", nrow(df), "."
  )
}

OUT <- OUTPUT_ROOT
if (!dir.exists(OUT) && !dir.create(OUT, recursive = TRUE)) {
  stop("Gagal membuat folder output.")
}

options(width = 180)

# 3. JALANKAN ANALISIS ----
# Semua blok dijalankan dalam environment yang sama.
source("R/helpers.R", local = TRUE)
source("R/cfa_helpers.R", local = TRUE)
source("R/fungsi_analisis.R", local = TRUE)
source("R/plot_cfa.R", local = TRUE)
CFA_DARI_ANALISIS_FINAL <- TRUE
source("R/cfa_esi.R", local = TRUE)
source("R/cfa_psmls.R", local = TRUE)
source("R/cfa_peb.R", local = TRUE)
source("R/cfa_pwb.R", local = TRUE)
rm(CFA_DARI_ANALISIS_FINAL)
source("R/ringkasan_cfa.R", local = TRUE)
if (TAHAP_TERAKHIR %in% c("skor", "hipotesis")) {
  source("R/skor_faktor.R", local = TRUE)
}
if (TAHAP_TERAKHIR == "hipotesis") {
  source("R/uji_hipotesis.R", local = TRUE)
}

# ==============================================================================
# 4. TATA HASIL + MANIFEST
# ==============================================================================

if (!file.exists("R/atur_hasil.R")) {
  stop("Helper tata letak tidak ditemukan: R/atur_hasil.R")
}
source("R/atur_hasil.R", local = TRUE)
rapikan_hasil_tsfs(OUT, data_dir = "data")
if (TAHAP_TERAKHIR != "cfa") {
  buat_manifest_hasil(OUT, data_dir = "data")
}


# ==============================================================================
# 5. SELESAI
# ==============================================================================

cat("\n")
cat(strrep("=", 90), "\n")
cat("ANALISIS ANECO SELESAI: ", TAHAP_TERAKHIR, "\n", sep = "")
cat(strrep("=", 90), "\n")
cat("Output folder:\n")
cat(normalizePath(OUT), "\n\n")
cat("File utama:\n")
if (TAHAP_TERAKHIR == "hipotesis") {
  cat(file.path(OUT, "laporan_lengkap.txt"), "\n\n")
} else if (TAHAP_TERAKHIR == "skor") {
  cat(file.path("data", "skor_faktor_tsfs.csv"), "\n\n")
} else {
  cat(file.path(OUT, "cfa", "<variabel>", "laporan_cfa.txt"), "\n\n")
}
cat("Catatan:\n")
cat("- data_bersih.rds tidak diubah.\n")
if (TAHAP_TERAKHIR != "cfa") {
  cat("- Skor faktor TSFS ada di folder data.\n")
}
cat("- Simpan skrip + data_bersih.rds + seluruh folder hasil untuk arsip.\n")
