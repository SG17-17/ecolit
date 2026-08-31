library(googlesheets4)

gs4_deauth()
link_spreadsheet <- "https://docs.google.com/spreadsheets/d/1u7L4GSSsuVDv9j7ccgWvccwr7A5Ks0szkiY-RTHVsA8/edit?usp=sharing"

cat("Mengunduh data mentah dari Google Sheets...\n")
df_raw <- read_sheet(link_spreadsheet, sheet = "Scoring")

df_raw[] <- lapply(df_raw, function(x) {
  if (is.list(x)) {
    x <- unlist(lapply(x, function(v) if(is.null(v)) NA else as.character(v)))
  }
  return(x)
})
df_raw <- as.data.frame(df_raw, check.names = FALSE)

# --- KALKULASI INFO DATABASE ---
total_masuk <- nrow(df_raw)
total_gugur <- sum(df_raw[["Status Validitas"]] == "GUGUR", na.rm = TRUE)
total_valid <- sum(df_raw[["Status Validitas"]] == "VALID", na.rm = TRUE)
# Kita pastikan Sys.time mencetak dalam WIB
Sys.setenv(TZ = "Asia/Jakarta")
waktu_update <- format(Sys.time(), "%d %B %Y, Pukul %H:%M WIB")

db_info <- list(
  total_masuk = total_masuk,
  total_gugur = total_gugur,
  total_valid = total_valid,
  waktu_update = waktu_update
)

dir.create("data", showWarnings = FALSE)
saveRDS(db_info, "data/db_info.rds")
cat("Info database berhasil disimpan ke data/db_info.rds\n")

# --- PEMBUATAN DATA BERSIH (DATA FINAL) ---
df <- df_raw[df_raw[["Status Validitas"]] == "VALID", ]

# Transformasi variabel demografi
df$Gender   <- df[["Jenis Kelamin"]]
df$Usia     <- df[["Usia Anda saat ini (dalam angka):"]]
df$Semester <- as.character(df[["Semester yang anda jalani saat ini"]])
df$Provinsi <- df[["Daerah tempat tinggal saat ini"]]
df$Area     <- df[["Mana yang paling menggambarkan area tempat tinggal Anda saat ini?"]]
df$Hunian   <- df[["Selama berkuliah, jenis tempat tinggal utama Anda adalah:"]]

saveRDS(df, "data/cleaned_data.rds")
cat("Data bersih berhasil disimpan ke data/cleaned_data.rds\n")
