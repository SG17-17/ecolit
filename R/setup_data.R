library(googlesheets4)

gs4_deauth()
link_spreadsheet <- "https://docs.google.com/spreadsheets/d/1u7L4GSSsuVDv9j7ccgWvccwr7A5Ks0szkiY-RTHVsA8/edit?usp=sharing"

cat("Mengunduh data dari Google Sheets...\n")
df <- read_sheet(link_spreadsheet, sheet = "DATA FINAL")

df[] <- lapply(df, function(x) {
  if (is.list(x)) {
    x <- unlist(lapply(x, function(v) if(is.null(v)) NA else as.character(v)))
  }
  return(x)
})
df <- as.data.frame(df, check.names = FALSE)

df$Gender   <- df[["Jenis Kelamin"]]
df$Usia     <- df[["Usia Anda saat ini (dalam angka):"]]
df$Semester <- as.character(df[["Semester yang anda jalani saat ini"]])
df$Provinsi <- df[["Daerah tempat tinggal saat ini"]]
df$Area     <- df[["Mana yang paling menggambarkan area tempat tinggal Anda saat ini?"]]
df$Hunian   <- df[["Selama berkuliah, jenis tempat tinggal utama Anda adalah:"]]

dir.create("data", showWarnings = FALSE)
saveRDS(df, "data/cleaned_data.rds")
cat("Data berhasil disimpan ke data/cleaned_data.rds\n")
