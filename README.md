# 📊 Laporan Analisis CFA Realtime

[![Render and Publish Quarto](https://github.com/SG17-17/ecolit/actions/workflows/quarto-publish.yml/badge.svg)](https://github.com/SG17-17/ecolit/actions/workflows/quarto-publish.yml)

Repositori ini berisi sistem otomatisasi untuk pelaporan **Confirmatory Factor Analysis (CFA)** secara *real-time*. Sistem ini dirancang untuk membaca data dari Google Sheets, melakukan uji validitas dan reliabilitas konstruk menggunakan R (`lavaan`), dan secara otomatis mempublikasikan hasilnya ke dalam format Buku Interaktif.

👉 **[Buka Halaman Laporan Live Disini](https://SG17-17.github.io/ecolit/)**

## 📂 Struktur File

Sistem ini memecah kode menjadi beberapa modul agar rapi:

- `R/setup_data.R` — Skrip yang bertanggung jawab **mengunduh data** dari Google Sheets dan menyimpannya sebagai `data/cleaned_data.rds`. Skrip ini otomatis berjalan *sebelum* buku di-*render*.
- `R/helpers.R` — Kumpulan fungsi bantuan (pembuat tabel demografi & penghitung reliabilitas) agar kode di dalam bab tidak berantakan.
- `index.qmd` — Halaman sampul dan analisis Demografi.
- `02_eid.qmd` sd `06_pwb.qmd` — Bab-bab spesifik yang menghitung CFA untuk masing-masing variabel ukur.
- `07_summary.qmd` — Halaman rekapitulasi *Model Fit* dan Reliabilitas dari semua bab CFA.
- `_quarto.yml` — Konfigurasi tema, *layout*, menu, dan susunan bab buku.
- `custom.scss` — Kustomisasi warna tema dan desain latar belakang web.

---

## 🔄 Cara Mengubah/Update Data Google Sheets

Jika di masa depan Anda memiliki data baru atau ingin mengganti *link* kuesioner Google Sheets:

1. Buka file `R/setup_data.R`.
2. Cari kode berikut (di baris ke-12):
   ```r
   sheet_url <- "https://docs.google.com/spreadsheets/d/1u7L4GSSsuVDv9j7ccgWvccwr7A5Ks0szkiY-RTHVsA8/edit?usp=sharing"
   ```
3. Ganti URL di dalam tanda kutip tersebut dengan *link* Google Sheets Anda yang baru. 
4. Jika nama tab (*Sheet*) di dalamnya bukan "DATA FINAL", pastikan Anda juga mengubah bagian `sheet = "DATA FINAL"`.
5. *Commit* dan *Push* ke GitHub. Sistem akan otomatis mengunduh data baru Anda.

---

## 🛠️ Cara Memodifikasi Model CFA (Lavaan)

Jika Anda ingin mengubah teori model (misalnya menghapus item karena *loading factor* jelek, atau mengubah model *First-Order* menjadi *Second-Order*):

1. Buka *file* bab yang bersangkutan, misalnya `02_eid.qmd`.
2. Cari bagian kode string *Lavaan* (biasanya bernama `model_...`), contoh:
   ```r
   model_eid <- "
     Identity =~ EID_1 + EID_2 + EID_3
   "
   ```
3. Ubah sintaksnya sesuai aturan paket `lavaan`. 
   - `=~` berarti "diukur oleh" (faktor laten).
   - `~~` berarti korelasi antar eror (*error covariance*).
4. **Catatan:** Selalu perhatikan *file* `07_summary.qmd`. Halaman ringkasan ini mengambil hasil dari *file* `.rds` yang disimpan oleh tiap-tiap bab (contoh: `data/fit_eid.rds`). Selama bab Anda berhasil melakukan proses *saveRDS()*, halaman Summary akan ikut menyesuaikan secara otomatis.

---

## 🤖 Cara Kerja Automasi (GitHub Actions)

Setiap tengah malam (atau setiap ada klik "Commit & Push" pada repositori ini), GitHub Actions akan:
1. Menyiapkan sistem operasi Ubuntu kosong.
2. Menginstal R, Quarto, dan paket-paket statistik (`lavaan`, `semTools`, `googlesheets4`).
3. Menjalankan `R/setup_data.R` untuk menarik Google Sheets terbaru.
4. Me-*render* semua bab `.qmd` menjadi HTML.
5. Mempublikasikan (*Deploy*) halaman HTML tersebut ke cabang `gh-pages`.
