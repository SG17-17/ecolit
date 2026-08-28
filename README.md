# 📊 Laporan Analisis CFA Realtime

[![Render and Publish Quarto](https://github.com/SG17-17/ecolit/actions/workflows/quarto-publish.yml/badge.svg)](https://github.com/SG17-17/ecolit/actions/workflows/quarto-publish.yml)

Repositori ini berisi sistem otomatisasi untuk skoring dan pelaporan **Confirmatory Factor Analysis (CFA)** secara *realtime*. Sistem ini dirancang untuk membaca data dari Google Sheets, melakukan uji validitas dan reliabilitas konstruk menggunakan R, dan secara otomatis mempublikasikan hasilnya ke dalam format Buku Interaktif (Quarto Book).

👉 **[Buka Halaman Laporan Live Disini](https://SG17-17.github.io/ecolit/)**

## ⚙️ Cara Kerja Sistem (Workflow)

Sistem pelaporan ini berjalan secara otomatis tanpa intervensi manual:

1. **Input Data**: Responden mengisi survei, data masuk ke Google Sheets.
2. **Kalkulasi R**: Skrip `index.qmd` menarik data terbaru menggunakan paket `googlesheets4`, kemudian melakukan pengujian CFA menggunakan paket `lavaan` dan `semTools`.
3. **Automasi GitHub Actions**: Setiap tengah malam (atau setiap ada pembaruan pada *file* repositori), GitHub Actions menyala secara otomatis untuk me-render kode R menjadi dokumen HTML yang utuh.
4. **Deploy ke GitHub Pages**: Hasil akhir langsung dipublikasikan ke cabang `gh-pages` sehingga bisa diakses dari *browser* mana saja.

## 📂 Struktur File

- `index.qmd` — Skrip inti (*Quarto markdown*) yang berisi kombinasi antara tulisan laporan, teori, dan kode pemrograman R.
- `_quarto.yml` — Konfigurasi struktur Quarto untuk mengubah format laporan menjadi *Book/Website*.
- `.github/workflows/quarto-publish.yml` — Mesin *runner* otomatis dari GitHub Actions.

## 🛠️ Modifikasi Laporan Lokal

Jika Anda ingin menjalankan atau memodifikasi *file* ini di laptop Anda sendiri:
1. *Clone* repositori ini atau buka *folder* `Scoring` di Positron / VS Code / RStudio.
2. Buka `index.qmd` dan jalankan kode baris-demi-baris (*Run Chunk*) untuk melihat grafik CFA.
3. Gunakan *tab Source Control* (Git) di panel kiri untuk melakukan **Commit & Push** jika Anda membuat perubahan pada laporan.

---
*Dibuat menggunakan [Quarto](https://quarto.org/) dan R.*
