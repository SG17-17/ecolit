# Analisis ANECO

Jalankan script dari folder proyek. Input utama adalah `data/data_bersih.rds`
(N = 704). Setiap run **memperbarui file pada lokasi yang sama** di
`hasil_analisis/`; tidak dibuat folder bertanggal.

## CFA per variabel

```r
source("R/cfa_esi.R")
source("R/cfa_psmls.R")
source("R/cfa_peb.R")
source("R/cfa_pwb.R")
```

Jalankan hanya script variabel yang diperlukan. Setiap script memuat model awal,
model pembanding, model akhir, reliability, dan plot. `R/helpers.R` dan
`R/cfa_helpers.R` menyediakan pemetaan item dan format laporan.

| Variabel | Laporan yang selalu diperbarui |
| --- | --- |
| ESI | `hasil_analisis/cfa/esi/laporan_cfa.txt` |
| PSMLS | `hasil_analisis/cfa/psmls/laporan_cfa.txt` |
| PEB | `hasil_analisis/cfa/peb/laporan_cfa.txt` |
| PWB | `hasil_analisis/cfa/pwb/laporan_cfa.txt` |

Tiap folder model juga berisi `plot_model_awal.png` dan `plot_model.png`.
PWB memiliki `plot_method_factor.png`. Riwayat model, factor loading,
reliability, modification indices, dan perbandingan yang relevan berada
dalam satu TXT per variabel. Script CFA tidak membuat CSV.

## Analisis lengkap

```r
source("analisis_final.R")
```

Script utama menjalankan keempat CFA, factor scores TSFS, deskriptif, uji
hipotesis, bootstrap, dan tiga gambar Model 7. Hasil utamanya diperbarui di
`hasil_analisis/laporan_lengkap.txt`. Factor scores responden disimpan dengan
nama tetap di `data/skor_faktor_tsfs.csv` dan `.rds`; output deskriptif dan
Model 7 ada di folder masing-masing dalam `hasil_analisis/`.

Untuk menjalankan hanya sampai CFA atau factor scores, ubah `TAHAP_TERAKHIR`
di bagian atas `analisis_final.R` menjadi `"cfa"` atau `"skor"`. Nilai default
`"hipotesis"` menjalankan seluruh analisis dengan 5.000 bootstrap resamples.

Model CFA final: ESI 6 item dengan `ESI_2 ~~ ESI_3`, PSMLS 14 item,
PEB 16 item dengan empat dimensi, dan PWB 21 favorable items. PSMLS, PEB,
dan PWB memakai faktor second-order. CFA diestimasi dengan MLR dan
`std.lv = TRUE`.

Gambar model untuk naskah berada di
`hasil_analisis/model_7/gambar_model_7.png`. Untuk membuat ulang ketiga
gambar Model 7 dari hasil tetap:

```sh
Rscript R/buat_ulang_plot.R
```

Run lama dan factor scores bertanggal disimpan di `arsip_analisis_lama.zip`.
`R/siapkan_data.R` menyiapkan data bersih dari sumber daring; simpan snapshot
sumber yang sesuai sebelum membangun arsip publikasi.
