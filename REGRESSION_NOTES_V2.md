# Catatan regresi metode Auctus V2.3

Dokumen ini mencatat perubahan hasil yang memang diharapkan ketika berpindah dari
`meta_nma_engine.R.bak.original` ke engine V2.3. Tujuannya adalah membedakan perubahan
metodologis yang disengaja dari regression bug.

## Perubahan yang memengaruhi hasil

| Area | Engine lama | Engine V2.2 | Dampak yang diharapkan |
|---|---|---|---|
| Binary raw | Effect studi dihitung generik dengan koreksi 0.5 pada zero cell | Non-sparse memakai Mantel-Haenszel, sparse OR memakai GLMM, sparse RR memakai Mantel-Haenszel dengan sensitivity TACC | Estimate dan CI dapat berubah, terutama pada rare event |
| Double-zero | Dapat dikeluarkan melalui opsi dan pesan console | Selalu tidak berkontribusi untuk OR/RR, dicatat sebagai `I201_DOUBLE_ZERO_EXCLUDED` | Jumlah studi informatif dapat lebih kecil dari jumlah studi input |
| HR | Event count dapat diperlakukan sebagai pendekatan RR | HR hanya diterima sebagai reported HR dengan CI atau SE | Analisis yang sebelumnya memakai pendekatan HR dari event akan berhenti pada validasi |
| Random-effects CI | REML digunakan, tetapi CI random tidak selalu memakai Hartung-Knapp | REML dan Hartung-Knapp dengan safeguard pada model yang mendukung | CI dapat lebih lebar pada jumlah studi kecil |
| Proportion | Transformasi log `PLN` | GLMM logit tanpa continuity correction | Zero-event tetap dapat masuk dan pooled proportion dapat berubah |
| Continuous SMD | Beberapa jalur input dapat bergantung pada label effect | Arm-level SMD selalu dihitung sebagai Hedges g | Estimate SMD dapat sedikit berubah karena small-sample correction |
| Median dan quantile | Rumus Wan/Luo internal | Quantile Estimation dari `estmeansd`, dengan warning dan sensitivity tanpa hasil konversi | Mean, SD, pooled effect, dan CI dapat berubah |
| NMA multi-arm | User memasukkan contrast pairwise | User memasukkan satu baris per arm, engine membentuk contrast dan koreksi multi-arm | SE tidak lagi diperlakukan sebagai contrast independen dan peserta tidak dihitung ganda |
| Reference dan arah | Reference dapat dipilih otomatis | Reference dan outcome direction wajib eksplisit | Orientasi effect, label Favours, dan ranking menjadi deterministik |
| Mixed estimates | Risiko satu studi muncul sebagai raw dan reported effect | Prioritas adjusted, crude, lalu raw-derived diterapkan per studi | Satu studi tidak dihitung dua kali; sensitivity menurut sumber dibuat bila cukup data |

## Perubahan presentasi V2.2.1

Perubahan V2.2.1 hanya menyentuh presentasi dan pelaporan. Baseline numerik di bawah tidak berubah.

| Area | V2.2.0 | V2.2.1 |
|---|---|---|
| Forest pairwise | Layout grid berwarna biru | Gaya `meta::forest` seperti V1 dengan marker hitam, pooled diamond, heterogeneity, prediction interval, dan label Favours |
| Kolom binary | Gabungan effect text dan weight | `event/sample` per treatment, effect natural, 95% CI, dan random weight |
| Kolom continuous | Gabungan effect text dan weight | `mean (SD); n` per treatment, effect natural, 95% CI, dan random weight |
| Subgroup | Ringkasan satu baris per subgroup | Studi individual, pooled subgroup, heterogeneity subgroup, overall result, dan test for subgroup differences |
| Leave-one-out | Layout forest grid V2.2 | Base plot ala V1 dengan CI hitam dan garis overall merah |
| Bubble plot | Tema ggplot V2.2 | Base plot ala V1 dengan inverse-variance bubbles, fitted line, CI band, R2, dan p value moderator |
| NMA forest | Layout grid V2.2 | Gaya forest V1 dengan kolom jumlah studi, peserta unik, dan P-score tetap dipertahankan |
| Laporan | `report.html` dengan path gambar relatif | `report.html` self-contained dan laporan lengkap `report.md` |

## Perubahan schema dan presentasi V2.3

V2.3 tidak mengubah baseline numerik model utama. Perubahan berikut mengurangi beban input dan memperjelas forest plot.

| Area | V2.2 | V2.3 |
|---|---|---|
| Aktivasi baris | Kolom `include` pada sheet input | Semua baris berisi otomatis aktif; baris kosong diabaikan |
| Template produksi | Contoh dapat berada pada tabel input | Tabel input kosong dan contoh dipindahkan ke `CONTOH_PENGISIAN` read-only |
| Migrasi | V1 dan V2 lama dikonversi ke schema berbasis label | Nilai legacy `include=FALSE` dibuang dan dicatat pada `MIGRATION_LOG` |
| Forest raw binary | Nilai arm ditampilkan ringkas | Header treatment dinamis dengan subkolom Event dan Total |
| Forest raw continuous | Nilai arm ditampilkan ringkas | Header treatment dinamis dengan subkolom Mean, SD, dan Total |
| Forest reported atau mixed | Source ditulis pada baris studi | Hanya Total per arm; Adjusted, Crude, dan Raw-derived menjadi blok source |
| Forest subgroup mixed | Source dapat membentuk tampilan bertingkat | Subgroup klinis tetap menjadi blok; source menjadi kolom |
| Label arah | Dinamis menurut treatment dan outcome direction | Dinamis menurut treatment dan outcome direction, dengan ruang header dihitung dari panjang label |
| Output source | Sensitivity menurut sumber | Ditambah `SOURCE_SUBGROUP`, object `source_subgroup`, serta keputusan `forest_mode` |

## Baseline numerik fixture V2.2

Fixture end-to-end pada `tests/testthat/helper-fixtures.R` dikunci dengan toleransi
`1e-6` dalam test suite.

| outcome_name | Hasil utama |
|---|---|
| `Mortality` | OR 0.4689902822 |
| `Systolic blood pressure` | MD -5.6965364830 |
| `Incidence` | Proportion 0.0470326218 |
| `Clinical response`, Treatment A vs Placebo | OR 0.4830962070 |
| `Clinical response`, Treatment B vs Placebo | OR 0.6911540176 |
| `Diagnostic accuracy`, sensitivity | 0.8361507487 |
| `Diagnostic accuracy`, specificity | 0.9412682809 |

## Aturan audit

1. Perubahan baseline hanya diterima bila ada keputusan metode yang tercatat pada
   `METHOD_DECISIONS` dan alasan klinis atau statistiknya terdokumentasi.
2. Perubahan package version harus terlihat pada `manifest.json`.
3. Perubahan akibat input harus dapat ditelusuri ke `outcome_name`, `study_label`, dan
   lokasi sel pada workbook validasi. ID internal tersedia pada `id_map.csv` untuk audit teknis.
4. Script lama tidak di-`source()` saat audit karena melakukan instalasi package,
   penghapusan object di `.GlobalEnv`, perubahan working directory, dan penutupan
   graphics device. Perbandingan lama dilakukan melalui inspeksi implementasi dan
   baseline V2.2 diuji secara executable.
