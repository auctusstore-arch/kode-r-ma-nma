# Auctus MA dan NMA Engine V2.3

Manual ini menjelaskan cara mengisi template Excel, menjalankan script R, memperbaiki error, dan membaca hasil.

## Daftar isi

1. [Cara paling singkat menjalankan Auctus](#1-cara-paling-singkat-menjalankan-auctus)
2. [Membuat template baru](#2-membuat-template-baru)
3. [Prinsip kunci V2.3](#3-prinsip-kunci-v23)
   - [`outcome_name` adalah kunci analisis](#31-outcome_name-adalah-kunci-analisis)
   - [`study_label` adalah kunci studi global](#32-study_label-adalah-kunci-studi-global)
   - [ID internal](#33-id-internal)
   - [Semua baris yang diisi otomatis aktif](#34-semua-baris-yang-diisi-otomatis-aktif)
   - [Migrasi workbook lama](#35-migrasi-workbook-lama)
4. [Mengisi sheet `analyses`](#4-mengisi-sheet-analyses)
   - [Mengapa `timepoint` tidak wajib?](#41-mengapa-timepoint-tidak-wajib)
   - [Mengapa `effect_measure` kondisional?](#42-mengapa-effect_measure-kondisional)
   - [Arah outcome](#43-arah-outcome)
5. [Mengisi `study_metadata`](#5-mengisi-study_metadata)
6. [Mengisi `arm_data`](#6-mengisi-arm_data)
   - [Studi multi-arm](#61-studi-multi-arm)
   - [Event nol dan incidence nol](#62-event-nol-dan-incidence-nol)
   - [Continuous outcome](#63-continuous-outcome)
7. [Mengisi `effect_data`](#7-mengisi-effect_data)
8. [Mengisi `diagnostic_data`](#8-mengisi-diagnostic_data)
9. [Validasi sebelum analisis](#9-validasi-sebelum-analisis)
   - [Warna validasi](#91-warna-validasi)
   - [Error label utama](#92-error-label-utama)
10. [Strict mode dan valid-only mode](#10-strict-mode-dan-valid-only-mode)
11. [Instalasi dependency otomatis](#11-instalasi-dependency-otomatis)
12. [Metode statistik utama](#12-metode-statistik-utama)
13. [Template penulisan bagian Methods](#13-template-penulisan-bagian-methods)
    - [Cara memakai template](#131-cara-memakai-template)
    - [Paragraf pembuka umum](#132-paragraf-pembuka-umum)
    - [Pairwise meta-analysis untuk outcome binary](#133-pairwise-meta-analysis-untuk-outcome-binary)
    - [Pairwise meta-analysis untuk outcome continuous](#134-pairwise-meta-analysis-untuk-outcome-continuous)
    - [Single-arm meta-analysis](#135-single-arm-meta-analysis)
    - [Diagnostic meta-analysis](#136-diagnostic-meta-analysis)
    - [Analisis tambahan untuk pairwise dan proportion meta-analysis](#137-analisis-tambahan-untuk-pairwise-dan-proportion-meta-analysis)
    - [Network meta-analysis](#138-network-meta-analysis)
    - [Pemeriksaan dan keluaran tambahan NMA](#139-pemeriksaan-dan-keluaran-tambahan-nma)
    - [Pelaporan software dan keputusan metode](#1310-pelaporan-software-dan-keputusan-metode)
14. [Plot](#14-plot)
15. [Struktur hasil](#15-struktur-hasil)
16. [Perbedaan dengan engine lama](#16-perbedaan-dengan-engine-lama)
17. [Checklist sebelum run](#17-checklist-sebelum-run)

## 1. Cara paling singkat menjalankan Auctus

1. Buka `meta_nma_engine.R` di RStudio.
2. Klik **Source**.
3. Jalankan satu baris berikut di Console:

```r
hasil <- run_auctus_meta()
```

4. Pilih workbook Excel melalui file picker.
5. Tunggu sampai folder hasil ditampilkan di Console.

Saat `meta_nma_engine.R` hanya di-Source, script tidak memasang package, tidak membuka file picker, dan tidak menjalankan analisis. Pemeriksaan dan instalasi dependency otomatis dimulai saat `run_auctus_meta()` dijalankan.

Jika ingin menentukan file secara langsung:

```r
hasil <- run_auctus_meta(
  file_path = "/lokasi/data_penelitian.xlsx"
)
```

## 2. Membuat template baru

Template produksi sudah tersedia sebagai `template dataset MA & NMA.xlsx`. Template baru juga dapat dibuat dari script:

```r
create_auctus_template("template_auctus_v23.xlsx")
```

Template berisi sheet berikut:

| Sheet | Fungsi |
|---|---|
| `PETUNJUK` | Panduan singkat dan legenda warna |
| `CONTOH_PENGISIAN` | Contoh read-only yang tidak dibaca engine |
| `analyses` | Satu baris per outcome atau analisis |
| `study_metadata` | Registry global, satu baris per studi |
| `arm_data` | Data event/sample atau mean/SD per arm |
| `effect_data` | Reported OR, RR, HR, MD, atau SMD |
| `diagnostic_data` | Tabel TP, FP, FN, dan TN |
| `LOOKUPS` | Sumber dropdown, disembunyikan dan diproteksi |

User tidak perlu membuat `analysis_id`, `study_id`, atau `arm_id`. Engine membuat ID teknis secara otomatis.

Sheet input produksi sengaja kosong. Salin pola pengisian dari `CONTOH_PENGISIAN` ke sheet input yang sesuai. Jangan memasukkan data penelitian ke `CONTOH_PENGISIAN` karena sheet tersebut tidak dibaca engine.

## 3. Prinsip kunci V2.3

### 3.1 `outcome_name` adalah kunci analisis

`outcome_name` bukan sekadar judul. Nilai ini menentukan data mana yang masuk ke analisis yang sama dan wajib unik pada sheet `analyses`.

Contoh yang baik:

| outcome_name |
|---|
| Mortality at 30 days |
| Mortality at 1 year |
| Clinical response at end of treatment |

Jika dua analisis harus dipisahkan, beri `outcome_name` yang berbeda. Hindari nama generik berulang seperti `Mortality` untuk beberapa follow-up.

Pencocokan label mengabaikan kapitalisasi, spasi di awal atau akhir, dan spasi ganda. Misalnya, ` mortality at 30 days ` tetap terhubung ke `Mortality at 30 days`. Engine mencatat normalisasi tersebut sebagai `INFO`.

Nama yang menjadi sama setelah normalisasi tetap dianggap collision dan menghasilkan `E202_DUPLICATE_OUTCOME_NAME`.

### 3.2 `study_label` adalah kunci studi global

`study_metadata` adalah registry global. Satu studi cukup ditulis satu kali dan dapat dipakai untuk beberapa outcome.

Gunakan label yang unik dan mudah dikenali, misalnya:

- `Smith 2024`
- `Smith 2024a` dan `Smith 2024b` untuk dua publikasi atau studi berbeda
- `Smith 2024 Cohort A` dan `Smith 2024 Cohort B` untuk cohort berbeda

Jangan membuat dua baris metadata untuk studi yang sama. Label yang menjadi sama setelah normalisasi menghasilkan `E204_DUPLICATE_STUDY_LABEL`.

Kolom `subgroup_` dan `moderator_` pada `study_metadata` adalah atribut studi global. Nilainya digunakan pada semua outcome yang melibatkan studi tersebut. Jika nilai moderator memang berbeda menurut outcome atau follow-up, buat variabel yang definisinya tetap valid secara studi atau pertimbangkan pemisahan cohort/study label yang secara metodologis dapat dibenarkan.

### 3.3 ID internal

Engine membuat ID deterministik dari label ternormalisasi:

- analysis internal: `a_` ditambah 12 karakter hash;
- study internal: `s_` ditambah 12 karakter hash.

Urutan baris tidak memengaruhi ID. User tidak perlu melihat, membuat, atau memperbaiki ID tersebut. ID internal hanya disimpan pada:

- `manifest.json`;
- `analysis_objects.rds`;
- `04_logs/id_map.csv`.

Workbook input, workbook koreksi, hasil utama, plot, serta report HTML dan Markdown menggunakan `outcome_name` dan `study_label`.

### 3.4 Semua baris yang diisi otomatis aktif

V2.3 tidak mempunyai kolom `include`. Setiap outcome pada `analyses` dan setiap baris yang mulai diisi pada sheet data dianggap aktif. Baris yang seluruh selnya kosong diabaikan. Baris yang baru diisi sebagian menghasilkan error dengan lokasi sel yang perlu dilengkapi.

Cara mengecualikan data dari salinan workbook:

- Untuk mengecualikan satu studi dari satu outcome, hapus baris studi tersebut dari `arm_data`, `effect_data`, atau `diagnostic_data` untuk outcome terkait.
- Untuk mengecualikan satu outcome, hapus baris data outcome tersebut terlebih dahulu, lalu hapus barisnya dari `analyses`.
- Untuk menghapus satu studi dari seluruh workbook, hapus semua baris datanya pada seluruh outcome, lalu hapus baris studinya dari `study_metadata` bila tidak lagi dipakai.

Jangan hanya mengosongkan satu sel pada baris yang masih berisi data lain. Engine akan membaca baris tersebut sebagai baris aktif yang belum lengkap.

### 3.5 Migrasi workbook lama

`run_auctus_meta()` mendeteksi V1, V2 berbasis ID, dan V2.2 berbasis label. Workbook lama dikonversi ke salinan V2.3 sebelum divalidasi. File sumber tidak diubah.

Saat workbook lama masih mempunyai kolom `include`, baris `TRUE` dipertahankan, baris `FALSE` dibuang, dan nilai kosong mengikuti perilaku lama sebagai `TRUE`. Sheet `MIGRATION_LOG` pada salinan konversi mencatat jumlah baris yang dibuang dan alasannya. Metadata studi yang tidak lagi dirujuk juga dibuang dari salinan.

## 4. Mengisi sheet `analyses`

Isi satu baris untuk setiap analisis.

| Kolom | Wajib | Keterangan |
|---|---:|---|
| `outcome_name` | Ya | Nama unik sekaligus kunci analisis |
| `analysis_type` | Ya | `pairwise_ma`, `nma`, `proportion_ma`, atau `diagnostic_ma` |
| `timepoint` | Tidak | Keterangan tambahan saja |
| `outcome_type` | Ya | `binary`, `continuous`, `proportion`, `mean`, atau `diagnostic` |
| `effect_measure` | Kondisional | OR, RR, HR, MD, atau SMD sesuai analisis |
| `reference_treatment` | Kondisional | Wajib untuk pairwise MA dan NMA |
| `outcome_direction` | Ya | `lower_better`, `higher_better`, atau `neutral` |
| `unit` | Kondisional | Wajib untuk MD, disarankan untuk mean |
| `notes` | Tidak | Catatan user |

### 4.1 Mengapa `timepoint` tidak wajib?

`timepoint` hanya keterangan tambahan dan bukan kunci penggabungan. Cara paling aman adalah menuliskan waktu langsung pada `outcome_name`, misalnya `Mortality at 30 days`, lalu membedakan outcome lain dengan nama berbeda.

### 4.2 Mengapa `effect_measure` kondisional?

Effect measure diperlukan untuk analisis komparatif:

- binary: `OR`, `RR`, atau reported `HR`;
- continuous: `MD` atau `SMD`.

Effect measure tidak perlu diisi untuk:

- `proportion_ma` dengan `outcome_type = proportion`, karena engine memodelkan proportion dan mengembalikannya ke skala persentase;
- `proportion_ma` dengan `outcome_type = mean`, karena hasil tetap berupa pooled mean;
- `diagnostic_ma`, karena sensitivity, specificity, dan SROC ditentukan oleh tabel diagnostik 2x2.

HR hanya diterima sebagai reported HR dengan CI atau SE pada `effect_data`. HR tidak dihitung dari event count.

### 4.3 Arah outcome

`outcome_direction` menjelaskan apakah nilai outcome yang lebih kecil atau lebih besar dianggap lebih baik secara klinis. Isian ini menentukan label `Favours` pada forest plot dan interpretasi ranking NMA.

| Isi | Gunakan ketika | Contoh |
|---|---|---|
| `lower_better` | Nilai outcome yang lebih rendah lebih menguntungkan | Mortality, adverse event, pain score, length of stay, blood pressure bila penurunan adalah tujuan terapi |
| `higher_better` | Nilai outcome yang lebih tinggi lebih menguntungkan | Clinical response, remission, survival, quality of life, functional score |
| `neutral` | Outcome bersifat deskriptif atau tidak memiliki satu arah manfaat yang ingin dinilai | Baseline prevalence, karakteristik populasi, diagnostic accuracy yang dianalisis sebagai sensitivity dan specificity terpisah |

Cara memilihnya:

1. Baca definisi event atau skala yang dimasukkan.
2. Tanyakan apakah nilai yang lebih kecil atau lebih besar merupakan hasil klinis yang lebih baik.
3. Pilih `neutral` bila analisis hanya bertujuan mengestimasi besarnya outcome dan tidak membandingkan arah manfaat.

Contoh binary outcome:

- Jika `event` berarti kematian, pilih `lower_better`.
- Jika `event` berarti kesembuhan atau response, pilih `higher_better`.
- Jangan memilih arah hanya berdasarkan dugaan bahwa treatment tertentu akan lebih efektif.

Contoh continuous outcome:

- Pain score biasanya `lower_better`.
- Functional score biasanya `higher_better`.
- Pastikan definisi skala konsisten. Beberapa instrumen memiliki scoring terbalik.

Untuk OR, RR, atau HR, arah outcome tetap ditentukan dari arti event, bukan dari apakah effect estimate lebih kecil atau lebih besar dari 1. Engine kemudian menggabungkan `outcome_direction`, orientasi `treat1` dan `treat2`, serta `reference_treatment` untuk membuat label `Favours` yang sesuai.

Jika ragu, periksa definisi outcome pada protocol atau artikel sumber. Engine tidak menebak arah outcome secara otomatis karena kesalahan arah dapat membalik interpretasi forest plot dan ranking.

Ranking NMA tidak dibuat untuk outcome `neutral`.

## 5. Mengisi `study_metadata`

Isi satu baris per studi untuk seluruh workbook.

| Kolom | Wajib | Keterangan |
|---|---:|---|
| `study_label` | Ya | Label studi global yang unik |
| `year` | Tidak | Tahun publikasi atau tahun studi |
| `study_design` | Disarankan | RCT, cohort, cross-sectional, dan sebagainya |

Kolom tambahan yang dikenali:

| Prefix | Fungsi | Contoh |
|---|---|---|
| `subgroup_` | Analisis subgroup kategorik | `subgroup_region` |
| `moderator_num_` | Meta-regression numerik dan bubble plot | `moderator_num_mean_age` |
| `moderator_cat_` | Moderator kategorik | `moderator_cat_design_group` |

Meta-regression membutuhkan minimal 10 studi per parameter. Subgroup membutuhkan minimal dua subgroup dengan minimal dua studi per subgroup.

## 6. Mengisi `arm_data`

Setiap baris adalah satu arm. Pilih `outcome_name` dan `study_label` dari dropdown.

| Kolom | Fungsi |
|---|---|
| `outcome_name` | Outcome yang dianalisis |
| `study_label` | Studi dari registry global |
| `treatment` | Nama treatment sekaligus identitas arm dalam studi |
| `event`, `sample` | Data binary atau proportion |
| `mean`, `sd`, `sample` | Data continuous atau pooled mean |
| `median`, `q1`, `q3`, `min`, `max` | Ringkasan untuk konversi mean/SD |
| `notes` | Catatan user |

Tidak ada `arm_id`. Kombinasi `outcome_name`, `study_label`, dan `treatment` sudah cukup untuk mengenali arm.

### 6.1 Studi multi-arm

Masukkan satu baris per arm:

| outcome_name | study_label | treatment | event | sample |
|---|---|---|---:|---:|
| Clinical response | Garcia 2022 | Treatment A | 22 | 120 |
| Clinical response | Garcia 2022 | Treatment B | 17 | 118 |
| Clinical response | Garcia 2022 | Placebo | 31 | 122 |

Studi 2, 3, dan 4 arm memerlukan 2, 3, dan 4 baris input. Engine membuat masing-masing 1, 3, dan 6 contrast internal sambil mempertahankan korelasi multi-arm.

### 6.2 Event nol dan incidence nol

`event = 0` valid. `sample` harus lebih besar dari 0 dan `event` tidak boleh melebihi `sample`.

- Studi dengan zero event pada satu arm tetap dapat dianalisis melalui metode rare-event yang sesuai.
- Studi double-zero tidak memberi informasi untuk pooled OR atau RR dan dikeluarkan dari effect relatif dengan pesan `INFO`.
- Proportion MA dapat menganalisis event nol dengan GLMM logit tanpa continuity correction tetap.

### 6.3 Continuous outcome

Pilihan input:

1. `mean + sd + sample`;
2. `median + q1 + q3 + sample`;
3. `median + min + max + sample`.

Median dan quantile dikonversi memakai Quantile Estimation dari `estmeansd`. Konversi selalu menghasilkan `W301_MEDIAN_CONVERTED` dan sensitivity analysis yang mengecualikan studi hasil konversi.

## 7. Mengisi `effect_data`

Gunakan sheet ini bila studi hanya melaporkan effect estimate, misalnya adjusted OR atau HR.

| Kolom | Keterangan |
|---|---|
| `outcome_name`, `study_label` | Kunci label dari sheet registry |
| `treat1`, `treat2` | Orientasi comparison |
| `effect` | Nilai pada skala natural, bukan log |
| `ci_low`, `ci_high` | Confidence interval pada skala natural |
| `se` | Alternatif CI; untuk ratio measure gunakan SE log-effect |
| `ci_level` | Persentase, biasanya 95 |
| `estimate_type` | `adjusted` atau `crude` |
| `adjustment_variables` | Variabel yang digunakan dalam model adjusted |
| `sample1`, `sample2` | Opsional, dipakai untuk tampilan dan ringkasan peserta |

Dalam satu studi, model utama hanya memilih satu sumber estimate dengan prioritas:

1. adjusted;
2. reported crude;
3. raw-derived dari `arm_data`.

Studi berbeda boleh menyumbang raw, crude, atau adjusted estimate pada outcome yang sama jika estimand kompatibel. Engine memberi `W201_MIXED_ESTIMATE_SOURCE` dan membuat sensitivity analysis menurut sumber estimate serta study design.

Reported contrast dari studi multi-arm harus lengkap atau memiliki informasi covariance yang memadai. Engine tidak merekayasa covariance.

## 8. Mengisi `diagnostic_data`

| Kolom | Keterangan |
|---|---|
| `outcome_name`, `study_label` | Kunci label |
| `tp`, `fp`, `fn`, `tn` | Tabel diagnostik 2x2 |
| `threshold` | Cut-off atau threshold |

Model bivariate Reitsma dan SROC memerlukan tabel 2x2. Sensitivity atau specificity tanpa tabel 2x2 hanya dapat dianalisis secara univariat dan tidak dipakai untuk membentuk bivariate SROC.

## 9. Validasi sebelum analisis

Validator dapat dijalankan terpisah:

```r
cek <- validate_auctus_data("data_penelitian.xlsx")
print(cek)
```

Setiap pemeriksaan membuat:

- `00_validation/validated_input.xlsx`;
- `00_validation/error_log.csv`.

Workbook sumber tidak ditimpa. Buka `validated_input.xlsx`, perbaiki sel yang ditandai, simpan sebagai file baru atau file revisi, lalu jalankan ulang.

### 9.1 Warna validasi

| Warna | Arti | Tindakan |
|---|---|---|
| Merah | `ERROR` | Wajib diperbaiki |
| Kuning | `WARNING` | Analisis dapat berjalan, tetapi asumsi perlu ditinjau |
| Biru muda | `INFO` | Keputusan, normalisasi, atau eksklusi non-blocking |

`ERROR_LOG` menampilkan sheet, baris Excel, kolom, outcome, studi, nilai bermasalah, pesan, saran, contoh, dan hyperlink ke sel.

### 9.2 Error label utama

| Kode | Arti | Cara memperbaiki |
|---|---|---|
| `E202_DUPLICATE_OUTCOME_NAME` | Outcome collision setelah normalisasi | Sisakan satu outcome atau beri nama analisis yang benar-benar berbeda |
| `E203_UNKNOWN_OUTCOME_NAME` | Outcome pada data tidak ada di `analyses` | Pilih dari dropdown atau perbaiki typo |
| `E204_DUPLICATE_STUDY_LABEL` | Studi collision pada registry global | Sisakan satu studi atau bedakan dengan `a/b` atau cohort |
| `E205_UNKNOWN_STUDY_LABEL` | Studi pada data tidak ada di metadata | Pilih dari dropdown atau tambahkan ke `study_metadata` |
| `I101_LABEL_NORMALIZED` | Kapitalisasi atau spasi dinormalisasi | Tidak memblokir, tetapi dropdown disarankan |

Error lain yang sering muncul:

| Kode | Arti |
|---|---|
| `E002_MISSING_REQUIRED` | Sel wajib kosong |
| `E107_EVENT_EXCEEDS_SAMPLE` | Event lebih besar dari sample |
| `E110_INVALID_CI_ORDER` | CI tidak mengapit effect |
| `E201_DUPLICATE_ESTIMATE` | Estimate studi dan comparison terduplikasi |
| `E401_DISCONNECTED_NETWORK` | Network NMA tidak terhubung |
| `E402_INCOMPLETE_MULTIARM_CONTRAST` | Reported contrast multi-arm tidak lengkap |
| `W201_MIXED_ESTIMATE_SOURCE` | Raw, crude, dan adjusted digabung |
| `W301_MEDIAN_CONVERTED` | Median dikonversi ke mean/SD |

## 10. Strict mode dan valid-only mode

Default:

```r
hasil <- run_auctus_meta(run_mode = "strict")
```

Semua analisis berhenti jika ada `ERROR`.

Mode lanjutan:

```r
hasil <- run_auctus_meta(run_mode = "valid_only")
```

Mode ini hanya menjalankan outcome yang lolos validasi. Gunakan dengan sadar karena paket hasil dapat berisi sebagian outcome saja.

## 11. Instalasi dependency otomatis

Saat `run_auctus_meta()` dijalankan, engine:

1. memeriksa package dan versi minimum;
2. memasang package yang hilang atau terlalu lama dari CRAN;
3. memeriksa ulang instalasi;
4. melanjutkan analisis jika semua dependency siap.

Jika package lama sedang aktif di session R setelah diperbarui, engine meminta restart R. Ini mencegah analisis memakai namespace versi lama.

Pemeriksaan tanpa instalasi dapat dilakukan dengan:

```r
check_auctus_dependencies()
```

## 12. Metode statistik utama

- Random-effects, REML, 95% CI, dan Hartung-Knapp dengan safeguard untuk generic inverse-variance.
- Binary raw non-sparse menggunakan Mantel-Haenszel random-effects.
- Sparse OR raw-only menggunakan GLMM random-effects dengan sensitivity Mantel-Haenszel.
- Sparse RR menggunakan Mantel-Haenszel dan sensitivity continuity correction.
- Continuous menggunakan MD atau Hedges g sesuai skala.
- Proportion menggunakan random-effects logit GLMM.
- NMA binary arm-only menggunakan penalized logistic random-effects.
- NMA continuous atau mixed reported effect menggunakan inverse-variance `netmeta` dengan koreksi multi-arm.
- Diagnostic menggunakan bivariate Reitsma dan SROC.

Prediction interval dibuat bila minimal lima studi dan model dapat diestimasi. Funnel plot dan asymmetry test dibuat mulai 10 studi. Leave-one-out dibuat mulai tiga studi.

## 13. Template penulisan bagian Methods

> **Catatan penting:** baca kembali dan sesuaikan template berikut dengan konteks penelitian, jenis outcome, protokol, dan metode yang benar-benar dijalankan. Hapus kalimat untuk analisis yang tidak dilakukan. Jangan menyalin seluruh template secara otomatis ke manuskrip.

Versi R dan package harus diambil dari `01_results/manifest.json`. Keputusan metode untuk setiap outcome harus diperiksa pada sheet `METHOD_DECISIONS` di `results.xlsx`. Template ini membantu penulisan, tetapi bukan pengganti protokol penelitian atau penilaian statistik.

### 13.1 Cara memakai template

1. Pilih bagian sesuai `analysis_type`.
2. Ganti semua teks dalam kurung siku, misalnya `[OR/RR/HR]`, `[OUTCOME]`, dan `[VERSION]`.
3. Untuk pilihan yang dipisahkan garis miring, sisakan hanya pilihan yang digunakan.
4. Hapus analisis tambahan yang tidak dibuat oleh engine.
5. Cocokkan metode akhir dengan `METHOD_DECISIONS`, bukan hanya dengan rencana awal.
6. Laporkan versi package yang benar-benar digunakan, bukan hanya versi minimum yang disyaratkan.

| Jenis analisis | Bagian yang digunakan |
|---|---|
| `pairwise_ma` binary | 13.2, 13.3, dan 13.7 bila relevan |
| `pairwise_ma` continuous | 13.2, 13.4, dan 13.7 bila relevan |
| `proportion_ma` | 13.2, 13.5, dan 13.7 bila relevan |
| `diagnostic_ma` | 13.2, 13.6, dan 13.7 bila relevan |
| `nma` | 13.2, 13.8, dan 13.9 bila relevan |

### 13.2 Paragraf pembuka umum

```text
Statistical analyses were performed using R version [R VERSION]. The analysis used the [PACKAGE NAMES] packages, with exact package versions recorded in the analysis manifest. All effect estimates are presented with 95% confidence intervals. A random-effects model was used as the primary model because clinical and methodological heterogeneity across studies was anticipated. A common-effect model was calculated only as a supplementary analysis where available.
```

Tambahkan nama package hanya bila digunakan untuk outcome tersebut, misalnya `meta`, `metafor`, `netmeta`, `mada`, atau `estmeansd`. Jangan menyebut package hanya karena terpasang di komputer.

### 13.3 Pairwise meta-analysis untuk outcome binary

Gunakan paragraf inti berikut:

```text
For binary outcomes, treatment effects were summarized as [odds ratios/risk ratios/hazard ratios] with 95% confidence intervals. Ratio measures reported by the primary studies were analyzed on the logarithmic scale and back-transformed to the natural scale for presentation. When a standard error was not reported, it was derived from the corresponding confidence interval and confidence level. Within each study, only one estimate was included in the primary model. Adjusted estimates were prioritized over reported crude estimates, followed by estimates derived from arm-level event data.
```

Pilih **satu** kalimat metode raw binary yang sesuai dengan `METHOD_DECISIONS`:

**Data binary non-sparse:**

```text
For non-sparse arm-level binary data, study effects were pooled using a random-effects Mantel-Haenszel model. Between-study heterogeneity was estimated using restricted maximum likelihood, and Hartung-Knapp confidence intervals with a safeguard were used where supported by the fitted model.
```

**Sparse odds ratio:**

```text
For sparse arm-level odds-ratio data, the primary analysis used a random-effects generalized linear mixed model without a fixed continuity correction. A random-effects Mantel-Haenszel analysis was performed as a sensitivity analysis when estimable.
```

**Sparse risk ratio:**

```text
For sparse arm-level risk-ratio data, the primary analysis used a random-effects Mantel-Haenszel model without a fixed continuity correction when estimable. A treatment-arm continuity correction was evaluated as a sensitivity analysis.
```

Tambahkan kalimat ini bila terdapat studi dengan zero event pada kedua arm:

```text
Studies with zero events in both comparison arms did not contribute information to relative odds-ratio or risk-ratio estimation and were excluded from the corresponding pooled relative-effect analysis. These exclusions were recorded explicitly in the analysis log.
```

Tambahkan paragraf berikut bila raw-derived, crude, dan adjusted estimates digabung:

```text
Because compatible effect estimates from different sources were combined across studies, sensitivity analyses were conducted according to estimate source and study design when sufficient studies were available. The implications of combining adjusted, crude, and arm-derived estimates were considered when interpreting the pooled result.
```

Untuk HR, jelaskan bahwa engine hanya menerima reported HR:

```text
Hazard ratios were included only when reported by the original study with a standard error or confidence interval. Hazard ratios were not estimated from event counts alone.
```

### 13.4 Pairwise meta-analysis untuk outcome continuous

```text
For continuous outcomes, treatment effects were summarized as mean differences when studies used a common measurement scale, or as standardized mean differences using Hedges' g when measurement scales differed. Effect estimates were pooled using a generic inverse-variance random-effects model. Between-study heterogeneity was estimated using restricted maximum likelihood, and Hartung-Knapp confidence intervals with a safeguard were used for the primary pooled estimate.
```

Tambahkan bagian berikut hanya bila median, IQR, atau range dikonversi:

```text
For studies reporting medians with interquartile ranges and/or ranges, means and standard deviations were estimated using an appropriate Quantile Estimation method implemented in the estmeansd package. Because these values were estimated rather than directly reported, the conversion was flagged and a sensitivity analysis excluding converted studies was performed when sufficient data were available.
```

Jangan menyebut metode Wan atau Luo kecuali metode tersebut benar-benar dipilih dan tercatat oleh engine. V2.3 menggunakan Quantile Estimation melalui `estmeansd` untuk konversi ini.

### 13.5 Single-arm meta-analysis

Untuk meta-analysis proporsi:

```text
Single-arm proportions were pooled using a random-effects logistic generalized linear mixed model without a continuity correction. Estimates were modeled on the logit scale and back-transformed to proportions for presentation. Studies with zero events or events in all participants were retained because the model supports boundary proportions. Between-study heterogeneity was estimated using maximum likelihood.
```

Untuk meta-analysis mean:

```text
Single-arm means were pooled using a random-effects raw-mean model. Between-study heterogeneity was estimated using restricted maximum likelihood, with Hartung-Knapp confidence intervals and the engine's safeguard applied to the pooled estimate.
```

### 13.6 Diagnostic meta-analysis

```text
Diagnostic test accuracy was analyzed from study-level 2 x 2 tables containing true-positive, false-positive, false-negative, and true-negative counts. A bivariate random-effects Reitsma model was fitted using the mada package to jointly synthesize sensitivity and specificity while accounting for their correlation. Study-specific sensitivity and specificity confidence intervals were calculated using exact binomial methods. Results were presented using paired sensitivity and specificity forest plots and a summary receiver operating characteristic curve.
```

Tambahkan kalimat berikut hanya bila output terkait dibuat:

```text
Leave-one-out analysis was performed when at least three eligible studies were available. Potential small-study effects were assessed using Deeks' funnel-plot asymmetry regression when at least ten studies were available, and the exact p value was reported.
```

Jangan menyatakan bahwa engine menghitung pooled likelihood ratios, diagnostic odds ratio, AUC, threshold-effect test, subgroup diagnostic, atau meta-regression diagnostic bila tabel tersebut tidak tersedia pada hasil run.

### 13.7 Analisis tambahan untuk pairwise dan proportion meta-analysis

Gunakan hanya kalimat yang sesuai dengan output:

```text
Statistical heterogeneity was described using I-squared and tau-squared. A 95% prediction interval was calculated when at least five studies were available and the model could be estimated.
```

```text
Subgroup analysis was performed only when at least two subgroups were available and each subgroup contained at least two studies. Differences between subgroups were evaluated using the between-subgroup heterogeneity test.
```

```text
Random-effects meta-regression was performed for [MODERATOR] using restricted maximum likelihood only when at least ten studies were available per model parameter. For numerical moderators, results were displayed in a bubble plot with bubble size reflecting study precision. Meta-regression findings were interpreted as observational and exploratory.
```

```text
Leave-one-out analysis was performed when at least three studies were available by refitting the primary model after sequentially omitting each study.
```

```text
Potential small-study effects were assessed only when at least ten studies were available. Funnel-plot asymmetry was evaluated using Egger's regression test, and the exact p value was reported.
```

V2.3 tidak otomatis menjalankan Begg's test atau trim-and-fill. Jangan memasukkan kedua metode tersebut ke Methods kecuali dilakukan sebagai analisis tambahan di luar engine dan dilaporkan secara terpisah.

### 13.8 Network meta-analysis

Gunakan paragraf inti berikut:

```text
A frequentist random-effects network meta-analysis was conducted to synthesize direct and indirect evidence across treatments. The reference treatment, [REFERENCE TREATMENT], was specified a priori in the analysis workbook. Relative effects were oriented consistently against the reference treatment, analyzed on the logarithmic scale for ratio measures, and back-transformed to the natural scale for presentation. Multi-arm studies were entered using one row per study arm, and the statistical model accounted for the correlation induced by shared treatment arms. Participants were counted from unique study arms rather than summed across pairwise contrasts.
```

Untuk NMA binary yang seluruhnya berasal dari arm-level event/sample:

```text
For arm-level binary data, a random-effects network meta-analysis was fitted using penalized logistic regression as implemented by netmetabin in the netmeta package.
```

Untuk NMA continuous atau NMA dengan reported effects:

```text
For continuous outcomes or networks containing compatible reported effects, a random-effects inverse-variance network meta-analysis was fitted using the netmeta package. Between-study heterogeneity was estimated using restricted maximum likelihood, with adjustment for the correlation of comparisons from multi-arm studies.
```

Tambahkan kalimat ini bila sumber estimate dicampur:

```text
Adjusted, reported crude, and arm-derived estimates from different studies were combined only when their estimands were considered compatible. Within each study, a single estimate was selected using the prespecified priority of adjusted, reported crude, and arm-derived estimates. Sensitivity analyses by estimate source and study design were performed when the resulting networks remained connected and contained sufficient evidence.
```

### 13.9 Pemeriksaan dan keluaran tambahan NMA

Gunakan hanya bagian yang benar-benar tersedia pada output:

```text
Network geometry was examined using a treatment network plot. Heterogeneity was assessed within the random-effects network model. Global inconsistency was evaluated using a design-by-treatment decomposition, and local inconsistency was examined using node-splitting where closed loops and sufficient evidence were available. Relative effects were summarized in a league table.
```

Untuk ranking, pastikan `outcome_direction` bukan `neutral`:

```text
Treatment ranking was summarized using P-scores, with the direction of benefit defined a priori as [LOWER/HIGHER] values indicating a better outcome. Rankings were interpreted alongside effect estimates, uncertainty, network connectivity, and inconsistency assessments.
```

Untuk transitivity:

```text
The distribution of available study-level effect modifiers was summarized across treatment comparisons as a descriptive assessment of the transitivity assumption. This assessment was not interpreted as a statistical proof of transitivity.
```

### 13.10 Pelaporan software dan keputusan metode

Contoh kalimat penutup:

```text
All data validation messages, exclusions, fallback methods, sensitivity analyses, and model decisions were recorded by the analysis engine. The exact R version, package versions, input-file hash, schema version, and configuration for each analysis were stored in the run manifest to support reproducibility.
```

Sebelum mengirim manuskrip, periksa kembali:

- `manifest.json` untuk versi R dan package;
- `METHOD_DECISIONS` untuk model utama, fallback, dan sensitivity analysis;
- `WARNINGS` dan `ERROR_LOG` untuk konversi atau eksklusi data;
- `POOLED_RESULTS`, `HETEROGENEITY`, dan tabel khusus analisis untuk angka yang dilaporkan;
- kesesuaian `outcome_direction`, reference treatment, dan arah interpretasi effect;
- bahwa ambang jumlah studi benar-benar terpenuhi untuk prediction interval, subgroup, meta-regression, leave-one-out, funnel plot, dan Deeks test.

Nilai alpha atau istilah "statistically significant" tidak ditambahkan otomatis oleh template. Jika digunakan, tuliskan hanya sesuai protokol penelitian dan tetap laporkan effect estimate, 95% CI, serta exact p value.

## 14. Plot

Setiap plot dibuat sebagai PNG 300 dpi dan PDF vector. Forest, subgroup, leave-one-out, dan bubble plot memakai bahasa visual engine V1: latar putih, garis dan marker utama hitam, pooled diamond hitam, serta overall reference merah pada leave-one-out. Model statistik dan data yang diplot tetap berasal dari pipeline V2.3.

Lebar dan tinggi dihitung dari jumlah studi, jumlah subgroup, panjang `study_label`, panjang nama treatment, dan jumlah subkolom. Nama treatment ditampilkan penuh tanpa singkatan. Ruang grup arm dan ukuran font dihitung secara dinamis agar header, nilai effect, confidence interval, serta bobot tidak saling bertabrakan.

Forest pairwise memakai kolom klinis ringkas dan tidak lagi menampilkan TE/log effect atau SE yang redundant:

- raw binary OR atau RR: `Study`, lalu grup bernama treatment aktual dengan subkolom `Event` dan `Total` untuk masing-masing arm;
- raw continuous MD atau SMD: `Study`, lalu grup bernama treatment aktual dengan subkolom `Mean`, `SD`, dan `Total` untuk masing-masing arm;
- reported-only atau mixed: hanya subkolom `Total` untuk treatment dan reference. Event, mean, SD, log effect, dan SE tidak ditampilkan;
- kolom kanan: effect pada skala natural, 95% CI, dan random-effects weight.

Pada forest reported-only atau mixed, studi dibagi menjadi blok `Adjusted`, `Crude`, dan `Raw-derived` sesuai sumber estimate yang benar-benar terpilih untuk model utama. Setiap blok yang dapat dipool menampilkan subtotal dan heterogeneity. Pooled overall tetap ditampilkan, disertai test for source differences bila terdapat lebih dari satu source type. Jika sample reported tidak tersedia, sel studi ditampilkan sebagai `NR`. Total peserta tidak dibuat dari penjumlahan parsial.

Pada subgroup forest, subgroup klinis dari kolom `subgroup_` tetap menjadi blok utama. Source estimate ditampilkan sebagai kolom agar tidak membentuk nested subgroup yang sulit dibaca.

Subgroup forest menampilkan studi individual, pooled result setiap subgroup, heterogeneity per subgroup, pooled result overall, dan test for subgroup differences. Leave-one-out menampilkan estimate setelah setiap studi dikeluarkan beserta 95% CI dan garis overall. Bubble plot menampilkan ukuran bubble berdasarkan inverse variance, fitted line, confidence band, nilai R2, dan p value moderator.

Output meliputi sesuai kelayakan data:

- forest binary dengan event/sample per arm;
- forest continuous dengan mean, SD, dan sample;
- forest proportion dengan event/sample dan persentase;
- forest NMA versus reference dengan jumlah studi dan peserta unik;
- subgroup forest dengan test for subgroup differences;
- leave-one-out plot;
- bubble plot untuk moderator numerik;
- network graph, league table, ranking, inconsistency, dan node splitting;
- paired sensitivity/specificity forest, SROC, dan Deeks plot.

Header arm dan label `Favours` memakai nama treatment canonical dari workbook, bukan teks `Intervention` atau `Control` yang ditulis tetap. Untuk `lower_better`, sisi effect lebih kecil diberi `Favours <non-reference>` dan sisi sebaliknya `Favours <reference>`. Untuk `higher_better`, kedua arah tersebut dibalik. Untuk `neutral`, label menjadi `Lower effect` dan `Higher effect` tanpa klaim treatment lebih baik.

## 15. Struktur hasil

```text
auctus_results/<nama_file>_<timestamp>/
|-- 00_validation/
|   |-- validated_input.xlsx
|   `-- error_log.csv
|-- 01_results/
|   |-- results.xlsx
|   |-- analysis_objects.rds
|   `-- manifest.json
|-- 02_plots/<slug_outcome>_<8_hash>/
|   |-- *.png
|   `-- *.pdf
|-- 03_report/
|   |-- report.html
|   `-- report.md
`-- 04_logs/
    |-- run_log.csv
    `-- id_map.csv
```

`hasil$analyses` dinamai berdasarkan `outcome_name`:

```r
names(hasil$analyses)
hasil$analyses[["Mortality at 30 days"]]
```

`report.html` bersifat self-contained. Gambar PNG ditanam langsung ke dalam file HTML agar dapat dibuka melalui `file://` pada Safari, Chrome, atau browser lain tanpa bergantung pada izin membaca folder lokal. Karena itu, ukuran file HTML dapat lebih besar daripada versi sebelumnya.

`report.md` mengikuti struktur laporan engine lama dalam format Markdown yang lebih mudah direvisi. Isinya mencakup konfigurasi analisis, overall result, sensitivity analysis, subgroup klinis, source subgroup, meta-regression, leave-one-out, publication bias, hasil khusus NMA, keputusan metode, seluruh plot PNG, dan diagnostics. Link plot bersifat relatif sehingga folder hasil dapat dipindahkan sebagai satu paket.

`results.xlsx` mempunyai sheet `SOURCE_SUBGROUP` untuk ringkasan adjusted, crude, dan raw-derived pada outcome reported atau mixed. Mode forest `raw_atomic` atau `reported_total_only` dicatat pada `METHOD_DECISIONS` dan `manifest.json`.

Jika `output_dir` tidak diisi, satu kali pemanggilan `run_auctus_meta()` membuat satu folder run dengan timestamp:

```text
auctus_results/<nama_workbook>_YYYYMMDD_HHMMSS/
```

Semua analisis dalam workbook masuk ke folder run yang sama. Setiap `outcome_name` mempunyai subfolder sendiri di `02_plots`, menggunakan slug outcome dan delapan karakter hash, tanpa timestamp tambahan. Jadi timestamp membedakan run, sedangkan slug dan hash membedakan analisis.

Jika `output_dir` diisi manual, lokasi tersebut dipakai persis dan timestamp tidak ditambahkan. Ini sebabnya generator contoh memakai folder tetap `dummy_pairwise_OR_MD_results`.

`results.xlsx`, report HTML, dan report Markdown tidak menampilkan ID internal.

## 16. Perbedaan dengan engine lama

| Aspek | Engine lama | Engine V2.3 |
|---|---|---|
| Menjalankan analisis | Fungsi lama dengan struktur sheet terpisah | Satu entry point `run_auctus_meta()` untuk MA dan NMA |
| Instalasi package | Otomatis saat fungsi analisis dijalankan | Tetap otomatis saat `run_auctus_meta()` dijalankan, tetapi `Source` saja tidak mengubah session |
| Kunci data | ID dan pengulangan data lebih banyak | User hanya menyamakan `outcome_name` dan `study_label`; ID teknis dibuat engine |
| Studi multi-arm | User dapat perlu membuat seluruh pairwise row | User memasukkan satu baris per arm |
| Menonaktifkan baris | Menggunakan flag `include` | Tidak ada flag; hapus seluruh baris dari salinan workbook |
| Contoh template | Dapat bercampur dengan area input | Contoh dipisahkan pada `CONTOH_PENGISIAN` read-only |
| Validasi | Pesan console lebih umum | Workbook koreksi, warna sel, hyperlink, kode error, pesan, dan saran |
| Forest pairwise | Kolom log effect dan SE dapat redundant | Kolom arm atomik, treatment dinamis, total-only untuk reported effect, dan label `Favours` dinamis |
| Mixed estimate | Tampilan sumber terbatas | Blok Adjusted, Crude, dan Raw-derived serta `SOURCE_SUBGROUP` |
| Report | Markdown bergaya engine lama | Markdown dipertahankan dan ditambah HTML self-contained |
| Reproducibility | Informasi run lebih terbatas | Manifest, input hash, package version, method decisions, ID map, dan run log |

Perubahan hasil numerik dapat terjadi bila V2.3 memperbaiki orientasi contrast, rare-event handling, multi-arm correlation, pemilihan satu estimate per studi, atau metode pooling. Periksa `METHOD_DECISIONS`, sensitivity analysis, dan regression notes sebelum membandingkan angka dengan output lama.

## 17. Checklist sebelum run

- [ ] Setiap `outcome_name` pada `analyses` unik.
- [ ] Setiap `study_label` pada `study_metadata` unik secara global.
- [ ] Sheet data memakai dropdown outcome dan studi.
- [ ] Outcome atau follow-up berbeda memakai nama outcome yang berbeda.
- [ ] Reference treatment sama ejaannya dengan data treatment.
- [ ] Arah outcome telah dipilih dan tidak ditebak engine.
- [ ] Event memenuhi `0 <= event <= sample`.
- [ ] OR, RR, dan HR pada `effect_data` dimasukkan pada skala natural.
- [ ] Reported effect memiliki CI lengkap atau SE.
- [ ] Reported contrast multi-arm lengkap.
- [ ] Moderator dan subgroup benar-benar merupakan atribut studi global.
- [ ] Semua sel merah pada workbook koreksi telah diperbaiki.
- [ ] Tidak ada baris setengah terisi. Hapus seluruh baris bila data memang ingin dikecualikan.
