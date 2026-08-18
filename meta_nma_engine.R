# =============================================================================
# AUCTUS META-ANALYSIS & NETWORK META-ANALYSIS ENGINE V2.3
# =============================================================================
# Public workflow:
#   source("meta_nma_engine.R")
#   hasil <- run_auctus_meta()
#
# Design guarantees:
#   * source() only defines functions and constants.
#   * Package installation only occurs when run_auctus_meta() is called.
#   * No setwd(), GlobalEnv writes, or broad dev.off().
#   * Every input value keeps its Excel sheet and row provenance.
#   * Validation happens before modelling.
#   * All method choices, exclusions, warnings, and package versions are logged.
# =============================================================================

local({

  .AUCTUS_ENGINE_VERSION <- "2.3.0"
.AUCTUS_SCHEMA_VERSION <- "2.3"

.auctus_packages <- c(
  readxl = "1.4.0",
  openxlsx = "4.2.5",
  meta = "8.0.0",
  metafor = "4.0.0",
  netmeta = "3.0.0",
  mada = "0.5.10",
  ggplot2 = "3.5.0",
  gridExtra = "2.3",
  jsonlite = "1.8.0",
  digest = "0.6.30",
  zip = "2.3.0",
  estmeansd = "1.0.0"
)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

.trim_chr <- function(x) {
  y <- trimws(as.character(x))
  y[is.na(x) | tolower(y) %in% c("", "na", "n/a", "null")] <- NA_character_
  y
}

.clean_names <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

.safe_name <- function(x) {
  y <- gsub("[^A-Za-z0-9_-]+", "_", as.character(x))
  y <- gsub("_+", "_", y)
  y <- gsub("^_|_$", "", y)
  ifelse(nzchar(y), y, "analysis")
}

.normalize_label_key <- function(x) {
  y <- .trim_chr(x)
  y <- enc2utf8(y)
  y <- gsub("[[:space:]]+", " ", y)
  tolower(y)
}

.internal_id <- function(prefix, label) {
  .require_namespace("digest")
  key <- .normalize_label_key(label)
  vapply(key, function(value) {
    if (is.na(value)) return(NA_character_)
    paste0(prefix, "_", substr(digest::digest(value, algo = "xxhash64", serialize = FALSE), 1L, 12L))
  }, character(1))
}

.analysis_output_key <- function(analysis) {
  slug <- substr(.safe_name(.first_nonmissing(analysis$outcome_name, "analysis")), 1L, 48L)
  hash <- sub("^a_", "", .first_nonmissing(analysis$analysis_id, .internal_id("a", slug)))
  paste0(slug, "_", substr(hash, 1L, 8L))
}

.analysis_title <- function(analysis) {
  outcome <- .first_nonmissing(
    c(analysis$outcome_name, analysis$analysis_id),
    "Analysis"
  )
  timepoint <- .first_nonmissing(analysis$timepoint, NA_character_)
  if (is.na(timepoint)) outcome else paste0(outcome, " (", timepoint, ")")
}

.absolute_path <- function(path) {
  path <- path.expand(path)
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", path)) path <- file.path(getwd(), path)
  normalizePath(path, mustWork = FALSE)
}

.as_num <- function(x) suppressWarnings(as.numeric(x))

.is_yes <- function(x, default = TRUE) {
  y <- tolower(trimws(as.character(x)))
  out <- rep(default, length(y))
  out[y %in% c("false", "no", "n", "0", "tidak")] <- FALSE
  out[y %in% c("true", "yes", "y", "1", "ya")] <- TRUE
  out[is.na(x) | !nzchar(y)] <- default
  out
}

.first_nonmissing <- function(x, default = NA_character_) {
  y <- .trim_chr(x)
  y <- y[!is.na(y)]
  if (length(y)) y[[1L]] else default
}

.empty_df <- function(columns) {
  out <- setNames(vector("list", length(columns)), columns)
  for (nm in columns) out[[nm]] <- logical()
  as.data.frame(out, stringsAsFactors = FALSE)
}

.ensure_columns <- function(dat, columns, value = NA) {
  for (nm in setdiff(columns, names(dat))) dat[[nm]] <- rep(value, nrow(dat))
  dat
}

.require_namespace <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf(
        "Package '%s' belum tersedia. Jalankan install.packages('%s'), lalu ulangi analisis.",
        pkg, pkg
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

check_auctus_dependencies <- function(stop_on_missing = FALSE) {
  rows <- lapply(names(.auctus_packages), function(pkg) {
    installed <- requireNamespace(pkg, quietly = TRUE)
    version <- if (installed) as.character(utils::packageVersion(pkg)) else NA_character_
    minimum <- unname(.auctus_packages[[pkg]])
    version_ok <- installed && utils::compareVersion(version, minimum) >= 0L
    data.frame(
      package = pkg,
      installed = installed,
      version = version,
      minimum = minimum,
      status = if (version_ok) "OK" else if (installed) "VERSION_TOO_OLD" else "MISSING",
      install_command = sprintf("install.packages('%s')", pkg),
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  bad <- result$status != "OK"
  if (stop_on_missing && any(bad)) {
    commands <- unique(result$install_command[bad])
    stop(
      paste0(
        "Dependensi Auctus belum lengkap:\n",
        paste(sprintf("  - %s", commands), collapse = "\n"),
        "\nSetelah instalasi selesai, jalankan kembali run_auctus_meta()."
      ),
      call. = FALSE
    )
  }
  class(result) <- c("auctus_dependency_check", class(result))
  result
}

print.auctus_dependency_check <- function(x, ...) {
  print.data.frame(x[, c("package", "installed", "version", "minimum", "status")],
                   row.names = FALSE)
  if (any(x$status != "OK")) {
    cat("\nPerintah instalasi yang diperlukan:\n")
    cat(paste0("  ", unique(x$install_command[x$status != "OK"])), sep = "\n")
    cat("\n")
  }
  invisible(x)
}

.install_auctus_dependencies <- function(packages) {
  packages <- unique(as.character(packages))
  packages <- packages[!is.na(packages) & nzchar(packages)]
  if (!length(packages)) return(invisible(character()))

  repos <- getOption("repos")
  cran_repo <- if (!is.null(names(repos)) && "CRAN" %in% names(repos)) {
    unname(repos[["CRAN"]])
  } else {
    NA_character_
  }
  if (!length(cran_repo) || is.na(cran_repo) || !nzchar(cran_repo) ||
      identical(cran_repo, "@CRAN@")) {
    other_repos <- if (!is.null(names(repos))) repos[names(repos) != "CRAN"] else NULL
    repos <- c(CRAN = "https://cloud.r-project.org", other_repos)
  }

  message(
    "Dependency Auctus belum lengkap. Instalasi otomatis dimulai untuk: ",
    paste(packages, collapse = ", "), "."
  )
  tryCatch(
    utils::install.packages(packages, repos = repos),
    error = function(e) {
      stop(
        paste0(
          "Instalasi otomatis dependency Auctus gagal: ", conditionMessage(e), "\n",
          "Periksa koneksi internet, izin menulis R library, dan konfigurasi CRAN, ",
          "lalu jalankan kembali run_auctus_meta()."
        ),
        call. = FALSE
      )
    }
  )
  invisible(packages)
}

.ensure_auctus_dependencies <- function(
    checker = check_auctus_dependencies,
    installer = .install_auctus_dependencies) {
  before <- checker(stop_on_missing = FALSE)
  needs_install <- before$status != "OK"
  if (!any(needs_install)) return(invisible(before))

  packages <- before$package[needs_install]
  installer(packages)

  after <- checker(stop_on_missing = FALSE)
  unresolved <- after$status != "OK"
  if (any(unresolved)) {
    details <- sprintf(
      "  - %s: %s, versi minimum %s",
      after$package[unresolved], after$status[unresolved], after$minimum[unresolved]
    )
    stop(
      paste0(
        "Instalasi otomatis selesai, tetapi dependency berikut belum siap:\n",
        paste(details, collapse = "\n"), "\n",
        "Periksa pesan instalasi di Console, koneksi internet, dan izin R library, ",
        "lalu jalankan kembali run_auctus_meta()."
      ),
      call. = FALSE
    )
  }

  previously_outdated <- before$package[before$status == "VERSION_TOO_OLD"]
  loaded_outdated <- previously_outdated[vapply(
    previously_outdated,
    function(pkg) {
      pkg %in% loadedNamespaces() &&
        utils::compareVersion(
          as.character(getNamespaceVersion(pkg)),
          unname(.auctus_packages[[pkg]])
        ) < 0L
    },
    logical(1)
  )]
  if (length(loaded_outdated)) {
    stop(
      paste0(
        "Package berhasil diperbarui, tetapi versi lama masih aktif di session R: ",
        paste(loaded_outdated, collapse = ", "), ".\n",
        "Restart R, Source kembali meta_nma_engine.R, lalu jalankan run_auctus_meta()."
      ),
      call. = FALSE
    )
  }

  message("Seluruh dependency Auctus sudah siap. Analisis dilanjutkan.")
  invisible(after)
}

.diagnostic_columns <- c(
  "severity", "error_code", "sheet", "excel_row", "column", "outcome_name",
  "study_label", "analysis_id", "study_id", "value", "message", "suggestion",
  "example", "cell_link"
)

.new_diagnostics <- function() .empty_df(.diagnostic_columns)

.add_diag <- function(diag, severity, error_code, sheet = NA_character_,
                      excel_row = NA_integer_, column = NA_character_,
                      outcome_name = NA_character_, study_label = NA_character_,
                      analysis_id = NA_character_, study_id = NA_character_,
                      value = NA_character_, message, suggestion = NA_character_,
                      example = NA_character_) {
  excel_row <- suppressWarnings(as.integer(excel_row))
  cell_link <- if (!is.na(sheet) && !is.na(excel_row) && !is.na(column)) {
    paste0(sheet, "!", column, excel_row)
  } else {
    NA_character_
  }
  row <- data.frame(
    severity = severity,
    error_code = error_code,
    sheet = sheet,
    excel_row = excel_row,
    column = column,
    outcome_name = outcome_name,
    study_label = study_label,
    analysis_id = analysis_id,
    study_id = study_id,
    value = as.character(value),
    message = message,
    suggestion = suggestion,
    example = example,
    cell_link = cell_link,
    stringsAsFactors = FALSE
  )
  rbind(diag, row)
}

.bind_diag <- function(...) {
  xs <- list(...)
  xs <- xs[vapply(xs, is.data.frame, logical(1))]
  if (!length(xs)) return(.new_diagnostics())
  columns <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    for (nm in setdiff(columns, names(x))) x[[nm]] <- rep(NA, nrow(x))
    x[, columns, drop = FALSE]
  })
  out <- do.call(rbind, xs)
  rownames(out) <- NULL
  out
}

.sheet_specs <- list(
  analyses = c(
    "outcome_name", "analysis_type", "timepoint",
    "outcome_type", "effect_measure", "reference_treatment",
    "outcome_direction", "unit", "notes"
  ),
  study_metadata = c("study_label", "year", "study_design"),
  arm_data = c(
    "outcome_name", "study_label", "treatment", "event", "sample",
    "mean", "sd", "median", "q1", "q3", "min", "max", "notes"
  ),
  effect_data = c(
    "outcome_name", "study_label", "treat1", "treat2", "effect", "ci_low",
    "ci_high", "se", "ci_level", "estimate_type", "adjustment_variables",
    "sample1", "sample2", "notes"
  ),
  diagnostic_data = c(
    "outcome_name", "study_label", "tp", "fp", "fn", "tn", "threshold",
    "notes"
  )
)

.read_sheet_with_provenance <- function(file_path, sheet, expected = character()) {
  dat <- readxl::read_excel(file_path, sheet = sheet, col_types = "text")
  names(dat) <- .clean_names(names(dat))
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  dat <- .ensure_columns(dat, expected)
  dat$source_sheet <- rep(sheet, nrow(dat))
  dat$source_row <- seq_len(nrow(dat)) + 1L
  non_provenance <- setdiff(names(dat), c("source_sheet", "source_row"))
  if (length(non_provenance)) {
    mat <- as.matrix(dat[non_provenance])
    keep <- rowSums(!is.na(mat) & trimws(mat) != "") > 0L
    dat <- dat[keep, , drop = FALSE]
  }
  rownames(dat) <- NULL
  dat
}

.read_auctus_workbook <- function(file_path) {
  .require_namespace("readxl")
  if (!file.exists(file_path)) stop("File tidak ditemukan: ", file_path, call. = FALSE)
  ext <- tolower(tools::file_ext(file_path))
  if (!ext %in% c("xlsx", "xls", "xlsm")) {
    stop("File harus berformat .xlsx, .xls, atau .xlsm.", call. = FALSE)
  }
  sheets <- readxl::excel_sheets(file_path)
  sheet_key <- setNames(sheets, .clean_names(sheets))
  required <- c("analyses", "study_metadata", "arm_data", "effect_data", "diagnostic_data")
  missing <- setdiff(required, names(sheet_key))
  if (length(missing)) {
    structure(
      list(file_path = normalizePath(file_path), sheets = list(), missing_sheets = missing,
           available_sheets = sheets, schema_version = NA_character_),
      class = "auctus_workbook"
    )
  } else {
    data <- lapply(required, function(key) {
      .read_sheet_with_provenance(file_path, sheet_key[[key]], .sheet_specs[[key]])
    })
    names(data) <- required
    structure(
      c(list(file_path = normalizePath(file_path), missing_sheets = character(),
             available_sheets = sheets, schema_version = .AUCTUS_SCHEMA_VERSION), data),
      class = "auctus_workbook"
    )
  }
}

.augment_internal_ids <- function(wb) {
  if (length(wb$missing_sheets)) return(wb)

  analyses <- wb$analyses
  analyses$outcome_key <- .normalize_label_key(analyses$outcome_name)
  analyses$analysis_id <- .internal_id("a", analyses$outcome_name)

  metadata <- wb$study_metadata
  metadata$study_key <- .normalize_label_key(metadata$study_label)
  metadata$study_id <- .internal_id("s", metadata$study_label)

  for (sheet in c("arm_data", "effect_data", "diagnostic_data")) {
    dat <- wb[[sheet]]
    dat$outcome_key <- .normalize_label_key(dat$outcome_name)
    dat$study_key <- .normalize_label_key(dat$study_label)
    ai <- match(dat$outcome_key, analyses$outcome_key)
    si <- match(dat$study_key, metadata$study_key)
    dat$analysis_id <- analyses$analysis_id[ai]
    dat$study_id <- metadata$study_id[si]
    dat$canonical_outcome_name <- analyses$outcome_name[ai]
    dat$canonical_study_label <- metadata$study_label[si]
    wb[[sheet]] <- dat
  }

  wb$analyses <- analyses
  wb$study_metadata <- metadata
  wb
}

.enrich_diagnostic_labels <- function(diag, analyses, metadata) {
  if (!nrow(diag)) return(diag)
  ai <- match(diag$analysis_id, analyses$analysis_id)
  si <- match(diag$study_id, metadata$study_id)
  missing_outcome <- is.na(.trim_chr(diag$outcome_name)) & !is.na(ai)
  missing_study <- is.na(.trim_chr(diag$study_label)) & !is.na(si)
  diag$outcome_name[missing_outcome] <- analyses$outcome_name[ai[missing_outcome]]
  diag$study_label[missing_study] <- metadata$study_label[si[missing_study]]
  diag
}

.user_diagnostics <- function(diag) {
  keep <- c(
    "severity", "error_code", "sheet", "excel_row", "column", "outcome_name",
    "study_label", "value", "message", "suggestion", "example", "cell_link"
  )
  diag[, intersect(keep, names(diag)), drop = FALSE]
}

.column_letter <- function(dat, column) {
  idx <- match(column, names(dat))
  if (is.na(idx)) return(NA_character_)
  if (requireNamespace("openxlsx", quietly = TRUE)) openxlsx::int2col(idx) else {
    letters <- character()
    while (idx > 0L) {
      idx <- idx - 1L
      letters <- c(LETTERS[(idx %% 26L) + 1L], letters)
      idx <- idx %/% 26L
    }
    paste0(letters, collapse = "")
  }
}

.diag_for_cell <- function(diag, severity, code, dat, i, column, message,
                           suggestion, example = NA_character_) {
  .add_diag(
    diag, severity, code,
    sheet = dat$source_sheet[[i]],
    excel_row = dat$source_row[[i]],
    column = .column_letter(dat, column),
    outcome_name = if ("outcome_name" %in% names(dat)) dat$outcome_name[[i]] else NA_character_,
    study_label = if ("study_label" %in% names(dat)) dat$study_label[[i]] else NA_character_,
    analysis_id = if ("analysis_id" %in% names(dat)) dat$analysis_id[[i]] else NA_character_,
    study_id = if ("study_id" %in% names(dat)) dat$study_id[[i]] else NA_character_,
    value = if (column %in% names(dat)) dat[[column]][[i]] else NA_character_,
    message = message,
    suggestion = suggestion,
    example = example
  )
}

.validate_required <- function(diag, dat, columns, sheet_label) {
  for (col in columns) {
    if (!col %in% names(dat)) {
      diag <- .add_diag(
        diag, "ERROR", "E001_MISSING_COLUMN", sheet_label,
        column = col,
        message = sprintf("Kolom wajib '%s' tidak ditemukan.", col),
        suggestion = "Gunakan template V2.3 atau tambahkan kolom dengan nama yang persis sama.",
        example = col
      )
      next
    }
    vals <- .trim_chr(dat[[col]])
    bad <- which(is.na(vals))
    for (i in bad) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E002_MISSING_REQUIRED", dat, i, col,
        sprintf("Nilai '%s' wajib diisi.", col),
        "Isi sel ini sesuai PETUNJUK dan contoh pada template.",
        switch(col,
          outcome_name = "Mortality at 30 days",
          study_label = "Smith 2024",
          "Lihat contoh pada sheet PETUNJUK"
        )
      )
    }
  }
  diag
}

.validate_enum <- function(diag, dat, column, allowed, required = TRUE) {
  if (!column %in% names(dat)) return(diag)
  vals <- tolower(.trim_chr(dat[[column]]))
  bad <- which(!is.na(vals) & !vals %in% allowed)
  if (required) bad <- union(bad, which(is.na(vals)))
  for (i in bad) {
    diag <- .diag_for_cell(
      diag, "ERROR", "E101_INVALID_VALUE", dat, i, column,
      sprintf("Nilai '%s' tidak dikenali untuk kolom '%s'.", dat[[column]][[i]], column),
      sprintf("Pilih salah satu nilai: %s.", paste(allowed, collapse = ", ")),
      allowed[[1L]]
    )
  }
  diag
}

.validate_numeric_cell <- function(diag, dat, i, column, minimum = -Inf,
                                   strictly_positive = FALSE, integer = FALSE,
                                   required = FALSE) {
  raw <- .trim_chr(dat[[column]])[[i]]
  if (is.na(raw)) {
    if (required) {
      return(.diag_for_cell(
        diag, "ERROR", "E002_MISSING_REQUIRED", dat, i, column,
        sprintf("Nilai numerik '%s' wajib diisi.", column),
        "Isi angka tanpa simbol atau teks tambahan.", "10"
      ))
    }
    return(diag)
  }
  value <- suppressWarnings(as.numeric(raw))
  bad <- is.na(value) || value < minimum || (strictly_positive && value <= 0) ||
    (integer && !is.na(value) && abs(value - round(value)) > 1e-8)
  if (bad) {
    diag <- .diag_for_cell(
      diag, "ERROR", "E102_INVALID_NUMBER", dat, i, column,
      sprintf("Nilai '%s' pada '%s' bukan angka yang valid.", raw, column),
      if (strictly_positive) "Isi angka lebih besar dari 0." else
        sprintf("Isi angka minimal %s.", minimum),
      if (strictly_positive) "100" else as.character(max(0, minimum))
    )
  }
  diag
}

.network_connected <- function(treatments, edges, reference) {
  treatments <- unique(.trim_chr(treatments))
  treatments <- treatments[!is.na(treatments)]
  if (!length(treatments) || is.na(reference) || !reference %in% treatments) return(FALSE)
  reached <- reference
  repeat {
    old <- reached
    for (i in seq_len(nrow(edges))) {
      a <- edges$treat1[[i]]
      b <- edges$treat2[[i]]
      if (a %in% reached || b %in% reached) reached <- unique(c(reached, a, b))
    }
    if (setequal(old, reached)) break
  }
  all(treatments %in% reached)
}

.treatment_key <- function(x) {
  y <- tolower(.trim_chr(x))
  gsub("[[:space:]]+", " ", y)
}

.validate_auctus_workbook <- function(wb) {
  diag <- .new_diagnostics()
  if (length(wb$missing_sheets)) {
    for (sheet in wb$missing_sheets) {
      diag <- .add_diag(
        diag, "ERROR", "E003_MISSING_SHEET", sheet = sheet,
        message = sprintf("Sheet wajib '%s' tidak ditemukan.", sheet),
        suggestion = "Gunakan template V2.3. Jangan mengganti nama sheet input.",
        example = sheet
      )
    }
    return(diag)
  }

  wb <- .augment_internal_ids(wb)

  analyses <- wb$analyses
  metadata <- wb$study_metadata
  arms <- wb$arm_data
  effects <- wb$effect_data
  dta <- wb$diagnostic_data

  diag <- .validate_required(diag, analyses,
    c("analysis_type", "outcome_name", "outcome_type", "outcome_direction"),
    "analyses")
  diag <- .validate_required(diag, metadata, "study_label", "study_metadata")
  diag <- .validate_enum(diag, analyses, "analysis_type",
    c("pairwise_ma", "nma", "proportion_ma", "diagnostic_ma"))
  diag <- .validate_enum(diag, analyses, "outcome_type",
    c("binary", "continuous", "proportion", "mean", "diagnostic"))
  diag <- .validate_enum(diag, analyses, "outcome_direction",
    c("lower_better", "higher_better", "neutral"))

  analyses$analysis_id <- .trim_chr(analyses$analysis_id)
  analyses$analysis_type <- tolower(.trim_chr(analyses$analysis_type))
  analyses$outcome_type <- tolower(.trim_chr(analyses$outcome_type))
  analyses$effect_measure <- toupper(.trim_chr(analyses$effect_measure))
  analyses$reference_treatment <- .trim_chr(analyses$reference_treatment)

  duplicate_outcome <- which(!is.na(analyses$outcome_key) & duplicated(analyses$outcome_key))
  for (i in duplicate_outcome) {
    diag <- .diag_for_cell(
      diag, "ERROR", "E202_DUPLICATE_OUTCOME_NAME", analyses, i, "outcome_name",
      sprintf("outcome_name '%s' sama dengan baris lain setelah normalisasi.", analyses$outcome_name[[i]]),
      "Gunakan satu nama outcome yang unik untuk setiap analisis.", "Mortality at 30 days"
    )
  }

  valid_effects <- list(binary = c("OR", "RR", "HR"), continuous = c("MD", "SMD"))
  for (i in seq_len(nrow(analyses))) {
    type <- analyses$outcome_type[[i]]
    atype <- analyses$analysis_type[[i]]
    em <- analyses$effect_measure[[i]]
    if (type %in% names(valid_effects) && (is.na(em) || !em %in% valid_effects[[type]])) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E103_INVALID_EFFECT_MEASURE", analyses, i, "effect_measure",
        sprintf("Effect measure '%s' tidak sesuai untuk outcome %s.", em %||% "", type),
        sprintf("Gunakan: %s.", paste(valid_effects[[type]], collapse = ", ")),
        valid_effects[[type]][[1L]]
      )
    }
    if (atype %in% c("pairwise_ma", "nma") &&
        is.na(analyses$reference_treatment[[i]])) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E104_MISSING_REFERENCE", analyses, i, "reference_treatment",
        "Reference treatment wajib diisi untuk comparative analysis.",
        "Isi nama treatment persis seperti pada arm_data atau effect_data.", "Placebo"
      )
    }
    expected <- switch(atype,
      pairwise_ma = c("binary", "continuous"),
      nma = c("binary", "continuous"),
      proportion_ma = c("proportion", "mean"),
      diagnostic_ma = "diagnostic",
      character()
    )
    if (!is.na(type) && length(expected) && !type %in% expected) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E105_INCOMPATIBLE_ANALYSIS_TYPE", analyses, i, "outcome_type",
        sprintf("Outcome type '%s' tidak kompatibel dengan analysis type '%s'.", type, atype),
        sprintf("Gunakan salah satu: %s.", paste(expected, collapse = ", ")),
        expected[[1L]]
      )
    }
    if (identical(em, "MD") && is.na(.trim_chr(analyses$unit[[i]]))) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E106_MISSING_UNIT", analyses, i, "unit",
        "Unit wajib diisi untuk mean difference.",
        "Isi unit klinis yang digunakan seluruh studi.", "mmHg"
      )
    }
  }

  analysis_ids <- unique(analyses$analysis_id[!is.na(analyses$analysis_id)])
  if (!length(analysis_ids)) {
    diag <- .add_diag(
      diag, "ERROR", "E300_NO_ANALYSIS", sheet = "analyses",
      message = "Sheet analyses belum berisi outcome yang akan dianalisis.",
      suggestion = "Tambahkan satu baris outcome lengkap pada sheet analyses.",
      example = "Mortality at 30 days"
    )
  }
  for (sheet_name in c("arm_data", "effect_data", "diagnostic_data")) {
    dat <- wb[[sheet_name]]
    if (!nrow(dat)) next
    bad <- which(!is.na(dat$outcome_key) & is.na(dat$analysis_id))
    for (i in bad) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E203_UNKNOWN_OUTCOME_NAME", dat, i, "outcome_name",
        sprintf("outcome_name '%s' tidak ditemukan pada sheet analyses.", dat$outcome_name[[i]]),
        "Pilih outcome_name dari dropdown atau perbaiki salah ketik.",
        .first_nonmissing(analyses$outcome_name, "Mortality at 30 days")
      )
    }
    normalized_outcome <- which(
      !is.na(dat$analysis_id) & !is.na(.trim_chr(dat$outcome_name)) &
        dat$outcome_name != dat$canonical_outcome_name
    )
    for (i in normalized_outcome) {
      diag <- .diag_for_cell(
        diag, "INFO", "I101_LABEL_NORMALIZED", dat, i, "outcome_name",
        sprintf("Label outcome dinormalisasi menjadi '%s'.", dat$canonical_outcome_name[[i]]),
        "Tidak perlu diperbaiki, tetapi gunakan dropdown agar label konsisten.",
        dat$canonical_outcome_name[[i]]
      )
    }
  }

  dup_meta <- which(!is.na(metadata$study_key) & duplicated(metadata$study_key))
  for (i in dup_meta) {
    diag <- .diag_for_cell(
      diag, "ERROR", "E204_DUPLICATE_STUDY_LABEL", metadata, i, "study_label",
      sprintf("study_label '%s' sama dengan baris lain setelah normalisasi.", metadata$study_label[[i]]),
      "Gunakan satu study_label global yang unik. Tambahkan a/b atau nama cohort bila diperlukan.",
      "Smith 2024a"
    )
  }

  for (sheet_name in c("arm_data", "effect_data", "diagnostic_data")) {
    dat <- wb[[sheet_name]]
    if (!nrow(dat)) next
    bad <- which(!is.na(dat$study_key) & is.na(dat$study_id))
    for (i in bad) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E205_UNKNOWN_STUDY_LABEL", dat, i, "study_label",
        sprintf("study_label '%s' tidak ditemukan pada study_metadata.", dat$study_label[[i]]),
        "Pilih study_label dari dropdown atau tambahkan studi ke study_metadata.",
        .first_nonmissing(metadata$study_label, "Smith 2024")
      )
    }
    normalized_study <- which(
      !is.na(dat$study_id) & !is.na(.trim_chr(dat$study_label)) &
        dat$study_label != dat$canonical_study_label
    )
    for (i in normalized_study) {
      diag <- .diag_for_cell(
        diag, "INFO", "I101_LABEL_NORMALIZED", dat, i, "study_label",
        sprintf("Label studi dinormalisasi menjadi '%s'.", dat$canonical_study_label[[i]]),
        "Tidak perlu diperbaiki, tetapi gunakan dropdown agar label konsisten.",
        dat$canonical_study_label[[i]]
      )
    }
  }

  arms$analysis_id <- .trim_chr(arms$analysis_id)
  arms$study_id <- .trim_chr(arms$study_id)
  arms$treatment <- .trim_chr(arms$treatment)
  active_arms <- seq_len(nrow(arms))
  for (i in active_arms) {
    required_arm <- c("outcome_name", "study_label", "treatment")
    for (col in required_arm) {
      if (is.na(.trim_chr(arms[[col]][[i]]))) {
        diag <- .diag_for_cell(
          diag, "ERROR", "E002_MISSING_REQUIRED", arms, i, col,
          sprintf("Nilai '%s' wajib diisi.", col),
          "Isi sel sesuai PETUNJUK.", if (col == "study_label") "Smith 2024" else "Placebo"
        )
      }
    }
    for (col in c("event", "sample", "mean", "sd", "median", "q1", "q3", "min", "max")) {
      diag <- .validate_numeric_cell(
        diag, arms, i, col,
        minimum = if (col %in% c("event", "sample", "sd")) 0 else -Inf,
        strictly_positive = col == "sample",
        integer = col %in% c("event", "sample")
      )
    }
    ev <- .as_num(arms$event[[i]])
    nn <- .as_num(arms$sample[[i]])
    if (!is.na(ev) && !is.na(nn) && ev > nn) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E107_EVENT_EXCEEDS_SAMPLE", arms, i, "event",
        sprintf("Event (%s) lebih besar dari sample (%s).", ev, nn),
        "Pastikan 0 <= event <= sample.", as.character(max(0, nn - 1))
      )
    }
    if (!is.na(.trim_chr(arms$median[[i]]))) {
      diag <- .diag_for_cell(
        diag, "WARNING", "W301_MEDIAN_CONVERTED", arms, i, "median",
        "Median akan dikonversi menjadi estimasi mean/SD.",
        "Periksa asumsi distribusi. Engine juga membuat sensitivity analysis tanpa studi hasil konversi.",
        arms$median[[i]]
      )
    }
  }

  treatment_arm_key <- paste(
    arms$analysis_id, arms$study_id, .treatment_key(arms$treatment), sep = "||"
  )
  dup_treatment_arm <- which(
    !is.na(arms$analysis_id) & !is.na(arms$study_id) &
      duplicated(treatment_arm_key) & !is.na(arms$treatment)
  )
  for (i in dup_treatment_arm) {
    diag <- .diag_for_cell(
      diag, "ERROR", "E209_DUPLICATE_TREATMENT_ARM", arms, i, "treatment",
      "Treatment yang sama muncul pada lebih dari satu baris arm dalam studi yang sama.",
      "Pertahankan satu baris per treatment, atau gunakan label treatment yang benar-benar berbeda.",
      arms$treatment[[i]]
    )
  }

  effects$analysis_id <- .trim_chr(effects$analysis_id)
  effects$study_id <- .trim_chr(effects$study_id)
  effects$treat1 <- .trim_chr(effects$treat1)
  effects$treat2 <- .trim_chr(effects$treat2)
  effects$estimate_type <- tolower(.trim_chr(effects$estimate_type))
  active_effects <- seq_len(nrow(effects))
  for (i in active_effects) {
    for (col in c("outcome_name", "study_label", "treat1", "treat2", "effect", "estimate_type")) {
      if (is.na(.trim_chr(effects[[col]][[i]]))) {
        diag <- .diag_for_cell(
          diag, "ERROR", "E002_MISSING_REQUIRED", effects, i, col,
          sprintf("Nilai '%s' wajib diisi.", col),
          "Isi sel sesuai PETUNJUK.", if (col == "estimate_type") "adjusted" else "Treatment A"
        )
      }
    }
    if (!is.na(effects$treat1[[i]]) && identical(effects$treat1[[i]], effects$treat2[[i]])) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E206_SELF_COMPARISON", effects, i, "treat2",
        "Treatment tidak boleh dibandingkan dengan dirinya sendiri.",
        "Perbaiki treat1 atau treat2.", "Placebo"
      )
    }
    if (!is.na(effects$estimate_type[[i]]) &&
        !effects$estimate_type[[i]] %in% c("adjusted", "crude")) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E101_INVALID_VALUE", effects, i, "estimate_type",
        "estimate_type harus adjusted atau crude.",
        "Pilih nilai dari dropdown.", "adjusted"
      )
    }
    for (col in c("effect", "ci_low", "ci_high", "se", "ci_level", "sample1", "sample2")) {
      diag <- .validate_numeric_cell(
        diag, effects, i, col,
        minimum = if (col %in% c("ci_level")) 1 else 0,
        strictly_positive = col %in% c("se", "sample1", "sample2")
      )
    }
    aid <- effects$analysis_id[[i]]
    ai <- match(aid, analyses$analysis_id)
    em <- if (!is.na(ai)) analyses$effect_measure[[ai]] else NA_character_
    val <- .as_num(effects$effect[[i]])
    lo <- .as_num(effects$ci_low[[i]])
    hi <- .as_num(effects$ci_high[[i]])
    se <- .as_num(effects$se[[i]])
    if (em %in% c("OR", "RR", "HR") && !is.na(val) && val <= 0) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E108_NONPOSITIVE_RATIO", effects, i, "effect",
        "OR, RR, dan HR harus lebih besar dari 0.",
        "Jika nol berasal dari event count, masukkan event dan sample pada arm_data.", "0.75"
      )
    }
    if (em %in% c("OR", "RR", "HR") &&
        ((!is.na(lo) && lo <= 0) || (!is.na(hi) && hi <= 0))) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E113_NONPOSITIVE_RATIO_CI", effects, i, "ci_low",
        "Confidence interval OR, RR, dan HR harus seluruhnya lebih besar dari 0.",
        "Periksa bahwa effect dan CI dimasukkan pada skala natural.", "0.60"
      )
    }
    ci_level <- .as_num(effects$ci_level[[i]])
    if (!is.na(ci_level) && (ci_level <= 1 || ci_level >= 100)) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E114_INVALID_CI_LEVEL", effects, i, "ci_level",
        "ci_level harus lebih besar dari 1 dan lebih kecil dari 100.",
        "Isi confidence level dalam persen.", "95"
      )
    }
    if (is.na(se) && (is.na(lo) || is.na(hi))) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E109_MISSING_UNCERTAINTY", effects, i, "se",
        "Reported effect membutuhkan SE atau CI lengkap.",
        "Isi se, atau isi ci_low dan ci_high.", "0.15"
      )
    }
    if (!is.na(val) && !is.na(lo) && !is.na(hi) && !(lo < val && val < hi)) {
      diag <- .diag_for_cell(
        diag, "ERROR", "E110_INVALID_CI_ORDER", effects, i, "ci_low",
        "Confidence interval harus memenuhi ci_low < effect < ci_high.",
        "Periksa kembali skala effect dan CI.", "0.60"
      )
    }
  }

  pair_key <- paste(
    effects$analysis_id, effects$study_id,
    pmin(effects$treat1, effects$treat2), pmax(effects$treat1, effects$treat2),
    effects$estimate_type, sep = "||"
  )
  dup_effect <- which(
    !is.na(effects$analysis_id) & !is.na(effects$study_id) &
      duplicated(pair_key)
  )
  for (i in dup_effect) {
    diag <- .diag_for_cell(
      diag, "ERROR", "E201_DUPLICATE_ESTIMATE", effects, i, "study_label",
      "Estimate untuk studi, comparison, dan estimate_type yang sama muncul lebih dari sekali.",
      "Hapus duplikasi atau pilih satu estimate yang akan digunakan.", effects$study_label[[i]]
    )
  }

  treatment_entries <- rbind(
    data.frame(
      analysis_id = arms$analysis_id[active_arms], value = arms$treatment[active_arms],
      sheet = arms$source_sheet[active_arms], row = arms$source_row[active_arms],
      column = rep(.column_letter(arms, "treatment"), length(active_arms)),
      stringsAsFactors = FALSE
    ),
    data.frame(
      analysis_id = effects$analysis_id[active_effects], value = effects$treat1[active_effects],
      sheet = effects$source_sheet[active_effects], row = effects$source_row[active_effects],
      column = rep(.column_letter(effects, "treat1"), length(active_effects)),
      stringsAsFactors = FALSE
    ),
    data.frame(
      analysis_id = effects$analysis_id[active_effects], value = effects$treat2[active_effects],
      sheet = effects$source_sheet[active_effects], row = effects$source_row[active_effects],
      column = rep(.column_letter(effects, "treat2"), length(active_effects)),
      stringsAsFactors = FALSE
    )
  )
  treatment_entries$key <- .treatment_key(treatment_entries$value)
  treatment_entries <- treatment_entries[
    !is.na(treatment_entries$key) & !is.na(treatment_entries$analysis_id), , drop = FALSE
  ]
  collision_groups <- split(
    seq_len(nrow(treatment_entries)),
    paste(treatment_entries$analysis_id, treatment_entries$key, sep = "||")
  )
  for (idx in collision_groups) {
    labels <- unique(treatment_entries$value[idx])
    if (length(labels) < 2L) next
    first <- idx[[1L]]
    diag <- .add_diag(
      diag, "ERROR", "E208_TREATMENT_LABEL_COLLISION",
      sheet = treatment_entries$sheet[[first]], excel_row = treatment_entries$row[[first]],
      column = treatment_entries$column[[first]],
      analysis_id = treatment_entries$analysis_id[[first]],
      value = paste(labels, collapse = " | "),
      message = "Beberapa treatment menjadi label yang sama setelah normalisasi huruf dan spasi.",
      suggestion = "Gunakan satu ejaan treatment yang konsisten pada seluruh sheet.",
      example = labels[[1L]]
    )
  }

  dta$analysis_id <- .trim_chr(dta$analysis_id)
  dta$study_id <- .trim_chr(dta$study_id)
  active_dta <- seq_len(nrow(dta))
  for (i in active_dta) {
    for (col in c("outcome_name", "study_label", "tp", "fp", "fn", "tn")) {
      if (is.na(.trim_chr(dta[[col]][[i]]))) {
        diag <- .diag_for_cell(
          diag, "ERROR", "E002_MISSING_REQUIRED", dta, i, col,
          sprintf("Nilai '%s' wajib diisi.", col), "Isi angka tabel 2x2.", "10"
        )
      }
    }
    for (col in c("tp", "fp", "fn", "tn")) {
      diag <- .validate_numeric_cell(diag, dta, i, col, minimum = 0, integer = TRUE, required = TRUE)
    }
  }

  for (aid in analysis_ids) {
    ai <- match(aid, analyses$analysis_id)
    atype <- analyses$analysis_type[[ai]]
    otype <- analyses$outcome_type[[ai]]
    ref <- analyses$reference_treatment[[ai]]
    aa <- arms[!is.na(arms$analysis_id) & arms$analysis_id == aid, , drop = FALSE]
    ee <- effects[!is.na(effects$analysis_id) & effects$analysis_id == aid, , drop = FALSE]
    dd <- dta[!is.na(dta$analysis_id) & dta$analysis_id == aid, , drop = FALSE]

    if (nrow(aa)) {
      for (arm_i in seq_len(nrow(aa))) {
        required_values <- if (otype %in% c("binary", "proportion")) {
          c("event", "sample")
        } else if (otype %in% c("continuous", "mean")) {
          "sample"
        } else {
          character()
        }
        for (col in required_values) {
          if (is.na(.as_num(aa[[col]][[arm_i]]))) {
            original_i <- which(arms$source_sheet == aa$source_sheet[[arm_i]] &
                                  arms$source_row == aa$source_row[[arm_i]])[[1L]]
            diag <- .diag_for_cell(
              diag, "ERROR", "E111_MISSING_OUTCOME_DATA", arms, original_i, col,
              sprintf("Kolom '%s' wajib untuk outcome type '%s'.", col, otype),
              if (otype %in% c("binary", "proportion")) "Isi event dan sample untuk arm ini." else "Isi sample untuk arm ini.",
              if (col == "event") "0" else "100"
            )
          }
        }
        if (otype %in% c("continuous", "mean")) {
          has_mean_sd <- !is.na(.as_num(aa$mean[[arm_i]])) &&
            !is.na(.as_num(aa$sd[[arm_i]])) && .as_num(aa$sd[[arm_i]]) > 0
          has_median_iqr <- !is.na(.as_num(aa$median[[arm_i]])) &&
            !is.na(.as_num(aa$q1[[arm_i]])) && !is.na(.as_num(aa$q3[[arm_i]]))
          has_median_range <- !is.na(.as_num(aa$median[[arm_i]])) &&
            !is.na(.as_num(aa$min[[arm_i]])) && !is.na(.as_num(aa$max[[arm_i]]))
          if (!(has_mean_sd || has_median_iqr || has_median_range)) {
            original_i <- which(arms$source_sheet == aa$source_sheet[[arm_i]] &
                                  arms$source_row == aa$source_row[[arm_i]])[[1L]]
            diag <- .diag_for_cell(
              diag, "ERROR", "E112_INCOMPLETE_CONTINUOUS_DATA", arms, original_i, "mean",
              "Data continuous membutuhkan mean+SD, median+Q1+Q3, atau median+min+max.",
              "Lengkapi salah satu set ringkasan pada baris arm yang sama.", "mean=120; sd=15"
            )
          }
        }
      }
      arm_counts <- table(aa$study_id)
      if (otype == "binary" && analyses$effect_measure[[ai]] %in% c("OR", "RR")) {
        for (sid in unique(aa$study_id)) {
          study_events <- .as_num(aa$event[aa$study_id == sid])
          if (length(study_events) >= 2L && all(!is.na(study_events)) && all(study_events == 0)) {
            diag <- .add_diag(
              diag, "INFO", "I201_DOUBLE_ZERO_EXCLUDED", sheet = "arm_data",
              analysis_id = aid, study_id = sid, value = "all arms event=0",
              message = "Studi double-zero tidak memberi informasi untuk pooled OR/RR.",
              suggestion = "Studi tetap tercatat, tetapi tidak dimasukkan ke effect relatif utama."
            )
          }
        }
      }
      if (atype %in% c("pairwise_ma", "nma")) {
        bad_studies <- names(arm_counts[arm_counts < 2L])
        for (sid in bad_studies) {
          diag <- .add_diag(
            diag, "ERROR", "E305_INSUFFICIENT_ARMS", sheet = "arm_data",
            analysis_id = aid, study_id = sid,
            message = "Comparative study membutuhkan minimal dua arm.",
            suggestion = "Tambahkan arm pembanding untuk study_label ini."
          )
        }
      }
      if (atype == "proportion_ma") {
        bad_studies <- names(arm_counts[arm_counts != 1L])
        for (sid in bad_studies) {
          diag <- .add_diag(
            diag, "ERROR", "E306_SINGLE_ARM_COUNT", sheet = "arm_data",
            analysis_id = aid, study_id = sid,
            message = "Single-arm analysis membutuhkan tepat satu baris arm per study_label.",
            suggestion = "Pisahkan population yang berbeda menjadi study_label yang berbeda."
          )
        }
      }
    }

    if (atype == "diagnostic_ma" && nrow(dd) < 2L) {
      diag <- .add_diag(
        diag, "ERROR", "E301_INSUFFICIENT_STUDIES", sheet = "diagnostic_data",
        analysis_id = aid,
        message = "Meta-analisis diagnostik membutuhkan minimal dua studi valid.",
        suggestion = "Tambahkan minimal dua studi lengkap atau hapus outcome ini dari sheet analyses."
      )
      next
    }
    if (atype == "proportion_ma" && length(unique(aa$study_id)) < 2L) {
      diag <- .add_diag(
        diag, "ERROR", "E301_INSUFFICIENT_STUDIES", sheet = "arm_data",
        analysis_id = aid,
        message = "Single-arm meta-analysis membutuhkan minimal dua studi valid.",
        suggestion = "Tambahkan minimal dua studi dengan data yang lengkap."
      )
      next
    }
    if (atype %in% c("pairwise_ma", "nma")) {
      studies <- unique(c(aa$study_id, ee$study_id))
      studies <- studies[!is.na(studies)]
      if (length(studies) < 2L) {
        diag <- .add_diag(
          diag, "ERROR", "E301_INSUFFICIENT_STUDIES", sheet = "analyses",
          analysis_id = aid,
          message = "Comparative meta-analysis membutuhkan minimal dua studi valid.",
          suggestion = "Tambahkan studi atau periksa outcome_name pada sheet data."
        )
      }
      if (otype == "binary" && identical(analyses$effect_measure[[ai]], "HR") && nrow(aa)) {
        diag <- .add_diag(
          diag, "ERROR", "E302_HR_FROM_RAW_EVENTS", sheet = "arm_data",
          analysis_id = aid,
          message = "HR tidak dapat dihitung dari event dan sample.",
          suggestion = "Masukkan reported HR beserta CI atau SE pada effect_data."
        )
      }

      treatments <- unique(c(aa$treatment, ee$treat1, ee$treat2))
      treatments <- treatments[!is.na(treatments)]
      if (!is.na(ref) && !ref %in% treatments) {
        diag <- .add_diag(
          diag, "ERROR", "E303_REFERENCE_NOT_FOUND", sheet = "analyses",
          analysis_id = aid, value = ref,
          message = sprintf("Reference treatment '%s' tidak ditemukan pada data.", ref),
          suggestion = "Samakan ejaan reference_treatment dengan nama treatment pada data."
        )
      }
      edges <- data.frame(treat1 = character(), treat2 = character(), stringsAsFactors = FALSE)
      if (nrow(aa)) {
        for (sid in unique(aa$study_id)) {
          tr <- unique(aa$treatment[aa$study_id == sid])
          tr <- tr[!is.na(tr)]
          if (length(tr) >= 2L) {
            cm <- utils::combn(tr, 2L)
            edges <- rbind(edges, data.frame(treat1 = cm[1, ], treat2 = cm[2, ]))
          }
        }
      }
      if (nrow(ee)) edges <- rbind(edges, ee[, c("treat1", "treat2")])
      if (atype == "pairwise_ma" && length(treatments) != 2L) {
        diag <- .add_diag(
          diag, "ERROR", "E304_PAIRWISE_TREATMENT_COUNT", sheet = "analyses",
          analysis_id = aid,
          message = sprintf("Pairwise MA harus memiliki tepat dua treatment; ditemukan %d.", length(treatments)),
          suggestion = "Gunakan analysis_type nma jika terdapat lebih dari dua treatment."
        )
      }
      if (atype == "nma" && nrow(edges) && !.network_connected(treatments, edges, ref)) {
        diag <- .add_diag(
          diag, "ERROR", "E401_DISCONNECTED_NETWORK", sheet = "analyses",
          analysis_id = aid,
          message = "Network tidak terhubung dengan reference treatment.",
          suggestion = "Periksa treatment name atau tambahkan studi yang menghubungkan komponen network."
        )
      }

      for (sid in unique(ee$study_id)) {
        e_sid <- ee[ee$study_id == sid, , drop = FALSE]
        tr <- unique(c(e_sid$treat1, e_sid$treat2))
        tr <- tr[!is.na(tr)]
        if (length(tr) > 2L) {
          observed <- unique(paste(pmin(e_sid$treat1, e_sid$treat2),
                                   pmax(e_sid$treat1, e_sid$treat2), sep = "||"))
          required_pairs <- choose(length(tr), 2L)
          if (length(observed) < required_pairs) {
            diag <- .add_diag(
              diag, "ERROR", "E402_INCOMPLETE_MULTIARM_CONTRAST", sheet = "effect_data",
              analysis_id = aid, study_id = sid,
              message = sprintf(
                "Studi contrast-only %s memiliki %d treatment tetapi hanya %d dari %d comparison.",
                sid, length(tr), length(observed), required_pairs
              ),
              suggestion = "Masukkan seluruh pairwise comparison atau gunakan arm_data. Covariance tidak boleh direka."
            )
          }
        }
      }

      shared_multiarm <- intersect(unique(aa$study_id), unique(ee$study_id))
      for (sid in shared_multiarm) {
        arm_treatments <- unique(aa$treatment[aa$study_id == sid])
        arm_treatments <- arm_treatments[!is.na(arm_treatments)]
        if (length(arm_treatments) <= 2L) next
        e_sid <- ee[ee$study_id == sid, , drop = FALSE]
        priority_type <- if ("adjusted" %in% e_sid$estimate_type) "adjusted" else "crude"
        priority_rows <- e_sid[e_sid$estimate_type == priority_type, , drop = FALSE]
        observed <- unique(paste(
          pmin(priority_rows$treat1, priority_rows$treat2),
          pmax(priority_rows$treat1, priority_rows$treat2), sep = "||"
        ))
        required_pairs <- choose(length(arm_treatments), 2L)
        if (length(observed) < required_pairs) {
          diag <- .add_diag(
            diag, "ERROR", "E403_PARTIAL_PRIORITY_MULTIARM", sheet = "effect_data",
            analysis_id = aid, study_id = sid,
            message = sprintf(
              "Studi multi-arm %s memiliki reported %s estimate yang hanya mencakup %d dari %d comparison.",
              sid, priority_type, length(observed), required_pairs
            ),
            suggestion = paste0(
              "Lengkapi seluruh comparison untuk sumber prioritas ini, atau hapus reported estimate ",
              "agar arm_data digunakan secara konsisten."
            )
          )
        }
      }

      source_types <- character()
      if (nrow(aa)) source_types <- c(source_types, "raw_derived")
      if (nrow(ee)) source_types <- c(source_types, unique(ee$estimate_type))
      source_types <- unique(source_types[!is.na(source_types)])
      if (length(source_types) > 1L) {
        diag <- .add_diag(
          diag, "WARNING", "W201_MIXED_ESTIMATE_SOURCE", sheet = "analyses",
          analysis_id = aid, value = paste(source_types, collapse = ", "),
          message = "Model utama menggabungkan adjusted, crude, atau raw-derived estimate.",
          suggestion = "Interpretasikan dengan hati-hati. Sensitivity analysis menurut sumber estimate akan dibuat."
        )
      }
    }
  }

  diag <- .enrich_diagnostic_labels(diag, analyses, metadata)
  diag$severity <- factor(diag$severity, levels = c("ERROR", "WARNING", "INFO"))
  diag <- diag[order(diag$severity, diag$outcome_name, diag$study_label,
                     diag$sheet, diag$excel_row), , drop = FALSE]
  diag$severity <- as.character(diag$severity)
  rownames(diag) <- NULL
  diag
}

# =============================================================================
# WORKBOOK CREATION, VALIDATION OUTPUT, AND LEGACY ADAPTER
# =============================================================================

.auctus_example_data <- function() {
  list(
    analyses = data.frame(
      analysis_type = c("pairwise_ma", "nma", "proportion_ma", "diagnostic_ma"),
      outcome_name = c(
        "Mortality at 30 days", "Clinical response at end of treatment",
        "Baseline prevalence", "Diagnostic accuracy"
      ),
      timepoint = NA_character_,
      outcome_type = c("binary", "binary", "proportion", "diagnostic"),
      effect_measure = c("OR", "OR", NA, NA),
      reference_treatment = c("Placebo", "Placebo", NA, NA),
      outcome_direction = c("lower_better", "higher_better", "neutral", "neutral"),
      unit = c(NA, NA, "%", NA),
      notes = c(
        "Contoh pairwise binary raw dan reported effect.",
        "Studi multi-arm tetap satu baris per arm.",
        "Proportion menerima event 0 dan event sama dengan sample.",
        "Diagnostic membutuhkan TP, FP, FN, dan TN."
      ),
      stringsAsFactors = FALSE
    ),
    study_metadata = data.frame(
      study_label = c(
        "Smith 2024", "Lee 2023", "Garcia 2022", "Khan 2024", "Budi 2023",
        "Sari 2024", "Reported Example 2021", "DTA Smith 2023", "DTA Lee 2024"
      ),
      year = c(2024, 2023, 2022, 2024, 2023, 2024, 2021, 2023, 2024),
      study_design = c(
        "RCT", "RCT", "RCT", "RCT", "Cross-sectional", "Cross-sectional",
        "Cohort", "Cross-sectional", "Cross-sectional"
      ),
      subgroup_region = c("Asia", "Europe", "America", "Asia", "Asia", "Asia", "Europe", "Asia", "Europe"),
      moderator_num_mean_age = c(57, 61, 48, 52, 35, 38, 64, 49, 53),
      stringsAsFactors = FALSE
    ),
    arm_data = data.frame(
      outcome_name = c(
        rep("Mortality at 30 days", 4),
        rep("Clinical response at end of treatment", 6),
        rep("Baseline prevalence", 2)
      ),
      study_label = c(
        "Smith 2024", "Smith 2024", "Lee 2023", "Lee 2023",
        rep("Garcia 2022", 3), rep("Khan 2024", 3), "Budi 2023", "Sari 2024"
      ),
      treatment = c(
        "Treatment A", "Placebo", "Treatment A", "Placebo",
        "Treatment A", "Treatment B", "Placebo", "Treatment A", "Treatment C", "Placebo",
        "Population", "Population"
      ),
      event = c(8, 15, 0, 7, 22, 17, 31, 18, 13, 26, 0, 24),
      sample = c(100, 100, 80, 82, 120, 118, 122, 105, 103, 108, 150, 180),
      mean = NA, sd = NA, median = NA, q1 = NA, q3 = NA, min = NA, max = NA,
      notes = "Contoh",
      stringsAsFactors = FALSE
    ),
    effect_data = data.frame(
      outcome_name = "Mortality at 30 days",
      study_label = "Reported Example 2021",
      treat1 = "Treatment A",
      treat2 = "Placebo",
      effect = 0.72,
      ci_low = 0.52,
      ci_high = 0.99,
      se = NA,
      ci_level = 95,
      estimate_type = "adjusted",
      adjustment_variables = "Age; sex; baseline risk",
      sample1 = 95,
      sample2 = 96,
      notes = "Contoh reported adjusted OR",
      stringsAsFactors = FALSE
    ),
    diagnostic_data = data.frame(
      outcome_name = c("Diagnostic accuracy", "Diagnostic accuracy"),
      study_label = c("DTA Smith 2023", "DTA Lee 2024"),
      tp = c(82, 74), fp = c(12, 9), fn = c(18, 16), tn = c(188, 201),
      threshold = c("10 ng/mL", "10 ng/mL"),
      notes = "Contoh diagnostic 2x2",
      stringsAsFactors = FALSE
    )
  )
}

.auctus_template_input_data <- function() {
  blank <- function(columns, extras = character()) {
    values <- setNames(rep(list(NA_character_), length(c(columns, extras))), c(columns, extras))
    as.data.frame(values, stringsAsFactors = FALSE)
  }
  list(
    analyses = blank(.sheet_specs$analyses),
    study_metadata = blank(
      .sheet_specs$study_metadata,
      c("subgroup_region", "moderator_num_mean_age")
    ),
    arm_data = blank(.sheet_specs$arm_data),
    effect_data = blank(.sheet_specs$effect_data),
    diagnostic_data = blank(.sheet_specs$diagnostic_data)
  )
}

.auctus_instructions <- function() {
  data.frame(
    Bagian = c(
      "Mulai", "Langkah 1", "Langkah 2", "Langkah 3", "Kunci outcome",
      "Kunci studi", "Studi multi-arm", "Zero event", "Reported effect", "Warna biru",
      "Semua baris aktif", "Mengecualikan data", "Warna merah", "Warna kuning",
      "Warna biru muda", "Contoh", "Forest raw", "Forest reported/mixed",
      "Arah Favours", "Bantuan"
    ),
    Penjelasan = c(
      paste0("Template Auctus MA dan NMA V", .AUCTUS_SCHEMA_VERSION),
      "Isi satu baris pada analyses untuk setiap analisis yang ingin dijalankan.",
      "Isi study_metadata satu kali per study_label untuk seluruh workbook.",
      "Isi arm_data, effect_data, atau diagnostic_data sesuai sumber data.",
      "outcome_name wajib unik dan menjadi kunci penggabungan analisis. Pilih dari dropdown pada sheet data.",
      "study_label wajib unik secara global. Gunakan Smith 2024a, Smith 2024b, atau nama cohort bila diperlukan.",
      paste0(
        "Masukkan satu baris per arm. Studi 3 arm cukup 3 baris, bukan 3 contrast manual. ",
        "Tidak perlu ID teknis; arm dikenali dari outcome_name, study_label, dan treatment."
      ),
      "event boleh 0. sample harus lebih besar dari 0.",
      "Masukkan OR/RR/HR pada skala natural dan isi CI atau SE.",
      "Sel input user menggunakan font biru.",
      "Setiap baris yang mulai diisi dianggap aktif dan wajib lengkap.",
      "Hapus baris outcome atau data dari salinan workbook bila tidak ingin dianalisis.",
      "Sel merah pada validated_input.xlsx wajib diperbaiki.",
      "Sel kuning adalah warning yang harus dibaca sebelum interpretasi.",
      "Sel biru muda adalah informasi tentang keputusan atau eksklusi yang tidak memblokir analisis.",
      "Lihat CONTOH_PENGISIAN. Sheet read-only tersebut tidak dibaca engine; salin pola ke sheet input.",
      "Binary menampilkan Event dan Total per treatment. Continuous menampilkan Mean, SD, dan Total.",
      paste0(
        "Forest reported atau mixed hanya menampilkan Total per treatment dan membagi studi menjadi ",
        "blok Adjusted, Crude, dan Raw-derived. Sample yang tidak dilaporkan tampil sebagai NR."
      ),
      paste0(
        "Nama treatment pada header dan label Favours dibuat dinamis. lower_better, higher_better, ",
        "atau neutral harus dipilih berdasarkan arti klinis outcome."
      ),
      "Jalankan source('meta_nma_engine.R'), lalu hasil <- run_auctus_meta()."
    ),
    stringsAsFactors = FALSE
  )
}

.auctus_lookups <- function() {
  list(
    analysis_type = c("pairwise_ma", "nma", "proportion_ma", "diagnostic_ma"),
    outcome_type = c("binary", "continuous", "proportion", "mean", "diagnostic"),
    effect_measure = c("OR", "RR", "HR", "MD", "SMD"),
    outcome_direction = c("lower_better", "higher_better", "neutral"),
    estimate_type = c("adjusted", "crude")
  )
}

.repair_openxlsx_comment_relationships <- function(path) {
  .require_namespace("zip")
  temp_dir <- tempfile("auctus_xlsx_repair_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(path, exdir = temp_dir)
  rel_dir <- file.path(temp_dir, "xl", "worksheets", "_rels")
  rel_files <- if (dir.exists(rel_dir)) list.files(rel_dir, pattern = "\\.xml\\.rels$", full.names = TRUE) else character()
  changed <- FALSE
  for (rel_file in rel_files) {
    xml <- paste(readLines(rel_file, warn = FALSE, encoding = "UTF-8"), collapse = "")
    matches <- gregexpr(
      "<Relationship[^>]+Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/drawing\"[^>]*/>",
      xml, perl = TRUE
    )[[1L]]
    if (matches[[1L]] < 0L) next
    entries <- regmatches(xml, list(matches))[[1L]]
    for (entry in entries) {
      target <- sub(".*Target=\"([^\"]+)\".*", "\\1", entry)
      target_path <- normalizePath(file.path(dirname(dirname(rel_file)), target), mustWork = FALSE)
      if (!file.exists(target_path)) {
        rel_id <- sub(".*Id=\"([^\"]+)\".*", "\\1", entry)
        xml <- sub(entry, "", xml, fixed = TRUE)
        sheet_num <- sub("^sheet([0-9]+)\\.xml\\.rels$", "\\1", basename(rel_file))
        sheet_file <- file.path(temp_dir, "xl", "worksheets", paste0("sheet", sheet_num, ".xml"))
        if (file.exists(sheet_file)) {
          sheet_xml <- paste(readLines(sheet_file, warn = FALSE, encoding = "UTF-8"), collapse = "")
          drawing_pattern <- paste0("<drawing r:id=\"", rel_id, "\"/>")
          sheet_xml <- sub(drawing_pattern, "", sheet_xml, fixed = TRUE)
          writeLines(sheet_xml, sheet_file, useBytes = TRUE)
        }
        changed <- TRUE
      }
    }
    writeLines(xml, rel_file, useBytes = TRUE)
  }
  comment_files <- list.files(file.path(temp_dir, "xl"), pattern = "^comments[0-9]+\\.xml$", full.names = TRUE)
  for (comment_file in comment_files) {
    xml <- paste(readLines(comment_file, warn = FALSE, encoding = "UTF-8"), collapse = "")
    xml <- gsub("<name val=", "<rFont val=", xml, fixed = TRUE)
    writeLines(xml, comment_file, useBytes = TRUE)
    changed <- TRUE
  }
  if (changed) {
    files <- list.files(temp_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    unlink(path)
    zip::zipr(path, files = files, root = temp_dir, include_directories = FALSE, mode = "mirror")
  }
  invisible(path)
}

.write_auctus_workbook <- function(data, output_path, write_instructions = TRUE,
                                   example_data = NULL, migration_log = NULL) {
  .require_namespace("openxlsx")
  output_path <- .absolute_path(output_path)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  wb <- openxlsx::createWorkbook(creator = "Auctus")

  navy <- "#1F4E78"
  blue <- "#0070C0"
  pale_blue <- "#D9EAF7"
  pale_yellow <- "#FFF2CC"
  light_gray <- "#E7E6E6"
  white <- "#FFFFFF"
  header_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 10, fontColour = white, fgFill = navy,
    textDecoration = "bold", halign = "center", valign = "center",
    border = "bottom", borderColour = "#B4C6E7", wrapText = TRUE
  )
  input_style <- openxlsx::createStyle(fontName = "Arial", fontSize = 10, fontColour = blue)
  body_style <- openxlsx::createStyle(fontName = "Arial", fontSize = 10, valign = "top")
  title_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 18, textDecoration = "bold", fontColour = navy
  )
  section_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 10, textDecoration = "bold", fgFill = pale_blue,
    valign = "top", wrapText = TRUE
  )

  if (write_instructions) {
    openxlsx::addWorksheet(wb, "PETUNJUK", gridLines = FALSE, tabColour = navy)
    openxlsx::writeData(wb, "PETUNJUK", "AUCTUS V2.3", startRow = 1, startCol = 1)
    openxlsx::addStyle(wb, "PETUNJUK", title_style, rows = 1, cols = 1)
    openxlsx::writeData(wb, "PETUNJUK", paste("Schema version", .AUCTUS_SCHEMA_VERSION), startRow = 2, startCol = 1)
    instructions <- .auctus_instructions()
    openxlsx::writeDataTable(wb, "PETUNJUK", instructions, startRow = 4,
                             tableStyle = "TableStyleMedium2", withFilter = FALSE)
    openxlsx::addStyle(wb, "PETUNJUK", section_style, rows = 5:(4 + nrow(instructions)), cols = 1,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::setColWidths(wb, "PETUNJUK", cols = 1, widths = 24)
    openxlsx::setColWidths(wb, "PETUNJUK", cols = 2, widths = 95)
    openxlsx::setRowHeights(wb, "PETUNJUK", rows = 5:(4 + nrow(instructions)), heights = 34)
    openxlsx::freezePane(wb, "PETUNJUK", firstActiveRow = 5)
  }

  if (!is.null(example_data)) {
    openxlsx::addWorksheet(wb, "CONTOH_PENGISIAN", gridLines = FALSE, tabColour = "#70AD47")
    openxlsx::writeData(wb, "CONTOH_PENGISIAN", "CONTOH PENGISIAN, TIDAK DIBACA ENGINE",
                        startRow = 1, startCol = 1)
    openxlsx::addStyle(wb, "CONTOH_PENGISIAN", title_style, rows = 1, cols = 1)
    row <- 3L
    for (sheet in names(.sheet_specs)) {
      dat <- example_data[[sheet]] %||% .empty_df(.sheet_specs[[sheet]])
      dat <- .ensure_columns(dat, .sheet_specs[[sheet]])
      dat <- dat[, c(.sheet_specs[[sheet]], setdiff(names(dat), .sheet_specs[[sheet]])), drop = FALSE]
      openxlsx::writeData(wb, "CONTOH_PENGISIAN", paste0("Contoh sheet: ", sheet),
                          startRow = row, startCol = 1)
      openxlsx::addStyle(wb, "CONTOH_PENGISIAN", section_style, rows = row,
                         cols = seq_len(max(1L, ncol(dat))), gridExpand = TRUE)
      row <- row + 1L
      openxlsx::writeDataTable(
        wb, "CONTOH_PENGISIAN", dat, startRow = row, startCol = 1,
        tableStyle = "TableStyleMedium4", withFilter = FALSE,
        tableName = paste0("tbl_example_", sheet)
      )
      openxlsx::addStyle(wb, "CONTOH_PENGISIAN", header_style, rows = row,
                         cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
      if (nrow(dat)) {
        openxlsx::addStyle(wb, "CONTOH_PENGISIAN", body_style,
                           rows = (row + 1L):(row + nrow(dat)), cols = seq_len(ncol(dat)),
                           gridExpand = TRUE, stack = TRUE)
      }
      row <- row + nrow(dat) + 3L
    }
    openxlsx::freezePane(wb, "CONTOH_PENGISIAN", firstActiveRow = 3)
    openxlsx::setColWidths(wb, "CONTOH_PENGISIAN", cols = 1:20, widths = 16)
    openxlsx::setColWidths(wb, "CONTOH_PENGISIAN", cols = 1:2, widths = 28)
    openxlsx::protectWorksheet(wb, "CONTOH_PENGISIAN", protect = TRUE,
                               password = "auctus", lockInsertingRows = TRUE,
                               lockDeletingRows = TRUE)
  }

  if (!is.null(migration_log) && nrow(migration_log)) {
    openxlsx::addWorksheet(wb, "MIGRATION_LOG", gridLines = FALSE, tabColour = pale_yellow)
    openxlsx::writeDataTable(wb, "MIGRATION_LOG", migration_log,
                             tableStyle = "TableStyleMedium4", withFilter = TRUE)
    openxlsx::freezePane(wb, "MIGRATION_LOG", firstActiveRow = 2)
    openxlsx::setColWidths(wb, "MIGRATION_LOG", cols = seq_len(ncol(migration_log)), widths = "auto")
  }

  for (sheet in names(.sheet_specs)) {
    openxlsx::addWorksheet(wb, sheet, gridLines = FALSE, tabColour = if (sheet == "analyses") navy else blue)
    dat <- data[[sheet]] %||% .empty_df(.sheet_specs[[sheet]])
    dat <- .ensure_columns(dat, .sheet_specs[[sheet]])
    extra <- setdiff(names(dat), .sheet_specs[[sheet]])
    dat <- dat[, c(.sheet_specs[[sheet]], extra), drop = FALSE]
    openxlsx::writeDataTable(
      wb, sheet, dat, startRow = 1, startCol = 1,
      tableStyle = "TableStyleMedium2", withFilter = TRUE,
      tableName = paste0("tbl_", sheet)
    )
    openxlsx::addStyle(wb, sheet, header_style, rows = 1, cols = seq_len(ncol(dat)),
                       gridExpand = TRUE, stack = TRUE)
    if (nrow(dat)) {
      openxlsx::addStyle(wb, sheet, input_style, rows = 2:(nrow(dat) + 1L),
                         cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::freezePane(wb, sheet, firstActiveRow = 2, firstActiveCol = 3)
    widths <- pmin(34, pmax(12, nchar(names(dat)) + 3))
    widths[names(dat) %in% c("notes", "adjustment_variables", "study_label", "outcome_name")] <- 28
    openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(dat)), widths = widths)
    openxlsx::setRowHeights(wb, sheet, rows = 1, heights = 32)
    if (nrow(dat)) openxlsx::setRowHeights(wb, sheet, rows = 2:(nrow(dat) + 1L), heights = 24)
  }

  outcome_col <- match("outcome_name", .sheet_specs$analyses)
  study_col <- match("study_label", .sheet_specs$study_metadata)
  if (!is.na(outcome_col)) {
    openxlsx::createNamedRegion(
      wb, "analyses", cols = outcome_col, rows = 2:5000,
      name = "AUCTUS_OUTCOMES", overwrite = TRUE
    )
  }
  if (!is.na(study_col)) {
    openxlsx::createNamedRegion(
      wb, "study_metadata", cols = study_col, rows = 2:5000,
      name = "AUCTUS_STUDIES", overwrite = TRUE
    )
  }

  lookups <- .auctus_lookups()
  max_len <- max(lengths(lookups))
  lookup_df <- as.data.frame(lapply(lookups, function(x) c(x, rep(NA, max_len - length(x)))),
                             stringsAsFactors = FALSE)
  openxlsx::addWorksheet(wb, "LOOKUPS", visible = FALSE)
  openxlsx::writeData(wb, "LOOKUPS", lookup_df)
  openxlsx::addStyle(wb, "LOOKUPS", header_style, rows = 1, cols = seq_len(ncol(lookup_df)),
                     gridExpand = TRUE)
  openxlsx::protectWorksheet(
    wb, "LOOKUPS", protect = TRUE, password = "auctus",
    lockFormattingCells = TRUE, lockInsertingRows = TRUE,
    lockDeletingRows = TRUE, lockSorting = TRUE
  )

  dv_map <- list(
    analyses = list(
      analysis_type = "analysis_type", outcome_type = "outcome_type",
      effect_measure = "effect_measure", outcome_direction = "outcome_direction"
    ),
    arm_data = list(),
    effect_data = list(estimate_type = "estimate_type"),
    diagnostic_data = list()
  )
  for (sheet in names(dv_map)) {
    dat <- data[[sheet]] %||% .empty_df(.sheet_specs[[sheet]])
    dat <- .ensure_columns(dat, .sheet_specs[[sheet]])
    dat <- dat[, c(.sheet_specs[[sheet]], setdiff(names(dat), .sheet_specs[[sheet]])), drop = FALSE]
    for (col_name in names(dv_map[[sheet]])) {
      col <- match(col_name, names(dat))
      lookup_col <- match(dv_map[[sheet]][[col_name]], names(lookup_df))
      if (is.na(col) || is.na(lookup_col)) next
      last <- length(lookups[[dv_map[[sheet]][[col_name]]]]) + 1L
      formula <- sprintf("'LOOKUPS'!$%s$2:$%s$%d",
                         openxlsx::int2col(lookup_col), openxlsx::int2col(lookup_col), last)
      openxlsx::dataValidation(wb, sheet, cols = col, rows = 2:5000,
                               type = "list", value = formula, allowBlank = TRUE)
    }
  }

  for (sheet in c("arm_data", "effect_data", "diagnostic_data")) {
    dat <- data[[sheet]] %||% .empty_df(.sheet_specs[[sheet]])
    dat <- .ensure_columns(dat, .sheet_specs[[sheet]])
    dat <- dat[, c(.sheet_specs[[sheet]], setdiff(names(dat), .sheet_specs[[sheet]])), drop = FALSE]
    outcome_input_col <- match("outcome_name", names(dat))
    study_input_col <- match("study_label", names(dat))
    if (!is.na(outcome_input_col)) {
      openxlsx::dataValidation(
        wb, sheet, cols = outcome_input_col, rows = 2:5000,
        type = "list", value = "AUCTUS_OUTCOMES", allowBlank = TRUE
      )
    }
    if (!is.na(study_input_col)) {
      openxlsx::dataValidation(
        wb, sheet, cols = study_input_col, rows = 2:5000,
        type = "list", value = "AUCTUS_STUDIES", allowBlank = TRUE
      )
    }
  }

  header_help <- list(
    outcome_name = paste0(
      "Nama outcome sekaligus kunci analisis. Wajib unik pada analyses dan dipilih dari dropdown ",
      "pada sheet data. Engine membuat analysis_id internal secara otomatis."
    ),
    study_label = paste0(
      "Nama studi sekaligus kunci global. Wajib unik pada study_metadata dan dipilih dari dropdown ",
      "pada sheet data. Engine membuat study_id internal secara otomatis."
    ),
    timepoint = "Opsional. Boleh dikosongkan. Jika perlu, waktu dapat ditulis pada outcome_name.",
    treatment = paste0(
      "Nama treatment sekaligus identitas arm dalam studi. ",
      "Satu treatment hanya boleh muncul satu kali untuk study_label dan outcome_name yang sama."
    ),
    reference_treatment = "Treatment pembanding. Wajib untuk pairwise MA dan NMA.",
    outcome_direction = "Menentukan arah Favours dan interpretasi ranking.",
    event = "Jumlah peserta dengan event. Nilai 0 diperbolehkan.",
    sample = "Jumlah peserta dalam arm. Harus lebih besar dari 0.",
    effect = "Reported OR/RR/HR/MD/SMD pada skala natural.",
    se = "SE pada skala model. Untuk OR/RR/HR gunakan SE log-effect. Jika ragu, isi CI saja.",
    ci_level = "Confidence level dalam persen. Default 95 bila dikosongkan.",
    estimate_type = "adjusted atau crude.",
    subgroup_region = "Kolom berawalan subgroup_ dianalisis sebagai subgroup.",
    moderator_num_mean_age = "Kolom moderator_num_ dipakai untuk meta-regression numerik."
  )
  for (sheet in names(.sheet_specs)) {
    dat <- data[[sheet]] %||% .empty_df(.sheet_specs[[sheet]])
    dat <- .ensure_columns(dat, .sheet_specs[[sheet]])
    dat <- dat[, c(.sheet_specs[[sheet]], setdiff(names(dat), .sheet_specs[[sheet]])), drop = FALSE]
    for (nm in names(dat)) {
      help_text <- header_help[[nm]] %||% paste0(
        "Kolom teknis '", nm, "'. Lihat sheet PETUNJUK untuk aturan pengisian dan contoh."
      )
      comment <- openxlsx::createComment(help_text, author = "Auctus", visible = FALSE)
      openxlsx::writeComment(wb, sheet, col = match(nm, names(dat)), row = 1, comment = comment)
    }
  }

  openxlsx::modifyBaseFont(wb, fontName = "Arial", fontSize = 10, fontColour = "#000000")
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  .repair_openxlsx_comment_relationships(output_path)
  normalizePath(output_path, mustWork = FALSE)
}

create_auctus_template <- function(output_path = "template dataset MA & NMA.xlsx") {
  path <- .write_auctus_workbook(
    .auctus_template_input_data(), output_path, write_instructions = TRUE,
    example_data = .auctus_example_data()
  )
  message("Template Auctus V2.3 dibuat: ", path)
  invisible(path)
}

.validation_summary <- function(diag) {
  if (!nrow(diag)) {
    return(data.frame(
      outcome_name = "ALL", errors = 0L, warnings = 0L, info = 0L,
      status = "VALID", stringsAsFactors = FALSE
    ))
  }
  labels <- unique(c("ALL", diag$outcome_name[!is.na(diag$outcome_name)]))
  rows <- lapply(labels, function(label) {
    d <- if (label == "ALL") diag else diag[diag$outcome_name == label, , drop = FALSE]
    data.frame(
      outcome_name = label,
      errors = sum(d$severity == "ERROR"),
      warnings = sum(d$severity == "WARNING"),
      info = sum(d$severity == "INFO"),
      status = if (any(d$severity == "ERROR")) "INVALID" else if (any(d$severity == "WARNING")) "VALID_WITH_WARNINGS" else "VALID",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.write_validation_workbook <- function(input_path, diag, output_path) {
  .require_namespace("openxlsx")
  output_path <- .absolute_path(output_path)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ext <- tolower(tools::file_ext(input_path))
  if (ext != "xlsx") {
    stop("Workbook koreksi saat ini membutuhkan input .xlsx.", call. = FALSE)
  }
  file.copy(input_path, output_path, overwrite = TRUE)
  wb <- openxlsx::loadWorkbook(output_path)
  for (sheet in intersect(c("VALIDATION_SUMMARY", "ERROR_LOG"), names(wb))) {
    openxlsx::removeWorksheet(wb, sheet)
  }

  openxlsx::addWorksheet(wb, "VALIDATION_SUMMARY", gridLines = FALSE, tabColour = "#1F4E78")
  summary <- .validation_summary(diag)
  openxlsx::writeDataTable(wb, "VALIDATION_SUMMARY", summary, tableStyle = "TableStyleMedium2")
  openxlsx::freezePane(wb, "VALIDATION_SUMMARY", firstActiveRow = 2)
  openxlsx::setColWidths(wb, "VALIDATION_SUMMARY", cols = 1:ncol(summary), widths = "auto")

  openxlsx::addWorksheet(wb, "ERROR_LOG", gridLines = FALSE,
                         tabColour = if (any(diag$severity == "ERROR")) "#C00000" else "#FFC000")
  log_out <- .user_diagnostics(diag)
  if (!nrow(log_out)) {
    log_out <- data.frame(
      severity = "INFO", error_code = "I000_VALID", sheet = NA, excel_row = NA,
      column = NA, outcome_name = NA, study_label = NA, value = NA,
      message = "Tidak ditemukan error atau warning.", suggestion = "Data siap dianalisis.",
      example = NA, cell_link = NA, stringsAsFactors = FALSE
    )
  }
  openxlsx::writeDataTable(wb, "ERROR_LOG", log_out, tableStyle = "TableStyleMedium2")
  openxlsx::freezePane(wb, "ERROR_LOG", firstActiveRow = 2)
  openxlsx::setColWidths(wb, "ERROR_LOG", cols = 1:ncol(log_out), widths = "auto")
  openxlsx::setColWidths(wb, "ERROR_LOG", cols = match(c("message", "suggestion"), names(log_out)), widths = 55)
  link_col <- match("cell_link", names(log_out))
  hyperlink_style <- openxlsx::createStyle(fontColour = "#0563C1", textDecoration = "underline")
  if (!is.na(link_col)) {
    for (i in seq_len(nrow(log_out))) {
      target_sheet <- .trim_chr(log_out$sheet[[i]])
      target_row <- suppressWarnings(as.integer(log_out$excel_row[[i]]))
      target_col <- .trim_chr(log_out$column[[i]])
      if (is.na(target_sheet) || is.na(target_row) || is.na(target_col) ||
          !target_sheet %in% names(wb)) next
      link_formula <- openxlsx::makeHyperlinkString(
        sheet = target_sheet, row = target_row, col = openxlsx::col2int(target_col),
        text = paste0(target_sheet, "!", target_col, target_row)
      )
      openxlsx::writeFormula(
        wb, "ERROR_LOG", link_formula, startCol = link_col, startRow = i + 1L
      )
      openxlsx::addStyle(
        wb, "ERROR_LOG", hyperlink_style, rows = i + 1L, cols = link_col, stack = TRUE
      )
    }
  }

  styles <- list(
    ERROR = openxlsx::createStyle(fgFill = "#F4CCCC", fontColour = "#9C0006", border = c("top", "bottom", "left", "right"), borderColour = "#C00000"),
    WARNING = openxlsx::createStyle(fgFill = "#FFF2CC", fontColour = "#7F6000", border = c("top", "bottom", "left", "right"), borderColour = "#FFC000"),
    INFO = openxlsx::createStyle(fgFill = "#D9EAF7", fontColour = "#1F4E78", border = c("top", "bottom", "left", "right"), borderColour = "#5B9BD5")
  )
  for (i in seq_len(nrow(diag))) {
    sheet <- diag$sheet[[i]]
    row <- suppressWarnings(as.integer(diag$excel_row[[i]]))
    col <- diag$column[[i]]
    if (is.na(sheet) || is.na(row) || is.na(col) || !sheet %in% names(wb)) next
    col_num <- openxlsx::col2int(col)
    openxlsx::addStyle(wb, sheet, styles[[diag$severity[[i]]]], rows = row, cols = col_num,
                       gridExpand = FALSE, stack = TRUE)
    comment_text <- paste0(
      diag$error_code[[i]], ": ", diag$message[[i]],
      if (!is.na(diag$suggestion[[i]])) paste0("\n\nCara memperbaiki: ", diag$suggestion[[i]]) else "",
      if (!is.na(diag$example[[i]])) paste0("\nContoh: ", diag$example[[i]]) else ""
    )
    openxlsx::writeComment(
      wb, sheet, col = col_num, row = row,
      comment = openxlsx::createComment(comment_text, author = "Auctus Validator", visible = FALSE)
    )
  }
  openxlsx::modifyBaseFont(wb, fontName = "Arial", fontSize = 10, fontColour = "#000000")
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  .repair_openxlsx_comment_relationships(output_path)
  normalizePath(output_path, mustWork = FALSE)
}

.default_run_root <- function(file_path, output_dir = NULL) {
  if (!is.null(output_dir)) {
    root <- path.expand(output_dir)
  } else {
    stem <- .safe_name(tools::file_path_sans_ext(basename(file_path)))
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    root <- file.path(dirname(normalizePath(file_path)), "auctus_results", paste0(stem, "_", stamp))
  }
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  normalizePath(root, mustWork = FALSE)
}

validate_auctus_data <- function(file_path = file.choose(), output_dir = NULL,
                                 write_workbook = TRUE) {
  file_path <- normalizePath(path.expand(file_path), mustWork = TRUE)
  wb <- .read_auctus_workbook(file_path)
  diag <- .validate_auctus_workbook(wb)
  run_root <- .default_run_root(file_path, output_dir)
  validation_dir <- file.path(run_root, "00_validation")
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(.user_diagnostics(diag), file.path(validation_dir, "error_log.csv"),
                   row.names = FALSE, na = "")
  checked_path <- NA_character_
  if (write_workbook && tolower(tools::file_ext(file_path)) == "xlsx" &&
      requireNamespace("openxlsx", quietly = TRUE)) {
    checked_path <- .write_validation_workbook(
      file_path, diag, file.path(validation_dir, "validated_input.xlsx")
    )
  }
  result <- structure(list(
    valid = !any(diag$severity == "ERROR"),
    diagnostics = .user_diagnostics(diag),
    workbook = wb,
    source_file = file_path,
    run_root = run_root,
    checked_workbook = checked_path,
    error_log = file.path(validation_dir, "error_log.csv"),
    error_outcome_names = unique(diag$outcome_name[diag$severity == "ERROR" & !is.na(diag$outcome_name)])
  ), class = "auctus_validation")
  result
}

print.auctus_validation <- function(x, ...) {
  summary <- .validation_summary(x$diagnostics)
  cat("\nAUCTUS VALIDATION\n")
  cat("File   :", x$source_file, "\n")
  cat("Status :", if (x$valid) "VALID" else "INVALID", "\n\n")
  print(summary, row.names = FALSE)
  if (!is.na(x$checked_workbook)) cat("\nWorkbook koreksi:", x$checked_workbook, "\n")
  invisible(x)
}

.legacy_value <- function(dat, names, default = NA) {
  hit <- intersect(names, names(dat))
  if (length(hit)) dat[[hit[[1L]]]] else rep(default, nrow(dat))
}

.legacy_make_id <- function(outcome, effect = NA_character_, suffix = NA_character_) {
  outcome <- ifelse(is.na(outcome), "", outcome)
  effect <- ifelse(is.na(effect), "", effect)
  suffix <- ifelse(is.na(suffix), "", suffix)
  key <- paste(outcome, effect, suffix, sep = "_")
  tolower(.safe_name(key))
}

.detect_auctus_schema <- function(file_path) {
  .require_namespace("readxl")
  sheets <- readxl::excel_sheets(file_path)
  keys <- .clean_names(sheets)
  if (any(keys %in% c("dikotomi", "kontinyu", "diagnostik", "single_arm"))) {
    return("v1")
  }
  required <- names(.sheet_specs)
  if (!all(required %in% keys)) return("unknown")
  analysis_sheet <- sheets[[match("analyses", keys)]]
  headers <- .clean_names(names(readxl::read_excel(
    file_path, sheet = analysis_sheet, n_max = 0
  )))
  if ("analysis_id" %in% headers) {
    "v2_id"
  } else if ("outcome_name" %in% headers && "include" %in% headers) {
    "v22_label"
  } else if ("outcome_name" %in% headers) {
    "v23_label"
  } else {
    "unknown"
  }
}

.legacy_v2_specs <- list(
  analyses = c(
    "analysis_id", "include", "analysis_type", "outcome_name", "timepoint",
    "outcome_type", "effect_measure", "reference_treatment", "outcome_direction", "unit", "notes"
  ),
  study_metadata = c("analysis_id", "study_id", "study_label", "year", "study_design"),
  arm_data = c(
    "analysis_id", "study_id", "treatment", "event", "sample", "mean", "sd",
    "median", "q1", "q3", "min", "max", "include", "notes"
  ),
  effect_data = c(
    "analysis_id", "study_id", "treat1", "treat2", "effect", "ci_low", "ci_high",
    "se", "ci_level", "estimate_type", "adjustment_variables", "sample1", "sample2",
    "include", "notes"
  ),
  diagnostic_data = c(
    "analysis_id", "study_id", "tp", "fp", "fn", "tn", "threshold", "include", "notes"
  )
)

.read_legacy_v2_tables <- function(file_path) {
  .require_namespace("readxl")
  sheet_names <- readxl::excel_sheets(file_path)
  keys <- .clean_names(sheet_names)
  names(sheet_names) <- keys
  out <- list()
  for (sheet in names(.legacy_v2_specs)) {
    dat <- readxl::read_excel(file_path, sheet = sheet_names[[sheet]], col_types = "text")
    names(dat) <- .clean_names(names(dat))
    dat <- as.data.frame(dat, stringsAsFactors = FALSE)
    dat <- .ensure_columns(dat, .legacy_v2_specs[[sheet]])
    if (sheet == "arm_data" && "arm_id" %in% names(dat)) dat$arm_id <- NULL
    out[[sheet]] <- dat
  }
  out
}

.legacy_map_value <- function(value, keys, labels) {
  idx <- match(.trim_chr(value), .trim_chr(keys))
  labels[idx]
}

.convert_v2_id_tables <- function(old) {
  analyses_old <- old$analyses
  metadata_old <- old$study_metadata
  analyses_old$analysis_id <- .trim_chr(analyses_old$analysis_id)
  analyses_old$outcome_name <- .trim_chr(analyses_old$outcome_name)
  metadata_old$analysis_id <- .trim_chr(metadata_old$analysis_id)
  metadata_old$study_id <- .trim_chr(metadata_old$study_id)
  metadata_old$study_label <- .trim_chr(metadata_old$study_label)

  analyses <- .ensure_columns(analyses_old, .legacy_v2_specs$analyses)
  analyses$legacy_analysis_id <- analyses$analysis_id
  analyses$analysis_id <- NULL
  analyses <- analyses[, c(.sheet_specs$analyses, "legacy_analysis_id"), drop = FALSE]

  meta_extra <- setdiff(
    names(metadata_old),
    c("analysis_id", "study_id", "source_sheet", "source_row")
  )
  metadata_rows <- metadata_old[, meta_extra, drop = FALSE]
  metadata_rows$legacy_analysis_id <- metadata_old$analysis_id
  metadata_rows$legacy_study_id <- metadata_old$study_id
  groups <- split(
    seq_len(nrow(metadata_rows)),
    .normalize_label_key(metadata_rows$study_label), drop = TRUE
  )
  collapsed <- lapply(groups, function(idx) {
    group <- metadata_rows[idx, , drop = FALSE]
    comparison_fields <- setdiff(names(group), c("legacy_analysis_id", "legacy_study_id"))
    consistent <- all(vapply(comparison_fields, function(nm) {
      x <- if (nm == "study_label") .normalize_label_key(group[[nm]]) else .trim_chr(group[[nm]])
      length(unique(x[!is.na(x)])) <= 1L
    }, logical(1)))
    if (!consistent) return(group)
    row <- group[1L, , drop = FALSE]
    for (nm in comparison_fields) row[[nm]] <- .first_nonmissing(group[[nm]], NA_character_)
    row$legacy_analysis_id <- paste(unique(na.omit(group$legacy_analysis_id)), collapse = "; ")
    row$legacy_study_id <- paste(unique(na.omit(group$legacy_study_id)), collapse = "; ")
    row
  })
  metadata <- .rbind_fill(collapsed)

  analysis_key <- analyses_old$analysis_id
  analysis_label <- analyses_old$outcome_name
  composite_key <- paste(metadata_old$analysis_id, metadata_old$study_id, sep = "||")

  convert_data <- function(dat, spec) {
    dat <- .ensure_columns(dat, spec)
    legacy_analysis <- .trim_chr(dat$analysis_id)
    legacy_study <- .trim_chr(dat$study_id)
    outcome_name <- .legacy_map_value(legacy_analysis, analysis_key, analysis_label)
    study_label <- .legacy_map_value(
      paste(legacy_analysis, legacy_study, sep = "||"),
      composite_key, metadata_old$study_label
    )
    missing_study <- is.na(study_label) & !is.na(legacy_study)
    if (any(missing_study)) {
      unique_study <- !duplicated(metadata_old$study_id) &
        !duplicated(metadata_old$study_id, fromLast = TRUE)
      study_label[missing_study] <- .legacy_map_value(
        legacy_study[missing_study], metadata_old$study_id[unique_study],
        metadata_old$study_label[unique_study]
      )
    }
    dat$analysis_id <- NULL
    dat$study_id <- NULL
    dat$outcome_name <- outcome_name
    dat$study_label <- study_label
    dat$legacy_analysis_id <- legacy_analysis
    dat$legacy_study_id <- legacy_study
    dat
  }

  arm <- convert_data(old$arm_data, .legacy_v2_specs$arm_data)
  effect <- convert_data(old$effect_data, .legacy_v2_specs$effect_data)
  diagnostic <- convert_data(old$diagnostic_data, .legacy_v2_specs$diagnostic_data)
  arm <- arm[, c(.sheet_specs$arm_data, "legacy_analysis_id", "legacy_study_id"), drop = FALSE]
  effect <- effect[, c(.sheet_specs$effect_data, "legacy_analysis_id", "legacy_study_id"), drop = FALSE]
  diagnostic <- diagnostic[, c(.sheet_specs$diagnostic_data, "legacy_analysis_id", "legacy_study_id"), drop = FALSE]

  list(
    analyses = analyses,
    study_metadata = metadata,
    arm_data = arm,
    effect_data = effect,
    diagnostic_data = diagnostic
  )
}

.convert_v1_tables <- function(file_path) {
  .require_namespace("readxl")
  sheets <- readxl::excel_sheets(file_path)
  keys <- .clean_names(sheets)
  names(sheets) <- keys
  recognized <- intersect(keys, c("dikotomi", "kontinyu", "diagnostik", "single_arm"))
  if (!length(recognized)) stop("Workbook bukan template V1 yang dikenali.", call. = FALSE)

  out <- lapply(.sheet_specs, function(cols) .empty_df(cols))
  migration_rows <- list()
  add_metadata <- function(label, dat, i) {
    base <- data.frame(
      study_label = label,
      year = .legacy_value(dat, c("year", "tahun"))[[i]],
      study_design = .legacy_value(dat, c("study_design", "design"))[[i]],
      stringsAsFactors = FALSE
    )
    extra_cols <- grep("^(subgroup_|moderator_num_|moderator_cat_)", names(dat), value = TRUE)
    for (nm in extra_cols) base[[nm]] <- dat[[nm]][[i]]
    base
  }

  for (key in recognized) {
    dat <- readxl::read_excel(file_path, sheet = sheets[[key]], col_types = "text")
    names(dat) <- .clean_names(names(dat))
    dat <- as.data.frame(dat, stringsAsFactors = FALSE)
    keep <- if ("include" %in% names(dat)) .is_yes(dat$include) else rep(TRUE, nrow(dat))
    migration_rows[[length(migration_rows) + 1L]] <- .migration_log_row(
      "v1", key, sum(!keep),
      "Baris include=FALSE dibuang saat migrasi ke V2.3."
    )
    dat <- dat[keep, , drop = FALSE]
    if ("include" %in% names(dat)) dat$include <- NULL
    if (!nrow(dat)) next
    study <- .trim_chr(.legacy_value(dat, c("study", "studlab", "author")))
    outcome <- .trim_chr(.legacy_value(dat, c("outcome_name", "outcome")))
    effect <- toupper(.trim_chr(.legacy_value(dat, c("effect_measure", "sm"))))
    timepoint <- .trim_chr(.legacy_value(dat, c("timepoint", "followup"), NA_character_))

    if (key %in% c("dikotomi", "kontinyu")) {
      otype <- if (key == "dikotomi") "binary" else "continuous"
      group <- .legacy_make_id(outcome, effect, timepoint)
      treat1 <- .trim_chr(.legacy_value(dat, c("treat1", "treatment1")))
      treat2 <- .trim_chr(.legacy_value(dat, c("treat2", "treatment2", "control")))
      pairs <- ifelse(treat1 < treat2, paste(treat1, treat2, sep = "||"), paste(treat2, treat1, sep = "||"))
      n_pairs <- tapply(pairs, group, function(x) length(unique(x)))
      for (grp in unique(group[!is.na(group)])) {
        idx <- which(group == grp)
        atype <- if (unname(n_pairs[[grp]]) > 1L) "nma" else "pairwise_ma"
        ref <- .first_nonmissing(.legacy_value(dat[idx, , drop = FALSE], c("nma_reference")), treat2[idx[[1L]]])
        old_direction <- tolower(.first_nonmissing(
          .legacy_value(dat[idx, , drop = FALSE], c("nma_small_values")), "desirable"
        ))
        direction <- if (old_direction == "undesirable") "higher_better" else "lower_better"
        out$analyses <- rbind(out$analyses, data.frame(
          outcome_name = outcome[idx[[1L]]], analysis_type = atype,
          timepoint = timepoint[idx[[1L]]], outcome_type = otype,
          effect_measure = effect[idx[[1L]]], reference_treatment = ref,
          outcome_direction = direction, unit = NA,
          notes = "Dikonversi dari V1. Periksa reference treatment dan arah outcome.",
          stringsAsFactors = FALSE
        ))
      }
      for (i in seq_len(nrow(dat))) {
        if (is.na(study[[i]]) || is.na(outcome[[i]])) next
        out$study_metadata <- .rbind_fill(list(out$study_metadata, add_metadata(study[[i]], dat, i)))
        e1 <- .legacy_value(dat, c("event1", "event_e", "event_treatment"))[[i]]
        n1 <- .legacy_value(dat, c("sample1", "n1", "n_e"))[[i]]
        e2 <- .legacy_value(dat, c("event2", "event_c", "event_control"))[[i]]
        n2 <- .legacy_value(dat, c("sample2", "n2", "n_c"))[[i]]
        m1 <- .legacy_value(dat, c("mean1", "mean_e"))[[i]]
        sd1 <- .legacy_value(dat, c("sd1", "sd_e"))[[i]]
        m2 <- .legacy_value(dat, c("mean2", "mean_c"))[[i]]
        sd2 <- .legacy_value(dat, c("sd2", "sd_c"))[[i]]
        has_raw <- if (otype == "binary") {
          all(!is.na(.trim_chr(c(e1, n1, e2, n2))))
        } else {
          all(!is.na(.trim_chr(c(m1, sd1, n1, m2, sd2, n2))))
        }
        if (has_raw) {
          out$arm_data <- rbind(out$arm_data, data.frame(
            outcome_name = rep(outcome[[i]], 2), study_label = rep(study[[i]], 2),
            treatment = c(treat1[[i]], treat2[[i]]),
            event = if (otype == "binary") c(e1, e2) else NA,
            sample = c(n1, n2), mean = if (otype == "continuous") c(m1, m2) else NA,
            sd = if (otype == "continuous") c(sd1, sd2) else NA,
            median = NA, q1 = NA, q3 = NA, min = NA, max = NA,
            notes = "Dikonversi dari V1", stringsAsFactors = FALSE
          ))
        } else {
          val <- if (otype == "binary") {
            .legacy_value(dat, c("yi", "effect", "or", "rr", "hr"))[[i]]
          } else {
            .legacy_value(dat, c("md", "yi", "effect", "smd"))[[i]]
          }
          out$effect_data <- rbind(out$effect_data, data.frame(
            outcome_name = outcome[[i]], study_label = study[[i]],
            treat1 = treat1[[i]], treat2 = treat2[[i]], effect = val,
            ci_low = .legacy_value(dat, c("lowci", "ci_low", "lower"))[[i]],
            ci_high = .legacy_value(dat, c("highci", "ci_high", "upper"))[[i]],
            se = .legacy_value(dat, c("se", "sete"))[[i]], ci_level = 95,
            estimate_type = "crude", adjustment_variables = NA,
            sample1 = n1, sample2 = n2,
            notes = "Dikonversi dari V1. Periksa skala effect.", stringsAsFactors = FALSE
          ))
        }
      }
    } else if (key == "diagnostik") {
      group <- .legacy_make_id(outcome, "diagnostic", timepoint)
      for (grp in unique(group[!is.na(group)])) {
        i <- which(group == grp)[[1L]]
        out$analyses <- rbind(out$analyses, data.frame(
          outcome_name = outcome[[i]], analysis_type = "diagnostic_ma",
          timepoint = timepoint[[i]], outcome_type = "diagnostic", effect_measure = NA,
          reference_treatment = NA, outcome_direction = "neutral", unit = NA,
          notes = "Dikonversi dari V1", stringsAsFactors = FALSE
        ))
      }
      for (i in seq_len(nrow(dat))) {
        out$study_metadata <- .rbind_fill(list(out$study_metadata, add_metadata(study[[i]], dat, i)))
        out$diagnostic_data <- rbind(out$diagnostic_data, data.frame(
          outcome_name = outcome[[i]], study_label = study[[i]],
          tp = .legacy_value(dat, "tp")[[i]], fp = .legacy_value(dat, "fp")[[i]],
          fn = .legacy_value(dat, "fn")[[i]], tn = .legacy_value(dat, "tn")[[i]],
          threshold = .legacy_value(dat, c("threshold", "cutoff"))[[i]],
          notes = "Dikonversi dari V1", stringsAsFactors = FALSE
        ))
      }
    } else if (key == "single_arm") {
      raw_type <- tolower(.trim_chr(.legacy_value(dat, c("analysis_type", "outcome_type"), "proportion")))
      raw_type[!raw_type %in% c("proportion", "mean")] <- "proportion"
      group <- .legacy_make_id(outcome, raw_type, timepoint)
      for (grp in unique(group[!is.na(group)])) {
        i <- which(group == grp)[[1L]]
        out$analyses <- rbind(out$analyses, data.frame(
          outcome_name = outcome[[i]], analysis_type = "proportion_ma",
          timepoint = timepoint[[i]], outcome_type = raw_type[[i]], effect_measure = NA,
          reference_treatment = NA, outcome_direction = "neutral",
          unit = if (raw_type[[i]] == "proportion") "%" else .legacy_value(dat, "unit")[[i]],
          notes = "Dikonversi dari V1", stringsAsFactors = FALSE
        ))
      }
      for (i in seq_len(nrow(dat))) {
        out$study_metadata <- .rbind_fill(list(out$study_metadata, add_metadata(study[[i]], dat, i)))
        out$arm_data <- rbind(out$arm_data, data.frame(
          outcome_name = outcome[[i]], study_label = study[[i]], treatment = "Population",
          event = .legacy_value(dat, c("event", "events"))[[i]],
          sample = .legacy_value(dat, c("sample", "n"))[[i]],
          mean = .legacy_value(dat, "mean")[[i]], sd = .legacy_value(dat, "sd")[[i]],
          median = .legacy_value(dat, "median")[[i]], q1 = .legacy_value(dat, "q1")[[i]],
          q3 = .legacy_value(dat, "q3")[[i]], min = .legacy_value(dat, "min")[[i]],
          max = .legacy_value(dat, "max")[[i]],
          notes = "Dikonversi dari V1", stringsAsFactors = FALSE
        ))
      }
    }
  }

  out$analyses <- unique(out$analyses)
  if (nrow(out$study_metadata)) out$study_metadata <- unique(out$study_metadata)
  if (nrow(out$arm_data)) {
    key <- paste(
      .normalize_label_key(out$arm_data$outcome_name),
      .normalize_label_key(out$arm_data$study_label),
      .treatment_key(out$arm_data$treatment), sep = "||"
    )
    keep <- logical(nrow(out$arm_data))
    for (idx in split(seq_len(nrow(out$arm_data)), key)) {
      if (length(idx) == 1L) {
        keep[idx] <- TRUE
      } else {
        fields <- c("event", "sample", "mean", "sd", "median", "q1", "q3", "min", "max")
        consistent <- all(vapply(fields, function(field) {
          x <- unique(.trim_chr(out$arm_data[[field]][idx]))
          length(x[!is.na(x)]) <= 1L
        }, logical(1)))
        if (consistent) keep[idx[[1L]]] <- TRUE else keep[idx] <- TRUE
      }
    }
    out$arm_data <- out$arm_data[keep, , drop = FALSE]
  }
  attr(out, "migration_log") <- .rbind_fill(migration_rows)
  out
}

.read_legacy_label_tables <- function(file_path) {
  wb <- .read_auctus_workbook(file_path)
  if (length(wb$missing_sheets)) {
    stop("Workbook label lama tidak memiliki seluruh sheet wajib.", call. = FALSE)
  }
  setNames(lapply(names(.sheet_specs), function(sheet) wb[[sheet]]), names(.sheet_specs))
}

.migration_log_row <- function(source_schema, sheet, removed_rows, reason) {
  data.frame(
    source_schema = source_schema,
    sheet = sheet,
    removed_rows = as.integer(removed_rows),
    reason = reason,
    stringsAsFactors = FALSE
  )
}

.filter_legacy_includes <- function(data, source_schema) {
  analyses <- data$analyses
  analysis_key_column <- if ("analysis_id" %in% names(analyses)) "analysis_id" else "outcome_name"
  analysis_key <- if (analysis_key_column == "analysis_id") {
    .trim_chr(analyses[[analysis_key_column]])
  } else {
    .normalize_label_key(analyses[[analysis_key_column]])
  }
  analysis_keep <- if ("include" %in% names(analyses)) .is_yes(analyses$include) else rep(TRUE, nrow(analyses))
  retained_keys <- unique(analysis_key[analysis_keep & !is.na(analysis_key)])
  logs <- list(.migration_log_row(
    source_schema, "analyses", sum(!analysis_keep),
    "Baris include=FALSE dibuang saat migrasi ke V2.3."
  ))
  analyses <- analyses[analysis_keep, , drop = FALSE]
  analyses$include <- NULL
  data$analyses <- analyses

  for (sheet in c("arm_data", "effect_data", "diagnostic_data")) {
    dat <- data[[sheet]]
    row_keep <- if ("include" %in% names(dat)) .is_yes(dat$include) else rep(TRUE, nrow(dat))
    row_key <- if (analysis_key_column == "analysis_id") {
      .trim_chr(dat$analysis_id)
    } else {
      .normalize_label_key(dat$outcome_name)
    }
    parent_keep <- !is.na(row_key) & row_key %in% retained_keys
    logs[[length(logs) + 1L]] <- .migration_log_row(
      source_schema, sheet, sum(!row_keep),
      "Baris include=FALSE dibuang saat migrasi ke V2.3."
    )
    logs[[length(logs) + 1L]] <- .migration_log_row(
      source_schema, sheet, sum(row_keep & !parent_keep),
      "Baris untuk outcome yang tidak dipertahankan dibuang."
    )
    dat <- dat[row_keep & parent_keep, , drop = FALSE]
    dat$include <- NULL
    data[[sheet]] <- dat
  }

  if (analysis_key_column == "analysis_id" && "analysis_id" %in% names(data$study_metadata)) {
    meta_key <- .trim_chr(data$study_metadata$analysis_id)
    meta_keep <- !is.na(meta_key) & meta_key %in% retained_keys
    logs[[length(logs) + 1L]] <- .migration_log_row(
      source_schema, "study_metadata", sum(!meta_keep),
      "Metadata untuk analysis yang tidak dipertahankan dibuang."
    )
    data$study_metadata <- data$study_metadata[meta_keep, , drop = FALSE]
  }
  list(data = data, log = .rbind_fill(logs))
}

.prune_converted_metadata <- function(data, source_schema, migration_log) {
  used <- unique(c(
    .normalize_label_key(data$arm_data$study_label),
    .normalize_label_key(data$effect_data$study_label),
    .normalize_label_key(data$diagnostic_data$study_label)
  ))
  used <- used[!is.na(used)]
  metadata_key <- .normalize_label_key(data$study_metadata$study_label)
  keep <- !is.na(metadata_key) & metadata_key %in% used
  removed <- sum(!keep)
  data$study_metadata <- data$study_metadata[keep, , drop = FALSE]
  migration_log <- .rbind_fill(list(
    migration_log,
    .migration_log_row(
      source_schema, "study_metadata", removed,
      "Metadata studi yang tidak dirujuk oleh data aktif dibuang."
    )
  ))
  for (sheet in names(.sheet_specs)) {
    dat <- data[[sheet]]
    drop <- intersect(
      c("include", "include_flag", "source_sheet", "source_row", "outcome_key",
        "study_key", "analysis_id", "study_id", "canonical_outcome_name",
        "canonical_study_label"),
      names(dat)
    )
    if (source_schema == "v2_id") {
      drop <- setdiff(drop, c("analysis_id", "study_id"))
    }
    if (length(drop)) dat[drop] <- NULL
    dat <- .ensure_columns(dat, .sheet_specs[[sheet]])
    data[[sheet]] <- dat[, c(.sheet_specs[[sheet]], setdiff(names(dat), .sheet_specs[[sheet]])), drop = FALSE]
  }
  list(data = data, log = migration_log)
}

convert_legacy_workbook <- function(file_path = file.choose(), output_path = NULL) {
  .require_namespace("readxl")
  .require_namespace("openxlsx")
  file_path <- normalizePath(path.expand(file_path), mustWork = TRUE)
  schema <- .detect_auctus_schema(file_path)
  if (identical(schema, "v23_label")) {
    stop("Workbook sudah menggunakan schema 2.3 berbasis label dan tidak perlu dikonversi.", call. = FALSE)
  }
  if (!schema %in% c("v1", "v2_id", "v22_label")) {
    stop("Workbook bukan template V1, V2, atau V2.2 yang dikenali.", call. = FALSE)
  }
  if (is.null(output_path)) {
    output_path <- file.path(
      dirname(file_path),
      paste0(tools::file_path_sans_ext(basename(file_path)), "_v23_converted.xlsx")
    )
  }
  migration_log <- .migration_log_row(
    schema, "ALL", 0L, "Migrasi workbook ke schema V2.3 dimulai."
  )
  converted <- if (schema == "v1") {
    legacy <- .convert_v1_tables(file_path)
    migration_log <- .rbind_fill(list(
      migration_log, attr(legacy, "migration_log") %||% data.frame()
    ))
    attr(legacy, "migration_log") <- NULL
    legacy
  } else if (schema == "v2_id") {
    filtered <- .filter_legacy_includes(.read_legacy_v2_tables(file_path), schema)
    migration_log <- .rbind_fill(list(migration_log, filtered$log))
    .convert_v2_id_tables(filtered$data)
  } else {
    filtered <- .filter_legacy_includes(.read_legacy_label_tables(file_path), schema)
    migration_log <- .rbind_fill(list(migration_log, filtered$log))
    filtered$data
  }
  pruned <- .prune_converted_metadata(converted, schema, migration_log)
  path <- .write_auctus_workbook(
    pruned$data, output_path, write_instructions = TRUE,
    migration_log = pruned$log
  )
  message("Workbook ", toupper(schema), " dikonversi ke schema 2.3: ", path)
  invisible(path)
}

# =============================================================================
# CANONICAL DATA LAYER
# =============================================================================

.normalize_workbook_data <- function(wb) {
  wb <- .augment_internal_ids(wb)
  analyses <- wb$analyses
  analyses$analysis_id <- .trim_chr(analyses$analysis_id)
  analyses$analysis_type <- tolower(.trim_chr(analyses$analysis_type))
  analyses$outcome_name <- .trim_chr(analyses$outcome_name)
  analyses$timepoint <- .trim_chr(analyses$timepoint)
  analyses$outcome_type <- tolower(.trim_chr(analyses$outcome_type))
  analyses$effect_measure <- toupper(.trim_chr(analyses$effect_measure))
  analyses$reference_treatment <- .trim_chr(analyses$reference_treatment)
  analyses$outcome_direction <- tolower(.trim_chr(analyses$outcome_direction))
  analyses$unit <- .trim_chr(analyses$unit)

  metadata <- wb$study_metadata
  metadata$study_id <- .trim_chr(metadata$study_id)
  metadata$study_label <- .trim_chr(metadata$study_label)

  arms <- wb$arm_data
  arms$analysis_id <- .trim_chr(arms$analysis_id)
  arms$study_id <- .trim_chr(arms$study_id)
  arms$outcome_name <- .trim_chr(arms$canonical_outcome_name)
  arms$study_label <- .trim_chr(arms$canonical_study_label)
  arms$treatment <- .trim_chr(arms$treatment)
  if ("arm_id" %in% names(arms)) arms$arm_id <- NULL
  for (nm in c("event", "sample", "mean", "sd", "median", "q1", "q3", "min", "max")) {
    arms[[nm]] <- .as_num(arms[[nm]])
  }

  effects <- wb$effect_data
  effects$analysis_id <- .trim_chr(effects$analysis_id)
  effects$study_id <- .trim_chr(effects$study_id)
  effects$outcome_name <- .trim_chr(effects$canonical_outcome_name)
  effects$study_label <- .trim_chr(effects$canonical_study_label)
  effects$treat1 <- .trim_chr(effects$treat1)
  effects$treat2 <- .trim_chr(effects$treat2)
  effects$estimate_type <- tolower(.trim_chr(effects$estimate_type))
  for (nm in c("effect", "ci_low", "ci_high", "se", "ci_level", "sample1", "sample2")) {
    effects[[nm]] <- .as_num(effects[[nm]])
  }
  effects$ci_level[is.na(effects$ci_level)] <- 95

  dta <- wb$diagnostic_data
  dta$analysis_id <- .trim_chr(dta$analysis_id)
  dta$study_id <- .trim_chr(dta$study_id)
  dta$outcome_name <- .trim_chr(dta$canonical_outcome_name)
  dta$study_label <- .trim_chr(dta$canonical_study_label)
  for (nm in c("tp", "fp", "fn", "tn")) dta[[nm]] <- .as_num(dta[[nm]])

  list(analyses = analyses, study_metadata = metadata, arm_data = arms,
       effect_data = effects, diagnostic_data = dta)
}

.mean_sd_from_arm <- function(row) {
  mean_value <- .as_num(row$mean)
  sd_value <- .as_num(row$sd)
  converted <- FALSE
  conversion_method <- NA_character_
  if (!is.na(mean_value) && !is.na(sd_value)) {
    return(list(mean = mean_value, sd = sd_value, converted = FALSE,
                conversion_method = NA_character_))
  }
  if (is.na(.as_num(row$median)) || is.na(.as_num(row$sample))) {
    return(list(mean = NA_real_, sd = NA_real_, converted = FALSE,
                conversion_method = NA_character_))
  }
  .require_namespace("estmeansd")
  fit <- tryCatch(
    estmeansd::qe.mean.sd(
      min.val = .as_num(row$min), q1.val = .as_num(row$q1),
      med.val = .as_num(row$median), q3.val = .as_num(row$q3),
      max.val = .as_num(row$max), n = .as_num(row$sample)
    ),
    error = function(e) NULL
  )
  if (!is.null(fit)) {
    mean_value <- fit$est.mean
    sd_value <- fit$est.sd
    converted <- TRUE
    conversion_method <- "Quantile Estimation (estmeansd::qe.mean.sd)"
  }
  list(mean = mean_value, sd = sd_value, converted = converted,
       conversion_method = conversion_method)
}

.binary_pair_effect <- function(e1, n1, e2, n2, sm) {
  non1 <- n1 - e1
  non2 <- n2 - e2
  if (sm %in% c("OR", "RR") && e1 == 0 && e2 == 0) {
    return(list(TE = NA_real_, seTE = NA_real_, method = "double_zero_excluded",
                excluded = TRUE))
  }
  zero_cell <- any(c(e1, non1, e2, non2) == 0)
  if (zero_cell) {
    .require_namespace("meta")
    fit <- suppressWarnings(meta::metabin(
      event.e = e1, n.e = n1, event.c = e2, n.c = n2,
      sm = sm, method = "Inverse", common = TRUE, random = FALSE,
      incr = "TACC", method.incr = "only0", allstudies = TRUE
    ))
    return(list(
      TE = as.numeric(fit$TE[[1L]]), seTE = as.numeric(fit$seTE[[1L]]),
      method = "TACC for mixed/generic canonicalization", excluded = FALSE
    ))
  }
  if (sm == "OR") {
    te <- log((e1 / non1) / (e2 / non2))
    se <- sqrt(1 / e1 + 1 / non1 + 1 / e2 + 1 / non2)
  } else if (sm == "RR") {
    te <- log((e1 / n1) / (e2 / n2))
    se <- sqrt(1 / e1 - 1 / n1 + 1 / e2 - 1 / n2)
  } else {
    return(list(TE = NA_real_, seTE = NA_real_, method = "unsupported_raw_hr",
                excluded = TRUE))
  }
  list(TE = te, seTE = se, method = "raw 2x2", excluded = FALSE)
}

.continuous_pair_effect <- function(m1, sd1, n1, m2, sd2, n2, sm) {
  if (sm == "MD") {
    te <- m1 - m2
    se <- sqrt(sd1^2 / n1 + sd2^2 / n2)
    return(list(TE = te, seTE = se, method = "raw mean difference"))
  }
  df <- n1 + n2 - 2
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / df)
  d <- (m1 - m2) / pooled_sd
  correction <- 1 - 3 / (4 * df - 1)
  g <- correction * d
  variance <- (n1 + n2) / (n1 * n2) + g^2 / (2 * df)
  list(TE = g, seTE = sqrt(variance), method = "Hedges g")
}

.raw_pairwise_contrasts <- function(arms, analysis) {
  columns <- c(
    "analysis_id", "study_id", "treat1", "treat2", "TE", "seTE", "source_type",
    "event1", "sample1", "event2", "sample2", "mean1", "sd1", "mean2", "sd2",
    "converted", "conversion_method", "method_note", "source_sheet", "source_row"
  )
  out <- .empty_df(columns)
  arms <- arms[arms$analysis_id == analysis$analysis_id, , drop = FALSE]
  if (!nrow(arms)) return(out)
  for (sid in unique(arms$study_id)) {
    study_arms <- arms[arms$study_id == sid, , drop = FALSE]
    if (nrow(study_arms) < 2L) next
    combos <- utils::combn(seq_len(nrow(study_arms)), 2L)
    for (j in seq_len(ncol(combos))) {
      a <- study_arms[combos[1L, j], , drop = FALSE]
      b <- study_arms[combos[2L, j], , drop = FALSE]
      if (identical(b$treatment[[1L]], analysis$reference_treatment) &&
          !identical(a$treatment[[1L]], analysis$reference_treatment)) {
        arm1 <- a; arm2 <- b
      } else if (identical(a$treatment[[1L]], analysis$reference_treatment) &&
                 !identical(b$treatment[[1L]], analysis$reference_treatment)) {
        arm1 <- b; arm2 <- a
      } else {
        ordered <- order(c(a$treatment[[1L]], b$treatment[[1L]]))
        arm1 <- list(a, b)[[ordered[[1L]]]]
        arm2 <- list(a, b)[[ordered[[2L]]]]
      }

      converted <- FALSE
      conversion_method <- NA_character_
      if (analysis$outcome_type == "binary") {
        eff <- .binary_pair_effect(
          arm1$event[[1L]], arm1$sample[[1L]], arm2$event[[1L]], arm2$sample[[1L]],
          analysis$effect_measure
        )
        m1 <- sd1 <- m2 <- sd2 <- NA_real_
      } else {
        ms1 <- .mean_sd_from_arm(arm1)
        ms2 <- .mean_sd_from_arm(arm2)
        converted <- isTRUE(ms1$converted) || isTRUE(ms2$converted)
        conversion_method <- paste(unique(na.omit(c(ms1$conversion_method, ms2$conversion_method))), collapse = "; ")
        eff <- .continuous_pair_effect(
          ms1$mean, ms1$sd, arm1$sample[[1L]],
          ms2$mean, ms2$sd, arm2$sample[[1L]], analysis$effect_measure
        )
        m1 <- ms1$mean; sd1 <- ms1$sd; m2 <- ms2$mean; sd2 <- ms2$sd
      }
      row <- data.frame(
        analysis_id = analysis$analysis_id, study_id = sid,
        treat1 = arm1$treatment[[1L]], treat2 = arm2$treatment[[1L]],
        TE = eff$TE, seTE = eff$seTE, source_type = "raw_derived",
        event1 = arm1$event[[1L]], sample1 = arm1$sample[[1L]],
        event2 = arm2$event[[1L]], sample2 = arm2$sample[[1L]],
        mean1 = m1, sd1 = sd1, mean2 = m2, sd2 = sd2,
        converted = converted, conversion_method = conversion_method,
        method_note = eff$method,
        source_sheet = arm1$source_sheet[[1L]], source_row = arm1$source_row[[1L]],
        stringsAsFactors = FALSE
      )
      out <- rbind(out, row)
    }
  }
  out
}

.reported_contrasts <- function(effects, analysis) {
  columns <- c(
    "analysis_id", "study_id", "treat1", "treat2", "TE", "seTE", "source_type",
    "event1", "sample1", "event2", "sample2", "mean1", "sd1", "mean2", "sd2",
    "converted", "conversion_method", "method_note", "source_sheet", "source_row"
  )
  out <- .empty_df(columns)
  dat <- effects[effects$analysis_id == analysis$analysis_id, , drop = FALSE]
  if (!nrow(dat)) return(out)
  ratio <- analysis$effect_measure %in% c("OR", "RR", "HR")
  for (i in seq_len(nrow(dat))) {
    z <- stats::qnorm(1 - (1 - dat$ci_level[[i]] / 100) / 2)
    te <- if (ratio) log(dat$effect[[i]]) else dat$effect[[i]]
    se <- dat$se[[i]]
    if (is.na(se)) {
      se <- if (ratio) {
        (log(dat$ci_high[[i]]) - log(dat$ci_low[[i]])) / (2 * z)
      } else {
        (dat$ci_high[[i]] - dat$ci_low[[i]]) / (2 * z)
      }
    }
    out <- rbind(out, data.frame(
      analysis_id = analysis$analysis_id, study_id = dat$study_id[[i]],
      treat1 = dat$treat1[[i]], treat2 = dat$treat2[[i]],
      TE = te, seTE = se, source_type = dat$estimate_type[[i]],
      event1 = NA, sample1 = dat$sample1[[i]], event2 = NA, sample2 = dat$sample2[[i]],
      mean1 = NA, sd1 = NA, mean2 = NA, sd2 = NA,
      converted = FALSE, conversion_method = NA,
      method_note = paste("reported", dat$estimate_type[[i]]),
      source_sheet = dat$source_sheet[[i]], source_row = dat$source_row[[i]],
      stringsAsFactors = FALSE
    ))
  }
  out
}

.select_primary_sources <- function(raw, reported) {
  all <- rbind(raw, reported)
  if (!nrow(all)) return(all)
  selected <- logical(nrow(all))
  for (sid in unique(all$study_id)) {
    idx <- which(all$study_id == sid)
    types <- all$source_type[idx]
    priority <- if ("adjusted" %in% types) "adjusted" else if ("crude" %in% types) "crude" else "raw_derived"
    selected[idx[types == priority]] <- TRUE
  }
  out <- all[selected, , drop = FALSE]
  rownames(out) <- NULL
  out
}

.orient_pairwise_reference <- function(dat, reference) {
  if (!nrow(dat) || is.na(reference)) return(dat)
  reverse <- dat$treat1 == reference & dat$treat2 != reference
  reverse[is.na(reverse)] <- FALSE
  if (!any(reverse)) return(dat)
  old_treat1 <- dat$treat1[reverse]
  dat$treat1[reverse] <- dat$treat2[reverse]
  dat$treat2[reverse] <- old_treat1
  dat$TE[reverse] <- -dat$TE[reverse]
  swap_pairs <- list(
    c("event1", "event2"), c("sample1", "sample2"),
    c("mean1", "mean2"), c("sd1", "sd2")
  )
  for (pair in swap_pairs) {
    old <- dat[[pair[[1L]]]][reverse]
    dat[[pair[[1L]]]][reverse] <- dat[[pair[[2L]]]][reverse]
    dat[[pair[[2L]]]][reverse] <- old
  }
  dat$method_note[reverse] <- paste(dat$method_note[reverse], "orientation reversed to reference")
  dat
}

.decorate_canonical <- function(canonical, metadata, analysis) {
  if (!nrow(canonical)) return(canonical)
  meta_a <- metadata
  extra_cols <- setdiff(
    names(meta_a),
    c("analysis_id", "study_key", "source_sheet", "source_row")
  )
  meta_a <- meta_a[, c("study_id", extra_cols[extra_cols != "study_id"]), drop = FALSE]
  canonical$._order <- seq_len(nrow(canonical))
  canonical <- merge(canonical, meta_a, by = "study_id", all.x = TRUE, sort = FALSE)
  canonical <- canonical[order(canonical$._order), , drop = FALSE]
  canonical$._order <- NULL
  canonical$study_label[is.na(canonical$study_label)] <- canonical$study_id[is.na(canonical$study_label)]
  canonical$analysis_id <- analysis$analysis_id
  canonical$outcome_name <- analysis$outcome_name
  canonical$effect_measure <- analysis$effect_measure
  canonical$lower <- canonical$TE - stats::qnorm(0.975) * canonical$seTE
  canonical$upper <- canonical$TE + stats::qnorm(0.975) * canonical$seTE
  canonical
}

.canonicalize_analysis <- function(data, analysis) {
  raw <- .raw_pairwise_contrasts(data$arm_data, analysis)
  reported <- .reported_contrasts(data$effect_data, analysis)
  primary <- .select_primary_sources(raw, reported)
  if (analysis$analysis_type == "pairwise_ma") {
    primary <- .orient_pairwise_reference(primary, analysis$reference_treatment)
    raw <- .orient_pairwise_reference(raw, analysis$reference_treatment)
    reported <- .orient_pairwise_reference(reported, analysis$reference_treatment)
  }
  primary <- primary[is.finite(primary$TE) & is.finite(primary$seTE) & primary$seTE > 0, , drop = FALSE]
  primary <- .decorate_canonical(primary, data$study_metadata, analysis)
  all_sources <- .decorate_canonical(rbind(raw, reported), data$study_metadata, analysis)
  list(primary = primary, all_sources = all_sources, raw = raw, reported = reported)
}

.raw_binary_pair_data <- function(arms, analysis, selected_studies = NULL) {
  dat <- arms[arms$analysis_id == analysis$analysis_id, , drop = FALSE]
  if (!is.null(selected_studies)) dat <- dat[dat$study_id %in% selected_studies, , drop = FALSE]
  out <- data.frame()
  for (sid in unique(dat$study_id)) {
    s <- dat[dat$study_id == sid, , drop = FALSE]
    if (nrow(s) < 2L) next
    combos <- utils::combn(seq_len(nrow(s)), 2L)
    for (j in seq_len(ncol(combos))) {
      a <- s[combos[1L, j], , drop = FALSE]
      b <- s[combos[2L, j], , drop = FALSE]
      if (b$treatment[[1L]] == analysis$reference_treatment) {
        e <- a; c <- b
      } else if (a$treatment[[1L]] == analysis$reference_treatment) {
        e <- b; c <- a
      } else {
        e <- a; c <- b
      }
      out <- rbind(out, data.frame(
        study_id = sid, treat1 = e$treatment[[1L]], treat2 = c$treatment[[1L]],
        event1 = e$event[[1L]], sample1 = e$sample[[1L]],
        event2 = c$event[[1L]], sample2 = c$sample[[1L]],
        stringsAsFactors = FALSE
      ))
    }
  }
  out
}

# =============================================================================
# MODELS, SENSITIVITY ANALYSES, AND PUBLICATION-READY PLOTS
# =============================================================================

.fit_metagen <- function(dat, sm, prediction = nrow(dat) >= 5L, subgroup = NULL) {
  .require_namespace("meta")
  args <- list(
    TE = dat$TE, seTE = dat$seTE, studlab = dat$study_label,
    data = dat, sm = sm, common = TRUE, random = TRUE,
    method.tau = "REML", method.random.ci = "HK", adhoc.hakn.ci = "se",
    prediction = prediction, level = 0.95, level.predict = 0.95
  )
  if (!is.null(subgroup)) args$subgroup <- subgroup
  do.call(meta::metagen, args)
}

.meta_summary_row <- function(model, analysis, method_name, backtransform = NULL) {
  if (is.null(backtransform)) {
    backtransform <- if (analysis$effect_measure %in% c("OR", "RR", "HR")) exp else identity
  }
  pred_low <- suppressWarnings(as.numeric(model$lower.predict %||% NA_real_))
  pred_high <- suppressWarnings(as.numeric(model$upper.predict %||% NA_real_))
  data.frame(
    analysis_id = analysis$analysis_id,
    outcome_name = analysis$outcome_name,
    timepoint = analysis$timepoint,
    analysis_type = analysis$analysis_type,
    outcome_type = analysis$outcome_type,
    effect_measure = analysis$effect_measure %||% toupper(analysis$outcome_type),
    method = method_name,
    k = as.integer(model$k %||% length(model$TE)),
    estimate = backtransform(as.numeric(model$TE.random)),
    ci_low = backtransform(as.numeric(model$lower.random)),
    ci_high = backtransform(as.numeric(model$upper.random)),
    p_value = as.numeric(model$pval.random %||% NA_real_),
    prediction_low = if (is.finite(pred_low)) backtransform(pred_low) else NA_real_,
    prediction_high = if (is.finite(pred_high)) backtransform(pred_high) else NA_real_,
    tau2 = as.numeric(model$tau2 %||% NA_real_),
    i2_percent = 100 * as.numeric(model$I2 %||% NA_real_),
    common_estimate = backtransform(as.numeric(model$TE.common %||% NA_real_)),
    common_ci_low = backtransform(as.numeric(model$lower.common %||% NA_real_)),
    common_ci_high = backtransform(as.numeric(model$upper.common %||% NA_real_)),
    reference_treatment = analysis$reference_treatment,
    stringsAsFactors = FALSE
  )
}

.inverse_logit <- function(x) stats::plogis(x)

.ratio_measure <- function(sm) sm %in% c("OR", "RR", "HR")

.natural_effect <- function(x, sm) if (.ratio_measure(sm)) exp(x) else x

.format_number <- function(x, digits = 2L) {
  ifelse(is.na(x), "NR", formatC(x, digits = digits, format = "f"))
}

.format_effect_ci <- function(est, low, high, sm, digits = 2L) {
  paste0(.format_number(est, digits), " [", .format_number(low, digits), "; ",
         .format_number(high, digits), "]")
}

.random_weight_percent <- function(model, n) {
  weights <- suppressWarnings(as.numeric(model$w.random %||% numeric()))
  if (length(weights) != n || any(!is.finite(weights)) || sum(weights) <= 0) {
    weights <- rep(1, n)
  }
  100 * weights / sum(weights)
}

.forest_favour_label <- function(analysis, treat1, treat2) {
  direction <- analysis$outcome_direction
  if (is.na(direction) || direction == "neutral") return(analysis$effect_measure %||% "Effect")
  if (direction == "lower_better") {
    paste0("Favours ", treat1, "                                      Favours ", treat2)
  } else {
    paste0("Favours ", treat2, "                                      Favours ", treat1)
  }
}

.forest_favour_labels <- function(analysis, treat1, treat2) {
  direction <- analysis$outcome_direction %||% "neutral"
  if (is.na(direction) || direction == "neutral") {
    return(list(left = "Lower effect", right = "Higher effect"))
  }
  if (direction == "lower_better") {
    list(left = paste("Favours", treat1), right = paste("Favours", treat2))
  } else {
    list(left = paste("Favours", treat2), right = paste("Favours", treat1))
  }
}

.wrap_plot_header <- function(x, width = 24L) {
  x <- .trim_chr(x)
  if (is.na(x) || !nzchar(x)) return("")
  paste(strwrap(x, width = width), collapse = "\n")
}

.plot_max_chars <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  if (!length(x)) return(0L)
  max(nchar(x), na.rm = TRUE)
}

.v1_forest_dimensions <- function(n_studies, study_labels, display_columns = NULL,
                                  headers = NULL, n_subgroups = 0L,
                                  atomic_columns = 0L) {
  study_chars <- .plot_max_chars(study_labels)
  display_chars <- if (is.null(display_columns)) 0L else
    max(vapply(display_columns, .plot_max_chars, integer(1)), 0L)
  header_chars <- .plot_max_chars(headers)
  header_total_chars <- sum(nchar(as.character(headers %||% character())), na.rm = TRUE)
  wrapped_headers <- vapply(headers %||% character(), function(x) {
    length(strsplit(.wrap_plot_header(x, 18L), "\n", fixed = TRUE)[[1L]])
  }, integer(1))
  header_lines <- if (length(wrapped_headers)) max(wrapped_headers) else 1L
  width <- 13.5 + atomic_columns * 0.75 + max(0, study_chars - 18) * 0.09 +
    max(0, display_chars - 14) * 0.09 + max(0, header_chars - 18) * 0.08 +
    max(0, header_total_chars - 36) * 0.045
  height <- 2.5 + n_studies * 0.23 + n_subgroups * 0.85 +
    max(0L, header_lines - 1L) * 0.35
  list(width = min(40, max(14, width)), height = min(30, max(5.2, height)))
}

.write_plot_pair <- function(draw, png_path, pdf_path, width, height, res = 300L) {
  dir.create(dirname(png_path), recursive = TRUE, showWarnings = FALSE)
  render <- function(open_device) {
    open_device()
    owned_device <- grDevices::dev.cur()
    on.exit({
      open <- grDevices::dev.list()
      if (!is.na(owned_device) && !is.null(open) && owned_device %in% open) {
        grDevices::dev.off(which = owned_device)
      }
    }, add = TRUE)
    draw()
    grDevices::dev.off(which = owned_device)
    owned_device <- NA_integer_
    invisible(NULL)
  }
  render(function() grDevices::png(
    png_path, width = width, height = height, units = "in", res = res
  ))
  render(function() grDevices::pdf(pdf_path, width = width, height = height,
                                    family = "Helvetica", onefile = TRUE))
  normalizePath(c(png_path, pdf_path), mustWork = FALSE)
}

.pairwise_forest_mode <- function(canonical) {
  if (nrow(canonical) && all(canonical$source_type == "raw_derived")) {
    "raw_atomic"
  } else {
    "reported_total_only"
  }
}

.pairwise_source_label <- function(source_type, effect_measure = NULL) {
  labels <- c(adjusted = "Adjusted", crude = "Crude", raw_derived = "Raw-derived")
  base <- unname(labels[source_type])
  base[is.na(base)] <- tools::toTitleCase(gsub("_", " ", source_type[is.na(base)]))
  if (!is.null(effect_measure)) paste(base, effect_measure) else base
}

.pairwise_source_factor <- function(canonical, effect_measure = NULL) {
  order <- c("adjusted", "crude", "raw_derived")
  present <- order[order %in% canonical$source_type]
  unknown <- setdiff(unique(canonical$source_type), order)
  levels_raw <- c(present, unknown)
  factor(
    .pairwise_source_label(canonical$source_type),
    levels = .pairwise_source_label(levels_raw)
  )
}

.pad_atomic_header_group <- function(leftlabs, leftcols, attach, treatment_label) {
  if (!length(attach)) return(leftlabs)
  positions <- match(attach, leftcols)
  positions <- positions[!is.na(positions)]
  if (!length(positions)) return(leftlabs)
  current_chars <- sum(nchar(leftlabs[positions])) + max(0L, length(positions) - 1L) * 2L
  target_chars <- nchar(treatment_label %||% "") + 6L
  pad_each <- max(3L, ceiling(max(0L, target_chars - current_chars) /
                               (2L * length(positions))))
  pad_each <- min(32L, pad_each)
  padding <- strrep("\u00A0", pad_each)
  leftlabs[positions] <- paste0(padding, leftlabs[positions], padding)
  leftlabs
}

.attach_pairwise_atomic_columns <- function(model, canonical, analysis,
                                            forest_mode, show_source_column = FALSE) {
  index <- match(.normalize_label_key(model$studlab),
                 .normalize_label_key(canonical$study_label))
  if (anyNA(index) && length(model$studlab) == nrow(canonical)) {
    index <- seq_len(nrow(canonical))
  }
  dat <- canonical[index, , drop = FALSE]
  model$label.e <- .first_nonmissing(dat$treat1, "Treatment")
  model$label.c <- .first_nonmissing(
    dat$treat2, analysis$reference_treatment %||% "Comparator"
  )
  model$data$source_display <- .pairwise_source_label(dat$source_type)

  leftcols <- "studlab"
  leftlabs <- "Study"
  label_e_attach <- label_c_attach <- NULL
  display_columns <- list()
  if (forest_mode == "raw_atomic" && analysis$outcome_type == "binary") {
    model[["event.e"]] <- dat$event1
    model[["n.e"]] <- dat$sample1
    model[["event.c"]] <- dat$event2
    model[["n.c"]] <- dat$sample2
    leftcols <- c(leftcols, "event.e", "n.e", "event.c", "n.c")
    leftlabs <- c(leftlabs, "Event", "Total", "Event", "Total")
    label_e_attach <- c("event.e", "n.e")
    label_c_attach <- c("event.c", "n.c")
    display_columns <- list(dat$event1, dat$sample1, dat$event2, dat$sample2)
  } else if (forest_mode == "raw_atomic") {
    model[["mean.e"]] <- dat$mean1
    model[["sd.e"]] <- dat$sd1
    model[["n.e"]] <- dat$sample1
    model[["mean.c"]] <- dat$mean2
    model[["sd.c"]] <- dat$sd2
    model[["n.c"]] <- dat$sample2
    leftcols <- c(leftcols, "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c")
    leftlabs <- c(leftlabs, "Mean", "SD", "Total", "Mean", "SD", "Total")
    label_e_attach <- c("mean.e", "sd.e", "n.e")
    label_c_attach <- c("mean.c", "sd.c", "n.c")
    display_columns <- list(dat$mean1, dat$sd1, dat$sample1, dat$mean2, dat$sd2, dat$sample2)
  } else {
    if (show_source_column) {
      leftcols <- c(leftcols, "source_display")
      leftlabs <- c(leftlabs, "Source")
      display_columns <- c(display_columns, list(model$data$source_display))
    }
    model[["n.e"]] <- dat$sample1
    model[["n.c"]] <- dat$sample2
    complete_overall <- all(is.finite(dat$sample1)) && all(is.finite(dat$sample2))
    model$n.e.pooled <- if (complete_overall) sum(dat$sample1) else NA_real_
    model$n.c.pooled <- if (complete_overall) sum(dat$sample2) else NA_real_
    if (!is.null(model$subgroup)) {
      subgroup_values <- as.character(model$subgroup)
      subgroup_levels <- model$subgroup.levels %||% unique(subgroup_values)
      total_pairs <- lapply(subgroup_levels, function(level) {
        keep <- !is.na(subgroup_values) & subgroup_values == level
        complete <- any(keep) && all(is.finite(dat$sample1[keep])) &&
          all(is.finite(dat$sample2[keep]))
        if (complete) {
          c(sum(dat$sample1[keep]), sum(dat$sample2[keep]))
        } else {
          c(NA_real_, NA_real_)
        }
      })
      total_matrix <- do.call(rbind, total_pairs)
      model$n.e.w <- stats::setNames(total_matrix[, 1L], subgroup_levels)
      model$n.c.w <- stats::setNames(total_matrix[, 2L], subgroup_levels)
    }
    leftcols <- c(leftcols, "n.e", "n.c")
    label_e_attach <- "n.e"
    label_c_attach <- "n.c"
    leftlabs <- c(leftlabs, "Total", "Total")
    display_columns <- c(display_columns, list(
      .format_number(dat$sample1, 0), .format_number(dat$sample2, 0)
    ))
  }
  leftlabs <- .pad_atomic_header_group(
    leftlabs, leftcols, label_e_attach, model$label.e
  )
  leftlabs <- .pad_atomic_header_group(
    leftlabs, leftcols, label_c_attach, model$label.c
  )
  list(
    model = model, leftcols = leftcols, leftlabs = leftlabs,
    label_e_attach = label_e_attach, label_c_attach = label_c_attach,
    display_columns = display_columns, treat1 = model$label.e, treat2 = model$label.c
  )
}

.export_pairwise_forest_v1 <- function(model, canonical, analysis, output_dir,
                                       basename, title, subgroup_name = NULL,
                                       forest_mode = NULL,
                                       show_source_column = FALSE,
                                       print_subgroup_name = TRUE) {
  if (!nrow(canonical)) return(character())
  .require_namespace("meta")
  forest_mode <- forest_mode %||% .pairwise_forest_mode(canonical)
  atomic <- .attach_pairwise_atomic_columns(
    model, canonical, analysis, forest_mode, show_source_column
  )
  display_model <- atomic$model
  treat1 <- atomic$treat1
  treat2 <- atomic$treat2
  favour <- .forest_favour_labels(analysis, treat1, treat2)
  n_subgroups <- if (is.null(subgroup_name)) 0L else
    length(unique(stats::na.omit(display_model$subgroup)))
  dims <- .v1_forest_dimensions(
    nrow(canonical), canonical$study_label, atomic$display_columns,
    c(treat1, treat2, analysis$effect_measure), n_subgroups,
    atomic_columns = length(atomic$leftcols) - 1L
  )
  heading_font_size <- if (max(nchar(c(treat1, treat2)), na.rm = TRUE) > 70L) {
    6.25
  } else if (max(nchar(c(treat1, treat2)), na.rm = TRUE) > 48L) {
    7
  } else if (max(nchar(c(treat1, treat2)), na.rm = TRUE) > 34L) {
    7.75
  } else {
    8.5
  }
  if (!is.null(subgroup_name)) display_model$subgroup.name <- subgroup_name
  prediction_available <- isTRUE(nrow(canonical) >= 5L) &&
    any(is.finite(c(display_model$lower.predict, display_model$upper.predict)))
  forest_args <- list(
    x = display_model,
    common = FALSE,
    random = TRUE,
    overall = TRUE,
    prediction = prediction_available,
    overall.hetstat = TRUE,
    print.I2 = TRUE,
    print.tau2 = TRUE,
    print.pval.Q = TRUE,
    test.subgroup.random = !is.null(subgroup_name) && n_subgroups >= 2L,
    leftcols = atomic$leftcols,
    leftlabs = atomic$leftlabs,
    label.e = treat1,
    label.c = treat2,
    label.e.attach = atomic$label_e_attach,
    label.c.attach = atomic$label_c_attach,
    rightcols = c("effect", "ci", "w.random"),
    rightlabs = c(analysis$effect_measure, "95% CI", "Weight"),
    label.left = favour$left,
    label.right = favour$right,
    lab.NA = "NR",
    digits = 2,
    digits.mean = 2,
    digits.sd = 2,
    digits.event = 0,
    digits.weight = 1,
    just.studlab = "left",
    just.addcols.left = "center",
    just.label.e = "center",
    just.label.c = "center",
    fontsize = 8.5,
    fs.heading = heading_font_size,
    fs.study = 8.5,
    fs.study.labels = 8.5,
    fs.lr = 8,
    col.square = "black",
    col.square.lines = "black",
    col.diamond = "black",
    col.diamond.lines = "black",
    col.predict = "#C00000",
    col.predict.lines = "black",
    spacing = if (nrow(canonical) > 25L) 0.9 else 1.05,
    plotwidth = "6cm",
    colgap = "3mm",
    colgap.left = "2mm",
    colgap.forest.left = "10mm",
    colgap.forest.right = "5mm",
    smlab = ""
  )
  if (!is.null(subgroup_name)) {
    # A vector prevents meta::forest() from suppressing the subtotal row for
    # a source block that contains a single reported estimate.
    forest_args$subgroup <- rep(TRUE, n_subgroups)
    forest_args$subgroup.hetstat <- rep(TRUE, n_subgroups)
    forest_args$print.Q.subgroup <- TRUE
    forest_args$common.subgroup <- FALSE
    forest_args$random.subgroup <- TRUE
    forest_args$print.subgroup.name <- print_subgroup_name
    forest_args$prediction.subgroup <- FALSE
    if (identical(subgroup_name, "Estimate source")) {
      dims$width <- max(dims$width, 19)
      dims$height <- min(30, max(
        dims$height,
        4.5 + nrow(canonical) * 0.36 + n_subgroups
      ))
      forest_args$colgap <- "5mm"
      forest_args$colgap.left <- "4mm"
    }
  }
  draw <- function() {
    do.call(meta::forest, forest_args)
    grid::grid.text(
      title, x = grid::unit(0.5, "npc"), y = grid::unit(0.985, "npc"),
      gp = grid::gpar(fontfamily = "sans", fontsize = 12, fontface = "bold")
    )
  }
  base <- file.path(output_dir, basename)
  .write_plot_pair(draw, paste0(base, ".png"), paste0(base, ".pdf"),
                   dims$width, dims$height)
}

.run_source_subgroups <- function(canonical, analysis) {
  forest_mode <- .pairwise_forest_mode(canonical)
  if (forest_mode == "raw_atomic") {
    return(list(
      tables = data.frame(), model = NULL, forest_mode = forest_mode,
      source_count = 0L
    ))
  }
  group <- .pairwise_source_factor(canonical, analysis$effect_measure)
  fit <- .fit_metagen(
    canonical, analysis$effect_measure,
    prediction = nrow(canonical) >= 5L, subgroup = group
  )
  levels_present <- levels(droplevels(group))
  rows <- lapply(levels_present, function(level) {
    keep <- !is.na(group) & group == level
    dat <- canonical[keep, , drop = FALSE]
    model <- .fit_metagen(dat, analysis$effect_measure, prediction = FALSE)
    complete_total1 <- nrow(dat) > 0L && all(is.finite(dat$sample1))
    complete_total2 <- nrow(dat) > 0L && all(is.finite(dat$sample2))
    data.frame(
      analysis_id = analysis$analysis_id,
      subgroup_variable = "estimate_source",
      subgroup = level,
      k = nrow(dat),
      TE = as.numeric(model$TE.random),
      seTE = as.numeric(model$seTE.random),
      lower = as.numeric(model$lower.random),
      upper = as.numeric(model$upper.random),
      tau2 = as.numeric(model$tau2 %||% NA_real_),
      i2_percent = 100 * as.numeric(model$I2 %||% NA_real_),
      total_treat1 = if (complete_total1) sum(dat$sample1) else NA_real_,
      total_treat2 = if (complete_total2) sum(dat$sample2) else NA_real_,
      test_for_difference_p = if (length(levels_present) >= 2L) {
        as.numeric(fit$pval.Q.b.random %||% NA_real_)
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  })
  fit$subgroup.name <- "Estimate source"
  list(
    tables = if (length(rows)) do.call(rbind, rows) else data.frame(),
    model = fit, forest_mode = forest_mode,
    source_count = length(levels_present)
  )
}

.export_nma_forest_v1 <- function(model, counts, rank, analysis, output_dir,
                                  reference) {
  .require_namespace("netmeta")
  treatments <- as.character(model$trts)
  add_data <- data.frame(
    Studies = rep(NA_character_, length(treatments)),
    Participants = rep(NA_character_, length(treatments)),
    Pscore = rep(NA_character_, length(treatments)),
    row.names = treatments, stringsAsFactors = FALSE
  )
  count_index <- match(treatments, counts$treatment)
  add_data$Studies <- ifelse(
    is.na(count_index), "NR", .format_number(counts$k_studies[count_index], 0)
  )
  add_data$Participants <- ifelse(
    is.na(count_index) | is.na(counts$participants[count_index]), "NR",
    .format_number(counts$participants[count_index], 0)
  )
  if (!is.null(rank)) {
    score <- unname(rank$Pscore.random[match(treatments, names(rank$Pscore.random))])
    add_data$Pscore <- ifelse(is.na(score), "NR", .format_number(score, 2))
  } else {
    add_data$Pscore <- "NR"
  }
  favour <- .forest_favour_labels(analysis, "listed treatment", reference)
  dims <- .v1_forest_dimensions(
    max(1L, length(treatments) - 1L), treatments,
    list(add_data$Studies, add_data$Participants, add_data$Pscore),
    c("Treatment", "Studies", "Participants", "P-score", analysis$effect_measure)
  )
  forest_args <- list(
    x = model,
    pooled = "random",
    reference.group = reference,
    baseline.reference = FALSE,
    drop.reference.group = TRUE,
    add.data = add_data,
    leftcols = c("studlab", "Studies", "Participants", "Pscore"),
    leftlabs = c("Treatment", "Studies", "Participants", "P-score"),
    rightcols = c("effect", "ci"),
    rightlabs = c(analysis$effect_measure, "95% CI"),
    label.left = favour$left,
    label.right = favour$right,
    overall.hetstat = TRUE,
    print.I2 = TRUE,
    print.tau2 = TRUE,
    digits = 2,
    equal.size = TRUE,
    col.square = "black",
    col.square.lines = "black",
    col.diamond = "black",
    col.diamond.lines = "black",
    plotwidth = "6cm",
    colgap = "3mm",
    colgap.forest.left = "10mm",
    colgap.forest.right = "5mm",
    smlab = ""
  )
  title <- paste0("Network meta-analysis: ", analysis$outcome_name, " vs ", reference)
  draw <- function() {
    do.call(meta::forest, forest_args)
    grid::grid.text(
      title, x = grid::unit(0.5, "npc"), y = grid::unit(0.985, "npc"),
      gp = grid::gpar(fontfamily = "sans", fontsize = 12, fontface = "bold")
    )
  }
  base <- file.path(output_dir, "forest_nma")
  .write_plot_pair(draw, paste0(base, ".png"), paste0(base, ".pdf"),
                   dims$width, dims$height)
}

.export_single_arm_forest_v1 <- function(model, dat, analysis, output_dir,
                                         basename, title, subgroup_name = NULL) {
  if (!nrow(dat)) return(character())
  .require_namespace("meta")
  display_model <- model
  n_subgroups <- if (is.null(subgroup_name)) 0L else
    length(unique(stats::na.omit(display_model$subgroup)))
  if (!is.null(subgroup_name)) display_model$subgroup.name <- subgroup_name
  if (analysis$outcome_type == "proportion") {
    leftcols <- c("studlab", "event", "n")
    leftlabs <- c("Study", "Events", "Total")
    measure <- "Proportion"
  } else {
    index <- match(.normalize_label_key(display_model$studlab),
                   .normalize_label_key(dat$study_label))
    display_model$data$mean_display <- .format_number(dat$mean[index])
    display_model$data$sd_display <- .format_number(dat$sd[index])
    display_model$data$n_display <- .format_number(dat$sample[index], 0)
    leftcols <- c("studlab", "mean_display", "sd_display", "n_display")
    leftlabs <- c("Study", "Mean", "SD", "Total")
    measure <- "Mean"
  }
  dims <- .v1_forest_dimensions(
    nrow(dat), dat$study_label,
    if (analysis$outcome_type == "proportion") list(dat$event, dat$sample) else
      list(dat$mean, dat$sd, dat$sample),
    c(leftlabs, measure), n_subgroups
  )
  prediction_available <- nrow(dat) >= 5L &&
    any(is.finite(c(display_model$lower.predict, display_model$upper.predict)))
  forest_args <- list(
    x = display_model, common = FALSE, random = TRUE, overall = TRUE,
    prediction = prediction_available, overall.hetstat = TRUE,
    print.I2 = TRUE, print.tau2 = TRUE, print.pval.Q = TRUE,
    print.subgroup.name = !is.null(subgroup_name),
    test.subgroup.random = !is.null(subgroup_name),
    leftcols = leftcols, leftlabs = leftlabs,
    rightcols = c("effect", "ci", "w.random"),
    rightlabs = c(measure, "95% CI", "Weight"),
    digits = 2, digits.weight = 1,
    col.square = "black", col.square.lines = "black",
    col.diamond = "black", col.diamond.lines = "black",
    col.predict = "#C00000", col.predict.lines = "black",
    spacing = if (nrow(dat) > 25L) 0.9 else 1.05,
    plotwidth = "6cm", colgap = "3mm",
    colgap.forest.left = "10mm", colgap.forest.right = "5mm", smlab = ""
  )
  draw <- function() {
    do.call(meta::forest, forest_args)
    grid::grid.text(
      title, x = grid::unit(0.5, "npc"), y = grid::unit(0.985, "npc"),
      gp = grid::gpar(fontfamily = "sans", fontsize = 12, fontface = "bold")
    )
  }
  base <- file.path(output_dir, basename)
  .write_plot_pair(draw, paste0(base, ".png"), paste0(base, ".pdf"),
                   dims$width, dims$height)
}

.export_loo_plot_v1 <- function(table_out, overall, analysis, output_dir,
                                measure = analysis$effect_measure) {
  if (!nrow(table_out)) return(character())
  dat <- table_out[order(table_out$estimate), , drop = FALSE]
  max_chars <- .plot_max_chars(dat$omitted_study)
  cex_axis <- if (max_chars > 45L) 0.55 else if (max_chars > 35L) 0.65 else
    if (max_chars > 25L) 0.75 else 0.85
  left_margin <- max(11, ceiling(max_chars * cex_axis * 0.5) + 3)
  width <- min(18, max(10, 8 + max(0, max_chars - 20) * 0.08))
  height <- min(24, max(5, 2.4 + nrow(dat) * 0.36))
  ratio <- .ratio_measure(measure)
  range_x <- range(c(dat$ci_low, dat$ci_high, overall), finite = TRUE)
  if (diff(range_x) == 0) range_x <- range_x + c(-0.5, 0.5)
  draw <- function() {
    graphics::par(mar = c(5, left_margin, 4, 2), family = "sans")
    y <- seq_len(nrow(dat))
    graphics::plot(
      dat$estimate, y, xlim = range_x, xlab = paste("Pooled", measure, "after omission"),
      ylab = "", pch = 19, yaxt = "n", log = if (ratio) "x" else "",
      main = paste("Leave-One-Out:", analysis$outcome_name)
    )
    graphics::axis(2, at = y, labels = dat$omitted_study, las = 1,
                   cex.axis = cex_axis, tick = FALSE)
    graphics::segments(dat$ci_low, y, dat$ci_high, y, lwd = 1.2)
    graphics::abline(v = overall, lty = 2, lwd = 2, col = "#C00000")
    graphics::mtext("Omitted study", side = 2, line = left_margin - 1.5,
                    cex = 0.9, font = 2)
    graphics::legend(
      "bottomright", legend = "Overall estimate", lty = 2, lwd = 2,
      col = "#C00000", bty = "n", cex = 0.85
    )
  }
  base <- file.path(output_dir, "leave_one_out")
  .write_plot_pair(draw, paste0(base, ".png"), paste0(base, ".pdf"), width, height)
}

.export_bubble_plot_v1 <- function(fit, x, dat, analysis, moderator, output_dir) {
  grid_x <- seq(min(x), max(x), length.out = 200L)
  pred <- stats::predict(fit, newmods = grid_x)
  ratio <- .ratio_measure(analysis$effect_measure)
  transform <- if (ratio) exp else identity
  y <- transform(dat$TE)
  fitted <- transform(pred$pred)
  lower <- transform(pred$ci.lb)
  upper <- transform(pred$ci.ub)
  weights <- 1 / (dat$seTE^2 + fit$tau2)
  point_cex <- 0.8 + 2.4 * sqrt(weights / max(weights, na.rm = TRUE))
  moderator_label <- gsub("_", " ", sub("^moderator_num_", "", moderator))
  y_range <- range(c(y, lower, upper), finite = TRUE)
  p_value <- as.numeric(fit$QMp %||% NA_real_)
  r2 <- as.numeric(fit$R2 %||% NA_real_)
  draw <- function() {
    graphics::par(mar = c(5, 5, 4.5, 2), family = "sans")
    graphics::plot(
      x, y, type = "n", xlab = moderator_label,
      ylab = analysis$effect_measure, ylim = y_range,
      log = if (ratio) "y" else "",
      main = paste("Bubble plot:", analysis$outcome_name)
    )
    graphics::polygon(
      c(grid_x, rev(grid_x)), c(lower, rev(upper)),
      border = NA, col = grDevices::adjustcolor("lightblue", alpha.f = 0.45)
    )
    graphics::lines(grid_x, fitted, col = "steelblue4", lwd = 2)
    graphics::abline(h = if (ratio) 1 else 0, lty = 2, col = "grey45")
    graphics::points(
      x, y, pch = 21, cex = point_cex, bg = grDevices::adjustcolor("steelblue", 0.7),
      col = "steelblue4", lwd = 0.8
    )
    legend_text <- paste0(
      "R2 = ", ifelse(is.finite(r2), paste0(.format_number(r2, 1), "%"), "NR"),
      "\nModerator p = ", ifelse(is.finite(p_value), .format_number(p_value, 3), "NR")
    )
    graphics::legend("topright", legend = legend_text, bty = "n", cex = 0.85)
  }
  base <- file.path(output_dir, paste0("bubble_", .safe_name(moderator)))
  .write_plot_pair(draw, paste0(base, ".png"), paste0(base, ".pdf"), 9, 7)
}

.forest_grob <- function(plot_data, title, sm, left_header = "Study",
                         data1_header = "Treatment", data2_header = "Comparator",
                         x_label = NULL, null = NULL, reference = NULL,
                         right_header = NULL) {
  .require_namespace("ggplot2")
  .require_namespace("gridExtra")
  if (!nrow(plot_data)) return(NULL)
  d <- plot_data
  d$row_index <- rev(seq_len(nrow(d)))
  d$weight_plot <- d$weight
  finite_weights <- d$weight_plot[is.finite(d$weight_plot)]
  replacement_weight <- if (length(finite_weights)) max(finite_weights) * 1.25 else 1
  d$weight_plot[!is.finite(d$weight_plot)] <- replacement_weight
  d$point_size <- 2 + 4 * sqrt(d$weight_plot / max(d$weight_plot, na.rm = TRUE))

  left <- ggplot2::ggplot(d) +
    ggplot2::geom_text(ggplot2::aes(x = 0.00, y = row_index, label = label),
                       hjust = 0, family = "sans", size = 3.1) +
    ggplot2::geom_text(ggplot2::aes(x = 0.64, y = row_index, label = data1),
                       hjust = 1, family = "sans", size = 2.9) +
    ggplot2::geom_text(ggplot2::aes(x = 0.98, y = row_index, label = data2),
                       hjust = 1, family = "sans", size = 2.9) +
    ggplot2::annotate("text", x = 0, y = max(d$row_index) + 1, label = left_header,
                      hjust = 0, fontface = "bold", family = "sans", size = 3.1) +
    ggplot2::annotate("text", x = 0.64, y = max(d$row_index) + 1, label = data1_header,
                      hjust = 1, fontface = "bold", family = "sans", size = 3.0) +
    ggplot2::annotate("text", x = 0.98, y = max(d$row_index) + 1, label = data2_header,
                      hjust = 1, fontface = "bold", family = "sans", size = 3.0) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0.3, max(d$row_index) + 1.4), clip = "off") +
    ggplot2::theme_void(base_family = "sans") +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 4, 5.5, 5.5))

  mid <- ggplot2::ggplot(d, ggplot2::aes(y = row_index)) +
    ggplot2::geom_vline(xintercept = null, colour = "#666666", linewidth = 0.5) +
    ggplot2::geom_segment(
      ggplot2::aes(x = ci_low, xend = ci_high, yend = row_index),
      linewidth = 0.55, colour = "black", na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = estimate, size = point_size, shape = is_overall),
      colour = "black", fill = "black", stroke = 0.8, na.rm = TRUE
    ) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_shape_manual(values = c(`FALSE` = 15, `TRUE` = 23), guide = "none") +
    ggplot2::coord_cartesian(ylim = c(0.3, max(d$row_index) + 1.4), clip = "off") +
    ggplot2::labs(x = x_label %||% sm, y = NULL) +
    ggplot2::theme_minimal(base_family = "sans", base_size = 9) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(5.5, 4, 5.5, 4)
    )
  if (!is.null(reference) && length(reference) == 1L && is.finite(reference)) {
    mid <- mid + ggplot2::geom_vline(
      xintercept = reference, colour = "#C00000", linewidth = 0.65, linetype = 3
    )
  }
  if (.ratio_measure(sm)) {
    positive <- c(d$ci_low, d$ci_high, d$estimate)
    positive <- positive[is.finite(positive) & positive > 0]
    limits <- if (length(positive)) {
      c(max(min(positive) * 0.8, .Machine$double.eps), max(positive) * 1.2)
    } else {
      c(0.5, 2)
    }
    mid <- mid + ggplot2::scale_x_log10(limits = limits)
  }

  right <- ggplot2::ggplot(d) +
    ggplot2::geom_text(ggplot2::aes(x = 0, y = row_index, label = effect_text),
                       hjust = 0, family = "sans", size = 3.0) +
    ggplot2::annotate("text", x = 0, y = max(d$row_index) + 1,
                      label = right_header %||% paste0(sm, " [95% CI]"), hjust = 0,
                      fontface = "bold", family = "sans", size = 3.0) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0.3, max(d$row_index) + 1.4), clip = "off") +
    ggplot2::theme_void(base_family = "sans") +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 4))

  gridExtra::arrangeGrob(
    left, mid, right, ncol = 3, widths = c(0.50, 0.30, 0.20),
    top = grid::textGrob(title, gp = grid::gpar(fontfamily = "sans", fontsize = 13, fontface = "bold"))
  )
}

.export_forest <- function(plot_data, output_dir, basename, title, sm,
                           left_header = "Study", data1_header = "Treatment",
                           data2_header = "Comparator", x_label = NULL,
                           null = NULL, page_size = 40L, reference = NULL,
                           right_header = NULL) {
  if (!nrow(plot_data)) return(character())
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (is.null(null)) null <- if (.ratio_measure(sm)) 1 else 0
  pages <- split(seq_len(nrow(plot_data)), ceiling(seq_len(nrow(plot_data)) / page_size))
  max_nchar <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x)]
    if (length(x)) max(nchar(x)) else 0
  }
  text_budget <- max_nchar(plot_data$label) + max_nchar(plot_data$data1) +
    max_nchar(plot_data$data2) + max_nchar(plot_data$effect_text)
  width <- min(20, max(12, 8.5 + text_budget * 0.07))
  paths <- character()
  pdf_path <- file.path(output_dir, paste0(basename, ".pdf"))
  grDevices::pdf(pdf_path, width = width, height = min(14, max(5, 2.2 + min(page_size, nrow(plot_data)) * 0.28)),
                 family = "Helvetica", onefile = TRUE)
  pdf_device <- grDevices::dev.cur()
  tryCatch({
    for (p in seq_along(pages)) {
      idx <- pages[[p]]
      d <- plot_data[idx, , drop = FALSE]
      overall <- plot_data[plot_data$is_overall %in% TRUE, , drop = FALSE]
      if (nrow(overall) && !any(d$is_overall)) d <- rbind(d, overall[1L, , drop = FALSE])
      grob <- .forest_grob(d, if (length(pages) > 1L) paste0(title, " (page ", p, ")") else title,
                           sm, left_header, data1_header, data2_header, x_label, null,
                           reference, right_header)
      grDevices::dev.set(pdf_device)
      grid::grid.newpage(); grid::grid.draw(grob)
      png_path <- file.path(output_dir, sprintf("%s_p%02d.png", basename, p))
      height <- min(30, max(5, 2.2 + nrow(d) * 0.28))
      grDevices::png(png_path, width = width, height = height, units = "in", res = 300)
      png_device <- grDevices::dev.cur()
      grid::grid.newpage(); grid::grid.draw(grob)
      grDevices::dev.off(png_device)
      paths <- c(paths, png_path)
    }
    grDevices::dev.off(pdf_device)
  }, error = function(e) {
    open_devices <- grDevices::dev.list()
    if (!is.null(open_devices) && pdf_device %in% open_devices) grDevices::dev.off(pdf_device)
    stop(e)
  })
  c(pdf_path, paths)
}

.pairwise_plot_data <- function(canonical, summary, analysis, tau2 = 0) {
  if (!nrow(canonical)) return(data.frame())
  sm <- analysis$effect_measure
  est <- .natural_effect(canonical$TE, sm)
  lo <- .natural_effect(canonical$lower, sm)
  hi <- .natural_effect(canonical$upper, sm)
  weights <- 1 / (canonical$seTE^2 + (tau2 %||% 0))
  weights <- 100 * weights / sum(weights)
  raw_binary <- !is.na(canonical$event1)
  raw_cont <- !is.na(canonical$mean1)
  data1 <- ifelse(
    raw_binary,
    paste0(.format_number(canonical$event1, 0), "/", .format_number(canonical$sample1, 0)),
    ifelse(raw_cont,
      paste0(.format_number(canonical$mean1), " (", .format_number(canonical$sd1), "); n=", .format_number(canonical$sample1, 0)),
      paste0(tools::toTitleCase(canonical$source_type), ifelse(is.na(canonical$sample1), "", paste0("; n=", canonical$sample1)))
    )
  )
  data2 <- ifelse(
    raw_binary,
    paste0(.format_number(canonical$event2, 0), "/", .format_number(canonical$sample2, 0)),
    ifelse(raw_cont,
      paste0(.format_number(canonical$mean2), " (", .format_number(canonical$sd2), "); n=", .format_number(canonical$sample2, 0)),
      ifelse(is.na(canonical$sample2), "NR", paste0("n=", canonical$sample2))
    )
  )
  d <- data.frame(
    label = canonical$study_label,
    data1 = data1, data2 = data2,
    estimate = est, ci_low = lo, ci_high = hi, weight = weights,
    effect_text = paste0(.format_effect_ci(est, lo, hi, sm), " | ", .format_number(weights, 1), "%"),
    is_overall = FALSE, stringsAsFactors = FALSE
  )
  overall <- data.frame(
    label = "Random-effects model", data1 = paste0("k=", summary$k), data2 = "",
    estimate = summary$estimate, ci_low = summary$ci_low, ci_high = summary$ci_high,
    weight = NA_real_,
    effect_text = .format_effect_ci(summary$estimate, summary$ci_low, summary$ci_high, sm),
    is_overall = TRUE, stringsAsFactors = FALSE
  )
  rbind(d, overall)
}

.run_subgroups <- function(canonical, analysis, output_dir) {
  subgroup_cols <- grep("^subgroup_", names(canonical), value = TRUE)
  tables <- list(); models <- list(); plots <- character()
  for (col in subgroup_cols) {
    values <- .trim_chr(canonical[[col]])
    counts <- table(values)
    eligible <- names(counts[counts >= 2L])
    keep <- !is.na(values) & values %in% eligible
    if (length(eligible) < 2L) next
    dat <- canonical[keep, , drop = FALSE]
    group <- factor(values[keep], levels = eligible)
    fit <- .fit_metagen(dat, analysis$effect_measure, prediction = FALSE, subgroup = group)
    rows <- lapply(eligible, function(level) {
      d <- dat[group == level, , drop = FALSE]
      m <- .fit_metagen(d, analysis$effect_measure, prediction = FALSE)
      data.frame(
        analysis_id = analysis$analysis_id, subgroup_variable = col, subgroup = level,
        k = nrow(d), TE = m$TE.random, seTE = m$seTE.random,
        lower = m$lower.random, upper = m$upper.random,
        test_for_difference_p = fit$pval.Q.b.random %||% NA_real_,
        stringsAsFactors = FALSE
      )
    })
    table_out <- do.call(rbind, rows)
    tables[[col]] <- table_out
    models[[col]] <- fit
    plots <- c(plots, .export_pairwise_forest_v1(
      fit, dat, analysis, output_dir,
      paste0("forest_subgroup_", .safe_name(col)),
      paste0(analysis$outcome_name, " | Subgroup: ", sub("^subgroup_", "", col)),
      subgroup_name = gsub("_", " ", sub("^subgroup_", "", col)),
      forest_mode = .pairwise_forest_mode(canonical),
      show_source_column = .pairwise_forest_mode(canonical) == "reported_total_only"
    ))
  }
  list(
    tables = if (length(tables)) do.call(rbind, tables) else data.frame(),
    models = models, plots = plots
  )
}

.run_meta_regression <- function(canonical, analysis, output_dir) {
  .require_namespace("metafor")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  moderator_cols <- grep("^moderator_(num|cat)_", names(canonical), value = TRUE)
  tables <- list(); models <- list(); plots <- character()
  for (col in moderator_cols) {
    numeric_mod <- grepl("^moderator_num_", col)
    x <- if (numeric_mod) .as_num(canonical[[col]]) else factor(.trim_chr(canonical[[col]]))
    keep <- is.finite(canonical$TE) & is.finite(canonical$seTE) & !is.na(x)
    n_parameters <- if (numeric_mod) 1L else max(1L, nlevels(droplevels(x[keep])) - 1L)
    if (sum(keep) < 10L * n_parameters) next
    dat <- canonical[keep, , drop = FALSE]
    x_use <- x[keep]
    fit <- tryCatch(
      metafor::rma(yi = dat$TE, sei = dat$seTE, mods = ~ x_use, method = "REML", slab = dat$study_label),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    coef_table <- data.frame(
      analysis_id = analysis$analysis_id, moderator = col,
      term = rownames(fit$beta), estimate = as.numeric(fit$beta),
      se = fit$se, ci_low = fit$ci.lb, ci_high = fit$ci.ub, p_value = fit$pval,
      moderator_test_p = as.numeric(fit$QMp %||% NA_real_),
      r2_percent = as.numeric(fit$R2 %||% NA_real_),
      residual_tau2 = as.numeric(fit$tau2 %||% NA_real_),
      k = fit$k, stringsAsFactors = FALSE
    )
    tables[[col]] <- coef_table
    models[[col]] <- fit
    if (numeric_mod) {
      plots <- c(plots, .export_bubble_plot_v1(
        fit, x_use, dat, analysis, col, output_dir
      ))
    }
  }
  list(
    tables = if (length(tables)) do.call(rbind, tables) else data.frame(),
    models = models, plots = plots
  )
}

.run_loo <- function(canonical, analysis, output_dir) {
  if (nrow(canonical) < 3L) return(list(table = data.frame(), model = NULL, plots = character()))
  .require_namespace("metafor")
  fit <- metafor::rma(yi = canonical$TE, sei = canonical$seTE, method = "REML", slab = canonical$study_label)
  loo <- metafor::leave1out(fit)
  table_out <- data.frame(
    analysis_id = analysis$analysis_id,
    omitted_study = sub("^-", "", loo$slab),
    TE = loo$estimate, seTE = loo$se, lower = loo$ci.lb, upper = loo$ci.ub,
    p_value = loo$pval, tau2 = loo$tau2, i2_percent = loo$I2,
    stringsAsFactors = FALSE
  )
  sm <- analysis$effect_measure
  plot_table <- data.frame(
    omitted_study = table_out$omitted_study,
    estimate = .natural_effect(table_out$TE, sm),
    ci_low = .natural_effect(table_out$lower, sm),
    ci_high = .natural_effect(table_out$upper, sm),
    stringsAsFactors = FALSE
  )
  paths <- .export_loo_plot_v1(
    plot_table, .natural_effect(as.numeric(fit$b[[1L]]), sm),
    analysis, output_dir, sm
  )
  list(table = table_out, model = fit, plots = paths)
}

.run_funnel <- function(canonical, analysis, output_dir) {
  if (nrow(canonical) < 10L) return(list(table = data.frame(), model = NULL, plots = character()))
  .require_namespace("metafor")
  fit <- metafor::rma(yi = canonical$TE, sei = canonical$seTE, method = "REML", slab = canonical$study_label)
  test <- metafor::regtest(fit, model = "lm")
  base <- file.path(output_dir, "funnel")
  draw <- function() metafor::funnel(fit, main = paste("Funnel plot:", analysis$outcome_name))
  grDevices::png(paste0(base, ".png"), width = 1800, height = 1500, res = 250)
  draw(); grDevices::dev.off()
  grDevices::pdf(paste0(base, ".pdf"), width = 8, height = 6.5)
  draw(); grDevices::dev.off()
  list(
    table = data.frame(
      analysis_id = analysis$analysis_id, test = "Egger regression",
      statistic = as.numeric(test$zval), p_value = as.numeric(test$pval), k = nrow(canonical),
      stringsAsFactors = FALSE
    ),
    model = fit, plots = paste0(base, c(".png", ".pdf"))
  )
}

.source_sensitivity <- function(all_sources, analysis) {
  if (!nrow(all_sources)) return(data.frame())
  types <- unique(all_sources$source_type)
  rows <- list()
  for (type in types) {
    d <- all_sources[all_sources$source_type == type & is.finite(all_sources$TE) &
                       is.finite(all_sources$seTE) & all_sources$seTE > 0, , drop = FALSE]
    if (length(unique(d$study_id)) < 2L) next
    fit <- tryCatch(.fit_metagen(d, analysis$effect_measure, prediction = nrow(d) >= 5L), error = function(e) NULL)
    if (is.null(fit)) next
    rows[[type]] <- .meta_summary_row(fit, analysis, paste0("Sensitivity: ", type))
  }
  converted <- all_sources[!all_sources$converted & is.finite(all_sources$TE) &
                             is.finite(all_sources$seTE) & all_sources$seTE > 0, , drop = FALSE]
  if (any(all_sources$converted %in% TRUE) && length(unique(converted$study_id)) >= 2L) {
    fit <- tryCatch(.fit_metagen(converted, analysis$effect_measure, prediction = nrow(converted) >= 5L),
                    error = function(e) NULL)
    if (!is.null(fit)) rows[["exclude_converted"]] <- .meta_summary_row(
      fit, analysis, "Sensitivity: exclude median/IQR conversion"
    )
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

.study_design_sensitivity <- function(canonical, analysis) {
  if (!"study_design" %in% names(canonical)) return(data.frame())
  designs <- unique(.trim_chr(canonical$study_design))
  designs <- designs[!is.na(designs)]
  if (length(designs) < 2L) return(data.frame())
  rows <- list()
  for (design in designs) {
    dat <- canonical[.trim_chr(canonical$study_design) == design, , drop = FALSE]
    if (length(unique(dat$study_id)) < 2L) next
    fit <- tryCatch(
      .fit_metagen(dat, analysis$effect_measure, prediction = nrow(dat) >= 5L),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    rows[[design]] <- .meta_summary_row(
      fit, analysis, paste0("Sensitivity: study design = ", design)
    )
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

.binary_raw_sensitivity <- function(raw_dat, analysis, primary_method, sparse) {
  if (!sparse || !nrow(raw_dat)) return(data.frame())
  fit <- tryCatch(
    meta::metabin(
      event.e = raw_dat$event1, n.e = raw_dat$sample1,
      event.c = raw_dat$event2, n.c = raw_dat$sample2,
      studlab = raw_dat$study_label, data = raw_dat,
      sm = analysis$effect_measure, method = "MH",
      common = TRUE, random = TRUE, method.tau = "REML",
      method.random.ci = "HK", adhoc.hakn.ci = "se",
      incr = "TACC", method.incr = "only0", prediction = nrow(raw_dat) >= 5L
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) return(data.frame())
  label <- if (primary_method == "GLMM") {
    "Sensitivity: Mantel-Haenszel with treatment-arm continuity correction"
  } else {
    "Sensitivity: treatment-arm continuity correction"
  }
  .meta_summary_row(fit, analysis, label)
}

.run_pairwise_analysis <- function(data, analysis, output_dir) {
  canonical_bundle <- .canonicalize_analysis(data, analysis)
  canonical <- canonical_bundle$primary
  if (length(unique(canonical$study_id)) < 2L) {
    stop("Kurang dari dua studi dengan effect dan SE yang dapat dihitung.", call. = FALSE)
  }
  raw_only <- all(canonical$source_type == "raw_derived")
  method_name <- "Generic inverse-variance random-effects (REML, Hartung-Knapp)"
  method_decisions <- data.frame()
  binary_sensitivity <- data.frame()

  if (analysis$outcome_type == "binary" && raw_only) {
    raw_dat <- .raw_binary_pair_data(data$arm_data, analysis, unique(canonical$study_id))
    raw_dat <- raw_dat[!(raw_dat$event1 == 0 & raw_dat$event2 == 0), , drop = FALSE]
    meta_a <- data$study_metadata[, c("study_id", "study_label"), drop = FALSE]
    raw_dat <- merge(raw_dat, meta_a, by = "study_id", all.x = TRUE, sort = FALSE)
    raw_dat$study_label[is.na(raw_dat$study_label)] <- raw_dat$study_id[is.na(raw_dat$study_label)]
    sparse <- any(raw_dat$event1 == 0 | raw_dat$event2 == 0 |
                    raw_dat$event1 == raw_dat$sample1 | raw_dat$event2 == raw_dat$sample2) ||
      sum(raw_dat$event1 + raw_dat$event2) < 20 ||
      sum(raw_dat$event1 + raw_dat$event2) / sum(raw_dat$sample1 + raw_dat$sample2) < 0.05
    method <- if (sparse && analysis$effect_measure == "OR") "GLMM" else "MH"
    method_name <- if (method == "GLMM") {
      "Sparse OR: GLMM random-effects"
    } else {
      "Mantel-Haenszel random-effects (REML, Hartung-Knapp)"
    }
    args <- list(
      event.e = raw_dat$event1, n.e = raw_dat$sample1,
      event.c = raw_dat$event2, n.c = raw_dat$sample2,
      studlab = raw_dat$study_label, data = raw_dat,
      sm = analysis$effect_measure, method = method,
      common = TRUE, random = TRUE, method.tau = if (method == "GLMM") "ML" else "REML",
      method.random.ci = "HK", adhoc.hakn.ci = "se", prediction = nrow(raw_dat) >= 5L
    )
    if (method == "MH") {
      args$incr <- 0
      args$MH.exact <- TRUE
      args$method.incr <- "only0"
    } else {
      args$method.random.ci <- "classic"
      args$adhoc.hakn.ci <- NULL
    }
    model <- do.call(meta::metabin, args)
    if (length(model$TE) == nrow(canonical)) {
      finite_study_effect <- is.finite(model$TE) & is.finite(model$seTE) & model$seTE > 0
      canonical$TE[finite_study_effect] <- model$TE[finite_study_effect]
      canonical$seTE[finite_study_effect] <- model$seTE[finite_study_effect]
      canonical$lower <- canonical$TE - stats::qnorm(0.975) * canonical$seTE
      canonical$upper <- canonical$TE + stats::qnorm(0.975) * canonical$seTE
    }
    method_decisions <- data.frame(
      analysis_id = analysis$analysis_id, decision = "binary_method",
      value = method, reason = if (sparse) "Sparse-event rule triggered" else "Non-sparse raw binary data",
      stringsAsFactors = FALSE
    )
    binary_sensitivity <- .binary_raw_sensitivity(raw_dat, analysis, method, sparse)
  } else {
    model <- .fit_metagen(canonical, analysis$effect_measure, prediction = nrow(canonical) >= 5L)
    method_decisions <- data.frame(
      analysis_id = analysis$analysis_id, decision = "primary_method",
      value = "metagen_REML_HK", reason = if (raw_only) "Continuous canonical effects" else "Reported or mixed effect sources",
      stringsAsFactors = FALSE
    )
  }

  summary <- .meta_summary_row(model, analysis, method_name)
  source_subgroup <- .run_source_subgroups(canonical, analysis)
  forest_model <- if (!is.null(source_subgroup$model)) source_subgroup$model else model
  forest_subgroup_name <- if (source_subgroup$forest_mode == "reported_total_only") {
    "Estimate source"
  } else {
    NULL
  }
  plots <- .export_pairwise_forest_v1(
    forest_model, canonical, analysis, output_dir, "forest_overall",
    .analysis_title(analysis), subgroup_name = forest_subgroup_name,
    forest_mode = source_subgroup$forest_mode,
    show_source_column = FALSE,
    print_subgroup_name = FALSE
  )
  subgroup <- .run_subgroups(canonical, analysis, output_dir)
  metareg <- .run_meta_regression(canonical, analysis, output_dir)
  loo <- .run_loo(canonical, analysis, output_dir)
  funnel <- .run_funnel(canonical, analysis, output_dir)
  sensitivity <- .rbind_fill(list(
    .source_sensitivity(canonical_bundle$all_sources, analysis),
    .study_design_sensitivity(canonical, analysis),
    binary_sensitivity
  ))
  method_decisions <- .rbind_fill(list(
    method_decisions,
    data.frame(
      analysis_id = analysis$analysis_id,
      decision = "forest_mode",
      value = source_subgroup$forest_mode,
      reason = if (source_subgroup$forest_mode == "raw_atomic") {
        "All selected primary estimates are raw-derived."
      } else {
        "At least one selected primary estimate is adjusted or crude."
      },
      stringsAsFactors = FALSE
    )
  ))

  list(
    status = "SUCCESS", analysis = analysis, model = model,
    canonical = canonical, all_sources = canonical_bundle$all_sources,
    summary = summary, sensitivity = sensitivity,
    subgroup = subgroup, source_subgroup = source_subgroup,
    meta_regression = metareg, loo = loo, funnel = funnel,
    method_decisions = method_decisions,
    plots = unique(c(plots, subgroup$plots, metareg$plots, loo$plots, funnel$plots))
  )
}

.single_arm_subgroups <- function(dat, analysis, output_dir) {
  subgroup_cols <- grep("^subgroup_", names(dat), value = TRUE)
  tables <- list(); models <- list(); plots <- character()
  for (col in subgroup_cols) {
    values <- .trim_chr(dat[[col]])
    counts <- table(values)
    eligible <- names(counts[counts >= 2L])
    if (length(eligible) < 2L) next
    eligible_data <- dat[values %in% eligible, , drop = FALSE]
    eligible_groups <- droplevels(factor(values[values %in% eligible], levels = eligible))
    difference_model <- tryCatch({
      if (analysis$outcome_type == "proportion") {
        meta::metaprop(
          event = eligible_data$event, n = eligible_data$sample,
          studlab = eligible_data$study_label, data = eligible_data,
          subgroup = eligible_groups,
          subgroup.name = gsub("_", " ", sub("^subgroup_", "", col)),
          sm = "PLOGIT", method = "GLMM", common = TRUE, random = TRUE,
          method.tau = "ML", method.random.ci = "classic", prediction = FALSE
        )
      } else {
        meta::metamean(
          n = eligible_data$sample, mean = eligible_data$mean, sd = eligible_data$sd,
          studlab = eligible_data$study_label, data = eligible_data,
          subgroup = eligible_groups,
          subgroup.name = gsub("_", " ", sub("^subgroup_", "", col)),
          sm = "MRAW", common = TRUE, random = TRUE, method.tau = "REML",
          method.random.ci = "HK", adhoc.hakn.ci = "se", prediction = FALSE
        )
      }
    }, error = function(e) NULL)
    p_difference <- if (is.null(difference_model)) NA_real_ else
      as.numeric(difference_model$pval.Q.b.random %||% NA_real_)
    rows <- list()
    for (level in eligible) {
      d <- dat[values == level, , drop = FALSE]
      if (analysis$outcome_type == "proportion") {
        fit <- meta::metaprop(
          event = d$event, n = d$sample, studlab = d$study_label,
          sm = "PLOGIT", method = "GLMM", common = TRUE, random = TRUE,
          method.tau = "ML", method.random.ci = "classic",
          prediction = nrow(d) >= 5L
        )
        estimate <- stats::plogis(fit$TE.random)
        low <- stats::plogis(fit$lower.random)
        high <- stats::plogis(fit$upper.random)
      } else {
        fit <- meta::metamean(
          n = d$sample, mean = d$mean, sd = d$sd, studlab = d$study_label,
          sm = "MRAW", common = TRUE, random = TRUE, method.tau = "REML",
          method.random.ci = "HK", adhoc.hakn.ci = "se", prediction = nrow(d) >= 5L
        )
        estimate <- fit$TE.random; low <- fit$lower.random; high <- fit$upper.random
      }
      rows[[level]] <- data.frame(
        analysis_id = analysis$analysis_id, subgroup_variable = col,
        subgroup = level, k = nrow(d), estimate = estimate,
        ci_low = low, ci_high = high,
        test_for_difference_p = p_difference, stringsAsFactors = FALSE
      )
    }
    table_out <- do.call(rbind, rows)
    tables[[col]] <- table_out
    if (!is.null(difference_model)) {
      models[[col]] <- difference_model
      plots <- c(plots, .export_single_arm_forest_v1(
        difference_model, eligible_data, analysis, output_dir,
        paste0("forest_subgroup_", .safe_name(col)),
        paste0(analysis$outcome_name, " | Subgroup: ", sub("^subgroup_", "", col)),
        subgroup_name = gsub("_", " ", sub("^subgroup_", "", col))
      ))
    }
  }
  list(
    tables = if (length(tables)) do.call(rbind, tables) else data.frame(),
    models = models, plots = plots
  )
}

.run_proportion_loo <- function(arms, analysis, output_dir, overall_model) {
  if (nrow(arms) < 3L) return(list(table = data.frame(), model = NULL, plots = character()))
  rows <- list()
  for (i in seq_len(nrow(arms))) {
    d <- arms[-i, , drop = FALSE]
    fit <- tryCatch(
      meta::metaprop(
        event = d$event, n = d$sample, studlab = d$study_label,
        sm = "PLOGIT", method = "GLMM", common = TRUE, random = TRUE,
        method.tau = "ML", method.random.ci = "classic", prediction = FALSE
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    rows[[length(rows) + 1L]] <- data.frame(
      analysis_id = analysis$analysis_id, omitted_study = arms$study_label[[i]],
      effect_measure = "PROPORTION", estimate = stats::plogis(fit$TE.random),
      ci_low = stats::plogis(fit$lower.random), ci_high = stats::plogis(fit$upper.random),
      tau2 = as.numeric(fit$tau2 %||% NA_real_),
      i2_percent = 100 * as.numeric(fit$I2 %||% NA_real_), stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(list(table = data.frame(), model = NULL, plots = character()))
  table_out <- do.call(rbind, rows)
  plot_table <- table_out[, c("omitted_study", "estimate", "ci_low", "ci_high"), drop = FALSE]
  plots <- .export_loo_plot_v1(
    plot_table, stats::plogis(overall_model$TE.random), analysis,
    output_dir, "Proportion"
  )
  list(table = table_out, model = overall_model, plots = plots)
}

.single_mean_sensitivity <- function(arms, analysis) {
  if (!any(arms$converted %in% TRUE)) return(data.frame())
  dat <- arms[!arms$converted, , drop = FALSE]
  if (length(unique(dat$study_id)) < 2L) return(data.frame())
  fit <- tryCatch(
    meta::metamean(
      n = dat$sample, mean = dat$mean, sd = dat$sd, studlab = dat$study_label,
      sm = "MRAW", common = TRUE, random = TRUE, method.tau = "REML",
      method.random.ci = "HK", adhoc.hakn.ci = "se", prediction = nrow(dat) >= 5L
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) return(data.frame())
  mean_analysis <- analysis
  mean_analysis$effect_measure <- "MEAN"
  .meta_summary_row(
    fit, mean_analysis, "Sensitivity: exclude median/IQR or range conversion", identity
  )
}

.run_single_arm_analysis_v2 <- function(data, analysis, output_dir) {
  arms <- data$arm_data[data$arm_data$analysis_id == analysis$analysis_id, , drop = FALSE]
  metadata <- data$study_metadata
  arms$._order <- seq_len(nrow(arms))
  metadata_drop <- c("analysis_id", "study_key", "source_sheet", "source_row")
  if ("study_label" %in% names(arms)) metadata_drop <- c(metadata_drop, "study_label")
  arms <- merge(arms, metadata[, setdiff(names(metadata), metadata_drop), drop = FALSE],
                by = "study_id", all.x = TRUE, sort = FALSE)
  arms <- arms[order(arms$._order), , drop = FALSE]
  arms$._order <- NULL
  arms$study_label[is.na(arms$study_label)] <- arms$study_id[is.na(arms$study_label)]
  if (length(unique(arms$study_id)) < 2L) stop("Kurang dari dua studi single-arm valid.", call. = FALSE)
  method_decisions <- data.frame()

  if (analysis$outcome_type == "proportion") {
    model <- meta::metaprop(
      event = arms$event, n = arms$sample, studlab = arms$study_label, data = arms,
      sm = "PLOGIT", method = "GLMM", common = TRUE, random = TRUE,
      method.tau = "ML", method.random.ci = "classic",
      prediction = nrow(arms) >= 5L
    )
    summary <- .meta_summary_row(model, analysis, "Logit GLMM random-effects", stats::plogis)
    summary$effect_measure <- "PROPORTION"
    est <- stats::plogis(model$TE)
    lo <- stats::plogis(model$lower)
    hi <- stats::plogis(model$upper)
    random_weights <- .random_weight_percent(model, nrow(arms))
    plot_data <- data.frame(
      label = arms$study_label,
      data1 = paste0(.format_number(arms$event, 0), "/", .format_number(arms$sample, 0)),
      data2 = ifelse(arms$event == 0, "Zero event", ""),
      estimate = est, ci_low = lo, ci_high = hi,
      weight = random_weights,
      effect_text = paste0(
        .format_effect_ci(est, lo, hi, "Proportion"), " | ",
        .format_number(random_weights, 1), "%"
      ),
      is_overall = FALSE, stringsAsFactors = FALSE
    )
    plot_data <- rbind(plot_data, data.frame(
      label = "Random-effects model", data1 = paste0("k=", nrow(arms)), data2 = "",
      estimate = summary$estimate, ci_low = summary$ci_low, ci_high = summary$ci_high,
      weight = NA, effect_text = .format_effect_ci(summary$estimate, summary$ci_low, summary$ci_high, "Proportion"),
      is_overall = TRUE, stringsAsFactors = FALSE
    ))
    method_decisions <- data.frame(
      analysis_id = analysis$analysis_id, decision = "primary_method",
      value = "metaprop_GLMM_PLOGIT", reason = "Handles zero and all-event studies without continuity correction",
      stringsAsFactors = FALSE
    )
    canonical <- data.frame(
      analysis_id = analysis$analysis_id, study_id = arms$study_id,
      study_label = arms$study_label, TE = model$TE, seTE = model$seTE,
      lower = model$lower, upper = model$upper, source_type = "raw_derived",
      event = arms$event, sample = arms$sample, converted = FALSE,
      stringsAsFactors = FALSE
    )
    extra_cols <- grep("^(subgroup_|moderator_)", names(arms), value = TRUE)
    for (col in extra_cols) canonical[[col]] <- arms[[col]]
    sm_plot <- "Proportion"
  } else {
    estimates <- lapply(seq_len(nrow(arms)), function(i) .mean_sd_from_arm(arms[i, , drop = FALSE]))
    arms$mean <- vapply(estimates, `[[`, numeric(1), "mean")
    arms$sd <- vapply(estimates, `[[`, numeric(1), "sd")
    arms$converted <- vapply(estimates, `[[`, logical(1), "converted")
    model <- meta::metamean(
      n = arms$sample, mean = arms$mean, sd = arms$sd, studlab = arms$study_label, data = arms,
      sm = "MRAW", common = TRUE, random = TRUE, method.tau = "REML",
      method.random.ci = "HK", adhoc.hakn.ci = "se", prediction = nrow(arms) >= 5L
    )
    summary <- .meta_summary_row(model, analysis, "Single mean random-effects (REML, Hartung-Knapp)", identity)
    summary$effect_measure <- "MEAN"
    random_weights <- .random_weight_percent(model, nrow(arms))
    plot_data <- data.frame(
      label = arms$study_label,
      data1 = paste0(.format_number(arms$mean), " (", .format_number(arms$sd), ")"),
      data2 = paste0("n=", .format_number(arms$sample, 0)),
      estimate = model$TE, ci_low = model$lower, ci_high = model$upper,
      weight = random_weights,
      effect_text = paste0(
        .format_effect_ci(model$TE, model$lower, model$upper, "Mean"), " | ",
        .format_number(random_weights, 1), "%"
      ),
      is_overall = FALSE, stringsAsFactors = FALSE
    )
    plot_data <- rbind(plot_data, data.frame(
      label = "Random-effects model", data1 = paste0("k=", nrow(arms)), data2 = "",
      estimate = summary$estimate, ci_low = summary$ci_low, ci_high = summary$ci_high,
      weight = NA, effect_text = .format_effect_ci(summary$estimate, summary$ci_low, summary$ci_high, "Mean"),
      is_overall = TRUE, stringsAsFactors = FALSE
    ))
    canonical <- data.frame(
      analysis_id = analysis$analysis_id, study_id = arms$study_id,
      study_label = arms$study_label, TE = model$TE, seTE = model$seTE,
      lower = model$lower, upper = model$upper, source_type = "raw_derived",
      mean = arms$mean, sd = arms$sd, sample = arms$sample, converted = arms$converted,
      stringsAsFactors = FALSE
    )
    extra_cols <- grep("^(subgroup_|moderator_)", names(arms), value = TRUE)
    for (col in extra_cols) canonical[[col]] <- arms[[col]]
    method_decisions <- data.frame(
      analysis_id = analysis$analysis_id, decision = "primary_method",
      value = "metamean_REML_HK", reason = "Single-arm mean outcome",
      stringsAsFactors = FALSE
    )
    sm_plot <- "Mean"
  }
  plots <- .export_single_arm_forest_v1(
    model, arms, analysis, output_dir, "forest_overall",
    .analysis_title(analysis)
  )
  subgroup <- .single_arm_subgroups(arms, analysis, output_dir)
  analysis_for_effects <- analysis
  analysis_for_effects$effect_measure <- if (analysis$outcome_type == "mean") "MEAN" else "LOGIT_PROPORTION"
  loo <- if (analysis$outcome_type == "mean") {
    .run_loo(canonical, analysis_for_effects, output_dir)
  } else {
    .run_proportion_loo(arms, analysis, output_dir, model)
  }
  metareg <- .run_meta_regression(canonical, analysis_for_effects, output_dir)
  funnel <- .run_funnel(canonical, analysis_for_effects, output_dir)
  sensitivity <- if (analysis$outcome_type == "mean") {
    .single_mean_sensitivity(arms, analysis)
  } else {
    data.frame()
  }
  if (analysis$outcome_type == "mean" && any(arms$converted %in% TRUE)) {
    method_decisions <- rbind(method_decisions, data.frame(
      analysis_id = analysis$analysis_id, decision = "summary_conversion",
      value = "estmeansd_quantile_estimation",
      reason = "Median/IQR or range converted; exclusion sensitivity attempted",
      stringsAsFactors = FALSE
    ))
  }
  list(
    status = "SUCCESS", analysis = analysis, model = model, canonical = canonical,
    summary = summary, sensitivity = sensitivity, subgroup = subgroup,
    meta_regression = metareg, loo = loo, funnel = funnel,
    method_decisions = method_decisions,
    plots = unique(c(plots, subgroup$plots, metareg$plots, loo$plots, funnel$plots))
  )
}

.diagnostic_prop_ci <- function(x, n, level = 0.95) {
  if (n <= 0) return(c(NA_real_, NA_real_))
  stats::binom.test(x, n, conf.level = level)$conf.int
}

.diagnostic_forest_data <- function(dat, measure = c("sensitivity", "specificity"), pooled = NULL) {
  measure <- match.arg(measure)
  if (measure == "sensitivity") {
    event <- dat$tp; n <- dat$tp + dat$fn
  } else {
    event <- dat$tn; n <- dat$tn + dat$fp
  }
  cis <- t(vapply(seq_along(event), function(i) .diagnostic_prop_ci(event[[i]], n[[i]]), numeric(2)))
  est <- event / n
  out <- data.frame(
    label = dat$study_label, data1 = paste0(event, "/", n), data2 = "",
    estimate = est, ci_low = cis[, 1], ci_high = cis[, 2], weight = n / sum(n) * 100,
    effect_text = .format_effect_ci(est, cis[, 1], cis[, 2], tools::toTitleCase(measure)),
    is_overall = FALSE, stringsAsFactors = FALSE
  )
  if (!is.null(pooled)) out <- rbind(out, data.frame(
    label = "Bivariate model", data1 = paste0("k=", nrow(dat)), data2 = "",
    estimate = pooled[[1L]], ci_low = pooled[[2L]], ci_high = pooled[[3L]], weight = NA,
    effect_text = .format_effect_ci(pooled[[1L]], pooled[[2L]], pooled[[3L]], tools::toTitleCase(measure)),
    is_overall = TRUE, stringsAsFactors = FALSE
  ))
  out
}

.deeks_test <- function(dat, analysis, output_dir) {
  if (nrow(dat) < 10L) return(list(table = data.frame(), plots = character()))
  cells <- dat[, c("tp", "fp", "fn", "tn")]
  zero <- apply(cells, 1, function(x) any(x == 0))
  cells[zero, ] <- cells[zero, ] + 0.5
  log_dor <- log((cells$tp * cells$tn) / (cells$fp * cells$fn))
  n_disease <- cells$tp + cells$fn
  n_nondisease <- cells$fp + cells$tn
  ess <- 4 / (1 / n_disease + 1 / n_nondisease)
  inv_sqrt_ess <- 1 / sqrt(ess)
  fit <- stats::lm(log_dor ~ inv_sqrt_ess, weights = ess)
  coef <- summary(fit)$coefficients
  p_value <- coef["inv_sqrt_ess", "Pr(>|t|)"]
  df <- data.frame(inv_sqrt_ess = inv_sqrt_ess, log_dor = log_dor, ess = ess)
  pred <- data.frame(inv_sqrt_ess = seq(min(inv_sqrt_ess), max(inv_sqrt_ess), length.out = 100))
  pred$fit <- stats::predict(fit, newdata = pred)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = inv_sqrt_ess, y = log_dor)) +
    ggplot2::geom_point(ggplot2::aes(size = ess), shape = 21, fill = "#5B9BD5", colour = "#1F4E78") +
    ggplot2::geom_line(data = pred, ggplot2::aes(y = fit), colour = "#1F4E78", linewidth = 0.8) +
    ggplot2::scale_size_area(max_size = 10, guide = "none") +
    ggplot2::labs(
      title = paste0("Deeks funnel plot: ", analysis$outcome_name),
      subtitle = paste0("Asymmetry test p=", .format_number(p_value, 3)),
      x = "1 / sqrt(effective sample size)", y = "log diagnostic odds ratio"
    ) + ggplot2::theme_minimal(base_family = "sans", base_size = 10)
  base <- file.path(output_dir, "deeks_funnel")
  ggplot2::ggsave(paste0(base, ".png"), p, width = 8, height = 6, dpi = 300)
  ggplot2::ggsave(paste0(base, ".pdf"), p, width = 8, height = 6, device = "pdf")
  list(
    table = data.frame(
      analysis_id = analysis$analysis_id, test = "Deeks asymmetry",
      slope = coef["inv_sqrt_ess", "Estimate"], p_value = p_value, k = nrow(dat),
      stringsAsFactors = FALSE
    ),
    plots = paste0(base, c(".png", ".pdf"))
  )
}

.diagnostic_loo <- function(dat, analysis, output_dir, pooled_sens, pooled_spec) {
  if (nrow(dat) < 3L) return(list(table = data.frame(), model = NULL, plots = character()))
  rows <- list()
  for (i in seq_len(nrow(dat))) {
    d <- dat[-i, , drop = FALSE]
    mada_data <- data.frame(
      TP = d$tp, FN = d$fn, FP = d$fp, TN = d$tn,
      row.names = make.unique(d$study_label)
    )
    fit <- tryCatch(mada::reitsma(mada_data), error = function(e) NULL)
    if (is.null(fit)) next
    beta <- as.numeric(fit$coefficients)
    model_se <- sqrt(diag(fit$vcov))
    z <- stats::qnorm(0.975)
    sens <- c(
      stats::plogis(beta[[1L]]), stats::plogis(beta[[1L]] - z * model_se[[1L]]),
      stats::plogis(beta[[1L]] + z * model_se[[1L]])
    )
    fpr <- c(
      stats::plogis(beta[[2L]]), stats::plogis(beta[[2L]] - z * model_se[[2L]]),
      stats::plogis(beta[[2L]] + z * model_se[[2L]])
    )
    spec <- c(1 - fpr[[1L]], 1 - fpr[[3L]], 1 - fpr[[2L]])
    rows[[length(rows) + 1L]] <- data.frame(
      analysis_id = analysis$analysis_id, omitted_study = dat$study_label[[i]],
      effect_measure = c("Sensitivity", "Specificity"),
      estimate = c(sens[[1L]], spec[[1L]]),
      ci_low = c(sens[[2L]], spec[[2L]]),
      ci_high = c(sens[[3L]], spec[[3L]]), stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(list(table = data.frame(), model = NULL, plots = character()))
  table_out <- do.call(rbind, rows)
  plots <- character()
  for (measure in c("Sensitivity", "Specificity")) {
    d <- table_out[table_out$effect_measure == measure, , drop = FALSE]
    if (!nrow(d)) next
    plot_data <- data.frame(
      label = d$omitted_study, data1 = "Study omitted", data2 = "",
      estimate = d$estimate, ci_low = d$ci_low, ci_high = d$ci_high,
      weight = 1, effect_text = .format_effect_ci(d$estimate, d$ci_low, d$ci_high, measure),
      is_overall = FALSE, stringsAsFactors = FALSE
    )
    reference <- if (measure == "Sensitivity") pooled_sens[[1L]] else pooled_spec[[1L]]
    plots <- c(plots, .export_forest(
      plot_data, output_dir, paste0("leave_one_out_", tolower(measure)),
      paste0("Leave-one-out ", tolower(measure), ": ", analysis$outcome_name),
      measure, data1_header = "Scenario", data2_header = "", x_label = measure,
      null = 0, reference = reference
    ))
  }
  list(table = table_out, model = NULL, plots = plots)
}

.run_diagnostic_analysis_v2 <- function(data, analysis, output_dir) {
  .require_namespace("mada")
  dat <- data$diagnostic_data[
    data$diagnostic_data$analysis_id == analysis$analysis_id, , drop = FALSE
  ]
  metadata <- data$study_metadata
  metadata_drop <- c("analysis_id", "study_key", "source_sheet", "source_row")
  if ("study_label" %in% names(dat)) metadata_drop <- c(metadata_drop, "study_label")
  dat <- merge(dat, metadata[, setdiff(names(metadata), metadata_drop), drop = FALSE],
               by = "study_id", all.x = TRUE, sort = FALSE)
  dat$study_label[is.na(dat$study_label)] <- dat$study_id[is.na(dat$study_label)]
  mada_data <- data.frame(TP = dat$tp, FN = dat$fn, FP = dat$fp, TN = dat$tn, row.names = dat$study_label)
  model <- mada::reitsma(mada_data)
  beta <- as.numeric(model$coefficients)
  se <- sqrt(diag(model$vcov))
  z <- stats::qnorm(0.975)
  pooled_sens <- c(stats::plogis(beta[[1L]]), stats::plogis(beta[[1L]] - z * se[[1L]]), stats::plogis(beta[[1L]] + z * se[[1L]]))
  pooled_fpr <- c(stats::plogis(beta[[2L]]), stats::plogis(beta[[2L]] - z * se[[2L]]), stats::plogis(beta[[2L]] + z * se[[2L]]))
  pooled_spec <- c(1 - pooled_fpr[[1L]], 1 - pooled_fpr[[3L]], 1 - pooled_fpr[[2L]])
  summary <- data.frame(
    analysis_id = analysis$analysis_id, outcome_name = analysis$outcome_name,
    timepoint = analysis$timepoint, analysis_type = "diagnostic_ma",
    outcome_type = "diagnostic", effect_measure = c("Sensitivity", "Specificity"),
    method = "Bivariate Reitsma", k = nrow(dat),
    estimate = c(pooled_sens[[1L]], pooled_spec[[1L]]),
    ci_low = c(pooled_sens[[2L]], pooled_spec[[2L]]),
    ci_high = c(pooled_sens[[3L]], pooled_spec[[3L]]),
    stringsAsFactors = FALSE
  )
  sens_plot <- .export_forest(
    .diagnostic_forest_data(dat, "sensitivity", pooled_sens), output_dir,
    "forest_sensitivity", paste0("Sensitivity: ", analysis$outcome_name),
    "Sensitivity", data1_header = "TP / Diseased", data2_header = "", x_label = "Sensitivity", null = 0
  )
  spec_plot <- .export_forest(
    .diagnostic_forest_data(dat, "specificity", pooled_spec), output_dir,
    "forest_specificity", paste0("Specificity: ", analysis$outcome_name),
    "Specificity", data1_header = "TN / Non-diseased", data2_header = "", x_label = "Specificity", null = 0
  )
  base <- file.path(output_dir, "sroc")
  draw_sroc <- function() plot(model, sroclwd = 2, main = paste("SROC:", analysis$outcome_name))
  grDevices::png(paste0(base, ".png"), width = 1800, height = 1600, res = 250)
  draw_sroc(); grDevices::dev.off()
  grDevices::pdf(paste0(base, ".pdf"), width = 7.5, height = 7)
  draw_sroc(); grDevices::dev.off()
  deeks <- .deeks_test(dat, analysis, output_dir)
  loo <- .diagnostic_loo(dat, analysis, output_dir, pooled_sens, pooled_spec)
  method_decisions <- data.frame(
    analysis_id = analysis$analysis_id, decision = "primary_method",
    value = "mada_reitsma", reason = "Complete diagnostic 2x2 data",
    stringsAsFactors = FALSE
  )
  canonical <- data.frame(
    analysis_id = analysis$analysis_id, study_id = dat$study_id, study_label = dat$study_label,
    tp = dat$tp, fp = dat$fp, fn = dat$fn, tn = dat$tn,
    sensitivity = dat$tp / (dat$tp + dat$fn), specificity = dat$tn / (dat$tn + dat$fp),
    stringsAsFactors = FALSE
  )
  list(
    status = "SUCCESS", analysis = analysis, model = model, canonical = canonical,
    summary = summary, sensitivity = data.frame(),
    subgroup = list(tables = data.frame(), models = list(), plots = character()),
    meta_regression = list(tables = data.frame(), models = list(), plots = character()),
    loo = loo,
    funnel = deeks, method_decisions = method_decisions,
    plots = unique(c(
      sens_plot, spec_plot, paste0(base, c(".png", ".pdf")), loo$plots, deeks$plots
    ))
  )
}

.nma_treatment_counts <- function(canonical, data, analysis) {
  treatments <- unique(c(canonical$treat1, canonical$treat2))
  arms <- data$arm_data[data$arm_data$analysis_id == analysis$analysis_id, , drop = FALSE]
  rows <- lapply(treatments, function(trt) {
    studies <- unique(canonical$study_id[canonical$treat1 == trt | canonical$treat2 == trt])
    arm_unique <- arms[arms$treatment == trt, c("study_id", "treatment", "sample"), drop = FALSE]
    arm_unique <- arm_unique[
      !duplicated(paste(arm_unique$study_id, .treatment_key(arm_unique$treatment))),
      , drop = FALSE
    ]
    n <- sum(arm_unique$sample, na.rm = TRUE)
    data.frame(treatment = trt, k_studies = length(studies), participants = if (n > 0) n else NA_real_, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

.nma_transitivity_table <- function(canonical) {
  moderator_cols <- grep("^(subgroup_|moderator_)", names(canonical), value = TRUE)
  if (!length(moderator_cols)) return(data.frame())
  comparison <- paste(pmin(canonical$treat1, canonical$treat2), pmax(canonical$treat1, canonical$treat2), sep = " vs ")
  rows <- list()
  for (col in moderator_cols) {
    x <- canonical[[col]]
    for (cmp in unique(comparison)) {
      values <- x[comparison == cmp & !is.na(x)]
      if (!length(values)) next
      if (grepl("^moderator_num_", col)) {
        rows[[length(rows) + 1L]] <- data.frame(
          variable = col, comparison = cmp, n = length(values),
          summary = paste0("mean=", .format_number(mean(.as_num(values)), 2),
                           "; range=", .format_number(min(.as_num(values)), 2),
                           " to ", .format_number(max(.as_num(values)), 2)),
          stringsAsFactors = FALSE
        )
      } else {
        tab <- table(values)
        rows[[length(rows) + 1L]] <- data.frame(
          variable = col, comparison = cmp, n = length(values),
          summary = paste(names(tab), as.integer(tab), sep = "=", collapse = "; "),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

.nma_sensitivity <- function(all_sources, primary, analysis, small_values) {
  candidates <- list()
  for (source in unique(all_sources$source_type)) {
    if (is.na(source)) next
    candidates[[paste0("estimate_type=", source)]] <-
      all_sources[all_sources$source_type == source, , drop = FALSE]
  }
  if ("study_design" %in% names(primary)) {
    designs <- unique(.trim_chr(primary$study_design))
    designs <- designs[!is.na(designs)]
    if (length(designs) > 1L) {
      for (design in designs) {
        candidates[[paste0("study_design=", design)]] <-
          primary[.trim_chr(primary$study_design) == design, , drop = FALSE]
      }
    }
  }
  rows <- list()
  for (scenario in names(candidates)) {
    dat <- candidates[[scenario]]
    dat <- dat[is.finite(dat$TE) & is.finite(dat$seTE) & dat$seTE > 0, , drop = FALSE]
    if (length(unique(dat$study_id)) < 2L) next
    treatments <- unique(c(dat$treat1, dat$treat2))
    edges <- dat[, c("treat1", "treat2"), drop = FALSE]
    if (!.network_connected(treatments, edges, analysis$reference_treatment)) next
    fit <- tryCatch(
      netmeta::netmeta(
        TE = dat$TE, seTE = dat$seTE, treat1 = dat$treat1, treat2 = dat$treat2,
        studlab = dat$study_id, sm = analysis$effect_measure,
        common = FALSE, random = TRUE, prediction = FALSE, method.tau = "REML",
        reference.group = analysis$reference_treatment, small.values = small_values,
        details.chkmultiarm = TRUE
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    reference <- analysis$reference_treatment
    for (treatment in setdiff(rownames(fit$TE.random), reference)) {
      te <- fit$TE.random[treatment, reference]
      lower <- fit$lower.random[treatment, reference]
      upper <- fit$upper.random[treatment, reference]
      rows[[length(rows) + 1L]] <- data.frame(
        analysis_id = analysis$analysis_id, scenario = scenario,
        treatment = treatment, reference_treatment = reference,
        effect_measure = analysis$effect_measure,
        estimate = .natural_effect(te, analysis$effect_measure),
        ci_low = .natural_effect(lower, analysis$effect_measure),
        ci_high = .natural_effect(upper, analysis$effect_measure),
        k = length(unique(dat$study_id)),
        method = "Sensitivity NMA inverse-variance REML", stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

.run_nma_analysis_v2 <- function(data, analysis, output_dir) {
  .require_namespace("netmeta")
  bundle <- .canonicalize_analysis(data, analysis)
  canonical <- bundle$primary
  if (length(unique(canonical$study_id)) < 2L) stop("Kurang dari dua studi NMA valid.", call. = FALSE)
  raw_only <- all(canonical$source_type == "raw_derived")
  small_values <- if (analysis$outcome_direction == "higher_better") "undesirable" else "desirable"
  if (analysis$outcome_type == "binary" && raw_only) {
    raw_dat <- .raw_binary_pair_data(data$arm_data, analysis, unique(canonical$study_id))
    model <- netmeta::netmetabin(
      event1 = raw_dat$event1, n1 = raw_dat$sample1,
      event2 = raw_dat$event2, n2 = raw_dat$sample2,
      treat1 = raw_dat$treat1, treat2 = raw_dat$treat2, studlab = raw_dat$study_id,
      sm = analysis$effect_measure, method = "LRP", common = TRUE, random = TRUE,
      prediction = TRUE, reference.group = analysis$reference_treatment,
      small.values = small_values, allstudies = TRUE
    )
    method_name <- "Penalized logistic regression network meta-analysis"
    method_value <- "netmetabin_LRP_random"
  } else {
    model <- netmeta::netmeta(
      TE = canonical$TE, seTE = canonical$seTE,
      treat1 = canonical$treat1, treat2 = canonical$treat2,
      studlab = canonical$study_id, sm = analysis$effect_measure,
      common = TRUE, random = TRUE, prediction = TRUE, method.tau = "REML",
      reference.group = analysis$reference_treatment, small.values = small_values,
      details.chkmultiarm = TRUE
    )
    method_name <- "Inverse-variance NMA random-effects (REML)"
    method_value <- "netmeta_inverse_REML"
  }
  treatments <- rownames(model$TE.random)
  reference <- analysis$reference_treatment
  treatments <- setdiff(treatments, reference)
  rank <- if (analysis$outcome_direction == "neutral") NULL else
    tryCatch(netmeta::netrank(model, small.values = small_values), error = function(e) NULL)
  counts <- .nma_treatment_counts(canonical, data, analysis)
  rows <- lapply(treatments, function(trt) {
    te <- model$TE.random[trt, reference]
    low <- model$lower.random[trt, reference]
    high <- model$upper.random[trt, reference]
    count <- counts[counts$treatment == trt, , drop = FALSE]
    data.frame(
      analysis_id = analysis$analysis_id, outcome_name = analysis$outcome_name,
      timepoint = analysis$timepoint, treatment = trt, reference_treatment = reference,
      effect_measure = analysis$effect_measure, method = method_name,
      TE = te, seTE = model$seTE.random[trt, reference], lower = low, upper = high,
      estimate = .natural_effect(te, analysis$effect_measure),
      ci_low = .natural_effect(low, analysis$effect_measure),
      ci_high = .natural_effect(high, analysis$effect_measure),
      k_studies = if (nrow(count)) count$k_studies else NA,
      participants = if (nrow(count)) count$participants else NA,
      pscore = if (!is.null(rank)) unname(rank$Pscore.random[[trt]]) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  summary <- do.call(rbind, rows)
  plots <- .export_nma_forest_v1(
    model, counts, rank, analysis, output_dir, reference
  )

  network_base <- file.path(output_dir, "network_graph")
  draw_network <- function() netmeta::netgraph(
    model, thickness = "number.of.studies", number.of.studies = TRUE,
    points = TRUE, cex.points = 4, cex = 0.9, plastic = FALSE,
    col = "#1F4E78", col.points = "#5B9BD5"
  )
  grDevices::png(paste0(network_base, ".png"), width = 2000, height = 1800, res = 250)
  draw_network(); grDevices::dev.off()
  grDevices::pdf(paste0(network_base, ".pdf"), width = 8, height = 7.2)
  draw_network(); grDevices::dev.off()
  plots <- c(plots, paste0(network_base, c(".png", ".pdf")))

  league <- tryCatch(netmeta::netleague(model, random = TRUE, digits = 3)$random, error = function(e) data.frame())
  split <- tryCatch(netmeta::netsplit(model), error = function(e) NULL)
  split_table <- if (!is.null(split) && is.data.frame(split$random)) split$random else data.frame()
  design <- tryCatch(suppressWarnings(netmeta::decomp.design(model)), error = function(e) NULL)
  inconsistency <- if (!is.null(design) && is.data.frame(design$Q.decomp)) design$Q.decomp else data.frame()
  transitivity <- .nma_transitivity_table(canonical)
  sensitivity <- .nma_sensitivity(bundle$all_sources, canonical, analysis, small_values)
  method_decisions <- data.frame(
    analysis_id = analysis$analysis_id,
    decision = c("primary_method", "reference_treatment", "small_values"),
    value = c(method_value, reference, small_values),
    reason = c(if (raw_only) "Arm-level raw data" else "Reported or mixed effect sources",
               "Explicit workbook setting", "Derived from outcome_direction"),
    stringsAsFactors = FALSE
  )
  list(
    status = "SUCCESS", analysis = analysis, model = model,
    canonical = canonical, all_sources = bundle$all_sources,
    summary = summary, sensitivity = sensitivity,
    league_table = league, ranking = if (is.null(rank)) data.frame() else data.frame(
      treatment = names(rank$Pscore.random), pscore = unname(rank$Pscore.random), stringsAsFactors = FALSE
    ),
    netsplit = split_table, inconsistency = inconsistency, transitivity = transitivity,
    subgroup = list(tables = data.frame(), models = list(), plots = character()),
    meta_regression = list(tables = data.frame(), models = list(), plots = character()),
    loo = list(table = data.frame(), model = NULL, plots = character()),
    funnel = list(table = data.frame(), model = NULL, plots = character()),
    method_decisions = method_decisions, plots = unique(plots)
  )
}

# =============================================================================
# OUTPUT BUNDLE AND ONE-LINE RUNNER
# =============================================================================

.rbind_fill <- function(xs) {
  xs <- xs[vapply(xs, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
  if (!length(xs)) return(data.frame())
  columns <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    for (nm in setdiff(columns, names(x))) x[[nm]] <- NA
    x[, columns, drop = FALSE]
  })
  out <- do.call(rbind, xs)
  rownames(out) <- NULL
  out
}

.user_output_table <- function(dat, outcome_name = NULL) {
  if (!is.data.frame(dat)) return(data.frame())
  if (nrow(dat) && !is.null(outcome_name) && !"outcome_name" %in% names(dat)) {
    dat$outcome_name <- outcome_name
  }
  internal <- c(
    "analysis_id", "study_id", "outcome_key", "study_key",
    "canonical_outcome_name", "canonical_study_label"
  )
  dat[, setdiff(names(dat), internal), drop = FALSE]
}

.flatten_result_tables <- function(results, diagnostics) {
  result_list <- unname(results)
  collect <- function(extractor) {
    .rbind_fill(lapply(result_list, function(x) {
      .user_output_table(extractor(x), x$analysis$outcome_name)
    }))
  }
  pooled_results <- collect(function(x) x$summary %||% data.frame())
  run_summary <- .rbind_fill(lapply(names(results), function(outcome_name) {
    res <- results[[outcome_name]]
    data.frame(
      outcome_name = outcome_name,
      status = res$status %||% "FAILED",
      analysis_type = res$analysis$analysis_type %||% NA,
      method = if (!is.null(res$summary) && nrow(res$summary)) .first_nonmissing(res$summary$method) else NA,
      plots_created = length(res$plots %||% character()),
      stringsAsFactors = FALSE
    )
  }))
  list(
    RUN_SUMMARY = run_summary,
    ANALYSIS_RESULTS = pooled_results,
    HETEROGENEITY = if (nrow(pooled_results)) pooled_results[, intersect(
      c("outcome_name", "effect_measure", "method", "k", "tau2", "i2_percent",
        "prediction_low", "prediction_high"), names(pooled_results)
    ), drop = FALSE] else data.frame(),
    STUDY_EFFECTS = collect(function(x) x$canonical %||% data.frame()),
    SENSITIVITY = collect(function(x) x$sensitivity %||% data.frame()),
    SUBGROUP = collect(function(x) x$subgroup$tables %||% data.frame()),
    SOURCE_SUBGROUP = .rbind_fill(lapply(result_list, function(x) {
      effect_measure <- .first_nonmissing(
        c(x$analysis$effect_measure, x$summary$effect_measure), "Effect"
      )
      .user_output_table(
        .markdown_natural_effect_table(
          x$source_subgroup$tables %||% data.frame(), effect_measure
        ),
        x$analysis$outcome_name
      )
    })),
    META_REGRESSION = collect(function(x) x$meta_regression$tables %||% data.frame()),
    LEAVE_ONE_OUT = collect(function(x) x$loo$table %||% data.frame()),
    FUNNEL_TESTS = collect(function(x) x$funnel$table %||% data.frame()),
    NMA_RANKING = collect(function(x) {
      d <- x$ranking %||% data.frame()
      d
    }),
    NMA_NETSPLIT = collect(function(x) {
      d <- x$netsplit %||% data.frame()
      d
    }),
    NMA_INCONSISTENCY = collect(function(x) {
      d <- x$inconsistency %||% data.frame()
      d
    }),
    TRANSITIVITY = collect(function(x) {
      d <- x$transitivity %||% data.frame()
      d
    }),
    METHOD_DECISIONS = collect(function(x) x$method_decisions %||% data.frame()),
    WARNINGS = .user_diagnostics(diagnostics[diagnostics$severity %in% c("WARNING", "INFO"), , drop = FALSE]),
    DIAGNOSTICS = .user_diagnostics(diagnostics)
  )
}

.write_results_workbook <- function(tables, results, output_path) {
  .require_namespace("openxlsx")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  wb <- openxlsx::createWorkbook(creator = "Auctus")
  header_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 10, fontColour = "#FFFFFF", fgFill = "#1F4E78",
    textDecoration = "bold", halign = "center", valign = "center", wrapText = TRUE
  )
  body_style <- openxlsx::createStyle(fontName = "Arial", fontSize = 10, valign = "top")
  for (sheet in names(tables)) {
    dat <- tables[[sheet]]
    if (!is.data.frame(dat)) next
    if (!ncol(dat) || !nrow(dat)) {
      dat <- data.frame(message = "Tidak ada hasil yang memenuhi syarat.", stringsAsFactors = FALSE)
    }
    sheet_name <- substr(sheet, 1, 31)
    openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)
    openxlsx::writeDataTable(wb, sheet_name, dat, tableStyle = "TableStyleMedium2", withFilter = TRUE)
    openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
    if (nrow(dat)) openxlsx::addStyle(wb, sheet_name, body_style, rows = 2:(nrow(dat) + 1L), cols = seq_len(ncol(dat)), gridExpand = TRUE)
    openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 2)
    openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(dat)), widths = "auto")
    long <- match(c("message", "suggestion", "method", "reason"), names(dat), nomatch = 0L)
    long <- long[long > 0L]
    if (length(long)) openxlsx::setColWidths(wb, sheet_name, cols = long, widths = 42)
  }
  for (outcome_name in names(results)) {
    league <- results[[outcome_name]]$league_table %||% data.frame()
    if (is.data.frame(league) && nrow(league)) {
      sheet_name <- substr(
        paste0("LEAGUE_", .analysis_output_key(results[[outcome_name]]$analysis)),
        1, 31
      )
      openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)
      league_out <- cbind(Treatment = rownames(league), league, stringsAsFactors = FALSE)
      openxlsx::writeDataTable(wb, sheet_name, league_out, tableStyle = "TableStyleMedium2")
      openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 2)
      openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(league_out)), widths = "auto")
    }
  }
  openxlsx::modifyBaseFont(wb, fontName = "Arial", fontSize = 10, fontColour = "#000000")
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  .repair_openxlsx_comment_relationships(output_path)
  normalizePath(output_path, mustWork = FALSE)
}

.html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

.html_table <- function(dat, max_rows = 100L) {
  if (!is.data.frame(dat) || !nrow(dat)) return("<p class='muted'>Tidak ada data.</p>")
  dat <- dat[seq_len(min(max_rows, nrow(dat))), , drop = FALSE]
  header <- paste0("<th>", .html_escape(names(dat)), "</th>", collapse = "")
  body <- apply(dat, 1, function(row) paste0("<tr>", paste0("<td>", .html_escape(ifelse(is.na(row), "", row)), "</td>", collapse = ""), "</tr>"))
  paste0("<div class='table-wrap'><table><thead><tr>", header, "</tr></thead><tbody>",
         paste(body, collapse = ""), "</tbody></table></div>")
}

.html_image_data_uri <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  size <- suppressWarnings(as.numeric(file.info(path)$size))
  if (!is.finite(size) || size <= 0) return(NA_character_)
  raw_image <- readBin(path, what = "raw", n = size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(raw_image))
}

.write_html_report <- function(results, diagnostics, output_path, run_root) {
  .require_namespace("jsonlite")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  sections <- character()
  for (outcome_name in names(results)) {
    res <- results[[outcome_name]]
    effect_measure <- .first_nonmissing(
      c(res$analysis$effect_measure, res$summary$effect_measure), "Effect"
    )
    summary_html <- .html_table(.user_output_table(res$summary %||% data.frame()), 30)
    source_html <- .html_table(
      .user_output_table(.markdown_natural_effect_table(
        res$source_subgroup$tables %||% data.frame(), effect_measure
      )), 50
    )
    plot_paths <- res$plots %||% character()
    pngs <- plot_paths[tolower(tools::file_ext(plot_paths)) == "png"]
    image_blocks <- lapply(pngs, function(plot_path) {
      source <- .html_image_data_uri(plot_path)
      if (is.na(source)) return("")
      plot_label <- tools::toTitleCase(gsub(
        "_", " ", tools::file_path_sans_ext(basename(plot_path))
      ))
      paste0(
        "<figure><img loading='lazy' src='", source, "' alt='Plot ",
        .html_escape(outcome_name), " - ", .html_escape(plot_label), "'>",
        "<figcaption>", .html_escape(plot_label), "</figcaption></figure>"
      )
    })
    image_blocks <- unlist(image_blocks, use.names = FALSE)
    image_blocks <- image_blocks[nzchar(image_blocks)]
    images <- if (length(image_blocks)) paste0(
      "<div class='plots'>", paste(image_blocks, collapse = ""), "</div>"
    ) else "<p class='muted'>Plot tidak ditemukan.</p>"
    sections <- c(sections, paste0(
      "<section><h2>", .html_escape(outcome_name), "</h2>",
      summary_html,
      if (nrow(res$source_subgroup$tables %||% data.frame())) {
        paste0("<h3>Source subgroup</h3>", source_html)
      } else {
        ""
      },
      images, "</section>"
    ))
  }
  diagnostics_html <- .html_table(.user_diagnostics(diagnostics), 200)
  html <- paste0(
    "<!doctype html><html lang='id'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width,initial-scale=1'>",
    "<title>Auctus MA & NMA Report</title><style>",
    "body{font-family:Arial,sans-serif;color:#222;margin:0;background:#f5f7fa}",
    "main{max-width:1200px;margin:0 auto;padding:28px}h1,h2{color:#1F4E78}",
    "section{background:white;margin:18px 0;padding:22px;border-radius:8px;box-shadow:0 1px 5px #ccd}",
    ".table-wrap{overflow:auto}table{border-collapse:collapse;width:100%;font-size:12px}",
    "th{background:#1F4E78;color:white;position:sticky;top:0}th,td{border:1px solid #ddd;padding:7px;text-align:left}",
    "tr:nth-child(even){background:#f3f6fa}.plots{display:grid;grid-template-columns:repeat(auto-fit,minmax(420px,1fr));gap:16px}",
    "figure{margin:0}figcaption{font-size:12px;color:#555;margin:6px 0 12px;text-align:center}",
    "img{display:block;max-width:100%;height:auto;border:1px solid #ddd}.muted{color:#666}</style></head><body><main>",
    "<h1>Auctus MA & NMA Report</h1>",
    "<p>Engine ", .AUCTUS_ENGINE_VERSION, " | Dibuat ", .html_escape(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")), "</p>",
    paste(sections, collapse = ""),
    "<section><h2>Diagnostics</h2>", diagnostics_html, "</section>",
    "</main></body></html>"
  )
  writeLines(html, output_path, useBytes = TRUE)
  normalizePath(output_path, mustWork = FALSE)
}

.markdown_escape <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("|", "\\|", x, fixed = TRUE)
  gsub("[\r\n]+", " ", x)
}

.markdown_table <- function(dat, max_rows = 100L) {
  dat <- .user_output_table(dat)
  if (!is.data.frame(dat) || !nrow(dat) || !ncol(dat)) return("_Tidak ada data._")
  dat <- dat[seq_len(min(max_rows, nrow(dat))), , drop = FALSE]
  dat[] <- lapply(dat, function(x) {
    if (is.list(x)) vapply(x, function(y) paste(y, collapse = ", "), character(1)) else x
  })
  header <- paste0("| ", paste(.markdown_escape(names(dat)), collapse = " | "), " |")
  separator <- paste0("| ", paste(rep("---", ncol(dat)), collapse = " | "), " |")
  rows <- apply(dat, 1L, function(row) {
    paste0("| ", paste(.markdown_escape(row), collapse = " | "), " |")
  })
  paste(c(header, separator, rows), collapse = "\n")
}

.markdown_analysis_config <- function(analysis) {
  keep <- setdiff(names(analysis), c("analysis_id", "outcome_key"))
  values <- vapply(analysis[keep], function(x) {
    if (!length(x) || all(is.na(x))) "" else paste(x, collapse = ", ")
  }, character(1))
  data.frame(field = keep, value = values, stringsAsFactors = FALSE)
}

.markdown_natural_effect_table <- function(dat, effect_measure) {
  if (!is.data.frame(dat) || !nrow(dat)) return(data.frame())
  if (!all(c("TE", "lower", "upper") %in% names(dat))) return(dat)
  transform <- if (.ratio_measure(effect_measure)) exp else identity
  dat$estimate <- transform(dat$TE)
  dat$ci_low <- transform(dat$lower)
  dat$ci_high <- transform(dat$upper)
  dat$effect_measure <- effect_measure
  dat[, c(
    intersect(c("subgroup_variable", "subgroup", "omitted_study", "effect_measure",
                "estimate", "ci_low", "ci_high", "p_value", "k", "tau2",
                "i2_percent", "test_for_difference_p"), names(dat)),
    setdiff(names(dat), c(
      "TE", "seTE", "lower", "upper", "subgroup_variable", "subgroup",
      "omitted_study", "effect_measure", "estimate", "ci_low", "ci_high",
      "p_value", "k", "tau2", "i2_percent", "test_for_difference_p"
    ))
  ), drop = FALSE]
}

.write_markdown_report <- function(results, diagnostics, output_path, run_root) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  lines <- c(
    "# Auctus MA & NMA Report",
    "",
    paste0("- Engine: `", .AUCTUS_ENGINE_VERSION, "`"),
    paste0("- Schema: `", .AUCTUS_SCHEMA_VERSION, "`"),
    paste0("- Dibuat: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "## Daftar outcome",
    "",
    paste0(seq_along(results), ". [", .markdown_escape(names(results)), "](#",
           gsub("(^-|-$)", "", gsub("[^a-z0-9]+", "-", tolower(names(results)))), ")"),
    ""
  )
  add_table_section <- function(title, dat, max_rows = 100L) {
    c(paste0("### ", title), "", .markdown_table(dat, max_rows), "")
  }
  for (outcome_name in names(results)) {
    res <- results[[outcome_name]]
    effect_measure <- .first_nonmissing(
      c(res$analysis$effect_measure, res$summary$effect_measure), "Effect"
    )
    subgroup_table <- .markdown_natural_effect_table(
      res$subgroup$tables %||% data.frame(), effect_measure
    )
    source_subgroup_table <- .markdown_natural_effect_table(
      res$source_subgroup$tables %||% data.frame(), effect_measure
    )
    loo_table <- .markdown_natural_effect_table(
      res$loo$table %||% data.frame(), effect_measure
    )
    lines <- c(
      lines,
      paste0("## ", .markdown_escape(outcome_name)),
      "",
      paste0("Status: **", .markdown_escape(res$status %||% "UNKNOWN"), "**"),
      "",
      add_table_section("Konfigurasi analisis", .markdown_analysis_config(res$analysis)),
      add_table_section("Overall result", res$summary %||% data.frame(), 30L),
      add_table_section("Sensitivity analysis", res$sensitivity %||% data.frame()),
      add_table_section("Subgroup analysis", subgroup_table),
      add_table_section("Source subgroup", source_subgroup_table),
      add_table_section("Meta-regression", res$meta_regression$tables %||% data.frame()),
      add_table_section("Leave-one-out analysis", loo_table),
      add_table_section("Publication bias", res$funnel$table %||% data.frame()),
      add_table_section("NMA ranking", res$ranking %||% data.frame()),
      add_table_section("NMA node splitting", res$netsplit %||% data.frame()),
      add_table_section("NMA inconsistency", res$inconsistency %||% data.frame()),
      add_table_section("Transitivity check", res$transitivity %||% data.frame()),
      add_table_section("Method decisions", res$method_decisions %||% data.frame())
    )
    league <- res$league_table %||% data.frame()
    if (is.matrix(league)) league <- as.data.frame(league, stringsAsFactors = FALSE)
    if (is.data.frame(league) && nrow(league)) {
      league <- cbind(Treatment = rownames(league), league, stringsAsFactors = FALSE)
      rownames(league) <- NULL
      lines <- c(lines, add_table_section("NMA league table", league))
    }
    pngs <- (res$plots %||% character())[
      tolower(tools::file_ext(res$plots %||% character())) == "png"
    ]
    lines <- c(lines, "### Plot", "")
    if (length(pngs)) {
      for (plot_path in pngs) {
        relative <- substring(normalizePath(plot_path, mustWork = FALSE), nchar(run_root) + 2L)
        label <- gsub("_", " ", tools::file_path_sans_ext(basename(plot_path)))
        lines <- c(
          lines,
          paste0("#### ", tools::toTitleCase(label)),
          "",
          paste0("![", .markdown_escape(outcome_name), " - ",
                 .markdown_escape(label), "](../", relative, ")"),
          ""
        )
      }
    } else {
      lines <- c(lines, "_Tidak ada plot._", "")
    }
  }
  lines <- c(
    lines,
    "## Diagnostics",
    "",
    .markdown_table(.user_diagnostics(diagnostics), 300L)
  )
  writeLines(lines, output_path, useBytes = TRUE)
  normalizePath(output_path, mustWork = FALSE)
}

.package_manifest <- function() {
  deps <- check_auctus_dependencies(FALSE)
  stats::setNames(as.list(deps$version), deps$package)
}

.write_manifest <- function(file_path, results, diagnostics, output_path, run_mode, id_map) {
  .require_namespace("jsonlite")
  .require_namespace("digest")
  manifest <- list(
    engine_version = .AUCTUS_ENGINE_VERSION,
    schema_version = .AUCTUS_SCHEMA_VERSION,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    input_file = normalizePath(file_path),
    input_sha256 = digest::digest(file = file_path, algo = "sha256"),
    run_mode = run_mode,
    r_version = R.version.string,
    platform = R.version$platform,
    packages = .package_manifest(),
    id_mappings = list(
      analyses = id_map[id_map$entity_type == "analysis", c("internal_id", "label"), drop = FALSE],
      studies = id_map[id_map$entity_type == "study", c("internal_id", "label"), drop = FALSE]
    ),
    analyses = lapply(results, function(x) list(
      analysis_id = x$analysis$analysis_id,
      outcome_name = x$analysis$outcome_name,
      status = x$status,
      analysis_type = x$analysis$analysis_type,
      forest_mode = x$source_subgroup$forest_mode %||% NA_character_,
      configuration = x$analysis,
      method_decisions = x$method_decisions
    )),
    diagnostics = list(
      errors = sum(diagnostics$severity == "ERROR"),
      warnings = sum(diagnostics$severity == "WARNING"),
      info = sum(diagnostics$severity == "INFO")
    )
  )
  jsonlite::write_json(manifest, output_path, pretty = TRUE, auto_unbox = TRUE, na = "null")
  normalizePath(output_path, mustWork = FALSE)
}

.is_v2_workbook <- function(file_path) {
  .detect_auctus_schema(file_path) %in% c("v2_id", "v22_label", "v23_label")
}

.is_legacy_workbook <- function(file_path) {
  sheets <- .clean_names(readxl::excel_sheets(file_path))
  any(c("dikotomi", "kontinyu", "diagnostik", "single_arm") %in% sheets)
}

run_auctus_meta <- function(file_path = file.choose(), output_dir = NULL,
                            run_mode = c("strict", "valid_only")) {
  run_mode <- match.arg(run_mode)
  started_at <- Sys.time()
  .ensure_auctus_dependencies()
  file_path <- normalizePath(path.expand(file_path), mustWork = TRUE)
  run_root <- .default_run_root(file_path, output_dir)

  source_file <- file_path
  schema <- .detect_auctus_schema(file_path)
  if (schema %in% c("v1", "v2_id", "v22_label")) {
    converted <- file.path(run_root, "00_validation", "converted_v23.xlsx")
    dir.create(dirname(converted), recursive = TRUE, showWarnings = FALSE)
    source_file <- convert_legacy_workbook(file_path, converted)
    message("Template lama dikonversi. Validasi dilanjutkan pada schema 2.3.")
  } else if (!identical(schema, "v23_label")) {
    stop("Workbook tidak sesuai template Auctus V1, V2, V2.2, atau V2.3 yang dikenali.", call. = FALSE)
  }

  validation <- validate_auctus_data(source_file, output_dir = run_root, write_workbook = TRUE)
  print(validation)
  global_error_codes <- c(
    "E003_MISSING_SHEET", "E202_DUPLICATE_OUTCOME_NAME",
    "E203_UNKNOWN_OUTCOME_NAME", "E204_DUPLICATE_STUDY_LABEL",
    "E300_NO_ANALYSIS"
  )
  global_errors <- validation$diagnostics$severity == "ERROR" & (
    is.na(validation$diagnostics$outcome_name) |
      validation$diagnostics$error_code %in% global_error_codes
  )
  if (any(global_errors) || (run_mode == "strict" && !validation$valid)) {
    stop(
      paste0(
        "Analisis dihentikan karena validasi menemukan error.\n",
        "Perbaiki file: ", validation$checked_workbook, "\n",
        "Setelah diperbaiki, jalankan kembali hasil <- run_auctus_meta()."
      ),
      call. = FALSE
    )
  }

  data <- .normalize_workbook_data(validation$workbook)
  analyses <- data$analyses
  if (run_mode == "valid_only" && length(validation$error_outcome_names)) {
    bad_outcome_keys <- .normalize_label_key(validation$error_outcome_names)
    analyses <- analyses[!.normalize_label_key(analyses$outcome_name) %in% bad_outcome_keys, , drop = FALSE]
  }
  results <- list()
  runtime_diag <- .new_diagnostics()

  for (i in seq_len(nrow(analyses))) {
    analysis <- as.list(analyses[i, , drop = FALSE])
    aid <- analysis$analysis_id
    outcome_name <- analysis$outcome_name
    plot_dir <- file.path(run_root, "02_plots", .analysis_output_key(analysis))
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
    message("[Auctus] Menjalankan ", outcome_name, " (", analysis$analysis_type, ")")
    result <- tryCatch(
      switch(analysis$analysis_type,
        pairwise_ma = .run_pairwise_analysis(data, analysis, plot_dir),
        nma = .run_nma_analysis_v2(data, analysis, plot_dir),
        proportion_ma = .run_single_arm_analysis_v2(data, analysis, plot_dir),
        diagnostic_ma = .run_diagnostic_analysis_v2(data, analysis, plot_dir),
        stop("analysis_type tidak dikenali: ", analysis$analysis_type)
      ),
      error = function(e) {
        runtime_diag <<- .add_diag(
          runtime_diag, "ERROR", "E901_MODEL_FAILED", sheet = "analyses",
          outcome_name = outcome_name, message = conditionMessage(e),
          suggestion = "Periksa METHOD_DECISIONS, versi package, dan data untuk outcome_name ini."
        )
        list(status = "FAILED", analysis = analysis, error = conditionMessage(e),
             summary = data.frame(), canonical = data.frame(), sensitivity = data.frame(),
             subgroup = list(tables = data.frame(), plots = character()),
             meta_regression = list(tables = data.frame(), plots = character()),
             loo = list(table = data.frame(), plots = character()),
             funnel = list(table = data.frame(), plots = character()),
             method_decisions = data.frame(), plots = character())
      }
    )
    results[[outcome_name]] <- result
  }

  diagnostics <- .bind_diag(validation$diagnostics, runtime_diag)
  result_dir <- file.path(run_root, "01_results")
  report_dir <- file.path(run_root, "03_report")
  log_dir <- file.path(run_root, "04_logs")
  dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  tables <- .flatten_result_tables(results, diagnostics)
  results_xlsx <- .write_results_workbook(tables, results, file.path(result_dir, "results.xlsx"))
  saveRDS(results, file.path(result_dir, "analysis_objects.rds"))
  id_map <- rbind(
    data.frame(
      entity_type = "analysis", internal_id = data$analyses$analysis_id,
      label = data$analyses$outcome_name, stringsAsFactors = FALSE
    ),
    data.frame(
      entity_type = "study", internal_id = data$study_metadata$study_id,
      label = data$study_metadata$study_label, stringsAsFactors = FALSE
    )
  )
  id_map_path <- file.path(log_dir, "id_map.csv")
  utils::write.csv(id_map, id_map_path, row.names = FALSE, na = "")
  manifest <- .write_manifest(
    source_file, results, diagnostics, file.path(result_dir, "manifest.json"),
    run_mode, id_map
  )
  report <- .write_html_report(results, diagnostics, file.path(report_dir, "report.html"), run_root)
  report_markdown <- .write_markdown_report(
    results, diagnostics, file.path(report_dir, "report.md"), run_root
  )
  run_log <- data.frame(
    started_at = format(started_at, "%Y-%m-%d %H:%M:%S %Z"),
    finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    input_file = source_file, run_mode = run_mode,
    analyses_requested = nrow(analyses),
    analyses_succeeded = sum(vapply(results, function(x) identical(x$status, "SUCCESS"), logical(1))),
    analyses_failed = sum(vapply(results, function(x) identical(x$status, "FAILED"), logical(1))),
    errors = sum(diagnostics$severity == "ERROR"),
    warnings = sum(diagnostics$severity == "WARNING"),
    output_folder = run_root, stringsAsFactors = FALSE
  )
  utils::write.csv(run_log, file.path(log_dir, "run_log.csv"), row.names = FALSE)

  out <- structure(list(
    status = if (run_log$analyses_failed == 0L) "SUCCESS" else "COMPLETED_WITH_FAILURES",
    source_file = source_file, run_root = run_root,
    validation = validation, diagnostics = .user_diagnostics(diagnostics),
    analyses = results, tables = tables,
    files = list(
      validated_input = validation$checked_workbook,
      error_log = validation$error_log,
      results_workbook = results_xlsx,
      manifest = manifest, report = report,
      report_html = report, report_markdown = report_markdown,
      run_log = file.path(log_dir, "run_log.csv"), id_map = id_map_path
    )
  ), class = "auctus_run")
  print(out)
  invisible(out)
}

print.auctus_run <- function(x, ...) {
  success <- sum(vapply(x$analyses, function(a) identical(a$status, "SUCCESS"), logical(1)))
  failed <- sum(vapply(x$analyses, function(a) identical(a$status, "FAILED"), logical(1)))
  cat("\n============================================================\n")
  cat("AUCTUS MA & NMA ENGINE V", .AUCTUS_ENGINE_VERSION, "\n", sep = "")
  cat("Status  :", x$status, "\n")
  cat("Berhasil:", success, "analysis | Gagal:", failed, "analysis\n")
  cat("Hasil   :", x$run_root, "\n")
  cat("HTML    :", x$files$report_html %||% x$files$report, "\n")
  cat("Markdown:", x$files$report_markdown %||% "Tidak dibuat", "\n")
  cat("============================================================\n")
  invisible(x)
}

run_ma_analyses <- function(file_path = file.choose(), output_dir = NULL,
                            run_mode = c("strict", "valid_only"), ...) {
  warning(
    "run_ma_analyses() dipertahankan untuk kompatibilitas. Gunakan run_auctus_meta() untuk workflow V2.3.",
    call. = FALSE
  )
  run_auctus_meta(file_path = file_path, output_dir = output_dir, run_mode = match.arg(run_mode))
}

run_nma_analyses <- function(file_path = file.choose(), output_dir = NULL,
                             run_mode = c("strict", "valid_only"), ...) {
  warning(
    "NMA sekarang dirouting melalui analysis_type pada workbook V2.3. Gunakan run_auctus_meta().",
    call. = FALSE
  )
  run_auctus_meta(file_path = file_path, output_dir = output_dir, run_mode = match.arg(run_mode))
}

.public_symbols <- c(
  "check_auctus_dependencies", "create_auctus_template", "convert_legacy_workbook",
  "validate_auctus_data", "run_auctus_meta", "run_ma_analyses", "run_nma_analyses",
  "print.auctus_dependency_check", "print.auctus_validation", "print.auctus_run"
)
.target_environment <- parent.env(environment())
for (.symbol in .public_symbols) {
  assign(.symbol, get(.symbol, inherits = FALSE), envir = .target_environment)
}
invisible(NULL)
})
