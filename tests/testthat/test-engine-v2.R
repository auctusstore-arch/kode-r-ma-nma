testthat::test_that("source has no session side effects", {
  old_wd <- getwd()
  old_device <- grDevices::dev.cur()
  env <- new.env(parent = globalenv())
  sys.source(engine_path, envir = env)
  testthat::expect_identical(getwd(), old_wd)
  testthat::expect_identical(grDevices::dev.cur(), old_device)
  expected_api <- c(
    "check_auctus_dependencies", "create_auctus_template", "convert_legacy_workbook",
    "validate_auctus_data", "run_auctus_meta", "run_ma_analyses", "run_nma_analyses",
    "print.auctus_dependency_check", "print.auctus_validation", "print.auctus_run"
  )
  testthat::expect_setequal(ls(env, all.names = TRUE), expected_api)
  testthat::expect_false(exists(".normalize_workbook_data", envir = env, inherits = FALSE))
})

testthat::test_that("dependency bootstrap installs only unresolved packages and rechecks", {
  state <- new.env(parent = emptyenv())
  state$installed <- FALSE
  state$requested <- character()

  checker <- function(stop_on_missing = FALSE) {
    status <- if (state$installed) {
      c("OK", "OK", "OK")
    } else {
      c("OK", "VERSION_TOO_OLD", "MISSING")
    }
    data.frame(
      package = c("readxl", "meta", "netmeta"),
      installed = status != "MISSING",
      version = c("1.4.0", "7.0.0", NA_character_),
      minimum = c("1.4.0", "8.0.0", "3.0.0"),
      status = status,
      stringsAsFactors = FALSE
    )
  }
  installer <- function(packages) {
    state$requested <- packages
    state$installed <- TRUE
    invisible(packages)
  }

  testthat::expect_message(
    result <- .ensure_auctus_dependencies(checker = checker, installer = installer),
    "Seluruh dependency Auctus sudah siap"
  )
  testthat::expect_identical(state$requested, c("meta", "netmeta"))
  testthat::expect_true(all(result$status == "OK"))
})

testthat::test_that("dependency bootstrap reports packages still unresolved", {
  checker <- function(stop_on_missing = FALSE) {
    data.frame(
      package = "netmeta", installed = FALSE, version = NA_character_,
      minimum = "3.0.0", status = "MISSING", stringsAsFactors = FALSE
    )
  }
  installer <- function(packages) invisible(packages)

  testthat::expect_error(
    .ensure_auctus_dependencies(checker = checker, installer = installer),
    "netmeta: MISSING, versi minimum 3.0.0"
  )
})

testthat::test_that("empty runtime diagnostics bind with user-facing diagnostics", {
  user_diag <- .user_diagnostics(.new_diagnostics())
  combined <- .bind_diag(user_diag, .new_diagnostics())

  testthat::expect_s3_class(combined, "data.frame")
  testthat::expect_equal(nrow(combined), 0L)
  testthat::expect_setequal(names(combined), .diagnostic_columns)
})

testthat::test_that("template contains the V2.3 schema without include flags", {
  path <- tempfile(fileext = ".xlsx")
  create_auctus_template(path)
  testthat::expect_true(file.exists(path))
  testthat::expect_setequal(
    readxl::excel_sheets(path),
    c("PETUNJUK", "CONTOH_PENGISIAN", "analyses", "study_metadata", "arm_data",
      "effect_data", "diagnostic_data", "LOOKUPS")
  )
  template_analyses <- readxl::read_excel(path, sheet = "analyses")
  testthat::expect_true(all(is.na(template_analyses$timepoint)))
  petunjuk <- readxl::read_excel(
    path, sheet = "PETUNJUK", range = "A1:B22", col_names = FALSE
  )
  testthat::expect_identical(petunjuk[[1L]][[1L]], "DATASET MA NMA AUCTUS V2.3")
  testthat::expect_true(is.na(petunjuk[[1L]][[2L]]))
  testthat::expect_identical(
    petunjuk[[1L]][5:22],
    c(
      "Mulai", "Langkah 1", "Langkah 2", "Langkah 3", "Kunci outcome",
      "Kunci studi", "Studi multi-arm", "Zero event", "Reported effect",
      "Semua baris aktif", "Mengecualikan data", "Warna merah", "Warna kuning",
      "Contoh", "Forest plot raw", "Forest plot reported/mixed",
      "Arah Favours", "Cara pakai kode"
    )
  )
  testthat::expect_identical(
    petunjuk[[2L]][[22L]],
    "Jalankan source('meta_nma_engine.R'), lalu run_auctus_meta()."
  )
  template_arms <- readxl::read_excel(path, sheet = "arm_data")
  for (technical_id in c("analysis_id", "study_id", "arm_id")) {
    testthat::expect_false(technical_id %in% names(template_analyses))
    testthat::expect_false(technical_id %in% names(template_arms))
  }
  template_metadata <- readxl::read_excel(path, sheet = "study_metadata")
  testthat::expect_false(any(c("analysis_id", "study_id") %in% names(template_metadata)))
  for (sheet in c("analyses", "arm_data", "effect_data", "diagnostic_data", "LOOKUPS")) {
    headers <- names(readxl::read_excel(path, sheet = sheet, n_max = 0))
    testthat::expect_false("include" %in% headers)
  }
  template_text <- unlist(lapply(readxl::excel_sheets(path), function(sheet) {
    unlist(readxl::read_excel(path, sheet = sheet, col_names = FALSE), use.names = FALSE)
  }), use.names = FALSE)
  template_text <- tolower(trimws(as.character(template_text[!is.na(template_text)])))
  testthat::expect_false(any(template_text == "include"))
  testthat::expect_false(any(grepl("kolom[[:space:]]+include", template_text)))
  validation <- validate_auctus_data(path, output_dir = tempfile("validate_"))
  testthat::expect_false(validation$valid)
  testthat::expect_true("E300_NO_ANALYSIS" %in% validation$diagnostics$error_code)
  testthat::expect_true(file.exists(validation$checked_workbook))
  extracted <- tempfile("template_xml_")
  dir.create(extracted)
  utils::unzip(path, exdir = extracted)
  lookup_xml <- paste(readLines(file.path(extracted, "xl", "worksheets", "sheet8.xml"), warn = FALSE), collapse = "")
  testthat::expect_match(lookup_xml, "sheetProtection")
  workbook_xml <- paste(readLines(file.path(extracted, "xl", "workbook.xml"), warn = FALSE), collapse = "")
  testthat::expect_match(workbook_xml, "AUCTUS_OUTCOMES")
  testthat::expect_match(workbook_xml, "AUCTUS_STUDIES")
  testthat::expect_match(workbook_xml, "'analyses'!\\$A\\$2:\\$A\\$5000")
  data_sheet_xml <- paste(unlist(lapply(
    file.path(extracted, "xl", "worksheets", paste0("sheet", 5:7, ".xml")),
    readLines, warn = FALSE
  )), collapse = "")
  testthat::expect_equal(lengths(regmatches(data_sheet_xml, gregexpr("AUCTUS_OUTCOMES", data_sheet_xml))), 3L)
  testthat::expect_equal(lengths(regmatches(data_sheet_xml, gregexpr("AUCTUS_STUDIES", data_sheet_xml))), 3L)
  testthat::expect_true(length(list.files(file.path(extracted, "xl"), pattern = "^comments.*xml$")) >= 5)
  styles_xml <- paste(readLines(file.path(extracted, "xl", "styles.xml"), warn = FALSE), collapse = "")
  testthat::expect_match(styles_xml, "FF274E13")
  testthat::expect_match(styles_xml, "FFD9EAD3")
  worksheet_xml <- paste(unlist(lapply(
    file.path(extracted, "xl", "worksheets", paste0("sheet", 1:7, ".xml")),
    readLines, warn = FALSE
  )), collapse = "")
  testthat::expect_gte(
    lengths(regmatches(worksheet_xml, gregexpr("FF38761D", worksheet_xml))), 7L
  )
})

testthat::test_that("timepoint is optional and blank titles remain clean", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = "pairwise")
  wb <- .read_auctus_workbook(path)
  wb$analyses$timepoint[] <- NA_character_
  diag <- .validate_auctus_workbook(wb)

  testthat::expect_false(any(
    diag$severity == "ERROR" & diag$sheet == "analyses" & diag$column == "timepoint",
    na.rm = TRUE
  ))
  testthat::expect_identical(
    .analysis_title(list(
      analysis_id = "mortality_30d",
      outcome_name = "Mortality at 30 days",
      timepoint = NA_character_
    )),
    "Mortality at 30 days"
  )
})

testthat::test_that("blank rows are ignored while partially filled rows are errors", {
  path <- tempfile(fileext = ".xlsx")
  create_auctus_template(path)
  wb <- openxlsx::loadWorkbook(path)
  openxlsx::writeData(
    wb, "arm_data", "Mortality", startCol = 1, startRow = 2,
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  imported <- .read_auctus_workbook(path)
  testthat::expect_equal(nrow(imported$analyses), 0L)
  testthat::expect_equal(nrow(imported$study_metadata), 0L)
  testthat::expect_equal(nrow(imported$arm_data), 1L)
  diag <- .validate_auctus_workbook(imported)
  missing <- diag[diag$error_code == "E002_MISSING_REQUIRED" &
                    diag$sheet == "arm_data", , drop = FALSE]
  testthat::expect_true(all(c("B", "C") %in% missing$column))
  testthat::expect_true("E300_NO_ANALYSIS" %in% diag$error_code)
})

testthat::test_that("older V2 arm_id columns are accepted and ignored", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = "pairwise")
  wb <- .read_auctus_workbook(path)
  wb$arm_data$arm_id <- paste0("legacy_arm_", seq_len(nrow(wb$arm_data)))

  diag <- .validate_auctus_workbook(wb)
  testthat::expect_false(any(diag$severity == "ERROR"))
  normalized <- .normalize_workbook_data(wb)
  testthat::expect_false("arm_id" %in% names(normalized$arm_data))
})

testthat::test_that("multi-arm input keeps k rows and creates k choose 2 contrasts", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = "nma")
  validation <- validate_auctus_data(path, output_dir = tempfile("validate_"))
  testthat::expect_true(validation$valid)
  dat <- .normalize_workbook_data(validation$workbook)
  analysis <- analysis_row(dat, .fixture_outcomes[["nma"]])
  nm1 <- study_internal_id(dat, "NM1")
  nm2 <- study_internal_id(dat, "NM2")
  bundle <- .canonicalize_analysis(dat, analysis)
  testthat::expect_equal(nrow(dat$arm_data[dat$arm_data$study_label == "NM1", ]), 3)
  testthat::expect_false("arm_id" %in% names(dat$arm_data))
  testthat::expect_equal(nrow(bundle$raw[bundle$raw$study_id == nm1, ]), 3)
  testthat::expect_equal(nrow(bundle$raw[bundle$raw$study_id == nm2, ]), 1)
  counts <- .nma_treatment_counts(bundle$primary, dat, analysis)
  expected_n <- c("Treatment A" = 190, "Treatment B" = 195, "Placebo" = 288)
  testthat::expect_equal(
    setNames(counts$participants, counts$treatment)[names(expected_n)],
    expected_n
  )

  four_arms <- dat$arm_data[dat$arm_data$study_id == nm1, , drop = FALSE]
  fourth <- four_arms[1, , drop = FALSE]
  fourth$treatment <- "Treatment C"
  fourth$event <- 24
  four_arms <- rbind(four_arms, fourth)
  four_contrasts <- .raw_pairwise_contrasts(four_arms, analysis)
  testthat::expect_equal(nrow(four_contrasts), 6)
})

testthat::test_that("zero event is valid and double-zero is explicit", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = "pairwise")
  validation <- validate_auctus_data(path, output_dir = tempfile("validate_"))
  testthat::expect_true(validation$valid)
  dat <- .normalize_workbook_data(validation$workbook)
  analysis <- analysis_row(dat, .fixture_outcomes[["pairwise"]])
  pb2 <- study_internal_id(dat, "PB2")
  pb4 <- study_internal_id(dat, "PB4")
  raw <- .raw_pairwise_contrasts(dat$arm_data, analysis)
  testthat::expect_true(is.finite(raw$TE[raw$study_id == pb2]))
  testthat::expect_identical(raw$method_note[raw$study_id == pb4], "double_zero_excluded")
  testthat::expect_true(is.na(raw$TE[raw$study_id == pb4]))
  testthat::expect_true("I201_DOUBLE_ZERO_EXCLUDED" %in% validation$diagnostics$error_code)
})

testthat::test_that("sparse RR keeps zero-event studies and creates continuity-correction sensitivity", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = "pairwise")
  wb <- .read_auctus_workbook(path)
  wb$analyses$effect_measure[wb$analyses$outcome_name == .fixture_outcomes[["pairwise"]]] <- "RR"
  dat <- .normalize_workbook_data(wb)
  analysis <- analysis_row(dat, .fixture_outcomes[["pairwise"]])
  out <- .run_pairwise_analysis(dat, analysis, tempfile("rr_sparse_"))
  testthat::expect_identical(out$status, "SUCCESS")
  testthat::expect_true(any(grepl("continuity correction", out$sensitivity$method, fixed = TRUE)))
})

testthat::test_that("reversed pairwise contrast is normalized to reference", {
  raw <- .empty_df(c(
    "analysis_id", "study_id", "treat1", "treat2", "TE", "seTE", "source_type",
    "event1", "sample1", "event2", "sample2", "mean1", "sd1", "mean2", "sd2",
    "converted", "conversion_method", "method_note", "source_sheet", "source_row"
  ))
  reported <- data.frame(
    analysis_id = "a", study_id = "s1", treat1 = "Placebo", treat2 = "Treatment A",
    TE = log(2), seTE = 0.2, source_type = "adjusted",
    event1 = 5, sample1 = 100, event2 = 10, sample2 = 100,
    mean1 = 130, sd1 = 15, mean2 = 124, sd2 = 14,
    converted = FALSE, conversion_method = NA, method_note = "reported adjusted",
    source_sheet = "effect_data", source_row = 2L, stringsAsFactors = FALSE
  )
  oriented <- .orient_pairwise_reference(reported, "Placebo")
  testthat::expect_identical(oriented$treat1, "Treatment A")
  testthat::expect_identical(oriented$treat2, "Placebo")
  testthat::expect_equal(oriented$TE, -log(2))
  testthat::expect_equal(oriented$sample1, 100)
  testthat::expect_equal(oriented$event1, 10)
  testthat::expect_equal(oriented$event2, 5)
  testthat::expect_equal(oriented$mean1, 124)
  testthat::expect_equal(oriented$mean2, 130)
})

testthat::test_that("adjusted estimate replaces raw estimate only within the same study", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = "pairwise")
  dat <- .normalize_workbook_data(.read_auctus_workbook(path))
  analysis <- analysis_row(dat, .fixture_outcomes[["pairwise"]])
  pb1 <- study_internal_id(dat, "PB1")
  dat$effect_data <- .rbind_fill(list(dat$effect_data, data.frame(
    analysis_id = analysis$analysis_id, study_id = pb1,
    outcome_name = .fixture_outcomes[["pairwise"]], study_label = "PB1",
    treat1 = "Treatment A", treat2 = "Placebo",
    effect = 0.50, ci_low = 0.30, ci_high = 0.80, se = NA_real_, ci_level = 95,
    estimate_type = "adjusted", adjustment_variables = "age; severity",
    sample1 = 100, sample2 = 100, notes = "Reported model",
    source_sheet = "effect_data", source_row = 2L, stringsAsFactors = FALSE
  )))
  bundle <- .canonicalize_analysis(dat, analysis)
  pb1_effect <- bundle$primary[bundle$primary$study_id == pb1, , drop = FALSE]
  testthat::expect_equal(nrow(pb1_effect), 1)
  testthat::expect_identical(pb1_effect$source_type, "adjusted")
  testthat::expect_equal(pb1_effect$TE, log(0.50), tolerance = 1e-12)
  testthat::expect_true(all(c("adjusted", "raw_derived") %in%
                              bundle$all_sources$source_type[bundle$all_sources$study_id == pb1]))
})

testthat::test_that("raw pairwise forest uses atomic columns and dynamic treatment labels", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = c("pairwise", "continuous"))
  dat <- .normalize_workbook_data(.read_auctus_workbook(path))

  binary_analysis <- analysis_row(dat, .fixture_outcomes[["pairwise"]])
  binary <- .canonicalize_analysis(dat, binary_analysis)$primary
  binary <- binary[is.finite(binary$TE) & is.finite(binary$seTE), , drop = FALSE]
  binary_model <- .fit_metagen(binary, "OR", prediction = FALSE)
  binary_atomic <- .attach_pairwise_atomic_columns(
    binary_model, binary, binary_analysis, "raw_atomic"
  )
  binary_labs <- gsub("\u00A0", "", binary_atomic$leftlabs, fixed = TRUE)
  testthat::expect_identical(
    binary_atomic$leftcols,
    c("studlab", "event.e", "n.e", "event.c", "n.c")
  )
  testthat::expect_identical(binary_labs, c("Study", "Event", "Total", "Event", "Total"))
  testthat::expect_identical(binary_atomic$treat1, "Treatment A")
  testthat::expect_identical(binary_atomic$treat2, "Placebo")

  continuous_analysis <- analysis_row(dat, .fixture_outcomes[["continuous"]])
  continuous <- .canonicalize_analysis(dat, continuous_analysis)$primary
  continuous_model <- .fit_metagen(continuous, "MD", prediction = FALSE)
  continuous_atomic <- .attach_pairwise_atomic_columns(
    continuous_model, continuous, continuous_analysis, "raw_atomic"
  )
  continuous_labs <- gsub("\u00A0", "", continuous_atomic$leftlabs, fixed = TRUE)
  testthat::expect_identical(
    continuous_atomic$leftcols,
    c("studlab", "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c")
  )
  testthat::expect_identical(
    continuous_labs,
    c("Study", "Mean", "SD", "Total", "Mean", "SD", "Total")
  )

  lower <- .forest_favour_labels(binary_analysis, "Treatment A", "Placebo")
  testthat::expect_identical(lower$left, "Favours Treatment A")
  testthat::expect_identical(lower$right, "Favours Placebo")
  binary_analysis$outcome_direction <- "higher_better"
  higher <- .forest_favour_labels(binary_analysis, "Treatment A", "Placebo")
  testthat::expect_identical(higher$left, "Favours Placebo")
  testthat::expect_identical(higher$right, "Favours Treatment A")
})

testthat::test_that("reported and mixed forest is total-only with ordered source groups", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = "pairwise")
  dat <- .normalize_workbook_data(.read_auctus_workbook(path))
  analysis <- analysis_row(dat, .fixture_outcomes[["pairwise"]])
  additions <- data.frame(
    analysis_id = analysis$analysis_id,
    study_id = c(study_internal_id(dat, "PB1"), study_internal_id(dat, "PB2")),
    outcome_name = .fixture_outcomes[["pairwise"]],
    study_label = c("PB1", "PB2"),
    treat1 = "Treatment A", treat2 = "Placebo",
    effect = c(0.50, 0.70), ci_low = c(0.30, 0.35), ci_high = c(0.80, 1.40),
    se = NA_real_, ci_level = 95,
    estimate_type = c("adjusted", "crude"),
    adjustment_variables = c("age; severity", NA_character_),
    sample1 = c(100, NA), sample2 = c(100, 100), notes = "Reported model",
    source_sheet = "effect_data", source_row = 2:3,
    stringsAsFactors = FALSE
  )
  dat$effect_data <- .rbind_fill(list(dat$effect_data, additions))
  output <- .run_pairwise_analysis(dat, analysis, tempfile("reported_forest_"))

  testthat::expect_identical(output$source_subgroup$forest_mode, "reported_total_only")
  testthat::expect_identical(
    as.character(output$source_subgroup$tables$subgroup),
    c("Adjusted", "Crude", "Raw-derived")
  )
  testthat::expect_true(is.na(
    output$source_subgroup$tables$total_treat1[
      output$source_subgroup$tables$subgroup == "Crude"
    ]
  ))
  display <- .attach_pairwise_atomic_columns(
    output$source_subgroup$model, output$canonical, analysis,
    "reported_total_only"
  )
  testthat::expect_false(any(grepl("event|mean|sd|seTE|TE", display$leftcols)))
  testthat::expect_true(all(c("n.e", "n.c") %in% display$leftcols))
  testthat::expect_true(anyNA(display$model$n.e))
  testthat::expect_true(is.na(display$model$n.e.pooled))
  testthat::expect_equal(
    unname(display$model$n.e.w[c("Adjusted", "Raw-derived")]), c(100, 100)
  )
  testthat::expect_true(is.na(display$model$n.e.w[["Crude"]]))
  testthat::expect_true(any(output$method_decisions$decision == "forest_mode" &
                              output$method_decisions$value == "reported_total_only"))
  testthat::expect_true(all(file.exists(output$plots)))

  tables <- .flatten_result_tables(list(Mortality = output), .new_diagnostics())
  testthat::expect_true(nrow(tables$SOURCE_SUBGROUP) == 3L)
  adjusted <- tables$SOURCE_SUBGROUP[tables$SOURCE_SUBGROUP$subgroup == "Adjusted", ]
  testthat::expect_equal(adjusted$estimate, 0.50, tolerance = 1e-12)
  testthat::expect_false(any(c("analysis_id", "study_id", "TE", "seTE") %in%
                               names(tables$SOURCE_SUBGROUP)))
})

testthat::test_that("single mean converts quantiles and creates exclusion sensitivity", {
  analysis <- list(
    analysis_id = "mean_single", analysis_type = "proportion_ma",
    outcome_name = "Length of stay", timepoint = "Discharge", outcome_type = "mean",
    effect_measure = NA_character_, reference_treatment = NA_character_,
    outcome_direction = "neutral", unit = "days"
  )
  metadata <- data.frame(
    study_id = paste0("m", 1:4),
    study_label = paste("Mean study", 1:4), year = 2020:2023,
    study_design = "Cohort", subgroup_region = rep(c("Asia", "Europe"), each = 2),
    stringsAsFactors = FALSE
  )
  arms <- data.frame(
    analysis_id = "mean_single", study_id = paste0("m", 1:4),
    treatment = "Population",
    event = NA_real_, sample = c(50, 55, 60, 52),
    mean = c(10, 11, 9, NA), sd = c(2, 2.4, 1.8, NA),
    median = c(NA, NA, NA, 10), q1 = c(NA, NA, NA, 8),
    q3 = c(NA, NA, NA, 12), min = c(NA, NA, NA, 5), max = c(NA, NA, NA, 16),
    notes = "Test", source_sheet = "arm_data", source_row = 2:5,
    stringsAsFactors = FALSE
  )
  out <- .run_single_arm_analysis_v2(
    list(arm_data = arms, study_metadata = metadata), analysis, tempfile("mean_plots_")
  )
  testthat::expect_identical(out$status, "SUCCESS")
  testthat::expect_true(out$canonical$converted[out$canonical$study_id == "m4"])
  testthat::expect_equal(nrow(out$sensitivity), 1)
  testthat::expect_equal(nrow(out$loo$table), 4)
  testthat::expect_true(any(out$method_decisions$decision == "summary_conversion"))
})

testthat::test_that("numeric moderator with ten studies creates a bubble plot", {
  canonical <- data.frame(
    analysis_id = "bubble", study_id = paste0("b", 1:10),
    study_label = paste("Bubble study", 1:10),
    TE = c(-0.20, -0.12, -0.15, -0.02, 0.03, 0.01, 0.11, 0.08, 0.19, 0.16),
    seTE = seq(0.14, 0.23, length.out = 10), moderator_num_mean_age = 41:50,
    stringsAsFactors = FALSE
  )
  analysis <- list(analysis_id = "bubble", outcome_name = "Functional score", effect_measure = "MD")
  out <- .run_meta_regression(canonical, analysis, tempfile("bubble_plots_"))
  testthat::expect_true(nrow(out$tables) >= 2)
  testthat::expect_true(all(c("moderator_test_p", "r2_percent", "residual_tau2") %in% names(out$tables)))
  testthat::expect_equal(length(out$plots), 2)
  testthat::expect_true(all(file.exists(out$plots)))
})

testthat::test_that("validation points to the exact invalid cell", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = "pairwise")
  wb <- openxlsx::loadWorkbook(path)
  openxlsx::writeData(wb, "arm_data", 200, startCol = 4, startRow = 2, colNames = FALSE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  validation <- validate_auctus_data(path, output_dir = tempfile("validate_"))
  err <- validation$diagnostics[validation$diagnostics$error_code == "E107_EVENT_EXCEEDS_SAMPLE", , drop = FALSE]
  testthat::expect_false(validation$valid)
  testthat::expect_equal(nrow(err), 1)
  testthat::expect_identical(err$sheet, "arm_data")
  testthat::expect_identical(err$column, "D")
  testthat::expect_true(file.exists(validation$checked_workbook))
  extracted <- tempfile("checked_xml_")
  dir.create(extracted)
  utils::unzip(validation$checked_workbook, exdir = extracted)
  worksheet_xml <- list.files(file.path(extracted, "xl", "worksheets"), pattern = "^sheet.*xml$", full.names = TRUE)
  xml_text <- paste(unlist(lapply(worksheet_xml, readLines, warn = FALSE)), collapse = "")
  testthat::expect_match(xml_text, "HYPERLINK")
})

testthat::test_that("validator catches normalized treatment collisions", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = "pairwise")
  wb <- .read_auctus_workbook(path)
  idx <- which(wb$arm_data$outcome_name == .fixture_outcomes[["pairwise"]] &
                 wb$arm_data$treatment == "Placebo")[[1L]]
  wb$arm_data$treatment[[idx]] <- "placebo"
  diag <- .validate_auctus_workbook(wb)
  testthat::expect_true("E208_TREATMENT_LABEL_COLLISION" %in% diag$error_code)
})

testthat::test_that("partial adjusted contrasts cannot silently replace a multi-arm study", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = "nma")
  wb <- .read_auctus_workbook(path)
  wb$effect_data <- .rbind_fill(list(wb$effect_data, data.frame(
    outcome_name = .fixture_outcomes[["nma"]], study_label = "NM1",
    treat1 = "Treatment A", treat2 = "Placebo",
    effect = "0.50", ci_low = "0.30", ci_high = "0.80", se = NA_character_, ci_level = "95",
    estimate_type = "adjusted", adjustment_variables = "baseline risk",
    sample1 = "100", sample2 = "100", notes = "Partial report",
    source_sheet = "effect_data", source_row = 2L, stringsAsFactors = FALSE
  )))
  diag <- .validate_auctus_workbook(wb)
  testthat::expect_true("E403_PARTIAL_PRIORITY_MULTIARM" %in% diag$error_code)
})

testthat::test_that("internal IDs are deterministic and independent of row order", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = "pairwise")
  wb <- .read_auctus_workbook(path)
  first <- .augment_internal_ids(wb)
  wb$analyses <- wb$analyses[rev(seq_len(nrow(wb$analyses))), , drop = FALSE]
  wb$study_metadata <- wb$study_metadata[rev(seq_len(nrow(wb$study_metadata))), , drop = FALSE]
  second <- .augment_internal_ids(wb)

  first_analysis <- sort(setNames(first$analyses$analysis_id, first$analyses$outcome_name))
  second_analysis <- sort(setNames(second$analyses$analysis_id, second$analyses$outcome_name))
  first_study <- sort(setNames(first$study_metadata$study_id, first$study_metadata$study_label))
  second_study <- sort(setNames(second$study_metadata$study_id, second$study_metadata$study_label))
  testthat::expect_identical(first_analysis, second_analysis)
  testthat::expect_identical(first_study, second_study)
  testthat::expect_true(all(grepl("^a_[0-9a-f]{12}$", first_analysis)))
  testthat::expect_true(all(grepl("^s_[0-9a-f]{12}$", first_study)))
})

testthat::test_that("label matching tolerates case and spaces but reports typos", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = "pairwise")
  wb <- .read_auctus_workbook(path)
  wb$arm_data$outcome_name[[1L]] <- "  mortality  "
  wb$arm_data$study_label[[1L]] <- " pb1 "
  diag <- .validate_auctus_workbook(wb)
  testthat::expect_false(any(diag$severity == "ERROR"))
  testthat::expect_true(sum(diag$error_code == "I101_LABEL_NORMALIZED") >= 2L)

  wb$arm_data$outcome_name[[1L]] <- "Mortalitty"
  wb$arm_data$study_label[[1L]] <- "PB-unknown"
  diag <- .validate_auctus_workbook(wb)
  unknown_outcome <- diag[diag$error_code == "E203_UNKNOWN_OUTCOME_NAME", , drop = FALSE]
  unknown_study <- diag[diag$error_code == "E205_UNKNOWN_STUDY_LABEL", , drop = FALSE]
  testthat::expect_equal(nrow(unknown_outcome), 1)
  testthat::expect_equal(nrow(unknown_study), 1)
  testthat::expect_identical(unknown_outcome$sheet, "arm_data")
  testthat::expect_identical(unknown_study$column, "B")
})

testthat::test_that("normalized outcome and study collisions are blocking errors", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = "pairwise")
  wb <- .read_auctus_workbook(path)
  duplicate_analysis <- wb$analyses[wb$analyses$outcome_name == "Mortality", , drop = FALSE]
  duplicate_analysis$outcome_name <- " mortality  "
  wb$analyses <- rbind(wb$analyses, duplicate_analysis)
  duplicate_study <- wb$study_metadata[wb$study_metadata$study_label == "PB1", , drop = FALSE]
  duplicate_study$study_label <- " pb1 "
  wb$study_metadata <- rbind(wb$study_metadata, duplicate_study)
  diag <- .validate_auctus_workbook(wb)
  testthat::expect_true("E202_DUPLICATE_OUTCOME_NAME" %in% diag$error_code)
  testthat::expect_true("E204_DUPLICATE_STUDY_LABEL" %in% diag$error_code)
})

testthat::test_that("global study metadata is reused across outcomes", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = c("pairwise", "continuous"))
  wb <- .read_auctus_workbook(path)
  wb$arm_data$study_label[
    wb$arm_data$outcome_name == .fixture_outcomes[["continuous"]] &
      wb$arm_data$study_label == "PC1"
  ] <- "PB1"
  diag <- .validate_auctus_workbook(wb)
  testthat::expect_false(any(diag$severity == "ERROR"))
  dat <- .normalize_workbook_data(wb)
  reused <- dat$arm_data[dat$arm_data$study_label == "PB1", , drop = FALSE]
  testthat::expect_equal(length(unique(reused$analysis_id)), 2)
  testthat::expect_equal(length(unique(reused$study_id)), 1)
  pair_bundle <- .canonicalize_analysis(dat, analysis_row(dat, .fixture_outcomes[["pairwise"]]))
  cont_bundle <- .canonicalize_analysis(dat, analysis_row(dat, .fixture_outcomes[["continuous"]]))
  testthat::expect_identical(
    unique(pair_bundle$primary$subgroup_region[pair_bundle$primary$study_label == "PB1"]),
    unique(cont_bundle$primary$subgroup_region[cont_bundle$primary$study_label == "PB1"])
  )
})

testthat::test_that("V2 ID workbook migrates to V2.3 and drops include FALSE rows", {
  old_path <- tempfile(fileext = ".xlsx")
  old <- list(
    analyses = data.frame(
      analysis_id = c("old_mortality", "old_readmission"), include = c(TRUE, FALSE),
      analysis_type = "pairwise_ma", outcome_name = c("Mortality", "Readmission"),
      timepoint = NA, outcome_type = "binary", effect_measure = "OR",
      reference_treatment = "Placebo", outcome_direction = "lower_better",
      unit = NA, notes = "Old V2"
    ),
    study_metadata = data.frame(
      analysis_id = rep(c("old_mortality", "old_readmission"), each = 2),
      study_id = rep(c("old_s1", "old_s2"), 2),
      study_label = rep(c("Smith 2024", "Lee 2023"), 2),
      year = rep(c(2024, 2023), 2), study_design = "RCT"
    ),
    arm_data = data.frame(
      analysis_id = "old_mortality", study_id = rep(c("old_s1", "old_s2"), each = 2),
      treatment = rep(c("Treatment A", "Placebo"), 2), event = c(3, 8, 5, 9),
      sample = c(100, 100, 90, 92), mean = NA, sd = NA, median = NA,
      q1 = NA, q3 = NA, min = NA, max = NA, include = TRUE, notes = "Old V2"
    ),
    effect_data = .empty_df(.legacy_v2_specs$effect_data),
    diagnostic_data = .empty_df(.legacy_v2_specs$diagnostic_data)
  )
  writexl::write_xlsx(old, old_path)
  converted <- convert_legacy_workbook(old_path, tempfile(fileext = ".xlsx"))
  converted_analyses <- readxl::read_excel(converted, sheet = "analyses")
  converted_arms <- readxl::read_excel(converted, sheet = "arm_data")
  testthat::expect_false("analysis_id" %in% names(converted_analyses))
  testthat::expect_false("include" %in% names(converted_analyses))
  testthat::expect_true("legacy_analysis_id" %in% names(converted_analyses))
  testthat::expect_identical(converted_analyses$outcome_name, "Mortality")
  testthat::expect_true(all(c("outcome_name", "study_label", "legacy_study_id") %in% names(converted_arms)))
  testthat::expect_equal(nrow(readxl::read_excel(converted, sheet = "study_metadata")), 2)
  migration_log <- readxl::read_excel(converted, sheet = "MIGRATION_LOG")
  testthat::expect_true(any(migration_log$removed_rows > 0))
  testthat::expect_true(validate_auctus_data(converted, output_dir = tempfile("validate_"))$valid)

  old$analyses$include <- TRUE
  old$study_metadata$year[[4L]] <- 2022
  conflict_path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(old, conflict_path)
  conflict_converted <- convert_legacy_workbook(conflict_path, tempfile(fileext = ".xlsx"))
  conflict_validation <- validate_auctus_data(conflict_converted, output_dir = tempfile("validate_"))
  testthat::expect_false(conflict_validation$valid)
  testthat::expect_true("E204_DUPLICATE_STUDY_LABEL" %in% conflict_validation$diagnostics$error_code)
})

testthat::test_that("legacy blank include values are retained as active", {
  legacy <- list(
    analyses = data.frame(
      outcome_name = c("Outcome retained", "Outcome dropped"),
      include = c(NA, FALSE), stringsAsFactors = FALSE
    ),
    study_metadata = data.frame(study_label = "Study 1", stringsAsFactors = FALSE),
    arm_data = data.frame(
      outcome_name = c("Outcome retained", "Outcome dropped"),
      study_label = "Study 1", include = c(NA, TRUE), stringsAsFactors = FALSE
    ),
    effect_data = data.frame(
      outcome_name = character(), study_label = character(), include = logical(),
      stringsAsFactors = FALSE
    ),
    diagnostic_data = data.frame(
      outcome_name = character(), study_label = character(), include = logical(),
      stringsAsFactors = FALSE
    )
  )
  filtered <- .filter_legacy_includes(legacy, "v22_label")
  testthat::expect_identical(filtered$data$analyses$outcome_name, "Outcome retained")
  testthat::expect_identical(filtered$data$arm_data$outcome_name, "Outcome retained")
  testthat::expect_false(any(vapply(filtered$data, function(x) {
    "include" %in% names(x)
  }, logical(1))))
  testthat::expect_true(sum(filtered$log$removed_rows) >= 2L)
})

testthat::test_that("legacy workbook converts before V2 validation", {
  old_path <- tempfile(fileext = ".xlsx")
  old <- data.frame(
    study = c("Smith 2024", "Lee 2023", "Garcia 2022"), outcome_name = "Mortality",
    effect_measure = "OR", event1 = c(3, 5, 4), sample1 = c(100, 90, 95),
    event2 = c(8, 9, 10), sample2 = c(100, 92, 96),
    treat1 = "Treatment A", treat2 = "Placebo", include = c(TRUE, FALSE, NA),
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(list(dikotomi = old), old_path)
  converted <- convert_legacy_workbook(old_path, tempfile(fileext = ".xlsx"))
  validation <- validate_auctus_data(converted, output_dir = tempfile("validate_"))
  testthat::expect_true(validation$valid)
  testthat::expect_true(all(c("analyses", "arm_data") %in% readxl::excel_sheets(converted)))
  converted_arms <- readxl::read_excel(converted, sheet = "arm_data")
  testthat::expect_setequal(converted_arms$study_label, c("Smith 2024", "Garcia 2022"))
  migration_log <- readxl::read_excel(converted, sheet = "MIGRATION_LOG")
  testthat::expect_true(any(migration_log$sheet == "dikotomi" & migration_log$removed_rows == 1))
})

testthat::test_that("valid_only skips an invalid outcome by normalized label", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path, active = c("pairwise", "proportion"))
  wb <- openxlsx::loadWorkbook(path)
  openxlsx::writeData(wb, "arm_data", "Unknown Study", startCol = 2, startRow = 2, colNames = FALSE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  result <- run_auctus_meta(path, output_dir = tempfile("valid_only_"), run_mode = "valid_only")
  testthat::expect_setequal(names(result$analyses), .fixture_outcomes[["proportion"]])
  testthat::expect_true("E205_UNKNOWN_STUDY_LABEL" %in% result$diagnostics$error_code)
})

testthat::test_that("all analysis engines run end to end", {
  path <- tempfile(fileext = ".xlsx")
  make_v2_fixture(path)
  output <- tempfile("auctus_run_")
  result <- run_auctus_meta(path, output_dir = output, run_mode = "strict")
  testthat::expect_identical(result$status, "SUCCESS")
  testthat::expect_true(all(vapply(result$analyses, function(x) x$status == "SUCCESS", logical(1))))
  testthat::expect_true(file.exists(result$files$results_workbook))
  testthat::expect_true(file.exists(result$files$manifest))
  testthat::expect_true(file.exists(result$files$report))
  testthat::expect_true(file.exists(result$files$report_html))
  testthat::expect_true(file.exists(result$files$report_markdown))
  testthat::expect_true(file.exists(result$files$id_map))
  testthat::expect_setequal(names(result$analyses), unname(.fixture_outcomes))
  testthat::expect_true(any(grepl("forest_overall", unlist(lapply(result$analyses, `[[`, "plots")))))
  testthat::expect_equal(nrow(result$analyses[[.fixture_outcomes[["proportion"]]]]$loo$table), 4)
  testthat::expect_equal(nrow(result$analyses[[.fixture_outcomes[["diagnostic"]]]]$loo$table), 12)
  testthat::expect_equal(result$analyses[[.fixture_outcomes[["pairwise"]]]]$summary$estimate, 0.4689902822, tolerance = 1e-6)
  testthat::expect_equal(result$analyses[[.fixture_outcomes[["continuous"]]]]$summary$estimate, -5.6965364830, tolerance = 1e-6)
  testthat::expect_equal(result$analyses[[.fixture_outcomes[["proportion"]]]]$summary$estimate, 0.0470326218, tolerance = 1e-6)
  nma_estimates <- result$analyses[[.fixture_outcomes[["nma"]]]]$summary$estimate
  testthat::expect_equal(nma_estimates, c(0.4830962070, 0.6911540176), tolerance = 1e-6)
  diagnostic_estimates <- result$analyses[[.fixture_outcomes[["diagnostic"]]]]$summary$estimate
  testthat::expect_equal(diagnostic_estimates, c(0.8361507487, 0.9412682809), tolerance = 1e-6)
  testthat::expect_true(all(vapply(result$tables, function(tab) {
    !is.data.frame(tab) || !any(c("analysis_id", "study_id") %in% names(tab))
  }, logical(1))))
  result_sheets <- readxl::excel_sheets(result$files$results_workbook)
  testthat::expect_true("SOURCE_SUBGROUP" %in% result_sheets)
  testthat::expect_true(all(vapply(result_sheets, function(sheet) {
    headers <- names(readxl::read_excel(result$files$results_workbook, sheet = sheet, n_max = 0))
    !any(c("analysis_id", "study_id") %in% headers)
  }, logical(1))))
  report_text <- paste(readLines(result$files$report, warn = FALSE), collapse = "\n")
  testthat::expect_match(report_text, "src='data:image/png;base64,")
  testthat::expect_false(grepl("src='../02_plots/", report_text, fixed = TRUE))
  testthat::expect_false(grepl("a_[0-9a-f]{12}", report_text))
  markdown_text <- paste(readLines(result$files$report_markdown, warn = FALSE), collapse = "\n")
  testthat::expect_match(markdown_text, "# Auctus MA & NMA Report")
  testthat::expect_match(markdown_text, "forest_overall\\.png")
  testthat::expect_match(markdown_text, "\\.\\./02_plots/")
  testthat::expect_false(grepl("a_[0-9a-f]{12}", markdown_text))
  testthat::expect_false(grepl("analysis_id|study_id", markdown_text))
  manifest_text <- paste(readLines(result$files$manifest, warn = FALSE), collapse = "\n")
  testthat::expect_match(manifest_text, '"id_mappings"')
  testthat::expect_match(manifest_text, '"forest_mode": "raw_atomic"')
  plot_folders <- unique(basename(dirname(unlist(lapply(result$analyses, `[[`, "plots")))))
  testthat::expect_true(all(grepl("_[0-9a-f]{8}$", plot_folders)))
})
