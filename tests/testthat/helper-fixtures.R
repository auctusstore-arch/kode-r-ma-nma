engine_path <- normalizePath(file.path("..", "..", "meta_nma_engine.R"), mustWork = TRUE)
source(engine_path)

# Production source exports only the public API. Tests expose the private
# closure environment to test canonicalization and validation helpers.
.engine_private <- environment(run_auctus_meta)
for (.engine_symbol in ls(.engine_private, all.names = TRUE)) {
  assign(.engine_symbol, get(.engine_symbol, envir = .engine_private, inherits = FALSE),
         envir = environment())
}
rm(.engine_private, .engine_symbol)

.fixture_outcomes <- c(
  pairwise = "Mortality",
  continuous = "Systolic blood pressure",
  proportion = "Incidence",
  nma = "Clinical response",
  diagnostic = "Diagnostic accuracy"
)

analysis_row <- function(dat, outcome_name) {
  as.list(dat$analyses[dat$analyses$outcome_name == outcome_name, , drop = FALSE])
}

study_internal_id <- function(dat, study_label) {
  dat$study_metadata$study_id[dat$study_metadata$study_label == study_label][[1L]]
}

make_v2_fixture <- function(path, active = c("pairwise", "continuous", "proportion", "nma", "diagnostic")) {
  active <- match.arg(active, several.ok = TRUE)
  analyses <- data.frame(
    outcome_name = unname(.fixture_outcomes),
    analysis_type = c("pairwise_ma", "pairwise_ma", "proportion_ma", "nma", "diagnostic_ma"),
    timepoint = c("30 days", "12 weeks", "1 year", "End of treatment", "Index test"),
    outcome_type = c("binary", "continuous", "proportion", "binary", "diagnostic"),
    effect_measure = c("OR", "MD", NA, "OR", NA),
    reference_treatment = c("Placebo", "Placebo", NA, "Placebo", NA),
    outcome_direction = c("lower_better", "lower_better", "neutral", "higher_better", "neutral"),
    unit = c(NA, "mmHg", "%", NA, NA), notes = "Test fixture",
    stringsAsFactors = FALSE
  )

  studies <- c(paste0("PB", 1:4), paste0("PC", 1:4), paste0("PP", 1:4),
               paste0("NM", 1:3), paste0("DT", 1:6))
  study_metadata <- data.frame(
    study_label = studies,
    year = seq(2001, length.out = length(studies)),
    study_design = c(rep("RCT", 8), rep("Cohort", 4), rep("RCT", 3), rep("Diagnostic cohort", 6)),
    subgroup_region = rep(c("Asia", "Europe"), length.out = length(studies)),
    moderator_num_mean_age = seq(45, by = 1, length.out = length(studies)),
    stringsAsFactors = FALSE
  )

  arm_data <- .empty_df(.sheet_specs$arm_data)
  add_arm <- function(outcome_name, study_label, treatment, event = NA, sample,
                      mean = NA, sd = NA, median = NA, q1 = NA, q3 = NA,
                      min = NA, max = NA) {
    data.frame(
      outcome_name = outcome_name, study_label = study_label, treatment = treatment,
      event = event, sample = sample, mean = mean, sd = sd,
      median = median, q1 = q1, q3 = q3, min = min, max = max,
      notes = "Fixture", stringsAsFactors = FALSE
    )
  }
  pair_events <- list(c(8, 14), c(0, 7), c(12, 18), c(0, 0))
  for (i in 1:4) {
    ev <- pair_events[[i]]
    arm_data <- rbind(
      arm_data,
      add_arm(.fixture_outcomes[["pairwise"]], paste0("PB", i), "Treatment A", ev[1], 100),
      add_arm(.fixture_outcomes[["pairwise"]], paste0("PB", i), "Placebo", ev[2], 100)
    )
  }
  cont <- list(c(120, 15, 126, 16), c(118, 14, 124, 15), c(122, 13, 127, 14), c(119, 16, 125, 17))
  for (i in 1:4) {
    z <- cont[[i]]
    arm_data <- rbind(
      arm_data,
      add_arm(.fixture_outcomes[["continuous"]], paste0("PC", i), "Treatment A", sample = 80, mean = z[1], sd = z[2]),
      add_arm(.fixture_outcomes[["continuous"]], paste0("PC", i), "Placebo", sample = 82, mean = z[3], sd = z[4])
    )
  }
  for (i in 1:4) {
    arm_data <- rbind(
      arm_data,
      add_arm(.fixture_outcomes[["proportion"]], paste0("PP", i), "Population",
              c(0, 4, 12, 25)[i], c(100, 110, 120, 130)[i])
    )
  }
  arm_data <- rbind(
    arm_data,
    add_arm(.fixture_outcomes[["nma"]], "NM1", "Treatment A", 20, 100),
    add_arm(.fixture_outcomes[["nma"]], "NM1", "Treatment B", 28, 100),
    add_arm(.fixture_outcomes[["nma"]], "NM1", "Placebo", 35, 100),
    add_arm(.fixture_outcomes[["nma"]], "NM2", "Treatment A", 18, 90),
    add_arm(.fixture_outcomes[["nma"]], "NM2", "Placebo", 30, 92),
    add_arm(.fixture_outcomes[["nma"]], "NM3", "Treatment B", 22, 95),
    add_arm(.fixture_outcomes[["nma"]], "NM3", "Placebo", 31, 96)
  )

  effect_data <- .empty_df(.sheet_specs$effect_data)
  diagnostic_data <- data.frame(
    outcome_name = .fixture_outcomes[["diagnostic"]], study_label = paste0("DT", 1:6),
    tp = c(82, 74, 91, 68, 85, 77), fp = c(12, 9, 17, 11, 13, 8),
    fn = c(18, 16, 9, 22, 15, 13), tn = c(188, 201, 173, 189, 187, 202),
    threshold = "10 ng/mL", notes = "Fixture",
    stringsAsFactors = FALSE
  )
  selected_outcomes <- unname(.fixture_outcomes[active])
  analyses <- analyses[analyses$outcome_name %in% selected_outcomes, , drop = FALSE]
  arm_data <- arm_data[arm_data$outcome_name %in% selected_outcomes, , drop = FALSE]
  diagnostic_data <- diagnostic_data[
    diagnostic_data$outcome_name %in% selected_outcomes, , drop = FALSE
  ]
  .write_auctus_workbook(
    list(analyses = analyses, study_metadata = study_metadata, arm_data = arm_data,
         effect_data = effect_data, diagnostic_data = diagnostic_data),
    path, write_instructions = TRUE
  )
}
