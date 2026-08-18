# Generate reproducible dummy pairwise forest plots with Auctus Engine V2.3.
# Run from any working directory with:
#   Rscript examples/generate_dummy_pairwise_forest.R

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath("examples/generate_dummy_pairwise_forest.R", mustWork = TRUE)
}

project_dir <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
example_dir <- file.path(project_dir, "examples")
input_path <- file.path(example_dir, "dummy_pairwise_OR_MD.xlsx")
output_dir <- file.path(example_dir, "dummy_pairwise_OR_MD_results")

if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE, force = TRUE)

source(file.path(project_dir, "meta_nma_engine.R"))

# The workbook writer is obtained from the current engine environment so the
# dummy input uses the same schema, styling, dropdowns, and validation rules.
engine_env <- environment(run_auctus_meta)
write_workbook <- get(".write_auctus_workbook", envir = engine_env, inherits = FALSE)

or_outcome <- "30-day mortality"
md_outcome <- "Systolic blood pressure at 12 weeks"

analyses <- data.frame(
  outcome_name = c(or_outcome, md_outcome),
  analysis_type = "pairwise_ma",
  timepoint = NA_character_,
  outcome_type = c("binary", "continuous"),
  effect_measure = c("OR", "MD"),
  reference_treatment = "Standard care",
  outcome_direction = "lower_better",
  unit = c(NA_character_, "mmHg"),
  notes = c(
    "Dummy example: event/sample data for a pairwise odds-ratio meta-analysis.",
    "Dummy example: mean, SD, and sample data for a pairwise mean-difference meta-analysis."
  ),
  stringsAsFactors = FALSE
)

or_studies <- c(
  "Anderson 2018", "Budi 2019", "Chen 2020", "Davis 2021",
  "Evans 2022", "Fauzi 2022", "Garcia 2023", "Hassan 2024",
  "Ibrahim 2024", "Kim 2025", "Lopez 2025", "Morgan 2026"
)
md_studies <- c(
  "Ito 2017", "Jones 2018", "Kumar 2019", "Lim 2020",
  "Miller 2021", "Nugroho 2022", "Oliveira 2023", "Patel 2024",
  "Quinn 2024", "Rahman 2025", "Silva 2025", "Tan 2026"
)

study_metadata <- data.frame(
  study_label = c(or_studies, md_studies),
  year = c(
    2018, 2019, 2020, 2021, 2022, 2022, 2023, 2024, 2024, 2025, 2025, 2026,
    2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2024, 2025, 2025, 2026
  ),
  study_design = "RCT",
  subgroup_region = rep(c("Asia", "Europe"), times = 12),
  moderator_num_mean_age = c(
    48, 52, 55, 59, 61, 64, 50, 57, 63, 67, 54, 70,
    46, 49, 53, 56, 60, 62, 51, 58, 65, 68, 55, 71
  ),
  stringsAsFactors = FALSE
)

make_binary_arms <- function(studies, event_treatment, sample_treatment,
                             event_control, sample_control) {
  rows <- vector("list", length(studies) * 2L)
  for (i in seq_along(studies)) {
    rows[[2L * i - 1L]] <- data.frame(
      outcome_name = or_outcome,
      study_label = studies[[i]],
      treatment = "Intensive treatment strategy",
      event = event_treatment[[i]],
      sample = sample_treatment[[i]],
      mean = NA_real_, sd = NA_real_, median = NA_real_,
      q1 = NA_real_, q3 = NA_real_, min = NA_real_, max = NA_real_,
      notes = "Dummy arm-level binary data",
      stringsAsFactors = FALSE
    )
    rows[[2L * i]] <- data.frame(
      outcome_name = or_outcome,
      study_label = studies[[i]],
      treatment = "Standard care",
      event = event_control[[i]],
      sample = sample_control[[i]],
      mean = NA_real_, sd = NA_real_, median = NA_real_,
      q1 = NA_real_, q3 = NA_real_, min = NA_real_, max = NA_real_,
      notes = "Dummy arm-level binary data",
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

make_continuous_arms <- function(studies, mean_treatment, sd_treatment,
                                 sample_treatment, mean_control, sd_control,
                                 sample_control) {
  rows <- vector("list", length(studies) * 2L)
  for (i in seq_along(studies)) {
    rows[[2L * i - 1L]] <- data.frame(
      outcome_name = md_outcome,
      study_label = studies[[i]],
      treatment = "Intensive treatment strategy",
      event = NA_real_, sample = sample_treatment[[i]],
      mean = mean_treatment[[i]], sd = sd_treatment[[i]], median = NA_real_,
      q1 = NA_real_, q3 = NA_real_, min = NA_real_, max = NA_real_,
      notes = "Dummy arm-level continuous data",
      stringsAsFactors = FALSE
    )
    rows[[2L * i]] <- data.frame(
      outcome_name = md_outcome,
      study_label = studies[[i]],
      treatment = "Standard care",
      event = NA_real_, sample = sample_control[[i]],
      mean = mean_control[[i]], sd = sd_control[[i]], median = NA_real_,
      q1 = NA_real_, q3 = NA_real_, min = NA_real_, max = NA_real_,
      notes = "Dummy arm-level continuous data",
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

binary_arms <- make_binary_arms(
  or_studies,
  event_treatment = c(12, 18, 7, 28, 15, 22, 9, 31, 10, 25, 17, 20),
  sample_treatment = c(150, 220, 95, 310, 170, 260, 130, 280, 160, 300, 210, 235),
  event_control = c(20, 29, 11, 34, 26, 24, 18, 37, 16, 27, 31, 26),
  sample_control = c(148, 218, 93, 305, 168, 255, 132, 276, 158, 295, 208, 230)
)

continuous_arms <- make_continuous_arms(
  md_studies,
  mean_treatment = c(
    128.4, 132.1, 125.8, 130.2, 127.6, 129.5, 126.9, 131.0,
    124.8, 130.6, 128.1, 127.2
  ),
  sd_treatment = c(14.2, 15.1, 13.8, 16.0, 14.7, 15.4, 13.9, 14.5, 13.6, 15.2, 14.3, 14.8),
  sample_treatment = c(84, 110, 76, 125, 98, 140, 92, 118, 88, 132, 105, 126),
  mean_control = c(
    134.6, 136.0, 131.7, 134.0, 133.1, 132.8, 133.5, 135.2,
    132.0, 135.7, 131.2, 134.5
  ),
  sd_control = c(14.8, 15.6, 14.1, 16.4, 15.0, 15.7, 14.4, 15.1, 14.0, 15.5, 14.6, 15.0),
  sample_control = c(82, 108, 78, 121, 101, 138, 90, 120, 86, 130, 107, 124)
)

arm_data <- rbind(binary_arms, continuous_arms)

write_workbook(
  data = list(
    analyses = analyses,
    study_metadata = study_metadata,
    arm_data = arm_data,
    effect_data = data.frame(),
    diagnostic_data = data.frame()
  ),
  output_path = input_path,
  write_instructions = TRUE
)

result <- run_auctus_meta(
  file_path = input_path,
  output_dir = output_dir,
  run_mode = "strict"
)

cat("\nDummy workbook:\n", input_path, "\n", sep = "")
cat("\nGenerated plots:\n")
for (outcome in names(result$analyses)) {
  selected_paths <- result$analyses[[outcome]]$plots[
    grepl(
      "forest_overall|leave_one_out|forest_subgroup|bubble_",
      result$analyses[[outcome]]$plots
    )
  ]
  cat("- ", outcome, ":\n  ", paste(selected_paths, collapse = "\n  "), "\n", sep = "")
}
