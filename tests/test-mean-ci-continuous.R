# Regression: continuous MA accepts mean1/lowci1/highci1 + mean2/lowci2/highci2
engine <- normalizePath(file.path("..", "archives", "meta_nma_engine.R.bak.original"),
                        mustWork = TRUE)
source(engine, local = FALSE)

outdir <- file.path(tempdir(), paste0("mean_ci_test_", format(Sys.time(), "%H%M%S")))
dir.create(outdir, recursive = TRUE)
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(outdir)

dat <- data.frame(
  study   = c("Kim et al (2026)", "Lee et al (2024)"),
  sample1 = c(50, 60),
  sample2 = c(48, 55),
  mean1   = c(7.2, 7.0),
  lowci1  = c(6.9, 6.7),
  highci1 = c(7.5, 7.3),
  mean2   = c(7.8, 7.6),
  lowci2  = c(7.5, 7.3),
  highci2 = c(8.1, 7.9),
  stringsAsFactors = FALSE
)

warns <- character()
withCallingHandlers(
  res <- run_continuous_outcome_analysis(
    dat, outcome_name = "HbA1c", effect_measure = "MD"
  ),
  warning = function(w) {
    warns <<- c(warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)

incomplete <- grepl("incomplete\\. Skipped|missing md/lowci/highci", warns)
stopifnot(!any(incomplete))
stopifnot(!is.null(res$overall_meta))
stopifnot(res$overall_meta$k == 2)
cat("PASS mean+CI with n: k=", res$overall_meta$k,
    " MD=", round(as.numeric(res$overall_meta$b), 4), "\n", sep = "")

# Without n: still compute MD from two mean CIs
dat2 <- dat
dat2$sample1 <- NA_real_
dat2$sample2 <- NA_real_
warns2 <- character()
withCallingHandlers(
  res2 <- run_continuous_outcome_analysis(
    dat2, outcome_name = "HbA1c no n", effect_measure = "MD"
  ),
  warning = function(w) {
    warns2 <<- c(warns2, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
incomplete2 <- grepl("incomplete\\. Skipped|missing md/lowci/highci", warns2)
stopifnot(!any(incomplete2))
stopifnot(res2$overall_meta$k == 2)
cat("PASS mean+CI without n: k=", res2$overall_meta$k, "\n", sep = "")

cat("ALL PASS\n")
