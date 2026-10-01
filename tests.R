# Self-check for app.R. Run from the project root with: Rscript tests.R
# Stops with an error at the first failing assertion.

app <- suppressMessages(source("app.R")$value)

evaluate <- function(
  x,
  sd = "",
  n = "",
  items = 1,
  type = "Whole-number mean",
  min = "",
  max = "",
  integer = TRUE
) {
  if (!integer) type <- "Any mean"
  evaluate_row(x, sd, n, items, type, min, max)
}

# GRIM / GRIMMER actually run and give known verdicts ----------------------
# (guards against scrutiny API changes silently disabling a test)

r <- evaluate("5.20", "2.54", "30")
stopifnot(identical(r$tests_run, c("GRIM", "GRIMMER")), isTRUE(r$ok))
r <- evaluate("5.20", "2.53", "30")
stopifnot(isFALSE(r$ok), identical(r$reasons, "SD fails GRIMMER (test 3)"))
r <- evaluate("5.21", "", "30")
stopifnot(isFALSE(r$ok), identical(r$reasons, "Mean fails GRIM"))
stopifnot(isTRUE(evaluate("45.5", "", "22", type = "Percentage")$ok))
stopifnot(isFALSE(evaluate("45.4", "", "22", type = "Percentage")$ok))

# Uninformative GRIM: N * items >= 10^digits
stopifnot(
  isTRUE(evaluate("5.2", "", "10")$uninformative),
  isFALSE(evaluate("5.2", "", "9")$uninformative),
  isTRUE(evaluate("5.23", "", "10", items = 10)$uninformative),
  isTRUE(evaluate("45.5", "", "2000", type = "Percentage")$uninformative),
  isFALSE(evaluate("45.5", "", "200", type = "Percentage")$uninformative),
  isFALSE(isTRUE(evaluate("5.2", "", "10", integer = FALSE)$uninformative))
)

# An internal failure is reported, not swallowed
real_grim <- grim
grim <- function(...) stop("boom")
r <- evaluate("5.20", "", "30")
stopifnot(is.na(r$ok), grepl("Internal error: boom", r$err))
grim <- real_grim
rm(real_grim)

# No false positives on real data ------------------------------------------
# Summary statistics of genuine integer data inside the bounds must never be
# flagged, whatever the rounding.

set.seed(42)
for (i in 1:2000) {
  n <- sample(2:60, 1)
  it <- sample(c(1, 1, 1, 3, 5), 1)
  lo <- sample(c(0, 1, -3), 1)
  hi <- lo + sample(1:10, 1)
  prob <- if (runif(1) < .4) c(20, rep(1, hi - lo)) else NULL
  d <- rowMeans(matrix(sample(lo:hi, n * it, TRUE, prob = prob), n))
  r <- evaluate(
    formatC(mean(d), format = "f", digits = sample(1:2, 1)),
    formatC(sd(d), format = "f", digits = sample(1:2, 1)),
    as.character(n),
    it,
    "Whole-number mean",
    as.character(lo),
    as.character(hi)
  )
  if (!isTRUE(r$ok)) {
    stop(
      "False positive: ",
      paste(c(r$reasons, r$err), collapse = "; "),
      " for data ",
      paste(d, collapse = ",")
    )
  }
}

# Bounds ---------------------------------------------------------------------

# One 1 among 24 zeros: mean 0.04 -> "0.0", SD 0.2. Possible.
stopifnot(isTRUE(evaluate("0.0", "0.2", "25", min = "0", max = "1")$ok))
# All values identical: SD 0 is possible.
stopifnot(isTRUE(evaluate("3.00", "0.00", "20", min = "1", max = "5")$ok))
# Genuinely impossible SD and mean are still caught.
stopifnot(identical(
  evaluate("3.0", "2.5", "20", min = "1", max = "5")$reasons,
  "SD exceeds Bhatia–Davis bound"
))
stopifnot(identical(
  evaluate("1.0", "0.9", "20", min = "1", max = "5")$reasons,
  "SD exceeds Bhatia–Davis bound"
))
stopifnot(
  "Mean out of bounds" %in%
    evaluate("5.2", "", "20", min = "1", max = "5")$reasons
)
stopifnot(isTRUE(evaluate("5.0", "", "20", min = "1", max = "5")$ok))
# "Any mean" skips GRIM/GRIMMER; Bounds still run and the note says so.
r <- evaluate("3.0", "", "20", min = "1", max = "5", integer = FALSE)
stopifnot(
  isTRUE(r$ok),
  identical(r$tests_run, "Bounds"),
  identical(r$notes, "Bounds only; GRIM/GRIMMER not run for \"Any mean\"")
)
# The default type runs no GRIM, even on an impossible mean.
stopifnot(isTRUE(is.na(evaluate_row("5.21", "", "30", 1, "Any mean", "", "")$ok)))

# Without N, skipped tests are named instead of passing silently.
r <- evaluate("3.0", "2.5", "", min = "1", max = "5")
stopifnot(
  isTRUE(r$ok),
  identical(r$tests_run, "Bounds"),
  identical(r$notes, "Mean bounds only; GRIM/GRIMMER and SD bound need N")
)
stopifnot(
  identical(
    evaluate("3.45", "", "", min = "1", max = "5")$notes,
    "Bounds only; GRIM/GRIMMER need N"
  ),
  identical(evaluate("3.45")$notes, "Awaiting N for GRIM/GRIMMER"),
  identical(evaluate("", "1.2", "30")$notes, "Awaiting mean"),
  identical(
    evaluate("3.0", "2.5", "", min = "1", max = "5", integer = FALSE)$notes,
    c("Mean bounds only; GRIM/GRIMMER not run for \"Any mean\"", "SD bound needs N")
  )
)

# A single bound still checks the mean, and a note asks for the other one.
r <- evaluate("7.2", "9.9", "20", max = "5")
stopifnot(
  identical(r$reasons, "Mean out of bounds"),
  identical(r$notes, "Only Max given; add Min for a more informative bounds check")
)
r <- evaluate("3.0", "", "", min = "1")
stopifnot(
  isTRUE(r$ok),
  identical(r$tests_run, "Bounds"),
  identical(
    r$notes,
    c(
      "Mean bound only; GRIM/GRIMMER need N",
      "Only Min given; add Max for a more informative bounds check"
    )
  )
)
stopifnot(isFALSE(evaluate("0.2", "", "", min = "1", integer = FALSE)$ok))

# Max == Min is allowed (all values identical): the SD must then round to 0.
stopifnot(
  isTRUE(evaluate("3.0", "0.0", "20", min = "3", max = "3")$ok),
  identical(
    evaluate("3.0", "0.5", "20", min = "3", max = "3")$reasons,
    "SD exceeds Bhatia–Davis bound"
  ),
  "Mean out of bounds" %in% evaluate("3.4", "", "20", min = "3", max = "3")$reasons
)

# Input validation -----------------------------------------------------------

validation_error <- function(...) evaluate(...)$err
stopifnot(
  identical(validation_error("Inf", "", "20"), "Mean must be a number"),
  identical(validation_error("0x10", "", "20"), "Mean must be a number"),
  identical(validation_error("1e1", "", "20"), "Mean must be a number"),
  identical(
    validation_error("5.2", "Inf", "20", min = "1", max = "7"),
    "SD must be a number"
  ),
  identical(validation_error("5.2", "-1.2", "20"), "SD cannot be negative"),
  identical(validation_error("5.2", "", "20.5"), N_FORMAT_MSG),
  # Separators are ambiguous across locales and would otherwise read as N = 2
  identical(validation_error("5.21", "", "2,000"), N_FORMAT_MSG),
  identical(validation_error("5.21", "", "2.000"), N_FORMAT_MSG),
  identical(validation_error("5.2", "", "10000000000"), "N is too large"),
  identical(
    validation_error("5.2", "", "20", items = NA),
    "Items must be a positive whole number"
  ),
  identical(
    validation_error("150", "", "20", type = "Percentage"),
    "Percentage must be between 0 and 100"
  ),
  identical(
    validation_error("5.2", "", "20", min = "7", max = "1"),
    "Max cannot be less than Min"
  ),
  # Commas are ambiguous (thousands or decimals) and rejected everywhere
  identical(validation_error("1,234", "", "20"), comma_msg("Mean")),
  identical(validation_error("5.20", "2,54", "30"), comma_msg("SD")),
  identical(validation_error("5.2", "", "20", max = "7,0"), comma_msg("Max")),
  is.null(validation_error("-.5", "", "30"))
)

# t-test recalculation ---------------------------------------------------------

run_t_test <- function(..., p = "", op = "equals") {
  evaluate_pair_t_test(..., p, op)
}
r <- run_t_test("5.23", "2.1", "30", "4.10", "1.9", "30", p = "0.03")
stopifnot(r$status == "ok", isTRUE(r$inbounds), isFALSE(r$mixed_digits))
stopifnot(isFALSE(
  run_t_test("5.23", "2.1", "30", "4.10", "1.9", "30", p = "0.30")$inbounds
))
stopifnot(isTRUE(
  run_t_test(
    "5.23",
    "2.1",
    "30",
    "4.10",
    "1.9",
    "30",
    p = "0.05",
    op = "less_than"
  )$inbounds
))
stopifnot(isFALSE(
  run_t_test(
    "5.23",
    "2.1",
    "30",
    "4.10",
    "1.9",
    "30",
    p = "0.001",
    op = "less_than"
  )$inbounds
))

# Mixed precision: a true m1 of 5.16 is reported as "5.2" and gives p = .045,
# which must be inside the range.
p_true <- 2 * pt(-abs((5.16 - 4.10) / sqrt(2.1^2 / 30 + 1.9^2 / 30)), 58)
r <- run_t_test("5.2", "2.1", "30", "4.10", "1.9", "30", p = "0.045")
stopifnot(
  isTRUE(r$mixed_digits),
  r$min_p < p_true,
  p_true < r$max_p,
  isTRUE(r$inbounds)
)
# The coarse range contains the fine one, including at an exact half (4.15).
fine <- run_t_test("5.20", "2.10", "30", "4.15", "1.90", "30")
coarse <- run_t_test("5.2", "2.1", "30", "4.15", "1.90", "30")
stopifnot(coarse$min_p <= fine$min_p, coarse$max_p >= fine$max_p)
stopifnot(
  identical(coarsen(4.15, 1), c(4.1, 4.2)),
  identical(coarsen(4.13, 1), 4.1)
)

stopifnot(
  run_t_test("5.23", "2.1", "30.9", "4.10", "1.9", "30")$msg == N_FORMAT_MSG,
  run_t_test("5.23", "2.1", "30", "4.10", "1.9", "2,000")$msg == N_FORMAT_MSG,
  run_t_test("abc", "2.1", "30", "4.10", "1.9", "30")$msg ==
    "Mean and SD must be numbers",
  run_t_test("5,23", "2.1", "30", "4.10", "1.9", "30")$msg ==
    comma_msg("Mean or SD"),
  run_t_test("5.23", "2.1", "30", "4.10", "1.9", "30", p = "0,03")$msg ==
    comma_msg("Reported p"),
  run_t_test("5.23", "-2.1", "30", "4.10", "1.9", "30")$msg ==
    "SD cannot be negative",
  run_t_test("5.23", "2.1", "30", "4.10", "1.9", "30", p = "1.5")$msg ==
    "Reported p must be between 0 and 1",
  run_t_test("5.23", "0.0", "30", "4.10", "1.9", "30")$status == "ok",
  run_t_test("5.23", "", "30", "4.10", "1.9", "30")$status == "incomplete",
  run_t_test("", "", "", "", "", "")$status == "blank"
)

stopifnot(
  format_p_value(0.0004) == "<0.001",
  format_p_value(0.0006) == "0.001",
  format_p_value(0.9994) == "0.999",
  format_p_value(0.9996) == ">0.999",
  format_p_value(1) == "1.000",
  format_p_value(0) == "<0.001"
)

# Server: row order, type/bounds sync, CSV --------------------------------------

shiny::testServer(app, {
  session$setInputs(gb_rm_2 = 1) # ignoreInit swallows the first value
  session$setInputs(gb_rm_2 = 2, gb_add = 1)
  stopifnot(identical(gb_slots(), 1:3))

  session$setInputs(
    gb_var_1 = "",
    gb_type_1 = "Whole-number mean",
    gb_x_1 = "5.2",
    gb_sd_1 = "",
    gb_n_1 = "10",
    gb_items_1 = 1,
    gb_min_1 = "",
    gb_max_1 = ""
  )
  csv <- read.csv(output$gb_download, colClasses = "character")
  stopifnot(
    nrow(csv) == 1,
    csv$test == "GRIM",
    csv$consistent == "TRUE",
    grepl("Uninformative GRIM", csv$notes)
  )

  for (rid in c("1a", "1b")) {
    do.call(
      session$setInputs,
      setNames(
        list(
          "Any mean",
          if (rid == "1a") "" else "4.10",
          "1.9",
          "30",
          1,
          "",
          "",
          ""
        ),
        paste0(
          "cb_",
          c("type_", "x_", "sd_", "n_", "items_", "min_", "max_", "grp_"),
          rid
        )
      )
    )
  }
  session$setInputs(
    cb_var_1 = "BDI",
    cb_p_1 = "1.5",
    cb_pop_1 = "equals"
  )
  csv <- read.csv(output$download_csv, colClasses = "character")
  stopifnot(
    nrow(csv) == 1,
    csv$label == "BDI",
    csv$group == "2",
    csv$reported_p == "1.5",
    csv$p_note == "Reported p must be between 0 and 1"
  )
})

cat("All checks passed.\n")
