# Self-check for app.R. Run from the project root with: Rscript tests.R
# Stops with an error at the first failing assertion. Takes about a minute.
# Run it before every deploy.

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
  if (!integer) {
    type <- "Any mean"
  }
  evaluate_row(x, sd, n, items, type, min, max)
}

# Prints each part as it runs and how long it took to pass.
section_start <- NULL
section <- function(name = NULL) {
  if (!is.null(section_start)) {
    elapsed <- (proc.time() - section_start)[["elapsed"]]
    cat(sprintf("passed (%.1f s)\n", elapsed))
  }
  if (!is.null(name)) {
    cat(name, "... ")
  }
  section_start <<- proc.time()
}

section("Known GRIM/GRIMMER verdicts")

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

section("No false positives on 2,000 simulated data sets")

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

section("GRIM against brute force (4,000 cases)")

# Brute-force oracles --------------------------------------------------------
# Hand-picked expected values come from the same reasoning as the code, so they
# can share its mistakes: the percentage bug of a0a4a05 (dp + 4 instead of
# dp + 2) went unnoticed for five months. The checks below compare the app with
# answers computed here from first principles, without scrutiny.

decimals <- function(s) {
  if (grepl(".", s, fixed = TRUE)) nchar(sub(".*\\.", "", s)) else 0L
}
# "45.5" -> 455: the reported value in units of its last decimal place
units <- function(s) as.numeric(sub(".", "", s, fixed = TRUE))

# Report `v` at `d` decimal places, rounding a tie up or down as authors may.
fmt <- function(v, d, up = runif(1) < .5) {
  u <- v * 10^d
  u <- if (up) floor(u + 0.5 + 1e-9) else ceiling(u - 0.5 - 1e-9)
  formatC(u / 10^d + 0, format = "f", digits = d) # + 0 turns -0 into 0
}

# A mean of N whole numbers is K / N, a percentage 100 * K / N. Reported as r
# units of 1 / M, it is possible if some K lands within half a unit of r
# (ties may go either way). Integer arithmetic, so no floating-point error.
grim_hit <- function(r, N, M) {
  K <- floor(r * N / M)
  2 * abs(K * M - r * N) <= N | 2 * abs((K + 1) * M - r * N) <= N
}

set.seed(7)
grim_cases <- lapply(1:4000, function(i) {
  pct <- runif(1) < .5
  d <- sample(0:3, 1)
  x <- if (pct) runif(1, 0, 100) else runif(1, -5, 20)
  list(
    pct = pct,
    x = formatC(round(x, d), format = "f", digits = d),
    n = sample(c(2:60, sample(61:20000, 1)), 1),
    items = if (pct) 1 else sample(c(1, 1, 2, 3, 7), 1)
  )
})
grim_cases <- c(
  grim_cases,
  lapply(c("0", "100", "0.0", "100.00", "50"), function(x) {
    list(pct = TRUE, x = x, n = 7, items = 1)
  })
)

# Returns the first case where the app disagrees with brute force, or NULL.
grim_mismatch <- function(cases) {
  for (k in cases) {
    type <- if (k$pct) "Percentage" else "Whole-number mean"
    r <- evaluate(k$x, "", as.character(k$n), k$items, type = type)
    N <- k$n * k$items
    M <- (if (k$pct) 100 else 1) * 10^decimals(k$x)
    # Uninformative: every reportable value in one period is possible.
    uninformative <- all(grim_hit(0:(M - 1), N, M))
    if (
      !identical(r$tests_run, "GRIM") ||
        !identical(r$ok, grim_hit(units(k$x), N, M)) ||
        !identical(r$uninformative, uninformative) ||
        r$grim_digits != log10(M)
    ) {
      return(c(k, list(app_ok = r$ok, app_uninformative = r$uninformative)))
    }
  }
  NULL
}

bad <- grim_mismatch(grim_cases)
if (!is.null(bad)) {
  stop("GRIM disagrees with brute force: ", deparse(bad))
}

section("Brute-force check catches 3 reinstated GRIM bugs")

# The oracle must have teeth: reinstate known bugs and expect it to object.
fixed_safe_grim <- safe_grim
mutants <- list(
  historical_dp_plus_4 = function(x_str, n_str, items, percent = FALSE) {
    dx <- count_decimal_places(x_str) + if (percent) 2L else 0L
    fixed_safe_grim(x_str, n_str, items, percent) &
      grim(
        parse_number(x_str),
        as.integer(n_str),
        dx,
        as.integer(items),
        percent
      )
  },
  percent_ignored = function(x_str, n_str, items, percent = FALSE) {
    fixed_safe_grim(x_str, n_str, items, percent = FALSE)
  },
  items_ignored = function(x_str, n_str, items, percent = FALSE) {
    fixed_safe_grim(x_str, n_str, 1, percent)
  }
)
for (name in names(mutants)) {
  safe_grim <- mutants[[name]]
  if (is.null(grim_mismatch(grim_cases))) {
    safe_grim <- fixed_safe_grim
    stop("GRIM oracle failed to catch the mutant ", name)
  }
}
safe_grim <- fixed_safe_grim
rm(fixed_safe_grim, mutants)

section("No false positives on every small data set (n = 2-7)")

# Every data set of n whole numbers on a small scale, for the small n where
# GRIM and GRIMMER are most powerful, rounded every way: none may be flagged.
for (scale in list(1:5, 0:3, -2:2)) {
  m <- length(scale)
  for (n in 2:7) {
    # Columns are all nondecreasing index vectors, i.e. all multisets.
    idx <- combn(m + n - 1, n) - 0:(n - 1)
    for (j in seq_len(ncol(idx))) {
      data <- scale[idx[, j]]
      for (d in 1:2) {
        for (up in c(TRUE, FALSE)) {
          x <- fmt(mean(data), d, up)
          sd_str <- fmt(sd(data), d, !up)
          r <- evaluate(
            x,
            sd_str,
            as.character(n),
            type = "Whole-number mean",
            min = as.character(min(scale)),
            max = as.character(max(scale))
          )
          if (!isTRUE(r$ok)) {
            stop(
              "False positive: ",
              paste(c(r$reasons, r$err), collapse = "; "),
              " for M = ",
              x,
              ", SD = ",
              sd_str,
              " from data ",
              paste(data, collapse = ",")
            )
          }
        }
      }
    }
  }
}

section("No false positives on 1,500 percentage and \"Any mean\" data sets")

# Percentages from genuine yes/no counts, with and without an SD in
# percentage points (Min 0 and Max 100, as the UI fills in), and continuous
# "Any mean" data, half the time piled up at the bounds where the Bhatia–Davis
# bound is reached exactly.
set.seed(11)
for (i in 1:1500) {
  n <- sample(c(2:100, sample(101:5000, 1)), 1)
  if (runif(1) < .5) {
    type <- "Percentage"
    lo <- 0
    hi <- 100
    data <- 100 * rbinom(n, 1, runif(1))
    x <- fmt(mean(data), sample(0:2, 1))
  } else {
    type <- "Any mean"
    lo <- sample(c(0, 1, -3), 1)
    hi <- lo + sample(1:10, 1)
    data <- if (runif(1) < .5) {
      sample(c(lo, hi), n, TRUE)
    } else {
      runif(n, lo, hi)
    }
    x <- fmt(mean(data), sample(1:2, 1))
  }
  sd_str <- if (runif(1) < .5) fmt(sd(data), sample(1:2, 1)) else ""
  r <- evaluate(
    x,
    sd_str,
    as.character(n),
    1,
    type,
    as.character(lo),
    as.character(hi)
  )
  if (!isTRUE(r$ok)) {
    stop(
      "False positive (",
      type,
      "): ",
      paste(c(r$reasons, r$err), collapse = "; "),
      " for M = ",
      x,
      ", SD = ",
      sd_str,
      ", N = ",
      n
    )
  }
}

section("Bounds")

# Bounds ---------------------------------------------------------------------

# The bounds are tight. Half 1s and half 5s give mean 3 and the largest
# possible SD, so that SD passes and one unit more fails.
for (n in c(2, 4, 10, 50)) {
  sd_max <- round(2 * sqrt(n / (n - 1)), 2)
  sd_ok <- function(s) {
    evaluate(
      "3.00",
      formatC(s, format = "f", digits = 2),
      as.character(n),
      min = "1",
      max = "5",
      integer = FALSE
    )$ok
  }
  stopifnot(isTRUE(sd_ok(sd_max)), isFALSE(sd_ok(sd_max + 0.01)))
}
mean_ok <- function(x) {
  evaluate(x, "", "20", min = "1", max = "5", integer = FALSE)$ok
}
stopifnot(
  isTRUE(mean_ok("5.00")),
  isFALSE(mean_ok("5.01")),
  isTRUE(mean_ok("1.00")),
  isFALSE(mean_ok("0.99"))
)

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
stopifnot(isTRUE(is.na(
  evaluate_row("5.21", "", "30", 1, "Any mean", "", "")$ok
)))

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
    c(
      "Mean bounds only; GRIM/GRIMMER not run for \"Any mean\"",
      "SD bound needs N"
    )
  )
)

# A single bound still checks the mean, and a note asks for the other one.
r <- evaluate("7.2", "9.9", "20", max = "5")
stopifnot(
  identical(r$reasons, "Mean out of bounds"),
  identical(
    r$notes,
    "Only Max given; add Min for a more informative bounds check"
  )
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
  "Mean out of bounds" %in%
    evaluate("3.4", "", "20", min = "3", max = "3")$reasons
)

section("Input validation")

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

section("t-test recalculation")

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

section("t-test p values from 60 simulated data sets")

# p values recalculated from genuine data, by Student's or Welch's test,
# must be reproduced from the rounded summary statistics.
set.seed(3)
for (i in 1:60) {
  n1 <- sample(5:80, 1)
  n2 <- sample(5:80, 1)
  g1 <- sample(1:7, n1, TRUE)
  g2 <- sample(1:7, n2, TRUE, prob = c(1, 1, 2, 3, 3, 2, 1))
  p <- t.test(g1, g2, var.equal = runif(1) < .5)$p.value
  dm <- sample(1:2, 1)
  ds <- sample(1:2, 1)
  args <- list(
    fmt(mean(g1), dm),
    fmt(sd(g1), ds),
    as.character(n1),
    fmt(mean(g2), dm),
    fmt(sd(g2), ds),
    as.character(n2),
    p = fmt(p, sample(2:3, 1))
  )
  if (!isTRUE(do.call(run_t_test, args)$inbounds)) {
    stop("Genuine p not reproduced: ", deparse(args))
  }
}

stopifnot(
  format_p_value(0.0004) == "<0.001",
  format_p_value(0.0006) == "0.001",
  format_p_value(0.9994) == "0.999",
  format_p_value(0.9996) == ">0.999",
  format_p_value(1) == "1.000",
  format_p_value(0) == "<0.001"
)

section("Server: rows, percentage type, CSV download")

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

  # The Percentage type reaches the percentage GRIM through the UI.
  session$setInputs(
    gb_type_1 = "Percentage",
    gb_x_1 = "45.4",
    gb_n_1 = "22",
    gb_min_1 = "0",
    gb_max_1 = "100"
  )
  csv <- read.csv(output$gb_download, colClasses = "character")
  stopifnot(
    csv$consistent == "FALSE",
    csv$inconsistency == "Percentage fails GRIM"
  )
  session$setInputs(gb_x_1 = "45.5")
  csv <- read.csv(output$gb_download, colClasses = "character")
  stopifnot(csv$consistent == "TRUE", csv$test == "GRIM+Bounds")

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

section()
cat("All checks passed.\n")
