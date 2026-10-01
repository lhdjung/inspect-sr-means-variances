# Self-check for app.R. Run from the project root with: Rscript tests.R
# Stops with an error at the first failing assertion.

app <- suppressMessages(source("app.R")$value)

ev <- function(
  x,
  sd = "",
  n = "",
  items = 1,
  type = "Mean",
  min = "",
  max = "",
  integer = TRUE
) {
  evaluate_row(x, sd, n, items, type, min, max, integer)
}

# GRIM / GRIMMER actually run and give known verdicts ----------------------
# (guards against scrutiny API changes silently disabling a test)

r <- ev("5.20", "2.54", "30")
stopifnot(identical(r$tests_run, c("GRIM", "GRIMMER")), isTRUE(r$ok))
r <- ev("5.20", "2.53", "30")
stopifnot(isFALSE(r$ok), identical(r$reasons, "SD fails GRIMMER (test 3)"))
r <- ev("5.21", "", "30")
stopifnot(isFALSE(r$ok), identical(r$reasons, "Mean fails GRIM"))
stopifnot(isTRUE(ev("45.5", "", "22", type = "Percentage")$ok))
stopifnot(isFALSE(ev("45.4", "", "22", type = "Percentage")$ok))

# Uninformative GRIM: N * items >= 10^digits
stopifnot(
  isTRUE(ev("5.2", "", "10")$uninformative),
  isFALSE(ev("5.2", "", "9")$uninformative),
  isTRUE(ev("5.23", "", "10", items = 10)$uninformative),
  isTRUE(ev("45.5", "", "2000", type = "Percentage")$uninformative),
  isFALSE(ev("45.5", "", "200", type = "Percentage")$uninformative),
  isFALSE(isTRUE(ev("5.2", "", "10", integer = FALSE)$uninformative))
)

# An internal failure is reported, not swallowed
real_grim <- grim
grim <- function(...) stop("boom")
r <- ev("5.20", "", "30")
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
  r <- ev(
    formatC(mean(d), format = "f", digits = sample(1:2, 1)),
    formatC(sd(d), format = "f", digits = sample(1:2, 1)),
    as.character(n),
    it,
    "Mean",
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
stopifnot(isTRUE(ev("0.0", "0.2", "25", min = "0", max = "1")$ok))
# All values identical: SD 0 is possible.
stopifnot(isTRUE(ev("3.00", "0.00", "20", min = "1", max = "5")$ok))
# Genuinely impossible SD and mean are still caught.
stopifnot(identical(
  ev("3.0", "2.5", "20", min = "1", max = "5")$reasons,
  "SD exceeds Bhatia–Davis bound"
))
stopifnot(identical(
  ev("1.0", "0.9", "20", min = "1", max = "5")$reasons,
  "SD exceeds Bhatia–Davis bound"
))
stopifnot(
  "Mean out of bounds" %in% ev("5.2", "", "20", min = "1", max = "5")$reasons
)
stopifnot(isTRUE(ev("5.0", "", "20", min = "1", max = "5")$ok))
# Without the integer flag, Bounds still run and the note says so.
r <- ev("3.0", "", "20", min = "1", max = "5", integer = FALSE)
stopifnot(
  isTRUE(r$ok),
  identical(r$tests_run, "Bounds"),
  grepl("Bounds only", r$notes)
)

# Input validation -----------------------------------------------------------

err <- function(...) ev(...)$err
stopifnot(
  identical(err("Inf", "", "20"), "Mean must be a number"),
  identical(err("0x10", "", "20"), "Mean must be a number"),
  identical(err("1e1", "", "20"), "Mean must be a number"),
  identical(
    err("5.2", "Inf", "20", min = "1", max = "7"),
    "SD must be a number"
  ),
  identical(err("5.2", "-1.2", "20"), "SD cannot be negative"),
  identical(err("5.2", "", "20.5"), "N must be a whole number"),
  identical(err("5.2", "", "10000000000"), "N is too large"),
  identical(
    err("5.2", "", "20", items = NA),
    "Items must be a positive whole number"
  ),
  identical(
    err("150", "", "20", type = "Percentage"),
    "Percentage must be between 0 and 100"
  ),
  identical(
    err("5.2", "", "20", min = "7", max = "1"),
    "Max must be greater than Min"
  ),
  is.null(err("5,20", "2,54", "30")),
  is.null(err("-.5", "", "30"))
)

# t-test recalculation ---------------------------------------------------------

tt <- function(..., p = "", op = "equals") evaluate_pair_ttest(..., p, op)
r <- tt("5.23", "2.1", "30", "4.10", "1.9", "30", p = "0.03")
stopifnot(r$status == "ok", isTRUE(r$inbounds), isFALSE(r$mixed_digits))
stopifnot(isFALSE(
  tt("5.23", "2.1", "30", "4.10", "1.9", "30", p = "0.30")$inbounds
))
stopifnot(isTRUE(
  tt(
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
  tt(
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
r <- tt("5.2", "2.1", "30", "4.10", "1.9", "30", p = "0.045")
stopifnot(
  isTRUE(r$mixed_digits),
  r$min_p < p_true,
  p_true < r$max_p,
  isTRUE(r$inbounds)
)
# The coarse range contains the fine one, including at an exact half (4.15).
fine <- tt("5.20", "2.10", "30", "4.15", "1.90", "30")
coarse <- tt("5.2", "2.1", "30", "4.15", "1.90", "30")
stopifnot(coarse$min_p <= fine$min_p, coarse$max_p >= fine$max_p)
stopifnot(
  identical(coarsen(4.15, 1), c(4.1, 4.2)),
  identical(coarsen(4.13, 1), 4.1)
)

stopifnot(
  tt("5.23", "2.1", "30.9", "4.10", "1.9", "30")$msg ==
    "N must be a whole number in both groups",
  tt("abc", "2.1", "30", "4.10", "1.9", "30")$msg ==
    "Mean, SD and N must be numbers",
  tt("5.23", "-2.1", "30", "4.10", "1.9", "30")$msg == "SD cannot be negative",
  tt("5.23", "2.1", "30", "4.10", "1.9", "30", p = "1.5")$msg ==
    "Reported p must be between 0 and 1",
  tt("5.23", "0.0", "30", "4.10", "1.9", "30")$status == "ok",
  tt("5.23", "", "30", "4.10", "1.9", "30")$status == "incomplete",
  tt("", "", "", "", "", "")$status == "blank"
)

stopifnot(
  fmt_p(0.0004) == "<0.001",
  fmt_p(0.0006) == "0.001",
  fmt_p(0.9994) == "0.999",
  fmt_p(0.9996) == ">0.999",
  fmt_p(1) == "1.000",
  fmt_p(0) == "<0.001"
)

# Server: row order, type/bounds sync, CSV --------------------------------------

shiny::testServer(app, {
  session$setInputs(gb_rm_2 = 1) # ignoreInit swallows the first value
  session$setInputs(gb_rm_2 = 2, gb_add = 1)
  stopifnot(identical(gb_slots(), 1:3))

  session$setInputs(
    gb_var_1 = "",
    gb_int_1 = TRUE,
    gb_type_1 = "Mean",
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
          "Mean",
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
    cb_int_1 = FALSE,
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
