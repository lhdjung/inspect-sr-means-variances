# Self-check for app.R. Run from the project root with: Rscript tests.R
# Stops with an error at the first failing assertion. Takes about four minutes.
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
  isFALSE(
    evaluate(
      "5.2",
      "",
      "10",
      min = "1",
      max = "7",
      integer = FALSE
    )$uninformative
  )
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
# flagged, whatever the rounding: 0 to 3 decimals, small and large samples,
# short scales and wide ranges (e.g. age in years, scores up to 1000).

set.seed(42)
for (i in 1:2000) {
  n <- if (runif(1) < .85) sample(2:60, 1) else sample(61:400, 1)
  it <- sample(c(1, 1, 1, 3, 5), 1)
  lo <- sample(c(0, 1, -3, 18), 1)
  hi <- lo + sample(c(1:10, 72, 1000), 1)
  prob <- if (runif(1) < .4) c(20, rep(1, hi - lo)) else NULL
  d <- rowMeans(matrix(sample(lo:hi, n * it, TRUE, prob = prob), n))
  r <- evaluate(
    formatC(mean(d), format = "f", digits = sample(0:3, 1)),
    formatC(sd(d), format = "f", digits = sample(0:3, 1)),
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
# Swaps in each mutant for the app function `name` and expects `find()` to
# report a mismatch; the original is restored either way.
expect_caught <- function(name, mutants, find) {
  original <- get(name, globalenv())
  on.exit(assign(name, original, globalenv()))
  for (m in names(mutants)) {
    assign(name, mutants[[m]], globalenv())
    if (is.null(find())) {
      stop("Oracle failed to catch the mutant ", m, " of ", name)
    }
  }
}
# A copy of app function `f` with one piece of its code replaced.
mutate <- function(f, from, to) {
  code <- paste(deparse(f), collapse = "\n")
  stopifnot(grepl(from, code, fixed = TRUE))
  eval(parse(text = sub(from, to, code, fixed = TRUE)))
}

fixed_safe_grim <- safe_grim
expect_caught(
  "safe_grim",
  list(
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
  ),
  function() grim_mismatch(grim_cases)
)
rm(fixed_safe_grim)

section("GRIMMER against brute force (3,000 cases) and 5 mutants")

# GRIMMER's conditions from first principles. Each of the n participants has a
# whole-number total T_i of their `items` answers. Some sum S of these must
# give the mean (as for GRIM), and some sum Q of their squares must give the
# SD, sqrt((Q - S^2 / n) / ((n - 1) * items^2)), and have the parity of S,
# since T_i^2 and T_i are both even or both odd. Ties may round either way.
# Integer arithmetic, so no floating-point error.
grimmer_hit <- function(x, sd, n, items) {
  N <- n * items
  Mx <- 10^decimals(x)
  Ms <- 10^decimals(sd)
  r <- units(x)
  u <- units(sd)
  S <- floor((r - 0.5) * N / Mx):ceiling((r + 0.5) * N / Mx)
  S <- S[2 * abs(S * Mx - r * N) <= N]
  # The SD lies within half a unit of u / Ms if 4 * Ms^2 * (n * Q - S^2) lies
  # between these two:
  lo <- n * (n - 1) * items^2 * max(2 * u - 1, 0)^2
  hi <- n * (n - 1) * items^2 * (2 * u + 1)^2
  for (s in S) {
    Q <- floor((s^2 + lo / (4 * Ms^2)) / n):ceiling((s^2 + hi / (4 * Ms^2)) / n)
    v <- 4 * Ms^2 * (n * Q - s^2)
    if (any(v >= lo & v <= hi & (Q - s) %% 2 == 0)) {
      return(TRUE)
    }
  }
  FALSE
}

# Genuine statistics of whole-number data, the mean nudged by a unit some of
# the time and the SD by up to 4, so that every verdict is common.
set.seed(5)
grimmer_cases <- lapply(1:3000, function(i) {
  n <- sample(c(2:30, sample(31:200, 1)), 1)
  items <- sample(c(1, 1, 1, 2, 3, 5), 1)
  d <- rowMeans(matrix(sample(1:7, n * items, TRUE), n))
  dx <- sample(0:3, 1)
  ds <- sample(0:3, 1)
  list(
    x = fmt(mean(d) + (runif(1) < .3) * sample(c(-1, 1), 1) * 10^-dx, dx),
    sd = fmt(max(sd(d) + sample(-4:4, 1) * 10^-ds, 0), ds),
    n = n,
    items = items
  )
})

# Returns the first case where the app disagrees with brute force, or NULL.
grimmer_mismatch <- function(cases) {
  for (k in cases) {
    r <- evaluate(k$x, k$sd, as.character(k$n), k$items)
    expected <- if (!grim_hit(units(k$x), k$n * k$items, 10^decimals(k$x))) {
      "Mean fails GRIM"
    } else if (!grimmer_hit(k$x, k$sd, k$n, k$items)) {
      "SD fails GRIMMER"
    } else {
      character(0)
    }
    if (
      !identical(r$tests_run, c("GRIM", "GRIMMER")) ||
        !identical(sub(" \\(test [123]\\)$", "", r$reasons), expected)
    ) {
      return(c(k, list(app = r$reasons, expected = expected)))
    }
  }
  NULL
}

bad <- grimmer_mismatch(grimmer_cases)
if (!is.null(bad)) {
  stop("GRIMMER disagrees with brute force: ", deparse(bad))
}

real_safe_grimmer <- safe_grimmer
# Lets sub-test k of GRIMMER pass whenever it fails.
skip_grimmer_test <- function(k) {
  function(...) {
    r <- real_safe_grimmer(...)
    if (isFALSE(r$ok) && grepl(paste("test", k), r$reason, fixed = TRUE)) {
      return(list(ok = TRUE, reason = ""))
    }
    r
  }
}
expect_caught(
  "safe_grimmer",
  list(
    off = function(...) list(ok = TRUE, reason = ""),
    # No test 2 mutant: scrutiny 1.0.0 derives the candidate sums of squares
    # from the SD's rounding bounds, so their SDs always round back to it and
    # test 2 never fails (none in 20,000 random cases).
    test_1_skipped = skip_grimmer_test(1),
    test_3_skipped = skip_grimmer_test(3),
    items_ignored = function(x_str, sd_str, n_str, items) {
      real_safe_grimmer(x_str, sd_str, n_str, 1)
    },
    sd_one_decimal_coarser = mutate(
      safe_grimmer,
      "ds <- count_decimal_places(sd_str)",
      "ds <- max(0L, count_decimal_places(sd_str) - 1L)"
    )
  ),
  function() grimmer_mismatch(grimmer_cases)
)
rm(real_safe_grimmer)

section("No false positives on every small data set (n = 2-7)")

# Every data set of n participants on a small scale, for the small n where
# GRIM and GRIMMER are most powerful, rounded every way: none may be flagged.
# A participant's score is the mean of their `items` whole-number answers.
for (cfg in list(
  list(scale = 1:5, items = 1, n = 2:7),
  list(scale = 0:3, items = 1, n = 2:7),
  list(scale = -2:2, items = 1, n = 2:7),
  list(scale = 1:5, items = 2, n = 2:4)
)) {
  lo <- min(cfg$scale)
  hi <- max(cfg$scale)
  values <- (lo * cfg$items):(hi * cfg$items) / cfg$items
  m <- length(values)
  for (n in cfg$n) {
    # Columns are all nondecreasing index vectors, i.e. all multisets.
    idx <- combn(m + n - 1, n) - 0:(n - 1)
    for (j in seq_len(ncol(idx))) {
      data <- values[idx[, j]]
      for (d in 1:3) {
        for (up in c(TRUE, FALSE)) {
          x <- fmt(mean(data), d, up)
          sd_str <- fmt(sd(data), d, !up)
          r <- evaluate(
            x,
            sd_str,
            as.character(n),
            cfg$items,
            min = as.character(lo),
            max = as.character(hi)
          )
          if (!isTRUE(r$ok)) {
            stop(
              "False positive: ",
              paste(c(r$reasons, r$err), collapse = "; "),
              " for M = ",
              x,
              ", SD = ",
              sd_str,
              ", items = ",
              cfg$items,
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
# The default type runs no GRIM, even on an impossible mean, and every Type
# menu starts on it.
stopifnot(
  isTRUE(is.na(evaluate_row("5.21", "", "30", 1, "Any mean", "", "")$ok)),
  lengths(gregexpr(
    "<option value=\"Any mean\" selected>",
    as.character(ui),
    fixed = TRUE
  )) ==
    MAX_ROWS + 2 * MAX_PAIRS
)

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

section("Bounds against brute force (5,000 cases) and 5 mutants")

# The mean is possible if its rounding interval meets [Min, Max]. The SD is
# possible if the smallest SD that rounds to it is at most the Bhatia–Davis
# maximum sqrt((Max - m) * (m - Min) * n / (n - 1)) for some possible mean m,
# here searched on a grid that includes the interval's ends. An impossible
# mean also makes every SD impossible. One bound alone limits only the mean.
bounds_expected <- function(x, sd, n, mn, mx) {
  h <- 0.5 * 10^-decimals(x)
  lo <- max(as.numeric(x) - h, mn)
  hi <- min(as.numeric(x) + h, mx)
  mean_out <- lo > hi + 1e-9
  sd_out <- if (is.finite(mn) && is.finite(mx)) {
    m <- if (mean_out) mn else seq(lo, hi, length.out = 1001)
    sd_min <- max(as.numeric(sd) - 0.5 * 10^-decimals(sd), 0)
    mean_out || sd_min > sqrt(max((mx - m) * (m - mn)) * n / (n - 1)) + 1e-9
  } else {
    FALSE
  }
  c(
    if (mean_out) "Mean out of bounds",
    if (sd_out) "SD exceeds Bhatia–Davis bound"
  )
}

set.seed(13)
bounds_cases <- lapply(1:5000, function(i) {
  mn <- sample(c(0, 1, -3), 1)
  mx <- mn + sample(0:10, 1)
  list(
    x = fmt(runif(1, mn - .3, mx + .3), sample(0:2, 1)),
    sd = fmt(runif(1, 0, (mx - mn) * .8 + .3), sample(0:2, 1)),
    n = sample(2:100, 1),
    # Sometimes only one bound
    mn = if (runif(1) < .1) -Inf else mn,
    mx = if (runif(1) < .1) Inf else mx
  )
})

# Returns the first case where the app disagrees with brute force, or NULL.
bounds_mismatch <- function(cases) {
  for (k in cases) {
    bound <- function(b) if (is.finite(b)) as.character(b) else ""
    r <- evaluate(
      k$x,
      k$sd,
      as.character(k$n),
      min = bound(k$mn),
      max = bound(k$mx),
      integer = FALSE
    )
    expected <- bounds_expected(k$x, k$sd, k$n, k$mn, k$mx)
    if (!identical(r$reasons, as.character(expected))) {
      return(c(k, list(app = r$reasons, expected = expected)))
    }
  }
  NULL
}

bad <- bounds_mismatch(bounds_cases)
if (!is.null(bad)) {
  stop("Bounds disagree with brute force: ", deparse(bad))
}

expect_caught(
  "safe_bounds",
  list(
    sd_bound_at_midpoint = mutate(
      safe_bounds,
      "m <- min(max((mn + mx)/2, lo), hi)",
      "m <- (mn + mx)/2"
    ),
    sd_bound_mean_window_doubled = mutate(
      safe_bounds,
      "m <- min(max((mn + mx)/2, lo), hi)",
      "m <- min(max((mn + mx)/2, x - 2 * half), x + 2 * half)"
    ),
    sd_reason_without_mean_out = mutate(
      safe_bounds,
      "if (mean_out || sd - tol",
      "if (sd - tol"
    ),
    sd_tolerance_doubled = mutate(
      safe_bounds,
      "tol <- 0.5 * 10^",
      "tol <- 1 * 10^"
    ),
    mean_tolerance_doubled = mutate(
      safe_bounds,
      "half <- 0.5 * 10^",
      "half <- 1 * 10^"
    )
  ),
  function() bounds_mismatch(bounds_cases)
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
  is.null(validation_error("-.5", "", "30")),
  identical(validation_error("5.2", "abc", "20"), "SD must be a number"),
  identical(validation_error("5.2", "", "1"), "N must be at least 2"),
  identical(
    validation_error("45.5", "", "22", items = 2, type = "Percentage"),
    "Percentages cannot have multiple items."
  ),
  is.null(validation_error("5.25", "", "22", items = 2)),
  identical(
    validation_error("5.2", "", "20", items = 1.5),
    "Items must be a positive whole number"
  ),
  identical(
    validation_error("5.2", "", "20", items = 0),
    "Items must be a positive whole number"
  ),
  identical(
    validation_error("45.5", "2.1", "22", type = "Percentage", max = "100"),
    "For percentages with SD, Min and Max are required (typically 0 and 100)"
  ),
  identical(
    validation_error("5.2", "", "20", min = "abc"),
    "Min must be a number"
  ),
  identical(
    validation_error("5.2", "", "20", max = "1e1"),
    "Max must be a number"
  )
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
# p = .085 is reachable from 4.2 but not from 4.1, and one neighbour is enough.
stopifnot(isTRUE(
  run_t_test("5.2", "2.1", "30", "4.15", "1.90", "30", p = "0.085")$inbounds
))

# Student's and Welch's ranges can be disjoint. A p in the gap is flagged, so
# the displayed ranges must not contain it, although [min_p, max_p] does.
gap_t_test <- function(p) {
  run_t_test("0.92", "0.4", "6", "1.864", "3", "117", p = p)
}
r <- gap_t_test("0.100")
stopifnot(
  isFALSE(r$inbounds),
  r$min_p < 0.1 && 0.1 < r$max_p,
  nrow(r$ranges) == 2,
  !any(r$ranges$p_min <= 0.1 & 0.1 <= r$ranges$p_max),
  identical(range(unlist(r$ranges)), c(r$min_p, r$max_p)),
  p_ranges_text(r$ranges, 3) == "p ∈ [<0.001, 0.019] or [0.356, 0.518]",
  grepl("or [0.356", format(t_test_result_ui(r)), fixed = TRUE),
  isTRUE(gap_t_test("0.400")$inbounds), # in Student's range
  nrow(run_t_test("5.23", "2.1", "30", "4.10", "1.9", "30")$ranges) == 1
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
  run_t_test("", "", "", "", "", "")$status == "blank",
  run_t_test("5.23", "2.1", "1", "4.10", "1.9", "30")$msg ==
    "N must be ≥ 2 in both groups",
  run_t_test("5.23", "2.1", "30", "4.10", "1.9", "10000000000")$msg ==
    "N is too large",
  run_t_test("5.23", "2.1", "30", "4.10", "1.9", "30", p = "abc")$msg ==
    "Reported p must be a number",
  # A reported p is checked before the groups are complete.
  run_t_test("5.23", "", "", "", "", "", p = "-0.1")$msg ==
    "Reported p must be between 0 and 1"
)

# An SD of 0 at the working precision (here SD1 "0.4" coarsened to SD2's 0
# decimals) still has a rounding interval reaching down to 0. This p is the
# Welch result for genuine data with these summary statistics.
r <- run_t_test("0.92", "0.4", "6", "1.864", "3", "117", p = "0.003")
stopifnot(isTRUE(r$inbounds), r$min_p < 0.001)
stopifnot(isTRUE(
  run_t_test(
    "5.2",
    "0.0",
    "6",
    "4.1",
    "0.6",
    "40",
    p = "0.001",
    op = "less_than"
  )$inbounds
))

section("t-test p values from 500 simulated data sets")

# p values recalculated from genuine data, by Student's or Welch's test,
# must be reproduced from the rounded summary statistics: whole-number, normal
# and skewed data, samples from n = 2, 0 to 3 decimals (sometimes differing
# between the groups), and p reported exactly or as an inequality.
set.seed(3)
for (i in 1:500) {
  n1 <- sample(c(2:10, 5:300), 1)
  n2 <- sample(c(2:10, 5:300), 1)
  kind <- sample(3, 1)
  g1 <- switch(kind, sample(1:7, n1, TRUE), rnorm(n1, 50, 10), rlnorm(n1))
  g2 <- switch(
    kind,
    sample(1:7, n2, TRUE, prob = c(1, 1, 2, 3, 3, 2, 1)),
    rnorm(n2, 50 + rnorm(1, 0, 4), 10 * runif(1, .3, 3)),
    rlnorm(n2, runif(1, 0, .5))
  )
  if (sd(g1) == 0 && sd(g2) == 0) {
    next # t is undefined
  }
  p <- t.test(g1, g2, var.equal = runif(1) < .5)$p.value
  dm <- sample(0:3, 2, TRUE)
  ds <- sample(0:3, 2, TRUE)
  if (runif(1) < .7) {
    dm[2] <- dm[1]
    ds[2] <- ds[1]
  }
  if (runif(1) < .7) {
    p_str <- fmt(p, sample(1:4, 1))
    op <- "equals"
  } else {
    threshold <- sample(c(0.05, 0.01, 0.001), 1)
    p_str <- as.character(threshold)
    op <- if (p < threshold) {
      "less_than"
    } else {
      sample(c("greater_than", "greater_than_or_equal_to"), 1)
    }
  }
  args <- list(
    fmt(mean(g1), dm[1]),
    fmt(sd(g1), ds[1]),
    as.character(n1),
    fmt(mean(g2), dm[2]),
    fmt(sd(g2), ds[2]),
    as.character(n2),
    p = p_str,
    op = op
  )
  if (!isTRUE(do.call(run_t_test, args)$inbounds)) {
    stop("Genuine p (", p, ") not reproduced: ", deparse(args))
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

section("t-test ranges against brute force (120 cases) and 3 mutants")

# Any true means and SDs within the rounding intervals are possible. Student's
# and Welch's p over a grid of them, corners included (where p is at its
# extremes, up to a relative 1e-5 for Welch), must match the app's range, and a
# reported p must count as reproduced exactly when its own rounding interval
# reaches one of the two methods' ranges. When the groups differ in decimal
# places, the finer value is first widened as the app does: to every value
# that rounds, either way, to the coarser precision.
interval_at <- function(v, d) {
  f <- 10^(decimals(v) - d)
  k <- floor(units(v) / f) + 0:1
  k <- k[2 * abs(units(v) - k * f) <= f]
  (range(k) + c(-0.5, 0.5)) / 10^d
}

t_p <- function(m1, s1, n1, m2, s2, n2, welch) {
  v1 <- s1^2 / n1
  v2 <- s2^2 / n2
  if (welch) {
    se <- sqrt(v1 + v2)
    df <- (v1 + v2)^2 / (v1^2 / (n1 - 1) + v2^2 / (n2 - 1))
  } else {
    se <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)) *
      sqrt(1 / n1 + 1 / n2)
    df <- n1 + n2 - 2
  }
  2 * pt(-abs(m1 - m2) / se, df)
}

# Columns: Student's and Welch's range; rows: lowest and highest p.
t_ranges <- function(k) {
  dm <- min(decimals(k$m1), decimals(k$m2))
  ds <- min(decimals(k$s1), decimals(k$s2))
  axis <- function(iv) seq(iv[1], iv[2], length.out = 5)
  # SD 0 itself leaves t undefined; the app also stands in 1e-8.
  sd_axis <- function(v) axis(pmax(interval_at(v, ds), 1e-8))
  g <- expand.grid(
    m1 = axis(interval_at(k$m1, dm)),
    s1 = sd_axis(k$s1),
    m2 = axis(interval_at(k$m2, dm)),
    s2 = sd_axis(k$s2)
  )
  i1 <- interval_at(k$m1, dm)
  i2 <- interval_at(k$m2, dm)
  # Overlapping mean intervals allow a difference of 0, so p = 1.
  overlap <- i1[2] >= i2[1] && i2[2] >= i1[1]
  sapply(c(student = FALSE, welch = TRUE), function(welch) {
    p <- t_p(g$m1, g$s1, k$n1, g$m2, g$s2, k$n2, welch)
    c(min(p), if (overlap) 1 else max(p))
  })
}

set.seed(9)
t_cases <- lapply(1:120, function(i) {
  dm <- sample(0:3, 1)
  ds <- sample(0:3, 1)
  # Group 2 sometimes has one more decimal place, often ending in an exact
  # half of group 1's last one.
  finer <- function(v, d) {
    if (runif(1) < .6) {
      return(fmt(v, d))
    }
    if (runif(1) < .5) {
      fmt(v, d + 1)
    } else {
      paste0(fmt(v, d), if (d) "5" else ".5")
    }
  }
  k <- list(
    m1 = fmt(rnorm(1, 10, 3), dm),
    s1 = fmt(runif(1, .05, 4), ds),
    n1 = sample(c(2:10, 5:150), 1),
    m2 = finer(rnorm(1, 10, 3), dm),
    s2 = finer(runif(1, .05, 4), ds),
    n2 = sample(c(2:10, 5:150), 1),
    op = sample(c("equals", "equals", "less_than", "greater_than"), 1)
  )
  # A reported p around the range, or for an inequality, around the end that
  # decides it, so that both verdicts are common.
  r <- range(t_ranges(k))
  r <- switch(
    k$op,
    equals = r,
    less_than = r[c(1, 1)],
    greater_than = r[c(2, 2)]
  )
  k$p <- fmt(runif(1, max(0, r[1] - .02), min(1, r[2] + .02)), sample(1:4, 1))
  k
})

# Returns the first case where the app disagrees with brute force, or NULL.
t_mismatch <- function(cases) {
  for (k in cases) {
    r <- run_t_test(
      k$m1,
      k$s1,
      as.character(k$n1),
      k$m2,
      k$s2,
      as.character(k$n2),
      p = k$p,
      op = k$op
    )
    rg <- t_ranges(k)
    near <- function(a, b) abs(a - b) <= 1e-4 * b + 1e-12
    p <- as.numeric(k$p)
    h <- 0.5 * 10^-max(1, decimals(k$p))
    reaches <- switch(
      k$op,
      equals = rg[1, ] <= p + h & rg[2, ] >= p - h,
      less_than = rg[1, ] < p + h,
      greater_than = rg[2, ] > p - h
    )
    # Skip the verdict where a range ends too close to call on the grid.
    edge <- any(abs(outer(c(rg), c(p - h, p + h), "-")) <= 1e-4 * c(rg))
    if (
      r$status != "ok" ||
        !near(r$min_p, min(rg[1, ])) ||
        !near(r$max_p, max(rg[2, ])) ||
        (!edge && !identical(r$inbounds, any(reaches)))
    ) {
      return(c(k, list(app = r[c("min_p", "max_p", "inbounds")], oracle = rg)))
    }
  }
  NULL
}

bad <- t_mismatch(t_cases)
if (!is.null(bad)) {
  stop("t-test disagrees with brute force: ", deparse(bad))
}

expect_caught(
  "evaluate_pair_t_test",
  list(
    sd_one_decimal_coarser = mutate(
      evaluate_pair_t_test,
      "sd_digits <- min(count_decimal_places(sd1s), count_decimal_places(sd2s))",
      "sd_digits <- max(0L, min(count_decimal_places(sd1s), count_decimal_places(sd2s)) - 1L)"
    ),
    max_p_from_first_combination = mutate(
      evaluate_pair_t_test,
      "max_p <- max(vapply(reps, function(r) r$max_p, numeric(1)))",
      "max_p <- reps[[1]]$max_p"
    ),
    zero_sd_skipped = mutate(
      evaluate_pair_t_test,
      "nonzero <- function(sd) pmax(sd, 1e-08)",
      "nonzero <- function(sd) sd"
    )
  ),
  function() t_mismatch(t_cases)
)

section("Server: rows, badges, summaries, percentage type, CSV download")

# Server: row order, badges, summaries, type/bounds sync, CSV ------------------

# Whether a rendered output contains every given string.
shows <- function(out, ...) {
  all(vapply(c(...), function(s) grepl(s, out$html, fixed = TRUE), NA))
}
# testServer() cannot see the browser apply updateTextInput(), so record it.
updated <- list()
updateTextInput <- function(session, inputId, label = NULL, value = NULL, ...) {
  updated[[inputId]] <<- value
}

shiny::testServer(app, {
  session$setInputs(gb_rm_2 = 1) # ignoreInit swallows the first value
  stopifnot(identical(gb_slots(), 1:3))
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
    csv$label == "1",
    csv$test == "GRIM",
    csv$consistent == "TRUE",
    grepl("Uninformative GRIM", csv$notes),
    shows(
      output$gb_badge_1,
      "bg-success",
      "Uninformative GRIM",
      "1 decimal place"
    ),
    !shows(output$gb_badge_1, "bg-danger"),
    shows(output$gb_summary, "1 case tested", "1 consistent", "0 inconsistent")
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
    csv$inconsistency == "Percentage fails GRIM",
    shows(output$gb_badge_1, "bg-danger", "Percentage fails GRIM"),
    !shows(output$gb_badge_1, "bg-success", "Uninformative"),
    shows(output$gb_summary, "1 case tested", "0 consistent", "1 inconsistent")
  )
  session$setInputs(gb_x_1 = "45.5")
  csv <- read.csv(output$gb_download, colClasses = "character")
  stopifnot(csv$consistent == "TRUE", csv$test == "GRIM+Bounds")
  # A percentage works on the proportion: 1 decimal place becomes 3.
  session$setInputs(gb_n_1 = "2000")
  stopifnot(shows(output$gb_badge_1, "Uninformative GRIM", "3 decimal places"))

  # Two rows: counted separately, numbered in the CSV, errors not counted.
  session$setInputs(
    gb_type_2 = "Whole-number mean",
    gb_x_2 = "5.21",
    gb_n_2 = "30",
    gb_items_2 = 1,
    gb_x_3 = "abc"
  )
  csv <- read.csv(output$gb_download, colClasses = "character")
  stopifnot(
    identical(csv$label, c("1", "2", "3")),
    identical(csv$consistent, c("TRUE", "FALSE", NA)),
    csv$inconsistency[3] == "Mean must be a number",
    shows(
      output$gb_summary,
      "2 cases tested",
      "1 consistent",
      "1 inconsistent"
    ),
    shows(output$gb_badge_3, "Mean must be a number")
  )
  session$setInputs(gb_x_3 = "")

  # Percentage fills in only the empty bounds, and switching away removes them
  # again unless the user changed them.
  session$setInputs(gb_type_3 = "Any mean", gb_min_3 = "", gb_max_3 = "50")
  updated <<- list()
  session$setInputs(gb_type_3 = "Percentage")
  stopifnot(identical(updated, list(gb_min_3 = "0")))
  session$setInputs(gb_min_3 = "0", gb_type_3 = "Whole-number mean")
  stopifnot(identical(updated, list(gb_min_3 = "")))
  session$setInputs(gb_min_3 = "", gb_type_3 = "Percentage")
  session$setInputs(gb_min_3 = "5", gb_type_3 = "Any mean")
  stopifnot(identical(updated, list(gb_min_3 = "0")))

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
    csv$p_note == "Reported p must be between 0 and 1",
    shows(output$cb_t_test_1, "Reported p must be between 0 and 1")
  )

  # Disjoint Student's and Welch's ranges are spelled out in the CSV.
  session$setInputs(
    cb_x_1a = "0.92",
    cb_sd_1a = "0.4",
    cb_n_1a = "6",
    cb_x_1b = "1.864",
    cb_sd_1b = "3",
    cb_n_1b = "117",
    cb_p_1 = "0.100"
  )
  csv <- read.csv(output$download_csv, colClasses = "character")
  stopifnot(
    csv$p_reproduces[1] == "FALSE",
    csv$p_note[1] ==
      paste0(
        MIXED_DIGITS_NOTE,
        "; Student's and Welch's t-tests give separate ranges: ",
        "p ∈ [<0.001, 0.019] or [0.356, 0.518]"
      ),
    shows(output$cb_t_test_1, "bg-danger", "or [0.356", MIXED_DIGITS_NOTE),
    !shows(output$cb_t_test_1, "bg-success"),
    shows(
      output$t_test_summary,
      "1 reported p checked",
      "0 consistent",
      "1 inconsistent"
    )
  )
  session$setInputs(cb_p_1 = "0.400")
  stopifnot(
    read.csv(output$download_csv, colClasses = "character")$p_reproduces[1] ==
      "TRUE",
    shows(output$cb_t_test_1, "bg-success"),
    !shows(output$cb_t_test_1, "bg-danger"),
    shows(output$t_test_summary, "1 consistent", "0 inconsistent")
  )
  session$setInputs(cb_p_1 = "")
  stopifnot(
    shows(output$cb_t_test_1, "Recalculated", "or [0.356"),
    is.null(output$t_test_summary$html) ||
      !shows(output$t_test_summary, "checked")
  )
})
rm(updateTextInput, updated)

section()
cat("All checks passed.\n")
