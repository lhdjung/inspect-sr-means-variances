library(shiny)
library(bslib)
library(scrutiny)
library(recalc)

# # Deploy like this:
# rsconnect::deployApp(
#   appName = "inspect-sr-means-variances",
#   account = "errors"
# )

addResourcePath("images", "images")

MAX_PAIRS <- 15
MAX_ROWS <- 15


# Helpers -----------------------------------------------------------------

parse_number <- function(s) {
  if (is.null(s)) {
    return(NA_real_)
  }
  s <- trimws(s)
  # Plain decimal notation only. as.numeric() alone would also accept "Inf",
  # hex ("0x10") and exponents, none of which is a reported summary statistic
  # with countable decimal places. Commas are rejected too; see comma_msg().
  if (!grepl("^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)$", s)) {
    return(NA_real_)
  }
  as.numeric(s)
}

# A comma is a thousands separator in some locales ("1,234" = 1234) and a
# decimal separator in others (= 1.234), so it is rejected rather than guessed.
has_comma <- function(s) !is.null(s) && grepl(",", s, fixed = TRUE)

comma_msg <- function(field) {
  paste0(
    field,
    " contains a comma, which is ambiguous: use a decimal point and no",
    " thousands separators (e.g., 1234.5)"
  )
}

# N is checked on the string, digits only: a separator is ambiguous across
# locales ("2,000" and "2.000" each mean 2 in some and 2000 in others).
is_plain_whole_number <- function(s) grepl("^[0-9]+$", trimws(s))

N_FORMAT_MSG <- "N must be a whole number without separators (e.g., 2000)"

count_decimal_places <- decimal_places_scalar

safe_grim <- function(x_str, n_str, items, percent = FALSE) {
  x <- parse_number(x_str)
  n <- suppressWarnings(as.integer(parse_number(n_str)))
  # grim() adds 2 to digits_x internally when percent = TRUE, so we must not add
  # it here as well: doing so would give percentages dp + 4 effective decimal
  # places, making the test wrongly strict and disagreeing with
  # grim_uninformative() (which relies on grim_probability()'s internal +2).
  dx <- count_decimal_places(x_str)
  it <- suppressWarnings(as.integer(items))
  if (anyNA(c(x, n, it)) || n < 2 || it < 1) {
    return(NA)
  }
  # No tryCatch here or in the other test wrappers: an error means the test
  # could not be run, which evaluate_row() reports instead of silently
  # treating the row as untested.
  grim(x = x, n = n, digits_x = dx, items = it, percent = percent)
}

grim_uninformative <- function(x_str, n_str, items, percent = FALSE) {
  x <- parse_number(x_str)
  n <- suppressWarnings(as.integer(parse_number(n_str)))
  it <- suppressWarnings(as.integer(items))
  if (anyNA(c(x, n, it)) || n < 2 || it < 1) {
    return(FALSE)
  }
  p <- grim_probability(
    x = x,
    n = n,
    digits_x = count_decimal_places(x_str),
    items = it,
    percent = percent
  )
  isTRUE(p == 0)
}

safe_grimmer <- function(x_str, sd_str, n_str, items) {
  x <- parse_number(x_str)
  sd <- parse_number(sd_str)
  n <- suppressWarnings(as.integer(parse_number(n_str)))
  dx <- count_decimal_places(x_str)
  ds <- count_decimal_places(sd_str)
  it <- suppressWarnings(as.integer(items))
  if (anyNA(c(x, sd, n, it)) || n < 2 || it < 1) {
    return(list(ok = NA, reason = ""))
  }
  # grimmer() itself returns a bare logical; only grimmer_map() can say which
  # sub-test failed.
  r <- grimmer_map(
    tibble::tibble(x = x, sd = sd, n = n),
    digits_x = dx,
    digits_sd = ds,
    items = it,
    show_reason = TRUE
  )
  list(ok = as.logical(r$consistency), reason = r$reason)
}

safe_bounds <- function(x_str, sd_str, n_str, min_str, max_str) {
  min_given <- !is.null(min_str) && nzchar(trimws(min_str))
  max_given <- !is.null(max_str) && nzchar(trimws(max_str))
  # A single bound still limits the mean from one side; the missing one is
  # infinite. The SD bound below needs both.
  x <- parse_number(x_str)
  mn <- if (min_given) parse_number(min_str) else -Inf
  mx <- if (max_given) parse_number(max_str) else Inf
  reasons <- character(0)
  if (anyNA(c(x, mn, mx))) {
    return(reasons)
  }
  # The reported mean and SD are rounded, so the true values may lie anywhere
  # within half a unit of the last reported decimal place. Only flag what no
  # value in those rounding intervals could satisfy. `eps` absorbs
  # floating-point error at the interval edges.
  eps <- sqrt(.Machine$double.eps)
  half <- 0.5 * 10^(-count_decimal_places(x_str))
  lo <- max(x - half, mn)
  hi <- min(x + half, mx)
  mean_out <- lo > hi + eps
  if (mean_out) {
    reasons <- c(reasons, "Mean out of bounds")
  }
  sd_given <- !is.null(sd_str) && nzchar(trimws(sd_str))
  if (sd_given) {
    sd <- parse_number(sd_str)
    n <- suppressWarnings(as.integer(parse_number(n_str)))
    if (!anyNA(c(sd, n)) && n >= 2 && min_given && max_given && mx >= mn) {
      # Bhatia–Davis upper bound on the variance for data confined to
      # [min, max] with mean m: (max - m) * (m - min), times n / (n - 1) for
      # the sample variance. It is largest at the midpoint of the scale, so
      # take the possible true mean closest to the midpoint. When no possible
      # mean lies inside [min, max], no in-range data exist at all, so no SD
      # is valid and both reasons are reported.
      m <- min(max((mn + mx) / 2, lo), hi)
      var_max <- max((mx - m) * (m - mn), 0) * n / (n - 1)
      tol <- 0.5 * 10^(-count_decimal_places(sd_str))
      if (mean_out || sd - tol > sqrt(var_max) + eps) {
        reasons <- c(reasons, "SD exceeds Bhatia–Davis bound")
      }
    }
  }
  reasons
}

friendly_reason <- function(reason) {
  if (reason == "GRIM inconsistent") {
    return("Mean fails GRIM")
  }
  if (grepl("GRIMMER inconsistent", reason)) {
    # grimmer() reports which of its 3 sub-tests failed, e.g.
    # "GRIMMER inconsistent (test 3)". Keep that number so the badge matches
    # the Guidance promise that the message says which test failed.
    m <- regmatches(reason, regexpr("\\d+", reason))
    if (length(m) > 0 && nzchar(m)) {
      return(paste0("SD fails GRIMMER (test ", m, ")"))
    }
    return("SD fails GRIMMER")
  }
  reason
}

format_p_value <- function(p, digits = 3) {
  if (is.null(p) || length(p) == 0 || is.na(p)) {
    return("NA")
  }
  digits <- max(1L, as.integer(digits))
  floor_val <- 10^(-digits)
  r <- round(p, digits)
  # Only values that would otherwise print as a misleading 0 or 1 get an
  # inequality sign.
  if (r < floor_val / 2) {
    return(paste0("<", formatC(floor_val, format = "f", digits = digits)))
  }
  if (r > 1 - floor_val / 2 && p < 1) {
    return(paste0(">", formatC(1 - floor_val, format = "f", digits = digits)))
  }
  formatC(r, format = "f", digits = digits)
}

# Display symbol for a recalc p_operator value.
operator_symbol <- function(op) {
  if (is.null(op) || !nzchar(op)) {
    return("=")
  }
  switch(
    op,
    equals = "=",
    less_than = "<",
    greater_than = ">",
    less_than_or_equal_to = "<=",
    greater_than_or_equal_to = ">=",
    "="
  )
}


# Validation --------------------------------------------------------------

validate_combined_row <- function(
  x_str,
  sd_str,
  n_str,
  items,
  type,
  min_str = NULL,
  max_str = NULL
) {
  fields <- list(Mean = x_str, SD = sd_str, Min = min_str, Max = max_str)
  for (field in names(fields)) {
    if (has_comma(fields[[field]])) return(comma_msg(field))
  }
  if (!is.null(x_str) && nzchar(trimws(x_str))) {
    x_num <- parse_number(x_str)
    if (is.na(x_num)) {
      return("Mean must be a number")
    }
    if (isTRUE(type == "Percentage") && (x_num < 0 || x_num > 100)) {
      return("Percentage must be between 0 and 100")
    }
  }
  sd_given <- !is.null(sd_str) && nzchar(trimws(sd_str))
  min_given <- !is.null(min_str) && nzchar(trimws(min_str))
  max_given <- !is.null(max_str) && nzchar(trimws(max_str))
  if (sd_given && isTRUE(type == "Percentage") && !(min_given && max_given)) {
    return(
      "For percentages with SD, Min and Max are required (typically 0 and 100)"
    )
  }
  if (sd_given) {
    sd_num <- parse_number(sd_str)
    if (is.na(sd_num)) {
      return("SD must be a number")
    }
    if (sd_num < 0) return("SD cannot be negative")
  }
  if (!is.null(n_str) && nzchar(trimws(n_str))) {
    if (!is_plain_whole_number(n_str)) {
      return(N_FORMAT_MSG)
    }
    n_num <- parse_number(n_str)
    if (n_num < 2) {
      return("N must be at least 2")
    }
    if (n_num > .Machine$integer.max) return("N is too large")
  }
  if (
    is.null(items) ||
      is.na(items) ||
      items != round(items) ||
      items < 1 ||
      items > .Machine$integer.max
  ) {
    return("Items must be a positive whole number")
  }
  if (min_given && is.na(parse_number(min_str))) {
    return("Min must be a number")
  }
  if (max_given && is.na(parse_number(max_str))) {
    return("Max must be a number")
  }
  if (min_given && max_given) {
    mn <- parse_number(min_str)
    mx <- parse_number(max_str)
    # Max == Min is allowed: with empirical bounds, it means all values are
    # identical.
    if (!is.na(mn) && !is.na(mx) && mx < mn) {
      return("Max cannot be less than Min")
    }
  }
  NULL
}


# Combined evaluator ------------------------------------------------------

# Returns: list(ok, reasons, tests_run, notes, err, uninformative, grim_digits)
# - ok: TRUE / FALSE / NA (NA = nothing testable)
# - reasons: character vector of failure reasons (friendly form)
# - tests_run: character vector e.g. c("GRIM", "Bounds")
# - err: validation or internal error string (or NULL); when set, ok = NA
# - uninformative: TRUE if GRIM ran but could not have failed
# - grim_digits: decimal places GRIM worked with (for the uninformative label)
evaluate_row <- function(...) {
  # A test that throws could not be run (e.g. after a breaking change in
  # scrutiny). Report that as an error instead of quietly showing a verdict
  # based on the remaining tests.
  tryCatch(
    evaluate_row_unsafe(...),
    error = function(e) {
      msg <- paste(
        "Internal error:",
        sub("\n.*", "", conditionMessage(e))
      )
      list(
        ok = NA,
        reasons = msg,
        tests_run = character(0),
        notes = character(0),
        err = msg
      )
    }
  )
}

evaluate_row_unsafe <- function(
  x_str,
  sd_str,
  n_str,
  items,
  type,
  min_str,
  max_str,
  integer = TRUE
) {
  if (is.null(x_str) || !nzchar(trimws(x_str))) {
    # Min and Max are left out: Percentage auto-fills them.
    started <- any(vapply(
      list(sd_str, n_str),
      function(s) !is.null(s) && nzchar(trimws(s)),
      logical(1)
    ))
    return(list(
      ok = NA,
      reasons = character(0),
      tests_run = character(0),
      notes = if (started) "Awaiting mean" else character(0),
      err = NULL
    ))
  }
  err <- validate_combined_row(
    x_str,
    sd_str,
    n_str,
    items,
    type,
    min_str,
    max_str
  )
  if (!is.null(err)) {
    return(list(
      ok = NA,
      reasons = err,
      tests_run = character(0),
      notes = character(0),
      err = err
    ))
  }

  sd_given <- !is.null(sd_str) && nzchar(trimws(sd_str))
  is_percent <- isTRUE(type == "Percentage")
  min_given <- !is.null(min_str) && nzchar(trimws(min_str))
  max_given <- !is.null(max_str) && nzchar(trimws(max_str))
  bounds_active <- min_given || max_given
  both_bounds <- min_given && max_given

  reasons <- character(0)
  tests_run <- character(0)
  notes <- character(0)
  uninformative <- FALSE

  # GRIM and GRIMMER are only valid for integer data. When the data are not
  # flagged as integer, skip them and say so; the Bounds checks (mean within
  # [min, max] and the Bhatia–Davis SD bound) still apply to continuous data.
  if (isTRUE(integer)) {
    grim_label <- if (is_percent) "Percentage fails GRIM" else "Mean fails GRIM"
    grim_ok <- safe_grim(x_str, n_str, items, percent = is_percent)
    if (!is.na(grim_ok)) {
      tests_run <- c(tests_run, "GRIM")
      if (!grim_ok) {
        reasons <- c(reasons, grim_label)
      }
      uninformative <- grim_uninformative(
        x_str,
        n_str,
        items,
        percent = is_percent
      )
    }

    if (sd_given && !is_percent) {
      res <- safe_grimmer(x_str, sd_str, n_str, items)
      if (!is.na(res$ok)) {
        tests_run <- c(tests_run, "GRIMMER")
        if (!res$ok) {
          reasons <- c(reasons, friendly_reason(res$reason))
        }
      }
    }
  }

  if (bounds_active) {
    bounds_reasons <- safe_bounds(x_str, sd_str, n_str, min_str, max_str)
    tests_run <- c(tests_run, "Bounds")
    if (length(bounds_reasons) > 0) {
      reasons <- c(reasons, bounds_reasons)
    }
  }

  # Explain every test that was skipped. When Bounds still yields a badge, word
  # the note so it qualifies that verdict rather than seeming to contradict it:
  # a "Consistent" here covers only the checks named. Without N, the
  # Bhatia–Davis SD bound is skipped too, leaving just the mean's bounds.
  n_given <- !is.null(n_str) && nzchar(trimws(n_str))
  sd_bound_skipped <- both_bounds && sd_given && !n_given
  scope <- if (!both_bounds) {
    "Mean bound only"
  } else if (sd_bound_skipped) {
    "Mean bounds only"
  } else {
    "Bounds only"
  }
  if (!isTRUE(integer)) {
    notes <- c(
      notes,
      if (length(tests_run) == 0) {
        "GRIM/GRIMMER only apply to integer data"
      } else {
        paste0(scope, "; GRIM/GRIMMER need integer data")
      },
      if (sd_bound_skipped) "SD bound needs N"
    )
  } else if (!n_given) {
    notes <- c(
      notes,
      if (length(tests_run) == 0) {
        "Awaiting N for GRIM/GRIMMER"
      } else {
        paste0(
          scope,
          "; GRIM/GRIMMER",
          if (sd_bound_skipped) " and SD bound",
          " need N"
        )
      }
    )
  }

  if (bounds_active && !both_bounds) {
    notes <- c(
      notes,
      if (min_given) {
        "Only Min given; add Max for a more informative bounds check"
      } else {
        "Only Max given; add Min for a more informative bounds check"
      }
    )
  }

  if (length(tests_run) == 0) {
    return(list(
      ok = NA,
      reasons = character(0),
      tests_run = character(0),
      notes = notes,
      err = NULL
    ))
  }

  # A mean that fails GRIM makes GRIMMER fail for the same "GRIM inconsistent"
  # reason, which friendly_reason() maps back to "Mean fails GRIM". Drop the
  # duplicate so the badge doesn't read "Mean fails GRIM; Mean fails GRIM".
  reasons <- unique(reasons)

  list(
    ok = length(reasons) == 0,
    reasons = reasons,
    tests_run = tests_run,
    notes = notes,
    err = NULL,
    uninformative = uninformative,
    # GRIM works on the proportion for percentages, so its granularity is
    # dp + 2.
    grim_digits = count_decimal_places(x_str) + if (is_percent) 2L else 0L
  )
}


# t-test recalculation evaluator ------------------------------------------

# Recalculates the independent-samples t-test p-value from the two groups'
# summary statistics (M, SD, N) using recalc::recalc_independent_t_p(), and -
# when a reported p is supplied - reports whether that p is reproducible.
#
# Returns list(status, ...) where status is one of:
# - "blank":      nothing entered for this pair yet
# - "incomplete": some but not all of M/SD/N for both groups present
# - "error":      an explicit problem (with $msg)
# - "ok":         recalculated (with $min_p, $max_p, $p_given, $p_reported,
#                 $inbounds, $p_digits, $mixed_digits)
evaluate_pair_t_test <- function(
  m1s,
  sd1s,
  n1s,
  m2s,
  sd2s,
  n2s,
  p_str,
  p_operator = "equals"
) {
  if (is.null(p_operator) || !nzchar(p_operator)) {
    p_operator <- "equals"
  }
  # A reported p outside [0, 1] is invalid regardless of whether the group
  # statistics are complete, so flag it before the completeness checks below.
  if (!is.null(p_str) && nzchar(trimws(p_str))) {
    if (has_comma(p_str)) {
      return(list(status = "error", msg = comma_msg("Reported p")))
    }
    p_check <- parse_number(p_str)
    if (is.na(p_check)) {
      return(list(status = "error", msg = "Reported p must be a number"))
    }
    if (p_check < 0 || p_check > 1) {
      return(list(status = "error", msg = "Reported p must be between 0 and 1"))
    }
  }
  cells <- list(m1s, sd1s, n1s, m2s, sd2s, n2s)
  filled <- vapply(
    cells,
    function(s) !is.null(s) && nzchar(trimws(s)),
    logical(1)
  )
  if (!any(filled)) {
    return(list(status = "blank"))
  }
  if (!all(filled)) {
    return(list(status = "incomplete"))
  }

  m1 <- parse_number(m1s)
  m2 <- parse_number(m2s)
  sd1 <- parse_number(sd1s)
  sd2 <- parse_number(sd2s)
  n1 <- parse_number(n1s)
  n2 <- parse_number(n2s)

  if (has_comma(paste(m1s, sd1s, m2s, sd2s))) {
    return(list(status = "error", msg = comma_msg("Mean or SD")))
  }
  if (anyNA(c(m1, m2, sd1, sd2))) {
    return(list(status = "error", msg = "Mean and SD must be numbers"))
  }
  if (!is_plain_whole_number(n1s) || !is_plain_whole_number(n2s)) {
    return(list(status = "error", msg = N_FORMAT_MSG))
  }
  if (max(n1, n2) > .Machine$integer.max) {
    return(list(status = "error", msg = "N is too large"))
  }
  if (n1 < 2 || n2 < 2) {
    return(list(status = "error", msg = "N must be ≥ 2 in both groups"))
  }
  if (sd1 < 0 || sd2 < 0) {
    return(list(status = "error", msg = "SD cannot be negative"))
  }
  n1 <- as.integer(n1)
  n2 <- as.integer(n2)

  # Reported p was already validated to lie in [0, 1] above.
  p_given <- !is.null(p_str) && nzchar(trimws(p_str))
  p_num <- if (p_given) parse_number(p_str) else NULL

  # recalc requires a single decimal-place count for the means and one for the
  # SDs. Baseline tables almost always report both groups to the same
  # precision. If they differ, use the smaller count for both: pretending the
  # coarser value had more decimals would shrink its rounding interval and
  # could wrongly flag a correct p. The finer value is rounded to the coarser
  # precision, whose rounding interval contains its own, so the recalculated
  # range can only widen.
  m_digits <- min(count_decimal_places(m1s), count_decimal_places(m2s))
  sd_digits <- min(count_decimal_places(sd1s), count_decimal_places(sd2s))
  mixed_digits <- count_decimal_places(m1s) != count_decimal_places(m2s) ||
    count_decimal_places(sd1s) != count_decimal_places(sd2s)
  p_digits <- if (p_given) max(1L, count_decimal_places(p_str)) else 3L

  # A finer value exactly halfway between two coarser ones (4.15 at 1 decimal)
  # has a rounding interval straddling both, so every such combination is
  # recalculated and the results are pooled.
  grid <- expand.grid(
    m1 = coarsen(m1, m_digits),
    m2 = coarsen(m2, m_digits),
    sd1 = coarsen(sd1, sd_digits),
    sd2 = coarsen(sd2, sd_digits)
  )
  reps <- tryCatch(
    lapply(seq_len(nrow(grid)), function(i) {
      suppressWarnings(recalc::recalc_independent_t_p(
        m1 = grid$m1[i],
        m2 = grid$m2[i],
        sd1 = grid$sd1[i],
        sd2 = grid$sd2[i],
        n1 = n1,
        n2 = n2,
        m_digits = m_digits,
        sd_digits = sd_digits,
        rounding = "either",
        p = p_num,
        p_digits = p_digits,
        p_operator = p_operator,
        alternative = "two.sided",
        direction = "both"
      ))$reproduced
    }),
    error = function(e) {
      paste("Could not recalculate:", sub("\n.*", "", conditionMessage(e)))
    }
  )
  if (is.character(reps)) {
    return(list(status = "error", msg = reps))
  }
  min_p <- min(vapply(reps, function(r) r$min_p, numeric(1)))
  max_p <- max(vapply(reps, function(r) r$max_p, numeric(1)))
  if (anyNA(c(min_p, max_p))) {
    return(list(status = "error", msg = "Could not recalculate"))
  }

  list(
    status = "ok",
    p_given = p_given,
    p_reported = if (p_given) p_num else NA_real_,
    min_p = min_p,
    max_p = max_p,
    inbounds = if (p_given) {
      any(vapply(reps, function(r) isTRUE(r$p_inbounds), logical(1)))
    } else {
      NA
    },
    p_digits = p_digits,
    p_operator = p_operator,
    mixed_digits = mixed_digits
  )
}

MIXED_DIGITS_NOTE <- "Groups differ in decimal places; coarser precision used"

# Candidate values of `v` when re-expressed at `digits` decimal places: its
# rounded value, or both neighbours if it sits exactly halfway between them.
coarsen <- function(v, digits) {
  s <- v * 10^digits
  if (abs(s - floor(s) - 0.5) < 1e-8) {
    return(c(floor(s), ceiling(s)) / 10^digits)
  }
  round(s) / 10^digits
}


# Result / error UI -------------------------------------------------------

error_ui <- function(msg) {
  span(
    style = "color:#e67e22; font-size:.8rem;",
    HTML("&#9888;&nbsp;"),
    msg
  )
}

uninformative_label <- function(digits) {
  dp_text <- if (digits == 1L) {
    "1 decimal place"
  } else {
    paste0(digits, " decimal places")
  }
  tooltip(
    span(
      class = "text-muted",
      style = "font-size:.75rem; cursor:help;",
      "Uninformative GRIM"
    ),
    paste0(
      "Every possible mean with ",
      dp_text,
      " is achievable for this N and item count, so the GRIM test cannot fail."
    )
  )
}

result_ui <- function(
  ok,
  reasons = character(0),
  uninformative = FALSE,
  digits = NULL,
  notes = character(0)
) {
  note_spans <- lapply(notes, function(n) {
    span(
      class = "text-muted",
      style = "font-size:.72rem; line-height:1.2;",
      n
    )
  })

  # NA = no pass/fail decision (nothing testable). Still surface any notes,
  # e.g. the "GRIM/GRIMMER only apply to integer data" message.
  if (is.na(ok)) {
    if (length(note_spans) == 0) {
      return(span())
    }
    return(do.call(
      div,
      c(list(class = "d-flex flex-wrap align-items-center gap-2"), note_spans)
    ))
  }
  if (ok) {
    badge <- span(
      class = "badge rounded-pill bg-success px-3 py-2",
      HTML("&#10003;&nbsp; Consistent")
    )
    extras <- note_spans
    if (uninformative) {
      extras <- c(extras, list(uninformative_label(digits)))
    }
    if (length(extras) > 0) {
      do.call(
        div,
        c(
          list(class = "d-flex flex-wrap align-items-center gap-2", badge),
          extras
        )
      )
    } else {
      badge
    }
  } else {
    label <- if (length(reasons) > 0) {
      paste(reasons, collapse = "; ")
    } else {
      NULL
    }
    badge <- span(
      class = "badge rounded-pill bg-danger px-3 py-2",
      HTML("&#10007;&nbsp; Inconsistent")
    )
    extras <- list()
    if (!is.null(label) && nzchar(label)) {
      extras <- c(
        extras,
        list(span(
          class = "text-danger",
          style = "font-size:.72rem; line-height:1.2;",
          label
        ))
      )
    }
    extras <- c(extras, note_spans)
    if (uninformative) {
      extras <- c(extras, list(uninformative_label(digits)))
    }
    if (length(extras) > 0) {
      do.call(
        div,
        c(
          list(class = "d-flex flex-wrap align-items-center gap-2", badge),
          extras
        )
      )
    } else {
      badge
    }
  }
}

# UI for the t-test recalculation result of a pair.
t_test_result_ui <- function(tt) {
  if (is.null(tt) || identical(tt$status, "blank")) {
    return(span())
  }
  if (identical(tt$status, "incomplete")) {
    return(span(
      class = "text-muted",
      style = "font-size:.75rem;",
      "Awaiting both groups' M, SD, N"
    ))
  }
  if (identical(tt$status, "error")) {
    return(error_ui(tt$msg))
  }

  dg <- tt$p_digits
  range_txt <- paste0(
    "p ∈ [",
    format_p_value(tt$min_p, dg),
    ", ",
    format_p_value(tt$max_p, dg),
    "]"
  )
  mixed_note <- if (isTRUE(tt$mixed_digits)) {
    span(
      class = "text-muted",
      style = "font-size:.72rem; line-height:1.2;",
      MIXED_DIGITS_NOTE
    )
  }

  if (!isTRUE(tt$p_given)) {
    return(div(
      class = "d-flex flex-wrap align-items-center gap-2",
      span(
        class = "badge rounded-pill bg-secondary px-3 py-2",
        "Recalculated"
      ),
      span(class = "text-muted", style = "font-size:.72rem;", range_txt),
      mixed_note
    ))
  }

  if (isTRUE(tt$inbounds)) {
    badge <- span(
      class = "badge rounded-pill bg-success px-3 py-2",
      HTML("&#10003;&nbsp; Consistent")
    )
    detail_class <- "text-muted"
  } else {
    badge <- span(
      class = "badge rounded-pill bg-danger px-3 py-2",
      HTML("&#10007;&nbsp; Inconsistent")
    )
    detail_class <- "text-danger"
  }
  div(
    class = "d-flex flex-wrap align-items-center gap-2",
    badge,
    span(
      class = detail_class,
      style = "font-size:.72rem; line-height:1.2;",
      range_txt
    ),
    mixed_note
  )
}

summary_bar <- function(results_vec) {
  tested <- !is.na(results_vec)
  if (sum(tested) == 0) {
    return(NULL)
  }
  n_pass <- sum(results_vec[tested])
  n_fail <- sum(!results_vec[tested])
  n_tot <- sum(tested)
  div(
    class = "d-flex gap-4 align-items-center px-3 py-2 rounded mt-3",
    style = "background:#f1f3f5; font-size:.875rem; border-left:3px solid #dee2e6;",
    span(
      class = "text-muted fw-semibold",
      "GRIM / GRIMMER / Bounds:"
    ),
    span(
      class = "text-muted",
      paste(n_tot, if (n_tot == 1) "case" else "cases", "tested")
    ),
    span(class = "text-success fw-semibold", paste(n_pass, "consistent")),
    span(class = "text-danger fw-semibold", paste(n_fail, "inconsistent"))
  )
}

# Summary line for the t-test recalculations across pairs.
t_test_summary_bar <- function(tts) {
  decided <- Filter(
    function(tt) {
      identical(tt$status, "ok") && isTRUE(tt$p_given) && !is.na(tt$inbounds)
    },
    tts
  )
  if (length(decided) == 0) {
    return(NULL)
  }
  inbounds <- vapply(decided, function(tt) isTRUE(tt$inbounds), logical(1))
  n_tot <- length(decided)
  n_ok <- sum(inbounds)
  n_bad <- sum(!inbounds)
  div(
    class = "d-flex gap-4 align-items-center px-3 py-2 rounded mt-2",
    style = "background:#f1f3f5; font-size:.875rem; border-left:3px solid #dee2e6;",
    span(
      class = "text-muted fw-semibold",
      "t-test recalculation:"
    ),
    span(
      class = "text-muted",
      paste(
        n_tot,
        if (n_tot == 1) "reported p" else "reported p-values",
        "checked"
      )
    ),
    span(class = "text-success fw-semibold", paste(n_ok, "consistent")),
    span(class = "text-danger fw-semibold", paste(n_bad, "inconsistent"))
  )
}

next_free_slot <- function(active, max_slots) {
  candidate <- setdiff(seq_len(max_slots), active)
  if (length(candidate) == 0) {
    return(NULL)
  }
  min(candidate)
}


# Row UI (pre-created; show/hide via CSS) ---------------------------------

remove_button <- function(id) {
  actionButton(
    id,
    label = tags$img(
      src = "images/trash-can.svg",
      height = "16px",
      alt = "Remove"
    ),
    class = "btn btn-sm p-1 rm-btn",
    style = "line-height:1;",
    title = "Remove this variable pair"
  )
}

items_input <- function(id) {
  numericInput(id, NULL, value = 1, min = 1, step = 1, width = "100%")
}

# The seven shared data cells (Type, Mean, SD, N, Items, Min, Max) for one
# group row, where `rid` is the row id stem (e.g. "1a").
row_data_cells <- function(rid) {
  tagList(
    div(
      class = "grid-cell",
      selectInput(
        paste0("cb_type_", rid),
        NULL,
        choices = c("Mean", "Percentage"),
        selected = "Mean",
        width = "100%"
      )
    ),
    div(
      class = "grid-cell",
      textInput(
        paste0("cb_x_", rid),
        NULL,
        width = "100%",
        placeholder = "5.23"
      )
    ),
    div(
      class = "grid-cell",
      textInput(
        paste0("cb_sd_", rid),
        NULL,
        width = "100%",
        placeholder = "8.41"
      )
    ),
    div(
      class = "grid-cell",
      textInput(
        paste0("cb_n_", rid),
        NULL,
        width = "100%",
        placeholder = "30"
      )
    ),
    div(class = "grid-cell", items_input(paste0("cb_items_", rid))),
    div(
      class = "grid-cell",
      textInput(
        paste0("cb_min_", rid),
        NULL,
        width = "100%",
        placeholder = "optional"
      )
    ),
    div(
      class = "grid-cell",
      textInput(
        paste0("cb_max_", rid),
        NULL,
        width = "100%",
        placeholder = "optional"
      )
    )
  )
}

# A variable pair: two group rows that share one Variable name. The Variable,
# Reported p, t-test result and remove control sit on the first row only.
combined_pair <- function(p) {
  shown <- if (p <= 2) "" else "display:none;"
  rid_a <- paste0(p, "a")
  rid_b <- paste0(p, "b")
  tagList(
    div(
      id = paste0("cb_slot_", p, "a"),
      class = "cb-row pair-start",
      style = shown,
      div(
        class = "grid-cell",
        textInput(
          paste0("cb_var_", p),
          NULL,
          width = "100%",
          placeholder = "BDI"
        )
      ),
      div(
        class = "grid-cell int-cell",
        checkboxInput(paste0("cb_int_", p), NULL, value = FALSE)
      ),
      div(
        class = "grid-cell",
        textInput(
          paste0("cb_grp_", rid_a),
          NULL,
          width = "100%",
          placeholder = "Intervention"
        )
      ),
      row_data_cells(rid_a),
      div(
        class = "grid-cell",
        selectInput(
          paste0("cb_pop_", p),
          NULL,
          choices = c(
            "=" = "equals",
            "<" = "less_than",
            ">" = "greater_than",
            "<=" = "less_than_or_equal_to",
            ">=" = "greater_than_or_equal_to"
          ),
          selected = "equals",
          width = "100%"
        )
      ),
      div(
        class = "grid-cell",
        textInput(
          paste0("cb_p_", p),
          NULL,
          width = "100%",
          placeholder = "optional"
        )
      ),
      div(
        class = "grid-cell d-flex align-items-center",
        uiOutput(paste0("cb_badge_", rid_a))
      ),
      div(
        class = "grid-cell d-flex align-items-center",
        uiOutput(paste0("cb_t_test_", p))
      ),
      div(class = "grid-cell", remove_button(paste0("cb_rm_", p)))
    ),
    div(
      id = paste0("cb_slot_", p, "b"),
      class = "cb-row pair-end",
      style = shown,
      div(class = "grid-cell"),
      div(class = "grid-cell"),
      div(
        class = "grid-cell",
        textInput(
          paste0("cb_grp_", rid_b),
          NULL,
          width = "100%",
          placeholder = "Control"
        )
      ),
      row_data_cells(rid_b),
      div(class = "grid-cell"),
      div(class = "grid-cell"),
      div(
        class = "grid-cell d-flex align-items-center",
        uiOutput(paste0("cb_badge_", rid_b))
      ),
      div(class = "grid-cell"),
      div(class = "grid-cell")
    )
  )
}


# Column headers ----------------------------------------------------------

combined_header <- div(
  class = "cb-row cb-header",
  div(class = "grid-hdr", "Label (optional)"),
  div(class = "grid-hdr", "Integer data"),
  div(class = "grid-hdr", "Group (optional)"),
  div(class = "grid-hdr", "Type"),
  div(class = "grid-hdr", "Mean or percentage"),
  div(class = "grid-hdr", "SD"),
  div(class = "grid-hdr", "Sample size"),
  div(class = "grid-hdr", "Items averaged over"),
  div(class = "grid-hdr", "Logical Min (optional)"),
  div(class = "grid-hdr", "Logical Max (optional)"),
  div(class = "grid-hdr", "p operator"),
  div(class = "grid-hdr", "Reported p (optional)"),
  div(class = "grid-hdr", "Result (GRIM / GRIMMER / Bounds)"),
  div(class = "grid-hdr", "Result (p value)"),
  div()
)


# Single-row tab (GRIM / GRIMMER / Bounds only) ---------------------------

# One independent row: M / SD / N with GRIM / GRIMMER / Bounds, no pairing
# and no t-test. Uses the `gb_` input-id namespace so it never collides with
# the paired tab's `cb_` ids.
single_row <- function(id) {
  div(
    id = paste0("gb_slot_", id),
    class = "sg-row",
    style = if (id <= 3) "" else "display:none;",
    div(
      class = "grid-cell",
      textInput(
        paste0("gb_var_", id),
        NULL,
        width = "100%",
        placeholder = "BDI"
      )
    ),
    div(
      class = "grid-cell int-cell",
      checkboxInput(paste0("gb_int_", id), NULL, value = FALSE)
    ),
    div(
      class = "grid-cell",
      selectInput(
        paste0("gb_type_", id),
        NULL,
        choices = c("Mean", "Percentage"),
        selected = "Mean",
        width = "100%"
      )
    ),
    div(
      class = "grid-cell",
      textInput(
        paste0("gb_x_", id),
        NULL,
        width = "100%",
        placeholder = "e.g. 5.23"
      )
    ),
    div(
      class = "grid-cell",
      textInput(
        paste0("gb_sd_", id),
        NULL,
        width = "100%",
        placeholder = "optional"
      )
    ),
    div(
      class = "grid-cell",
      textInput(
        paste0("gb_n_", id),
        NULL,
        width = "100%",
        placeholder = "e.g. 30"
      )
    ),
    div(class = "grid-cell", items_input(paste0("gb_items_", id))),
    div(
      class = "grid-cell",
      textInput(
        paste0("gb_min_", id),
        NULL,
        width = "100%",
        placeholder = "optional"
      )
    ),
    div(
      class = "grid-cell",
      textInput(
        paste0("gb_max_", id),
        NULL,
        width = "100%",
        placeholder = "optional"
      )
    ),
    div(
      class = "grid-cell d-flex align-items-center",
      uiOutput(paste0("gb_badge_", id))
    ),
    div(class = "grid-cell", remove_button(paste0("gb_rm_", id)))
  )
}

single_header <- div(
  class = "sg-row sg-header",
  div(class = "grid-hdr", "Label (optional)"),
  div(class = "grid-hdr", "Integer data"),
  div(class = "grid-hdr", "Type"),
  div(class = "grid-hdr", "Mean or percentage"),
  div(class = "grid-hdr", "SD (optional)"),
  div(class = "grid-hdr", "Sample size"),
  div(class = "grid-hdr", "Items averaged over"),
  div(class = "grid-hdr", "Logical Min (optional)"),
  div(class = "grid-hdr", "Logical Max (optional)"),
  div(class = "grid-hdr", "Result"),
  div()
)


# Custom CSS --------------------------------------------------------------

custom_css <- tags$style(HTML(
  "
  body { background-color: #f8f9fa; }

  /* ── navbar ─────────────────────────────────────────────────────────── */
  nav.navbar {
    min-height: 56px;
    padding-top: 0 !important;
    padding-bottom: 0 !important;
  }
  nav.navbar > .container-fluid {
    align-items: center !important;
    min-height: 56px;
  }
  .navbar-brand {
    position: relative;
    padding: 0 1.5rem 0 0 !important;
    margin-right: .75rem !important;
    align-self: stretch;
    display: flex !important;
    align-items: center !important;
    font-weight: 700;
    letter-spacing: -.02em;
  }
  .navbar-brand::after {
    content: '';
    position: absolute;
    right: 0;
    top: 15%;
    height: 70%;
    width: 1px;
    background: linear-gradient(to bottom, transparent, rgba(255,255,255,.5) 30%, rgba(255,255,255,.5) 70%, transparent);
  }
  .navbar-brand img {
    display: block;
    height: 56px;
    width: auto;
  }
  /* The brand is not a functional link, so suppress flatly's green
     (--bs-navbar-brand-hover-color: #18bc9c) hover/focus colour change. */
  nav.navbar .navbar-brand:hover,
  nav.navbar .navbar-brand:focus {
    color: var(--bs-navbar-brand-color, #fff) !important;
  }
  nav.navbar .navbar-nav {
    align-items: center !important;
    gap: .25rem;
  }
  nav.navbar .nav-link {
    border-bottom: none !important;
    border-radius: .375rem !important;
    padding: .35rem .8rem !important;
    color: rgba(255,255,255,.75) !important;
    font-weight: 500;
    transition: background-color .18s ease, color .18s ease;
  }
  nav.navbar .nav-link:hover {
    background-color: rgba(255,255,255,.12) !important;
    color: #fff !important;
  }
  nav.navbar .nav-link.active {
    background-color: #fff !important;
    color: #1e3a5f !important;
    font-weight: 600;
  }

  /* ── cards & inputs ─────────────────────────────────────────────────── */
  .card { border: none; box-shadow: 0 1px 4px rgba(0,0,0,.08); }
  .card-header { background: white; border-bottom: 1px solid #e9ecef; font-weight: 600; font-size: 1rem; }
  .form-control { border-color: #dee2e6; font-size: .9rem; }
  .form-control:focus { border-color: #2c7be5; box-shadow: 0 0 0 .2rem rgba(44,123,229,.15); }
  .btn-outline-primary { color: #2c7be5; border-color: #2c7be5; }
  .btn-outline-primary:hover { background: #2c7be5; color: white; }

  /* ── action buttons ──────────────────────────────────────────────────── */
  #combined_add, #download_csv, #gb_add, #gb_download {
    border: none !important;
    color: #fff !important;
    font-size: .875rem !important;
    font-weight: 500 !important;
    padding: .375rem .875rem !important;
    border-radius: .375rem !important;
    transition: background-color .2s ease, box-shadow .2s ease, transform .1s ease !important;
  }
  #combined_add, #gb_add {
    background-color: #2c7be5 !important;
    box-shadow: 0 1px 4px rgba(44,123,229,.35) !important;
  }
  #combined_add:hover, #combined_add:focus, #gb_add:hover, #gb_add:focus {
    background-color: #1a68d1 !important;
    color: #fff !important;
    box-shadow: 0 4px 12px rgba(44,123,229,.45) !important;
    transform: translateY(-1px);
  }
  #combined_add:active, #gb_add:active {
    background-color: #155ab8 !important;
    transform: translateY(0);
    box-shadow: 0 1px 4px rgba(44,123,229,.35) !important;
  }
  #download_csv, #gb_download {
    background-color: #495057 !important;
    box-shadow: 0 1px 4px rgba(73,80,87,.35) !important;
  }
  #download_csv:hover, #download_csv:focus, #gb_download:hover, #gb_download:focus {
    background-color: #343a40 !important;
    color: #fff !important;
    box-shadow: 0 4px 12px rgba(73,80,87,.45) !important;
    transform: translateY(-1px);
  }
  #download_csv:active, #gb_download:active {
    background-color: #212529 !important;
    transform: translateY(0);
    box-shadow: 0 1px 4px rgba(73,80,87,.35) !important;
  }
  .badge { font-size: .8rem !important; font-weight: 500; letter-spacing: .01em; }
  .bg-success { background-color: #12b886 !important; }
  .bg-danger  { background-color: #fa5252 !important; }
  .bg-secondary { background-color: #868e96 !important; }
  .shiny-input-container { margin-bottom: 0; }
  ::placeholder { color: #adb5bd !important; font-style: italic; }
  .rm-btn { background: transparent !important; border: none !important; opacity: 1 !important; }
  .rm-btn img { opacity: 1 !important; filter: none; }
  .rm-btn:hover { background-color: #fa5252 !important; border-radius: 4px; }
  .rm-btn:hover img { filter: brightness(0) invert(1) !important; }

  /* ── input grid ──────────────────────────────────────────────────────── */
  .combined-grid-wrap { overflow-x: auto; }
  .combined-grid {
    display: grid;
    grid-template-columns: 120px 80px 110px 115px 100px 80px 75px 100px 90px 90px 80px 90px minmax(200px, 1.1fr) minmax(260px, 1.5fr) auto;
    column-gap: .5rem;
    row-gap: 0;
    min-width: 1728px;
    padding-right: 1.25rem;
  }
  .combined-grid > div {
    display: grid;
    grid-column: 1 / -1;
    grid-template-columns: subgrid;
    align-items: center;
  }
  .combined-grid > div.cb-header { align-items: end; }
  .cb-row.pair-start { border-top: 2px solid #ced4da; padding-top: 5px; }
  .cb-row.pair-end { padding-bottom: 7px; }

  /* ── single-row grid (GRIM / GRIMMER / Bounds tab) ───────────────────── */
  .single-grid-wrap { overflow-x: auto; }
  .single-grid {
    display: grid;
    grid-template-columns: 120px 80px 140px 110px 100px 100px 80px 150px 150px minmax(280px, 1.6fr) auto;
    column-gap: .5rem;
    row-gap: 0;
    min-width: 1418px;
  }
  .single-grid > div {
    display: grid;
    grid-column: 1 / -1;
    grid-template-columns: subgrid;
    align-items: center;
  }
  .single-grid > div.sg-header { align-items: end; }

  .grid-cell { padding: 2px 0; }
  .int-cell { display: flex; align-items: center; justify-content: center; }
  .int-cell .form-group, .int-cell .checkbox, .int-cell .shiny-input-container { margin: 0 !important; min-height: 0 !important; }
  .int-cell .form-check { margin: 0 !important; min-height: 0 !important; padding-left: 0 !important; }
  .int-cell input[type=checkbox] { margin: 0 !important; float: none !important; width: 18px; height: 18px; cursor: pointer; }
  .grid-hdr {
    padding: 4px 0 2px;
    font-size: .8rem;
    font-weight: 600;
    color: #868e96;
    text-transform: uppercase;
    letter-spacing: .05em;
    word-break: normal;
    hyphens: none;
  }
"
))


# Main UI -----------------------------------------------------------------

ui <- page_navbar(
  title = div(
    class = "d-flex align-items-center gap-2",
    tags$img(
      src = "images/inspect-sr.png",
      alt = "INSPECT-SR",
      height = "56"
    ),
    "Consistency Tester"
  ),
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2c7be5",
    "navbar-bg" = "#1e3a5f",
    "navbar-padding-y" = "0",
    "navbar-brand-padding-y" = "0",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  navbar_options = navbar_options(bg = "#1e3a5f", underline = FALSE),
  header = tagList(custom_css),

  nav_panel(
    "GRIM / GRIMMER / Bounds",
    div(
      class = "container py-4",
      style = "max-width:1470px;",
      card(
        card_header("GRIM, GRIMMER and Bounds Tests"),
        card_body(
          p(
            class = "text-muted mb-3",
            tags$em("Key assumptions:"),
            br(),
            br(),
            "1. ",
            tags$em("Items Averaged Over"),
            " is often misunderstood. It is ",
            tags$em("not"),
            " the number of items in a multi-item Likert scale, but the number of items averaged over at the participant level. If the scale is sum-scored (which is the most common scoring method in psychology), no averaging has occurred so ",
            tags$em("Items Averaged Over"),
            " = 1. If the scale was mean-scored, then ",
            tags$em("Items Averaged Over"),
            " = the number of items in the scale. Variables such as \"age\" or \"days\" are implicitly single-item scales, therefore ",
            tags$em("Items Averaged Over"),
            " = 1.",
            br(),
            br(),
            "2. ",
            tags$em("Logical Min"),
            " and ",
            tags$em("Logical Max"),
            " should be set to the scale's logical min and max, not the observed min and max in the data."
          ),
          div(
            class = "single-grid-wrap",
            div(
              class = "single-grid",
              single_header,
              tagList(lapply(seq_len(MAX_ROWS), single_row))
            )
          ),
          uiOutput("gb_empty"),
          uiOutput("gb_vis"),
          div(
            class = "mt-3 d-flex gap-2",
            actionButton(
              "gb_add",
              "+ Add row"
            ),
            downloadButton(
              "gb_download",
              "Download CSV"
            )
          ),
          uiOutput("gb_summary")
        )
      ),
      p(
        class = "text-muted mt-3 mb-0",
        style = "font-size:.78rem; text-align:center;",
        "App by Lukas Jung and Ian Hussey, University of Bern."
      )
    )
  ),

  nav_panel(
    "GRIM / GRIMMER / Bounds / t-test p value",
    div(
      class = "container py-4",
      style = "max-width:1800px;",
      card(
        card_header("GRIM, GRIMMER, Bounds and t-test Recalculation"),
        card_body(
          p(
            class = "text-muted mb-3",
            tags$em("Key assumptions:"),
            br(),
            br(),
            "1. ",
            tags$em("Items Averaged Over"),
            " is often misunderstood. It is ",
            tags$em("not"),
            " the number of items in a multi-item Likert scale, but the number of items averaged over at the participant level. If the scale is sum-scored (which is the most common scoring method in psychology), no averaging has occurred so ",
            tags$em("Items Averaged Over"),
            " = 1. If the scale was mean-scored, then ",
            tags$em("Items Averaged Over"),
            " = the number of items in the scale. Variables such as \"age\" or \"days\" are implicitly single-item scales, therefore ",
            tags$em("Items Averaged Over"),
            " = 1.",
            br(),
            br(),
            "2. ",
            tags$em("Logical Min"),
            " and ",
            tags$em("Logical Max"),
            " should be set to the scale's logical min and max, not the observed min and max in the data."
          ),
          div(
            class = "combined-grid-wrap",
            div(
              class = "combined-grid",
              combined_header,
              tagList(lapply(seq_len(MAX_PAIRS), combined_pair))
            )
          ),
          uiOutput("combined_empty"),
          uiOutput("combined_vis"),
          div(
            class = "mt-3 d-flex gap-2",
            actionButton(
              "combined_add",
              "+ Add variable"
            ),
            downloadButton(
              "download_csv",
              "Download CSV"
            )
          ),
          uiOutput("combined_summary"),
          uiOutput("t_test_summary")
        )
      ),
      p(
        class = "text-muted mt-3 mb-0",
        style = "font-size:.78rem; text-align:center;",
        "App by Lukas Jung and Ian Hussey, University of Bern."
      )
    )
  ),

  nav_panel(
    "Guidance",
    div(
      class = "container py-4",
      style = "max-width:900px;",
      card(
        card_header("Guidance on app usage"),
        card_body(
          p(
            "This app is primarily directed at INSPECT-SR users who are not",
            "experts in GRIM and GRIMMER. It is designed to emphasize ease of",
            "use above maximum functionality. For more advanced use of these",
            "tests, see",
            a(
              "this app",
              href = "https://errors.shinyapps.io/scrutiny/",
              style = "color:#ca225e;",
            ),
            "or the",
            a(
              "scrutiny R package",
              href = "https://lhdjung.github.io/scrutiny/",
              style = "color:#ca225e;",
              .noWS = "after"
            ),
            ". Both allow you to test many values at once, e.g., via",
            "a CSV file. See also",
            a(
              "this website",
              href = "https://trustworthy.scientific.claims/tools/",
              style = "color:#ca225e;"
            ),
            "for more forensic metascience tools.",
            br(),
            br(),
            tags$strong(
              "With mean-scored scales composed of multiple items",
              .noWS = "after"
            ),
            ", make sure to set",
            tags$em("Items averaged over"),
            "to the number of those items. This is crucial for the test outcome.",
            tags$em("Items averaged over"),
            "is the number of test items combined to one final mean score –",
            " not the number of scale points! Don't use this field for",
            " sum scores. Only use it if the score was averaged over multiple",
            " test items. For example, age is implicitly a one-item measure:",
            " It is not composed of multiple items.",
            br(),
            br(),
            tags$strong("Cautionary tale about using GRIM:"),
            a(
              "Sakaluk (2020)",
              href = "https://doi.org/10.1007/s10508-020-01795-8",
              style = "color:#ca225e;"
            ),
            " misapplied GRIM by failing to take multi-item scales into account.",
            "This was pointed out by the original authors he had criticised",
            a(
              "(Wisman and Shira, 2022)",
              href = "https://doi.org/10.1007/s10508-021-02239-7",
              style = "color:#ca225e;",
              .noWS = "after"
            ),
            ", who showed that all of their mean-N pairs in question were",
            "consistent when appropriately multiplying the reported Ns with the",
            "respective numbers of items. This shows the importance of guarding",
            "against incorrect claims of errors when using forensic methods.",
            br(),
            br(),
            "Also, don't transform any values – enter them just as you read",
            "them in an article, including any trailing zeros. For rounding,",
            "the app assumes numbers were rounded either up from 5 or down",
            "from 5; both are accepted.",
            br(),
            br(),
            "Possible reasons for inconsistency:",
            br(),
            tags$ul(
              tags$li(
                "\"Mean fails GRIM\": mean and sample size are inconsistent."
              ),
              tags$li(
                "\"SD fails GRIMMER\": mean, SD, and sample size are inconsistent.",
                "GRIMMER adds 3 separate tests; the message will say which one failed."
              ),
              tags$li(
                "\"Mean out of bounds\": the reported mean is below ",
                tags$em("Min"),
                " or above ",
                tags$em("Max"),
                ", even allowing for rounding."
              ),
              tags$li(
                "\"SD exceeds Bhatia–Davis bound\": for a variable in [Min, Max] ",
                "with the reported mean and N, the maximum possible sample SD is ",
                tags$code("sqrt((Max - Mean) * (Mean - Min) * N / (N - 1))"),
                ". A reported SD above this bound is impossible. Because the",
                " reported mean and SD are rounded, the app only flags an SD",
                " that exceeds the bound for every mean and SD that would",
                " round to the reported values."
              )
            ),
            "If multiple checks fail, all failing reasons are listed.",
            br(),
            br(),
            "GRIMMER is GRIM plus 3 additional tests:",
            tags$ol(
              tags$li(
                "The reconstructed sum of squared observations must be a whole number."
              ),
              tags$li("The reconstructed SD must match the reported one."),
              tags$li(
                "The reconstructed sum of squared observations and the",
                "reconstructed sum of integers of which the reported means",
                "are fractions must both be even or both be odd."
              )
            ),
            "Bounds inputs (i.e., \"Logical Min (optional)\" and \"Logical Max (optional)\")",
            "are optional. They enable two additional checks: (a) the mean",
            " must lie inside [Logical Min, Logical Max]; (b) the SD must",
            " not exceed the Bhatia–Davis upper bound. If only one of the two",
            " is entered, the mean is checked against that bound alone and a",
            " note says so; check (b) needs both. When \"Type\" is set to \"Percentage\"",
            "and an SD is provided, \"Logical Min\" and \"Logical Max\" are required",
            " (typically 0 and 100), and GRIMMER is not run.",
            br(),
            br(),
            tags$strong("Integer data:"),
            "GRIM and GRIMMER are only valid if the underlying data are whole",
            "numbers (e.g., Likert responses, counts, age in years). They are",
            "therefore only run when you tick the",
            tags$em("Integer data"),
            "box, which is unticked by default. Without it, only the Bounds",
            "checks are run, so a \"Consistent\" result then refers to the",
            "bounds alone. Likewise, GRIM, GRIMMER and the Bhatia–Davis SD",
            "bound need the sample size; without it, only the mean's bounds",
            "are checked. A note next to the result names any skipped tests.",
            "Enter N as plain digits, without separators (e.g., 2000, not",
            "2,000). Commas are not accepted in any number, because they",
            "mean thousands in some countries and decimals in others: always",
            "use a decimal point and no thousands separators (e.g., 1234.5).",
            br(),
            br(),
            "Click \"Download CSV\" to get all the results in a tabular file."
          ),
          p(
            tags$strong("t-test recalculation between paired Group rows:"),
            "Each pair of rows that shares a ",
            tags$em("Label"),
            " is treated as the two arms of an independent-samples comparison",
            " (e.g., intervention vs. control). When both rows have a Mean, SD",
            " and N, the app recalculates the range of two-sided t-test",
            " p-values that are compatible with those summary statistics, using",
            " the ",
            a(
              "recalc",
              href = "https://github.com/ianhussey/recalc",
              style = "color:#ca225e;"
            ),
            " package. The recalculation explores a small multiverse of",
            " defensible analytic choices: the reported means and SDs are each",
            " varied within their rounding intervals, and both Student's (pooled)",
            " and Welch's t-tests are computed, in both effect directions. This",
            " yields a range ",
            tags$em("[min p, max p]"),
            " rather than a single value.",
            br(),
            br(),
            "If you enter a ",
            tags$em("Reported p"),
            " on the first row of the pair, the app checks whether that",
            " reported value falls inside the recalculated range (after rounding",
            " both to the reported p's precision):"
          ),
          tags$ul(
            tags$li(
              tags$strong("\"Consistent\""),
              ": the reported p-value is compatible with the reported means,",
              " SDs and Ns under at least one of the analytic choices explored."
            ),
            tags$li(
              tags$strong("\"Inconsistent\""),
              ": no combination of the explored choices yields the reported",
              " p-value. This warrants a closer look – it may reflect a typo,",
              " a different (e.g. adjusted or non-parametric) test, a covariate",
              " adjustment, or an error."
            )
          ),
          p(
            "The ",
            tags$em("p operator"),
            " selector controls how the reported p is compared: ",
            tags$em("="),
            " checks that the value lies within the recalculated range, while ",
            tags$em("<"),
            ", ",
            tags$em(">"),
            ", ",
            tags$em("<="),
            " and ",
            tags$em(">="),
            " check the reported inequality against that range (useful when a",
            " paper reports, e.g., \"p < .001\").",
            br(),
            br(),
            "If no reported p is entered, the app simply shows the recalculated",
            " range. The t-test recalculation uses only the Mean, SD and N; it",
            " ignores ",
            tags$em("Type"),
            ", ",
            tags$em("Items"),
            " and the bounds, which apply to GRIM / GRIMMER / Bounds only.",
            " Decimal precision for the means and SDs is detected automatically",
            " from the values you enter, so enter them exactly as reported,",
            " including trailing zeros. If the two groups' means (or SDs) are",
            " reported to different numbers of decimal places, the coarser",
            " precision is used for both, which widens the recalculated range."
          ),
          p(
            tags$strong("When GRIM is uninformative:"),
            "GRIM cannot fail when every possible mean is achievable",
            "for the given sample size, i.e., when",
            tags$em("N * (Items averaged over)"),
            "≥ 10",
            tags$sup("D", .noWS = "outside"),
            ", where",
            tags$em("D"),
            "is the number of decimal places of the reported mean,",
            "plus 2 for percentages. In these cases the app marks the result",
            "with an",
            tags$em("Uninformative GRIM"),
            "label and adds a corresponding entry to the CSV",
            tags$em("notes"),
            "column.",
            "If an SD is provided, the GRIMMER SD-based checks",
            "remain informative even when the GRIM portion is not."
          )
        )
      ),
      p(
        class = "text-muted mt-3 mb-0",
        style = "font-size:.78rem; text-align:center;",
        "App by Lukas Jung and Ian Hussey, University of Bern."
      )
    )
  ),

  nav_panel(
    "About",
    div(
      class = "container py-4",
      style = "max-width:900px;",
      card(
        card_header("About"),
        card_body(
          p(
            class = "text-muted mb-3",
            "GRIM and GRIMMER represent",
            a(
              "INSPECT-SR check 4.8",
              href = "https://inspect.sr/chapters/check_4_8.html",
              style = "color:#ca225e;",
              .noWS = "after"
            ),
            ". INSPECT-SR (",
            a(
              "Wilkinson et al., 2025",
              href = "https://www.medrxiv.org/content/10.1101/2025.09.03.25334905v3",
              style = "color:#ca225e;",
              .noWS = "outside"
            ),
            ") is a framework for assessing the trustworthiness of",
            "randomised controlled trials in systematic reviews. However, it",
            "can be applied to other fields, as well.",
            br(),
            br(),
            "For GRIM, see",
            a(
              "Brown and Heathers (2017)",
              href = "https://doi.org/10.1177/1948550616673876",
              style = "color:#ca225e;",
              .noWS = "after"
            ),
            ". For GRIMMER, see ",
            a(
              "Allard (2018)",
              href = "https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/",
              style = "color:#ca225e;",
              .noWS = "after"
            ),
            ". The t-test p-value recalculation uses the ",
            a(
              "recalc",
              href = "https://github.com/ianhussey/recalc",
              style = "color:#ca225e;"
            ),
            "package.",
            br(),
            br(),
            "Shiny app made by Lukas Jung and Ian Hussey, University of Bern, using the",
            a(
              "scrutiny",
              href = "https://lhdjung.github.io/scrutiny/",
              style = "color:#ca225e;"
            ),
            "package for error detection in science. Source code is available ",
            a(
              "on Github",
              href = "https://github.com/lhdjung/inspect-sr-means-variances",
              style = "color:#ca225e;",
              .noWS = "after"
            ),
            "."
          )
        )
      )
    )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # Percentages default to bounds of 0 and 100. Remember which cells were
  # auto-filled so that switching back to "Mean" removes them again instead of
  # silently applying percentage bounds to a mean. `prefix` is "gb_" or "cb_";
  # `rid` is the row id stem.
  sync_type_bounds <- function(prefix, rid) {
    defaults <- c(min = "0", max = "100")
    auto <- c(min = FALSE, max = FALSE)
    type_id <- paste0(prefix, "type_", rid)
    observeEvent(
      input[[type_id]],
      {
        is_pct <- isTRUE(input[[type_id]] == "Percentage")
        for (b in names(defaults)) {
          id <- paste0(prefix, b, "_", rid)
          cur <- input[[id]]
          cur <- if (is.null(cur)) "" else trimws(cur)
          if (is_pct && !nzchar(cur)) {
            updateTextInput(session, id, value = defaults[[b]])
            auto[[b]] <<- TRUE
          } else if (!is_pct && auto[[b]]) {
            if (cur == defaults[[b]]) {
              updateTextInput(session, id, value = "")
            }
            auto[[b]] <<- FALSE
          }
        }
      },
      ignoreInit = TRUE
    )
  }

  # ── Single-row tab: GRIM / GRIMMER / Bounds (gb_ namespace) ───────────────
  gb_slots <- reactiveVal(1:3)

  single_visibility_css <- function(active) {
    rules <- vapply(
      seq_len(MAX_ROWS),
      function(i) {
        display <- if (i %in% active) "grid" else "none"
        sprintf("#gb_slot_%d{display:%s!important}", i, display)
      },
      character(1)
    )
    tags$style(paste(rules, collapse = ""))
  }

  output$gb_vis <- renderUI(single_visibility_css(gb_slots()))

  output$gb_empty <- renderUI({
    if (length(gb_slots()) == 0) {
      p(
        class = "text-muted fst-italic small mt-2 mb-0",
        "No rows. Click \"+ Add row\" to add one."
      )
    }
  })

  # Slots are kept sorted so that the CSV lists rows in on-screen (DOM) order.
  observeEvent(input$gb_add, {
    s <- gb_slots()
    ns <- next_free_slot(s, MAX_ROWS)
    if (!is.null(ns)) gb_slots(sort(c(s, ns)))
  })

  for (i in seq_len(MAX_ROWS)) {
    local({
      ii <- i

      observeEvent(
        input[[paste0("gb_rm_", ii)]],
        {
          current <- gb_slots()
          if (ii %in% current) {
            updateTextInput(session, paste0("gb_var_", ii), value = "")
            updateCheckboxInput(session, paste0("gb_int_", ii), value = FALSE)
            updateTextInput(session, paste0("gb_x_", ii), value = "")
            updateTextInput(session, paste0("gb_sd_", ii), value = "")
            updateTextInput(session, paste0("gb_n_", ii), value = "")
            updateNumericInput(session, paste0("gb_items_", ii), value = 1)
            updateTextInput(session, paste0("gb_min_", ii), value = "")
            updateTextInput(session, paste0("gb_max_", ii), value = "")
            updateSelectInput(
              session,
              paste0("gb_type_", ii),
              selected = "Mean"
            )
            gb_slots(setdiff(current, ii))
          }
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      sync_type_bounds("gb_", ii)

      output[[paste0("gb_badge_", ii)]] <- renderUI({
        x_str <- input[[paste0("gb_x_", ii)]]
        sd_str <- input[[paste0("gb_sd_", ii)]]
        n_str <- input[[paste0("gb_n_", ii)]]
        items <- input[[paste0("gb_items_", ii)]]
        type <- input[[paste0("gb_type_", ii)]]
        min_str <- input[[paste0("gb_min_", ii)]]
        max_str <- input[[paste0("gb_max_", ii)]]
        integer <- isTRUE(input[[paste0("gb_int_", ii)]])
        res <- evaluate_row(
          x_str,
          sd_str,
          n_str,
          items,
          type,
          min_str,
          max_str,
          integer
        )
        if (!is.null(res$err)) {
          return(error_ui(res$err))
        }
        result_ui(
          res$ok,
          res$reasons,
          uninformative = isTRUE(res$uninformative),
          digits = res$grim_digits,
          notes = res$notes
        )
      })
    })
  }

  output$gb_summary <- renderUI({
    s <- gb_slots()
    results <- vapply(
      s,
      function(i) {
        evaluate_row(
          input[[paste0("gb_x_", i)]],
          input[[paste0("gb_sd_", i)]],
          input[[paste0("gb_n_", i)]],
          input[[paste0("gb_items_", i)]],
          input[[paste0("gb_type_", i)]],
          input[[paste0("gb_min_", i)]],
          input[[paste0("gb_max_", i)]],
          isTRUE(input[[paste0("gb_int_", i)]])
        )$ok
      },
      logical(1)
    )
    summary_bar(results)
  })

  output$gb_download <- downloadHandler(
    filename = function() {
      paste0("grim-grimmer-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      s <- gb_slots()
      row_counter <- 0L
      rows <- lapply(s, function(i) {
        x_str <- input[[paste0("gb_x_", i)]]
        sd_str <- input[[paste0("gb_sd_", i)]]
        n_str <- input[[paste0("gb_n_", i)]]
        items <- input[[paste0("gb_items_", i)]]
        type <- input[[paste0("gb_type_", i)]]
        min_str <- input[[paste0("gb_min_", i)]]
        max_str <- input[[paste0("gb_max_", i)]]
        variable <- input[[paste0("gb_var_", i)]]
        integer <- isTRUE(input[[paste0("gb_int_", i)]])
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NULL)
        }
        # When the Variable cell is blank, fall back to an incremental value
        # numbered per emitted row.
        row_counter <<- row_counter + 1L
        var_val <- if (!is.null(variable) && nzchar(trimws(variable))) {
          trimws(variable)
        } else {
          as.character(row_counter)
        }
        sd_given <- !is.null(sd_str) && nzchar(trimws(sd_str))
        min_given <- !is.null(min_str) && nzchar(trimws(min_str))
        max_given <- !is.null(max_str) && nzchar(trimws(max_str))
        # fmt: skip
        res <- evaluate_row(
          x_str, sd_str, n_str, items, type, min_str, max_str, integer
        )
        test_label <- if (length(res$tests_run) == 0) {
          ""
        } else {
          paste(res$tests_run, collapse = "+")
        }
        inconsistency <- if (!is.null(res$err)) {
          res$err
        } else if (!is.na(res$ok) && !res$ok) {
          paste(res$reasons, collapse = "; ")
        } else {
          ""
        }
        note_parts <- res$notes
        if (isTRUE(res$uninformative)) {
          note_parts <- c(
            note_parts,
            paste(
              "Uninformative GRIM: every possible mean is achievable for this N",
              "and item count."
            )
          )
        }
        notes <- paste(note_parts, collapse = "; ")
        data.frame(
          label = var_val,
          integer_data = integer,
          type = if (is.null(type)) "Mean" else type,
          mean = trimws(x_str),
          sd = if (sd_given) trimws(sd_str) else "",
          n = if (!is.null(n_str)) trimws(n_str) else "",
          items = if (!is.null(items) && !is.na(items)) items else NA_real_,
          min = if (min_given) trimws(min_str) else "",
          max = if (max_given) trimws(max_str) else "",
          test = test_label,
          consistent = res$ok,
          inconsistency = inconsistency,
          notes = notes,
          stringsAsFactors = FALSE
        )
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0) {
        df <- data.frame(
          label = character(),
          integer_data = logical(),
          type = character(),
          mean = character(),
          sd = character(),
          n = character(),
          items = numeric(),
          min = character(),
          max = character(),
          test = character(),
          consistent = logical(),
          inconsistency = character(),
          notes = character(),
          stringsAsFactors = FALSE
        )
      } else {
        df <- do.call(rbind, rows)
      }
      write.csv(df, file, row.names = FALSE)
    }
  )

  # ── Paired tab: GRIM / GRIMMER / Bounds / t-test p value (cb_ namespace) ──
  pairs <- reactiveVal(1:2)

  paired_visibility_css <- function(active) {
    rules <- vapply(
      seq_len(MAX_PAIRS),
      function(p) {
        display <- if (p %in% active) "grid" else "none"
        sprintf(
          "#cb_slot_%da{display:%s!important}#cb_slot_%db{display:%s!important}",
          p,
          display,
          p,
          display
        )
      },
      character(1)
    )
    tags$style(paste(rules, collapse = ""))
  }

  output$combined_vis <- renderUI(paired_visibility_css(pairs()))

  output$combined_empty <- renderUI({
    if (length(pairs()) == 0) {
      p(
        class = "text-muted fst-italic small mt-2 mb-0",
        "No variables. Click \"+ Add variable\" to add one."
      )
    }
  })

  observeEvent(input$combined_add, {
    s <- pairs()
    ns <- next_free_slot(s, MAX_PAIRS)
    if (!is.null(ns)) pairs(sort(c(s, ns)))
  })

  # Read the GRIM/GRIMMER/Bounds inputs for one row id stem (e.g. "1a").
  read_row <- function(rid) {
    list(
      x = input[[paste0("cb_x_", rid)]],
      sd = input[[paste0("cb_sd_", rid)]],
      n = input[[paste0("cb_n_", rid)]],
      items = input[[paste0("cb_items_", rid)]],
      type = input[[paste0("cb_type_", rid)]],
      min = input[[paste0("cb_min_", rid)]],
      max = input[[paste0("cb_max_", rid)]]
    )
  }

  row_result_from_inputs <- function(rid) {
    r <- read_row(rid)
    # The Integer-data flag is per-pair; strip the trailing side letter to get
    # the pair index (e.g. "12a" -> "12").
    p <- substr(rid, 1, nchar(rid) - 1)
    integer <- isTRUE(input[[paste0("cb_int_", p)]])
    evaluate_row(r$x, r$sd, r$n, r$items, r$type, r$min, r$max, integer)
  }

  pair_t_test_from_inputs <- function(p) {
    rid_a <- paste0(p, "a")
    rid_b <- paste0(p, "b")
    evaluate_pair_t_test(
      input[[paste0("cb_x_", rid_a)]],
      input[[paste0("cb_sd_", rid_a)]],
      input[[paste0("cb_n_", rid_a)]],
      input[[paste0("cb_x_", rid_b)]],
      input[[paste0("cb_sd_", rid_b)]],
      input[[paste0("cb_n_", rid_b)]],
      input[[paste0("cb_p_", p)]],
      input[[paste0("cb_pop_", p)]]
    )
  }

  # The recalculation is slow (~0.1 s per pair), so each pair is computed once
  # here and shared by its badge, the summary and the CSV. An edit then only
  # reruns the pair it touches.
  t_test_results <- lapply(seq_len(MAX_PAIRS), function(p) {
    reactive(pair_t_test_from_inputs(p))
  })

  # Pre-register outputs and observers for every possible pair / row
  for (p in seq_len(MAX_PAIRS)) {
    local({
      pp <- p

      # Per-row machinery (both sides of the pair)
      for (side in c("a", "b")) {
        local({
          rid <- paste0(pp, side)

          sync_type_bounds("cb_", rid)

          output[[paste0("cb_badge_", rid)]] <- renderUI({
            r <- read_row(rid)
            integer <- isTRUE(input[[paste0("cb_int_", pp)]])
            res <- evaluate_row(
              r$x,
              r$sd,
              r$n,
              r$items,
              r$type,
              r$min,
              r$max,
              integer
            )
            if (!is.null(res$err)) {
              return(error_ui(res$err))
            }
            result_ui(
              res$ok,
              res$reasons,
              uninformative = isTRUE(res$uninformative),
              digits = res$grim_digits,
              notes = res$notes
            )
          })
        })
      }

      # Per-pair t-test recalculation result
      output[[paste0("cb_t_test_", pp)]] <- renderUI({
        t_test_result_ui(t_test_results[[pp]]())
      })

      # Per-pair removal (clears both rows + Variable + Reported p)
      observeEvent(
        input[[paste0("cb_rm_", pp)]],
        {
          current <- pairs()
          if (pp %in% current) {
            updateTextInput(session, paste0("cb_var_", pp), value = "")
            updateCheckboxInput(session, paste0("cb_int_", pp), value = FALSE)
            updateTextInput(session, paste0("cb_p_", pp), value = "")
            updateSelectInput(
              session,
              paste0("cb_pop_", pp),
              selected = "equals"
            )
            for (side in c("a", "b")) {
              rid <- paste0(pp, side)
              updateTextInput(session, paste0("cb_grp_", rid), value = "")
              updateTextInput(session, paste0("cb_x_", rid), value = "")
              updateTextInput(session, paste0("cb_sd_", rid), value = "")
              updateTextInput(session, paste0("cb_n_", rid), value = "")
              updateNumericInput(session, paste0("cb_items_", rid), value = 1)
              updateTextInput(session, paste0("cb_min_", rid), value = "")
              updateTextInput(session, paste0("cb_max_", rid), value = "")
              updateSelectInput(
                session,
                paste0("cb_type_", rid),
                selected = "Mean"
              )
            }
            pairs(setdiff(current, pp))
          }
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )
    })
  }

  output$combined_summary <- renderUI({
    s <- pairs()
    rids <- unlist(lapply(s, function(p) paste0(p, c("a", "b"))))
    results <- vapply(
      rids,
      function(rid) row_result_from_inputs(rid)$ok,
      logical(1)
    )
    summary_bar(results)
  })

  output$t_test_summary <- renderUI({
    s <- pairs()
    tts <- lapply(s, function(p) t_test_results[[p]]())
    t_test_summary_bar(tts)
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      paste0(
        "grim-grimmer-t-test-",
        format(Sys.time(), "%Y%m%d-%H%M%S"),
        ".csv"
      )
    },
    content = function(file) {
      s <- pairs()
      pair_counter <- 0L
      rows <- lapply(s, function(p) {
        tt <- t_test_results[[p]]()
        variable <- input[[paste0("cb_var_", p)]]
        integer <- isTRUE(input[[paste0("cb_int_", p)]])
        p_str <- input[[paste0("cb_p_", p)]]
        pop <- input[[paste0("cb_pop_", p)]]

        # A pair contributes rows only if at least one side has a mean. When the
        # Variable / Group cells are left blank, fall back to incremental values:
        # Variable is numbered per emitted pair, Group 1/2 within the pair.
        has_data <- function(side) {
          xv <- input[[paste0("cb_x_", p, side)]]
          !is.null(xv) && nzchar(trimws(xv))
        }
        if (!has_data("a") && !has_data("b")) {
          return(NULL)
        }
        pair_counter <<- pair_counter + 1L
        # The pair-level fields (label, reported p, t-test result) go on the
        # pair's first emitted row, which is the second group's row if the
        # first group has no mean.
        first_side <- if (has_data("a")) "a" else "b"
        var_val <- if (!is.null(variable) && nzchar(trimws(variable))) {
          trimws(variable)
        } else {
          as.character(pair_counter)
        }

        per_side <- lapply(c("a", "b"), function(side) {
          rid <- paste0(p, side)
          r <- read_row(rid)
          group <- input[[paste0("cb_grp_", rid)]]
          if (is.null(r$x) || !nzchar(trimws(r$x))) {
            return(NULL)
          }
          group_val <- if (!is.null(group) && nzchar(trimws(group))) {
            trimws(group)
          } else if (side == "a") {
            "1"
          } else {
            "2"
          }
          sd_given <- !is.null(r$sd) && nzchar(trimws(r$sd))
          min_given <- !is.null(r$min) && nzchar(trimws(r$min))
          max_given <- !is.null(r$max) && nzchar(trimws(r$max))
          # fmt: skip
          res <- evaluate_row(
            r$x, r$sd, r$n, r$items, r$type, r$min, r$max, integer
          )
          test_label <- if (length(res$tests_run) == 0) {
            ""
          } else {
            paste(res$tests_run, collapse = "+")
          }
          inconsistency <- if (!is.null(res$err)) {
            res$err
          } else if (!is.na(res$ok) && !res$ok) {
            paste(res$reasons, collapse = "; ")
          } else {
            ""
          }
          note_parts <- res$notes
          if (isTRUE(res$uninformative)) {
            note_parts <- c(
              note_parts,
              paste(
                "Uninformative GRIM: every possible mean is achievable for this N",
                "and item count."
              )
            )
          }
          notes <- paste(note_parts, collapse = "; ")
          # t-test fields only on the first row of the pair
          is_first <- side == first_side
          tt_ok <- identical(tt$status, "ok")
          data.frame(
            label = if (is_first) var_val else "",
            integer_data = integer,
            group = group_val,
            type = if (is.null(r$type)) "Mean" else r$type,
            mean = trimws(r$x),
            sd = if (sd_given) trimws(r$sd) else "",
            n = if (!is.null(r$n)) trimws(r$n) else "",
            items = if (!is.null(r$items) && !is.na(r$items)) {
              r$items
            } else {
              NA_real_
            },
            min = if (min_given) trimws(r$min) else "",
            max = if (max_given) trimws(r$max) else "",
            test = test_label,
            consistent = res$ok,
            inconsistency = inconsistency,
            p_operator = if (
              is_first && !is.null(p_str) && nzchar(trimws(p_str))
            ) {
              operator_symbol(pop)
            } else {
              ""
            },
            reported_p = if (
              is_first && !is.null(p_str) && nzchar(trimws(p_str))
            ) {
              trimws(p_str)
            } else {
              ""
            },
            recalc_p_min = if (is_first && tt_ok) tt$min_p else NA_real_,
            recalc_p_max = if (is_first && tt_ok) tt$max_p else NA_real_,
            p_reproduces = if (is_first && tt_ok && isTRUE(tt$p_given)) {
              tt$inbounds
            } else {
              NA
            },
            p_note = if (is_first && identical(tt$status, "error")) {
              tt$msg
            } else if (is_first && tt_ok && isTRUE(tt$mixed_digits)) {
              MIXED_DIGITS_NOTE
            } else {
              ""
            },
            notes = notes,
            stringsAsFactors = FALSE
          )
        })
        per_side <- Filter(Negate(is.null), per_side)
        if (length(per_side) == 0) {
          return(NULL)
        }
        do.call(rbind, per_side)
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0) {
        df <- data.frame(
          label = character(),
          integer_data = logical(),
          group = character(),
          type = character(),
          mean = character(),
          sd = character(),
          n = character(),
          items = numeric(),
          min = character(),
          max = character(),
          test = character(),
          consistent = logical(),
          inconsistency = character(),
          p_operator = character(),
          reported_p = character(),
          recalc_p_min = numeric(),
          recalc_p_max = numeric(),
          p_reproduces = logical(),
          p_note = character(),
          notes = character(),
          stringsAsFactors = FALSE
        )
      } else {
        df <- do.call(rbind, rows)
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
