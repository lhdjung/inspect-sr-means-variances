# recalc: an SD of 0 narrows the recalculated p range

Draft issue for <https://github.com/ianhussey/recalc>. Found on 2026-10-01
while fuzzing this app's t-test recalculation; written up on 2026-10-02.

## Summary

`recalc_independent_t_p()` skips every candidate in its grid where either SD
is exactly 0. When a reported SD is 0 (e.g. "0.0", or "0" at 0 decimals), both
the "reported" and the "lower" candidate are 0, so only the upper end of that
SD's rounding interval is evaluated. The recalculated `min_p` is then too
large, and a correct reported p can come back as `p_inbounds = FALSE`.

Affected: recalc 0.6 as installed here (commit `02871a2`), and still present
on `main` as of commit `ab90a04` (2026-07-27), in `R/recalc_independent_t.R`
lines 1110 and 1230:

```r
if (sd1_eff <= 0 || sd2_eff <= 0) {
  next
}
```

## Is it a bug?

The guard itself looks deliberate: with both SDs at 0 the standard error is 0
and t is undefined. But it also fires when only one SD is 0, where both tests
are well defined, and the consequence is a silently narrower range rather than
an error or a warning. That makes the result wrong in the direction that
matters most for an error-detection tool: a false "not reproduced".

So: intended guard, unintended effect. The both-zero case needs the guard; the
one-zero case does not.

## Reproducible example

```r
library(recalc)

# Group 1's SD is reported as 0 at 0 decimals: the true SD is in [0, 0.5].
r <- recalc_independent_t_p(
  m1 = 0.92, m2 = 1.86, sd1 = 0, sd2 = 3, n1 = 6, n2 = 117,
  m_digits = 2, sd_digits = 0, p = 0.002, p_digits = 3
)
r$reproduced[c("p", "min_p", "max_p", "p_inbounds")]
#>       p       min_p     max_p p_inbounds
#> 1 0.002 0.005068778 0.5181851      FALSE

# Only the upper end of sd1's interval is ever evaluated:
unique(sub(".*\\|(sd1:[a-z]+)\\|.*", "\\1", r$p_results$input_adj_stats))
#> [1] "sd1:upper"

# Yet p = 0.002 is possible. True SDs of 0.4 and 2.567 round to 0 and 3, and
# Welch's test then gives:
s1 <- 0.4; s2 <- 2.567
se <- sqrt(s1^2 / 6 + s2^2 / 117)
df <- se^4 / ((s1^2 / 6)^2 / 5 + (s2^2 / 117)^2 / 116)
2 * pt(-abs(0.92 - 1.864) / se, df)
#> [1] 0.002153805
```

With `sd1` at the true lower end (0) and `sd2 = 2.5`, Welch's p is about
7.4e-05, so the correct `min_p` is far below the 0.0051 returned.

An SD of exactly 0 is also a legitimate input in its own right. `t.test()`
handles a group of identical values as long as the other group varies:

```r
t.test(c(1, 1, 1, 1, 1, 1), c(1, 3, 2, 5, 4, 2, 3))$p.value
#> [1] 0.0106721
```

## Suggested fix

Skip a candidate only when the test is undefined, i.e. when both SDs are 0:

```r
if (sd1_eff < 0 || sd2_eff < 0 || (sd1_eff == 0 && sd2_eff == 0)) {
  next
}
```

With one SD at 0, the pooled SD and Welch's standard error are both positive,
and Welch's df reduces to the other group's `n - 1`. I have not tested this
change inside recalc. The same guard at line 1230 probably needs the same
treatment, and other `recalc_*` functions built on the same grid may share it;
I only checked `recalc_independent_t_p()`.

## How often it matters

Rarely. It needs an SD that is 0 at the reported precision *and* a reported p
near the low end of the range. In a simulation of genuine data (Welch's test,
p reported to 3 decimals):

- 1 of 1,500 cases with mixed data types and precisions was wrongly flagged;
- 1 of 380 cases was wrongly flagged when one group's SD was forced to round
  to 0.

## What this app does about it

`evaluate_pair_t_test()` in `app.R` replaces a zero SD with `1e-8` before
calling recalc, so the lower end of the interval is evaluated. `tests.R` has a
regression test for it. The workaround can go once recalc evaluates SD = 0.

This app reaches the zero-SD case more often than a direct user of recalc
would: when the two groups' SDs are reported to different precisions it
re-expresses the finer one at the coarser precision, which turns e.g. "0.4"
into 0 at 0 decimals.
