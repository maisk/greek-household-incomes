# Parametric model of the Greek equivalised disposable income distribution,
# fitted to ELSTAT SILC group data (quantile limits + income shares + Gini).
#
# Distribution: Dagum (Burr III) with shape a, shape p, scale b.
#   F(x) = (1 + (x/b)^-a)^-p
#   Q(u) = b * (u^(-1/p) - 1)^(-1/a)
# Its upper tail is Pareto with index a, while unlike a pure Pareto it also
# reproduces the body of the distribution (a pure Pareto fitted to Gini 31.6%
# puts the median at ~8.9k€ instead of the observed 11.7k€).
# Base R only.

# ---- Dagum distribution -----------------------------------------------------

qdagum <- function(u, a, p, b) b * (u^(-1 / p) - 1)^(-1 / a)

pdagum <- function(x, a, p, b) (1 + (x / b)^(-a))^(-p)

ddagum <- function(x, a, p, b) {
  a * p / x * (x / b)^(a * p) / (1 + (x / b)^a)^(p + 1)
}

rdagum <- function(n, a, p, b) qdagum(runif(n), a, p, b)

# Mean exists for a > 1.
mean_dagum <- function(a, p, b) {
  b * gamma(p + 1 / a) * gamma(1 - 1 / a) / gamma(p)
}

# Lorenz curve: L(u) = I_{u^(1/p)}(p + 1/a, 1 - 1/a)
lorenz_dagum <- function(u, a, p) pbeta(u^(1 / p), p + 1 / a, 1 - 1 / a)

gini_dagum <- function(a, p) {
  exp(lgamma(p) + lgamma(2 * p + 1 / a) - lgamma(2 * p) - lgamma(p + 1 / a)) - 1
}

# Income share of each of k equal population groups (k = 5 quintiles, 4 quartiles, ...).
group_shares_dagum <- function(k, a, p) diff(lorenz_dagum(seq(0, 1, length.out = k + 1), a, p))

# ---- Income concentration from any Lorenz curve ---------------------------------

# Piecewise-linear Lorenz curve through the points of equal-size groups with the
# given income shares (e.g. 10 decile shares), normalised to sum to 1.
lorenz_from_shares <- function(shares) {
  approxfun(seq(0, 1, length.out = length(shares) + 1), c(0, cumsum(shares)) / sum(shares))
}

# Half split: the poorest x hold half of all income (and the richest 1 - x the other half).
half_split <- function(L) uniroot(function(u) L(u) - 0.5, c(1e-6, 1 - 1e-6))$root

# ---- Data -------------------------------------------------------------------

read_silc <- function(dir = "data") {
  list(
    groups     = read.csv(file.path(dir, "elstat_silc_groups.csv")),
    indicators = read.csv(file.path(dir, "elstat_silc_indicators.csv"))
  )
}

# Targets for one survey year: quantile points (p, x), group shares, Gini.
silc_targets <- function(silc, year) {
  g <- silc$groups[silc$groups$survey_year == year, ]
  if (nrow(g) == 0) stop("no SILC group data for survey year ", year)
  g$k <- ifelse(g$group_type == "quintile", 5, 4)
  lim <- g[!is.na(g$upper_limit_eur), ]
  q <- unique(data.frame(p = lim$group / lim$k, x = lim$upper_limit_eur))
  q <- q[order(q$p), ]
  list(
    year      = year,
    quantiles = q,
    shares    = split(g$share_pct / 100, g$group_type),
    gini      = silc$indicators$gini_pct[silc$indicators$survey_year == year] / 100
  )
}

# Targets for one age group from Eurostat tables (data/eurostat/, see
# scripts/fetch_eurostat.R). Eurostat publishes no quantile cut-offs or shares
# by age, so the distribution is pinned down by:
#   - ilc_li02: % of the group below 40/50/60/70% of the *national* median and
#     40/50/60% of the national mean  -> points (p, x) of the group's CDF
#   - ilc_di20: % of the group at or above 130-160% of the national median/mean
#     (published for Y18-64, not for Y_LT65)            -> more CDF points
#   - ilc_di03: the group's own median (a CDF point) and mean
#   - ilc_di11/11d/11e: S80/S20, S80/S50, S50/S20 where "S50" is the share of the
#     middle quintile (published for Y_LT65, not for Y18-64)
# The national thresholds come from ilc_di03 for age TOTAL.
eurostat_age_targets <- function(age, year, dir = "data/eurostat") {
  rd <- function(f) {
    d <- read.csv(file.path(dir, paste0(f, ".csv")))
    keep_sex <- if (is.null(d$sex)) TRUE else d$sex == "T"
    d[d$time == year & keep_sex, , drop = FALSE]
  }
  di03 <- rd("ilc_di03"); di03 <- di03[di03$unit == "EUR", ]
  nat  <- function(stat) di03$value[di03$age == "TOTAL" & di03$statinfo == stat]
  own  <- function(stat) di03$value[di03$age == age & di03$statinfo == stat]
  if (!length(own("MED_EI"))) stop("no ilc_di03 data for age ", age, " in ", year)
  ref <- c(MED_EI = nat("MED_EI"), MEAN_EI = nat("MEAN_EI"),
           MED = nat("MED_EI"), MEAN = nat("MEAN_EI"))

  li <- rd("ilc_li02"); li <- li[li$unit == "PC" & li$age == age & grepl("^B_", li$rskpovth), ]
  cdf_points <- function(p, pct, stat, label) {
    if (!length(p)) return(NULL)
    data.frame(p = p, x = as.numeric(pct) / 100 * unname(ref[stat]),
               src = paste0(label, pct, "% ", stat))
  }
  q_li <- cdf_points(li$value / 100, sub("B_", "", li$rskpovth), li$statinfo, "ilc_li02 <")
  di20 <- rd("ilc_di20"); di20 <- di20[di20$unit == "PC" & di20$age == age, ]
  q_hi <- cdf_points(1 - di20$value / 100, sub("PC_GE", "", di20$income), di20$statinfo, "ilc_di20 >=")
  q_med <- data.frame(p = 0.5, x = own("MED_EI"), src = "ilc_di03 median")
  q <- rbind(q_li, q_hi, q_med)
  q <- q[order(q$p), ]
  rownames(q) <- NULL

  ratio <- function(f) { d <- rd(f); d$value[d$age == age] }
  ratios <- c(S80_S20 = ratio("ilc_di11"), S80_S50 = ratio("ilc_di11d"), S50_S20 = ratio("ilc_di11e"))
  ratios <- ratios[!is.na(ratios)]

  list(year = year, group = age, quantiles = q, shares = list(), gini = NULL,
       mean = own("MEAN_EI"), ratios = ratios)
}

# Targets for one household type (Eurostat hhcomp code, e.g. "A2_2LT65" = two
# adults both under 65 without dependent children, "A2_DCH1" = two adults with
# one dependent child). Same idea as eurostat_age_targets(), from:
#   - ilc_li03: % of persons in that household type below 40/50/60/70% of the
#     national median and 40/50/60% of the national mean -> CDF points
#   - ilc_di04: the type's own median (a CDF point) and mean
# Eurostat publishes nothing above the median by household type.
eurostat_hhcomp_targets <- function(hhcomp, year, dir = "data/eurostat") {
  rd <- function(f) {
    d <- read.csv(file.path(dir, paste0(f, ".csv")))
    d[d$time == year, , drop = FALSE]
  }
  di03 <- rd("ilc_di03"); di03 <- di03[di03$unit == "EUR" & di03$sex == "T" & di03$age == "TOTAL", ]
  ref  <- c(MED_EI = di03$value[di03$statinfo == "MED_EI"], MEAN_EI = di03$value[di03$statinfo == "MEAN_EI"])
  di04 <- rd("ilc_di04"); di04 <- di04[di04$unit == "EUR" & di04$hhcomp == hhcomp, ]
  own  <- function(stat) di04$value[di04$statinfo == stat]
  if (!length(own("MED_EI"))) stop("no ilc_di04 median for household type ", hhcomp, " in ", year)

  li <- rd("ilc_li03"); li <- li[li$unit == "PC" & li$hhcomp == hhcomp & grepl("^B_", li$rskpovth), ]
  pct <- sub("B_", "", li$rskpovth)
  q <- rbind(
    data.frame(p = li$value / 100, x = as.numeric(pct) / 100 * unname(ref[li$statinfo]),
               src = paste0("ilc_li03 <", pct, "% ", li$statinfo)),
    data.frame(p = 0.5, x = own("MED_EI"), src = "ilc_di04 median"))
  q <- q[order(q$p), ]
  rownames(q) <- NULL

  list(year = year, group = hhcomp, quantiles = q, shares = list(), gini = NULL,
       mean = own("MEAN_EI"), ratios = numeric(0))
}

# Household types analysed, with the equivalence scale of each variant.
# "Dependent child" (Eurostat) = under 18, or 18-24 and economically inactive
# living with a parent, so A2_DCH1 mixes young children and students.
household_types <- data.frame(
  hhcomp = c("A2_2LT65", "A2_DCH1"),
  slug   = c("couple_lt65", "couple_1child"),
  label  = c("Ζευγάρι <65 χωρίς παιδιά", "Ζευγάρι με 1 παιδί"),
  who    = c("άτομα σε νοικοκυριά 2 ενηλίκων, και οι δύο κάτω των 65, χωρίς εξαρτώμενα παιδιά",
             "άτομα σε νοικοκυριά 2 ενηλίκων με 1 εξαρτώμενο παιδί (κάτω των 18, ή 18–24 χωρίς εργασία)")
)

# Quintile share ratios implied by the Lorenz curve. "S50" is the share of the
# middle (3rd) quintile, as in Eurostat ilc_di11d/ilc_di11e.
share_ratios_dagum <- function(a, p) {
  L <- lorenz_dagum(c(0.2, 0.4, 0.6, 0.8), a, p)
  q1 <- L[1]; q3 <- L[3] - L[2]; q5 <- 1 - L[4]
  c(S80_S20 = q5 / q1, S80_S50 = q5 / q3, S50_S20 = q3 / q1)
}

# ---- Fitting ----------------------------------------------------------------

# Least squares on the log scale over all targets, each equally weighted per
# observation: quantile points, group shares, and when present Gini, mean and
# quintile share ratios.
fit_dagum <- function(targets) {
  q <- targets$quantiles
  loss <- function(th) {
    a <- exp(th[1]); p <- exp(th[2]); b <- exp(th[3])
    if (a <= 1) return(Inf)
    err <- log(qdagum(q$p, a, p, b)) - log(q$x)
    for (s in targets$shares) {
      err <- c(err, log(group_shares_dagum(length(s), a, p)) - log(s))
    }
    if (length(targets$gini)) err <- c(err, log(gini_dagum(a, p)) - log(targets$gini))
    if (length(targets$mean)) err <- c(err, log(mean_dagum(a, p, b)) - log(targets$mean))
    if (length(targets$ratios)) {
      err <- c(err, log(share_ratios_dagum(a, p)[names(targets$ratios)]) - log(targets$ratios))
    }
    sum(err^2)
  }
  start <- c(log(3), log(1), log(approx(q$p, q$x, 0.5, rule = 2, ties = mean)$y))
  opt <- optim(start, loss, control = list(maxit = 5000, reltol = 1e-12))
  if (opt$convergence != 0) warning("optim did not converge (code ", opt$convergence, ")")
  par <- setNames(exp(opt$par), c("a", "p", "b"))
  structure(list(par = par, targets = targets, loss = opt$value), class = "dagum_fit")
}

# Fitted vs observed, for checking the fit.
compare_fit <- function(fit) {
  a <- fit$par[["a"]]; p <- fit$par[["p"]]; b <- fit$par[["b"]]
  t <- fit$targets
  rows <- data.frame(
    target   = if (is.null(t$quantiles$src)) sprintf("Q%02d", round(100 * t$quantiles$p))
               else sprintf("P%04.1f %s", 100 * t$quantiles$p, t$quantiles$src),
    observed = t$quantiles$x,
    fitted   = qdagum(t$quantiles$p, a, p, b)
  )
  for (type in names(t$shares)) {
    s <- t$shares[[type]]
    rows <- rbind(rows, data.frame(
      target   = sprintf("%s %d share %%", type, seq_along(s)),
      observed = 100 * s,
      fitted   = 100 * group_shares_dagum(length(s), a, p)
    ))
  }
  if (length(t$gini)) {
    rows <- rbind(rows, data.frame(target = "Gini %", observed = 100 * t$gini,
                                   fitted = 100 * gini_dagum(a, p)))
  }
  if (length(t$mean)) {
    rows <- rbind(rows, data.frame(target = "mean", observed = t$mean, fitted = mean_dagum(a, p, b)))
  }
  if (length(t$ratios)) {
    rows <- rbind(rows, data.frame(target = names(t$ratios), observed = unname(t$ratios),
                                   fitted = unname(share_ratios_dagum(a, p)[names(t$ratios)])))
  }
  rows$rel_err_pct <- 100 * (rows$fitted / rows$observed - 1)
  rows
}

print.dagum_fit <- function(x, ...) {
  group <- if (is.null(x$targets$group)) "" else paste0(", group ", x$targets$group)
  cat(sprintf("Dagum fit, SILC %d (income %d)%s: a = %.3f, p = %.3f, b = %.0f EUR\n",
              x$targets$year, x$targets$year - 1, group, x$par[["a"]], x$par[["p"]], x$par[["b"]]))
  cat(sprintf("mean = %.0f EUR, median = %.0f EUR, Gini = %.3f\n",
              mean_dagum(x$par[["a"]], x$par[["p"]], x$par[["b"]]),
              qdagum(0.5, x$par[["a"]], x$par[["p"]], x$par[["b"]]),
              gini_dagum(x$par[["a"]], x$par[["p"]])))
  invisible(x)
}

# ---- Equivalence scale ------------------------------------------------------

# Modified OECD scale used by ELSTAT/Eurostat.
oecd_scale <- function(adults = 2, children_14plus = 0, children_under14 = 1) {
  1 + 0.5 * (adults - 1 + children_14plus) + 0.3 * children_under14
}

# Equivalised income -> total household disposable income.
household_income <- function(eq_income, ...) eq_income * oecd_scale(...)
