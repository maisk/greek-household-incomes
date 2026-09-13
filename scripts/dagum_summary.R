# Numbers behind doc/dagum_2025.qmd: the Dagum fit of the whole-population
# distribution, its check against published figures, and the comparison with
# Pareto and lognormal. Writes CSVs to data/derived/.
#   Rscript scripts/dagum_summary.R [survey_year]
#
# Outputs (survey_year = SILC wave; income_year = survey_year - 1):
#   dagum_by_year.csv          Dagum fit per year with model and real mean/median
#   dagum_fit_check.csv        survey_year: every fitted target and every out-of-sample
#                              Eurostat cut-off, observed vs fitted
#   dagum_model_comparison.csv survey_year: Pareto, lognormal and Dagum fitted to the same targets
#   group_cutoffs_check.csv    survey_year: ELSTAT vs Eurostat cut-offs (quintiles, or quartiles
#                              for years where ELSTAT publishes no quintiles)
#   real_mean_median.csv       survey_year: real mean/median for total and children (ilc_di03)
#   income_concentration.csv   survey_year: "the poorest a% hold as much as the richest b%" and the
#                              split of the population into two halves of total income
#   lorenz_points.csv          survey_year: model Lorenz curve and ELSTAT Lorenz points

source("R/income_distribution.R")

args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args)) as.integer(args[1]) else 2025L

out_dir <- "data/derived"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

silc <- read_silc()
di03 <- read.csv("data/eurostat/ilc_di03.csv")
di03 <- di03[di03$sex == "T" & di03$unit == "EUR", ]
real <- function(y, age, stat) {
  v <- di03$value[di03$time == y & di03$age == age & di03$statinfo == stat]
  if (length(v)) v else NA
}
di01 <- read.csv("data/eurostat/ilc_di01.csv")
di01 <- di01[di01$unit == "EUR" & di01$statinfo == "TC", ]

# ---- Dagum fit per year ------------------------------------------------------

years <- sort(unique(silc$groups$survey_year))
by_year <- do.call(rbind, lapply(years, function(y) {
  f <- fit_dagum(silc_targets(silc, y)); a <- f$par[["a"]]; p <- f$par[["p"]]; b <- f$par[["b"]]
  sh <- group_shares_dagum(5, a, p)
  el <- silc$groups[silc$groups$survey_year == y & silc$groups$group_type == "quartile" &
                    silc$groups$group == 2, ]
  data.frame(survey_year = y, income_year = y - 1, a = a, p = p, b = b,
             n_targets = nrow(f$targets$quantiles) + length(unlist(f$targets$shares)) + 1,
             mean_model = mean_dagum(a, p, b), median_model = qdagum(0.5, a, p, b),
             gini_model = gini_dagum(a, p), gini_elstat = f$targets$gini,
             p99_model = qdagum(0.99, a, p, b),
             share_below_mean_model = pdagum(mean_dagum(a, p, b), a, p, b),
             share_q1_model = sh[1], share_q5_model = sh[5],
             mean_real = real(y, "TOTAL", "MEAN_EI"), median_real = real(y, "TOTAL", "MED_EI"),
             median_elstat = el$upper_limit_eur)
}))
by_year$mean_err_pct   <- 100 * (by_year$mean_model / by_year$mean_real - 1)
by_year$median_err_pct <- 100 * (by_year$median_model / by_year$median_real - 1)
write.csv(by_year, file.path(out_dir, "dagum_by_year.csv"), row.names = FALSE)

# ---- Fit check for one year ----------------------------------------------------

fit <- fit_dagum(silc_targets(silc, year)); a <- fit$par[["a"]]; p <- fit$par[["p"]]; b <- fit$par[["b"]]
cf <- compare_fit(fit)
cf$kind <- ifelse(grepl("^Q", cf$target), "cutoff", ifelse(grepl("share", cf$target), "share", "gini"))
cf$source <- "ΕΛΣΤΑΤ (στόχος του fit)"

es <- di01[di01$time == year & grepl("^(D[1-9]|P[1-5]|P9[5-9])$", di01$quant_inc), ]
num <- as.numeric(sub("^[DP]", "", es$quant_inc))
pp  <- ifelse(grepl("^D", es$quant_inc), num / 10, num / 100)
oos <- data.frame(target = es$quant_inc, observed = es$value, fitted = qdagum(pp, a, p, b),
                  kind = "out_of_sample", source = "Eurostat ilc_di01 (εκτός fit)")
oos$rel_err_pct <- 100 * (oos$fitted / oos$observed - 1)
oos$p <- pp
# Cut-offs at a population share that is also an ELSTAT target (D2 = P20, D4 = P40, ...)
# are the same statistic, so they are not an out-of-sample check.
oos <- oos[!round(oos$p, 4) %in% round(fit$targets$quantiles$p, 4), ]
oos <- oos[order(oos$p), ]
# The mean is never a target; the median is, via the ELSTAT cut-off at the same share.
same_as_target <- c(FALSE, 0.5 %in% fit$targets$quantiles$p)
mm <- data.frame(target = c("mean", "median"),
                 observed = c(real(year, "TOTAL", "MEAN_EI"), real(year, "TOTAL", "MED_EI")),
                 fitted = c(mean_dagum(a, p, b), qdagum(0.5, a, p, b)),
                 kind = ifelse(same_as_target, "same_as_target", "out_of_sample"),
                 source = ifelse(same_as_target, "Eurostat ilc_di03 (ίδιο μέγεθος με στόχο της ΕΛΣΤΑΤ)",
                                 "Eurostat ilc_di03 (εκτός fit)"),
                 p = c(NA, 0.5))
mm$rel_err_pct <- 100 * (mm$fitted / mm$observed - 1)
cf$p <- NA
cols <- c("target", "kind", "source", "p", "observed", "fitted", "rel_err_pct")
check <- rbind(cf[, cols], oos[, cols], mm[, cols])
check$survey_year <- year
write.csv(check, file.path(out_dir, "dagum_fit_check.csv"), row.names = FALSE)

# ---- Pareto / lognormal / Dagum on the same targets ----------------------------

# Each model: quantile function Q(u, th), Lorenz curve L(u, th), Gini G(th),
# with parameters th on the log scale. Same loss as fit_dagum().
models <- list(
  Pareto = list(
    Q = function(u, th) exp(th[2]) * (1 - u)^(-1 / exp(th[1])),
    L = function(u, th) 1 - (1 - u)^(1 - 1 / exp(th[1])),
    G = function(th) 1 / (2 * exp(th[1]) - 1),
    start = c(log(2), log(5000)), valid = function(th) exp(th[1]) > 1),
  Lognormal = list(
    Q = function(u, th) exp(th[2] + exp(th[1]) * qnorm(u)),
    L = function(u, th) pnorm(qnorm(u) - exp(th[1])),
    G = function(th) 2 * pnorm(exp(th[1]) / sqrt(2)) - 1,
    start = c(log(0.5), log(11000)), valid = function(th) TRUE),
  Dagum = list(
    Q = function(u, th) qdagum(u, exp(th[1]), exp(th[2]), exp(th[3])),
    L = function(u, th) lorenz_dagum(u, exp(th[1]), exp(th[2])),
    G = function(th) gini_dagum(exp(th[1]), exp(th[2])),
    start = c(log(3), 0, log(11000)), valid = function(th) exp(th[1]) > 1)
)
tg <- fit$targets
comparison <- do.call(rbind, lapply(names(models), function(nm) {
  m <- models[[nm]]
  shares <- function(k, th) diff(m$L(seq(0, 1, length.out = k + 1), th))
  loss <- function(th) {
    if (!m$valid(th)) return(Inf)
    err <- log(m$Q(tg$quantiles$p, th)) - log(tg$quantiles$x)
    for (s in tg$shares) err <- c(err, log(shares(length(s), th)) - log(s))
    sum(c(err, log(m$G(th)) - log(tg$gini))^2)
  }
  o <- optim(m$start, loss, control = list(maxit = 5000, reltol = 1e-12))
  q_err <- 100 * (m$Q(tg$quantiles$p, o$par) / tg$quantiles$x - 1)
  sh <- shares(5, o$par)
  data.frame(survey_year = year, model = nm, n_params = length(m$start), loss = o$value,
             median = m$Q(0.5, o$par), gini = m$G(o$par),
             share_q1 = sh[1], share_q2 = sh[2], share_q3 = sh[3], share_q4 = sh[4], share_q5 = sh[5],
             max_abs_cutoff_err_pct = max(abs(q_err)))
}))
# ELSTAT publishes quintile shares only from the 2023 survey on.
obs_sh <- if (is.null(tg$shares$quintile)) rep(NA_real_, 5) else tg$shares$quintile
comparison <- rbind(comparison, data.frame(
  survey_year = year, model = "ΕΛΣΤΑΤ", n_params = NA, loss = NA,
  median = tg$quantiles$x[tg$quantiles$p == 0.5], gini = tg$gini,
  share_q1 = obs_sh[1], share_q2 = obs_sh[2], share_q3 = obs_sh[3], share_q4 = obs_sh[4],
  share_q5 = obs_sh[5], max_abs_cutoff_err_pct = NA))
write.csv(comparison, file.path(out_dir, "dagum_model_comparison.csv"), row.names = FALSE)

# ---- ELSTAT vs Eurostat group cut-offs ---------------------------------------

g_year <- silc$groups[silc$groups$survey_year == year & !is.na(silc$groups$upper_limit_eur), ]
gt <- if ("quintile" %in% g_year$group_type) "quintile" else "quartile"
el_g <- g_year[g_year$group_type == gt, ]
es_code <- paste0(if (gt == "quintile") "QU" else "Q", el_g$group)
es_g <- di01[di01$time == year & di01$quant_inc %in% es_code, ]
qc <- data.frame(survey_year = rep(year, nrow(el_g)), group_type = rep(gt, nrow(el_g)),
                 group = el_g$group, p = el_g$group / if (gt == "quintile") 5 else 4,
                 elstat = el_g$upper_limit_eur,
                 eurostat = es_g$value[match(es_code, es_g$quant_inc)])
write.csv(qc, file.path(out_dir, "group_cutoffs_check.csv"), row.names = FALSE)

# ---- Income concentration ------------------------------------------------------

# L: Lorenz curve (share of total income held by the poorest u of the population).
# "Equal totals": the poorest a hold as much as the richest b, i.e. L(a) = 1 - L(1 - b).
# Half split: the whole population in two groups with equal total income, L(x) = 1/2.
L_model <- function(u) lorenz_dagum(u, a, p)
el_type <- if (!is.null(tg$shares$quintile)) "quintile" else "quartile"
el_sh <- tg$shares[[el_type]]
el_x <- seq(0, 1, length.out = length(el_sh) + 1)
el_y <- c(0, cumsum(el_sh)) / sum(el_sh)
L_elstat <- lorenz_from_shares(el_sh)

equal_top <- function(L, bottom) uniroot(function(b) (1 - L(1 - b)) - L(bottom), c(1e-6, 1 - bottom))$root

conc <- list()
if (el_type == "quintile") {
  conc[[length(conc) + 1]] <- data.frame(
    kind = "equal_total_real", bottom_pop = 0.6, bottom_share = sum(el_sh[1:3]),
    top_pop = 0.2, top_share = el_sh[5], source = "ΕΛΣΤΑΤ, μερίδια πεμπτημορίων")
}
for (bottom in c(0.2, 0.4, 0.5, 0.6)) {
  top <- equal_top(L_model, bottom)
  conc[[length(conc) + 1]] <- data.frame(
    kind = "equal_total_model", bottom_pop = bottom, bottom_share = L_model(bottom),
    top_pop = top, top_share = 1 - L_model(1 - top), source = "μοντέλο Dagum")
}
for (nm in c("model", "elstat_linear")) {
  L <- if (nm == "model") L_model else L_elstat
  xh <- half_split(L)
  conc[[length(conc) + 1]] <- data.frame(
    kind = paste0("half_split_", nm), bottom_pop = xh, bottom_share = 0.5, top_pop = 1 - xh,
    top_share = 0.5,
    source = if (nm == "model") "μοντέλο Dagum" else paste0("ΕΛΣΤΑΤ, γραμμική παρεμβολή (", el_type, ")"))
}
conc <- do.call(rbind, conc)
conc$survey_year <- year
conc$top_to_bottom_ratio <- conc$top_share / conc$bottom_share
write.csv(conc[, c("survey_year", "kind", "bottom_pop", "bottom_share", "top_pop", "top_share", "source",
                   "top_to_bottom_ratio")],
          file.path(out_dir, "income_concentration.csv"), row.names = FALSE)

grid_u <- seq(0, 1, length.out = 201)
lorenz <- rbind(
  data.frame(survey_year = year, series = "model", pop_share = grid_u, income_share = L_model(grid_u)),
  data.frame(survey_year = year, series = "elstat", pop_share = el_x, income_share = el_y))
write.csv(lorenz, file.path(out_dir, "lorenz_points.csv"), row.names = FALSE)

# ---- Real mean / median for total and children -----------------------------

ages <- c(TOTAL = "Όλος ο πληθυσμός", Y_LT18 = "Παιδιά κάτω των 18", Y_LT6 = "Παιδιά κάτω των 6")
rm_ <- data.frame(survey_year = year, age = names(ages), group = unname(ages),
                  median = sapply(names(ages), function(g) real(year, g, "MED_EI")),
                  mean = sapply(names(ages), function(g) real(year, g, "MEAN_EI")))
write.csv(rm_, file.path(out_dir, "real_mean_median.csv"), row.names = FALSE)

message("wrote ", paste(file.path(out_dir, c("dagum_by_year.csv", "dagum_fit_check.csv",
                                             "dagum_model_comparison.csv", "group_cutoffs_check.csv",
                                             "real_mean_median.csv", "income_concentration.csv",
                                             "lorenz_points.csv")), collapse = ", "))
