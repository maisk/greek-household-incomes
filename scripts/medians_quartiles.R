# Medians and quartiles of equivalised disposable income, for the whole
# population, persons under 65 and persons 18-64. Writes CSVs to data/derived/
# that doc/medians_quartiles.qmd reads.
#   Rscript scripts/medians_quartiles.R [survey_year]
#
# Outputs (survey_year = SILC wave; income_year = survey_year - 1):
#   medians_by_age.csv      real median and mean per group and year (ilc_di03)
#   quartiles_total.csv     real P25/P50/P75 of the whole population per year (ilc_di01),
#                           with the ELSTAT figures alongside where available
#   quartiles_age.csv       P25/P50/P75 per group for survey_year: model estimate, real value
#                           where published, and bounds implied by published CDF points
#   household_examples.csv  equivalised values converted to household income

source("R/income_distribution.R")

args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args)) as.integer(args[1]) else 2025L

out_dir <- "data/derived"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

groups <- c(TOTAL = "Όλος ο πληθυσμός", Y_LT65 = "Κάτω των 65", `Y18-64` = "18–64")

# ---- Medians and means by age (real) ------------------------------------------

di03 <- read.csv("data/eurostat/ilc_di03.csv")
di03 <- di03[di03$sex == "T" & di03$unit == "EUR" & di03$age %in% names(groups) & di03$time >= 2015, ]
med <- merge(
  setNames(di03[di03$statinfo == "MED_EI", c("time", "age", "value")], c("survey_year", "age", "median")),
  setNames(di03[di03$statinfo == "MEAN_EI", c("time", "age", "value")], c("survey_year", "age", "mean"))
)
med$income_year <- med$survey_year - 1
med$group <- unname(groups[med$age])
tot <- med[med$age == "TOTAL", c("survey_year", "median")]
med$median_vs_total_pct <- 100 * (med$median / tot$median[match(med$survey_year, tot$survey_year)] - 1)
med <- med[order(med$survey_year, match(med$age, names(groups))),
           c("survey_year", "income_year", "age", "group", "median", "mean", "median_vs_total_pct")]
write.csv(med, file.path(out_dir, "medians_by_age.csv"), row.names = FALSE)

# ---- Quartiles of the whole population (real) --------------------------------

di01 <- read.csv("data/eurostat/ilc_di01.csv")
di01 <- di01[di01$unit == "EUR" & di01$statinfo == "TC" & di01$quant_inc %in% c("Q1", "Q2", "Q3") &
             di01$time >= 2015, ]
qt <- reshape(di01[, c("time", "quant_inc", "value")], idvar = "time", timevar = "quant_inc",
              direction = "wide")
names(qt) <- c("survey_year", "p25", "p50", "p75")
qt$income_year <- qt$survey_year - 1
qt$p75_p25 <- qt$p75 / qt$p25

el <- read_silc()$groups
el <- el[el$group_type == "quartile" & !is.na(el$upper_limit_eur), ]
for (k in 1:3) {
  e <- el[el$group == k, ]
  qt[[paste0("elstat_p", 25 * k)]] <- e$upper_limit_eur[match(qt$survey_year, e$survey_year)]
}
qt <- qt[order(qt$survey_year), c("survey_year", "income_year", "p25", "p50", "p75", "p75_p25",
                                  "elstat_p25", "elstat_p50", "elstat_p75")]
write.csv(qt, file.path(out_dir, "quartiles_total.csv"), row.names = FALSE)

# ---- Quartiles by group for one year: model, real, bounds ---------------------

# A published CDF point (p, x) says a share p of the group has income below x.
# Since the CDF is increasing, for quantile u every point with p < u gives a
# lower bound Q(u) > x and every point with p > u an upper bound Q(u) < x.
bounds <- function(points, u) {
  lo <- points[points$p < u, ]; hi <- points[points$p > u, ]
  lo <- lo[which.max(lo$x), ]; hi <- hi[which.min(hi$x), ]
  data.frame(lower = if (nrow(lo)) lo$x else NA, lower_share_below = if (nrow(lo)) lo$p else NA,
             lower_source = if (nrow(lo)) lo$src else NA,
             upper = if (nrow(hi)) hi$x else NA, upper_share_below = if (nrow(hi)) hi$p else NA,
             upper_source = if (nrow(hi)) hi$src else NA)
}

fits <- list(TOTAL = fit_dagum(silc_targets(read_silc(), year)))
for (g in c("Y_LT65", "Y18-64")) fits[[g]] <- fit_dagum(eurostat_age_targets(g, year))

qt_y <- qt[qt$survey_year == year, ]
rows <- list()
for (g in names(groups)) {
  f <- fits[[g]]; a <- f$par[["a"]]; p <- f$par[["p"]]; b <- f$par[["b"]]
  pts <- if (g == "TOTAL") NULL else f$targets$quantiles
  for (u in c(0.25, 0.5, 0.75)) {
    real <- if (g == "TOTAL") qt_y[[sprintf("p%d", 100 * u)]]
            else if (u == 0.5) med$median[med$survey_year == year & med$age == g]
            else NA
    bd <- if (is.null(pts)) bounds(data.frame(p = numeric(0), x = numeric(0), src = character(0)), u)
          else bounds(pts[pts$p != u, ], u)
    rows[[length(rows) + 1]] <- data.frame(
      survey_year = year, income_year = year - 1, age = g, group = unname(groups[g]),
      quantile = sprintf("P%d", 100 * u), model = qdagum(u, a, p, b), real = real,
      model_vs_real_pct = if (is.na(real)) NA else 100 * (qdagum(u, a, p, b) / real - 1),
      bd)
  }
}
qa <- do.call(rbind, rows)
write.csv(qa, file.path(out_dir, "quartiles_age.csv"), row.names = FALSE)

# ---- Household income examples ------------------------------------------------

ex <- rbind(
  data.frame(statistic = "P25 συνόλου", equivalised = qt_y$p25),
  data.frame(statistic = "Διάμεσος συνόλου", equivalised = qt_y$p50),
  data.frame(statistic = "Διάμεσος 18–64", equivalised = med$median[med$survey_year == year & med$age == "Y18-64"]),
  data.frame(statistic = "P75 συνόλου", equivalised = qt_y$p75)
)
ex$survey_year <- year
ex$one_adult     <- ex$equivalised * oecd_scale(adults = 1, children_under14 = 0)
ex$two_adults    <- ex$equivalised * oecd_scale(adults = 2, children_under14 = 0)
ex$couple_1child <- ex$equivalised * oecd_scale(adults = 2, children_under14 = 1)
write.csv(ex, file.path(out_dir, "household_examples.csv"), row.names = FALSE)

message("wrote ", paste(file.path(out_dir, c("medians_by_age.csv", "quartiles_total.csv",
                                             "quartiles_age.csv", "household_examples.csv")),
                        collapse = ", "))
