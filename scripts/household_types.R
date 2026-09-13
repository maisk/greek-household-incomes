# Income distribution of two household types (couple under 65 without children,
# couple with one dependent child): Dagum fits, real medians, and what the
# median means in household income. Writes CSVs to data/derived/ for
# doc/household_types.qmd.
#   Rscript scripts/household_types.R [survey_year]
#
# Outputs (survey_year = SILC wave; income_year = survey_year - 1):
#   household_types.csv            per type: real and model median/mean, Dagum parameters, P25/P75
#   household_fit_check.csv        per type: every fitted target, observed vs fitted
#   household_income_at_median.csv per type and child-age variant: household net income of a
#                                  family exactly at the type's real median
#   household_medians_by_year.csv  real median and mean per year (ilc_di04) for the whole
#                                  population, the two types, and one adult living alone (A1,
#                                  any age, no dependent children; reported only, not fitted)

source("R/income_distribution.R")

args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args)) as.integer(args[1]) else 2025L

out_dir <- "data/derived"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Maximum yearly parental deposit that is matched 1:1 in the first five years by the
# child investment account announced by the government in September 2026.
match_cap <- 1200

fits <- lapply(household_types$hhcomp, function(h) fit_dagum(eurostat_hhcomp_targets(h, year)))
names(fits) <- household_types$hhcomp

types <- do.call(rbind, lapply(seq_len(nrow(household_types)), function(i) {
  f <- fits[[i]]; a <- f$par[["a"]]; p <- f$par[["p"]]; b <- f$par[["b"]]
  real_med <- f$targets$quantiles$x[f$targets$quantiles$src == "ilc_di04 median"]
  cf <- compare_fit(f)
  data.frame(survey_year = year, income_year = year - 1, household_types[i, ],
             median_real = real_med, mean_real = f$targets$mean,
             median_model = qdagum(0.5, a, p, b), mean_model = mean_dagum(a, p, b),
             p25_model = qdagum(0.25, a, p, b), p75_model = qdagum(0.75, a, p, b),
             p99_model = qdagum(0.99, a, p, b), gini_model = gini_dagum(a, p),
             a = a, p = p, b = b, n_targets = nrow(cf),
             max_abs_err_pct = max(abs(cf$rel_err_pct)))
}))
write.csv(types, file.path(out_dir, "household_types.csv"), row.names = FALSE)

check <- do.call(rbind, lapply(seq_len(nrow(household_types)), function(i) {
  f <- fits[[i]]; cf <- compare_fit(f)
  q <- f$targets$quantiles
  cf$p <- c(q$p, rep(NA, nrow(cf) - nrow(q)))
  data.frame(survey_year = year, hhcomp = household_types$hhcomp[i], label = household_types$label[i], cf)
}))
write.csv(check, file.path(out_dir, "household_fit_check.csv"), row.names = FALSE)

variants <- data.frame(
  hhcomp  = c("A2_2LT65", "A2_DCH1", "A2_DCH1"),
  variant = c("χωρίς παιδιά", "παιδί κάτω των 14", "παιδί 14–24"),
  scale   = c(oecd_scale(adults = 2, children_under14 = 0),
              oecd_scale(adults = 2, children_under14 = 1),
              oecd_scale(adults = 2, children_14plus = 1, children_under14 = 0))
)
variants <- merge(variants, types[, c("hhcomp", "label", "median_real")], by = "hhcomp", sort = FALSE)
variants$survey_year <- year
variants$household_income <- variants$median_real * variants$scale
variants$monthly_12 <- variants$household_income / 12
variants$monthly_14 <- variants$household_income / 14
variants$match_cap <- match_cap
variants$match_cap_share_pct <- 100 * match_cap / variants$household_income
write.csv(variants[, c("survey_year", "hhcomp", "label", "variant", "median_real", "scale",
                       "household_income", "monthly_12", "monthly_14", "match_cap",
                       "match_cap_share_pct")],
          file.path(out_dir, "household_income_at_median.csv"), row.names = FALSE)

labels <- c(TOTAL = "Όλος ο πληθυσμός", setNames(household_types$label, household_types$hhcomp),
            A1 = "Ένας ενήλικας μόνος (κάθε ηλικίας)")
di04 <- read.csv("data/eurostat/ilc_di04.csv")
di04 <- di04[di04$unit == "EUR" & di04$hhcomp %in% names(labels), ]
by_year <- merge(
  setNames(di04[di04$statinfo == "MED_EI", c("time", "hhcomp", "value")], c("survey_year", "hhcomp", "median")),
  setNames(di04[di04$statinfo == "MEAN_EI", c("time", "hhcomp", "value")], c("survey_year", "hhcomp", "mean")))
by_year$income_year <- by_year$survey_year - 1
by_year$label <- unname(labels[by_year$hhcomp])
by_year <- by_year[order(by_year$survey_year, by_year$hhcomp),
                   c("survey_year", "income_year", "hhcomp", "label", "median", "mean")]
write.csv(by_year, file.path(out_dir, "household_medians_by_year.csv"), row.names = FALSE)

message("wrote ", paste(file.path(out_dir, c("household_types.csv", "household_fit_check.csv",
                                             "household_income_at_median.csv",
                                             "household_medians_by_year.csv")), collapse = ", "))
