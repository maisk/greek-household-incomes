# Greece against the other EU member states: Gini and S80/S20 of equivalised
# disposable income (Eurostat ilc_di12, ilc_di11, all countries). Writes CSVs to
# data/derived/ for article/01_household_incomes.qmd.
#   Rscript scripts/eu_comparison.R [survey_year]
#
# Outputs (survey_year = SILC wave; income_year = survey_year - 1):
#   eu_inequality.csv  survey_year: Gini and S80/S20 per member state and EU-27, with ranks
#                      (1 = most unequal) among the member states with data, and, from the
#                      published decile shares (ilc_di01, no model), the half split (richest
#                      share holding half of all income) and the two deciles that bracket it
#   eu_trend.csv       2015..survey_year: Greece and EU-27, and Greece's rank per year
#   eu_rank_changes.csv member states that changed side relative to Greece between 2015 and
#                      survey_year (were more unequal and became less unequal, or the reverse)

source("R/income_distribution.R")

args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args)) as.integer(args[1]) else 2025L
first_year <- 2015L
out_dir <- "data/derived"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

countries <- read.csv("data/eu_countries.csv")
members <- setdiff(countries$code, "EU27_2020")

read_indicator <- function(file, name) {
  d <- read.csv(file.path("data/eurostat/eu", file))
  d <- d[d$geo %in% countries$code, c("time", "geo", "value")]
  names(d) <- c("survey_year", "code", name)
  d
}
ind <- merge(read_indicator("ilc_di12.csv", "gini"), read_indicator("ilc_di11.csv", "s80s20"),
             by = c("survey_year", "code"), all = TRUE)
ind$name_el <- countries$name_el[match(ind$code, countries$code)]
ind$article <- countries$article[match(ind$code, countries$code)]
ind$is_eu27 <- ind$code == "EU27_2020"
ind$is_greece <- ind$code == "EL"

# Rank 1 = most unequal, among member states with a value in that year.
rank_desc <- function(x) { r <- rep(NA_integer_, length(x)); ok <- !is.na(x); r[ok] <- rank(-x[ok], ties.method = "min"); r }
ind <- do.call(rbind, lapply(split(ind, ind$survey_year), function(d) {
  m <- !d$is_eu27
  d$gini_rank <- NA_integer_; d$s80s20_rank <- NA_integer_
  d$gini_rank[m] <- rank_desc(d$gini[m]); d$s80s20_rank[m] <- rank_desc(d$s80s20[m])
  d$n_gini <- sum(!is.na(d$gini[m])); d$n_s80s20 <- sum(!is.na(d$s80s20[m]))
  d
}))
ind$income_year <- ind$survey_year - 1

y <- ind[ind$survey_year == year, ]
if (!nrow(y)) stop("no Eurostat inequality data for survey year ", year)

# Concentration points from the decile shares, by piecewise-linear Lorenz curve.
dec <- read.csv("data/eurostat/eu/ilc_di01.csv")
dec <- dec[dec$time == year & dec$unit == "EUR" & dec$quant_inc %in% paste0("D", 1:10), ]
conc <- do.call(rbind, lapply(y$code, function(g) {
  d <- dec[dec$geo == g, ]
  sh <- d$value[match(paste0("D", 1:10), d$quant_inc)]
  if (length(sh) != 10 || anyNA(sh)) {
    return(data.frame(code = g, half_top_pop = NA, half_lo_pop = NA, half_lo_share = NA,
                      half_hi_pop = NA, half_hi_share = NA))
  }
  # The published deciles bracket the half split: the richest k tenths hold less than half,
  # the richest k + 1 tenths at least half.
  top <- cumsum(rev(sh)) / sum(sh)
  k <- which(top >= 0.5)[1]
  data.frame(code = g, half_top_pop = 1 - half_split(lorenz_from_shares(sh)),
             half_lo_pop = (k - 1) / 10, half_lo_share = if (k > 1) top[k - 1] else 0,
             half_hi_pop = k / 10, half_hi_share = top[k])
}))
y <- merge(y, conc, by = "code", all.x = TRUE)
y$lorenz_basis <- ifelse(is.na(y$half_top_pop), NA, "deciles")
# Linear reading of the Gini: the average income gap between two randomly chosen
# people equals 2 x Gini x mean income.
y$mean_gap_share <- 2 * y$gini / 100
y <- y[order(y$is_eu27, y$gini_rank), c("survey_year", "income_year", "code", "name_el", "article", "is_greece",
                                         "is_eu27", "gini", "gini_rank", "n_gini", "s80s20",
                                         "s80s20_rank", "n_s80s20", "half_top_pop", "half_lo_pop",
                                         "half_lo_share", "half_hi_pop", "half_hi_share",
                                         "lorenz_basis", "mean_gap_share")]
write.csv(y, file.path(out_dir, "eu_inequality.csv"), row.names = FALSE)

tr <- ind[ind$survey_year >= first_year & ind$survey_year <= year & (ind$is_greece | ind$is_eu27), ]
trend <- merge(
  setNames(tr[tr$is_greece, c("survey_year", "gini", "s80s20", "gini_rank", "n_gini")],
           c("survey_year", "gini_greece", "s80s20_greece", "gini_rank_greece", "n_gini")),
  setNames(tr[tr$is_eu27, c("survey_year", "gini", "s80s20")],
           c("survey_year", "gini_eu27", "s80s20_eu27")),
  by = "survey_year", all = TRUE)
# EU range of the Gini per year (member states only; years without Gini data are skipped).
ms <- ind[!ind$is_eu27 & !is.na(ind$gini), ]
mm <- do.call(rbind, lapply(split(ms, ms$survey_year), function(d)
  data.frame(survey_year = d$survey_year[1], gini_min = min(d$gini), gini_max = max(d$gini))))
trend <- merge(trend, mm, by = "survey_year", all.x = TRUE)
trend$income_year <- trend$survey_year - 1
trend$gini_gap <- trend$gini_greece - trend$gini_eu27
write.csv(trend, file.path(out_dir, "eu_trend.csv"), row.names = FALSE)

g_first <- ind[ind$survey_year == first_year & !ind$is_eu27, c("code", "name_el", "article", "gini")]
g_last  <- ind[ind$survey_year == year & !ind$is_eu27, c("code", "gini")]
chg <- merge(setNames(g_first, c("code", "name_el", "article", "gini_first")), setNames(g_last, c("code", "gini_last")))
el_first <- chg$gini_first[chg$code == "EL"]; el_last <- chg$gini_last[chg$code == "EL"]
chg$was_above_greece <- chg$gini_first > el_first
chg$is_above_greece  <- chg$gini_last > el_last
chg <- chg[chg$code != "EL" & chg$was_above_greece != chg$is_above_greece, ]
chg$first_year <- first_year; chg$survey_year <- year
chg$direction <- ifelse(chg$was_above_greece, "moved_below_greece", "moved_above_greece")
write.csv(chg[order(chg$direction, -chg$gini_first),
              c("first_year", "survey_year", "code", "name_el", "article", "gini_first", "gini_last", "direction")],
          file.path(out_dir, "eu_rank_changes.csv"), row.names = FALSE)

message(sprintf("Greece %d: Gini %.1f, rank %d of %d; EU-27 %.1f", year,
                y$gini[y$is_greece], y$gini_rank[y$is_greece], y$n_gini[y$is_greece], y$gini[y$is_eu27]))
