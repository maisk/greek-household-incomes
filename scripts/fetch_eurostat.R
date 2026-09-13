# Download the Eurostat tables used in the analysis: Greece-only tables into
# data/eurostat/<dataset>.csv, and all-country inequality tables into
# data/eurostat/eu/<dataset>.csv. Run from the project root:
#   Rscript scripts/fetch_eurostat.R

source("R/eurostat.R")

datasets <- c(
  ilc_di01     = "Distribution of income by quantiles (cut-off points and shares)",
  ilc_di03     = "Mean and median equivalised income by age and sex",
  ilc_di04     = "Mean and median equivalised income by household composition",
  ilc_di11     = "Income quintile share ratio S80/S20 by sex and age group",
  ilc_di11d    = "Income share ratio S80/S50 by sex and age group",
  ilc_di11e    = "Income share ratio S50/S20 by sex and age group",
  ilc_di20     = "Share of people with income >= 130-160% of national mean/median, by age and sex",
  ilc_li02     = "Share of people below 40-70% of national mean/median (poverty thresholds), by age and sex",
  ilc_li03     = "Share of people below 40-70% of national mean/median (poverty thresholds), by household type"
)

out_dir <- "data/eurostat"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

index <- data.frame(dataset = names(datasets), description = unname(datasets),
                    eurostat_label = NA_character_, updated = NA_character_, rows = NA_integer_)
for (i in seq_along(datasets)) {
  ds <- names(datasets)[i]
  message("fetching ", ds)
  df <- fetch_eurostat(ds, list(geo = "EL"))
  write.csv(df, file.path(out_dir, paste0(ds, ".csv")), row.names = FALSE, na = "")
  index$eurostat_label[i] <- attr(df, "label")
  index$updated[i]        <- attr(df, "updated")
  index$rows[i]           <- nrow(df)
}
write.csv(index, file.path(out_dir, "_index.csv"), row.names = FALSE)
print(index[, c("dataset", "rows", "updated")])

# ---- All countries: inequality indicators for the EU comparison ------------------
# Written to data/eurostat/eu/. Eurostat returns every reporting country and the
# EU aggregates; scripts/eu_comparison.R keeps the 27 member states and EU-27.

eu_datasets <- list(
  ilc_di12 = list(description = "Gini coefficient of equivalised disposable income, all countries",
                  filters = list(age = "TOTAL")),
  ilc_di11 = list(description = "Income quintile share ratio S80/S20, all countries",
                  filters = list(age = "TOTAL", sex = "T")),
  ilc_di01 = list(description = "Share of national equivalised income by quantile (deciles etc.), all countries",
                  filters = list(statinfo = "SHARE", unit = "EUR"))
)
eu_dir <- file.path(out_dir, "eu")
dir.create(eu_dir, showWarnings = FALSE, recursive = TRUE)
eu_index <- data.frame(dataset = names(eu_datasets),
                       description = vapply(eu_datasets, `[[`, "", "description"),
                       eurostat_label = NA_character_, updated = NA_character_, rows = NA_integer_,
                       row.names = NULL)
for (i in seq_along(eu_datasets)) {
  ds <- names(eu_datasets)[i]
  message("fetching ", ds, " (all countries)")
  df <- fetch_eurostat(ds, eu_datasets[[i]]$filters)
  write.csv(df, file.path(eu_dir, paste0(ds, ".csv")), row.names = FALSE, na = "")
  eu_index$eurostat_label[i] <- attr(df, "label")
  eu_index$updated[i]        <- attr(df, "updated")
  eu_index$rows[i]           <- nrow(df)
}
write.csv(eu_index, file.path(eu_dir, "_index.csv"), row.names = FALSE)
print(eu_index[, c("dataset", "rows", "updated")])
