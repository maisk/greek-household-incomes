# Data

Source: ELSTAT, *Income Inequality — 2025 Survey on Income and Living Conditions*
(press release 19 March 2026, `research/Income inequality ( 2025 ).pdf`), Tables 1–4.

Income concept: **equivalised disposable (net) income** per person, modified OECD scale
(1.0 first adult, 0.5 each other person 14+, 0.3 each child under 14). Each household
member is assigned the household's equivalised income.

`survey_year` is the SILC wave; `income_year` is the reference year of the income (survey_year − 1).

- `elstat_silc_groups.csv` — share of total equivalised income per group and the group's
  upper income limit (€/year; blank for the top group). Quartiles 2015–2025 (Table 2),
  quintiles 2023–2025 (Table 3).
- `elstat_silc_indicators.csv` — Gini (%, Table 4) and S80/S20 ratio in total and by age group (Table 1).

Known issue: the ELSTAT tables give the 2024 survey median (upper limit of quartile 2) as
10,615 €, in both the 2024 and 2025 press releases. Eurostat gives 10,850 € in `ilc_di01` (D5 and
Q2) and in `ilc_di03`, and uses 10,850 € for the official poverty thresholds. The ELSTAT figure is
kept as published in `elstat_silc_groups.csv`, but the Eurostat value should be preferred.

- `eu_countries.csv` — reference list of the 27 EU member states and the EU-27 aggregate
  (Eurostat codes, Greek names and articles), used by `scripts/eu_comparison.R`.

## `derived/`

Computed from the files above by `Rscript scripts/medians_quartiles.R`,
`Rscript scripts/dagum_summary.R`, `Rscript scripts/household_types.R` and
`Rscript scripts/eu_comparison.R` (or `just derive`). They feed `doc/*.qmd` and `article/*.qmd`.
Do not edit them by hand.

| file | content |
|---|---|
| `medians_by_age.csv` | real median and mean per age group (total, <65, 18–64) and year |
| `quartiles_total.csv` | real P25/P50/P75 of the whole population per year (Eurostat, with ELSTAT alongside) |
| `quartiles_age.csv` | P25/P50/P75 per age group: Dagum model, real value where published, bounds from published CDF points |
| `household_examples.csv` | equivalised values × OECD scale for typical household types |
| `dagum_by_year.csv` | Dagum fit of the whole population per year, model vs real mean/median |
| `dagum_fit_check.csv` | one year: fitted ELSTAT targets and out-of-sample Eurostat cut-offs, observed vs fitted |
| `dagum_model_comparison.csv` | one year: Pareto, lognormal and Dagum fitted to the same targets |
| `group_cutoffs_check.csv` | one year: ELSTAT vs Eurostat cut-offs (quintiles; quartiles before 2023) |
| `real_mean_median.csv` | one year: real mean/median for the total population and children |
| `income_concentration.csv` | one year: "the poorest a% hold as much as the richest b%" (ELSTAT and model) and the half split (the richest share holding half of all income) |
| `lorenz_points.csv` | one year: model Lorenz curve and ELSTAT Lorenz points |
| `eu_inequality.csv` | one year: Gini and S80/S20 per EU member state and EU-27, with ranks (1 = most unequal); from the decile shares (no model) the half split and the two deciles that bracket it |
| `eu_trend.csv` | 2015 onwards: Greece and EU-27 Gini and S80/S20, Greece's rank, EU min–max |
| `eu_rank_changes.csv` | member states that moved from above to below Greece (or the reverse) since 2015 |
| `household_types.csv` | per household type (couple <65 without children, couple with 1 child): real and model median/mean, Dagum parameters |
| `household_fit_check.csv` | per household type: fitted targets, observed vs fitted |
| `household_income_at_median.csv` | household net income of a family exactly at its type's real median |
| `household_medians_by_year.csv` | real median and mean per household type and year (ilc_di04) |

## `eurostat/`

Greece-only extracts from the Eurostat dissemination API, produced by
`Rscript scripts/fetch_eurostat.R` (see `eurostat/_index.csv` for fetch date and Eurostat update time).
Tidy long format: one row per cell, dimension codes plus `*_label` columns, `value`, `flag`.
`time` is the SILC survey year (income year = time − 1).

All-country tables for the EU comparison are in `eurostat/eu/` (`ilc_di12` Gini, `ilc_di11` S80/S20,
`ilc_di01` income shares by quantile in EUR, all reporting countries and EU aggregates, with their own
`_index.csv`).

| file | content |
|---|---|
| `ilc_di01` | equivalised income cut-offs (deciles, quartiles, quintiles, P1–P5, P95–P99) and shares; matches the ELSTAT figures |
| `ilc_di03` | mean and median equivalised income by age group and sex (EUR, NAC, PPS) |
| `ilc_di04` | mean and median equivalised income by household type (e.g. `A2_2LT65`, `A2_DCH1`) |
| `ilc_di11`, `ilc_di11d`, `ilc_di11e` | S80/S20, S80/S50, S50/S20 by age group ("S50" = middle quintile share) |
| `ilc_di20` | % of an age group at or above 130–160% of the national median/mean |
| `ilc_li02` | % of an age group below 40–70% of the national median/mean |
| `ilc_li03` | % of a household type below 40–70% of the national median/mean |

## Licences of the sources

The statistics in this folder belong to their sources, not to the author of this repository, so the
repository's own licences (MIT for the code, CC BY 4.0 for the text) do not apply to them. Each source
carries its own terms:

| files | source | terms |
|---|---|---|
| `eurostat/`, `eurostat/eu/` | Eurostat, EU-SILC, dissemination API | **CC BY 4.0** ([Eurostat copyright notice](https://ec.europa.eu/eurostat/web/main/help/copyright-notice)) |
| `elstat_silc_groups.csv`, `elstat_silc_indicators.csv` | ELSTAT, press release of 19 March 2026, typed in by hand | ELSTAT [Copyright and reuse of data policy](https://www.statistics.gr/documents/20181/1412250/Copyright_Reuse_Policy_EN.pdf/dfacb7d1-3d9b-471f-851a-8b8b09994a74): free reuse, commercial too, with no licence needed. Not a Creative Commons licence |
| `derived/` | computed from the two above by `scripts/` | the terms of those two sources |
| `eu_countries.csv` | written for this project (Eurostat codes, Greek names and articles) | CC BY 4.0, like the article |

**What both sources ask for.** Both require the same two things: name the source, and state any
changes you made. Eurostat asks for its datasets to be cited by DOI and access date, for example
`Source: Eurostat, https://doi.org/10.2908/ILC_DI12, accessed 2026-09-11`. The DOI is `10.2908/` + the
dataset code; the access date is the day of the fetch (2026-09-11 for the extracts committed here),
and each `_index.csv` records when Eurostat last updated the table. ELSTAT additionally asks you to state that ELSTAT bears no responsibility for the result of
the modification.

**Changes made here.** The files in `eurostat/` are Eurostat's values unchanged, only reshaped into
long format with the labels alongside. The files in `derived/`, the figures and every number in the
article are the author's own computations from those data (medians, Dagum fits, ranks, shares,
interpolations). They are not figures published by Eurostat or ELSTAT, and neither body bears any
responsibility for them.

**One Eurostat exception.** Eurostat excludes from commercial reuse the data of non-EU countries that
are neither EFTA members nor official EU candidates. The all-country tables in `eurostat/eu/` include
such rows (`UK`, and `XK` Kosovo, a potential rather than an official candidate). The analysis uses
only the 27 member states and the EU-27 aggregate (`eu_countries.csv`), so `derived/` holds none of
them.

The ELSTAT press releases and other reference documents in `research/` are kept locally and not
committed, because they are the publishers' own documents.
