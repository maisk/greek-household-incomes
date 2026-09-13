# Orchestration for the "Greek household incomes and inequality" article.
# Run `just` to list recipes. Everything runs from the project root.
#
# Pipeline:
#   fetch    Eurostat API           -> data/eurostat/*.csv, data/eurostat/eu/*.csv (network)
#   derive   data/                  -> data/derived/*.csv
#   plots    data/                  -> figures/*.png
#   docs     data/derived/, figures -> doc/*.md, doc/*.html            (quarto render)
#   articles data/derived/, figures -> article/*.html                  (quarto render)
#
# `year` is the SILC survey year; incomes refer to year - 1.

set shell := ["bash", "-euo", "pipefail", "-c"]

year := "2025"

# List available recipes
default:
    @just --list --unsorted

# Offline rebuild: derived CSVs, figures, docs and articles from the data already in data/
all: derive plots docs articles

# Full rebuild: download Eurostat tables first, then everything else
refresh: fetch all

# Download the Eurostat tables (Greece, and all EU countries for the comparison)
fetch:
    Rscript scripts/fetch_eurostat.R

# Compute the CSVs in data/derived/ that the docs and the article read
derive y=year:
    Rscript scripts/medians_quartiles.R {{y}}
    Rscript scripts/dagum_summary.R {{y}}
    Rscript scripts/household_types.R {{y}}
    Rscript scripts/eu_comparison.R {{y}}

# Whole-population density and fit-check figures
plot-total y=year:
    Rscript scripts/plot_income_distribution.R {{y}}

# Age-group (18-64, <65) densities, fit check and comparison figures
plot-age y=year:
    Rscript scripts/plot_income_distribution_age.R {{y}}

# Household-type (couple <65 without children, couple with 1 child) densities, fit check and comparison
plot-household y=year:
    Rscript scripts/plot_income_distribution_household.R {{y}}

# Lorenz curve with income concentration (poorest 60% vs richest 20%, half of all income)
plot-lorenz y=year:
    Rscript scripts/plot_lorenz.R {{y}}

# Greece vs the other EU member states (Gini by country, Gini over time)
plot-eu y=year:
    Rscript scripts/plot_eu_comparison.R {{y}}

# All figures
plots y=year: (plot-total y) (plot-age y) (plot-household y) (plot-lorenz y) (plot-eu y)

# Render every Quarto document in doc/ to Markdown and self-contained HTML
docs:
    for f in doc/*.qmd; do just doc "$(basename "$f" .qmd)"; done

# Render the HTML article in article/
articles:
    for f in article/*.qmd; do quarto render "$f" --quiet; echo "rendered ${f%.qmd}.html"; done

# Render one document, e.g. `just doc medians_quartiles`
doc name:
    quarto render doc/{{name}}.qmd --to gfm --quiet
    quarto render doc/{{name}}.qmd --to html -M embed-resources:true -M toc:true -M lang:el --quiet
    @echo "rendered doc/{{name}}.md and doc/{{name}}.html"

# Print the Dagum fits (whole population, age groups, household types) with fitted vs observed targets
fit y=year:
    Rscript -e 'source("R/income_distribution.R"); \
      f <- fit_dagum(silc_targets(read_silc(), {{y}})); print(f); print(compare_fit(f), digits = 4); \
      for (g in c("Y_LT65", "Y18-64")) { f <- fit_dagum(eurostat_age_targets(g, {{y}})); \
        cat("\n"); print(f); print(compare_fit(f), digits = 4) }; \
      for (h in household_types$hhcomp) { f <- fit_dagum(eurostat_hhcomp_targets(h, {{y}})); \
        cat("\n"); print(f); print(compare_fit(f), digits = 4) }'

# Show which generated files changed since the last commit
status:
    git status --short data/derived figures doc data/eurostat

# Remove Quarto render caches (generated outputs are kept)
clean:
    rm -rf doc/.quarto doc/*_files doc/*_cache article/.quarto article/*_files
