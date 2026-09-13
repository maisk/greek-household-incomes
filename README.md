# Greek Household Incomes and Inequality

*How much Greek households earn, how unequally income is shared, and how Greece compares with the rest of the EU.*

An article in Greek, **«Τι εισοδήματα έχουν τα ελληνικά νοικοκυριά;»**, published at
<https://maistrelis.com/articles/greek-household-incomes/>. It is written in Quarto
(`article/01_household_incomes.qmd`) and built from official statistics (ELSTAT and Eurostat, EU-SILC)
with a reproducible R pipeline. It covers:

- equivalised disposable income and the modified OECD scale;
- the income distribution of the whole population, of age groups and of two household types, modelled
  with a Dagum distribution fitted to the published figures;
- the median family and how incomes changed over the last ten years;
- income concentration, the Lorenz curve and the Gini coefficient;
- Greece against the other EU member states (Gini, S80/S20, share of the richest holding half of all income).

Companion project: [`greek-child-investment-account`](../greek-child-investment-account), an article on who
benefits from the state-matched child investment account, which links here for the basic concepts.

## Layout

| folder | content | source or generated |
|---|---|---|
| `R/` | shared functions: Dagum distribution and fits, Eurostat API client, chart style | source |
| `scripts/` | one script per step: fetch, derive CSVs, draw figures | source |
| `data/` | ELSTAT tables typed in from the press release, EU country list | source |
| `data/eurostat/` | Eurostat extracts | downloaded (`just fetch`) |
| `data/derived/` | every number used in the texts | generated (`just derive`) |
| `figures/` | charts | generated (`just plots`) |
| `doc/*.qmd` | technical notes (Dagum fit, medians and quartiles, household types, S80/S20) | source |
| `doc/*.md` | rendered notes, readable on GitHub | generated (`just docs`) |
| `article/*.qmd` | the article | source |
| `research/` | press releases, papers and saved web pages used as references (PDFs kept locally, not committed) | reference material |

`data/README.md` describes every data file and derived CSV.

## Rebuild

```sh
just all       # derived CSVs, figures, docs and the article, from the data already in data/
just refresh   # download the Eurostat tables first, then everything else
just           # list all recipes
```

The rendered article (`article/*.html`) and the HTML notes (`doc/*.html`) are not committed; `just all`
recreates them.

Requirements: R (≥ 4.3) with `ggplot2`, `knitr`, `rmarkdown` and `jsonlite`; [Quarto](https://quarto.org);
[just](https://github.com/casey/just).

## Reproducibility rule

No number in the article or the notes is typed by hand. Every figure is read from a CSV in
`data/derived/`, which the R scripts write. To change a number, change the script that computes it and
rerun `just all`.

## Shared code

`R/income_distribution.R`, `R/plot_helpers.R`, `R/eurostat.R`, `scripts/household_types.R`,
`scripts/plot_income_distribution_household.R`, `doc/household_types.qmd` and the two ELSTAT tables in
`data/` are also used by the companion project. They are kept as identical copies in both repositories;
change them in both.

## License

The repository holds three kinds of material under three sets of terms:

| what | licence |
|---|---|
| the code: `R/`, `scripts/`, `justfile` | [MIT](LICENSE) |
| the text and the figures: `article/`, `doc/`, `figures/`, the READMEs | [CC BY 4.0](LICENSE-CC-BY-4.0) |
| the data: `data/` | the terms of each source (Eurostat, ELSTAT), see [`data/README.md`](data/README.md#licences-of-the-sources) |

Code and text have different licences because Creative Commons
[recommends against](https://creativecommons.org/faq/#can-i-apply-a-creative-commons-license-to-software)
its licences for software. MIT asks for the same thing as CC BY: keep the author's name.

To credit the article: Kostas Maistrelis, «Τι εισοδήματα έχουν τα ελληνικά νοικοκυριά;», 2026,
<https://maistrelis.com/articles/greek-household-incomes/>, CC BY 4.0.
