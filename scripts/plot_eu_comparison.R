# Greece against the other EU member states: Gini of equivalised disposable
# income. Reads the CSVs written by scripts/eu_comparison.R.
#   Rscript scripts/plot_eu_comparison.R [survey_year]
# Writes figures/eu_gini_<year>.png and figures/eu_gini_trend_<year>.png.

library(ggplot2)
source("R/plot_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args)) as.integer(args[1]) else 2025L

eu <- read.csv("data/derived/eu_inequality.csv")
eu <- eu[eu$survey_year == year, ]
tr <- read.csv("data/derived/eu_trend.csv")
tr <- tr[tr$survey_year <= year, ]
other <- "#c4c3bd"   # neutral grey for the other member states
num1 <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")

# ---- 1. Gini by member state ------------------------------------------------------

m <- eu[!is.na(eu$gini), ]
m$label <- ifelse(m$is_eu27, "ΕΕ-27 (μέσος όρος)", m$name_el)
m$label <- factor(m$label, levels = m$label[order(m$gini, m$is_eu27)])
m$kind <- ifelse(m$is_greece, "greece", ifelse(m$is_eu27, "eu", "other"))
gr <- m[m$is_greece, ]

g1 <- ggplot(m, aes(gini, label)) +
  geom_col(aes(fill = kind), width = 0.72, show.legend = FALSE) +
  geom_text(aes(label = num1(gini), fontface = ifelse(kind == "other", "plain", "bold")),
            hjust = -0.2, size = 3, colour = ink) +
  scale_fill_manual(values = c(other = other, greece = series[2], eu = ink_2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = sprintf("Ανισότητα εισοδήματος στις χώρες της ΕΕ, %d", year - 1),
       subtitle = sprintf(paste0(
         "Συντελεστής Gini του ισοδύναμου διαθέσιμου εισοδήματος (0 = όλοι έχουν το ίδιο, 100 = ένας τα έχει όλα).\n",
         "Η Ελλάδα είναι %dη από τις %d χώρες, από την πιο άνιση προς την πιο ίση."),
         gr$gini_rank, gr$n_gini),
       x = "Συντελεστής Gini", y = NULL,
       caption = sprintf("Πηγή: Eurostat, EU-SILC %d (εισοδήματα %d), πίνακας ilc_di12.", year, year - 1)) +
  theme_chart + theme(panel.grid.major.y = element_blank(),
                      panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.3),
                      axis.text.y = element_text(colour = ink, size = 10))
save_chart(g1, sprintf("figures/eu_gini_%d.png", year), width = 9, height = 8)

# ---- 2. Gini over time: Greece and EU-27 ----------------------------------------------

lines <- rbind(
  data.frame(survey_year = tr$survey_year, series = "Ελλάδα", gini = tr$gini_greece),
  data.frame(survey_year = tr$survey_year, series = "ΕΕ-27 (μέσος όρος)", gini = tr$gini_eu27))
lines <- lines[!is.na(lines$gini), ]
lines$series <- factor(lines$series, levels = c("Ελλάδα", "ΕΕ-27 (μέσος όρος)"))
ends <- do.call(rbind, lapply(split(lines, lines$series), function(d) d[which.max(d$survey_year), ]))
rk <- tr[!is.na(tr$gini_rank_greece) & tr$survey_year %in% range(tr$survey_year), ]
band <- tr[!is.na(tr$gini_min), ]

g2 <- ggplot() +
  geom_ribbon(data = band, aes(income_year, ymin = gini_min, ymax = gini_max), fill = grid_col, alpha = 0.7) +
  annotate("text", x = min(band$income_year), y = max(band$gini_max), hjust = 0, vjust = -0.5,
           size = 3, colour = ink_2, label = "εύρος των χωρών της ΕΕ (πιο ίση – πιο άνιση)") +
  geom_line(data = lines, aes(survey_year - 1, gini, colour = series), linewidth = 1) +
  geom_point(data = lines, aes(survey_year - 1, gini, colour = series), size = 1.8) +
  geom_text(data = ends, aes(survey_year - 1, gini, label = sprintf("%s: %s", series, num1(gini))),
            hjust = -0.08, size = 3.3, colour = ink) +
  geom_text(data = rk, aes(income_year, gini_greece, label = sprintf("%dη από %d", gini_rank_greece, n_gini),
                           hjust = ifelse(income_year == min(income_year), 0, 1)),
            vjust = -1.1, size = 3, colour = ink_2) +
  scale_colour_manual(values = c(series[2], series[1]), name = NULL) +
  scale_x_continuous(breaks = seq(min(lines$survey_year) - 1, max(lines$survey_year) - 1, 2),
                     expand = expansion(mult = c(0.02, 0.28))) +
  labs(title = "Ανισότητα εισοδήματος: Ελλάδα και ΕΕ",
       subtitle = "Συντελεστής Gini ανά έτος εισοδήματος. Στο πρώτο και στο τελευταίο έτος: η θέση της Ελλάδας στην ΕΕ (1η = η πιο άνιση).",
       x = "Έτος εισοδήματος", y = "Συντελεστής Gini",
       caption = sprintf("Πηγή: Eurostat, EU-SILC %d–%d, πίνακας ilc_di12. Μέσος όρος ΕΕ-27: EU27_2020.",
                         min(lines$survey_year), max(lines$survey_year))) +
  theme_chart + theme(legend.position = "top", legend.justification = "left",
                      legend.text = element_text(colour = ink_2, size = 10),
                      panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.3))
save_chart(g2, sprintf("figures/eu_gini_trend_%d.png", year))

message("wrote figures/eu_gini_", year, ".png and figures/eu_gini_trend_", year, ".png")
