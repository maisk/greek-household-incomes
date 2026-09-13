# Lorenz curve of equivalised disposable income with two readings of income
# concentration: "the poorest 60% hold about as much as the richest 20%" and the
# split of the population into two halves of total income. Reads the CSVs written
# by scripts/dagum_summary.R.
#   Rscript scripts/plot_lorenz.R [survey_year]
# Writes figures/lorenz_<year>.png.

library(ggplot2)
source("R/plot_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args)) as.integer(args[1]) else 2025L
d <- function(f) { x <- read.csv(file.path("data/derived", f)); x[x$survey_year == year, ] }

lor  <- d("lorenz_points.csv")
conc <- d("income_concentration.csv")
curve <- lor[lor$series == "model", ]
pts   <- lor[lor$series == "elstat", ]
L <- approxfun(curve$pop_share, curve$income_share)
pc0 <- function(x) paste0(formatC(100 * x, format = "f", digits = 0), "%")
pc1 <- function(x) paste0(formatC(100 * x, format = "f", digits = 1, decimal.mark = ","), "%")

half  <- conc[conc$kind == "half_split_model", ]
real   <- conc[conc$kind == "equal_total_real", ]
has_real <- nrow(real) == 1
# Brackets: real ELSTAT shares where published, else the model pair for the poorest 60%.
eq <- if (has_real) real else conc[conc$kind == "equal_total_model" & abs(conc$bottom_pop - 0.6) < 1e-9, ]
x_top <- 1 - eq$top_pop
y_top <- L(x_top)

g <- ggplot() +
  geom_abline(slope = 1, intercept = 0, colour = ink_2, linewidth = 0.5, linetype = "22") +
  annotate("text", x = 0.30, y = 0.36, angle = 45, size = 3.1, colour = ink_2,
           label = "απόλυτη ισότητα") +
  geom_line(data = curve, aes(pop_share, income_share), colour = series[1], linewidth = 0.9) +
  geom_point(data = pts, aes(pop_share, income_share), colour = ink, fill = surface, shape = 21,
             size = 2.4, stroke = 0.8) +
  # poorest share: bracket at x = bottom_pop from 0 to its share
  annotate("segment", x = eq$bottom_pop, xend = eq$bottom_pop, y = 0, yend = eq$bottom_share,
           colour = series[2], linewidth = 1.1) +
  annotate("text", x = eq$bottom_pop - 0.015, y = eq$bottom_share / 2, hjust = 1, size = 3.2, colour = ink,
           lineheight = 0.95,
           label = sprintf("το φτωχότερο %s\nέχει το %s\nτου εισοδήματος", pc0(eq$bottom_pop), pc1(eq$bottom_share))) +
  # richest share: bracket on the right edge from L(1 - top_pop) to 1
  annotate("segment", x = x_top, xend = 1, y = y_top, yend = y_top, colour = ink_2, linewidth = 0.4,
           linetype = "22") +
  annotate("segment", x = 1, xend = 1, y = y_top, yend = 1, colour = series[2], linewidth = 1.1) +
  annotate("text", x = 1.025, y = (y_top + 1) / 2, hjust = 0, size = 3.2, colour = ink, lineheight = 0.95,
           label = sprintf("το πλουσιότερο\n%s έχει\nτο %s", pc0(eq$top_pop), pc1(eq$top_share))) +
  # half split: the income level where the poorest and the richest groups hold 50% each
  annotate("segment", x = 0, xend = half$bottom_pop, y = 0.5, yend = 0.5, colour = ink_2,
           linewidth = 0.4, linetype = "22") +
  annotate("point", x = half$bottom_pop, y = 0.5, size = 2.4, colour = series[1]) +
  annotate("text", x = 0.02, y = 0.5, hjust = 0, vjust = -0.35, size = 3.1, colour = ink, lineheight = 0.95,
           label = sprintf("το μισό εισόδημα της χώρας:\nφτωχότερο %s | πλουσιότερο %s",
                           pc0(half$bottom_pop), pc0(half$top_pop))) +
  scale_x_continuous(labels = function(v) paste0(100 * v, "%"), breaks = seq(0, 1, 0.2),
                     expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(labels = function(v) paste0(100 * v, "%"), breaks = seq(0, 1, 0.2),
                     expand = expansion(mult = c(0.01, 0.02))) +
  coord_equal(clip = "off") +
  labs(title = sprintf("Πόσο συγκεντρωμένο είναι το εισόδημα στην Ελλάδα, %d", year - 1),
       subtitle = paste0("Καμπύλη Lorenz: μερίδιο του συνολικού εισοδήματος που έχει το φτωχότερο x% του πληθυσμού.\n",
                         "Όσο πιο κάτω από τη διαγώνιο, τόσο πιο άνισο το εισόδημα. Οι κύκλοι είναι τα στοιχεία της ΕΛΣΤΑΤ."),
       x = "Ποσοστό πληθυσμού (από τον φτωχότερο)", y = "Μερίδιο συνολικού εισοδήματος",
       caption = sprintf(paste0(
         "Πηγή: ΕΛΣΤΑΤ, SILC %d (εισοδήματα %d). Ισοδύναμο διαθέσιμο εισόδημα.\n",
         "Γραμμή: κατανομή Dagum. Οι κάθετες μπάρες είναι %s."),
         year, year - 1, if (has_real) "μερίδια της ΕΛΣΤΑΤ" else "εκτίμηση του μοντέλου")) +
  theme_chart + theme(panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.3),
                      plot.margin = margin(14, 95, 10, 14))

save_chart(g, sprintf("figures/lorenz_%d.png", year), width = 9, height = 9)
message("wrote figures/lorenz_", year, ".png")
