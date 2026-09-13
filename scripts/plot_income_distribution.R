# Plots of the fitted Dagum distribution of equivalised disposable income.
#   Rscript scripts/plot_income_distribution.R [survey_year]
# Writes figures/dagum_density_<year>.png and figures/dagum_fit_<year>.png.

library(ggplot2)
source("R/income_distribution.R")
source("R/plot_helpers.R")   # density_grid()

args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args)) as.integer(args[1]) else 2025L

silc <- read_silc()
fit  <- fit_dagum(silc_targets(silc, year))
a <- fit$par[["a"]]; p <- fit$par[["p"]]; b <- fit$par[["b"]]

ink       <- "#0b0b0b"
ink_2     <- "#52514e"
surface   <- "#fcfcfb"
grid_col  <- "#e6e5e1"
seq_blues <- c("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281")  # sequential steps 250-650

eur <- function(x) paste0(formatC(x, format = "d", big.mark = ".", decimal.mark = ","), " €")
pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 1, decimal.mark = ","), "%")

theme_chart <- theme_minimal(base_size = 12) +
  theme(
    plot.background   = element_rect(fill = surface, colour = NA),
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.3),
    axis.text         = element_text(colour = ink_2),
    axis.title        = element_text(colour = ink_2, size = 10),
    plot.title        = element_text(colour = ink, face = "bold", size = 14),
    plot.subtitle     = element_text(colour = ink_2, size = 10),
    plot.caption      = element_text(colour = ink_2, size = 8, hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin       = margin(14, 18, 10, 14)
  )

dir.create("figures", showWarnings = FALSE)
source_note <- sprintf(paste0(
  "Πηγή: ΕΛΣΤΑΤ, SILC %d (εισοδήματα %d). Ισοδύναμο διαθέσιμο εισόδημα, τροποποιημένη κλίμακα OECD.\n",
  "Κατανομή Dagum: a = %.2f, p = %.2f, b = %s, fit σε όρια ομάδων, μερίδια εισοδήματος και Gini."),
  year, year - 1, a, p, eur(round(b)))

# ---- 1. Density with quintile bands ------------------------------------------

x_max <- qdagum(0.99, a, p, b)
cuts  <- qdagum(c(0, 0.2, 0.4, 0.6, 0.8), a, p, b)
cuts[1] <- 0
bands <- do.call(rbind, lapply(1:5, function(i) {
  hi <- if (i < 5) qdagum(i / 5, a, p, b) else x_max
  x  <- seq(max(cuts[i], 1), hi, length.out = 200)
  data.frame(quintile = factor(i), x = x, y = ddagum(x, a, p, b))
}))
curve <- data.frame(x = seq(1, x_max, length.out = 800))
curve$y <- ddagum(curve$x, a, p, b)

shares <- group_shares_dagum(5, a, p)
labs_q <- do.call(rbind, lapply(1:5, function(i) {
  hi <- if (i < 5) qdagum(i / 5, a, p, b) else qdagum(0.93, a, p, b)
  data.frame(x = (max(cuts[i], qdagum(0.02, a, p, b)) + hi) / 2,
             label = sprintf("Q%d\n%s", i, pct(shares[i])))
}))
y_top  <- max(curve$y)
med    <- qdagum(0.5, a, p, b)
mu     <- mean_dagum(a, p, b)

g1 <- ggplot() +
  geom_area(data = bands, aes(x, y, fill = quintile, group = quintile),
            colour = surface, linewidth = 0.6, show.legend = FALSE) +
  density_grid(x_max, y_top * 1.04) +
  geom_line(data = curve, aes(x, y), colour = ink, linewidth = 0.6) +
  scale_fill_manual(values = seq_blues) +
  geom_segment(aes(x = med, xend = med, y = 0, yend = ddagum(med, a, p, b)),
               colour = ink, linewidth = 0.4, linetype = "22") +
  geom_segment(aes(x = mu, xend = mu, y = 0, yend = ddagum(mu, a, p, b)),
               colour = ink, linewidth = 0.4, linetype = "22") +
  geom_segment(aes(x = c(med, mu), xend = c(med, mu), y = ddagum(c(med, mu), a, p, b),
                   yend = y_top * 1.06), colour = ink_2, linewidth = 0.3) +
  annotate("text", x = med, y = y_top * 1.07, hjust = 1, vjust = 0,
           label = paste0("διάμεσος ", eur(round(med))), size = 3.2, colour = ink) +
  annotate("text", x = mu, y = y_top * 1.07, hjust = 0, vjust = 0,
           label = paste0("μέσος ", eur(round(mu))), size = 3.2, colour = ink) +
  geom_text(data = labs_q, aes(x, y = -0.07 * y_top, label = label),
            size = 3.1, colour = ink_2, lineheight = 0.9, vjust = 1) +
  annotate("text", x = x_max, y = 0.2 * y_top, hjust = 1, size = 3, colour = ink_2,
           label = "μερίδιο συνολικού\nεισοδήματος ανά πεμπτημόριο ↓", lineheight = 0.9) +
  scale_x_continuous(labels = function(x) formatC(x / 1000, format = "d"),
                     breaks = seq(0, 60000, 5000), expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(labels = NULL, expand = expansion(mult = c(0.02, 0.12))) +
  coord_cartesian(ylim = c(-0.2 * y_top, y_top * 1.12), clip = "off") +
  labs(title = sprintf("Κατανομή ισοδύναμου διαθέσιμου εισοδήματος στην Ελλάδα, %d", year - 1),
       subtitle = sprintf("Εκτιμώμενη πυκνότητα (Dagum) και πεμπτημόρια πληθυσμού · Gini μοντέλου %s, ΕΛΣΤΑΤ %s · εμφανίζεται έως το P99 (%s)",
                          pct(gini_dagum(a, p)), pct(fit$targets$gini), eur(round(x_max, -2))),
       x = "Ισοδύναμο διαθέσιμο εισόδημα (χιλ. € / έτος)", y = "Πυκνότητα",
       caption = source_note) +
  theme_chart + theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank())

ggsave(sprintf("figures/dagum_density_%d.png", year), g1, width = 10, height = 6, dpi = 150,
       bg = surface, device = png, type = "cairo")

# ---- 2. Fit check: fitted CDF vs observed cut-off points --------------------

obs <- data.frame(p = fit$targets$quantiles$p, x = fit$targets$quantiles$x, src = "ΕΛΣΤΑΤ (όρια ομάδων)")
es_file <- "data/eurostat/ilc_di01.csv"
if (file.exists(es_file)) {
  es <- read.csv(es_file)
  es <- es[es$time == year & es$unit == "EUR" & es$statinfo == "TC" &
           grepl("^(D[1-9]|P9[5-9])$", es$quant_inc), ]
  num <- as.numeric(sub("^[DP]", "", es$quant_inc))
  pp  <- ifelse(grepl("^D", es$quant_inc), num / 10, num / 100)
  obs <- rbind(obs, data.frame(p = pp, x = es$value, src = "Eurostat (δεκατημόρια, P95–P99)"))
  obs <- obs[!duplicated(round(obs$p, 3)) | obs$src == "ΕΛΣΤΑΤ (όρια ομάδων)", ]
}
cdf <- data.frame(x = seq(1, qdagum(0.995, a, p, b), length.out = 600))
cdf$F <- pdagum(cdf$x, a, p, b)

g2 <- ggplot() +
  geom_line(data = cdf, aes(x, F), colour = "#2a78d6", linewidth = 0.7) +
  geom_point(data = obs, aes(x, p, shape = src), colour = ink, fill = surface, size = 2.6, stroke = 0.8) +
  scale_shape_manual(values = c(21, 16), name = NULL) +
  annotate("text", x = qdagum(0.55, a, p, b), y = 0.55, hjust = -0.15, colour = "#1c5cab",
           size = 3.3, label = "Dagum (μοντέλο)") +
  scale_x_continuous(labels = function(x) formatC(x / 1000, format = "d"), breaks = seq(0, 60000, 10000)) +
  scale_y_continuous(labels = function(v) paste0(100 * v, "%"), breaks = seq(0, 1, 0.2)) +
  labs(title = "Έλεγχος προσαρμογής: αθροιστική κατανομή μοντέλου έναντι παρατηρήσεων",
       subtitle = "Ποσοστό πληθυσμού με εισόδημα έως x. Τα δεκατημόρια και P95–P99 της Eurostat δεν χρησιμοποιήθηκαν στο fit (έλεγχος εκτός δείγματος).",
       x = "Ισοδύναμο διαθέσιμο εισόδημα (χιλ. € / έτος)", y = "Αθροιστικό ποσοστό πληθυσμού",
       caption = source_note) +
  theme_chart + theme(legend.position = "top", legend.justification = "left",
                      legend.text = element_text(colour = ink_2, size = 9),
                      panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.3))

ggsave(sprintf("figures/dagum_fit_%d.png", year), g2, width = 10, height = 6, dpi = 150,
       bg = surface, device = png, type = "cairo")

message("wrote figures/dagum_density_", year, ".png and figures/dagum_fit_", year, ".png")
