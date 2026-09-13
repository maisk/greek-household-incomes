# Dagum income distribution by household type (couple under 65 without
# children, couple with one dependent child), compared with the whole population.
#   Rscript scripts/plot_income_distribution_household.R [survey_year]
# Writes figures/dagum_density_<year>_<slug>.png for each type,
# figures/dagum_fit_<year>_household.png and figures/dagum_compare_household_<year>.png.

library(ggplot2)
source("R/income_distribution.R")
source("R/plot_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args)) as.integer(args[1]) else 2025L

ht   <- household_types
fits <- lapply(ht$hhcomp, function(h) fit_dagum(eurostat_hhcomp_targets(h, year)))
names(fits) <- ht$hhcomp
total <- fit_dagum(silc_targets(read_silc(), year))
reference <- "#8d8c86"  # neutral grey for the whole-population reference curve

# ---- 1. Density per household type -------------------------------------------

for (i in seq_len(nrow(ht))) {
  f <- fits[[i]]; t <- f$targets
  a <- f$par[["a"]]; p <- f$par[["p"]]; b <- f$par[["b"]]
  g <- plot_dagum_density(
    f$par,
    title = sprintf("Κατανομή ισοδύναμου διαθέσιμου εισοδήματος: %s, %d", tolower(ht$label[i]), year - 1),
    subtitle = sprintf(paste0(
      "Πληθυσμός: %s.\nΠραγματικά (Eurostat ilc_di04): διάμεσος %s, μέσος %s. ",
      "Τα μερίδια ανά πεμπτημόριο και οι τιμές στο γράφημα είναι εκτίμηση του μοντέλου."),
      ht$who[i], eur(t$quantiles$x[t$quantiles$src == "ilc_di04 median"]), eur(t$mean)),
    caption = sprintf(paste0(
      "Πηγή: Eurostat, EU-SILC %d (εισοδήματα %d). Ισοδύναμο διαθέσιμο εισόδημα, τροποποιημένη κλίμακα OECD.\n",
      "Κατανομή Dagum: a = %.2f, p = %.2f, b = %s, fit σε όρια 40–70%% εθνικής διαμέσου/μέσου (ilc_li03), διάμεσο και μέσο (ilc_di04)."),
      year, year - 1, a, p, eur(round(b))),
    share_note = "μερίδιο εισοδήματος της ομάδας\nανά πεμπτημόριο (μοντέλο) ↓")
  save_chart(g, sprintf("figures/dagum_density_%d_%s.png", year, ht$slug[i]))
}

# ---- 2. Fit check -------------------------------------------------------------

pts <- do.call(rbind, lapply(seq_len(nrow(ht)), function(i) {
  q <- fits[[i]]$targets$quantiles
  data.frame(group = ht$label[i], x = q$x, p = q$p,
             src = ifelse(grepl("median", q$src), "διάμεσος ομάδας", "κάτω από 40–70% εθνικής διαμέσου/μέσου"))
}))
x_hi <- max(sapply(fits, function(f) qdagum(0.99, f$par[["a"]], f$par[["p"]], f$par[["b"]])))
cdfs <- do.call(rbind, lapply(seq_len(nrow(ht)), function(i) {
  f <- fits[[i]]; x <- seq(1, x_hi, length.out = 600)
  data.frame(group = ht$label[i], x = x, F = pdagum(x, f$par[["a"]], f$par[["p"]], f$par[["b"]]))
}))
errs <- do.call(rbind, lapply(seq_len(nrow(ht)), function(i) {
  cf <- compare_fit(fits[[i]]); m <- cf[cf$target == "mean", ]
  data.frame(group = ht$label[i],
             label = sprintf("σημεία κατανομής: έως ±%.1f%%\nμέσος: μοντέλο %s, πραγματικός %s (%+.1f%%)",
                             max(abs(cf$rel_err_pct[seq_len(nrow(fits[[i]]$targets$quantiles))])),
                             eur(round(m$fitted)), eur(m$observed), m$rel_err_pct))
}))

g2 <- ggplot() +
  geom_line(data = cdfs, aes(x, F), colour = series[1], linewidth = 0.7) +
  geom_point(data = pts, aes(x, p, shape = src), colour = ink, fill = surface, size = 2.4, stroke = 0.8) +
  geom_text(data = errs, aes(x = x_hi, y = 0.08, label = label), hjust = 1, vjust = 0,
            size = 3, colour = ink_2, lineheight = 0.95) +
  facet_wrap(~group) +
  scale_shape_manual(values = c(16, 21), name = NULL) +
  scale_x_continuous(labels = k_eur, breaks = seq(0, 60000, 10000)) +
  scale_y_continuous(labels = function(v) paste0(100 * v, "%"), breaks = seq(0, 1, 0.2)) +
  labs(title = "Έλεγχος προσαρμογής ανά τύπο νοικοκυριού",
       subtitle = "Αθροιστική κατανομή του μοντέλου (γραμμή) και τα σημεία της Eurostat στα οποία έγινε το fit.\nΤα όρια είναι ποσοστά της εθνικής διαμέσου ή του εθνικού μέσου.",
       x = "Ισοδύναμο διαθέσιμο εισόδημα (χιλ. € / έτος)", y = "Αθροιστικό ποσοστό της ομάδας",
       caption = sprintf("Πηγή: Eurostat, EU-SILC %d (ilc_li03, ilc_di04). Η Eurostat δεν δίνει σημεία πάνω από τη διάμεσο ανά τύπο νοικοκυριού· το fit εκεί στηρίζεται στον μέσο.", year)) +
  theme_chart + theme(legend.position = "top", legend.justification = "left",
                      legend.text = element_text(colour = ink_2, size = 9),
                      strip.text = element_text(colour = ink, face = "bold", size = 11),
                      panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.3),
                      panel.spacing = unit(1.5, "lines"))
save_chart(g2, sprintf("figures/dagum_fit_%d_household.png", year), width = 11, height = 6)

# ---- 3. Comparison with the whole population ------------------------------------

comp <- rbind(
  data.frame(name = "Όλος ο πληθυσμός", par = I(list(total$par))),
  data.frame(name = ht$label, par = I(unname(lapply(fits, `[[`, "par"))))
)
comp$name <- factor(comp$name, levels = comp$name)
x_cmp <- 45000
dens <- do.call(rbind, lapply(seq_len(nrow(comp)), function(i) {
  pr <- comp$par[[i]]; x <- seq(1, x_cmp, length.out = 800)
  data.frame(name = comp$name[i], x = x, y = ddagum(x, pr[["a"]], pr[["p"]], pr[["b"]]))
}))
meds <- do.call(rbind, lapply(seq_len(nrow(comp)), function(i) {
  pr <- comp$par[[i]]; m <- qdagum(0.5, pr[["a"]], pr[["p"]], pr[["b"]])
  data.frame(name = comp$name[i], x = m, y = ddagum(m, pr[["a"]], pr[["p"]], pr[["b"]]))
}))
di04 <- read.csv("data/eurostat/ilc_di04.csv")
di04 <- di04[di04$time == year & di04$unit == "EUR", ]
real <- function(h, stat) di04$value[di04$hhcomp == h & di04$statinfo == stat]
tab <- paste0("Πραγματικά (Eurostat ilc_di04), διάμεσος / μέσος:  όλοι ",
              eur(real("TOTAL", "MED_EI")), " / ", eur(real("TOTAL", "MEAN_EI")), "\n",
              paste0(tolower(ht$label), " ", eur(sapply(ht$hhcomp, real, "MED_EI")), " / ",
                     eur(sapply(ht$hhcomp, real, "MEAN_EI")), collapse = "  ·  "))

x0 <- 16000
labs_c <- do.call(rbind, lapply(seq_len(nrow(comp)), function(i) {
  pr <- comp$par[[i]]
  data.frame(name = comp$name[i], x = x0, y = ddagum(x0, pr[["a"]], pr[["p"]], pr[["b"]]))
}))
y_top <- max(dens$y)
labs_c <- labs_c[order(labs_c$y), ]
labs_c$y_lab <- labs_c$y + 0.10 * y_top
for (j in seq_len(nrow(labs_c))[-1]) {
  labs_c$y_lab[j] <- max(labs_c$y_lab[j], labs_c$y_lab[j - 1] + 0.07 * y_top)
}
labs_c$x_lab <- x0 + 2500
cols <- c(reference, series[1:2])

g3 <- ggplot(dens, aes(x, y, colour = name)) +
  geom_line(aes(linewidth = name)) +
  geom_point(data = meds, size = 2.6, stroke = 1.2, fill = surface, shape = 21) +
  geom_segment(data = labs_c, aes(x = x, y = y, xend = x_lab, yend = y_lab), linewidth = 0.4,
               show.legend = FALSE) +
  geom_point(data = labs_c, aes(x = x, y = y), size = 1.4, show.legend = FALSE) +
  geom_text(data = labs_c, aes(x = x_lab, y = y_lab, label = name), colour = ink, hjust = -0.05,
            size = 3.3, show.legend = FALSE) +
  scale_colour_manual(values = cols, name = NULL) +
  scale_linewidth_manual(values = c(0.6, 0.9, 0.9), guide = "none") +
  scale_x_continuous(labels = k_eur, breaks = seq(0, 60000, 5000), limits = c(0, x_cmp),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(labels = NULL, expand = expansion(mult = c(0, 0.08))) +
  labs(title = sprintf("Ισοδύναμο διαθέσιμο εισόδημα ανά τύπο νοικοκυριού, %d", year - 1),
       subtitle = paste0("Εκτιμώμενες πυκνότητες (Dagum). Ο κύκλος σημειώνει τη διάμεσο του μοντέλου.\n", tab),
       x = "Ισοδύναμο διαθέσιμο εισόδημα (χιλ. € / έτος)", y = "Πυκνότητα",
       caption = sprintf(paste0(
         "Πηγή: ΕΛΣΤΑΤ και Eurostat, EU-SILC %d (εισοδήματα %d). «Όλος ο πληθυσμός»: fit σε όρια και μερίδια ομάδων της ΕΛΣΤΑΤ.\n",
         "Τύποι νοικοκυριού: fit σε ilc_li03 και ilc_di04. «Παιδί» = εξαρτώμενο παιδί κατά Eurostat (κάτω των 18, ή 18–24 χωρίς εργασία)."),
         year, year - 1)) +
  theme_chart + theme(legend.position = "top", legend.justification = "left",
                      legend.text = element_text(colour = ink_2, size = 10),
                      panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.3),
                      panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.3))
save_chart(g3, sprintf("figures/dagum_compare_household_%d.png", year))

for (f in fits) print(f)
message("wrote household figures for ", year)
