# Dagum income distribution for persons aged 18-64 and under 65, compared with
# the whole population. Fitted to Eurostat tables by age (see
# eurostat_age_targets() in R/income_distribution.R).
#   Rscript scripts/plot_income_distribution_age.R [survey_year]
# Writes figures/dagum_density_<year>_18-64.png, figures/dagum_density_<year>_lt65.png,
# figures/dagum_fit_<year>_age.png and figures/dagum_compare_<year>.png.

library(ggplot2)
source("R/income_distribution.R")
source("R/plot_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args)) as.integer(args[1]) else 2025L

groups <- data.frame(
  age   = c("Y18-64", "Y_LT65"),
  slug  = c("18-64", "lt65"),
  label = c("18–64 ετών", "κάτω των 65"),
  who   = c("άτομα 18–64 ετών (ενήλικες σε ηλικία εργασίας)",
            "άτομα κάτω των 65 (μαζί με τα παιδιά)")
)
fits <- lapply(groups$age, function(g) fit_dagum(eurostat_age_targets(g, year)))
di03 <- read.csv("data/eurostat/ilc_di03.csv")
di03 <- di03[di03$time == year & di03$sex == "T" & di03$unit == "EUR", ]
real <- function(age, stat) di03$value[di03$age == age & di03$statinfo == stat]
names(fits) <- groups$age
total <- fit_dagum(silc_targets(read_silc(), year))

target_note <- function(t) {
  used <- c("όρια 40–70% εθνικής διαμέσου/μέσου (ilc_li02)",
            if (any(grepl("ilc_di20", t$quantiles$src))) "όρια 130–160% (ilc_di20)",
            "διάμεσος και μέσος (ilc_di03)",
            if (length(t$ratios)) "λόγοι S80/S20, S80/S50, S50/S20 (ilc_di11)")
  paste(used, collapse = ", ")
}

# ---- 1. Density per age group ----------------------------------------------

for (i in seq_len(nrow(groups))) {
  f <- fits[[groups$age[i]]]; t <- f$targets
  a <- f$par[["a"]]; p <- f$par[["p"]]; b <- f$par[["b"]]
  g <- plot_dagum_density(
    f$par,
    title = sprintf("Κατανομή ισοδύναμου διαθέσιμου εισοδήματος, %s, %d", groups$label[i], year - 1),
    subtitle = sprintf(paste0(
      "Πληθυσμός: %s. Πραγματικά (Eurostat): διάμεσος %s, μέσος %s.\n",
      "Τα πεμπτημόρια είναι της ομάδας· τα μερίδια ανά πεμπτημόριο και οι τιμές στο γράφημα είναι εκτίμηση του μοντέλου."),
      groups$who[i], eur(t$quantiles$x[t$quantiles$src == "ilc_di03 median"]), eur(t$mean)),
    caption = sprintf(paste0(
      "Πηγή: Eurostat, EU-SILC %d (εισοδήματα %d). Ισοδύναμο διαθέσιμο εισόδημα, τροποποιημένη κλίμακα OECD.\n",
      "Κατανομή Dagum: a = %.2f, p = %.2f, b = %s, fit σε: %s."),
      year, year - 1, a, p, eur(round(b)), target_note(t)),
    share_note = "μερίδιο εισοδήματος της ομάδας\nανά πεμπτημόριο (μοντέλο) ↓")
  save_chart(g, sprintf("figures/dagum_density_%d_%s.png", year, groups$slug[i]))
}

# ---- 2. Fit check: CDF vs the points it was fitted to ------------------------

pts <- do.call(rbind, lapply(seq_len(nrow(groups)), function(i) {
  q <- fits[[groups$age[i]]]$targets$quantiles
  data.frame(group = groups$label[i], x = q$x, p = q$p,
             src = ifelse(grepl("median", q$src), "διάμεσος ομάδας",
                          ifelse(grepl("ilc_li02", q$src), "κάτω από 40–70% εθνικής διαμέσου/μέσου",
                                 "πάνω από 130–160% εθνικής διαμέσου/μέσου")))
}))
x_hi <- max(sapply(fits, function(f) qdagum(0.99, f$par[["a"]], f$par[["p"]], f$par[["b"]])))
cdfs <- do.call(rbind, lapply(seq_len(nrow(groups)), function(i) {
  f <- fits[[groups$age[i]]]
  x <- seq(1, x_hi, length.out = 600)
  data.frame(group = groups$label[i], x = x, F = pdagum(x, f$par[["a"]], f$par[["p"]], f$par[["b"]]))
}))
errs <- do.call(rbind, lapply(seq_len(nrow(groups)), function(i) {
  cf <- compare_fit(fits[[groups$age[i]]])
  m  <- cf[cf$target == "mean", ]
  data.frame(group = groups$label[i],
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
  scale_shape_manual(values = c(16, 21, 24), name = NULL) +
  scale_x_continuous(labels = k_eur, breaks = seq(0, 60000, 10000)) +
  scale_y_continuous(labels = function(v) paste0(100 * v, "%"), breaks = seq(0, 1, 0.2)) +
  labs(title = "Έλεγχος προσαρμογής ανά ηλικιακή ομάδα",
       subtitle = sprintf("Αθροιστική κατανομή του μοντέλου (γραμμή) και τα σημεία της Eurostat στα οποία έγινε το fit.\nΤα όρια είναι ποσοστά της εθνικής διαμέσου (%s) ή του εθνικού μέσου (%s).",
                          eur(real("TOTAL", "MED_EI")), eur(real("TOTAL", "MEAN_EI"))),
       x = "Ισοδύναμο διαθέσιμο εισόδημα (χιλ. € / έτος)", y = "Αθροιστικό ποσοστό της ομάδας",
       caption = sprintf("Πηγή: Eurostat, EU-SILC %d (ilc_li02, ilc_di20, ilc_di03). Για τα κάτω των 65 η Eurostat δεν δίνει σημεία πάνω από τη διάμεσο· το fit εκεί στηρίζεται στον μέσο και στους λόγους S80/S20, S80/S50, S50/S20.", year)) +
  theme_chart + theme(legend.position = "top", legend.justification = "left",
                      legend.text = element_text(colour = ink_2, size = 9),
                      strip.text = element_text(colour = ink, face = "bold", size = 11),
                      panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.3),
                      panel.spacing = unit(1.5, "lines"))
save_chart(g2, sprintf("figures/dagum_fit_%d_age.png", year), width = 11, height = 6)

# ---- 3. Comparison of the three densities -----------------------------------

comp <- rbind(
  data.frame(name = "Όλος ο πληθυσμός", par = I(list(total$par))),
  data.frame(name = paste0("Άτομα ", groups$label), par = I(unname(lapply(fits, `[[`, "par"))))
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
tab <- sprintf("Πραγματικά (Eurostat ilc_di03) — διάμεσος / μέσος:  όλοι %s / %s  ·  κάτω των 65 %s / %s  ·  18–64 %s / %s",
               eur(real("TOTAL", "MED_EI")), eur(real("TOTAL", "MEAN_EI")),
               eur(real("Y_LT65", "MED_EI")), eur(real("Y_LT65", "MEAN_EI")),
               eur(real("Y18-64", "MED_EI")), eur(real("Y18-64", "MEAN_EI")))
# Direct labels to the right of x0, where the three curves separate, spread
# vertically so they never overlap, each tied to its curve by a short leader.
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

g3 <- ggplot(dens, aes(x, y, colour = name)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = meds, size = 2.6, stroke = 1.2, fill = surface, shape = 21) +
  geom_segment(data = labs_c, aes(x = x, y = y, xend = x_lab, yend = y_lab), linewidth = 0.4,
               show.legend = FALSE) +
  geom_point(data = labs_c, aes(x = x, y = y), size = 1.4, show.legend = FALSE) +
  geom_text(data = labs_c, aes(x = x_lab, y = y_lab, label = name), colour = ink, hjust = -0.05,
            size = 3.3, show.legend = FALSE) +
  scale_colour_manual(values = series, name = NULL) +
  scale_x_continuous(labels = k_eur, breaks = seq(0, 60000, 5000), limits = c(0, x_cmp),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(labels = NULL, expand = expansion(mult = c(0, 0.08))) +
  labs(title = sprintf("Ισοδύναμο διαθέσιμο εισόδημα ανά ηλικιακή ομάδα, %d", year - 1),
       subtitle = paste0("Εκτιμώμενες πυκνότητες (Dagum). Ο κύκλος σημειώνει τη διάμεσο του μοντέλου.\n", tab),
       x = "Ισοδύναμο διαθέσιμο εισόδημα (χιλ. € / έτος)", y = "Πυκνότητα",
       caption = sprintf(paste0(
         "Πηγή: ΕΛΣΤΑΤ και Eurostat, EU-SILC %d (εισοδήματα %d). «Όλος ο πληθυσμός»: fit σε όρια και μερίδια ομάδων της ΕΛΣΤΑΤ.\n",
         "Ηλικιακές ομάδες: fit σε πίνακες Eurostat ανά ηλικία. Η ηλικία είναι του ατόμου· το εισόδημα είναι του νοικοκυριού του, με όλα τα μέλη του."),
         year, year - 1)) +
  theme_chart + theme(legend.position = "top", legend.justification = "left",
                      legend.text = element_text(colour = ink_2, size = 10),
                      panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.3),
                      panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.3))
save_chart(g3, sprintf("figures/dagum_compare_%d.png", year))

for (f in fits) print(f)
message("wrote figures/dagum_density_", year, "_{18-64,lt65}.png, figures/dagum_fit_", year,
        "_age.png, figures/dagum_compare_", year, ".png")
