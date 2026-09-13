# Shared ggplot styling and the Dagum density chart (quintile bands).
# Requires ggplot2 and R/income_distribution.R.

ink       <- "#0b0b0b"
ink_2     <- "#52514e"
surface   <- "#fcfcfb"
grid_col  <- "#e6e5e1"
seq_blues <- c("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281")  # sequential steps 250-650
series    <- c("#2a78d6", "#eb6834", "#1baf7a")                       # categorical slots 1-3

eur <- function(x) paste0(formatC(x, format = "d", big.mark = ".", decimal.mark = ","), " €")
pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 1, decimal.mark = ","), "%")
k_eur <- function(x) formatC(x / 1000, format = "d")

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

save_chart <- function(g, file, width = 10, height = 6) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  ggsave(file, g, width = width, height = height, dpi = 150, bg = surface, device = png, type = "cairo")
}

# Density of a fitted Dagum with its five quintile bands, each labelled with its
# share of total income, and the model median and mean. x axis ends at P99.
plot_dagum_density <- function(par, title, subtitle, caption,
                               share_note = "μερίδιο συνολικού\nεισοδήματος ανά πεμπτημόριο ↓") {
  a <- par[["a"]]; p <- par[["p"]]; b <- par[["b"]]
  x_max <- qdagum(0.99, a, p, b)
  cuts  <- c(0, qdagum(c(0.2, 0.4, 0.6, 0.8), a, p, b))
  bands <- do.call(rbind, lapply(1:5, function(i) {
    hi <- if (i < 5) cuts[i + 1] else x_max
    x  <- seq(max(cuts[i], 1), hi, length.out = 200)
    data.frame(quintile = factor(i), x = x, y = ddagum(x, a, p, b))
  }))
  curve <- data.frame(x = seq(1, x_max, length.out = 800))
  curve$y <- ddagum(curve$x, a, p, b)

  shares <- group_shares_dagum(5, a, p)
  labs_q <- do.call(rbind, lapply(1:5, function(i) {
    hi <- if (i < 5) cuts[i + 1] else qdagum(0.93, a, p, b)
    data.frame(x = (max(cuts[i], qdagum(0.02, a, p, b)) + hi) / 2,
               label = sprintf("Q%d\n%s", i, pct(shares[i])))
  }))
  y_top <- max(curve$y)
  med   <- qdagum(0.5, a, p, b)
  mu    <- mean_dagum(a, p, b)
  marks <- data.frame(x = c(med, mu), y = ddagum(c(med, mu), a, p, b))

  ggplot() +
    geom_area(data = bands, aes(x, y, fill = quintile, group = quintile),
              colour = surface, linewidth = 0.6, show.legend = FALSE) +
    density_grid(x_max, y_top * 1.04) +
    geom_line(data = curve, aes(x, y), colour = ink, linewidth = 0.6) +
    scale_fill_manual(values = seq_blues) +
    geom_segment(data = marks, aes(x = x, xend = x, y = 0, yend = y),
                 colour = ink, linewidth = 0.4, linetype = "22") +
    geom_segment(data = marks, aes(x = x, xend = x, y = y, yend = y_top * 1.06),
                 colour = ink_2, linewidth = 0.3) +
    annotate("text", x = med, y = y_top * 1.07, hjust = 1, vjust = 0,
             label = paste0("διάμεσος ", eur(round(med))), size = 3.2, colour = ink) +
    annotate("text", x = mu, y = y_top * 1.07, hjust = 0, vjust = 0,
             label = paste0("μέσος ", eur(round(mu))), size = 3.2, colour = ink) +
    geom_text(data = labs_q, aes(x, y = -0.07 * y_top, label = label),
              size = 3.1, colour = ink_2, lineheight = 0.9, vjust = 1) +
    annotate("text", x = x_max, y = 0.2 * y_top, hjust = 1, size = 3, colour = ink_2,
             label = share_note, lineheight = 0.9) +
    scale_x_continuous(labels = k_eur, breaks = seq(0, 60000, 5000),
                       expand = expansion(mult = c(0, 0.01))) +
    scale_y_continuous(labels = NULL, expand = expansion(mult = c(0.02, 0.12))) +
    coord_cartesian(ylim = c(-0.2 * y_top, y_top * 1.12), clip = "off") +
    labs(title = title, subtitle = subtitle, caption = caption,
         x = "Ισοδύναμο διαθέσιμο εισόδημα (χιλ. € / έτος)", y = "Πυκνότητα") +
    theme_chart + theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank())
}

# Grid drawn as a layer, so it can sit over filled areas while staying inside the
# data region (y from 0 to y_max) and under any text added afterwards.
density_grid <- function(x_max, y_max, x_step = 5000) {
  xb <- seq(0, x_max, by = x_step)
  yb <- pretty(c(0, y_max)); yb <- yb[yb > 0 & yb <= y_max]
  list(annotate("segment", x = xb, xend = xb, y = 0, yend = y_max, colour = grid_col, linewidth = 0.3),
       annotate("segment", x = 0, xend = x_max, y = yb, yend = yb, colour = grid_col, linewidth = 0.3))
}
