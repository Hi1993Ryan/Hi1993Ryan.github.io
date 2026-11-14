#!/usr/bin/env Rscript
# Directly renders web-ready Tufte-style cohort chart (transparent PNG).
# Usage from site repo root:
#   Rscript figures/cohort-history/build.R

arg <- grep("^--file=", commandArgs(), value = TRUE)
src_dir <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[1]))) else getwd()
root <- if (basename(src_dir) == "cohort-history") {
  normalizePath(file.path(src_dir, "../.."))
} else {
  normalizePath(".")
}
out_dir <- file.path(root, "static", "figures", "cohort-history")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

events <- data.frame(
  year = c(1911, 1937, 1949, 1966, 1978, 2001, 2020),
  label = c(
    "1911\nXinhai Revolution",
    "1937\nFull-scale war\nwith Japan",
    "1949\nFounding\nof the PRC",
    "1966\nCultural Revolution\nbegins",
    "1978\nReform and\nopening begins",
    "2001\nWTO accession",
    "2020\nCOVID-19"
  )
)
cohorts <- data.frame(
  birth = c(1910, 1935, 1960, 1980, 2000),
  peak = c(51, 44, 53, 35, 34),
  height = c(.65, .82, .75, .92, .64),
  rise = c(20, 14, 23, 11, 12),
  fall = c(14, 29, 17, 25, 22),
  color = c("#775A9C", "#007574", "#CB5535", "#234F8D", "#8C6709")
)

# Illustrative curves; event lines are context, not causal estimates.
font <- "PT Sans Narrow"

themes <- list(
  light = list(
    event = "#C8CBCE",
    label = "#5A6066",
    axis = "#6E747A",
    axis_line = "#A3A7A8",
    caption = "#5A6066",
    title = "#1A1A1A"
  ),
  dark = list(
    event = "#4A5058",
    label = "#B8BEC6",
    axis = "#A8AEB6",
    axis_line = "#6A7078",
    caption = "#A8AEB6",
    title = "#E8E8EA"
  )
)

draw <- function(th) {
  par(mar = c(5.1, 4.5, 5.5, 1.5), family = font, las = 1, xaxs = "i", yaxs = "i")
  plot(NA, xlim = c(1900, 2037), ylim = c(0, 1.17), axes = FALSE, xlab = "", ylab = "")
  segments(events$year, 0, events$year, .995, col = th$event, lty = 3, lwd = .8)
  text(events$year, 1.145, events$label, adj = c(.5, 1), cex = .82, col = th$label)
  for (i in seq_len(nrow(cohorts))) {
    z <- cohorts[i, ]
    age <- seq(18, min(80, 2026 - z$birth), by = .1)
    year <- z$birth + age
    y <- .12 + z$height * .86 * exp(-.5 * ((age - z$peak) / ifelse(age < z$peak, z$rise, z$fall))^2)
    lines(year, y, col = z$color, lwd = 2.45)
    xx <- tail(year, 1)
    yy <- tail(y, 1)
    ly <- c(yy, yy, .664, yy, .510)[i]
    lx <- xx + 1.2
    if (ly != yy) {
      segments(xx, yy, xx + 1.7, ly, col = z$color, lwd = .75)
      lx <- xx + 2.1
    }
    if (z$birth == 1935) {
      lx <- xx - 7.5
      ly <- yy - .045
    }
    text(lx, ly, paste("Born", z$birth), adj = c(0, .5), col = z$color, font = 2, cex = .91)
  }
  axis(1, at = c(seq(1900, 2020, 20), 2026), lwd = 0, lwd.ticks = .7, col = th$axis, cex.axis = .88, tck = -.016, col.axis = th$axis)
  axis(2, at = seq(0, 1, .25), labels = c("0", ".25", ".50", ".75", "1"), lwd = 0, lwd.ticks = .7,
       col = th$axis, cex.axis = .86, tck = -.010, col.axis = th$axis)
  segments(1900, 0, 2026, 0, col = th$axis_line, lwd = .7)
  mtext("Calendar year", side = 1, line = 2.4, adj = 0, col = th$axis)
  mtext("Illustrative outcome", side = 2, line = 2.8, las = 0, col = th$axis)
  title(main = "Generations Across China's Historical Time", adj = 0, line = 3, cex.main = 1.8, col.main = th$title)
  mtext(
    "Curves are hypothetical. Event lines provide context, not causal estimates.",
    side = 1, line = 3.65, adj = 0, cex = .83, col = th$caption
  )
}

for (nm in names(themes)) {
  path <- file.path(out_dir, sprintf("generations-historical-time-%s.png", nm))
  ragg::agg_png(path, width = 2400, height = 1260, res = 200, background = "transparent")
  draw(themes[[nm]])
  invisible(dev.off())
  message("Wrote ", path)
}

message("Done. Transparent PNGs ready for the homepage.")
