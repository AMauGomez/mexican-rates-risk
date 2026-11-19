# Part A. Factors of the Mexican yield curve: SVD and principal components ----
source("R/00_setup.R")

m <- read.csv(file.path(DATA, "curva_mensual.csv"))
m$fecha <- as.Date(m$fecha)
# October 2006 is the first month with a 30-year bond; September 2026 the last full quarter
m <- m[m$fecha >= as.Date("2006-10-01") & m$fecha <= as.Date("2026-09-30"), ]
m$q <- paste0(format(m$fecha, "%Y"), "Q", (as.integer(format(m$fecha, "%m")) - 1) %/% 3 + 1)

# Long bonds are auctioned every few weeks, so monthly averages have gaps.
# Quarterly averages fill almost all of them.
Q <- aggregate(m[, TENORS], by = list(q = m$q), FUN = function(x) mean(x, na.rm = TRUE))
Q[, TENORS] <- lapply(Q[, TENORS], function(x) { x[is.nan(x)] <- NA; x })
missing_cells <- sum(is.na(Q[, TENORS]))
missing_where <- which(is.na(Q[, TENORS]), arr.ind = TRUE)
# The few remaining gaps: linear interpolation in time, tenor by tenor
for (v in TENORS) {
  if (anyNA(Q[[v]])) Q[[v]] <- approx(seq_len(nrow(Q)), Q[[v]], xout = seq_len(nrow(Q)), rule = 2)$y
}
Y <- as.matrix(Q[, TENORS]); rownames(Y) <- Q$q
qdate <- as.Date(paste0(substr(Q$q, 1, 4), "-", (as.integer(substr(Q$q, 6, 6)) - 1) * 3 + 2, "-15"))
saveRDS(list(Y = Y, qdate = qdate), file.path(RES, "curve_quarterly.rds"))

# Quarterly changes, centred
dY <- diff(Y)
X <- scale(dY, center = TRUE, scale = FALSE)

# SVD: X = U D V'. Columns of V are the loadings; U D are the factor scores.
s <- svd(X)
share <- s$d^2 / sum(s$d^2)
# Same thing as principal components: eigenvalues of the covariance = d^2 / (n - 1)
pc <- prcomp(dY)
stopifnot(isTRUE(all.equal(pc$sdev^2, s$d^2 / (nrow(X) - 1))))

# Orient the factors so they read naturally: level up = rates up; slope up = long end up
V <- s$v[, 1:3]
if (sum(V[, 1]) < 0) V[, 1] <- -V[, 1]
if (V[9, 2] < V[1, 2]) V[, 2] <- -V[, 2]
if (V[5, 3] > 0) V[, 3] <- -V[, 3]
scores <- X %*% V
colnames(V) <- colnames(scores) <- c("Level", "Slope", "Curvature")
saveRDS(list(scores = scores, loadings = V, share = share, qdate = qdate[-1]), file.path(RES, "curve_factors.rds"))

# Same decomposition on levels (not changes)
s_lev <- svd(scale(Y, scale = FALSE))
share_lev <- s_lev$d^2 / sum(s_lev$d^2)

summary_curve <- data.frame(
  quarters = nrow(Y), missing_cells = missing_cells, total_cells = length(Y),
  share_change_1 = share[1], share_change_2 = share[2], share_change_3 = share[3],
  share_level_1 = share_lev[1], share_level_2 = share_lev[2],
  min_rate = min(Y), max_rate = max(Y),
  first_q = rownames(Y)[1], last_q = rownames(Y)[nrow(Y)])
write.csv(summary_curve, file.path(RES, "curve_summary.csv"), row.names = FALSE)
write.csv(data.frame(tenor = LABELS, maturity = MATURITY, round(V, 3)), file.path(RES, "curve_loadings.csv"), row.names = FALSE)

# Figures ----------------------------------------------------------------------
lv <- data.frame(date = rep(qdate, 3), rate = c(Y[, "cetes28"], Y[, "bono3"], Y[, "bono10"]),
                 tenor = rep(c("Cetes 28 days", "Bono M 3 years", "Bono M 10 years"), each = nrow(Y)))
lv$tenor <- factor(lv$tenor, levels = c("Cetes 28 days", "Bono M 3 years", "Bono M 10 years"))
p1 <- ggplot(lv, aes(date, rate, colour = tenor)) + geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c(RED, ORANGE, BLUE)) +
  labs(x = NULL, y = "Yield (% per year)", colour = NULL,
       title = "Mexican government yields, quarterly averages") +
  theme(legend.position = "top")
save_fig(p1, "curve_levels.png")

ld <- data.frame(maturity = rep(MATURITY, 3), loading = c(V),
                 factor = factor(rep(colnames(V), each = 9), levels = colnames(V)))
ld$factor <- factor(ld$factor, labels = sprintf("%s (%.1f%%)", colnames(V), 100 * share[1:3]))
p2 <- ggplot(ld, aes(maturity, loading, colour = factor)) +
  geom_hline(yintercept = 0, colour = GREY) + geom_line(linewidth = 0.9) + geom_point(size = 2) +
  scale_x_log10(breaks = MATURITY, labels = LABELS) +
  scale_colour_manual(values = c(BLUE, RED, GREEN)) +
  labs(x = "Maturity (log scale)", y = "Loading", colour = "Factor (share of variance)",
       title = "Three factors explain how the curve moves",
       subtitle = "Loadings of the first three principal components of quarterly changes") +
  theme(legend.position = "top")
save_fig(p2, "curve_loadings.png")

message(sprintf("Curve: %d quarters, factor shares %.1f / %.1f / %.1f %%",
                nrow(Y), 100 * share[1], 100 * share[2], 100 * share[3]))
