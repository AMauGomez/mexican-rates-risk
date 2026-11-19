# Part C. Regimes with Gaussian mixtures (model-based clustering) ---------------
source("R/00_setup.R")

# C1. Quarterly curve factors (level and slope) --------------------------------
fac <- readRDS(file.path(RES, "curve_factors.rds"))
mq <- Mclust(fac$scores[, 1:2], G = 1:4)
bic_q <- as.data.frame(unclass(mq$BIC))
write.csv(data.frame(model = mq$modelName, G = mq$G, n = nrow(fac$scores),
                     best_bic = max(mq$BIC, na.rm = TRUE)),
          file.path(RES, "regimes_quarterly.csv"), row.names = FALSE)

# C2. Daily USD/MXN returns ----------------------------------------------------
f <- read.csv(file.path(DATA, "fix_diario.csv")); f$fecha <- as.Date(f$fecha)
ret <- 100 * diff(log(f$fix)); dates <- f$fecha[-1]
md <- Mclust(ret, G = 1:4)
pr <- md$parameters
comp <- data.frame(component = seq_len(md$G), share = pr$pro, mean = pr$mean,
                   sd = sqrt(rep_len(pr$variance$sigmasq, md$G)))
comp <- comp[order(comp$sd), ]
comp$regime <- c("Calm", "Normal", "Stress")[seq_len(nrow(comp))]
write.csv(comp, file.path(RES, "regimes_fx_components.csv"), row.names = FALSE)
write.csv(data.frame(model = md$modelName, G = md$G, n = length(ret)), file.path(RES, "regimes_fx.csv"), row.names = FALSE)

stress <- comp$component[comp$regime == "Stress"]
post <- md$z[, stress]
by_year <- aggregate(data.frame(stress_share = post), by = list(year = as.integer(format(dates, "%Y"))), FUN = mean)
write.csv(by_year, file.path(RES, "regimes_fx_by_year.csv"), row.names = FALSE)

# Figure: histogram vs normal vs mixture (log density shows the tails)
grid <- seq(-6, 7.5, length.out = 600)
dens_mix <- rowSums(sapply(seq_len(md$G), function(g) pr$pro[g] * dnorm(grid, pr$mean[g], sqrt(rep_len(pr$variance$sigmasq, md$G)[g]))))
dd <- rbind(data.frame(x = grid, y = dnorm(grid, mean(ret), sd(ret)), model = "Normal"),
            data.frame(x = grid, y = dens_mix, model = sprintf("Mixture of %d normals (BIC)", md$G)))
h <- hist(ret, breaks = seq(-6, 7.5, by = 0.25), plot = FALSE)
hd <- data.frame(x = h$mids, y = h$density)
dd$model <- factor(dd$model, levels = c("Normal", sprintf("Mixture of %d normals (BIC)", md$G)))
p1 <- ggplot() +
  geom_point(data = hd[hd$y > 0, ], aes(x, y), colour = "grey40", size = 1.4) +
  geom_line(data = dd, aes(x, y, colour = model), linewidth = 0.9) +
  scale_y_log10() + coord_cartesian(ylim = c(1e-5, 1)) +
  scale_colour_manual(values = c(RED, BLUE)) +
  labs(x = "Daily change in USD/MXN (%)", y = "Density (log scale)", colour = NULL,
       title = "Daily peso moves: a normal misses the tails, a mixture does not",
       subtitle = "Points: observed density (histogram, 0.25% bins). Log scale shows the tails") +
  theme(legend.position = "top")
save_fig(p1, "fx_mixture.png")

p2 <- ggplot(by_year, aes(year, 100 * stress_share)) + geom_col(fill = RED) +
  labs(x = NULL, y = "% of days in the stress regime",
       title = "Stress days cluster in time",
       subtitle = "Average posterior probability of the high-volatility component, by year")
save_fig(p2, "fx_stress_by_year.png", h = 3.6)
message(sprintf("Quarterly regimes: %s, G = %d. Daily FX: %s, G = %d.", mq$modelName, mq$G, md$modelName, md$G))
