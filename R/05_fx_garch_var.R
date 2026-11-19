# Part D. Volatility of the peso and Value at Risk -----------------------------
source("R/00_setup.R")

f <- read.csv(file.path(DATA, "fix_diario.csv")); f$fecha <- as.Date(f$fecha)
ret <- 100 * diff(log(f$fix)); dates <- f$fecha[-1]

# Stylised facts ---------------------------------------------------------------
lb <- Box.test(ret^2, lag = 10, type = "Ljung-Box")
arch <- ArchTest(ret, lags = 10)
facts <- data.frame(n = length(ret), first = min(dates), last = max(dates),
                    mean = mean(ret), sd = sd(ret), excess_kurtosis = excess_kurtosis(ret),
                    max = max(ret), max_date = dates[which.max(ret)],
                    min = min(ret), min_date = dates[which.min(ret)],
                    lb_sq_stat = unname(lb$statistic), lb_sq_p = lb$p.value,
                    arch_stat = unname(arch$statistic), arch_p = arch$p.value)
write.csv(facts, file.path(RES, "fx_facts.csv"), row.names = FALSE)

# GARCH(1,1) on the whole sample, normal vs Student-t errors -------------------
spec <- function(dist, mean = TRUE) ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = mean), distribution.model = dist)
fit_n <- ugarchfit(spec("norm"), ret)
fit_t <- ugarchfit(spec("std"), ret)
ct <- coef(fit_t)
pers <- unname(ct["alpha1"] + ct["beta1"])
garch <- data.frame(model = c("GARCH-normal", "GARCH-t"),
                    loglik = c(likelihood(fit_n), likelihood(fit_t)),
                    omega = c(coef(fit_n)["omega"], ct["omega"]),
                    alpha = c(coef(fit_n)["alpha1"], ct["alpha1"]),
                    beta = c(coef(fit_n)["beta1"], ct["beta1"]),
                    df = c(NA, ct["shape"]),
                    persistence = c(sum(coef(fit_n)[c("alpha1", "beta1")]), pers),
                    half_life_days = c(log(0.5) / log(sum(coef(fit_n)[c("alpha1", "beta1")])), log(0.5) / log(pers)))
write.csv(garch, file.path(RES, "garch_fit.csv"), row.names = FALSE)

# Out-of-sample 1-day VaR at 99% ------------------------------------------------
# Position: an investor long Mexican pesos. Daily P&L (in %) = -(change in USD/MXN):
# the investor loses when the peso depreciates.
pnl <- -ret
alpha <- 0.01
test <- which(dates >= as.Date("2016-01-01"))
act <- pnl[test]

var_normal <- sapply(test, function(t) qnorm(alpha, 0, sd(pnl[(t - 250):(t - 1)])))
var_hs250 <- sapply(test, function(t) quantile(pnl[(t - 250):(t - 1)], alpha, names = FALSE))
var_hs500 <- sapply(test, function(t) quantile(pnl[(t - 500):(t - 1)], alpha, names = FALSE))
roll <- function(dist) {
  r <- ugarchroll(spec(dist, mean = FALSE), data = pnl, n.ahead = 1, forecast.length = length(test),
                  refit.every = 50, refit.window = "moving", window.size = 1500,
                  calculate.VaR = TRUE, VaR.alpha = alpha, solver = "hybrid")
  as.numeric(r@forecast$VaR[, 1])
}
var_gn <- roll("norm")
var_gt <- roll("std")

models <- list("Normal, 250-day window" = var_normal,
               "Historical simulation, 250 days" = var_hs250,
               "Historical simulation, 500 days" = var_hs500,
               "GARCH(1,1), normal errors" = var_gn,
               "GARCH(1,1), Student-t errors" = var_gt)
bt <- do.call(rbind, lapply(names(models), function(k) {
  v <- VaRTest(alpha, act, models[[k]])
  data.frame(model = k, days = length(act), exceptions = v$actual.exceed, expected = v$expected.exceed,
             rate = v$actual.exceed / length(act), kupiec_p = v$uc.LRp, christoffersen_p = v$cc.LRp,
             mean_var = mean(models[[k]]))
}))
write.csv(bt, file.path(RES, "var_backtest.csv"), row.names = FALSE)

yr <- format(dates[test], "%Y")
per_year <- data.frame(year = sort(unique(yr)),
                       days = as.integer(table(yr)),
                       normal = as.integer(tapply(act < var_normal, yr, sum)),
                       hs250 = as.integer(tapply(act < var_hs250, yr, sum)),
                       garch_t = as.integer(tapply(act < var_gt, yr, sum)))
write.csv(per_year, file.path(RES, "var_exceptions_by_year.csv"), row.names = FALSE)

# Figure: P&L with the two VaR lines --------------------------------------------
dv <- data.frame(date = dates[test], pnl = act, normal = var_normal, garch_t = var_gt)
p1 <- ggplot(dv, aes(date)) +
  geom_point(aes(y = pnl), colour = ifelse(dv$pnl < dv$garch_t, RED, "grey60"), size = 0.5) +
  geom_line(aes(y = normal, colour = "Normal, 250-day window"), linewidth = 0.4) +
  geom_line(aes(y = garch_t, colour = "GARCH(1,1), Student-t"), linewidth = 0.4) +
  scale_colour_manual(values = c("GARCH(1,1), Student-t" = BLUE, "Normal, 250-day window" = ORANGE)) +
  labs(x = NULL, y = "Daily P&L of a long-peso position (%)", colour = "99% VaR",
       title = "Out-of-sample 99% VaR, 2016–2026",
       subtitle = "Red points: losses beyond the GARCH-t VaR. GARCH adapts within days; the 250-day window lags") +
  theme(legend.position = "top")
save_fig(p1, "var_backtest.png", w = 8, h = 4.2)

ey <- rbind(data.frame(year = per_year$year, model = "Normal, 250-day window", n = per_year$normal),
            data.frame(year = per_year$year, model = "Historical simulation, 250 days", n = per_year$hs250),
            data.frame(year = per_year$year, model = "GARCH(1,1), Student-t", n = per_year$garch_t))
p2 <- ggplot(ey, aes(year, n, fill = model)) + geom_col(position = "dodge") +
  geom_hline(yintercept = 2.5, linetype = 2, colour = GREY) +
  scale_fill_manual(values = c(BLUE, GREEN, ORANGE)) +
  labs(x = NULL, y = "VaR exceptions", fill = NULL,
       title = "Exceptions per year: about 2.5 expected (dashed line)") +
  theme(legend.position = "top")
save_fig(p2, "var_exceptions_by_year.png", w = 8, h = 3.8)
message("GARCH and VaR backtest written.")
