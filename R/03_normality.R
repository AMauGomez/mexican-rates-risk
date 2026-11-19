# Part B. Are curve changes multivariate normal? -------------------------------
source("R/00_setup.R")

cf <- readRDS(file.path(RES, "curve_quarterly.rds"))
dY <- diff(cf$Y)
fac <- readRDS(file.path(RES, "curve_factors.rds"))

mardia9 <- mvn(dY, mvn_test = "mardia")$multivariate_normality
hz9 <- mvn(dY, mvn_test = "hz")$multivariate_normality
mardia3 <- mvn(fac$scores, mvn_test = "mardia")$multivariate_normality

tests <- rbind(
  data.frame(data = "9 tenors", mardia9[, c("Test", "Statistic", "p.value")]),
  data.frame(data = "9 tenors", hz9[, c("Test", "Statistic", "p.value")]),
  data.frame(data = "3 factors", mardia3[, c("Test", "Statistic", "p.value")]))
write.csv(tests, file.path(RES, "normality_tests.csv"), row.names = FALSE)

# Squared Mahalanobis distances vs chi-square(p) quantiles
d2 <- mahalanobis(dY, colMeans(dY), cov(dY))
n <- nrow(dY); p <- ncol(dY)
qq <- data.frame(theoretical = qchisq((seq_len(n) - 0.5) / n, df = p), observed = sort(d2))
write.csv(data.frame(n = n, p = p, max_d2 = max(d2), q99 = qchisq(0.99, p),
                     n_above_q99 = sum(d2 > qchisq(0.99, p)), expected_above = 0.01 * n,
                     kurt_level = excess_kurtosis(fac$scores[, 1]),
                     kurt_slope = excess_kurtosis(fac$scores[, 2]),
                     kurt_curv = excess_kurtosis(fac$scores[, 3])),
          file.path(RES, "normality_summary.csv"), row.names = FALSE)

p1 <- ggplot(qq, aes(theoretical, observed)) +
  geom_abline(colour = GREY, linetype = 2) + geom_point(colour = BLUE, size = 1.8) +
  labs(x = expression(chi[9]^2 ~ "quantiles"), y = "Ordered squared Mahalanobis distances",
       title = "Quarterly curve changes have heavier tails than a normal",
       subtitle = "Under multivariate normality the points would follow the dashed line")
save_fig(p1, "mahalanobis_qq.png", w = 6, h = 4.2)
message("Normality tests written.")
