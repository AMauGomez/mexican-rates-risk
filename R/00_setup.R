# Shared setup: packages, paths and helpers ------------------------------------
suppressPackageStartupMessages({
  library(ggplot2)
  library(mclust)    # Gaussian mixtures chosen by BIC
  library(MVN)       # multivariate normality tests
  library(rugarch)   # GARCH models and VaR backtests
  library(FinTS)     # ARCH LM test
})

DATA <- "data"
FIG <- "figures"
RES <- "results"
dir.create(FIG, showWarnings = FALSE)
dir.create(RES, showWarnings = FALSE)

TENORS <- c("cetes28", "cetes91", "cetes182", "cetes364",
            "bono3", "bono5", "bono10", "bono20", "bono30")
# Maturity of each tenor in years
MATURITY <- c(28, 91, 182, 364) / 364
MATURITY <- c(MATURITY, 3, 5, 10, 20, 30)
LABELS <- c("28d", "91d", "182d", "1y", "3y", "5y", "10y", "20y", "30y")

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(), plot.title.position = "plot"))
BLUE <- "#1a73e8"; RED <- "#d93025"; GREEN <- "#188038"; GREY <- "#80868b"; ORANGE <- "#e37400"

save_fig <- function(p, name, w = 7, h = 4) ggsave(file.path(FIG, name), p, width = w, height = h, dpi = 150, bg = "white")

excess_kurtosis <- function(x) mean((x - mean(x))^4) / mean((x - mean(x))^2)^2 - 3
