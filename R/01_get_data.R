# Download the data from Banco de México's SIE API -----------------------------
# Source: Banco de México, Sistema de Información Económica (SIE),
# https://www.banxico.org.mx/SieAPIRest/service/v1 . Reproduced citing the source,
# as Banco de México requires.
#
# The processed files are already in data/, so every other script runs without
# this step. To refresh them, request a free token at
# https://www.banxico.org.mx/SieAPIRest/service/v1/token and set the environment
# variable BANXICO_TOKEN (never write the token into a file under version control).

source("R/00_setup.R")
library(jsonlite)

token <- Sys.getenv("BANXICO_TOKEN")
if (!nzchar(token)) {
  message("BANXICO_TOKEN not set: using the files already in data/.")
} else {
  sie <- function(ids, from, to) {
    url <- sprintf("https://www.banxico.org.mx/SieAPIRest/service/v1/series/%s/datos/%s/%s",
                   paste(ids, collapse = ","), from, to)
    con <- url(url, headers = c("Bmx-Token" = token))
    on.exit(close(con))
    fromJSON(paste(readLines(con, warn = FALSE), collapse = ""), simplifyVector = FALSE)$bmx$series
  }
  as_series <- function(s) {
    d <- s$datos
    ok <- vapply(d, function(o) !o$dato %in% c("N/E", ""), logical(1))
    data.frame(fecha = as.Date(vapply(d[ok], `[[`, "", "fecha"), "%d/%m/%Y"),
               valor = as.numeric(gsub(",", "", vapply(d[ok], `[[`, "", "dato"))))
  }

  # Monthly average yields of government securities (percent per year)
  ids <- c(SF4830 = "cetes28", SF3338 = "cetes91", SF3270 = "cetes182", SF3367 = "cetes364",
           SF17990 = "bono3", SF18608 = "bono5", SF33229 = "bono7", SF30057 = "bono10",
           SF41841 = "bono20", SF60719 = "bono30")
  out <- NULL
  for (s in sie(names(ids), "2000-01-01", format(Sys.Date(), "%Y-%m-%d"))) {
    x <- as_series(s); names(x)[2] <- ids[[s$idSerie]]
    out <- if (is.null(out)) x else merge(out, x, all = TRUE)
  }
  out <- out[order(out$fecha), c("fecha", unname(ids))]
  write.csv(out, file.path(DATA, "curva_mensual.csv"), row.names = FALSE)

  # Daily FIX exchange rate, pesos per US dollar (SF43718)
  fx <- as_series(sie("SF43718", "2006-01-01", format(Sys.Date(), "%Y-%m-%d"))[[1]])
  names(fx)[2] <- "fix"
  write.csv(fx, file.path(DATA, "fix_diario.csv"), row.names = FALSE)
  message("Data refreshed from the SIE API.")
}
