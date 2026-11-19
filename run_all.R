# Runs the whole analysis and renders the report (about 2 minutes) -------------
for (s in c("R/01_get_data.R", "R/02_curve_factors.R", "R/03_normality.R",
            "R/04_regimes.R", "R/05_fx_garch_var.R")) {
  message("== ", s)
  source(s, local = new.env())
}
if (requireNamespace("quarto", quietly = TRUE)) quarto::quarto_render("index.qmd")
