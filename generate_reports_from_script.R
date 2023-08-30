
# Knit park-level reports
rmarkdown::render('park_summary.Rmd',
                  output_file = "BOHA_summary.html",
                  params = list(year_curr = 2021,
                                park = "BOHA",
                                all_years = TRUE))


rmarkdown::render('park_summary.Rmd',
                  output_file = "ACAD_summary.html",
                  params = list(year_curr = 2021,
                                park = "ACAD",
                                all_years = TRUE))
