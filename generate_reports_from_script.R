
# Knit park-level reports
filepath = "C:/NETN/R_Dev/rockyintertidal_reports/"

rmarkdown::render(paste0(filepath, 'park_summary.Rmd'),
                  output_file = paste0(filepath, "BOHA_summary.html"),
                  params = list(year_curr = 2021,
                                park = "BOHA",
                                all_years = TRUE))


rmarkdown::render(paste0(filepath, 'park_summary.Rmd'),
                  output_file = paste0(filepath, "ACAD_summary.html"),
                  params = list(year_curr = 2021,
                                park = "ACAD",
                                all_years = TRUE))


