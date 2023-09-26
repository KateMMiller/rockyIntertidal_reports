
# Knit full park-level reports
filepath = "C:/NETN/R_Dev/rockyintertidal_reports/"

rmarkdown::render(paste0(filepath, 'park_summary.Rmd'),
                  output_file = paste0(filepath, "BOHA_summary.html"),
                  params = list(year_curr = 2021,
                                park = "BOHA",
                                all_years = TRUE))


filepath = "C:/NETN/R_Dev/rockyintertidal_reports/"

rmarkdown::render(paste0(filepath, 'park_summary.Rmd'),
                  output_file = paste0(filepath, "ACAD_summary.html"),
                  params = list(year_curr = 2021,
                                park = "ACAD",
                                all_years = TRUE))


# Knit abbreviated park-level reports
filepath = "C:/NETN/R_Dev/rockyintertidal_reports/"

rmarkdown::render(paste0(filepath, 'park_summary_abbrev.Rmd'),
                  output_file = paste0(filepath, "BOHA_summary_abbrev.html"),
                  params = list(year_curr = 2021,
                                park = "BOHA",
                                all_years = TRUE))


filepath = "C:/NETN/R_Dev/rockyintertidal_reports/"

rmarkdown::render(paste0(filepath, 'park_summary_abbrev.Rmd'),
                  output_file = paste0(filepath, "ACAD_summary_abbrev.html"),
                  params = list(year_curr = 2021,
                                park = "ACAD",
                                all_years = TRUE))


# Knit QAQV park-level reports
filepath = "C:/NETN/R_Dev/rockyintertidal_reports/"

rmarkdown::render(paste0(filepath, 'park_QC_checks.Rmd'),
                  output_file = paste0(filepath, "BOHA_QC_checks.html"),
                  params = list(year_curr = 2021,
                                park = "BOHA",
                                all_years = TRUE))


filepath = "C:/NETN/R_Dev/rockyintertidal_reports/"

rmarkdown::render(paste0(filepath, 'park_QC_checks.Rmd'),
                  output_file = paste0(filepath, "ACAD_QC_checks.html"),
                  params = list(year_curr = 2021,
                                park = "ACAD",
                                all_years = TRUE))

