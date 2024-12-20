# Knit QAQV park-level reports for all years
filepath = "C:/NETN/R_Dev/rockyintertidal_reports/"

rmarkdown::render(paste0(filepath, 'park_QC_checks_all_years.Rmd'),
                  output_file = paste0(filepath, "BOHA_QC_checks_all_years.html"),
                  params = list(year_curr = 2024,
                                park = "BOHA"))


rmarkdown::render(paste0(filepath, 'park_QC_checks_all_years.Rmd'),
                  output_file = paste0(filepath, "ACAD_QC_checks_all_years.html"),
                  params = list(year_curr = 2024,
                                park = "ACAD"))

# QC checks for 2024 only

rmarkdown::render(paste0(filepath, 'park_QC_checks.Rmd'),
                  output_file = paste0(filepath, "BOHA_QC_checks_2024.html"),
                  params = list(year_curr = 2024,
                                park = "BOHA"))


filepath = "C:/NETN/R_Dev/rockyintertidal_reports/"

rmarkdown::render(paste0(filepath, 'park_QC_checks.Rmd'),
                  output_file = paste0(filepath, "ACAD_QC_checks_2024.html"),
                  params = list(year_curr = 2024,
                                park = "ACAD"))

