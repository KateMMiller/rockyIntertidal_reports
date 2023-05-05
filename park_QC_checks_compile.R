#----------------------------------------
# Conducts QC checks on Rocky Intertidal Data. Output is reported in park_QC_checks.Rmd
#----------------------------------------
#
# library(rockyIntertidal)
# library(tidyverse)
# library(knitr)
# library(kableExtra)
# #library(htmltools)
#
# importData()
#
#
# year = 2013:2021
# park = "ACAD"

#----- Functions -----
QC_check <- function(df, tab, check){
  result <- data.frame("Data" = tab, "Description" = check, "Num_Records" = nrow(df))
}

make_kable <- function(df, cap){
  QC_table <- if(nrow(df) > 0){
    kable(df, format = 'html', align = 'c', caption = cap)  |>
      kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                    full_width = TRUE, position = 'left', font_size = 12) |>
      row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
      collapse_rows(1, valign = 'top') |>
      row_spec(nrow(df), extra_css = 'border-bottom: 1px solid #000000;')
  } else NULL
}

# Determine whether to include/drop tab in rmd output
tab_include <- function(df){ifelse(nrow(df) > 0, TRUE, FALSE)}

# Determine if table exists or is null used in eval for rmd
check_null <- function(table){
  if(!is.null(table)){table}
}

#----- Setup ------
arglist <- list(park = park, years = year)

locs <- do.call(getPIBoltDistance, arglist) |>
  select(Loc_Code) |> unique() |> c() #|> as.character()

#----- Visit Notes -----
notes_bolts <- getBolts(park = park) |>
  filter(!is.na(Notes)) |>
  mutate(Note_Type = "Bolts",
         Year = NA_real_,
         Notes = paste0("Type: ", Plot_Type, "; Plot_Name:  ", Plot_Name,
                        "; Label: ", Label, "; Note: ", Notes)) |>
  select(Loc_Code, Year, Note_Type, Notes)

notes_events <- do.call(getEventNotes, arglist) |>
  select(Loc_Code, Year, Notes_Conditions, Notes_Marker, Notes_Other, Notes_Additional_Spp)

notes_long <- notes_events |> pivot_longer(Notes_Conditions:Notes_Additional_Spp,
                                           names_to = "Note_Type", values_to = "Notes") |>
  filter(!is.na(Notes) & !Notes %in% c("None", "NA", "None.", "N/A", 'none')) |> unique() |>
  mutate(Note_Type = substr(Note_Type, 7, nchar(Note_Type)))

# Add other notes if they're stored in other places here
notes_comb <- rbind(notes_long, notes_bolts) |> arrange(Loc_Code, Year, Note_Type)


visit_table <- kable(notes_comb, format = 'html', align = c(rep('c', 2), 'l', 'l'),
                     col.names = c("Location", "Year", "Type", "Note")) |>
               kable_styling(fixed_thead = TRUE, bootstrap_options = 'condensed', full_width = TRUE,
                             position = 'left', font_size = 11) |>
               column_spec(1:3, width = "15%") |>
               row_spec(c(0, nrow(notes_long)), extra_css = 'border-bottom: 1px solid #000000')

include_visit_table <- tab_include(notes_comb)

#----- Point intercept checks -----

# Check for bolts with blank Elevation or Distance
bolt_dist_na <- do.call(getPIBoltDistance, arglist) |>
  #getPIBoltDistance(park = park, years = year) |>
  filter(is.na(Elevation_MLLW_m) | is.na(Distance_m)) |>
  select(Loc_Code, Year, Label, Elevation_MLLW_m, Distance_m, Notes_Event)

QC_table <- QC_check(bolt_dist_na, "PI Transect", "Bolts missing elevation or distance")

miss_bolt_elev_dist <- kable(bolt_dist_na, format = 'html', align = 'c',
                             caption = "Bolts missing an elevation or distance measurement")  |>
  kable_styling(fixed_thead = TRUE, bootstrap_options = 'condensed',
                full_width = TRUE, position = 'left', font_size = 12) |>
  row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
  column_spec(4, background = ifelse(is.na(bolt_dist_na$Elevation_MLLW_m), "#F2F2A0", "#ffffff")) |>
  column_spec(5, background = ifelse(is.na(bolt_dist_na$Distance_m), "#F2F2A0", "#ffffff")) |>
  collapse_rows(1, valign = 'top') |>
  row_spec(nrow(bolt_dist_na), extra_css = 'border-bottom: 1px solid #000000;')

# Check for bolts not labeled 1 with Distance of 0
bolt_dist_0 <- do.call(getPIBoltDistance, arglist) |>
  filter(!grepl("01", Label) & Distance_m == 0) |>
  select(Loc_Code, Year, Label, Elevation_MLLW_m, Distance_m, Notes_Event)

QC_table <- rbind(QC_table,
                  QC_check(bolt_dist_0, "PI Transect", "Bolts with distance of 0 after first transect bolt"))

later_bolt_dist_0 <- make_kable(bolt_dist_0, "Bolts with distance of 0 after the first transect bolt.")

# Check for impossible elevation and distance combinations between bolts (forces NANs in arcsin calc)
spp_pi <- do.call(sumPISppDetections, arglist) |>
  group_by(Loc_Code, Spp_Code, Spp_Name) |>
  summarize(num_pis = sum(!is.na(PI_Distance)), .groups = 'drop')

QC_table <- rbind(QC_table,
                  QC_check(bolt_checks, "PI Transect", "Impossible bolt elevation and distance combinations."))

bolt_check_tbl <- make_kable(bolt_checks, "Impossible bolt elevation and distance combinations.")

# Check if Vertical Transect tab returned any records to determine whether to plot that tab in report
pit_check <- QC_table |> filter(Data %in% "PI Transect" & Num_Records > 0)

pit_include <- tab_include(pit_check)

#---- Final QC check table ----
QC_check_table <- kable(QC_table, format = 'html', align = 'c', caption = "QC checking results",
                        col.names = c("Data Tab", "Check Description", "Number of Records")) |>
                  kable_styling(fixed_thead = TRUE, bootstrap_options = c("condensed"),
                                full_width = TRUE, position = 'left', font_size = 12) |>
                  row_spec(0, extra_css =
                             "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
                  column_spec(2:ncol(QC_table),
                              background = ifelse(QC_table$Num_Records > 0, "#F2F2A0", "#ffffff")) |>
                  collapse_rows(1, valign = 'top') |>
                  row_spec(nrow(QC_table), extra_css = 'border-bottom: 1px solid #000000;') #|>
                  #column_spec(2, width = "150px")
