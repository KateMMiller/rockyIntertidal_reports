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
spp_pi <- do.call(sumPISpecies, arglist) |>
  group_by(Loc_Code, Spp_Code, Spp_Name) |>
  summarize(num_pis = sum(!is.na(PI_Distance)), .groups = 'drop')

QC_table <- rbind(QC_table,
                  QC_check(bolt_checks, "PI Transect", "Impossible bolt elevation and distance combinations."))

bolt_check_tbl <- make_kable(bolt_checks, "Impossible bolt elevation and distance combinations.")

# Check if Point Intercept tab returned any records to determine whether to plot that tab in report
pit_check <- QC_table |> filter(Data %in% "PI Transect" & Num_Records > 0)

pit_include <- tab_include(pit_check)


#---- Photoplot checks ----
pctcov <- do.call(getPhotoCover, arglist) |>
  mutate(sampID1 = paste(Site_Code, Loc_Code, Year, sep = "_"),
         sampID = ifelse(QAQC == TRUE, paste0(sampID1, "_Q"), sampID1)) |>
  select(sampID, Site_Code, Loc_Code, Start_Date, Year, QAQC, Plot_Name, Target_Species,
         Spp_Code, Category, Perc_Cover, Notes)

table(pctcov$sampID, pctcov$Target_Species)

# Check for photoplots that have been sampled before but are missing in at least one survey
# Or that have duplicate data

# location/plot name combinations found in data to check for missing
sample_combos <- pctcov |> select(Site_Code, Loc_Code, Target_Species, Plot_Name) |> unique() |>
  group_by(Site_Code, Loc_Code, Target_Species) |>
  summarize(num_plots = sum(!is.na(Plot_Name)),
            .groups = 'drop')

plot_check1 <- as.data.frame(table(pctcov$sampID, pctcov$Target_Species)) |>
  filter(Freq < 130 | Freq > 130) |> mutate(sampID = as.character(Var1), # 5 plots * 26 species = 130
                               Target_Species = as.character(Var2),
                               numplots = as.numeric(Freq)) |>
  select(sampID, Target_Species, numplots) |>
  mutate(Loc_Code = substr(sampID, 6, 11),
         Year = as.numeric(substr(sampID, 13, 16)))

plot_check <- left_join(sample_combos |> select(-num_plots),
                        plot_check1,
                        by = c("Loc_Code", "Target_Species")) |>
              filter(!is.na(numplots)) |>
              select(Site_Code, Loc_Code, Year, Target_Species, numplots)

QC_table <- rbind(QC_table,
                  QC_check(plot_check, "Photoplots", "Photoplots either missing scores or having duplicate scores."))

photoplot_tbl <- make_kable(plot_check, "Photoplots either missing scores or with duplicate scores. Will only return missing target species plots that have been sampled at least one year in a given location.")

# Check that each site has the same species list, and species not detected have a 0 for Perc_Cover.
spp_combos <- pctcov |> select(Site_Code, Loc_Code, Spp_Code, Plot_Name) |> unique() |>
  group_by(Site_Code, Loc_Code, Spp_Code) |>
  summarize(numplots_max = sum(!is.na(Plot_Name)),
            .groups = 'drop')

spp_check1 <- as.data.frame(table(pctcov$sampID, pctcov$Spp_Code)) |>
  filter(Freq < 26 | Freq > 26) |> mutate(sampID = as.character(Var1), # 5 plots * 26 species = 130
                                          Spp_Code = as.character(Var2),
                                          numplots_samp = as.numeric(Freq)) |>
  select(sampID, Spp_Code, numplots_samp) |>
  mutate(Loc_Code = substr(sampID, 6, 11),
         Year = as.numeric(substr(sampID, 13, 16)))

spp_check <- left_join(spp_combos,
                        spp_check1,
                        by = c("Loc_Code", "Spp_Code")) |>
  filter(!is.na(numplots_samp)) |>
  select(Site_Code, Loc_Code, Year, Spp_Code, numplots_max, numplots_samp) |>
  filter(numplots_max != numplots_samp)

QC_table <- rbind(QC_table,
                  QC_check(spp_check, "Photoplots",
                           "Photoplots either missing a species % cover that was recorded in a past survey or having duplicate percent cover."))

spp_plot_tbl <- make_kable(spp_check, "Photoplots either missing a species % cover that was recorded in a past survey or having duplicate percent cover. Will only return missing species for plotoplots that have been sampled at least one year in a given location.")

# Check for NAs in Perc_Cover field

head(pctcov)

# Check if Point Intercept tab returned any records to determine whether to plot that tab in report
photoplot_check <- QC_table |> filter(Data %in% "Photoplots" & Num_Records > 0)

photoplot_include <- tab_include(photoplot_check)


#+++++++++++++++++++++ Checks to add +++++++++++++++++++++++
#+ Find NAs in photoplot motile inverts
#+ Find NAs in motile invert measurements (SHIHAR-2021-Barnacle-TECTES = NA)
#+ Motile invert measurements > 99.9.
#+ Show photoplot cover by site/year to see where plots haven't been scored yet.
#+ Show echino counts that read in as NA
#+ EchinoMeasures have a lot of 0s in 2019, but nowhere else. Show table of values

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
