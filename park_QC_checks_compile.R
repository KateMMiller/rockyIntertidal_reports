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


#+++++++++++++++++++++ Checks to add +++++++++++++++++++++++
#+ Find NAs in photoplot motile inverts
#+ Find NAs in motile invert measurements (SHIHAR-2021-Barnacle-TECTES = NA)
#+ Motile invert measurements > 99.9.
#+ Show photoplot cover by site/year to see where plots haven't been scored yet.
#+ Show echino counts that read in as NA
#+ EchinoMeasures have a lot of 0s in 2019, but nowhere else. Show table of values


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
  select(SiteCode) |> unique() |> c() #|> as.character()

#----- Visit Notes -----
notes_bolts <- getBolts(park = park, dropNA = F) |>
  filter(!is.na(Notes)) |>
  mutate(Note_Type = "Bolts",
         Year = NA_real_,
         Notes = paste0("Type: ", PlotType, "; PlotName:  ", PlotName,
                        "; Label: ", Label, "; Note: ", Notes)) |>
  select(SiteCode, Year, Note_Type, Notes)

notes_events <- do.call(getEvents, arglist) |>
  select(SiteCode, Year, Notes_Conditions, Notes_Marker, Notes_Other, Notes_Additional_Spp)

notes_long <- notes_events |> pivot_longer(Notes_Conditions:Notes_Additional_Spp,
                                           names_to = "Note_Type", values_to = "Notes") |>
  filter(!is.na(Notes) & !Notes %in% c("None", "NA", "None.", "N/A", 'none')) |> unique() |>
  mutate(Note_Type = substr(Note_Type, 7, nchar(Note_Type)))

# Add other notes if they're stored in other places here
notes_comb <- rbind(notes_long, notes_bolts) |> arrange(SiteCode, Year, Note_Type)


visit_table <- kable(notes_comb, format = 'html', align = c(rep('c', 2), 'l', 'l'),
                     col.names = c("Location", "Year", "Type", "Note")) |>
               kable_styling(fixed_thead = TRUE, bootstrap_options = 'condensed', full_width = TRUE,
                             position = 'left', font_size = 11) |>
               column_spec(1:3, width = "15%") |>
               row_spec(c(0, nrow(notes_long)), extra_css = 'border-bottom: 1px solid #000000')

include_visit_table <- tab_include(notes_comb)

#----- Point intercept checks -----

# Check for bolts with blank Elevation or Distance
bolt_dist_na <- do.call(getPIBoltDistance, args = c(arglist, dropNA = F)) |>
  #getPIBoltDistance(park = park, years = year) |>
  filter(is.na(Elevation_MLLW_m) | is.na(Distance_m)) |>
  select(SiteCode, Year, Label, Elevation_MLLW_m, Distance_m)

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
bolt_dist_0 <- do.call(getPIBoltDistance, args = c(arglist, dropNA = F)) |>
  filter(!grepl("01", Label) & Distance_m == 0) |>
  select(SiteCode, Year, Label, Elevation_MLLW_m, Distance_m)

QC_table <- rbind(QC_table,
                  QC_check(bolt_dist_0, "PI Transect", "Bolts with distance of 0 after first transect bolt"))

later_bolt_dist_0 <- make_kable(bolt_dist_0, "Bolts with distance of 0 after the first transect bolt.")

# Check for impossible elevation and distance combinations between bolts (forces NANs in arcsin calc)
spp_pi <- do.call(sumPISpecies, arglist) |>
  group_by(SiteCode, CoverType, CoverCode) |>
  summarize(num_pis = sum(!is.na(PI_Distance)), .groups = 'drop')

QC_table <- rbind(QC_table,
                  QC_check(bolt_checks, "PI Transect", "Impossible bolt elevation and distance combinations."))

bolt_check_tbl <- make_kable(bolt_checks, "Impossible bolt elevation and distance combinations.")

# Check if Point Intercept tab returned any records to determine whether to plot that tab in report
pit_check <- QC_table |> filter(Data %in% "PI Transect" & Num_Records > 0)

pit_include <- tab_include(pit_check)


#---- Photoplot substrate checks ----
pctcov <- do.call(getPhotoCover, args = c(arglist, dropNA = F)) |>
  mutate(sampID1 = paste(UnitCode, SiteCode, Year, sep = "_"),
         sampID = ifelse(QAQC == TRUE, paste0(sampID1, "_Q"), sampID1)) |>
  select(sampID, UnitCode, SiteCode, StartDate, Year, QAQC, PlotName, CommunityType,
         CoverCode, CoverType, PercentCover, Notes)

table(pctcov$sampID, pctcov$CommunityType)
table(pctcov$SiteCode, pctcov$Year)

# All of 2019 data are missing in the db version I have. Need to make a skeleton df of 2018 to show blanks.
# The if statement below only runs if 2019 is missing from the raw dataset
if(!2019 %in% unique(pctcov$Year)){
  table(pctcov$SiteCode, pctcov$Year)
  cov19 <- pctcov |> filter(Year == 2018) |> filter(QAQC == FALSE)
  cov19$StartDate <- NA
  cov19$PercentCover <- NA_real_
  cov19$sampID <- gsub("18", "19", cov19$sampID)
  cov19$Year <- 2019
  cov19 <- unique(cov19)
  pctcov <- rbind(pctcov, cov19)
}

if(!2021 %in% unique(pctcov$Year)){
  table(pctcov$SiteCode, pctcov$Year)
  cov21 <- pctcov |> filter(Year == 2018) |> filter(QAQC == FALSE)
  cov21$StartDate <- NA
  cov21$PercentCover <- NA_real_
  cov21$sampID <- gsub("18", "21", cov21$sampID)
  cov21$Year <- 2021
  cov21 <- unique(cov21)
  pctcov <- rbind(pctcov, cov21)
}

# location/plot name combinations found in data to check for missing
sample_combos <- pctcov |> select(UnitCode, SiteCode, CommunityType, PlotName) |> unique() |>
  group_by(UnitCode, SiteCode, CommunityType) |>
  summarize(num_plots = sum(!is.na(PlotName)),
            .groups = 'drop')

# Check for photoplots that have been sampled before but are missing in at least one survey
# Or that have duplicate data
plot_check1 <- as.data.frame(table(pctcov$sampID, pctcov$CommunityType)) |>
  filter(Freq < 130 | Freq > 130) |> mutate(sampID = as.character(Var1), # 5 plots * 26 species = 130
                               CommunityType = as.character(Var2),
                               numplots = as.numeric(Freq)) |>
  select(sampID, CommunityType, numplots) |>
  mutate(SiteCode = substr(sampID, 6, 11),
         Year = as.numeric(substr(sampID, 13, 16)))

plot_check <- left_join(sample_combos |> select(-num_plots),
                        plot_check1,
                        by = c("SiteCode", "CommunityType")) |>
              filter(!is.na(numplots)) |>
              select(UnitCode, SiteCode, Year, CommunityType, numplots)


QC_table <- rbind(QC_table,
                  QC_check(plot_check, "Photoplot substrate", "Photoplots either missing scores or having duplicate scores."))

photoplot_tbl <- make_kable(plot_check, "Photoplots either missing scores or with duplicate scores. Will only return missing target species plots that have been sampled at least one year in a given location.")


# Find year/target species combinations that haven't been scored yet
plot_check2 <- pctcov |> group_by(sampID, SiteCode, Year, QAQC, PlotName, CommunityType) |>
  summarize(num_pct_cov_NAs = sum(is.na(PercentCover)),
            .groups = 'keep') |> filter(num_pct_cov_NAs > 0)

QC_table <- rbind(QC_table,
                  QC_check(plot_check2, "Photoplot substrate", "Photoplots that haven't been scored for the entire year."))

photoplot2_tbl <- make_kable(plot_check2, "Photoplots that haven't been scored for the entire year.")


# Check that each site has the same species list, and species not detected have a 0 for PercentCover.
spp_combos <- pctcov |> select(UnitCode, SiteCode, CoverCode, PlotName) |> unique() |>
  group_by(UnitCode, SiteCode, CoverCode) |>
  summarize(numplots_max = sum(!is.na(PlotName)),
            .groups = 'drop')

spp_check1 <- as.data.frame(table(pctcov$sampID, pctcov$CoverCode)) |>
  filter(Freq < 26 | Freq > 26) |> mutate(sampID = as.character(Var1), # 5 plots * 26 species = 130
                                          CoverCode = as.character(Var2),
                                          numplots_samp = as.numeric(Freq)) |>
  select(sampID, CoverCode, numplots_samp) |>
  mutate(SiteCode = substr(sampID, 6, 11),
         Year = as.numeric(substr(sampID, 13, 16)))

spp_check <- left_join(spp_combos,
                        spp_check1,
                        by = c("SiteCode", "CoverCode")) |>
  filter(!is.na(numplots_samp)) |>
  select(UnitCode, SiteCode, Year, CoverCode, numplots_max, numplots_samp) |>
  filter(numplots_max != numplots_samp)

QC_table <- rbind(QC_table,
                  QC_check(spp_check, "Photoplot substrate",
                           "Photoplots either missing a species % cover that was recorded in a past survey or having duplicate percent cover."))

spp_plot_tbl <- make_kable(spp_check, "Photoplots either missing a species % cover that was recorded in a past survey or having duplicate percent cover. Will only return missing species for plotoplots that have been sampled at least one year in a given location.")

# Check for covers that sum to >100%
pctcov_sum <- pctcov |> group_by(sampID, SiteCode, Year, QAQC, PlotName, CommunityType) |>
  summarize(tot_pctcov = sum(PercentCover, na.rm = T), .groups = 'drop') |> filter(tot_pctcov > 100)

QC_table <- rbind(QC_table, QC_check(pctcov_sum, "Photoplot substrate",
                                     "Photoplots that sum to more than 100% cover. If the total percent cover sums to 200, there are likely duplicate scores for that location/photoplot."))

pctcov_sum_tbl <- make_kable(spp_check, "Photoplots that sum to more than 100% cover. If the total percent cover sums to 200, there are likely duplicate scores for that location/photoplot.")

# Check if photoplot tab returned any records to determine whether to plot that tab in report
photoplot_check <- QC_table |> filter(Data %in% "Photoplot substrate" & Num_Records > 0)

photoplot_include <- tab_include(photoplot_check)

#---- Photoplot Motile Inverts -----
micnt <- do.call(getMotileInvertCounts, arglist) |>
  select(UnitCode, SiteCode, Year, QAQC, CommunityType,
         PlotName, SpeciesCode, ScientificName, CommonName, Damage, No.Damage, Subsampled)

# Find NAs
micnt_nas <- micnt[!complete.cases(micnt),]

QC_table <- rbind(QC_table, QC_check(micnt_nas, "Photoplot Motile Inverts",
                                     "Photoplots with at least 1 NA in motile invertebrate count data."))

micnt_nas_tbl <- make_kable(micnt_nas, "Photoplots with at least 1 NA in motile invertebrate count data.")

# Find plots with > 99% for Damage or No.Damage, in case typo
micnt99dam <- quantile(micnt$Damage, probs = 0.99, na.rm = T)
micnt99nodam <- quantile(micnt$No.Damage, probs = 0.99, na.rm = T)

micnt_99dam <- micnt |> filter(Damage > micnt99dam)
QC_table <- rbind(QC_table, QC_check(micnt_99dam, "Photoplot Motile Inverts",
                                     "Photoplots with a Damage count > 99% of all recorded sites and years."))

micnt_99dam_tbl <- make_kable(micnt_99dam, "Photoplots with a Damage count > 99% of all recorded sites and years.")

micnt_99nodam <- micnt |> filter(No.Damage > micnt99nodam)
QC_table <- rbind(QC_table, QC_check(micnt_99nodam, "Photoplot Motile Inverts",
                                     "Photoplots with a No.Damage count > 99% of all recorded sites and years."))

micnt_99nodam_tbl <- make_kable(micnt_99nodam, "Photoplots with a No.Damage count > 99% of all recorded sites and years.")

#---- Motile Invertebrate Measures ----
mimeas <- do.call(getMotileInvertMeas, arglist) |> select(UnitCode, SiteCode, Year, QAQC, PlotName, SpeciesCode,
                                                          ScientificName, CommonName, Measurement)
mimeas_na <- mimeas[which(!complete.cases(mimeas)),]

QC_table <- rbind(QC_table, QC_check(mimeas_na, "Photoplot Motile Inverts",
                                     "Motile Inverts with NA measurement."))

mimeas_nas_tbl <- make_kable(mimeas_na, "Motile Inverts with NA measurement.")


# Measurements > 99.9mm (summary and plotting functions will fail)
mimeas99.9 <- mimeas |> filter(Measurement > 99.9)

QC_table <- rbind(QC_table, QC_check(mimeas99.9, "Photoplot Motile Inverts",
                                     "Motile Inverts with a measurement > 99.9mm. The rockyIntertidal package is only programmed to handle measurements <99.9mm. If this is a true value, update sumMotileInvertMeas() and plotMotileInvertMeas() to allow for higher measurement classes."))

mimeas_99.9_tbl <- make_kable(mimeas99.9, "Motile Inverts with a measurement > 99.9mm. The rockyIntertidal package is only programmed to handle measurements <99.9mm. If this is a true value, update sumMotileInvertMeas() and plotMotileInvertMeas() to allow for higher measurement classes.")

# Measurements > 99% of recorded dataset
mimeas_99 <- quantile(mimeas$Measurement, probs = 0.99, na.rm = T)
mimeas99 <- mimeas |> filter(Measurement > mimeas_99)

QC_table <- rbind(QC_table, QC_check(mimeas99, "Photoplot Motile Inverts",
                                     "Motile Inverts with a measurement > 99% of all measurements recorded among all sites and years."))

mimeas_99_tbl <- make_kable(mimeas99, "Motile Inverts with a measurement > 99% of all measurements recorded among all sites and years.")

# Years with lots of 0s instead of measurements
mimeas_0s <- mimeas |> mutate(zero = ifelse(Measurement == 0, 1, 0)) |>
  group_by(UnitCode, SiteCode, Year, QAQC) |>
  summarize(num_0s = sum(zero), .groups = 'drop') |>
  filter(num_0s > 0)

QC_table <- rbind(QC_table, QC_check(mimeas_0s, "Photoplot Motile Inverts",
                                     "Motile Invert sites and years that have measurements of 0."))

mimeas_0_tbl <- make_kable(mimeas_0s, "Motile Invert sites and years that have measurements of 0.")

# Check if photoplot tab returned any records to determine whether to plot that tab in report
photoplot_mi_check <- QC_table |> filter(Data %in% "Photoplot Motile Inverts" & Num_Records > 0)

photoplot_mi_include <- tab_include(photoplot_mi_check)

#---- Echinoderms -----
# Counts
eccnt <- do.call(getEchinoCounts, arglist) |>
  select(UnitCode, SiteCode, Year, QAQC,
         PlotName, SpeciesCode, ScientificName, Count)

# Find NAs
eccnt_nas <- eccnt[!complete.cases(eccnt),]

QC_table <- rbind(QC_table, QC_check(eccnt_nas, "Echinoderms",
                                     "Echinoderms with at least 1 NA in count data."))

eccnt_nas_tbl <- make_kable(eccnt_nas, "Echinoderms with at least 1 NA in count data.")

# Find plots with > 99% for Count, in case typo
eccnt99 <- quantile(eccnt$Count, probs = 0.99, na.rm = T)

eccnt_99 <- eccnt |> filter(Count > eccnt99)
QC_table <- rbind(QC_table, QC_check(eccnt_99, "Echinoderms",
                                     "Echinoderms with a count > 99% of all recorded sites and years."))

eccnt_99_tbl <- make_kable(eccnt_99, "Echinoderms with a count > 99% of all recorded sites and years.")


# Measures
ecmeas <- do.call(getEchinoMeas, arglist) |> select(UnitCode, SiteCode, Year, QAQC, PlotName, SpeciesCode,
                                                    ScientificName, Measurement)
ecmeas_na <- ecmeas[which(!complete.cases(ecmeas)),]

QC_table <- rbind(QC_table, QC_check(ecmeas_na, "Echinoderms",
                                     "Echinoderms with NA measurement."))

ecmeas_nas_tbl <- make_kable(ecmeas_na, "Echinoderms with NA measurement.")

# Measurements > 99.9mm (summary and plotting functions will fail)
ecmeas99.9 <- ecmeas |> filter(Measurement > 99.9)

QC_table <- rbind(QC_table, QC_check(ecmeas99.9, "Echinoderms",
                                     "Echinoderms with a measurement > 99.9mm. The rockyIntertidal package is only programmed to handle measurements <99.9mm. If this is a true value, update sumEchinoMeas() and plotEchinoMeas() to allow for higher measurement classes."))

ecmeas_99.9_tbl <- make_kable(ecmeas99.9, "Motile Inverts with a measurement > 99.9mm. The rockyIntertidal package is only programmed to handle measurements <99.9mm. If this is a true value, update sumEchinoMeas() and plotEchinoMeas() to allow for higher measurement classes.")

# Measurements > 99% of recorded dataset
ecmeas_99 <- quantile(ecmeas$Measurement, probs = 0.99, na.rm = T)
ecmeas99 <- ecmeas |> filter(Measurement > ecmeas_99)

QC_table <- rbind(QC_table, QC_check(ecmeas99, "Echinoderms",
                                     "Echinoderms with a measurement > 99% of all measurements recorded among all sites and years."))

ecmeas_99_tbl <- make_kable(ecmeas99, "Echinoderms with a measurement > 99% of all measurements recorded among all sites and years.")

# Years with lots of 0s instead of measurements
ecmeas_0s <- ecmeas |> mutate(zero = ifelse(Measurement == 0, 1, 0)) |>
  group_by(UnitCode, SiteCode, Year, QAQC) |>
  summarize(num_0s = sum(zero), .groups = 'drop') |>
  filter(num_0s > 0)

QC_table <- rbind(QC_table, QC_check(ecmeas_0s, "Echinoderms",
                                     "Echinoderm sites and years that have measurements of 0."))

ecmeas_0_tbl <- make_kable(ecmeas_0s, "Echinoderm sites and years that have measurements of 0.")


# Check if photoplot tab returned any records to determine whether to plot that tab in report
echino_check <- QC_table |> filter(Data %in% "Echinoderms" & Num_Records > 0)

echino_include <- tab_include(echino_check)

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
