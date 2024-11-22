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
    kable(df, format = 'html', align = 'c', caption = cap, row.names = F)  |>
      kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                    full_width = TRUE, position = 'left', font_size = 12) |>
      row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
      collapse_rows(1, valign = 'top') |>
      row_spec(nrow(df), extra_css = 'border-bottom: 1px solid #000000;')
  } else NULL
}

make_kable2 <- function(df, cap){
  QC_table <- if(nrow(df) > 0){
    kable(df, format = 'html', align = 'c', caption = cap, row.names = F)  |>
      kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                    full_width = TRUE, position = 'left', font_size = 12) |>
      row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
      collapse_rows(1:2, valign = 'top') |>
      row_spec(nrow(df), extra_css = 'border-bottom: 1px solid #000000;')
  } else NULL
}

# Determine whether to include/drop tab in rmd output
tab_include <- function(df){ifelse(nrow(df) > 0, TRUE, FALSE)}

# Determine if table exists or is null used in eval for rmd
check_null <- function(table){
  if(!is.null(table)){table}
}

check_null_print <- function(table, tab_level = 4, tab_title){
  if(!is.null(table)){cat(paste0(rep("#", tab_level), collapse = ""), " ", tab_title, " {.tabset} ", "\n\n")}
  check_null(table)
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
  filter(is.na(Distance_m)) |>
  select(SiteCode, Year, Label, Distance_m) |>
  arrange(SiteCode, Year, Label)

QC_table <- QC_check(bolt_dist_na, "PI Transect", "Bolts missing distance")

miss_bolt_elev_dist <- kable(bolt_dist_na, format = 'html', align = 'c',
                             caption = "Bolts missing a distance measurement")  |>
  kable_styling(fixed_thead = TRUE, bootstrap_options = 'condensed',
                full_width = TRUE, position = 'left', font_size = 12) |>
  row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
  column_spec(4, background = ifelse(is.na(bolt_dist_na$Distance_m), "#F2F2A0", "#ffffff")) |>
  collapse_rows(1, valign = 'top') |>
  row_spec(nrow(bolt_dist_na), extra_css = 'border-bottom: 1px solid #000000;')


# Check for bolts not labeled 1 with Distance of 0
bolt_dist_0 <- do.call(getPIBoltDistance, args = c(arglist, dropNA = F)) |>
  filter(!grepl("01", Label) & Distance_m == 0) |>
  select(SiteCode, Year, Label, Distance_m)

QC_table <- rbind(QC_table,
                  QC_check(bolt_dist_0, "PI Transect", "Bolts with distance of 0 after first transect bolt"))

later_bolt_dist_0 <- make_kable(bolt_dist_0, "Bolts with distance of 0 after the first transect bolt.")

# Check for bolts with distance measurements that aren't chronological
bolt_chron <- do.call(getPIBoltDistance, args = c(arglist, dropNA = F)) |>
  select(SiteCode, Year, PlotName, Label, Distance_m) |>
  group_by(SiteCode, Year, PlotName) |>
  arrange(SiteCode, Year, Label) |>
  mutate(lead_dist = dplyr::lead(Distance_m, 1),
         not_chron = ifelse(lead_dist - Distance_m < 0, "X", NA_character_)) |>
  select(SiteCode, Year, PlotName, Label, Distance_m, not_chron)

# Take 2 records before/after non-chronological records
non_chron <- which(bolt_chron$not_chron == "X")
non_chron_prev2 <- non_chron - 1
non_chron_foll2 <- non_chron + 1

bolt_nonchron <- bolt_chron[sort(c(non_chron_prev2, non_chron, non_chron_foll2)),]

QC_table <- rbind(QC_table,
                  QC_check(bolt_nonchron, "PI Transect", "Bolts with distances out of chronological order."))

nonchron_bolt_dist <- kable(bolt_nonchron, format = 'html', align = 'c',
                            caption = "Bolts with distances out of chronological order, showing previous and following bolt for comparison.",
                        col.names = c("SiteCode", "Year", "PlotName", "Label", "Distance (m)", "Out-of-order")) |>
  kable_styling(fixed_thead = TRUE, bootstrap_options = c("condensed"),
                full_width = TRUE, position = 'left', font_size = 12) |>
  row_spec(0, extra_css =
             "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
  column_spec(2:ncol(bolt_nonchron),
              background = ifelse(!(is.na(bolt_nonchron$not_chron)), "#F2F2A0", "#ffffff")) |>
  collapse_rows(1:2, valign = 'top') |>
  row_spec(nrow(QC_table), extra_css = 'border-bottom: 1px solid #000000;')

# Check that all bolts have a distance, using list of unique bolt labels from all years
bolt_data <- do.call(getPIBoltDistance, args = c(arglist, dropNA = F)) |>
  select(SiteCode, Year, PlotName, Label) |> mutate(Bolt_Check = 1)

bolt_missed1 <- bolt_data |> pivot_wider(names_from = Year, values_from = Bolt_Check, values_fill = 0) |>
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "recorded") |>
  filter(recorded == 0) |> mutate(Year = as.numeric(Year))

# Drop visits that didn't occur
bolt_visits <- bolt_data |> select(SiteCode, Year) |> unique()

bolt_missed <- left_join(bolt_visits, bolt_missed1, by = c("SiteCode", "Year")) |>
  filter(!is.na(Label)) |> select(-recorded) |>
  arrange(SiteCode, Year, Label)

QC_table <- rbind(QC_table,
                  QC_check(bolt_missed, "PI Transect", "Bolts that were missed in a given visit."))

missing_bolts <- make_kable2(bolt_missed, "Bolts that were missed in a given visit.")

# Check transect lengths, look for lengths <> 95% of lengths recorded
trans_dist <- do.call(getPIBoltDistance, args = c(arglist, dropNA = F)) |>
  group_by(SiteCode, Year, PlotName) |>
  summarize(trans_dist = max(Distance_m, na.rm = T), .groups = 'drop')

trans_dist95 <- trans_dist |> group_by(SiteCode, PlotName) |>
  summarize(upper95 = quantile(trans_dist, 0.975),
            lower95 = quantile(trans_dist, 0.025),
            .groups = 'drop')

trans_check <- left_join(trans_dist, trans_dist95, by = c("SiteCode", "PlotName")) |>
  mutate(short_transect = ifelse(trans_dist < lower95, 1, 0),
         long_transect = ifelse(trans_dist > upper95, 1, 0))

trans_short <- trans_check |> filter(short_transect == 1) |> select(SiteCode, Year, PlotName, trans_dist, upper95, lower95)
QC_table <- rbind(QC_table,
                  QC_check(trans_short, "PI Transect", "Transects with lengths shorter than 95% of all other visits."))

short_trans <- make_kable2(trans_short, "Transects with lengths shorter than 95% of all other visits.")

trans_long <- trans_check |> filter(long_transect == 1) |> select(SiteCode, Year, PlotName, trans_dist, upper95, lower95)
QC_table <- rbind(QC_table,
                  QC_check(trans_long, "PI Transect", "Transects with lengths longer than 95% of all other visits."))

long_trans <- make_kable2(trans_long, "Transects with lengths longer than 95% of all other visits.")

# Check that all transects were sampled in a given year
bolt_trans <- do.call(getPIBoltDistance, args = c(arglist, dropNA = F)) |>
  select(SiteCode, Year, PlotName) |> mutate(Sampled = 1) |> unique() |>
  pivot_wider(names_from = PlotName, values_from = Sampled) |>
  mutate(Num_Transects = T1 + T2 + T3) |> filter(Num_Transects < 3)

QC_table <- rbind(QC_table,
                  QC_check(bolt_trans, "PI Transect", "Sites with fewer than 3 transects sampled in a given visit."))

missing_trans <- make_kable2(bolt_trans, "Sites with fewer than 3 transects sampled in a given visit.")

# Check for impossible elevation and distance combinations between bolts (forces NANs in arcsin calc)
spp_pi <- do.call(sumPISpecies, arglist) |>
  group_by(SiteCode, CoverType, CoverCode) |>
  summarize(num_pis = sum(!is.na(PI_Distance)), .groups = 'drop')

QC_table <- rbind(QC_table,
                  QC_check(bolt_checks, "PI Transect", "Impossible bolt elevation and distance combinations."))

bolt_check_tbl <- make_kable(bolt_checks, "Impossible bolt elevation and distance combinations.")

# Check for species higher or lower than observed range for all previous visits
# Doing this for all possible visits returned 850 records, which just isn't helpful
spp_elev <- do.call(sumPISpecies, args = c(arglist, drop_missing = T)) |>
  select(SiteCode, Year, PlotName, CoverType, CoverCode, PI_Elevation) |>
  filter(!is.na(PI_Elevation)) |>
  mutate(PI_Elevation = as.numeric(format(ceiling(PI_Elevation*100)/100, nsmall = 2))) # So completely drops numbers after 2 decimals

spp_elev_curr <- spp_elev |> filter(Year == year_curr)

spp_elev_sum <- spp_elev |> filter(!Year %in% year_curr) |>
  group_by(SiteCode, PlotName, CoverType, CoverCode) |>
  summarize(max_elev = round(max(PI_Elevation, na.rm = T), 2),
            min_elev = round(min(PI_Elevation, na.rm = T), 2),
            .groups = 'drop')

spp_elev_check <- left_join(spp_elev_curr, spp_elev_sum, by = c("SiteCode", "PlotName", "CoverType", "CoverCode")) |>
  mutate(low_elev = ifelse(PI_Elevation < min_elev, 1, 0), # to ignore rounding errors
         high_elev = ifelse(PI_Elevation > max_elev, 1, 0), # to ignore rounding errors
         outside_range = low_elev + high_elev) |>
  filter(outside_range > 0)

spp_elev_high <- spp_elev_check |> filter(high_elev == 1) |>
  select(SiteCode, Year, PlotName, CoverType, CoverCode, PI_Elevation, max_elev)
spp_elev_low <- spp_elev_check |> filter(low_elev == 1) |>
  select(SiteCode, Year, PlotName, CoverType, CoverCode, PI_Elevation, min_elev)

QC_table <- rbind(QC_table,
                  QC_check(spp_elev_high, "PI Transect",
                           paste0("Species elevations in ", year_curr, " higher than elevations recorded in previous visits.")))

QC_table <- rbind(QC_table,
                  QC_check(spp_elev_low, "PI Transect",
                           paste0("Species elevations in ", year_curr, " lower than elevations recorded in previous visits.")))

spp_high_elev <- make_kable(spp_elev_high,
                            paste0("Species elevations in ", year_curr, " higher than elevations recorded in previous visits."))

spp_low_elev <- make_kable(spp_elev_low,
                           paste0("Species elevations in ", year_curr, " lower than elevations recorded in previous visits."))


# Check if Point Intercept tab returned any records to determine whether to plot that tab in report
pit_check <- QC_table |> filter(Data %in% "PI Transect" & Num_Records > 0)

pit_include <- tab_include(pit_check)

#---- Photoplot substrate checks ----
# Check for missing photoplots
# Create schedule of photoplot sampling events and # plots
photocov <- do.call(getPhotoCover, arglist) |> select(SiteCode, StartDate, Year, QAQC, PlotName) |> unique()

photo_sch <- as.data.frame(table(photocov$Year, photocov$SiteCode, photocov$PlotName))
colnames(photo_sch) <- c("Year", "SiteCode", "PlotName", "Num_Samples")

num_years = length(unique(photo_sch$Year))

photo_sch_wide <- photo_sch |> pivot_wider(names_from = Year, values_from = Num_Samples,
                                           names_prefix = "yr") |>
  filter(!is.na(SiteCode)) |> filter(!is.na(PlotName)) |>
  mutate(num_samples = rowSums(across(where(is.numeric)), na.rm = T)) |>
  filter(num_samples < num_years) |> filter(num_samples > 0) |>
  arrange(SiteCode, PlotName)

QC_table <- rbind(QC_table, QC_check(photo_sch_wide, "Photoplot substrate",
                                     "Site X photoplot combinations missed at least one year."))

photo_sch_tbl <-
  kable(photo_sch_wide, format = 'html', align = 'c', row.names = F,
        caption = "Site X photoplot combinations missed at least one year. 0s indicate no cover data for plot") %>%
  kable_styling(fixed_thead = TRUE, bootstrap_options = c("condensed"),
                full_width = TRUE, position = 'left', font_size = 12) %>%
  row_spec(0, extra_css =
             "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") %>%
  purrr::reduce(3:ncol(photo_sch_wide), function(x, y){
    col <- photo_sch_wide[,y]
    column_spec(x, y, background = ifelse(col == 0, "#F2F2A0", "#ffffff"))}, .init = .) %>%
  collapse_rows(1, valign = 'top') %>%
  row_spec(nrow(photo_sch_wide), extra_css = 'border-bottom: 1px solid #000000;') %>%
  scroll_box(height = "600px")


# Check for photoplots with data that don't have bolts in bolts view
# had to use raw views, b/c functions drop plots without bolt record
bolts <- ROCKY$Bolts |> select(SiteCode, PlotName, CommunityType) |> unique() |> mutate(Bolt = 1)
photocov <- ROCKY$PhotoQuadrats_Cover |> mutate(Year = as.numeric(format(StartDate, "%Y"))) |>
  select(SiteCode, Year, PlotName, CommunityType) |> unique() |> mutate(Photoplot = 1)

photo_vs_bolt <- full_join(photocov, bolts, by = c("SiteCode", "PlotName", "CommunityType")) |>
  filter(is.na(Bolt)) |> select(-Photoplot)

QC_table <- rbind(QC_table,
                  QC_check(photo_vs_bolt, "Photoplot substrate", "Photoplots missing record in bolt view"))

miss_photo_bolts <- make_kable2(photo_vs_bolt, "Photoplots missing record in bolt view.")

# Photoplot substrate checks
pctcov <- do.call(getPhotoCover, args = c(arglist, dropNA = F)) |>
  mutate(sampID1 = paste(UnitCode, SiteCode, Year, sep = "_"),
         sampID = ifelse(QAQC == TRUE, paste0(sampID1, "_Q"), sampID1)) |>
  select(sampID, UnitCode, SiteCode, StartDate, Year, QAQC, PlotName, CommunityType,
         CoverCode, CoverType, PercentCover, Notes)

#table(pctcov$sampID, pctcov$CommunityType)
#table(pctcov$SiteCode, pctcov$Year)

# location/plot name combinations found in data to check for missing
sample_combos <- pctcov |> select(UnitCode, SiteCode, CommunityType, PlotName) |> unique() |>
  group_by(UnitCode, SiteCode, CommunityType) |>
  summarize(num_plots = sum(!is.na(PlotName)),
            .groups = 'drop')

# Check for photoplots that have duplicate data
plot_check1 <- as.data.frame(table(pctcov$sampID, pctcov$CommunityType)) |>
  filter(Freq > 130) |> mutate(sampID = as.character(Var1), # 5 plots * 26 species = 130
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
                  QC_check(plot_check, "Photoplot substrate", "Photoplots with duplicate scoring."))

photoplot_tbl <- make_kable(plot_check, "Photoplots with duplicate scoring. Need to decide which one to use for analysis in R package, and how to select it.")

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

spp_plot_tbl <- make_kable2(spp_check, "Photoplots either missing a species % cover that was recorded in a past survey or having duplicate percent cover. Will only return missing species for plotoplots that have been sampled at least one year in a given location.")

# Check for covers that sum to >100%
pctcov_sum <- pctcov |> group_by(sampID, SiteCode, Year, QAQC, PlotName, CommunityType) |>
  summarize(tot_pctcov = sum(PercentCover, na.rm = T), .groups = 'drop') |> filter(tot_pctcov > 100)

QC_table <- rbind(QC_table, QC_check(pctcov_sum, "Photoplot substrate",
                                     "Photoplots that sum to more than 100 cover. If the total percent cover sums to 200, there are likely duplicate scores for that location/photoplot."))

pctcov_sum_tbl <- make_kable(pctcov_sum, "Photoplots that sum to more than 100 cover. If the total percent cover sums to 200, there are likely duplicate scores for that location/photoplot.")

# Check for covers that sum to <100%
cov_sum <- pctcov |> group_by(SiteCode, Year, PlotName, CommunityType, QAQC) |>
  summarize(total_cover = sum(PercentCover), .groups = 'drop') |>
  filter(total_cover < 100) |> filter(total_cover > 0)

QC_table <- rbind(QC_table,
                  QC_check(cov_sum, "Photoplot substrate", "Total photoplot cover < 100."))

covsum_tbl <- make_kable(cov_sum, "Total photoplot cover < 100.")

# Check for covers that sum to 0
cov_sum0 <- pctcov |> group_by(SiteCode, Year, PlotName, CommunityType, QAQC) |>
  summarize(total_cover = sum(PercentCover), .groups = 'drop') |>
  filter(total_cover == 0)

QC_table <- rbind(QC_table,
                  QC_check(cov_sum0, "Photoplot substrate", "Total photoplot cover = 0."))

covsum0_tbl <- make_kable(cov_sum0, "Total photoplot cover = 0.")

# Check if photoplot tab returned any records to determine whether to plot that tab in report
photoplot_check <- QC_table |> filter(Data %in% "Photoplot substrate" & Num_Records > 0)

photoplot_include <- tab_include(photoplot_check)

#---- Photoplot Motile Inverts -----
micnt <- do.call(getMotileInvertCounts, arglist) |>
  select(UnitCode, SiteCode, Year, QAQC, CommunityType,
         PlotName, SpeciesCode, ScientificName, CommonName, Damage, No.Damage, Subsampled)

# Find NAs
micnt_nas <- micnt[!complete.cases(micnt),c("SiteCode", "Year", "QAQC", "CommunityType", "PlotName", "SpeciesCode",
                                            "CommonName", "Damage", "No.Damage", "Subsampled")]

QC_table <- rbind(QC_table, QC_check(micnt_nas, "Photoplot Motile Inverts",
                                     "Photoplots with at least 1 NA in motile invertebrate count data."))

micnt_nas_tbl <- kable(micnt_nas, format = 'html', align = 'c', row.names = F,
                       col.names = c("SiteCode", "Year", "QAQC", "CommunityType", "PlotName",
                                     "SpeciesCode", "CommonName", "Damage", "No.Damage", "Subsampled"),
                       caption = "Photoplots with at least 1 NA in motile invertebrate count data.") |>
  kable_styling(fixed_thead = TRUE, bootstrap_options = c("condensed"),
                full_width = TRUE, position = 'left', font_size = 12) |>
  row_spec(0, extra_css =
             "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
  column_spec(8, background = ifelse(is.na(micnt_nas[,8]), "#F2F2A0", "#ffffff")) |>
  column_spec(9, background = ifelse(is.na(micnt_nas[,9]), "#F2F2A0", "#ffffff")) |>
  column_spec(10, background = ifelse(is.na(micnt_nas[,10]), "#F2F2A0", "#ffffff")) |>
  collapse_rows(1, valign = 'top') |>
  row_spec(nrow(micnt_nas), extra_css = 'border-bottom: 1px solid #000000;') #|>
#column_spec(2, width = "150px")

# Find plots with > 99% for Damage or No.Damage, in case typo
micnt99dam <- quantile(micnt$Damage, probs = 0.99, na.rm = T)
micnt99nodam <- quantile(micnt$No.Damage, probs = 0.99, na.rm = T)

micnt_99dam <- micnt |> filter(Damage > micnt99dam) |>
  select(SiteCode, Year, QAQC, CommunityType, PlotName, SpeciesCode, CommonName, Damage, Subsampled)

QC_table <- rbind(QC_table, QC_check(micnt_99dam, "Photoplot Motile Inverts",
                                     "Photoplots with a Damage count > 99% of all recorded sites and years."))
head(micnt_99dam)

micnt_99dam_tbl <- make_kable2(micnt_99dam, "Photoplots with a Damage count > 99% of all recorded sites and years.")

micnt_99nodam <- micnt |> filter(No.Damage > micnt99nodam)|>
  select(SiteCode, Year, QAQC, CommunityType, PlotName, SpeciesCode, CommonName, No.Damage, Subsampled)
QC_table <- rbind(QC_table, QC_check(micnt_99nodam, "Photoplot Motile Inverts",
                                     "Photoplots with a No.Damage count > 99% of all recorded sites and years."))

micnt_99nodam_tbl <- make_kable2(micnt_99nodam, "Photoplots with a No.Damage count > 99% of all recorded sites and years.")

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

mimeas_99_tbl <- make_kable2(mimeas99, "Motile Inverts with a measurement > 99% of all measurements recorded among all sites and years.")

# Years with lots of 0s instead of measurements
mimeas_0s <- mimeas |> mutate(zero = ifelse(Measurement == 0, 1, 0)) |>
  group_by(UnitCode, SiteCode, Year, QAQC) |>
  summarize(num_0s = sum(zero), .groups = 'drop') |>
  filter(num_0s > 0)

QC_table <- rbind(QC_table, QC_check(mimeas_0s, "Photoplot Motile Inverts",
                                     "Motile Invert sites and years that have measurements of 0."))

mimeas_0_tbl <- make_kable(mimeas_0s, "Motile Invert sites and years that have measurements of 0.")

# Check that counts match number of measurements up to 10 per species
micnt <- do.call(getMotileInvertCounts, arglist) |>
  select(UnitCode, SiteCode, Year, QAQC, StartDate, CommunityType,
         PlotName, SpeciesCode, ScientificName, CommonName, Damage, No.Damage, Subsampled)

micnt_sum <- micnt |>
  mutate(Damage = ifelse(is.na(Damage), 0, Damage),
         No.Damage = ifelse(is.na(No.Damage), 0, No.Damage),
         num_count = Damage + No.Damage) |>
  select(SiteCode, Year, PlotName, StartDate, SpeciesCode, ScientificName, CommonName,
         Subsampled, num_count)

mimeas <- do.call(getMotileInvertMeas, arglist) |>
  select(UnitCode, SiteCode, Year, StartDate, PlotName, SpeciesCode,
         ScientificName, CommonName, Measurement)

mimeas_sum <- mimeas |> group_by(SiteCode, Year, PlotName, StartDate, SpeciesCode,
                                 ScientificName, CommonName) |>
  summarize(num_meas = sum(!is.na(Measurement)), .groups = 'drop')

mi_comb <- full_join(micnt_sum, mimeas_sum,
                     by = c("SiteCode", "Year", "PlotName", "StartDate", "SpeciesCode",
                            "ScientificName", "CommonName")) |>
  mutate(num_meas = ifelse(is.na(num_meas), 0, num_meas),
         num_count = ifelse(is.na(num_count), 0, num_count),
         incorr_count = ifelse(num_count == num_meas | num_count >10 & num_meas == 10, 0, 1)) |>
  filter(incorr_count == 1)

nrow(mi_comb) #319 records, so splitting up results to be more digestable

# Check for number of measurements > 10 for a given species and photoplot
mi_comb11 <- mi_comb |> filter(num_meas > 10) |> select(-incorr_count)

QC_table <- rbind(QC_table, QC_check(mi_comb11, "Photoplot Motile Inverts",
                                     "Motile Invert species with more than 10 measurments."))

mimeas11_tbl <- make_kable(mi_comb11, "Motile Invert species with more than 10 measurments.")

# Check for crab species with measurements
mi_crab <- mi_comb |> filter(SpeciesCode %in% c("HEMISAN", "CARMAE")) |>
  filter(num_meas > 0) |> select(-incorr_count)

QC_table <- rbind(QC_table, QC_check(mi_crab, "Photoplot Motile Inverts",
                                     "Crab species with measurements."))

mi_crab_tbl <- make_kable(mi_crab, "Crab species with measurements.")

# Check for number of measurements < number of counts
mi_meas_miss <- mi_comb |> filter(!SpeciesCode %in% c("HEMISAN", "CARMAE")) |>
  filter(incorr_count == 1) |>
  filter(num_meas < num_count) |>
  filter(num_meas <= 10) |> select(-incorr_count)

QC_table <- rbind(QC_table, QC_check(mi_meas_miss, "Photoplot Motile Inverts",
                                     "Motile Invert species with fewer measurements than counts (under 10)."))

mi_meas_miss_tbl <- make_kable2(mi_meas_miss, "Motile Invert species with fewer measurements than counts (under 10).")

# Check for number of counts < number of measurements
mi_cnt_miss <- mi_comb |> filter(!SpeciesCode %in% c("HEMISAN", "CARMAE")) |>
  filter(incorr_count == 1) |>
  filter(num_meas > num_count) |> select(-incorr_count)

QC_table <- rbind(QC_table, QC_check(mi_cnt_miss, "Photoplot Motile Inverts",
                                     "Motile Invert species with fewer counts than measurements."))

mi_cnt_miss_tbl <- make_kable2(mi_cnt_miss, "Motile Invert species with fewer counts than measurements.")

# Check that all photoplots sampled for cover have motile invert counts and vice versa
photo <- do.call(getPhotoCover, arglist) |>
  group_by(SiteCode, Year, StartDate, QAQC, CommunityType, PlotName) |>
  summarize(photo_sampled = 1, .groups = "drop")

motinv <- do.call(getMotileInvertCounts, arglist) |>
  group_by(SiteCode, Year, StartDate, QAQC, CommunityType, PlotName) |>
  summarize(count = sum(No.Damage) + sum(Damage),
            .groups = 'drop') |>
  mutate(motinv_sampled = ifelse(!is.na(count), 1, 0)) |> select(-count)

photo_vs_mot <- left_join(photo, motinv, by = c("SiteCode", "Year", "StartDate", "QAQC", "CommunityType", "PlotName")) |>
  filter(is.na(motinv_sampled))

QC_table <- rbind(QC_table, QC_check(photo_vs_mot, "Photoplot Motile Inverts",
                                     "Motile Invert species with fewer counts than measurements."))

photo_vs_mot_tbl <- make_kable2(photo_vs_mot, "Photoplots missing motile Invert counts. Note that LITHUN Red Algae records are because there are no records for the Red Algae plots in the Bolts view.")

mot_vs_photo <- left_join(motinv, photo, by = c("SiteCode", "Year", "StartDate", "QAQC", "CommunityType", "PlotName")) |>
  filter(is.na(photo_sampled))

QC_table <- rbind(QC_table, QC_check(mot_vs_photo, "Photoplot Motile Inverts",
                                     "Motile Invert data missing photoplot data."))

mot_vs_photo_tbl <- make_kable2(mot_vs_photo, "Motile Invert data missing photoplot data.")


# Check if photoplot tab returned any records to determine whether to plot that tab in report
photoplot_mi_check <- QC_table |> filter(Data %in% "Photoplot Motile Inverts" & Num_Records > 0)

photoplot_mi_include <- tab_include(photoplot_mi_check)


#----- Echinoderms -----
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
