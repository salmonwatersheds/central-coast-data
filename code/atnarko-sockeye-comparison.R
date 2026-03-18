# Atnarko sockeye data were provided by Kate McGivney (DFO) in 
# https://github.com/Pacific-salmon-assess/Atnarko-sockeye/blob/main/data/spawners/spawners.csv
# How do these compare to NuSEDS?
library(dplyr)

nuseds <- read.csv("~/Downloads/All Areas NuSEDS_20250103.csv")
cuss <- read.csv("~/Downloads/conservation_unit_system_sites.csv")

# Define variables to include in MAX_ESTIMATE
var_in_MAX_ESTIMATE <- c("NATURAL_ADULT_SPAWNERS", "NATURAL_JACK_SPAWNERS", "NATURAL_SPAWNERS_TOTAL", "ADULT_BROODSTOCK_REMOVALS", "JACK_BROODSTOCK_REMOVALS", "TOTAL_BROODSTOCK_REMOVALS", "OTHER_REMOVALS", "TOTAL_RETURN_TO_RIVER")

# Calculate MAX_ESTIMATE
nuseds$MAX_ESTIMATE <- apply(nuseds[, var_in_MAX_ESTIMATE], 1, max, na.rm = TRUE)
nuseds$MAX_ESTIMATE[nuseds$MAX_ESTIMATE == -Inf] <- NA

unique(nuseds$POPULATION[grep("Atnarko", nuseds$POPULATION)])

# "Atnarko Spawning Channel (Bella Coola) Sockeye" # Just a few years of data in the 1
# "Atnarko Lakes Sockeye"
# "Atnarko River (Bella Coola) Sockeye" 

nuseds1 <- nuseds %>% filter(grep("Atnarko", POPULATION))

nuseds1 <- nuseds %>% 
  filter(POPULATION %in% c("Atnarko Spawning Channel (Bella Coola) Sockeye", "Atnarko Lakes Sockeye", "Atnarko River (Bella Coola) Sockeye") & !is.na(MAX_ESTIMATE)) %>%
  select(WATERBODY, POPULATION, POP_ID, ANALYSIS_YR, MAX_ESTIMATE, ESTIMATE_CLASSIFICATION, ESTIMATE_METHOD) %>%
  arrange(POPULATION, ANALYSIS_YR)

# Data from Kate
atn_spw <- read.csv("data/spawners.csv")
# old Atnarko data
old_dat2 <- read.csv("/Users/stephaniepeacock/Library/CloudStorage/Dropbox-SalmonWatersheds/Stephanie\ Peacock/X\ Drive/1_PROJECTS/1_Active/Central\ Coast\ PSE/analysis/data\ for\ Katy/dataset_1part2.MAR162018_kkAtnarkoFix.csv") %>% 
  filter(CUID == 528)

old_dat1 <- read.csv("/Users/stephaniepeacock/Library/CloudStorage/Dropbox-SalmonWatersheds/Stephanie\ Peacock/X\ Drive/1_PROJECTS/1_Active/Central\ Coast\ PSE/analysis/data\ for\ Katy/dataset_1part1.May312018.csv") %>% 
  filter(CUID == 528)

plot(atn_spw$year, atn_spw$total_spawn, "o", xlim = c(1950,2024), ylim = c(0, 103510), xlab = "", ylab = "abundance")
abline(v = seq(1950, 2025, 2), lwd = 0.5, col = grey(0.8))
points(nuseds1$ANALYSIS_YR, nuseds1$MAX_ESTIMATE, col = as.numeric(factor(nuseds1$POPULATION, levels = c("nuggy", "Atnarko Spawning Channel (Bella Coola) Sockeye", "Atnarko Lakes Sockeye", "Atnarko River (Bella Coola) Sockeye"))), cex = 1.2, lwd = 1.2)

points(old_dat2$year, old_dat2$NuSEDS.counts.by.stream, pch = 19, cex = 0.6)
points(old_dat1$Year, old_dat1$LGL.counts, pch = 3, col = grey(0.6), cex = 1.5)
points(old_dat1$Year, old_dat1$Total.run, pch = 4)

legend(1997, 120000, pt.cex = c(1, 1.2, 1.2, 1.2, 0.6, 1.5, 1), c("GitHub 2025", "Atnarko Sp. Ch. (POP_ID = 45641)", "Atnarko Lks (POP_ID = 7818)", "Atnarko R. (POP_ID = 51780)", "dataset_1part2.MAR162018_kkAtnarkoFix.csv", "dataset_1part1.May312018.csv (LGL.count)", "dataset_1part1.May312018.csv (Total.run)"), title = "Source", col = c(1:4,1, grey(0.6), 1), pch = c(rep(1,4),19, 3, 4), pt.lwd = 1.2, cex = 0.8, xpd = NA)

# Sketchy recruits per spawner: 
# https://www.dropbox.com/scl/fi/mhjwierzs2itbr5739kan/dataset_5.Atnarko.csv?rlkey=oa9loyqid4275a1cw7c54z945&dl=0 
# Other data: https://www.dropbox.com/scl/fo/i3vstlf30yxfr2xgnvmq7/AF9qP_U4fAgKwgnOVKJlGCc?rlkey=62e784bgu2uob9nwyttyzij9g&dl=0 


#------------------------------------------------------------------------------
# Prep. spawner survey data for PSE
#------------------------------------------------------------------------------
source("../../population-indicators/code/functions_general.R")

ss <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_streamspawnersurveys_output")

ss_cc <- ss %>% filter(region == "Central Coast", species_name == "Lake sockeye")

head(ss_cc)
ss_cc %>% filter(cuid == 528)

ss_raw <- old_dat2 %>% full_join(atn_spw)

ss_raw$NuSEDS.counts.by.stream - ss_raw$total_spawn
ss_raw$stream_observed_count <- apply(ss_raw[, c("NuSEDS.counts.by.stream", "total_spawn")], 1, max, na.rm = TRUE)

ss_raw$NuSEDS.counts.by.stream - ss_raw$stream_observed_count

n_obs <- length(ss_raw$year)
ss_new <- data.frame(
  region = rep("Central Coast", n_obs),
  species_name = rep("Sockeye", n_obs),
  species_qualified = rep("SEL", n_obs),
  cuid = rep(528, n_obs),
  cu_name_pse = rep("South Atnarko Lake", n_obs),
  streamid = rep(NA, n_obs),
  stream_name_pse = rep("Atnarko River", n_obs),
  indicator = rep("N", n_obs),
  latitude = rep(52.341852, n_obs), # Put just below Stillwater, but includes counts above and below in some years
  longitude = rep(-125.771562, n_obs),
  year = ss_raw$year,
  stream_observed_count = ss_raw$stream_observed_count,
  stream_survey_method = rep("Unknown Estimate Method", n_obs), # Fill in when we can
  stream_survey_quality = rep("Unknown", n_obs), # Fill in when we can
  source_id = rep("McGivney_20250331", n_obs)
  )


write.csv(ss_new, file = paste0("output/dataset2_spawner-surveys_CentralCoast_", strftime(Sys.Date(), format = "%Y-%m-%d"), ".csv"), row.names = FALSE)
