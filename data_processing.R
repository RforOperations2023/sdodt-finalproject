library(qdapTools)

# Saving meeting and port data as RDS file so that they read in faster
encounter <- read.csv("data/encounter.csv")
loitering <- read.csv("data/loitering.csv")
port <- read.csv("data/port.csv")

df <- rbind(loitering, encounter)
df <- cbind(df, mtabulate(strsplit(df$regions.rfmo, "[|]")))

ship_mmsi <- rbind(encounter, loitering) %>%
  group_by(vessel.mmsi) %>%
  summarise(number_of_meetings = n_distinct(id)) %>%
  filter(number_of_meetings > 10) %>%
  pull(vessel.mmsi)

port <- port %>%
  filter(vessel.mmsi %in% ship_mmsi)

df <- df %>%
  filter(vessel.mmsi %in% ship_mmsi)
saveRDS(df, file = "data/dataset.RDS")
saveRDS(port, file = "data/port.RDS")


# Saving all ship IDs for the selection field
save(list = c("ship_mmsi"), file = "data/ship_ids.Rdata")


# Saving subtitle as RDS to read into UI
subtitle <- paste0(
  "Illegal fishing is a major ecological and humanitarian problem. ",
  "This portal aims to provide details on some of the largest offenders",
  ", so-called 'reefers'. <br><br>",
  "Reefers are large cargo vessels that meet fishing boats around the ",
  "ocean, collect their fish, and give them fuel. ",
  "This allows fishing vessels to stay undetected while fishing in areas ",
  "where they are not allowed to fish. <br><br>",
  "Reefers help conceal where the fish is coming from and allow illegally ",
  "caught fish to enter the supply chain.<br><br>",
  "Meetings (regardless of whether they are tracked or dark) have been ",
  "designated as illegal under UN conventions.<br><br>",
  "The data for this portal comes from Global Fishing Watch.")
saveRDS(HTML(subtitle),
  file = "data/subtitle.RDS"
)



## download from https://www.nato.int/structur/AC/135/main/scripts/data/ncs_country_codes.txt
nato_countries <- read.csv(file = "data/nato_countries.csv")


# Downloading EEZ data
library(sp)
library(rworldmap)
library(rgeos)
r.pts <- sp::SpatialPoints(r)

# download file from here: http://www.marineregions.org/download_file.php?fn=v9_20161021
# put the zip file in your working directory: getwd()
unzip('data/World_EEZ_v9_20161021.zip')


# Filtering EEZ shapefile by only U.S. EEZ. Otherwise it takes too long to read in
us_eezs <- sf::st_read("data/World_EEZ_v11_20191118/eez_v11.shp") %>%
  filter(ISO_TER1 == "USA")
saveRDS(us_eezs, file = "data/shapefiles_for_us_eez.RDS")