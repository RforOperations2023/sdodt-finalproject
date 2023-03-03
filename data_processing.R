library(qdapTools)

# load("combined.Rdata")
# df <- rbind(loitering, encounter)
# df <- cbind(df, mtabulate(strsplit(df$regions.rfmo, "[|]")))
# saveRDS(df, file = "dataset.RDS")


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


choices <- sort(unique(rbind(encounter, loitering)$vessel.flag))
saveRDS(choices, file = "data/flags.RDS")


save(list = c("ship_mmsi"), file = "data/ship_ids.Rdata")


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



library(sp)
library(rworldmap)
library(rgeos)
# 39.494828, -74.235441
r <- data.frame(long = c(-74.235441, 39.494828, -178.311660375408,-176.511660375408, -174.711660375408, -172.911660375408, -171.111660375408,-169.311660375408), 
               lat = c(39.494828, -74.235441, 73.1088933113454, 73.1088933113454,73.1088933113454, 73.1088933113454, 73.1088933113454, 73.1088933113454))

# or
#r <- data.frame(long = c(-178.311660375408,-176.511660375408, -174.711660375408, -172.911660375408, -171.111660375408,-169.311660375408), 
#                lat = c(73.1088933113454, 73.1088933113454,73.1088933113454, 73.1088933113454, 73.1088933113454, 73.1088933113454))

r.pts <- sp::SpatialPoints(r)

# download file from here: http://www.marineregions.org/download_file.php?fn=v9_20161021
# put the zip file in your working directory: getwd()
unzip('data/World_EEZ_v9_20161021.zip')

# countriesSP <- rworldmap::getMap(resolution = "high")
# or
countriesSP <- rgdal::readOGR(dsn = "World_EEZ_v9_20161021", layer = "eez_boundaries")

r.pts <- sp::SpatialPoints(r.pts, proj4string = sp::CRS(proj4string(countriesSP))) 
indices <- over(r.pts, countriesSP)
indices$MRGID_Ter1
