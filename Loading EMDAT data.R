library(data.table)
library(ggplot2)
library(readxl)

# Load EMDAT data, and explore ####
emdat <- as.data.table(read_excel('data/EMDAT/241204_emdat_archive.xlsx'))
emdat

names(emdat)

emdat[,which(grepl('Heat wave', emdat)), with = FALSE]

sort(table(emdat$`Disaster Subtype`)) # 295 cases are Heat wave
sort(table(emdat$`Associated Types`)) # 51 cases are Heat wave
sort(table(emdat$`Origin`)) # 3 cases are Heat wave

table(emdat[`Associated Types` == "Heat wave", ]$`Disaster Subtype`)
table(emdat[`Associated Types` == "Heat wave", ]$`Disaster Type`)
# Drought and wildfire can have "Heat wave" as associated type

table(emdat[`Disaster Subtype` == "Heat wave", ]$`Disaster Type`)
# Where subtype is "Heat wave" disaster type is always "Extreme temperature"

table(emdat[`Disaster Type` == "Extreme temperature", ]$`Disaster Subtype`)
# But, disaster type of "Extreme temperature" can be cold or hot

# It seems we want to use 'Disaster Subtype' == "Heat wave" as our labels
# This gives us 295 labels

heatwave <- emdat[`Disaster Subtype` == "Heat wave", ]
heatwave <- heatwave[,which(unlist(lapply(heatwave, function(x)!all(is.na(x))))),with=F]
# Excludes columns with only NA values

heatwave

hist(heatwave$`Start Year`, breaks = 50)
hist(heatwave$`Start Month`, breaks = 12)
hist(heatwave$`Start Day`, breaks = 31)

hist(heatwave$`Total Deaths`, breaks = 50)
hist(log(heatwave$`Total Deaths`), breaks = 18)

sort(table(heatwave$ISO))
sort(table(heatwave$Location))

heatwave[is.na(ISO), ]
View(heatwave[order(`Start Year`), ])

sum(heatwave$`Total Deaths`, na.rm = TRUE) # total 282 420 heatwave deaths

# Load countries data, in order to be able to link EMDAT to location ####
library(sf)

countries <- st_transform(st_read("data/countries/World_Countries_Generalized.shp"), crs = 4326)
countries

ggplot() + 
  geom_raster(data = daily_met[date == "1979-01-02"], aes(x = lon, y = lat, fill = minimum_2m_air_temperature)) +
  geom_sf(data = countries, size = 1.5, color = "darkred", fill = "transparent") +
  scale_fill_viridis_c(option = "G") + coord_sf()
ggsave("plots/countries_with_min_air_temp_1979_01_02.png", width = 140, height = 64, units = "mm", dpi = 300, scale = 1.325)
