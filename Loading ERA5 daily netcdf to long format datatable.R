library(ncdf4)
library(data.table)
library(ggplot2)

ERA5 <- nc_open('data/ERA5_daily_1deg.nc')

# Extract lon, lat, and time values
lon <- ncvar_get(ERA5, "lon")
lat <- ncvar_get(ERA5, "lat")
time <- ncvar_get(ERA5, "time")
time_units <- ncatt_get(ERA5, "time", "units")$value

# Extract origin from time units string (e.g., "days since 1900-01-01")
origin <- sub(".*since ", "", time_units)
# dates <- as.Date(time, origin = origin)

names(ERA5$var)

mean_2m_air_temperature <- ncvar_get(ERA5, "mean_2m_air_temperature")
dimnames(mean_2m_air_temperature) <- list(lon=lon, lat=lat, date=time)

daily_met <- as.data.table(reshape2::melt(mean_2m_air_temperature, value.name = "mean_2m_air_temperature"))
daily_met

daily_met[, minimum_2m_air_temperature := as.numeric(ncvar_get(ERA5, "minimum_2m_air_temperature"))]
daily_met[, maximum_2m_air_temperature := as.numeric(ncvar_get(ERA5, "maximum_2m_air_temperature"))]
daily_met[, dewpoint_2m_temperature := as.numeric(ncvar_get(ERA5, "dewpoint_2m_temperature"))]
daily_met[, total_precipitation := as.numeric(ncvar_get(ERA5, "total_precipitation"))]
daily_met[, surface_pressure := as.numeric(ncvar_get(ERA5, "surface_pressure"))]
daily_met[, mean_sea_level_pressure := as.numeric(ncvar_get(ERA5, "mean_sea_level_pressure"))]
daily_met[, u_component_of_wind_10m := as.numeric(ncvar_get(ERA5, "u_component_of_wind_10m"))]
daily_met[, v_component_of_wind_10m := as.numeric(ncvar_get(ERA5, "v_component_of_wind_10m"))]

daily_met[, date := as.Date(date, origin = origin), ]

gc()

ggplot(daily_met[date == "1979-01-02"]) + geom_raster(aes(x = lon, y = lat, fill = total_precipitation)) + coord_equal() +
  scale_fill_viridis_c(option = "G")


