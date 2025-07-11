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
dates <- as.Date(time, origin = origin)

names(ERA5$var)

# Loop through variables, converting each to long-format data.table
ERA5_dat <- list()

v <- names(ERA5$var)[1]
for(v in names(ERA5$var)){
  ERA5_dat[[v]] <- ncvar_get(ERA5, v)
  dimnames(ERA5_dat[[v]]) <- list(lon=lon, lat=lat, date=as.character(dates))
  str(ERA5_dat[[v]])
  ERA5_dat[[v]] <- as.data.table(reshape2::melt(ERA5_dat[[v]], value.name = v))
  ERA5_dat[[v]][, date := as.character(date), ]
}

ggplot(ERA5_dat[[v]][date == "1979-01-02"]) + geom_raster(aes(x = lon, y = lat, fill = mean_2m_air_temperature)) + coord_equal()


names(ERA5$var)
str(ERA5)

msl <- ncvar_get(ERA5, varid = 'mean_sea_level_pressure')

msl


ERA5 <- rast('data/ERA5_daily_1deg.nc')
ERA5

sources(ERA5)

ERA5[[15166]]

names(ERA5)[1:20]

min_air_temp <- ERA5[[1:15165]]
min_air_temp

subset(min_air_temp, s = 1:10)

str(array(ERA5))

prod(dim(ERA5)[1:2])

rep(time(subset(ERA5, s = 1:10)), each = prod(dim(ERA5)[1:2]))

ERA5_dt <- cbind(date = rep(time(subset(ERA5, s = 1:10)), each = prod(dim(ERA5)[1:2])),
                 as.data.table(subset(ERA5[[1:15165]], s = 1:10), xy = TRUE))

ERA5_dt

ggplot(ERA5_dt) + geom_raster(aes(x = x, y = y, fill = ))

time(subset(ERA5, s = 1:10))






# Step 1: Extract time vector
dates <- time(ERA5)

# Step 2: Get varnames in order from the SpatRaster
varnames <- unique(varnames(ERA5))



# Get all layer names
layer_names <- names(ERA5)

# Extract variable names from layer names
var_for_each_layer <- sub("_\\d+$", "", layer_names)  # remove trailing _number

# Split layer indices by variable name
layer_indices_by_var <- split(seq_along(layer_names), var_for_each_layer)

# Now extract rasters per variable based on actual names
raster_list <- lapply(layer_indices_by_var, function(idx) ERA5[[idx]])

# This now works without error:
names(raster_list) <- names(layer_indices_by_var)



names(raster_list)

dt_list <- list()

n <- names(raster_list)[1]
for(n in names(raster_list)){
  dt_list[[n]] <- cbind(date = rep(time(subset(raster_list[[n]], s = 1:10)), each = prod(dim(ERA5)[1:2])),
                        as.data.table(subset(raster_list[[n]], s = 1:10), xy = TRUE))
}

dt_list[[n]][x == 1.5 & y == 1.5]

raster_list

ggplot(dt_list[[n]][date == "1979-01-02"]) + geom_raster(aes(x = x, y = -y, fill = dewpoint_2m_temperature_5)) + coord_equal()

ggplot(dt_list[[n]][x == 1.5 & y == 1.5]) + geom_point(aes(x = dewpoint_2m_temperature_1, y = dewpoint_2m_temperature_5)) + coord_equal()








# Step 1: Convert to wide-format data.table
dt_wide <- as.data.table(as.data.frame(subset(raster_list[[n]], 1:10), xy = TRUE))

# Step 2: Melt into long format
dt_long <- melt(dt_wide, id.vars = c("x", "y"),
                variable.name = "layer", value.name = varnames(raster_list[[n]])[1])

# Step 3: Extract date index from layer names
dt_long[, time_index := as.integer(gsub(".*_(\\d+)$", "\\1", layer))]

# Step 4: Map index to actual date
dates <- time(raster_list[[n]])  # get date vector for this variable
dt_long[, date := dates[time_index]]

# Step 5: Optional cleanup
dt_long[, c("layer", "time_index") := NULL]








# Assume r is a SpatRaster with multiple layers of the same variable
r <- raster_list[[n]]  # e.g., dewpoint_2m_temperature

# Extract raster values in long format
df_long <- as.data.table(terra::as.data.frame(subset(r, s = 1:10), xy = TRUE, cells = FALSE, long = TRUE))

# `df_long` will have columns: x, y, layer, value
# Rename "value" column to actual variable name
setnames(df_long, "value", varnames(r)[1])

# Add actual dates using the layer index
df_long[, date := time(r)[layer]]
df_long[, layer := NULL]  # optional cleanup







# Convert each raster stack to a data.table
dt_list <- lapply(names(raster_list), function(varname) {
  r <- raster_list[[varname]]
  
  # Convert to data.frame with spatial coordinates
  df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  
  # Melt to long format: one row per x, y, time
  dt <- melt(as.data.table(df), id.vars = c("x", "y"), 
             variable.name = "layer", value.name = varname)
  
  # Extract the time index from the layer name (assumes names end in _1, _2, ...)
  dt[, time_index := as.integer(gsub(".*_(\\d+)$", "\\1", layer))]
  
  # Map time_index to actual date
  dt[, date := dates[time_index]]
  
  # Clean up
  dt[, c("layer", "time_index") := NULL]
  
  return(dt)
})





# Step 3: Split the SpatRaster by variable (each has 15165 layers)
layers_per_var <- length(dates)

raster_list <- lapply(seq_along(varnames), function(i) {
  start <- (i - 1) * layers_per_var + 1
  end <- i * layers_per_var
  ERA5[[start:end]]
})
names(raster_list) <- varnames

# Step 4: Convert each variable's raster stack to long-format data.table
dt_list <- lapply(names(raster_list), function(varname) {
  r <- raster_list[[varname]]
  
  # Convert to data.frame with coordinates
  df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  
  # Convert to data.table and reshape
  dt <- melt(as.data.table(df), id.vars = c("x", "y"), 
             variable.name = "layer", value.name = varname)
  
  # Extract layer index and map to date
  dt[, time_index := as.integer(gsub(".*_", "", layer))]
  dt[, date := dates[time_index]]
  dt[, c("layer", "time_index") := NULL]
  
  return(dt)
})

# Step 5: Merge all variables into one data.table by x, y, date
dt_xyz <- Reduce(function(x, y) merge(x, y, by = c("x", "y", "date")), dt_list)
     