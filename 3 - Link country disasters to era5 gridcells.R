# Combining country-level heatwaves data with ERA5:

heatwave_sub <- heatwave[, .(ISO,
                             Country, 
                             Location, 
                             `Total Deaths`, 
                             `Start Year`, 
                             `Start Month`,
                             `Start Day`,
                             `End Year`,
                             `End Month`,
                             `End Day`)]

names(heatwave)
table(!is.na(heatwave$Magnitude))
table(!is.na(heatwave$`Total Deaths`))
table(!is.na(heatwave$`Total Damage ('000 US$)`))

table(!is.na(heatwave$`End Month`))

table(heatwave$`Start Month` == heatwave$`End Month`)

year(daily_met[1:10,]$date)

daily_met[ month := month()]

# unique(daily_met[,.(lon, lat, date)])
# expand grid instead?

dates <- uniquey

unique(year(as.Date(time, origin = origin)))

loc_year_month <- as.data.table(expand.grid(list(lon, lat, 
                                   unique(year(as.Date(time, origin = origin))), 
                                   unique(month(as.Date(time, origin = origin))))))
names(loc_year_month) <- c("lon", "lat", "year", "month")
loc_year_month

loc_countries <- merge(unique(loc_year_month[, .(lon, lat)]), overlaps_melt, all = TRUE)
loc_countries

sort(table(loc_countries$ISO), decreasing = TRUE)

ggplot(loc_countries) + 
  geom_raster(aes(x = lon, y = -lat, fill = ISO)) + coord_equal() +
  theme(legend.position="none")

# ISO_loc <- merge(heatwave_sub[, .(ISO, Country)], loc_countries[, .(ISO, Country, lon, lat)], all = TRUE)[!is.na(ISO)]
# ISO_loc <- overlaps_melt[, .(ISO, lon, lat)]

heatwave_sub_loc <- as.data.table(merge(heatwave_sub, loc_countries[!is.na(ISO), .(ISO, lon, lat)], by = 'ISO', allow.cartesian = TRUE))
heatwave_sub_loc[!is.na(`Start Year`)]
heatwave_sub_loc
