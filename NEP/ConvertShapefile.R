FU <- geojsonio::geojson_read("Nephrops_FU_20160621.shp", what = "sp")
FU_simple <- rmapshaper::ms_simplify(FU, keep = 0.05, keep_shapes = TRUE)
geojsonio::geojson_write(FU_simple, file="FU_simple.geojson")
div2 <- geojsonio::geojson_read("FU_simple.geojson", what = "sp")



library(rgdal)
# From https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
states <- readOGR("Shapefiles/cb_2017_us_state_20m.shp",
                  layer = "cb_2017_us_state_20m", GDAL1_integer64_policy = TRUE)
neStates <- subset(states, states$STUSPS %in% c(
  "CT","ME","MA","NH","RI","VT","NY","NJ","PA"
))
leaflet(neStates) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))



FU <- readOGR("Nephrops_FU_20160621.shp")
leaflet(FU) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              #fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))
