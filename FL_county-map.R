#Making maps with counties or other natural area boundaries.

#Libraries

library(terra)
library(geodata)
library(maps)
library(wesanderson)
library(spData)
library(sf) #Using this
library(png)
library(oce) #Using this
data("us_states")

#Set some logical variables

save_figs = FALSE

#Let's read a shapefile with county polygons.

counties = read_sf("mapping/cb_2024_us_county_500k/cb_2024_us_county_500k.shp")

logo = readPNG("Herbarium_Logo_Orchid_Square_Black_Trans.png")

#Subset by state

FL = counties$STATE_NAME == "Florida"
FL_counties = counties[FL, ]

###Blocking all of this out for testing... CK 7OCT2026
#Plot outlines and add Hillsborough county as fill. This is where you can pull from a file or list of some kind to automate.
#
#if(save_figs == TRUE){
#  setEPS()
#  tiff("SV_specimen-map.tiff", height = 2200, width = 2300, res = 300)
#}
#
#plot(0, 0, xlim = c(-88, -78), ylim = c(24,32), axes = FALSE, ann = FALSE)
#plot(FL_counties$geometry, add = TRUE)
#plot(FL_counties$geometry[FL_counties$NAME == "St. Lucie"], add = TRUE, col = "red")
#
##Add herbarium logo - needs adjustment.
#
#rasterImage(logo, -81,29,-79,31)
#
#if(save_figs == TRUE){
#  dev.off()
#}
#
#Plotting conservation lands boundaries for plotting or research.

#Define window - format: xmin, ymin, xmax, ymax OR Lon_min, Lat_min, Lon_max, Lat_max OR UTMe_win[1], UTMn_win[1], UTMe_win[2], UTMn_win[2]

#Lat/Lon is preferred Herbarium format.
Lat_win = c(28,29.5)
Lon_win = c(-83.00, -81.5)

#Most modern GPS runs on UTMs. Need to convert. OCE library
UTM_min = lonlat2utm(Lon_win[1], Lat_win[1])
UTM_max = lonlat2utm(Lon_win[2], Lat_win[2])

#Define the extent of the plot - can we use this to trim giant files from FNAI?
LL_extent = c(-83.0, 28, -81.5, 29.5) #long/lats
UTM_extent = c(UTM_min[[1]], UTM_min[[2]], UTM_max[[1]], UTM_max[[2]]) #UTMs - need options, apparently?
names(UTM_extent) = c("xmin", "ymin", "xmax", "ymax")
names(LL_extent) = c("xmin", "ymin", "xmax", "ymax")

#Setting the boudnaries/extent for mapping. Using a method to call a function from a library without loading it "raster::extent()"
extentUTM <- raster::extent(UTM_min[[1]], UTM_max[[1]], UTM_min[[2]], UTM_max[[2]])
extentLND <- raster::extent(52652.5, 799595.9, 45555.78, 781583)

#We need a sf object to work with the sf library. To do so, we define the extent with a coordinate system.
extentLND_sf <- st_set_crs(st_as_sf(as(extentLND, "SpatialPolygons")), "EPSG:3087") #This is Florida UTMs - NAD83(HARN) / Florida GDL Albers
extentUTM_sf <- st_set_crs(st_as_sf(as(extentUTM, "SpatialPolygons")), "EPSG:3087")

#For some reason we're making another extent here...different EPSG too...
#We're doing this because it's a little easier to experiement with it this way - need to make some extents for relevant projects...
extentLL_sf <- st_set_crs(st_as_sf(as(raster::extent(-82.65003, -81.91927, 28.41894, 29.00968), "SpatialPolygons")), "EPSG:4269") #NAD 83
extentOK_co <- st_set_crs(st_as_sf(as(raster::extent(-81.5, -80.5, 27, 28), "SpatialPolygons")), "EPSG:4269") #NAD 83
#Pulling conservation lands from FNAI

cons_lands = st_read("maps/flma_202512/FloridaConservationLands.gdb") #conservation lands is a geodatabase
WithlacoocheeSF = cons_lands[2889,] #This is Withlacoochee State Forest
KissPrairPresSF = cons_lands[1217,] #This is Kissimmee Praire Preserve State Park

plot(WithlacoocheeSF$Shape) #This just plots the area.

#Here we transform the original object to a different coordinate system.
WithlacoocheeSFt = st_transform(WithlacoocheeSF, "EPSG:4269")
KissimmeePresSFt = st_transform(KissPrairPresSF, "EPSG:4269")

#Read landcover from FWC, file is huge and will need to be trimmed. UPDATE: Colleen may have saved the day

WTL_landcover = read_sf("maps/Export_WSF_Current_NC_Pys_2024/Export_WSF_Current_NC_Pys_2024.shp") #This only plots inside the forest area! 

state_landcover = st_read("maps/Polygon/CLC_v4_Poly.gdb") #Another geodatabase, showing statewide landcover from FNAI
sf_use_s2(FALSE) #This setting lets us put straight lines on a curved reality. It's complicated.
stld_LatLon = st_transform(state_landcover, "EPSG:4269") #Turning state landcover classes into NAD83 UTM
LL_crop = st_intersection(stld_LatLon, extentLL_sf) #One way to make giant files smaller is to crop them.
OK_crop = st_intersection(stld_LatLon, extentOK_co) #Okeechobee co. extent.


#hydric_flatwoods = state_landcover[state_landcover$NAME_SITE == "Hydric Pine Flatwoods",]
#stld_crop = st_crop(hydric_flatwoods, UTM_extent)




#Transform counties

#counties_UTMGDL = st_transform(FL_counties: "EPSG:3087")
counties_LatLon = st_transform(FL_counties, "EPSG:4269")

#Import USF Herbarium collections with LAT/LON data

WTL_sp <- read.csv("WTL_specimens.csv", header = TRUE, row.names = "barcode")

WTL_sploc = WTL_sp[is.na(as.numeric(WTL_sp$LatDecL))==FALSE,]
WTL_points = data.frame(as.numeric(WTL_sploc$LatDecL),as.numeric(WTL_sploc$LongDecL))

WTL_UTMpt = lonlat2utm(as.numeric(WTL_points$WTL_sploc.LongDecL), as.numeric(WTL_points$WTL_sploc.LatDecL))

#This is complicated by the geodatabase thing which isn't playing nice with R...try this method to convert points.

#Need numeric matrix of locations first...

WTL_spmx = matrix(c(easting = WTL_UTMpt$easting, northing = WTL_UTMpt$northing), nrow = length(WTL_UTMpt$easting), ncol = 2)

WTL_Cpoint = st_multipoint(WTL_spmx)

WTL_geom = st_sfc(WTL_Cpoint, crs = "EPSG:4269")


WTL_attrib = data.frame(
  name = row.names(WTL_sp)[is.na(as.numeric(WTL_sp$LatDecL))==FALSE],
  genus = WTL_sp$genus[is.na(as.numeric(WTL_sp$LatDecL))==FALSE],
  date = WTL_sp$Collection.Date[is.na(as.numeric(WTL_sp$LatDecL))==FALSE]
)

WTL_sf = st_sf(WTL_attrib, geometry = WTL_geom)

#We need to extract vegetation communities for plotting.

types = unique(LL_crop$NAME_SITE)

#Summarize by area

type_area = sapply(types, function(x){
  n = sum(LL_crop$Shape_Area[LL_crop$NAME_SITE == x])
})

type_area = type_area[rev(order(type_area))]

#Let's make a table of the most common types and map them.

type_table = data.frame(type_area[1:20])

#Wrote a table that I will fill with color and class definitions to ease plotting. Will read it at the top and use it here...

type_table = read.csv("WTL_regional-types.csv", header = TRUE)

#Method for combining things based on site types...


plot(st_union(LL_crop[LL_crop$NAME_SITE == type_table$TYPE[1] |
                      LL_crop$NAME_SITE == type_table$TYPE[2] |
                      LL_crop$NAME_SITE == type_table$TYPE[3] ,]), col = "darkorange")

hyd_hmk = st_combine(OK_crop[OK_crop$NAME_STATE == "Hydric Hammock",])
frsh_wt = st_combine(OK_crop[OK_crop$NAME_STATE == "Isolated Freshwater Marsh" |
                             OK_crop$NAME_STATE == "Freshwater Non-Forested Wetlands",])


###Empty function is empty.
#impacted = sapply(type_table$TYPE[type_table$CLASS == type_table$CLASS[1]], function(x){
#  
#})

#Plotting

#plot(0,0, xlim = c(-83.0, -82.0), ylim = c(28.0,29.0), pch = NA) #Big View
plot(0,0, xlim = c(-81.3000, -80.9000), ylim = c(27.45,27.7), pch = NA) #Kissimmee Prairie Preserve View

plot(counties_LatLon$geometry, add = TRUE, lty = 2, col = NA)

#plot(WithlacoocheeSFt$Shape, add = TRUE, col = "lightblue")

for(i in 1:nrow(type_table)){
  plot(OK_crop$Shape[OK_crop$NAME_STATE == type_table$TYPE[i]], add = TRUE, col = type_table$COLOR[i], lwd = 0.5)
  print(paste0(i, " of ", nrow(type_table), " types plotted"))
}

#points(WTL_points$as.numeric.WTL_sploc.LongDecL., WTL_points$as.numeric.WTL_sploc.LatDecL., pch = 21, bg = "gold")

#Let's turn this around and make a plot for Okechobee Co. 

###Need better choices for what I'm plotting.

type_names = c("Impacted", #1
               "Herbaceous Wetland", #2
               "Forested Wetland", #3
               "Hammocks", #4
               "Grasslands", #5
               "Open Water", #6
               "Flatwoods", #7
               "Mixed Forests", #8
               "Riparian", #9
               "Scrub", #10
               "Sandhill", #11
               "Undiff. Shrubland", #12
               "Barrens", #13
               "Coastal Strand", #14
               "Estuaries" #15
                )

KS_plotkey = c(FALSE,
               TRUE,
               TRUE,
               TRUE,
               TRUE,
               FALSE,
               TRUE,
               TRUE,
               TRUE,
               TRUE,
               TRUE,
               FALSE,
               TRUE,
               TRUE,
               TRUE)


type_colors = c("darkorange",
                "lightblue",
                "darkgreen",
                "forestgreen",
                "gold",
                "blue",
                "goldenrod",
                "lightgreen",
                "darkgreen",
                "orange",
                "goldenrod2",
                "yellow",
                "white",
                "darkblue",
                "darkblue")

state_clkey = c(0,  0,  1,  2,  3,  4,  0,  0,  0,  0,
                0,  0,  3,  3,  3,  5,  0,  0,  6,  2,
                2,  7,  0,  3,  6,  7,  8,  2,  6,  0,
                0,  3,  5,  3,  9,  0,  10, 11, 10, 7,
                12, 0,  8,  0,  4,  3,  0,  3,  13, 3,
                0,  0,  4, 10,  2,  7,  6,  0,  14, 0,
                15, 15, 4,  2,  0,  3,  0,  6,  0)


#Adding DEM

#ksSRTM <- elevation_3s(-81, 27, "mapping/") #Hashed this out because file exists, but this will download it.

ksTILE1 <- raster::raster("maps/n27_w082_1arc_v3.tif")
ksTILE2 <- raster::raster("maps/n27_w081_1arc_v3.tif")
ksSRTM = raster::mosaic(ksTILE1, ksTILE2, fun = mean)

plot(0,0, xlim = c(-81.25, -80.90), ylim = c(27.45,27.7), pch = NA) #Kissimmee Prairie Preserve View

#plot(ksSRTM, add = TRUE, alpha = 0.8, col = gray.colors(121))
plot(counties_LatLon$geometry, add = TRUE, lty = 2, col = NA)
plot(KissimmeePresSFt$Shape, add = TRUE, col = "pink")



for(i in 1:length(type_colors)){
  if(KS_plotkey[i] == TRUE){
    plot(st_combine(OK_crop$Shape[state_clkey == i-1,]), add = TRUE, col = type_colors[i])
  }
}

plot(ksSRTM, add = TRUE, alpha = 0.8, col = gray.colors(50))



