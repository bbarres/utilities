##############################################################################/
##############################################################################/
#code borrowed from https://dieghernan.github.io/201906_Beautiful1/
##############################################################################/
##############################################################################/

library(sf)
library(rnaturalearth)
library(dplyr)
library(RColorBrewer)

library(maps)
library(rworldmap)
library(rgdal)
library(rgeos)


##############################################################################/
#loading and formating the data####
##############################################################################/

GB <- ne_download(50,
                  type = "map_subunits",
                  returnclass = "sf",
                  destdir = tempdir()
) %>%
  subset(CONTINENT == "Europe") %>%
  subset(ADM0_A3 == "GBR")

# Projecting and cleaning
GB <- st_transform(GB, 3857) %>% select(NAME_EN, ADM0_A3)

initial <- GB
initial$index_target <- 1:nrow(initial)
target <- st_geometry(initial)

# Create my own color palette
mypal <- colorRampPalette(c("#F3F8F8", "#008080"))


##############################################################################/
#defining a function####
##############################################################################/

stdh_gridpol <- function(sf,
                         to = "fishnet",
                         gridsize = as.integer(
                           min(
                             diff(st_bbox(sf)[c(1, 3)]),
                             diff(st_bbox(sf)[c(2, 4)])
                           ) / 40
                         ),
                         sliver = 0.5) {
  if (!unique(st_geometry_type(sf)) %in% c("POLYGON", "MULTIPOLYGON")) {
    stop("Input should be  MULTIPOLYGON or POLYGON")
  }
  if (!to %in% c("fishnet", "puzzle", "honeycomb", "hexbin", "pixel")) {
    stop("'to' should be 'fishnet','puzzle','honeycomb','hexbin' or 'pixel'")
  }
  
  if (class(sf)[1] == "sf") {
    initial <- sf
    initial$index_target <- 1:nrow(initial)
  } else {
    initial <- st_sf(index_target = 1:length(sf), geom = sf)
  }
  
  target <- st_geometry(initial)
  
  if (to %in% c("fishnet", "puzzle")) {
    sq <- T
  } else {
    sq <- F
  }
  if (to == "pixel") {
    grid <- st_make_grid(target,
                         gridsize,
                         crs = st_crs(initial),
                         what = "centers"
    )
  } else {
    grid <- st_make_grid(
      target,
      gridsize,
      crs = st_crs(initial),
      what = "polygons",
      square = sq
    )
  }
  grid <- st_sf(index = 1:length(lengths(grid)), grid) # Add index
  if (to == "pixel") {
    cent_merge <- st_join(grid, initial["index_target"], left = F)
    grid_new <- st_buffer(cent_merge, gridsize / 2)
  } else {
    cent_grid <- st_centroid(grid)
    cent_merge <- st_join(cent_grid, initial["index_target"], left = F)
    grid_new <- inner_join(grid, st_drop_geometry(cent_merge))
  }
  if (to %in% c("fishnet", "honeycomb", "pixel")) {
    geom <- aggregate(
      grid_new,
      by = list(grid_new$index_target),
      FUN = min,
      do_union = FALSE
    )
  } else {
    geom <- aggregate(
      st_buffer(grid_new, sliver),
      by = list(grid_new$index_target),
      FUN = min,
      do_union = TRUE
    )
  }
  if (class(initial)[1] == "sf") {
    fin <- left_join(
      geom %>% select(index_target),
      st_drop_geometry(initial)
    ) %>%
      select(-index_target)
    fin <- st_cast(fin, "MULTIPOLYGON")
    return(fin)
  } else {
    fin <- st_cast(geom, "MULTIPOLYGON")
    return(st_geometry(fin))
  }
}
# End of the function-----


##############################################################################/
#some examples using the function####
##############################################################################/

fish <- stdh_gridpol(GB, to = "fishnet", gridsize = 50 * 1000)
puzz <- stdh_gridpol(GB, to = "puzzle", gridsize = 50 * 1000)
hon <- stdh_gridpol(GB, to = "honeycomb", gridsize = 50 * 1000)
hex <- stdh_gridpol(GB, to = "hexbin", gridsize = 50 * 1000)
pix <- stdh_gridpol(GB, to = "pixel", gridsize = 50 * 1000)


##############################################################################/
#some example by me, preparing the dataset####
##############################################################################/

sPDF<-getMap()[-which(getMap()$ADMIN=="Antarctica"),]
sPDF<-spTransform(sPDF,CRS=CRS("+proj=robin +ellps=WGS84"))
sPDF<-sPDF[which(sPDF@data$ADMIN!="Antarctica" & 
                  gArea(sPDF,byid=TRUE)>20000000000 ),]
sPDF<-st_as_sf(sPDF) %>% select(SRESmajor,LEVEL)
sPDF<-sPDF[-c(150),]
plot(sPDF)

initial<-sPDF
initial$index_target<-1:nrow(initial)
target<-st_geometry(initial)
mypal<-brewer.pal(12,"Paired")
mypal<-brewer.pal(6,"Dark2")


##############################################################################/
#fishnet map####
##############################################################################/

grid <- st_make_grid(target,
                     130 * 2500,
                     # Kms
                     crs = st_crs(initial),
                     what = "polygons",
                     square = TRUE
)

# To sf
grid <- st_sf(index = 1:length(lengths(grid)), grid) # Add index

# We identify the grids that belongs to a entity by assessing the centroid
cent_grid <- st_centroid(grid)
cent_merge <- st_join(cent_grid, initial["index_target"], left = F)
grid_new <- inner_join(grid, st_drop_geometry(cent_merge))

# Fishnet
Fishgeom <- aggregate(grid_new,
                      by = list(grid_new$index_target),
                      FUN = min,
                      do_union = FALSE
)

# Lets add the df
Fishnet <- left_join(
  Fishgeom %>% select(index_target),
  st_drop_geometry(initial)
) %>%
  select(-index_target)

plot(st_geometry(Honeycomb),col=mypal)
plot(st_geometry(Fishnet),col=mypal,bg="lightblue")
#for a full map 40 x 22.5 with grid set to 50 x 1000


##############################################################################/
#hex map####
##############################################################################/

grid <- st_make_grid(target,
                     130 * 2500, # Kms
                     crs = st_crs(initial),
                     what = "polygons",
                     square = FALSE # This is the only piece that changes!!!
)
# Make sf
grid <- st_sf(index = 1:length(lengths(grid)), grid) # Add index

# We identify the grids that belongs to a entity by assessing the centroid
cent_grid <- st_centroid(grid)
cent_merge <- st_join(cent_grid, initial["index_target"], left = F)
grid_new <- inner_join(grid, st_drop_geometry(cent_merge))

# Honeycomb
Honeygeom <- aggregate(
  grid_new,
  by = list(grid_new$index_target),
  FUN = min,
  do_union = FALSE
)

# Lets add the df
Honeycomb <- left_join(
  Honeygeom %>%
    select(index_target),
  st_drop_geometry(initial)
) %>%
  select(-index_target)


plot(st_geometry(Honeycomb),col=mypal)
#export to pdf 15 x 10
plot(st_geometry(Honeycomb),col=mypal,bg="lightblue")
#for a full map 40 x 22.5 with grid set to 50 x 1000


##############################################################################/
#pixel map####
##############################################################################/

grid <- st_make_grid(target,
                     130 * 2500, # Kms
                     crs = st_crs(initial),
                     what = "centers"
)

# Make sf
grid <- st_sf(index = 1:length(lengths(grid)), grid) # Add index

# We identify the grids that belongs to a entity by assessing the centroid
cent_grid <- st_centroid(grid)
cent_merge <- st_join(cent_grid, initial["index_target"], left = F)
grid_new <- st_buffer(cent_merge, 130 * 2500 / 2)
Pixelgeom <- aggregate(
  grid_new,
  by = list(grid_new$index_target),
  FUN = min,
  do_union = FALSE
)
# Lets add the df
Pixel <- left_join(
  Pixelgeom %>%
    select(index_target),
  st_drop_geometry(initial)
) %>%
  select(-index_target)

plot(st_geometry(Pixel), col = mypal, main = "Pixel")

#the function doesn't work for the data set I use and I can't figure out
#why
fish<-stdh_gridpol(sPDF,to="fishnet",gridsize = 10000 * 200000)


##############################################################################/
#France map for the website Monitoring section####
##############################################################################/

#preparing the data
load("data/cartes/regionsLight.RData")
sPDF<-spTransform(regionsLight,CRS=CRS("+proj=robin +ellps=WGS84"))
sPDF<-st_as_sf(sPDF)
plot(sPDF)
initial<-sPDF
initial$index_target<-1:nrow(initial)
target<-st_geometry(initial)
mypal<-"white"
mypal<-brewer.pal(12,"Paired")
mypal<-brewer.pal(6,"Dark2")
mypal<-brewer.pal(9,"Greys")
mypal<-brewer.pal(11,"Spectral")
mypal<-brewer.pal(12,"Set3")
mypal<-c(brewer.pal(8,"Set2")[1:7],brewer.pal(9,"Set1")[1:6])

#hex map
grid <- st_make_grid(target,
                     60 * 800, # Kms
                     crs = st_crs(initial),
                     what = "polygons",
                     square = FALSE # This is the only piece that changes!!!
)
# Make sf
grid <- st_sf(index = 1:length(lengths(grid)), grid) # Add index

# We identify the grids that belongs to a entity by assessing the centroid
cent_grid <- st_centroid(grid)
cent_merge <- st_join(cent_grid, initial["index_target"], left = F)
grid_new <- inner_join(grid, st_drop_geometry(cent_merge))

# Honeycomb
Honeygeom <- aggregate(
  grid_new,
  by = list(grid_new$index_target),
  FUN = min,
  do_union = FALSE
)

# Lets add the df
Honeycomb <- left_join(
  Honeygeom %>%
    select(index_target),
  st_drop_geometry(initial)
) %>%
  select(-index_target)

#map with one color for each region
mypal<-c(brewer.pal(8,"Set2")[1:7],brewer.pal(9,"Set1")[1:6])
png("output/cartoFrance.png",width=800,height=800,
    units="px",bg="transparent")
plot(st_geometry(Honeycomb),col=mypal,lwd=3)
dev.off()

#map with all regions in white
mypal<-"white"
plot(st_geometry(Honeycomb),col=mypal,lwd=3)
#export to pdf 8 x 8 inches


##############################################################################/
#END
##############################################################################/