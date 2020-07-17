# install.packages('osmdata')
# install.packages('rnaturalearth')
library(osmdata)
library(rnaturalearth)
library(sf)
library(tidyverse)
library(ggsn)
library(ggmap)
library(lwgeom)

# ------ Register Google Maps API key -------------------

ggmap::register_google(key = "AIzaSyBom2FXN3N-mOfawD574O746Osuk0FOyco")


# --------- READ DATA ------------
Otter_catch <- st_read("./7_Site_Location/data/otter_catchment/New_OtterCatch.shp") %>%
  st_make_valid()

st_crs(Otter_catch)

BudBrook_catch <- st_read("./7_Site_Location/data/Bud_Brook_catchment/Bud_Brook_Catch.shp")
st_crs(BudBrook_catch)


ColBrook_catch <- st_read("./7_Site_Location/data/Pophams_catchment/Pophams_Catch.gpkg") %>%
  rename(geometry=geom)
st_crs(ColBrook_catch) 


# ---------- FUNCTIONS -------------

# this function returns an sf object with the desired sf area clipped out from the extent of the ggmap bounding box. This allows
# for the use of alpha in th plot.
ggmap.highlight <- function(ggmap.obj, sf.feat) {
  ggmap.area <- ggmap(ggmap.obj) 
  
  Xs <- ggplot_build(ggmap.area)$layout$panel_scales_x[[1]]$range$range
  Ys <- ggplot_build(ggmap.area)$layout$panel_scales_y[[1]]$range$range
  
  res <- matrix(c(Xs[2], Ys[1], Xs[2], Ys[2], Xs[1], Ys[2], Xs[1], Ys[1], Xs[2], Ys[1]),  ## need to close the polygon
                ncol =2, byrow = T)
  
  area.df <- data.frame(mapbox = 'gmap') %>%
    mutate(geometry = st_sfc(st_polygon(list(res))))
  
  MAP.Bnds.sf <- st_sf(area.df) %>%
    st_set_crs(4326) 
  
  if (missing(sf.feat)){
    return(MAP.Bnds.sf)
  }
  
  gmap.mask <- MAP.Bnds.sf %>%
    st_difference(., sf.feat)
  
  return(gmap.mask)
}


bb.as.sf <- function(sf.obj){
  st_as_sfc(st_bbox(sf.obj)) %>%
    st_set_crs(4326) %>%
    st_as_sf() %>%
    rename(geometry=x)
}

# ----- transform catchment areas from OSGB to WGS84 -----------------

OC_trans <- Otter_catch %>%
  st_transform(crs = 4326)

# st_crs(OC_trans)

BB_trans <- BudBrook_catch %>%
  st_transform(crs = 4326)


CB_trans <- ColBrook_catch %>%
  st_transform(crs = 4326)

# st_crs(BB_trans)

OC_trans.buff <- st_buffer(Otter_catch,500) %>%
  st_transform(crs = 4326)

# st_crs(OC_trans)

BB_trans.buff <- st_buffer(BudBrook_catch,100) %>%
  st_transform(crs = 4326)

CB_trans.buff <- st_buffer(ColBrook_catch,100) %>%
  st_transform(crs = 4326)

# ------- Download Open Map Data for Rivers in Otter Catchment ----------------


box <- OC_trans  %>%
  st_bbox()

Otter_rivers <- opq(bbox = box, memsize = 1073741824) %>%
  add_osm_feature(key='waterway') %>%
  osmdata_sf() %>%
  .$osm_lines %>%
  st_intersection(OC_trans) %>%
  st_union()%>%
  st_transform(crs = 4326) %>%
  st_sf() %>%
  mutate(river = 'River Network')

# ------------- Download GB Area as sf for panel map -----------------------

GB.sf  <- ne_countries(country = 'united kingdom', returnclass = 'sf', scale = 10)%>%
  st_cast(.,"POLYGON") %>%
  mutate(.area = st_area(.)) %>%
  filter(.area == max(.area)) %>%
  st_transform(crs = 4326)


# ---------------- Create Town Points - Required? ---------------
# p1 <- st_point(c(316393,100519))
# p2 <- st_point(c(309853, 95412))
# p3 <- st_point(c(306363, 81929))
# 
# d <- data.frame(place = c('Honiton', 'Ottery St. Mary', 'Budleigh Salterton')) %>%
#   mutate(geometry = st_sfc(p1, p2, p3)) %>%
#   st_as_sf()%>%
#   st_set_crs(27700)%>%
#   st_transform(crs = 4326)
# 
# OtterTowns_lat_lon <- as.data.frame(cbind(d, st_coordinates(d)))


# ------------------ Get Basemap for Catchments -------------
# Otter Catchment - Stamen Maps
bnds.Ott <- unname(st_bbox(OC_trans.buff))

OC.Stamen.base <- ggmap::get_stamenmap(bbox = bnds.Ott, zoom=12, maptype = 'terrain')

ggmap(OC.Stamen.base)

# Budleigh Brook Catchment Google Maps

BB_coord <- c(lon = st_coordinates(st_centroid(BB_trans.buff))[1], lat = st_coordinates(st_centroid(BB_trans.buff))[2])

BB.satmap.base <- ggmap::get_googlemap(center = BB_coord, zoom=14, size=c(640, 640), maptype = 'satellite')

ggmap(BB.satmap.base)

# Colaton Brook Catchment Google Maps

# CB_coord <- c(lon = st_coordinates(st_centroid(CB_trans.buff))[1], lat = st_coordinates(st_centroid(CB_trans.buff))[2])
# 
# CB.satmap.base <- ggmap::get_googlemap(center = CB_coord, zoom=13, size=c(640, 640), maptype = 'satellite')
# 
# ggmap(CB.satmap.base)

# --------------- Create sf data frames for plotting ------------- 

# get bounds as sf ...
Ott.bnds_sf <- ggmap.highlight(OC.Stamen.base)
BB.bnds_sf <- ggmap.highlight(BB.satmap.base)
# CB.bnds_sf <- ggmap.highlight(CB.satmap.base)

bnd.BB <- unname(st_bbox(BB.bnds_sf))
# bnd.CB <- unname(st_bbox(CB.bnds_sf))

# get highlighted catchment areas as sf ...
OC_mask <- ggmap.highlight(OC.Stamen.base, OC_trans)
BB_mask <- ggmap.highlight(BB.satmap.base, BB_trans) %>%
  mutate(area = 'Budleigh Brook Catchment')
# CB_mask <- ggmap.highlight(CB.satmap.base, CB_trans) %>%
#   mutate(area = 'Colaton Brook Catchment')

# Join GB Area and Otter Box
gb_otter_join <- GB.sf %>%
  dplyr::select(geometry)%>%
  rbind(select(Ott.bnds_sf, geometry)) %>%
  mutate(feature = c('Great Britain', 'Box')) %>%
  mutate(feature = factor(feature, levels = c('Great Britain', 'Box')))

# Join All OC mask and Bud Brook shape/box

All_plys_join <- OC_mask %>%
  dplyr::select(geometry)%>%
  rbind(dplyr::select(BB_trans, geometry),
        dplyr::select(CB_trans, geometry),
        dplyr::select(BB.bnds_sf, geometry)) %>%
  mutate(feature = c('R. Otter Catchment', 'Budleigh Brook Catchment', 'Colaton Brook Catchment (control)','BB.Box')) %>%
  mutate(feature = factor(feature, levels = c('R. Otter Catchment', 'Budleigh Brook Catchment', 
                                              'Colaton Brook Catchment (control)', 'BB.Box')))

# Get Beaver Dam Sequence segment.

sp1 <- st_point(c(305807, 85204))
sp2 <- st_point(c(306106, 85137))

split_points <- data.frame(place = c('upstream', 'downstream')) %>%
  mutate(geometry = st_sfc(sp1, sp2)) %>%
  st_as_sf()%>%
  st_set_crs(27700)%>%
  st_nearest_points(., st_transform(Otter_rivers, crs=27700)) %>%
  st_cast(., 'POINT') %>%
  st_as_sf() %>%
  rename(geometry=x) %>%
  mutate(point.n = rownames(.))%>%
  dplyr::filter(point.n != 1, point.n != 3) %>%
  select(geometry)%>%
  st_buffer(dist=1)%>%
  mutate(place = c('upstream', 'downstream'))%>%
  st_transform(crs = 4326)  

BB.rivers <- Otter_rivers %>%
  st_intersection(BB_trans)


BB.riv.split <- st_collection_extract(st_split(st_geometry(BB.rivers), st_geometry(split_points)),"LINESTRING") %>%
  st_as_sf()%>%
  mutate(Reaches = rownames(.)) %>%
  mutate(Beavs = 'Beaver Dam Complex')%>%
  filter(Reaches == 3)%>%
  rename(geometry=x)


# EA gauging stations as sf point.
BB.gauge <- st_point(c(306435, 84894))

gauge.sf <- data.frame(place = 'Hayes Lane Gauging Station') %>%
  mutate(geometry = st_sfc(BB.gauge)) %>%
  st_as_sf()%>%
  st_set_crs(27700)%>%
  st_transform(crs = 4326)

gauge.sf_lat_lon <- as_tibble(cbind(gauge.sf,st_coordinates(gauge.sf))) %>%
  mutate(place=as.character(place))

CB.gauge <- st_point(c(307219, 087669))
CBgauge.sf <- data.frame(place = 'Pophams Farm Gauging Station') %>%
  mutate(geometry = st_sfc(CB.gauge)) %>%
  st_as_sf()%>%
  st_set_crs(27700)%>%
  st_transform(crs = 4326)
CBgauge.sf_lat_lon <- as.data.frame(cbind(CBgauge.sf,st_coordinates(CBgauge.sf)))

# ------- Plotting ---------------------

# GB plot
plt1 <- gb_otter_join  %>%
  ggplot(aes(fill = feature, colour = feature)) +
  geom_sf(alpha = 0.8)+
  theme_bw() +
  scale_fill_manual(values = c('grey90', 'black'))+
  scale_colour_manual(values = c('grey40', 'black'))+
  guides(fill=FALSE, colour = FALSE)+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        rect = element_rect(fill = NA),
        panel.background = element_rect(fill = '#B3CCFC'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_blank())


#River Otter Plot
plt2 <- ggmap(OC.Stamen.base) +
  geom_sf(data = All_plys_join, aes(fill=feature),lwd=0.6, colour='grey30',inherit.aes = FALSE, alpha = 0.5)+
  geom_sf(data = Otter_rivers, mapping = aes(geometry = geometry, x = NULL, y= NULL, colour=river),inherit.aes = FALSE, 
          fill = NA)+
  scale_fill_manual(breaks = c('R. Otter Catchment', 'Budleigh Brook Catchment', 'Colaton Brook Catchment (control)'), 
                    values = c('grey60', '#F87B17', '#DD7AF5', NA, NA))+
  scale_colour_manual(values = c('#5CA0EC'))+
  theme_bw() +
  ggsn::north(location = 'bottomright', symbol = 15, scale =0.15,
              x.min = bnds.Ott[1], x.max = bnds.Ott[3]-0.015,
              y.min = bnds.Ott[2] + 0.012,  y.max = bnds.Ott[4]) +
  ggsn::scalebar(x.min = bnds.Ott[1], x.max = bnds.Ott[3]-0.015,
              y.min = bnds.Ott[2] + 0.01,  y.max = bnds.Ott[4],
              dist = 5, transform = TRUE, model = 'WGS84', dist_unit = "km", st.bottom=TRUE, st.size = 2.5)+
  annotation_custom(ggplotGrob(plt1), ymin = bnds.Ott[4]-0.1, ymax = bnds.Ott[4]  +0.0026,
                       xmin = bnds.Ott[1]-0.01, xmax = bnds.Ott[1]+ 0.09) +
  theme(panel.background = element_rect(fill = '#B3CCFC'),
        legend.position=c(.52,.87),
        legend.background=element_blank(),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.title=element_blank(),
        legend.text=element_text(size=7),
        legend.spacing.y = unit(-0.1, "cm"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())

plt2

ggsave("./7_Site_Location/exports/RivOtterLoc.jpg", plot = plt2, width = 15, height = 20, units = 'cm', dpi = 600)



# Budleigh Brook Plot

psat <-
  ggmap(BB.satmap.base) +
  geom_sf(data = BB.rivers, mapping = aes(geometry = geometry, x = NULL, y= NULL, colour = river), lwd = 0.5) +
  geom_sf(data = BB.riv.split, mapping = aes(geometry = geometry, x = NULL, y= NULL,colour=Beavs), lwd = 1.5) + 
  geom_sf(data= BB_mask, mapping = aes(geometry = geometry, fill=area, x = NULL, y= NULL),colour='grey10', alpha=0.5, inherit.aes = FALSE) +
  geom_point(data = gauge.sf_lat_lon, mapping = aes(x = X, y= Y, colour=place), size=1.5,stroke = 2,shape = 2) + 
  coord_sf(datum = st_crs(27700)) +
  scale_fill_manual(values = c("grey70")) +
  scale_colour_manual(breaks = c('Beaver Dam Complex', 'River Network', 'Hayes Lane Gauging Station'),
                      values = c('#900C3F', '#4396F3',  '#E2570D'),
                      guide = guide_legend(override.aes = list(
                        linetype = c('solid', 'solid', 'blank'),
                        shape = c(NA, NA, 2)))) +
  ggsn::scalebar(x.min = bnd.BB [1] , x.max = bnd.BB [3]- 0.002,
                 y.min = bnd.BB [2] + 0.002,  y.max = bnd.BB [4],
                 dist = 0.5, transform = TRUE, model = 'WGS84', dist_unit = "km", st.bottom=TRUE, st.size = 2.5)+
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank(),
        legend.position=c(.25,.15),
        legend.background=element_blank(),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.spacing.y = unit(-0.1, "cm"))
psat

# Combine the main plots

loc.plot <- ggplot()+
  annotation_custom(ggplotGrob(plt2), xmin = 0, xmax = 0.393, ymin = 0, ymax = 1)+
  annotation_custom(ggplotGrob(psat), xmin = 0.393, xmax = 1, ymin = 0, ymax = 1) +
  geom_segment(aes(x=0.085, y=0.135, xend=0.405, yend=0.5), linetype = 2) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme_void() +
  annotate("text", x = 0.91, y = 0, label = "© OpenStreetMap contributors", size=3)

# loc.plot
ggsave("./7_Site_Location/exports/BudBrookLoc.jpg", plot = loc.plot, width = 28, height = 17, units = 'cm', dpi = 600)


