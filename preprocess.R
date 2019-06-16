library(rgdal)
library(rgeos)
library(tidyverse)

standard = 3857

# Read shp Italian data ---------------------------------------------------
shapefile <- readOGR("data/Com01012019_g_WGS84.shp", "Com01012019_g_WGS84")
shpll = sp::spTransform(shapefile, "+init=epsg:3857")

# Selessiona 'l veneto
shp_ven <- subset(shpll, COD_REG == "5")
# shp_ven$COMUNE <- as.character(shp_ven$COMUNE)
# shp_ven$COMUNE[shp_ven$COMUNE == "Negrar"] <- "Negrar di Valpolicella"

# Merge the shapefile with its fortified version cussì vien puito
shp_mgd <- merge(fortify(shp_ven), as.data.frame(shp_ven), by.x="id", by.y=0)
df <- shp_mgd %>% as.tbl
rm(shapefile, shpll)

# Create veneto dataframe ------------

# quei che sta vissini
# adj_prova = spdep::poly2nb(sf_ven_)

# comuni table
lista_comuni <- as.character(shp_ven$COMUNE)
com_cd = tibble(comune = lista_comuni, comune_cd = 1:length(lista_comuni))
# df_ <- df
df <- df %>% rename(comune = COMUNE)
df <- df %>% full_join(com_cd, by = "comune" )
df <- df %>% mutate(paron = as.character(comune),
                            paron_cd = as.integer(comune_cd))
# palette preparation
municipalities <- df %>% count(comune_cd) %>% NROW
nice_colors = read_csv("data/bunch_of_colors.csv", col_names = FALSE) %>% distinct %>% getElement("X1")
set.seed(0)
palette = sample(colorRampPalette(nice_colors)(municipalities + 10))
f_palette = factor(palette, levels = palette)

df <- df %>% mutate(coeore = f_palette[paron_cd])

# crea grupi novi ----
# sf_ven <- shp_ven %>% as("sf") 
sf_ven_proj <- shp_ven %>% as("sf") %>% st_transform(crs=standard)
sf_ven_ <- sf_ven_proj %>% rename(comune = COMUNE) %>% full_join(com_cd, by = "comune" )
# sf_ven_ <- sf_ven_ %>% mutate(paron = as.character(comune),
                              # paron_cd = as.integer(comune_cd))
# sf_ven_ <- sf_ven_ %>% mutate(coeore = f_palette[paron_cd])

sf_ven <- meti_insieme(df, sf_ven_)

ven_doug <- dame_bordini(sf_ven_) %>% st_transform(crs=standard) %>% 
  mutate(paron = as.character(comune),
         paron_cd = as.integer(comune_cd)) %>%
  mutate(coeore = f_palette[paron_cd])

ven_doug_single <- dame_bordini(sf_ven_, dist=-685) %>% st_transform(crs=standard) %>%
  mutate(paron = as.character(comune),
         paron_cd = as.integer(comune_cd)) %>%
  mutate(coeore = f_palette[paron_cd])

df_ <- df

sta_data = as.Date("1518-08-10")

page_id = "336322509867777"
TOKEN = "EAAgMbXYmGxQBALF1dQZBMbZAduhribLBLOfwkIoO1KFFJMr0wndSQtiMF38C2xi1NHrreOg2TEFDUS32vX8RRtdLkGsB00RVDAdPc9vb6rHnQPmPdLyMCS3rtG1lqQsRZAEOPsTS8MpBPbtryPDRnSBzZAfPq77db1v9AcDUZC7KsagrEZCZA4r1NsdEfsJqZBUZD"

i = 24051991
save(list = c("adj", "sta_data", "page_id", "TOKEN", "df_", "f_palette", "ven_doug", "ven_doug_single", "sf_ven", "sf_ven_", "com_cd", "i"), file = "start.RData")


#### initial map
dimensioni_mapeta <- limits_tot$h_lat - limits_tot$l_lat
base <- ggplot()  +
  geom_sf(data = sf_ven, mapping = aes(fill = coeore),
          alpha = .9) +
  scale_fill_identity() + guides(fill = FALSE, color = FALSE) +
  ggmap::theme_nothing() + theme(panel.grid.major = element_line(colour = 'transparent'), panel.background = element_rect(fill = "#F5FFFA")) +
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_raster(prof_pic,
                    xmin = limits_tot$l_lon - dimensioni_mapeta * .03, xmax = limits_tot$l_lon + dimensioni_mapeta * .05,
                    ymin = limits_tot$l_lat - dimensioni_mapeta * .03, ymax = limits_tot$l_lat + dimensioni_mapeta * .05
  )
base
