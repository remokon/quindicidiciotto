source("utils.R")
load("current.RData")

agiornamenti = df_ %>% distinct(paron, comune) %>% group_by(paron) %>% summarise(n = n()) %>% arrange(desc(n)) %>% filter(n > 1) %>% head(7)
scrita = agiornamenti_scrita(agiornamenti)
i_mejo = df_ %>% distinct(paron_cd, comune) %>% group_by(paron_cd) %>% summarise(n = n()) %>% arrange(desc(n)) %>% filter(n > 1) %>% head(7) %>% getElement("paron_cd")

limiti_grandi = damei_quadrati(limits_tot)
dimensioni_mapona <- limiti_grandi$h_lat - limiti_grandi$l_lat

cnames <- meio_posissioni(sf_ven, paroni = i_mejo)

go <- ggplot() +
  # annotation_map_tile(type = "stamenwatercolor", zoom = 10) +
  geom_sf(data = sf_ven, mapping = aes(fill = coeore), alpha = .10, inherit.aes = FALSE) +
  # geom_sf(data = sf_ven, fill = "#fafafa", inherit.aes = FALSE) +
  geom_sf(data = sf_ven %>% filter(paron_cd %in% i_mejo), mapping = aes(fill = coeore)) +
  geom_sf(data = ven_doug %>% filter(paron_cd %in% i_mejo), fill = "#000000", colour = "transparent", inherit.aes = FALSE) +
  geom_sf(data = sf_ven_, fill = "transparent",  color = "#717171") +
  geom_label_repel(data = cnames, mapping = aes(label = paron, y = c_lat, x = c_long),
                   segment.colour = "antiquewhite",
                   fill = "#4B4B5B",
                   color = "antiquewhite",
                   box.padding = 0.5,
                   segment.size = 1.2,
                   arrow = arrow(length = unit(0.015, "npc"), ends = "last", type = "closed"),
                   family = "mono",
                   size = 6,
                   fontface = "bold",
                   force = 10,
                   alpha = .85,
                   seed = 1991) +
  scale_fill_identity() + guides(color = FALSE) +
  coord_sf(xlim = c(limiti_grandi$l_lon, limiti_grandi$h_lon),  
           ylim = c(limiti_grandi$l_lat, limiti_grandi$h_lat), datum=NA) +
  ggmap::theme_nothing() + 
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.text = NULL,
        panel.background = element_rect(fill="#F5FFFA")) +
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering) + 
  annotation_raster(prof_pic,
                    xmin = limiti_grandi$l_lon - dimensioni_mapona * .03, xmax = limiti_grandi$l_lon + dimensioni_mapona * .05,
                    ymin = limiti_grandi$l_lat - dimensioni_mapona * .03, ymax = limiti_grandi$l_lat + dimensioni_mapona * .05
)

ggsave("img/globae_orario_11.png", plot = go, width=10, height=10,
       units="in", dpi=200)

rg = POST(url = glue("https://graph.facebook.com/v3.1/{page_id}/photos"), body = list(access_token = TOKEN, caption = scrita, filedata = upload_file("img/globae_orario_11.png")))
