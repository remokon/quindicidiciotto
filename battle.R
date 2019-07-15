prof_pic = png::readPNG("~/quindicidiciotto/img/prof_pic.png")
coord_fctr = 3e4
m     = .2*coord_fctr
ratio = 1.55
b_force = 150
perc = .8
others_alpha = .15
full_alpha = 1
p_d = .49
grandessa_testo = 8
grandessa_emoji = 8
  
set.seed(i)
wl <- batallia(adj, df_, i, p_daghe = p_d)
i = wl$i
vinti <- wl$wl[-1]
paroni_dei_vinti <- df_ %>% distinct(comune_cd, paron_cd) %>% filter(comune_cd %in% vinti) %>% getElement("paron_cd") %>% unique

df_dopo <- situassion_nova(df_, wl$wl)
sf_ven_dopo <- meti_insieme(df_dopo, sf_ven_)
sf_ven_rossi <- meti_insieme(df_ %>% filter(comune_cd %in% vinti), sf_ven_ %>% filter(comune_cd %in% vinti))
ven_doug_dopo <- dame_bordini(sf_ven_dopo)
ven_doug_rossi <- dame_bordini(sf_ven_rossi)


v_force = b_force + 10*(length(unique(paroni_dei_vinti))- 1)
biancheti_df <- df_ %>% distinct(paron_cd, comune_cd) %>% filter(paron_cd %in% paroni_dei_vinti, !(comune_cd %in% vinti)) %>% group_by(paron_cd) %>% summarise(n = n(), tuti = list(comune_cd)) %>% filter(n > 0)
biancheti <- biancheti_df %>% getElement("paron_cd")
biancheti_com <- biancheti_df %>% getElement("tuti") %>% unlist

el_paron <- df_ %>% filter(comune_cd %in% wl$wl[1]) %>% getElement("paron_cd") %>% unique
ex_paron <- df_ %>% filter(comune_cd %in% wl$wl[-1]) %>% getElement("paron_cd") %>% unique

cnames <- meio_posissioni(sf_ven, paroni = c(el_paron, ex_paron))
cnames_swords <- sf_ven_ %>% sfc_as_cols %>% inner_join(df_ %>% distinct(comune_cd, paron_cd), by = "comune_cd")
lwd = 2

limits_tot <- df_ %>% summarise(l_lat = min(lat) - m, l_lon = min(long), h_lat = max(lat), h_lon = max(long))
limits <- df_ %>% filter(paron_cd %in% c(el_paron, ex_paron)) %>% summarise(
  l_lat = max(min(lat) - m, limits_tot$l_lon),
  l_lon = max(min(long) - m*ratio, limits_tot$l_lon),
  h_lat = min(max(lat) + m, limits_tot$h_lat),
  h_lon = min(max(long) + m*ratio, limits_tot$h_lon)
)
limits <- damei_quadrati(limits)
dimensioni_mapeta <- limits$h_lat - limits$l_lat
emoji = ifelse(length(wl$wl) > 2, "\U001F525", "")

base <- ggplot()  +
  geom_sf(data = sf_ven, fill = "#fafafa", inherit.aes = FALSE) +
  geom_sf(data = sf_ven, mapping = aes(fill = coeore),
          alpha = others_alpha) +
  geom_sf(data = sf_ven %>% filter(paron_cd %in% c(el_paron, ex_paron)) , mapping = aes(fill = coeore)) +
  geom_polygon(data = df_,  mapping = aes(x = long, y = lat, group = group), fill = "transparent",  color = "#717171") +
  geom_sf(data = ven_doug %>% filter(paron_cd %in% el_paron), fill = "#11FF22",
          alpha = full_alpha, colour = "transparent") +
  geom_sf(data = ven_doug_dopo %>% filter(paron_cd %in% biancheti), fill = "#FBFBFB",
          alpha = full_alpha, colour = "transparent") +
  geom_sf(data = sf_ven, fill = "transparent",
          color = "#515151")
if(length(vinti)) base <- base +
  geom_sf(data = ven_doug_rossi,
          fill = "#FF2211",
          alpha = full_alpha, colour = "transparent")

g <- base +
  geom_text(data = cnames_swords %>% filter(comune_cd %in% wl$wl[-1]) %>%
              mutate(symbol = ifelse(paron_cd %in% biancheti, "\U001F69C", "\U001F480")),
            aes(c_long, c_lat, label = symbol), size=grandessa_emoji, fontface = "bold",
            color =  "slategray3") +
  geom_point() +
  geom_label_repel(data = cnames %>% filter(paron_cd %in% c(el_paron, ex_paron)) %>% group_by(paron_cd) %>% summarise(
    paron = ifelse(unique(paron_cd) == el_paron, paste0(emoji, unique(paron)), unique(paron)), long = mean(c_long), name_lat = mean(c_lat),
    label_fill = ifelse(unique(paron_cd) == el_paron, "#00924b", ifelse(unique(paron_cd) %in% biancheti, "#2E4053", "#AA1111"))
  ), aes(long, name_lat,
         fill = label_fill,
         label = paron
  ),
  segment.colour = "antiquewhite",
  color = "antiquewhite",
  box.padding = 0.5,
  segment.size = 1.2,
  arrow = arrow(length = unit(0.015, "npc"), ends = "last", type = "closed"),
  family = "mono",
  size = grandessa_testo,
  fontface = "bold",
  alpha = .92,
  seed = 2000,
  force = v_force,
  label.size = .9,
  label.padding = .5) +
  scale_fill_identity() + guides(fill = FALSE, color = FALSE) +
  coord_sf(xlim = c(limits$l_lon, limits$h_lon),  ylim = c(limits$l_lat, limits$h_lat), datum=NA) +
  ggmap::theme_nothing() + theme(panel.grid.major = element_line(colour = 'transparent')) +
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_raster(prof_pic,
                    xmin = limits$l_lon - dimensioni_mapeta * .03, xmax = limits$l_lon + dimensioni_mapeta * .05,
                    ymin = limits$l_lat - dimensioni_mapeta * .03, ymax = limits$l_lat + dimensioni_mapeta * .05
                    )

# tempo = substring(as.character(Sys.time()), first = 1, last = 19)
# filename = gsub("\\s{1,}|[-]|[:]", "_", paste0("img/", i, "_", tempo,".png"))
filename = "~/quindicidiciotto/img/ultima.png"
ggsave(filename, plot = g,  width=10, height=10,
       units="in", dpi=200)

impeto = ifelse(wl$carico, "\U001F692 #impetodeguera \U001F692:\n\n", "")
set.seed(Sys.time())
vv = verbo_vinsere()
set.seed(Sys.time())
ss = verbo_sparire()

batui_scrita = batui(vinti, paroni_dei_vinti, df_)
rimasti = df_dopo %>% distinct(paron_cd) %>% NROW

morti = (df_ %>% distinct(paron_cd) %>% NROW) - rimasti
sp = sparii(df_, df_dopo)
sparii_scrita = ifelse(morti == 0, "", glue(" {ss}: {sp}. Ghen resta {rimasti}"))
vero_vincitore = df_ %>% filter(comune_cd %in% wl$wl[1]) %>% distinct(paron) %>% getElement("paron") %>% asctag
cossa_se_cata = ifelse(length(vinti) > 1, "i teritori", "'l teritorio")
canallia = ifelse(length(vinti) < 4, "", interiessioni())
canallia = ifelse(length(vinti) < 4, "", interiessioni())
scrita_temp = glue("{impeto}{vero_vincitore} {vv} {cossa_se_cata} de {batui_scrita}. {canallia}{sparii_scrita}")
scrita_temp = gsub("\\s{2,10}", " ", scrita_temp)
bd = bea_data(format(sta_data, "%d-%m-%Y"))
scrita_finae = glue("{bd} \n\n{scrita_temp}")

fc <- file("~/quindicidiciotto/img/ultima.txt")
writeLines(scrita_finae, fc)
close(fc)

df_ <- df_dopo
sf_ven <- sf_ven_dopo
ven_doug <- ven_doug_dopo


sta_data = sta_data + 1; i = i + 1
rm(base, g, df_dopo, sf_ven_dopo, ven_doug_dopo)
  
