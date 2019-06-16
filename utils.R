lp = .libPaths()[1]
library(dplyr, lib.loc = lp) 
library(httr, lib.loc = lp) 
library(sf, lib.loc = lp) 
library(glue, lib.loc = lp) 
# library(spdep, lib.loc = lp) 
library(ggmap, lib.loc = lp)
library(ggspatial, lib.loc = lp) 
library(ggthemes, lib.loc = lp) 
library(ggrepel, lib.loc = lp) 
library(ggplot2, lib.loc = lp) 

standard = 3857

httr::set_config(httr::config(http_version = 0))

# Get Stamen map
focused_stam_map <- function(limits, mode = "toner-background", zoom = 7){
  selection_borders <- c(bottom  = limits$l_lat,
                         top     = limits$h_lat,
                         left    = limits$l_lon,
                         right   = limits$h_lon)
  stam_map <- get_stamenmap(selection_borders, zoom = zoom, maptype = mode)
  return(map)
}

# provincia -----
dame_provincia <- function(municipality, mode="sigla"){
  cd <- veneto %>% distinct(comune_cd, COD_PROV) %>% filter(comune_cd == municipality) %>% getElement("COD_PROV")
  if(mode != "numero"){
    return(province[as.character(cd)])
  }
  return(as.character(cd))
}
dame_provincia <- Vectorize(dame_provincia, "municipality")

# Trova fora do canallie
batallia <- function(adj, veneto, i, p_daghe = .05){
  posibie = NULL
  carico = FALSE
  
  while(TRUE){
    candidato <- sample(length(adj), 1)
    paron_cand <- veneto %>% filter(comune_cd %in% candidato) %>% 
      distinct(paron_cd) %>% getElement("paron_cd")
    
    while(is.null(posibie)){
      posibie <- adj[[candidato]]
    }
    
    puito = veneto %>% filter(comune_cd %in% posibie, paron_cd != paron_cand) %>% 
      distinct(comune_cd) 
    
    if(puito %>% NROW > 0){
      bataliabili <- puito %>% getElement("comune_cd")
      difende <- sample(length(bataliabili), 1)
      taca = candidato
      break
    }
    # qua ghe rivemo solche se 'a prima volta no 'ndava ben
    i = i + 1
    set.seed(i)
    candidato <- sample(length(adj), 1)
    posibie = NULL
  }
  eleto <- sample(1:2, 2)
  wl <- c(taca, bataliabili[difende])[eleto]
  vitima <- wl[2]
  tacai <- setdiff(adj[[vitima]], wl[1])
  paron_cand <- veneto %>% filter(comune_cd %in% wl[1]) %>% 
    distinct(paron_cd) %>% getElement("paron_cd")
  i_scampa <- veneto %>% 
    filter(comune_cd %in% tacai, !(paron_cd %in% paron_cand)) %>% 
    distinct(comune_cd) %>% getElement("comune_cd")
  
  while(length(i_scampa) && !carico){
    daghe <- rbinom(n = 1, size = 1, prob = p_daghe)
    if(daghe == 0){
      break
    } 
    n <- length(i_scampa)
    # al masimo chi che vinse ghen ciapa 6 in totae
    quanti <- min(ceiling(runif(1)**2.11*(n)), 5)
    # quanti <- ceiling(runif(1)**2.11*(n))
    
    if(n == 1) n_altra = i_scampa
    else n_altra = sample(i_scampa, quanti)
    vitima <- append(vitima, n_altra)
    carico = TRUE
  }
  
  # Sitadea no poe perdare co Fontaniva
  if(wl[1] == 549 && !carico && vitima == 543)
  {
    wl[1] = 543
    vitima = 549
  }
  # San Piero no poe perdare co Coneian
  if(wl[1] == 395 && !carico && vitima == 447)
  {
    wl[1] = 447
    vitima = 395
  }
  
  return(
    list(wl = as.integer(c(wl[1], vitima)),
         carico = carico, i = i)
  )
}

# Crea 'l novo Veneto
situassion_nova <- function(veneto, wl){
  wl[1] = veneto %>% distinct(paron_cd, comune_cd) %>% filter(comune_cd == wl[1]) %>% getElement("paron_cd")
  wl_n = com_cd$comune[wl] # need to do this in order to maintain the sorting in wl
  hex_to_input = veneto %>% filter(paron_cd == wl[1]) %>% distinct(coeore) %>% getElement("coeore")
  out = veneto %>% 
    mutate(paron = if_else(comune_cd %in% wl[-1], wl_n[1], paron))%>%
    mutate(paron_cd = if_else(comune_cd %in% wl[-1], wl[1], paron_cd)) %>% 
    mutate(coeore = if_else(comune_cd %in% wl[-1], hex_to_input, coeore))
  
  return(out)
}

# Fate metare 'na fotina del veneto in te un fail
dame_na_foto <- function(map, filename="img/sim/prova.bmp", size=8){
  bmp(filename,
      width=size, height=size, 
      units="in", res=100)
  print(map)
  dev.off()
}

# squadrata i limiti
damei_quadrati <- function(l){
  diff_lat = l$h_lat - l$l_lat
  diff_lon = l$h_lon - l$l_lon
  zonta = diff_lat - diff_lon
  if(zonta < 0){
    l$h_lat = l$h_lat + abs(zonta)/2
    l$l_lat = l$l_lat - abs(zonta)/2
  }
  else{
    l$h_lon = l$h_lon + abs(zonta)/2
    l$l_lon = l$l_lon - abs(zonta)/2
  }
  return(l)
}

# Te dae i poigoni coi bordini par dentro
dame_bordini <- function(x, dist=-670, crs=standard){
  x <- x %>% st_transform(crs=3857)
  buf <- st_cast(st_buffer(x, dist = dist))
  buf_comb <- buf %>% st_combine() %>% st_sf()
  buf_comb  <- st_buffer(buf_comb , dist = 0)
  doug <- st_difference(x, buf_comb) %>% st_cast() %>% st_transform(crs=crs)
  
  return(doug)
  
}

# na roba che serve pa scrivere i nomi dei paeseti che se lexe qualcossa al manco tiocà
meio_posissioni <- function(sf_ven, paroni){
  temp <- sf_ven %>% 
    filter(paron_cd %in% paroni)
  cnames <- temp %>% sfc_as_cols
  return(cnames)
}

sfc_as_cols <- function(x, names = c("c_long", "c_lat")) {
  ret <- try(sf::st_coordinates(sf::st_point_on_surface(x)))
  
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret, names)
  dplyr::bind_cols(x, ret)
}

# Crea el novo poigono coi paroni iusti
meti_insieme <- function(veneto, veneto_sf, crs = standard){
  veneto_sf <- veneto_sf %>% st_transform(crs = crs)
  mgd <- veneto_sf %>% full_join(veneto %>% distinct(paron_cd, paron, comune_cd, coeore), 
                                 by = c("comune_cd"))
  
  # print(colnames(mgd))
  out <- mgd %>% 
    group_by(paron_cd, paron, coeore) %>% 
    summarise(geometry = st_union(geometry)) %>% 
    ungroup %>% 
    st_transform(crs = crs)
  
  return(out)
}

dame_comuni_par_longo <- function(vinti, p, df){
  return(paste0(df %>%
                  distinct(paron_cd, paron, comune_cd, comune) %>%
                  filter(comune_cd %in% vinti, paron_cd %in% p) %>% 
                  getElement("comune"), collapse = ", "))
}

# scrite bee pai batui come mesajo

batui <- function(vinti, paroni_dei_vinti, df){
  out = ""
  lp = length(paroni_dei_vinti)
  if(lp > 1) {
    for(p in paroni_dei_vinti[-length(paroni_dei_vinti)]){
      b = dame_comuni_par_longo(vinti, p, df)
      tra_parentesi = ifelse(b != com_cd$comune[p], com_cd$comune[p], "👑")
      out = paste0(out, b, " (", tra_parentesi, "), ")
    }
    out = paste0(out, "e ")
  }
  ultimo = paroni_dei_vinti[length(paroni_dei_vinti)]
  b = dame_comuni_par_longo(vinti, 
                            ultimo,
                            df)
  tra_parentesi = ifelse(b != com_cd$comune[ultimo], com_cd$comune[ultimo], "👑")
  out = paste0(out, b, " (", tra_parentesi, ")")
  return(out)
} 

sparii <- function(prima, dopo){
  out = ""
  paroni_prima <- prima %>% distinct(paron) %>% getElement("paron")
  paroni_dopo <- dopo %>% distinct(paron) %>% getElement("paron")
  morti = setdiff(paroni_prima, paroni_dopo)
  
  if(length(morti) > 0){
    if(length(morti) > 1) {
      out = paste0(morti[-length(morti)], collapse = ", ")
      out = paste0(out, " e ")
    }
    out = paste0(out, morti[length(morti)])
  }
  return(out)
}


# dame dei bei verbi pae frasete
verbo_vinsere <- function(){
  lista = c("se ciava",
            "conquista",
            "se guadagna",
            "ciapa su",
            "cata",
            "sbarca de prepotensa su",
            "sgrafiña",
            "pètena"
  )
  set.seed(Sys.time())
  return(sample(lista, 1))
}


verbo_sparire <- function(){
  lista = c("Ne assa par sempre",
            "Sparisse par sempre",
            "No vedaren pi'",
            "Scampa par sempre",
            "Se arende",
            "Buta 'a spugna meritevolmente",
            "No xe sta' bon de scrivere 'l verbae",
            "Fora dai xoghi")
  set.seed(Sys.time())
  return(sample(lista, 1))
}

interiessioni <- function(){
  lista = c("Sacranon che disastro!",
            "Maria Vergine che afari!",
            "Dio da Dio, Luce da Luce!",
            "'na roba che gnanca 'e guere puniche!",
            "Can dal porco!",
            "Canallia!",
            "Come che i cresse in freta...",
            "Deve 'na calmada fioi!")
  set.seed(Sys.time())
  return(sample(lista, 1))
}

# data pi' bea
numaro_mese <- c("Genaio",
                 "Febraio",
                 "Marso",
                 "Aprïe",
                 "Majo",
                 "Xuño",
                 "Lullio",
                 "'gosto", 
                 "Setenbre", 
                 "Otobre", 
                 "Novenbre", 
                 "Dicenbre")

bea_data <- function(d){
  s = stringr::str_extract(d, "-[0-9]{2}-")
  numaro = as.integer(gsub("-", "", s))
  d_novo = gsub(s, paste0(" de ", numaro_mese[numaro], ", "), d)
  d_novo = gsub("^0", "", d_novo)
}


agiornamenti_scrita <- function(ag){
  out = "I mejo fin deso:\n"
  for(r in 1:NROW(ag)){
    t = paste0(ag[r,], collapse = " : ")
    out = glue("{out}\n{t}")
  }
  return(out)
}



