# Fa solche do tre guere
df_ <- df

tot_sim = 12e3
# i = 1
partenza = i
# n_sim = 1
n_sim = 10
ogni_x_min = 10
probabilities = c(.3, .4, .5, .7, .9, .99)
results = tibble(.rows = n_sim)

# fa n guere
df_ <- df_bak
p = .4
for(s in 1:tot_sim){
  set.seed(i)
  wl <- batallia(adj, df_, i, p_daghe = p)
  df_ <- situassion_nova(df_, wl$wl)
  i <- wl$i
  i = i + 1
}




# Fame vedere un poche de simuassioni
for(p in probabilities){
  for(j in 1:(n_sim-1)){
    df_ <- df_bak
    left = df_ %>% count(paron) %>% NROW
    partenza = i
    while(left >= 7){
      set.seed(i)
      wl <- batallia(adj, df_, i, p_daghe = p)
      df_ <- situassion_nova(df_, wl$wl)
      # if(left - df_ %>% count(paron) %>% NROW > 0) 
      #   {
      #     print(paste(i, ":", left, df_ %>% count(paron) %>% NROW))
      # }
      left = df_ %>% count(paron) %>% NROW
      i = i + 1
    }
    results[j, as.character(p)] = (i-partenza)/(60*24)
    print(paste0(j, p, collapse = " : "))
  }
}
beepr::beep(2)
print((i-partenza)/(60/ogni_x_min*24))
wl <- batallia(adj, df_, i)


# Fa do tre guere e fa 'e mapete
tot_sim = 10e3
results <- list() 
maps <- list()

for(off in 3:10){
  print(Sys.time())
  print(paste("-- off to n°:", off))
  result = tibble(iter = 0, left = 571)
  veneto = veneto_
  offset =  off * tot_sim
  i = 0
  left = veneto %>% count(paron) %>% NROW
  
  for(i in i:(i+tot_sim)){
    set.seed(i + offset)
    wl <- batallia(adj, veneto, i)
    veneto <- situassion_nova(veneto, wl$wl)
    if(left > veneto %>% count(paron) %>% NROW) left = left - 1
    if(i %% 500 == 0) result <- result %>% rbind(c(i, left))
  }
  
  results[[off]] = result
  map <- ggmap(full_stam_map) +
    geom_polygon(data = veneto, 
                 aes(x = long, y = lat, group = group,
                     fill = paron
                 ),
                 color = "slategray4",
                 size = .3,
                 alpha = .90) + 
    scale_fill_manual(values = as.character(f_palette)) + guides(fill = FALSE, color = FALSE)
  maps[[off]] = map
}
beep()



# Map results -----
for(i in 1:length(maps)){
  filename = paste0("img/sim/", i ,".bmp")
  bmp(filename,
      width=6, height=6, 
      units="in", res=100)
  print(maps[[i]])
  dev.off()
  }