library(sf)
library(tmap)

# henan = cnmap::getMap(code = "410000", subRegion = TRUE) |> 
#   dplyr::select(adcode,name)

cn = dplyr::select(st_make_valid(mapchina::china),
                   Code = Code_Perfecture,
                   Pop = Pop_2010,Area) |> 
  dplyr::group_by(Code) |> 
  dplyr::summarise(geometry = sf::st_union(geometry) |> 
                     sf::st_cast("MULTIPOLYGON"),
                   Pop = sum(Pop,na.rm = TRUE),
                   Area = sum(Area,na.rm = TRUE)) |> 
  dplyr::mutate(popdensity = round(Pop / Area,0))
henan = cn |> 
  dplyr::filter(stringr::str_detect(Code, "^41.{2}$")) |> 
  dplyr::select(Code,popdensity) |> 
  tibble::rowid_to_column(var = "rid")

bb = henan |> 
  st_bbox() |>
  st_as_sfc() |>
  st_as_sf() |>
  st_buffer(dist = units::set_units(0.35,"degree")) |> 
  st_bbox() |> 
  as.numeric()

map1 = tm_shape(henan) + 
  tm_polygons(fill = "popdensity",fill.legend = tm_legend_hide()) +
  tm_text("popdensity",size = 0.75, # angle = 5,
          options = opt_tm_text(just = "top",on_surface = TRUE)) +
  tm_layout(frame = FALSE)
tmap_save(map1,'./figure/figure1_1.jpg',dpi = 300)

nb = sdsfun::spdep_nb(henan)

jpeg("./figure/figure1_2.jpg", width = 1500, height = 1500, res = 300)  
par(mar = rep(0,4))
plot(sf::st_geometry(henan), col = 'white', lwd = 1.25, border = "grey40")
plot(nb,coords = sdsfun::sf_coordinates(henan), lwd=1.05, col="blue", cex = 1.25, add = TRUE)
dev.off()

embeddings = spEDM::embedded(henan, target = "popdensity", E = 3, tau = 1)
# colnames(embeddings) = c("hs1","hs2","hs3")
# readr::write_csv(as.data.frame(embeddings),'./result/figure1_embeddings.csv')
embeddings[10,1:3]

jpeg("./figure/figure1_3.jpg", width = 1800, height = 1500, res = 300)
par(mar = rep(0,4))
scatterplot3d::scatterplot3d(x = embeddings[,1], y = embeddings[,2], z = embeddings[,3],
                             xlab = latex2exp::TeX("$h_{s(1)}(x)$"),
                             ylab = latex2exp::TeX("$h_{s(2)}(x)$"),
                             zlab = latex2exp::TeX("$h_{s(3)}(x)$"),
                             pch = 16, color="red", angle = 45)
dev.off()


# Selected spatial unit for illustrative calculation
mapview::mapview(henan,zcol = "Code")
henan[henan$Code == "4110",]

spunit = list()
spunit[[1]] = nb[[10]]
spunit[[2]] = setdiff(spEDM:::RcppLaggedNeighbor4Lattice(nb,2)[[10]],c(nb[[10]],10))
spunit[[3]] = setdiff(spEDM:::RcppLaggedNeighbor4Lattice(nb,3)[[10]],
                      spEDM:::RcppLaggedNeighbor4Lattice(nb,2)[[10]])

henan = henan |> 
  dplyr::mutate(
    lagnum = dplyr::case_when(rid == 10 ~ "0",
                              rid %in% spunit[[1]] ~ "1",
                              rid %in% spunit[[2]] ~ "2",
                              rid %in% spunit[[3]] ~ "3")
  ) |> 
  dplyr::mutate(lagnum = factor(lagnum,levels = as.character(0:3)))


map2 = tm_shape(cn, bbox = bb) + 
  tm_polygons(col = "grey50", fill = "white", lwd = 1.05, fill_alpha = 0.5) +
  tm_shape(henan) + 
  tm_polygons(fill = "lagnum",
              fill.scale = tm_scale_categorical(n.max = 4,
                                                values = c("#fc4e2a",
                                                           "#fd8d3c",
                                                           "#fed976",
                                                           "#ffeda0"),
                                                value.na = "white"),
              col = 'grey', lwd = 1.25) +
  tm_text("popdensity",size = 0.75, # angle = 5,
          options = opt_tm_text(just = "top",on_surface = TRUE))
tmap_save(map2,'./figure/map2.jpg',dpi = 300)