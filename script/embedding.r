library(sf)
library(tmap)

# henan = cnmap::getMap(code = "410000", subRegion = TRUE) |> 
#   dplyr::select(adcode,name)

henan = mapchina::china |> 
  dplyr::filter(Code_Province == "41") |> 
  dplyr::select(Code = Code_Perfecture,Pop = Pop_2010,Area) |> 
  dplyr::group_by(Code) |> 
  dplyr::summarise(geometry = sf::st_union(geometry) |> 
                       sf::st_cast("MULTIPOLYGON"),
                   Pop = sum(Pop,na.rm = TRUE),
                   Area = sum(Area,na.rm = TRUE)) |> 
  dplyr::mutate(popdensity = round(Pop / Area,0)) |> 
  dplyr::select(popdensity)

map1 = tm_shape(henan) + 
  tm_polygons(fill = "popdensity",fill.legend = tm_legend_hide()) +
  tm_text("popdensity",size = 0.75, # angle = 5,
          options = opt_tm_text(just = "top",on_surface = TRUE))
tmap_save(map1,'./figure/map1.jpg',dpi = 300)

embeddings = spEDM::embedded(henan,target = "popdensity", E = 3, tau = 1)
