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
  dplyr::mutate(popdensity = round(Pop / Area,2)) |> 
  dplyr::select(popdensity)

tm_shape(henan) + 
  tm_polygons(fill = "popdensity",fill.legend = tm_legend_hide()) +
  tm_text("popdensity",angle = 5,size = 0.75)

e1 = spEDM::embedded(ex,target = "ex", E = 3, tau = 1)
