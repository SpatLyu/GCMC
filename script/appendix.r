#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~     Discussing the sensitivity of GCMC to noice     ~~~~~~~~~~#
#~~~~~~~~~~~~~~         Author: Wenbo Lv; Date: 2025-03-27          ~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(spEDM)

popd_nb = spdep::read.gal(system.file("case/popd_nb.gal",package = "spEDM"))
popd = readr::read_csv(system.file("case/popd.csv",package = "spEDM"))

#------------------------------------------------------------------------------#
#------------------    Run GCMC on the synthetic dataset    -------------------#
#------------------------------------------------------------------------------#

set.seed(2025)
eta = rnorm(nrow(popd),0,1)
a = seq(0,1,by = 0.1)

s_popd = a |> 
  purrr::map_dfc(\(.a) (1+.a*eta)*popd$popd) |> 
  purrr::set_names(paste0("p","_",a))
popd_sf = popd |> 
  dplyr::select(elev,x,y) |> 
  dplyr::bind_cols(s_popd) |> 
  sf::st_as_sf(coords = c("x","y"), crs = 4326)
popd_sf

res = data.frame()
for (i in seq_along(a)) {
  g = gcmc(data = popd_sf, cause = "elev", effect = paste0("p","_",a[i]),
           E = c(1,5), k = 210, nb = popd_nb, trend.rm = TRUE)
  
  tempdf = g$xmap |> 
    dplyr::select(x_xmap_y_mean,x_xmap_y_sig,
                  y_xmap_x_mean,y_xmap_x_sig) |> 
    dplyr::mutate(x = "elev", y = "popd",eta = a[i])
  res = rbind(res,tempdf)
}
res
readr::write_csv(res,'./result/appendix.csv')

#------------------------------------------------------------------------------#
#----------------------------    Plot the result    ---------------------------#
#------------------------------------------------------------------------------#

noise_levels = paste0(a * 100, "% noise")
noise_levels[1] = "no noise"

res = readr::read_csv('./result/appendix.csv') |> 
  dplyr::select(eta,
                `popd -> elev` = x_xmap_y_mean,
                `elev -> popd` = y_xmap_x_mean) |> 
  dplyr::mutate(eta = noise_levels) |> 
  tidyr::pivot_longer(cols = 2:3, names_to = "direction",values_to = "cs") |> 
  tibble::rowid_to_column("id") |> 
  tidyr::pivot_wider(id_cols = direction,
                     names_from = eta,
                     values_from = cs)
res
# readr::write_csv(res,'./result/figure6.csv')

fig6 = ggradar::ggradar(res,
                 label.gridline.min = F,
                 label.gridline.mid = F,
                 label.gridline.max = F,
                 group.line.width = 0.75,
                 group.point.size = 2.05,
                 legend.position = "bottom") +
  ggview::canvas(6.65,5.85)

ggview::save_ggplot(fig6,"./figure/figure6_meta.jpg", dpi = 300)