source('./script/.internal_funs.r')

tibetbio = readr::read_csv('./data/tibet_bio.csv') |> 
  sf::st_as_sf(coords = c("x","y"), crs = 4326)

k = floor(nrow(tibetbio) / 4)

# bio_res = spEDM::gcmc(data = tibetbio,
#                       cause = "ndvi",
#                       effect = "bio",
#                       E = 3,
#                       k = k,
#                       r = 0,
#                       trend.rm = FALSE)

# g = spEDM::gccm(data = tibetbio,
#                 cause = "ndvi",
#                 effect = "bio",
#                 libsizes = seq(100,1600,100),
#                 E = 3)

bio_gcmc = gcmc4lattice(tibetbio,E = 3,k = k,r = 0,trend.rm = FALSE)
readr::write_csv(bio_gcmc,'./result/bio_gcmc.csv')

bio_gccm = gccm4lattice(tibetbio,libsizes = seq(100,1600,100),E = 3,k = 4,trend.rm = TRUE)
readr::write_csv(bio_gccm,'./result/bio_gccm.csv')

bio_gcmc = readr::read_csv('./result/bio_gcmc.csv') 
bg1 = bio_gcmc |> 
  dplyr::select(x,y,y_xmap_x_mean,y_xmap_x_sig)|> 
  purrr::set_names(c("cause","effect","cs","sig"))
bg2 = bio_gcmc |> 
  dplyr::select(y,x,x_xmap_y_mean,x_xmap_y_sig) |> 
  purrr::set_names(c("cause","effect","cs","sig"))
bio_gcmc = rbind(bg1,bg2) |> 
  dplyr::mutate(sig = dplyr::if_else(sig < 0.05,"T","F")) |> 
  dplyr::mutate(sig = factor(sig,levels = c("T","F")))

ggplot2::ggplot(bio_gcmc, ggplot2::aes(x = cause, y = effect, fill = cs)) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradient(low = "white", high = "blue") +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Cause", y = "Effect", fill = "CS Value") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

ggplot2::ggplot(data = bio_gcmc,
                ggplot2::aes(x = effect, y = cause, fill = sig)) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::scale_fill_manual(
    values = c("T" = "#081c57", "F" = "#ffffd9"), 
    labels = c("significant"," not significant")) +
  ggplot2::geom_text(ggplot2::aes(label = round(cs, 3)), color = "black") +
  ggplot2::labs(x = "Effect", y = "Cause") +
  ggplot2::coord_equal() +
  ggplot2::theme_void() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90),
    axis.text.y = ggplot2::element_text(color = "black"),
    axis.title.y = ggplot2::element_text(angle = 90),
    axis.title = ggplot2::element_text(face = "italic", color = "red"),
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank()
  )

bio_gccm = readr::read_csv('./result/bio_gccm.csv')|> 
  dplyr::select(x,y,x_xmap_y_mean,x_xmap_y_sig,y_xmap_x_mean,y_xmap_x_sig)