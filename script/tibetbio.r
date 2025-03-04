source('./script/.internal_funs.r')

tibetbio = readr::read_csv('./data/tibet_bio.csv') |> 
  sf::st_as_sf(coords = c("x","y"), crs = 4326)

# select the dimensions of embdedding
simplex4lattice(tibetbio,lib = 1:1000, pred = 1001:nrow(tibetbio))

k = 500

# g = spEDM::gcmc(data = tibetbio,
#                 cause = "pre",
#                 effect = "bio",
#                 E = c(3,8),
#                 k = 500,
#                 r = 0,
#                 trend.rm = FALSE)
# 
# g1 = spEDM::gcmc(data = tibetbio,
#             cause = "tem",
#             effect = "pre",
#             E = c(3,3),
#             k = 500,
#             r = 0,
#             trend.rm = FALSE)

bio_gcmc = gcmc4lattice(tibetbio,E = c(7,7,8,3,3,3),k = k,r = 0,trend.rm = FALSE)
readr::write_csv(bio_gcmc,'./result/bio_gcmc.csv')

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

ggplot2::ggplot(data = bio_gcmc,
                ggplot2::aes(x = effect, y = cause, fill = sig)) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 0.75) +
  ggplot2::scale_fill_manual(
    values = c("T" = "#d1deca", "F" = "#eee2c8"), 
    labels = c("significant"," not significant")) +
  ggplot2::geom_text(ggplot2::aes(label = round(cs, 3)), color = "black") +
  ggplot2::labs(x = "Effect", y = "Cause", fill = "Significance") +
  ggplot2::scale_x_discrete(expand = c(0, 0)) +
  ggplot2::scale_y_discrete(expand = c(0, 0)) +
  ggplot2::coord_equal() +
  ggplot2::theme_void() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90),
    axis.text.y = ggplot2::element_text(color = "black"),
    axis.title.y = ggplot2::element_text(angle = 90),
    axis.title = ggplot2::element_text(face = "italic", color = "black"),
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank()
)


# result of gccm
bio_gccm = gccm4lattice(tibetbio,libsizes = seq(100,1600,100),E = c(7,7,8,3,3,3),k = 4,trend.rm = TRUE)
readr::write_csv(bio_gccm,'./result/bio_gccm.csv')
bio_gccm = readr::read_csv('./result/bio_gccm.csv')|> 
  dplyr::select(libsizes,x,y,x_xmap_y_mean,x_xmap_y_sig,y_xmap_x_mean,y_xmap_x_sig)

plot_gccmcurve = \(rhodf){
  fig = ggplot2::ggplot(data = rhodf,
                  ggplot2::aes(x = libsizes)) +
    ggplot2::geom_line(ggplot2::aes(y = y_xmap_x_mean,
                                    color = "y xmap x"),
                       lwd = 1.25) +
    ggplot2::geom_line(ggplot2::aes(y = x_xmap_y_mean,
                                    color = "x xmap y"),
                       lwd = 1.25) +
    ggplot2::scale_x_continuous(breaks = rhodf$libsizes, limits = c(min(rhodf$libsizes)-1,max(rhodf$libsizes)+1),
                                expand = c(0, 0), name = "Lib of Sizes") +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1),
                                expand = c(0, 0), name = expression(rho)) +
    ggplot2::scale_color_manual(values = c("x xmap y" = "#608dbe",
                                           "y xmap x" = "#ed795b"),
                                labels = c(paste0(rhodf$y[1], " causes ", rhodf$x[1]),
                                           paste0(rhodf$x[1], " causes ", rhodf$y[1])),
                                name = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(family = "serif"),
                   axis.text.x = ggplot2::element_text(angle = 30),
                   axis.title = ggplot2::element_text(family = "serif"),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = "inside",
                   legend.justification = c('left','top'),
                   legend.background = ggplot2::element_rect(fill = 'transparent'),
                   legend.text = ggplot2::element_text(family = "serif"))
  return(fig)
}

plot_gccmcurve(dplyr::filter(bio_gccm,x == "elev", y == "ndvi"))
