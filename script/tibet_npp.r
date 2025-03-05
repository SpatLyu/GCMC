library(terra)

npp = terra::rast('./data/npp.tif')
plot(npp)

terra::global(npp[["npp"]],"notNA")

strata = sgsR::strat_quantiles(npp[["npp"]],nStrata = 5,
                               plot = TRUE, map = TRUE)
sam = sgsR::sample_strat(strata,nSamp = 2000,force = TRUE) |> 
  sdsfun::sf_coordinates()
predindice = terra::rowColFromCell(npp,terra::cellFromXY(npp,sam))

source('./.internal_funs.r')

# select the dimensions of embdedding
# simplex4grid(npp,lib = nnaindice, pred = predindice, trend.rm = TRUE)

npp_gcmc = gcmc4grid(npp,E = c(3,3,3,3,5),k = 450,r = 0,pred = predindice,trend.rm = FALSE)
readr::write_csv(npp_gcmc,'./npp_gcmc.csv')

npp_gcmc = readr::read_csv('./result/npp_gcmc.csv') 
ng1 = npp_gcmc |> 
  dplyr::select(x,y,y_xmap_x_mean,y_xmap_x_sig)|> 
  purrr::set_names(c("cause","effect","cs","sig"))
ng2 = npp_gcmc |> 
  dplyr::select(y,x,x_xmap_y_mean,x_xmap_y_sig) |> 
  purrr::set_names(c("cause","effect","cs","sig"))
npp_gcmc = rbind(ng1,ng2) |> 
  dplyr::mutate(sig = dplyr::if_else(sig < 0.05,"T","F")) |> 
  dplyr::mutate(sig = factor(sig,levels = c("T","F")))

ggplot2::ggplot(data = npp_gcmc,
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
    axis.text.x = ggplot2::element_text(angle = 0, family = "serif"),
    axis.text.y = ggplot2::element_text(color = "black", family = "serif"),
    axis.title.y = ggplot2::element_text(angle = 90, family = "serif"),
    axis.title = ggplot2::element_text(face = "italic", color = "black", family = "serif"),
    legend.text = ggplot2::element_text(family = "serif"),
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank()
  )
