library(terra)

# load data
npp = terra::rast('./data/npp.tif')
terra::global(npp[["npp"]],"notNA")

# sample 2000 points to balance computational accuracy and processing time.
strata = sgsR::strat_quantiles(npp[["npp"]],nStrata = 5,
                               plot = TRUE, map = TRUE)
set.seed(2004)
sam = sgsR::sample_strat(strata,nSamp = 2000,force = TRUE) |> 
  sdsfun::sf_coordinates()
predindice = terra::rowColFromCell(npp,terra::cellFromXY(npp,sam))

# construct the parameter sets for running GCMC
Es = c(3,3,3,3,5)
names(Es) = names(npp)
vars = utils::combn(names(npp),2,simplify = TRUE)
params = data.frame(
  cause = vars[1,],
  effect = vars[2,],
  Ex = Es[vars[1,]],
  Ey = Es[vars[2,]],
  k = 450,
  r = 0,
  trend.rm = c(rep(FALSE,length.out = length(vars[1,]) - 1),TRUE) # remove the linear trend when running for NPP and elevation.
)

# take approximately ten minutes to run
npp_gcmc = data.frame()
for (v in 1:nrow(params)) {
  g = spEDM::gcmc(data = npp,
                  cause = params[v,"cause",drop = TRUE],
                  effect = params[v,"effect",drop = TRUE],
                  E = c(params[v,"Ex",drop = TRUE],params[v,"Ey",drop = TRUE]),
                  k = params[v,"k",drop = TRUE],
                  r = params[v,"r",drop = TRUE],
                  pred = predindice,
                  trend.rm = params[v,"trend.rm",drop = TRUE],
                  progressbar = TRUE)
  tempdf = g$xmap
  tempdf$x = params[v,"cause",drop = TRUE]
  tempdf$y = params[v,"effect",drop = TRUE]
  npp_gcmc = rbind(npp_gcmc,tempdf)
}
readr::write_csv(npp_gcmc,'./result/npp_gcmc.csv')

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
