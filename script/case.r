#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~         Case: Tibetan Plateau NPP        ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~    Author: Wenbo Lv; Date: 2025-03-07    ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

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

#------------------------------------------------------------------------------#
#------    Causality by Geographical Cross Mapping Cardinality (GCMC)    ------#
#------------------------------------------------------------------------------#

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
  dplyr::mutate(sig = dplyr::case_when(
    sig < 0.001 ~ "***",
    sig < 0.01  ~ "**",
    sig < 0.05  ~ "*",
    .default =  ""
  )) |> 
  dplyr::mutate(sig = paste0(round(cs,3),sig))

fig_gcmc = ggplot2::ggplot(data = npp_gcmc,
                ggplot2::aes(x = effect, y = cause)) +
  ggplot2::geom_tile(color = "black", ggplot2::aes(fill = cs)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 0.25) +
  ggplot2::geom_text(ggplot2::aes(label = sig), color = "black", family = "serif") +
  ggplot2::labs(x = "Effect", y = "Cause", fill = "Causal Score") +
  ggplot2::scale_x_discrete(expand = c(0, 0)) +
  ggplot2::scale_y_discrete(expand = c(0, 0)) +
  ggplot2::scale_fill_gradient(low = "#9bbbb8", high = "#256c68") +
  ggplot2::coord_equal() +
  ggplot2::theme_void() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 0, family = "serif"),
    axis.text.y = ggplot2::element_text(color = "black", family = "serif"),
    axis.title.y = ggplot2::element_text(angle = 90, family = "serif"),
    axis.title.x = ggplot2::element_text(color = "black", family = "serif",
                                         margin = ggplot2::margin(t = 5.5, unit = "pt")),
    legend.text = ggplot2::element_text(family = "serif"),
    legend.title = ggplot2::element_text(family = "serif"),
    legend.background = ggplot2::element_rect(fill = NA, color = NA),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.margin = ggplot2::margin(t = 1, r = 0, b = 0, l = 0, unit = "pt"),
    legend.key.width = ggplot2::unit(30, "pt"),
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color = "black", fill = NA)
) +
  ggview::canvas(width = 4.5, height = 5)
# ggview::save_ggplot(fig_gcmc, "./figure/fig_case_gcmc.pdf", device = cairo_pdf)
ggview::save_ggplot(fig_gcmc, "./figure/fig_case_gcmc.jpg", dpi = 300)

#------------------------------------------------------------------------------#
#------        Correlation by Pearson Correlation Coefficient(PCC)       ------#
#------------------------------------------------------------------------------#

pred.df = npp[terra::cellFromRowCol(npp,predindice[,1],predindice[,2])]
npp_pcc = psych::corr.test(pred.df)
pcc_v = npp_pcc |> 
  purrr::pluck("r") |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "xvar") |> 
  tidyr::pivot_longer(cols = -1,
                      names_to = "yvar",
                      values_to = "pcc")
pcc_p = npp_pcc |> 
  purrr::pluck("p") |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "xvar") |> 
  tidyr::pivot_longer(cols = -1,
                      names_to = "yvar",
                      values_to = "sig")
npp_pcc = dplyr::left_join(pcc_v,pcc_p,
                           by = c("xvar","yvar"))
readr::write_csv(npp_pcc,'./result/npp_pcc.csv')

npp_pcc = readr::read_csv('./result/npp_pcc.csv') 
npp_pcc = npp_pcc |> 
  dplyr::mutate(sig = dplyr::case_when(
    sig < 0.001 ~ "***",
    sig < 0.01  ~ "**",
    sig < 0.05  ~ "*",
    .default =  ""
  )) |> 
  dplyr::mutate(sig = paste0(round(pcc,3),sig))

fig_pcc = ggplot2::ggplot(data = npp_pcc,
                          ggplot2::aes(x = xvar, y = yvar)) +
  ggplot2::geom_tile(color = "black", ggplot2::aes(fill = pcc)) +
  ggplot2::geom_text(ggplot2::aes(label = sig), color = "black", family = "serif") +
  ggplot2::labs(x = "Variables", y = "Variables", fill = "Pearson Correlation") +
  ggplot2::scale_x_discrete(expand = c(0, 0)) +
  ggplot2::scale_y_discrete(expand = c(0, 0)) +
  ggplot2::scale_fill_gradient(low = "#9bbbb8", high = "#256c68") +
  ggplot2::coord_equal() +
  ggplot2::theme_void() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 0, family = "serif"),
    axis.text.y = ggplot2::element_text(color = "black", family = "serif"),
    axis.title.y = ggplot2::element_text(angle = 90, family = "serif"),
    axis.title.x = ggplot2::element_text(color = "black", family = "serif",
                                         margin = ggplot2::margin(t = 5.5, unit = "pt")),
    legend.text = ggplot2::element_text(family = "serif"),
    legend.title = ggplot2::element_text(family = "serif"),
    legend.background = ggplot2::element_rect(fill = NA, color = NA),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.margin = ggplot2::margin(t = 1, r = 0, b = 0, l = 0, unit = "pt"),
    legend.key.width = ggplot2::unit(30, "pt"),
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color = "black", fill = NA)
  ) +
  ggview::canvas(width = 4.5, height = 5)
# ggview::save_ggplot(fig_pcc, "./figure/fig_case_pcc.pdf", device = cairo_pdf)
ggview::save_ggplot(fig_pcc, "./figure/fig_case_pcc.jpg", dpi = 300)

#------------------------------------------------------------------------------#
#------    Causality by Geographical Convergent Cross Mapping (GCCM)     ------#
#------------------------------------------------------------------------------#

# construct the parameter sets for running GCCM
Es = c(3,3,3,3,5)
names(Es) = names(npp)
vars = utils::combn(names(npp),2,simplify = TRUE)
params = data.frame(
  cause = vars[1,],
  effect = vars[2,],
  Ex = Es[vars[1,]],
  Ey = Es[vars[2,]],
)

# take approximately ten minutes to run
npp_gccm = data.frame()
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
