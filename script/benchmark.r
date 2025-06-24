#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~            Validation of GCMC in benchmark systems            ~~~~~~~#
#~~~~~~~               Author: Wenbo Lv; Date: 2025-06-25              ~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Remove all R objects
rm(list = ls())

# install required packages
if (!requireNamespace("fields")) install.packages("fields")
if (!requireNamespace("MASS")) install.packages("MASS")
if (!requireNamespace("spEDM")) install.packages("spEDM")

#-----------------------------------------------------------------------------#
#------           Simulate spatial distribution of three species        ------#
#-----------------------------------------------------------------------------#

sim_trispecies = \(nx,ny,seed = 123){
  grid = expand.grid(seq(0, 10, length.out = nx), 
                     seq(0, 10, length.out = ny))
  cov.fun = \(d, range = 1.5, sill=1) sill * exp(-d/range)
  dist.mat = fields::rdist(grid)
  cov.mat = cov.fun(dist.mat, range=1.5, sill=1)
  set.seed(seed)
  res = replicate(3, {
    MASS::mvrnorm(1, rep(0, nrow(grid)), cov.mat) |>
      pmax(0) |>
      sdsfun::normalize_vector(0,1) |>
      matrix(nrow = nx, ncol = ny) |> 
      terra::rast()
  }, simplify = FALSE)
  terra::rast(res)
}

species = sim_trispecies(20,20, seed = 42) 
names(species) = letters[1:3]
terra::plot(species,nc = 3)

#-----------------------------------------------------------------------------#
#------                        Baseline scenario                        ------#
#-----------------------------------------------------------------------------#

s_val = terra::values(species)
stats::cor.test(s_val[,"a"],s_val[,"b"])
stats::cor.test(s_val[,"a"],s_val[,"c"])
stats::cor.test(s_val[,"b"],s_val[,"c"])

# spEDM::fnn(species, "a", E = 1:25, 
#            eps = stats::sd(terra::values(species[["a"]]), na.rm = TRUE))
# spEDM::fnn(species, "b", E = 1:25, 
#            eps = stats::sd(terra::values(species[["b"]]), na.rm = TRUE))
# spEDM::fnn(species, "c", E = 1:25, 
#            eps = stats::sd(terra::values(species[["c"]]), na.rm = TRUE))
# 
# g1 = spEDM::gcmc(species, "a", "b", E = 6, k = 120)
# g1
# g1$xmap
# 
# g2 = spEDM::gcmc(species, "b", "c", E = 6, k = 120)
# g2
# g2$xmap
# 
# g3 = spEDM::gcmc(species, "a", "c", E = 6, k = 120)
# g3
# g3$xmap
# 
# s0 = list(g1,g2,g3)

#-----------------------------------------------------------------------------#
#------                        Scenario 1: a→b→c                        ------#
#-----------------------------------------------------------------------------#

sim1 = spEDM::slm(species, x = "a", y = "b", z = "c", k = 4, step = 15, transient = 1, threshold = Inf,
                  alpha_x = 0.2, alpha_y = 0.2, alpha_z = 0.2, 
                  beta_xy = 1, beta_xz = 0, beta_yx = 0, beta_yz = 1, beta_zx = 0, beta_zy = 0)

species_scenario1 = species
terra::values(species_scenario1[["a"]]) = sim1$x
terra::values(species_scenario1[["b"]]) = sim1$y
terra::values(species_scenario1[["c"]]) = sim1$z
species_scenario1
terra::plot(species_scenario1)

spEDM::fnn(species_scenario1, "a", E = 1:25, 
           eps = stats::sd(terra::values(species_scenario1[["a"]]), na.rm = TRUE))
spEDM::fnn(species_scenario1, "b", E = 1:25, 
           eps = stats::sd(terra::values(species_scenario1[["b"]]), na.rm = TRUE))
spEDM::fnn(species_scenario1, "c", E = 1:25, 
           eps = stats::sd(terra::values(species_scenario1[["c"]]), na.rm = TRUE))

g1 = spEDM::gcmc(species_scenario1, "a", "b", E = 6, k = 120)
g1
g1$xmap

g2 = spEDM::gcmc(species_scenario1, "b", "c", E = 6, k = 120)
g2
g2$xmap

g3 = spEDM::gcmc(species_scenario1, "a", "c", E = 6, k = 120)
g3
g3$xmap

s1 = list(g1,g2,g3)
readr::write_rds(s1,'./result/benchmark/s1.rds')

#-----------------------------------------------------------------------------#
#------                        Scenario 2: a→b←c                        ------#
#-----------------------------------------------------------------------------#

sim2 = spEDM::slm(species, x = "a", y = "b", z = "c", k = 4, step = 15, transient = 1, threshold = Inf,
                  alpha_x = 0.2, alpha_y = 0.2, alpha_z = 0.2, 
                  beta_xy = 1, beta_xz = 0, beta_yx = 0, beta_yz = 0, beta_zx = 0, beta_zy = 1)

species_scenario2 = species
terra::values(species_scenario2[["a"]]) = sim2$x
terra::values(species_scenario2[["b"]]) = sim2$y
terra::values(species_scenario2[["c"]]) = sim2$z
species_scenario2
terra::plot(species_scenario2)

spEDM::fnn(species_scenario2, "a", E = 1:25, 
           eps = stats::sd(terra::values(species_scenario2[["a"]]), na.rm = TRUE))
spEDM::fnn(species_scenario2, "b", E = 1:25, 
           eps = stats::sd(terra::values(species_scenario2[["b"]]), na.rm = TRUE))
spEDM::fnn(species_scenario2, "c", E = 1:25, 
           eps = stats::sd(terra::values(species_scenario2[["c"]]), na.rm = TRUE))

g1 = spEDM::gcmc(species_scenario2, "a", "b", E = 6, k = 75)
g1
g1$xmap

g2 = spEDM::gcmc(species_scenario2, "b", "c", E = 6, k = 75)
g2
g2$xmap

g3 = spEDM::gcmc(species_scenario2, "a", "c", E = 6, k = 80)
g3
g3$xmap

s2 = list(g1,g2,g3)
readr::write_rds(s2,'./result/benchmark/s2.rds')

#-----------------------------------------------------------------------------#
#------                      Scenario 3: a←b→c                          ------#
#-----------------------------------------------------------------------------#

sim3 = spEDM::slm(species, x = "a", y = "b", z = "c", k = 4, step = 15, transient = 1, threshold = Inf,
                  alpha_x = 0.2, alpha_y = 0.2, alpha_z = 0.2, 
                  beta_xy = 0, beta_xz = 0, beta_yx = 1, beta_yz = 1, beta_zx = 0, beta_zy = 0)

species_scenario3 = species
terra::values(species_scenario3[["a"]]) = sim3$x
terra::values(species_scenario3[["b"]]) = sim3$y
terra::values(species_scenario3[["c"]]) = sim3$z
species_scenario3
terra::plot(species_scenario3)

spEDM::fnn(species_scenario3, "a", E = 1:25, 
           eps = stats::sd(terra::values(species_scenario3[["a"]]), na.rm = TRUE))
spEDM::fnn(species_scenario3, "b", E = 1:25, 
           eps = stats::sd(terra::values(species_scenario3[["b"]]), na.rm = TRUE))
spEDM::fnn(species_scenario3, "c", E = 1:25, 
           eps = stats::sd(terra::values(species_scenario3[["c"]]), na.rm = TRUE))

g1 = spEDM::gcmc(species_scenario3, "a", "b", E = 8, k = 160)
g1
g1$xmap

g2 = spEDM::gcmc(species_scenario3, "b", "c", E = 8, k = 160)
g2
g2$xmap

g3 = spEDM::gcmc(species_scenario3, "a", "c", E = 8, k = 160)
g3
g3$xmap

s3 = list(g1,g2,g3)
readr::write_rds(s3,'./result/benchmark/s3.rds')

#------------------------------------------------------------------------------#
#----------------------------    Plot the result    ---------------------------#
#------------------------------------------------------------------------------#

.process_xmap_result = \(g){
  tempdf = g$xmap
  tempdf$x = g$varname[1]
  tempdf$y = g$varname[2]
  tempdf = dplyr::select(tempdf, 1, x, y,
                         x_xmap_y_mean,x_xmap_y_sig,
                         y_xmap_x_mean,y_xmap_x_sig,
                         dplyr::everything())
  
  g1 = tempdf |>
    dplyr::select(x,y,y_xmap_x_mean,y_xmap_x_sig)|>
    purrr::set_names(c("cause","effect","ca","sig"))
  g2 = tempdf |>
    dplyr::select(y,x,x_xmap_y_mean,x_xmap_y_sig) |>
    purrr::set_names(c("cause","effect","ca","sig"))
  
  return(rbind(g1,g2))
}

plot_ca_matrix = \(.tbf,legend_title = "Causal Association"){
  .tbf = .tbf |>
    dplyr::mutate(sig_marker = dplyr::case_when(
      sig < 0.001 ~ "***",
      sig < 0.01  ~ "**",
      sig < 0.05  ~ "*",
      .default =  ""
    )) |>
    dplyr::mutate(sig_marker = paste0(round(ca,3),sig_marker))
  
  fig = ggplot2::ggplot(data = .tbf,
                        ggplot2::aes(x = effect, y = cause)) +
    ggplot2::geom_tile(color = "black", ggplot2::aes(fill = ca)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 0.25) +
    ggplot2::geom_text(ggplot2::aes(label = sig_marker), color = "black", family = "serif") +
    ggplot2::labs(x = "Effect", y = "Cause", fill = legend_title) +
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
      legend.key.width = ggplot2::unit(20, "pt"),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA)
    )
  return(fig)
}

s_list = list(s1,s2,s3)

purrr::walk(1:3, \(.i) {
  fig_s = s_list[[.i]] |>
    purrr::map(.process_xmap_result) |>
    purrr::list_rbind() |> 
    plot_ca_matrix()
  ggview::save_ggplot(fig_s +
                        ggview::canvas(width = 3.65, height = 4.05), 
                      paste0("./figure/benchmark/fig_benchmark",.i,".jpg"), dpi = 300)
})
