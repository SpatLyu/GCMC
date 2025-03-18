#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~             Plot Case Result             ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~    Author: Wenbo Lv; Date: 2025-03-18    ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#------------------------------------------------------------------------------#
#-------------            The causation matrix plot             ---------------#
#------------------------------------------------------------------------------#

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
    ) +
    ggview::canvas(width = 3.5, height = 4.05)
  return(fig)
}

save_ca_plot = \(casenum){
  case_xlsx = paste0('./result/case/case',casenum,'.xlsx')
  c("gcmc","gccm","pcc","gd") |> 
    purrr::walk(\(.x) {
      readxl::read_xlsx(case_xlsx,sheet = .x) |> 
        plot_ca_matrix() |> 
        ggview::save_ggplot(paste0('./figure/case/case',casenum,'_',.x,'.jpg'), dpi = 300)
    })
}

for (i in 1:3) save_ca_plot(i)


#------------------------------------------------------------------------------#
#----------------             Maps of case data              ------------------#
#------------------------------------------------------------------------------#

library(tmap)

# case1
columbus = system.file("case/columbus.gpkg", package="spEDM") |> 
  sf::read_sf() |> 
  dplyr::select(hoval,inc,crime)
columbus

fig11 = tm_shape(columbus) + 
  tm_polygons(fill = "hoval",
              fill.scale = tm_scale_continuous(n = 5),
              fill.legend = tm_legend(
                title = "Housing value (unit: $1000)",
                orientation = "landscape",
                frame = FALSE,
                title.color = "black",
                bg.color = "white",
                position = tm_pos_out(cell.h = "center",
                                      cell.v = "bottom",
                                      pos.h = "center",
                                      pos.v = "center"),
                show = TRUE
              ),
              col = 'grey', lwd = 1.25) +
  tm_compass(position = tm_pos_in(pos.h = 0.15,
                                  pos.v = 0.95)) +
  tm_layout(frame = FALSE,
            legend.title.fontfamily = "serif")
tmap_save(fig11,'./figure/case/map_case11.jpg',dpi = 300)

fig12 = tm_shape(columbus) + 
  tm_polygons(fill = "inc",
              fill.scale = tm_scale_continuous(n = 5),
              fill.legend = tm_legend(
                title = "Household income (unit: $1000)",
                orientation = "landscape",
                frame = FALSE,
                title.color = "black",
                bg.color = "white",
                position = tm_pos_out(cell.h = "center",
                                      cell.v = "bottom",
                                      pos.h = "center",
                                      pos.v = "center"),
                show = TRUE
              ),
              col = 'grey', lwd = 1.25) +
  tm_compass(position = tm_pos_in(pos.h = 0.15,
                                  pos.v = 0.95)) +
  tm_layout(frame = FALSE,
            legend.title.fontfamily = "serif")
tmap_save(fig12,'./figure/case/map_case12.jpg',dpi = 300)


fig13 = tm_shape(columbus) + 
  tm_polygons(fill = "crime",
              fill.scale = tm_scale_continuous(n = 5),
              fill.legend = tm_legend(
                title = "Residential burglaries and vehicle thefts per thousand households in the neighborhood",
                orientation = "landscape",
                frame = FALSE,
                title.color = "black",
                bg.color = "white",
                position = tm_pos_out(cell.h = "center",
                                      cell.v = "bottom",
                                      pos.h = "center",
                                      pos.v = "center"),
                show = TRUE
              ),
              col = 'grey', lwd = 1.25) +
  tm_compass(position = tm_pos_in(pos.h = 0.15,
                                  pos.v = 0.95)) +
  tm_layout(frame = FALSE,
            legend.title.fontfamily = "serif")
tmap_save(fig13,'./figure/case/map_case13.jpg',dpi = 300)

# case2
popd_sf = system.file("case/popdensity.csv",package = "spEDM") |> 
  readr::read_csv() |> 
  sf::st_as_sf(coords = c("x","y"), crs = 4326)
popd_sf