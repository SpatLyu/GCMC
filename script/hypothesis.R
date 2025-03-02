tibetbio = readr::read_csv('./data/tibet_bio.csv') |> 
  sf::st_as_sf(coords = c("x","y"), crs = 4326)

# embeddings:
m1 = spEDM::embedded(tibetbio,"sm",E = 3,tau = 5,trend.rm = FALSE)
m2 = spEDM::embedded(tibetbio,"bio",E = 3,tau = 6,trend.rm = FALSE)
# colnames(m1) = colnames(m2) = c("x","y","z")
# readr::write_csv(as.data.frame(m1),'./result/phase_space1.csv')
# readr::write_csv(as.data.frame(m2),'./result/phase_space2.csv')

H1 = spEDM:::RcppIntersectionCardinality(m1,m2,1:nrow(tibetbio),450,0,8,FALSE)
H0 = seq_along(H1) / 450
aucH1 = sdsfun::normalize_vector(H1)
aucH0 = sdsfun::normalize_vector(H0)
cmcH = data.frame(H1 = aucH1, H0 = aucH0)
# readr::write_csv(cmcH,'./result/cmcH.csv')

ggplot2::ggplot(data = cmcH,ggplot2::aes(x = aucH0)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = aucH1), 
                                    fill = "#ffcaa8", alpha = 0.5) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = aucH0), 
                       fill = "grey", alpha = 0.5) +
  ggplot2::geom_line(ggplot2::aes(y = aucH0, color = "H0"),lwd = 1) +
  ggplot2::geom_line(ggplot2::aes(y = aucH1, color = "H1"),lwd = 1) +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey",
                       lwd = 0.5, linetype = 3) +
  ggplot2::scale_color_manual(
    values = c("H0" = "#5F95D9","H1" = "#D95F5F"), 
    labels = c(latex2exp::TeX("$H_0: x \\nRightarrow y$"),
               latex2exp::TeX("$H_1: x \\Rightarrow y$"))) +
  ggplot2::scale_x_continuous(expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::labs(x = "Normalized r", y = "Normalized IC", color = "") +
  ggplot2::coord_equal()