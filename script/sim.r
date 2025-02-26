coords = expand.grid(x = 1:20, y = 1:20)
grid_sf = sf::st_as_sf(coords, coords = c("x", "y"))

g.dummy = gstat::gstat(formula = z ~ 1, dummy = TRUE, beta = 0,
                       model = gstat::vgm(psill = 1, model = "Sph", range = 5), 
                       nmax = 50) 

sim = predict(g.dummy, newdata = grid_sf, nsim = 1) |> 
  dplyr::select(x = sim1)

spEDM::embedded(sim,"x",E = 1)

listw = sdsfun::spdep_nb(sim) |> 
  spdep::nb2listw(style = "W")

wx = spEDM::embedded(sim,"x",E = 1)
