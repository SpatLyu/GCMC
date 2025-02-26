coords = expand.grid(x = 1:20, y = 1:20)
grid_sf = sf::st_as_sf(coords, coords = c("x", "y"))

g.dummy = gstat::gstat(formula = z ~ 1, dummy = TRUE, beta = 0,
                       model = gstat::vgm(psill = 1, model = "Sph", range = 5), 
                       nmax = 50) 

sim = predict(g.dummy, newdata = grid_sf, nsim = 1) |> 
  dplyr::select(x = sim1)

wxg = spEDM::embedded(sim,"x",E = 1)[,1]

set.seed(42)
u = rnorm(400)

sim$y = spdgp::sim_slx(u, sim$x, wxg)

cor.test(sim$x,sim$y)

g1 = spEDM::gccm(sim,"x","y",libsizes = seq(20,400,20),E = 3)
g2 = spEDM::gcmc(sim,"x","y",E = c(8,3),k = 100)



listw = sdsfun::spdep_nb(sim) |> 
  spdep::nb2listw(style = "W")

wx = spEDM::embedded(sim,"x",E = 1)
