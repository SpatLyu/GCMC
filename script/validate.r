library(terra)

# load data
npp = terra::rast('./npp.tif')
terra::global(npp[["npp"]],"notNA")

# sample 1500 points
strata = sgsR::strat_quantiles(npp[["npp"]],nStrata = 5)
set.seed(2025)
sam = sgsR::sample_strat(strata,nSamp = 1500,force = TRUE)
tibet_npp = terra::extract(npp,terra::vect(sam)) |> 
  dplyr::bind_cols(as.data.frame(sdsfun::sf_coordinates(sam))) |> 
  dplyr::select(-ID)
readr::write_csv(tibet_npp,'./data/tibet_npp.csv')
sam = sgsR::sample_strat(strata,nSamp = 1500,force = TRUE) |> 
  sdsfun::sf_coordinates()
predindice = terra::rowColFromCell(npp,terra::cellFromXY(npp,sam))
nnaindice = which(!is.na(terra::as.matrix(npp[["npp"]],wide = TRUE)),arr.ind = TRUE)

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
readr::write_csv(npp_gcmc,'./npp_gcmc.csv')

spEDM::simplex(npp,"npp",lib = predindice, pred = predindice,tau = 0,k = 6,trend.rm = FALSE)
spEDM::simplex(npp,"pre",lib = predindice, pred = predindice,tau = 0,k = 6,trend.rm = FALSE)
spEDM::simplex(npp,"tem",lib = predindice, pred = predindice,tau = 0,k = 6,trend.rm = FALSE)

tictoc::tic()
g = spEDM::gccm(data = npp,
                cause = "pre",
                effect = "npp",
                libsizes = as.matrix(expand.grid(seq(10,150,20),seq(20,320,20))),
                E = c(3,5),
                k = 6,
                lib = nnaindice,
                pred = predindice,
                trend.rm = TRUE,
                progressbar = TRUE)
tictoc::toc()
readr::write_rds(g,'./g.rds')
g = spEDM::gccm(data = npp,
                cause = "pre",
                effect = "npp",
                libsizes = seq(10,2000,100),
                E = c(3,5),
                tau = 0,
                k = 6,
                lib = predindice,
                pred = predindice,
                trend.rm = TRUE,
                progressbar = TRUE)
g1 = spEDM::gccm(data = npp,
                 cause = "pre",
                 effect = "tem",
                 libsizes = seq(10,2000,100),
                 E = c(2,6),
                 k = 6,
                 lib = nnaindice,
                 pred = predindice,
                 trend.rm = TRUE,
                 progressbar = TRUE)
g2 = spEDM::gccm(data = npp,
                 cause = "pre",
                 effect = "sm",
                 E = 3,
                 k = 5,
                 lib = nnaindice,
                 pred = predindice,
                 trend.rm = TRUE,
                 progressbar = TRUE)
g3 = spEDM::gccm(data = npp,
                 cause = "npp",
                 effect = "elev",
                 E = c(5,3),
                 k = 5,
                 lib = nnaindice,
                 pred = predindice,
                 trend.rm = TRUE,
                 progressbar = TRUE)