simplex4lattice = \(data,lib,pred = lib,E = 1:10,
                    k = E + 2,tau = 1,trend.rm = FALSE) {
  vars = names(data)[-which(names(data) == sdsfun::sf_geometry_name(data))]
  
  for (v in vars){
    g = spEDM::simplex(data = data,
                       target = v,
                       lib = lib,
                       pred = pred,
                       E = E,
                       k = k,
                       tau = tau,
                       trend.rm = trend.rm)
    print(g)
  }
  return(NULL)
}

gcmc4lattice = \(data,E,k,r = 0,trend.rm = FALSE,verbose = TRUE) {
    vars = names(data)[-which(names(data) == sdsfun::sf_geometry_name(data))]
    names(E) = vars
    vars = utils::combn(vars,2,simplify = FALSE)
    Es = purrr::map(vars,\(.x) E[.x])
    
    resdf = data.frame()
    for (v in seq_along(vars)) {
        g = spEDM::gcmc(data = data,
                        cause = vars[[v]][1],
                        effect = vars[[v]][2],
                        E = Es[[v]],
                        k = k,
                        r = r,
                        trend.rm = trend.rm,
                        progressbar = verbose)
        tempdf = g$xmap
        tempdf$x = vars[[v]][1]
        tempdf$y = vars[[v]][2]
        resdf = rbind(resdf,tempdf)
    }
    return(resdf)
}

gccm4lattice = \(data,libsizes,E,k,trend.rm = TRUE,verbose = TRUE) {
    vars = names(data)[-which(names(data) == sdsfun::sf_geometry_name(data))]
    names(E) = vars
    names(k) = vars
    vars = utils::combn(vars,2,simplify = FALSE)
    Es = purrr::map(vars,\(.x) E[.x])
    Ks = purrr::map(vars,\(.x) k[.x])
    
    resdf = data.frame()
    for (v in seq_along(vars)) {
        g = spEDM::gccm(data = data,
                        cause = vars[[v]][1],
                        effect = vars[[v]][2],
                        libsizes = libsizes,
                        E = Es[[v]],
                        k = Ks[[v]],
                        trend.rm = trend.rm,
                        progressbar = verbose)
        tempdf = g$xmap
        tempdf$x = vars[[v]][1]
        tempdf$y = vars[[v]][2]
        resdf = rbind(resdf,tempdf)
    }
    return(resdf)
}
