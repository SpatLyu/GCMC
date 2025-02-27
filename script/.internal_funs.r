simplex4lattice = \(data,lib,pred = lib,E = 1:10,
                    k = 4,tau = 1,trend.rm = FALSE) {
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
  }
  return(NULL)
}

gcmc4lattice = \(data,E,k,r = 0,trend.rm = FALSE,verbose = TRUE) {
    vars = names(data)[-which(names(data) == sdsfun::sf_geometry_name(data))]
    vars = utils::combn(vars,2,simplify = FALSE)
    
    resdf = data.frame()
    for (v in vars){
        g = spEDM::gcmc(data = data,
                        cause = v[1],
                        effect = v[2],
                        E = E,
                        k = k,
                        r = r,
                        trend.rm = trend.rm,
                        progressbar = verbose)
        tempdf = g$xmap
        tempdf$x = v[1]
        tempdf$y = v[2]
        resdf = rbind(resdf,tempdf)
    }
    return(resdf)
}

gccm4lattice = \(data,libsizes,E,k,trend.rm = TRUE,verbose = TRUE) {
    vars = names(data)[-which(names(data) == sdsfun::sf_geometry_name(data))]
    vars = utils::combn(vars,2,simplify = FALSE)
    
    resdf = data.frame()
    for (v in vars){
        g = spEDM::gccm(data = data,
                        cause = v[1],
                        effect = v[2],
                        libsizes = libsizes,
                        E = E,
                        k = k,
                        trend.rm = trend.rm,
                        progressbar = verbose)
        tempdf = g$xmap
        tempdf$x = v[1]
        tempdf$y = v[2]
        resdf = rbind(resdf,tempdf)
    }
    return(resdf)
}
