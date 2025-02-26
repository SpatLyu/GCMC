gcmc = \(data,E,k,r = 0,trend.rm = FALSE,verbose = TRUE) {
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

