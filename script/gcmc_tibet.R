bio = readr::read_csv('./tibet2020.csv') |> 
  sf::st_as_sf(coords = c("x","y"), crs = 4326)

spEDM::gccm(data = bio,
            cause = "sm",
            effect = "bio",
            libsizes = 450,
            E = 3)

spEDM::gccm(data = bio,
            cause = "pre",
            effect = "bio",
            libsizes = 415,
            E = 3)


startTime = Sys.time()
bio_res = spEDM::gcmc(data = bio,
                      cause = "pre",
                      effect = "bio",
                      E = 3,
                      k = 415,
                      r = 0,
                      trend.rm = FALSE)
bio_res
endTime = Sys.time()
print(difftime(endTime,startTime, units ="mins"))