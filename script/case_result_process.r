#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~         Handling the case results        ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~    Author: Wenbo Lv; Date: 2025-03-16    ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

.process_xmap_result = \(g,gcmc = TRUE){
  tempdf = g$xmap
  tempdf$x = g$varname[1]
  tempdf$y = g$varname[2]
  tempdf = dplyr::select(tempdf, 1, x, y,
                         x_xmap_y_mean,x_xmap_y_sig,
                         y_xmap_x_mean,y_xmap_x_sig,
                         dplyr::everything())
  
  if(!gcmc){
    tempdf = dplyr::slice_tail(tempdf,n = 1)
  }
  
  g1 = tempdf |> 
    dplyr::select(x,y,y_xmap_x_mean,y_xmap_x_sig)|> 
    purrr::set_names(c("cause","effect","ca","sig"))
  g2 = tempdf |> 
    dplyr::select(y,x,x_xmap_y_mean,x_xmap_y_sig) |> 
    purrr::set_names(c("cause","effect","ca","sig"))
  
  return(rbind(g1,g2))
}

gcmc_case1 = readr::read_rds('./result/case/gcmc_case1.rds') |> 
  purrr::map(.process_xmap_result) |> 
  purrr::list_rbind()
gcmc_case2 = readr::read_rds('./result/case/gcmc_case2.rds')
gcmc_case3 = readr::read_rds('./result/case/gcmc_case3.rds')