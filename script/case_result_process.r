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

.process_pcc_result = \(g){
  pcc_v = g |> 
    purrr::pluck("r") |> 
    as.data.frame() |> 
    tibble::rownames_to_column(var = "cause") |> 
    tidyr::pivot_longer(cols = -1,
                        names_to = "effect",
                        values_to = "ca")
  pcc_p = g |> 
    purrr::pluck("p") |> 
    as.data.frame() |> 
    tibble::rownames_to_column(var = "cause") |> 
    tidyr::pivot_longer(cols = -1,
                        names_to = "effect",
                        values_to = "sig")
  return(dplyr::left_join(pcc_v,pcc_p,
                          by = c("cause","effect")))
}

.process_case_result = \(casenum,save = FALSE){
  case = list(
    gcmc = paste0('./result/case/gcmc_case',casenum,'.rds') |> 
      readr::read_rds() |> 
      purrr::map(.process_xmap_result) |> 
      purrr::list_rbind(),
    gccm = paste0('./result/case/gccm_case',casenum,'.rds') |>
      readr::read_rds() |> 
      purrr::map(.process_xmap_result,gcmc = FALSE) |> 
      purrr::list_rbind(),
    pcc = paste0('./result/case/pcc_case',casenum,'.rds') |>
      readr::read_rds() |>
      .process_pcc_result(),
    gd = paste0('./result/case/gd_case',casenum,'.rds') |>
      readr::read_rds() |>
      dplyr::select(cause = x, effect = y, ca = qv, sig)
  )
  
  if (save) writexl::write_xlsx(case,paste0("./result/case/case",casenum,".xlsx"))
  
  return(case)
}

purrr::map(1:3,.process_case_result,save = TRUE)