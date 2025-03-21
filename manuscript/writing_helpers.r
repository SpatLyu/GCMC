# figures: pdf -> jpg
.pdf2jpg = \(pdfname,jpgname,dpi = 300){
  pdftools::pdf_convert(pdf = pdfname, filenames = jpgname, dpi = dpi)
}

1:5 |> 
  purrr::walk(\(.x) {
    .pdf2jpg(paste0('./figure/figure',.x,'.pdf'),
             paste0('./manuscript/figure/figure',.x,'.jpg'))
  })



# references: doi -> bibtex
doi2bib = \(doi,style = "aps"){
  return(rcrossref::cr_cn(dois = doi, style = style, format = "bibtex"))
}

doi2bib("10.1038/344734a0")           # simplex projection
doi2bib("10.1126/science.1227079")    # ccm
doi2bib("10.1038/s41467-020-16238-0") # pcm
doi2bib("10.1016/j.fmre.2023.01.007") # cmc
doi2bib("10.1093/bib/bbad281")        # cme
doi2bib("10.1038/srep07464")          # cms
doi2bib("10.1038/s41467-023-41619-6") # gccm
doi2bib("10.1890/14-1479.1")          # multispatialccm
doi2bib("10.2139/ssrn.2637764")       # sdid
doi2bib("10.1177/0160017619869781")   # srdd         

# cit_styles = rcrossref::get_styles()
# stringr::str_subset(cit_styles, "^international-journal-of")
# "annals-of-the-association-of-american-geographers"