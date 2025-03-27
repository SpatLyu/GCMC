# figures: pdf -> jpg
.pdf2jpg = \(pdfname,jpgname,dpi = 300){
  pdftools::pdf_convert(pdf = pdfname, filenames = jpgname, dpi = dpi)
}

1:6 |> 
  purrr::walk(\(.x) {
    .pdf2jpg(paste0('./figure/figure',.x,'.pdf'),
             paste0('./manuscript/figure/figure',.x,'.jpg'))
  })



# references: doi -> bibtex
doi2bib = \(doi,style = "aps"){
  return(rcrossref::cr_cn(dois = doi, style = style, format = "bibtex"))
}

doi2bib("10.1098/rspl.1895.0041")     # pcc

doi2bib("10.2307/1912791")            # granger test
doi2bib("10.1111/pirs.12144")         # spatial-granger

doi2bib("10.1103/PhysRevLett.85.461") # transfer entropy


doi2bib("10.1111/pirs.12144")         # sem
doi2bib("10.1126/sciadv.aau4996")     # pcmci

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

doi2bib("10.1038/s41467-019-10105-3") # review1 
doi2bib("10.1038/s43017-023-00431-y") # review2
doi2bib("10.1016/j.scib.2021.10.002") # ts vs scs


# cit_styles = rcrossref::get_styles()
# stringr::str_subset(cit_styles, "^international-journal-of")
# "annals-of-the-association-of-american-geographers"