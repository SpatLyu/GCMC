# figures: pdf -> jpeg
.pdf2jpeg = \(pdfname, jpegname, dpi = 600){
  pdftools::pdf_convert(pdf = pdfname, filenames = jpegname, dpi = dpi)
}

.pdf2jpeg("./gcmc_data_codes/Spatial embedding/figure1.pdf",
          "./gcmc_manuscript/manuscript_revision/figure/figure1.jpeg")

.pdf2jpeg("./gcmc_data_codes/Schematic diagram/figure2.pdf",
          "./gcmc_manuscript/manuscript_revision/figure/figure2.jpeg")

.pdf2jpeg("./gcmc_data_codes/Schematic diagram/figure3.pdf",
          "./gcmc_manuscript/manuscript_revision/figure/figure3.jpeg")

.pdf2jpeg("./gcmc_data_codes/Synthetic benchmark/figure4.pdf",
          "./gcmc_manuscript/manuscript_revision/figure/figure4.jpeg")

.pdf2jpeg("./gcmc_data_codes/Case of residential crime study/figure5.pdf",
          "./gcmc_manuscript/manuscript_revision/figure/figure5.jpeg")

.pdf2jpeg("./gcmc_data_codes/Case of population density study/figure6.pdf",
          "./gcmc_manuscript/manuscript_revision/figure/figure6.jpeg")

.pdf2jpeg("./gcmc_data_codes/Case of net primary productivity study/figure7.pdf",
          "./gcmc_manuscript/manuscript_revision/figure/figure7.jpeg")

.pdf2jpeg("./gcmc_data_codes/Sensitivity analysis/figure8.pdf",
          "./gcmc_manuscript/manuscript_revision/figure/figure8.jpeg")



# references: doi -> bibtex
doi2bib = \(doi, style = "aps"){
  return(rcrossref::cr_cn(dois = doi, style = style, format = "bibtex"))
}

doi2bib("10.1098/rspl.1895.0041")     # pcc

doi2bib("10.2307/1912791")            # granger test
doi2bib("10.1111/pirs.12144")         # spatial-granger

doi2bib("10.1103/PhysRevLett.85.461") # transfer entropy


doi2bib("10.1111/pirs.12144")         # sem
doi2bib("10.1126/sciadv.aau4996")     # pcmci
doi2bib("10.1038/s41467-024-53373-4") # surd

doi2bib("10.1103/PhysRevA.45.3403")   # fnn
doi2bib("10.1063/1.1568692")          # spatial logistic map

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

doi2bib("10.1016/j.scib.2021.10.002")   # Temporally or spatially
doi2bib("10.1111/gean.12312")           # spatial causality review
doi2bib("10.1016/j.spasta.2022.100621") # causal inference in spatial statistics

                   
# style -> elsevier-harvard