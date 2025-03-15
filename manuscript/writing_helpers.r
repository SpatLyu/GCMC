# figures: pdf -> jpg
.pdf2jpg = \(pdfname,jpgname,dpi = 300){
  pdftools::pdf_convert(pdf = pdfname, filenames = jpgname, dpi = dpi)
}

.pdf2jpg('./figure/figure1.pdf','./manuscript/figure/figure1.jpg')
.pdf2jpg('./figure/figure2.pdf','./manuscript/figure/figure2.jpg')


# references: doi -> bibtex


doi2bib = \(doi,style = "annals-of-the-association-of-american-geographers"){
  return(rcrossref::cr_cn(dois = doi, format = "bibtex"))
}

# cit_styles = rcrossref::get_styles()
# stringr::str_subset(cit_styles, "^international-journal-of")