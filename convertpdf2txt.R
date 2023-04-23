# activate packages
library(pdftools)
library(dplyr)
library(stringr)

# you can use an url or a path that leads to a pdf document
pdf_path <- "C:/Users/Daniel Granados/Google Drive/Affaires/Universidad de Murcia/Estudios/Doctorado en Lingüística/1. Diseño de corpus/EN Corpus Dissertation/EN Statute Law/ukpga_20210003_en.pdf"
# extract text
txt_output <- pdftools::pdf_text(pdf_path) %>%
  paste(sep = " ") %>%
  stringr::str_replace_all(fixed("\n"), " ") %>%
  stringr::str_replace_all(fixed("\r"), " ") %>%
  stringr::str_replace_all(fixed("\t"), " ") %>%
  stringr::str_replace_all(fixed("\""), " ") %>%
  paste(sep = " ", collapse = " ") %>%
  stringr::str_squish() %>%
  stringr::str_replace_all("- ", "") 
# inspect
str(txt_output)
#save one single text to a txt file
write(txt_output, file="ACT001.txt")


#multiple documents at the same time
dirpath<-"C:/Users/Daniel Granados/Google Drive/Affaires/Universidad de Murcia/Estudios/Doctorado en LingÃ¼Ã­stica/1. DiseÃ±o de corpus/EN British Public Law Corpus/EN Statute Law - PDF/Acts of the Nothern Irish Parliament/NI-2014"

convertpdf2txt <- function(dirpath){
  files <- list.files(dirpath, full.names = T)
  x <- sapply(files, function(x){
    x <- pdftools::pdf_text(x) %>%
      paste(sep = " ") %>%
      stringr::str_replace_all(fixed("\n"), " ") %>%
      stringr::str_replace_all(fixed("\r"), " ") %>%
      stringr::str_replace_all(fixed("\t"), " ") %>%
      stringr::str_replace_all(fixed("\""), " ") %>%
      paste(sep = " ", collapse = " ") %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("- ", "") 
    return(x)
  })
}

#APPLY FUNCTION CREATED
txts <- convertpdf2txt(dirpath)
str(txts)
# add names to txt files
names(txts) <- paste("nia2014-", 1:length(txts), sep = "")
# save result to disc
lapply(seq_along(txts), function(i)writeLines(text = unlist(txts[i]),
                                              con = paste("C:/Users/Daniel Granados/Google Drive/Affaires/Universidad de Murcia/Estudios/Doctorado en Lingüística/1. Diseño de corpus/EN British Public Law Corpus/EN Statute Law", names(txts)[i],".txt", sep = "")))
