#0. Install and load the following packages
library(pdftools)
library(dplyr)
library(stringr)

#A. Working with one single document

#1.a. Provide R with the URL or path to your document
pdf_path <- "C:\\Users\\Daniel\\Desktop\\Texts Workshop CILC23\\58.pdf"

#2.a. Extract the text using R
txt_output <- pdftools::pdf_text(pdf_path) %>%
  paste(sep = " ") %>%
  stringr::str_replace_all(fixed("\n"), " ") %>%
  stringr::str_replace_all(fixed("\r"), " ") %>%
  stringr::str_replace_all(fixed("\t"), " ") %>%
  stringr::str_replace_all(fixed("\""), " ") %>%
  paste(sep = " ", collapse = " ") %>%
  stringr::str_squish() %>%
  stringr::str_replace_all("- ", "") 

#3.a. Inspect the text
str(txt_output)

#4.a. save one single text to a .txt file
write(txt_output, file="58.txt")


#B. Working with multiple documents

#1.b. Provide R with the URL or path to your documents
dirpath<-"C:/Users/Daniel/Desktop/Texts Workshop CILC23"

#2.b. Create a function allowing us to create plain txt files from several PDF documents.

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

#3.b. Apply the function created to your imported texts
txts <- convertpdf2txt(dirpath)
str(txts)

#4.b. Add names to your txt files
names(txts) <- paste("textworkshopcilc23-", 1:length(txts), sep = "")

#5.b. Save result to disc
lapply(seq_along(txts), function(i)writeLines(text = unlist(txts[i]),
                                              con = paste("C:\\Users\\Daniel\\Desktop\\Texts Workshop CILC23", 
                                                          names(txts)[i],
                                                          ".txt", 
                                                          sep = "")))
