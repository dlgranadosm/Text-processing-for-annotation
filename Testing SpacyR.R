library(reticulate)

library(spacyr)
library(Rcpp)
library(quanteda)
library(readtext)
library(dplyr)
spacy_initialize(
  model = "en_core_web_trf", #select language
  condaenv = "phddissertation"
  
)
txt <- c(d1 = "Jon is playing music with the guitar.",
         d2 = "Jon is coming to the party tomorrowspaxy.")

#Here we need a quanteda corpus first - some useful code with quanteda
statutelawukpngazip<-readtext("G:\\Mi unidad\\Affaires\\Universidad de Murcia\\Estudios\\Doctorado en Lingüística\\1. Diseño de corpus\\EN Corpus Dissertation\\ENStatutelawukpnga.zip")
StatuteLawukpngaCorpus<-corpus(statutelawukpngazip)

#POS Tagging with SpacyR
POSTaggedStatuteLawukCorpus <- spacy_parse(StatuteLawukpngaCorpus, pos=TRUE, tag = TRUE, entity = FALSE, lemma = FALSE, dependency=FALSE, nounphrase=FALSE)


spacy_finalize()






StatuteLawascTokensClean<-quanteda::tokens(StatuteLawascCorpus, remove_numbers = TRUE, remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE)

statutetokens<-tokens(
  StatuteLawCorpusClean,
  what = "word",
  remove_punct = FALSE,
  remove_symbols = FALSE,
  remove_numbers = FALSE,
  remove_url = FALSE,
  remove_separators = TRUE,
  split_hyphens = FALSE,
  include_docvars = TRUE,
  padding = FALSE,
  verbose = quanteda_options("verbose"),
)


#Creating a wordcloud
StatuteCorpusnopunct<-tokens(Statutetokens, remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE, remove_separators = TRUE) # extracting tokens BUT ALSO removing numbers, punctuation, symbols, and so on.
dfmstatutelaw<- dfm(statutetokens, remove = stopwords("english"), remove_punct = TRUE) #doc matrix of a corpus

set.seed(100)
library(quanteda.textplots)
textplot_wordcloud(dfmstatutelaw, min_freq = 6, random_order = FALSE,
                   rotation = .25,
                   colors = RColorBrewer::brewer.pal(8, "Dark2"))

topfeatures(dfmstatutelaw, 50)

#Creating a Eucledean Distance on Normalized Token Frequency Plot
dfmstatutelawEuclePlot <- dfm_trim(dfmstatutelaw, min_termfreq = 5, min_docfreq = 3)
library(quanteda.textstats)
# hierarchical clustering - get distances on normalized dfm
tstat_dist <- textstat_dist(dfm_weight(dfmstatutelawEuclePlot, scheme = "prop"))
# hierarchical clustering the distance object
pres_cluster <- hclust(as.dist(tstat_dist))
# label with document names
pres_cluster$labels <- docnames(dfmstatutelawEuclePlot)
# plot as a dendrogram
plot(pres_cluster, xlab = "", sub = "",
     main = "Euclidean Distance on Normalized Token Frequency")


