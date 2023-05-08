#0. Install and load the following packages
library(reticulate)
library(spacyr)
library(Rcpp)
library(quanteda)
library(readtext)
library(dplyr)

#1. Import texts and create a corpus
Judgments<-readtext("C:\\Users\\Daniel\\Desktop\\Texts Workshop CILC23")
Judgmentscorpus<-corpus(Judgments)

#2. Tokenize text removing numbers and punctuation
Judgmentstokens <- tokens(Judgmentscorpus, 
                          what = "word", 
                          remove_numbers = TRUE, 
                          remove_punct = TRUE)

#3. Remove stop words
library(stringr)
Judgmentstokens_cleaned <- tokens_remove(Judgmentstokens, 
                                         stopwords("english"))

#Alternative way to remove punctuation
punctuation <- c(",", ".", "!", "?", ";", ":", "-", "'", "\"", "(", ")", "[", "]", "{", "}")
Judgmentstokens_cleaned <- tokens_remove(Judgmentstokens_cleaned, 
                                         pattern = paste0("\\", 
                                                          punctuation, 
                                                          collapse = "|"))


#4. Convert tokens to a document-feature matrix (DFM)
Judgmentsdfm <- dfm(Judgmentstokens_cleaned)


#5.Create a wordcloud
library(quanteda.textplots)
set.seed(100)
textplot_wordcloud(Judgmentsdfm, min_freq = 6, random_order = FALSE,
                   rotation = .25,
                   colors = RColorBrewer::brewer.pal(8, "Dark2"))

topfeatures(Judgmentsdfm, 50)


#6. Display 10 most common words in a barplot
library(quanteda.textstats)
library(tidyr)
# calculate term frequency
tf <- textstat_frequency(Judgmentsdfm)

# view top 10 most frequent words
top_words <- head(tf, 10)
print(top_words)

# create a bar chart of top 10 most frequent words
barplot(top_words$frequency, names.arg = top_words$feature, 
        xlab = "Word", ylab = "Frequency", 
        main = "Top 10 Most Frequent Words")

#7. Calculate Lexical Diversity
library(quanteda)
# calculate lexical density
unique_words <- sum(Judgmentsdfm > 0)
total_words <- sum(Judgmentsdfm)
lexdensity <- unique_words / total_words

# display results
cat(paste0("Lexical Density: ", round(lexdensity * 100, 2), "%"))











#BONUS: Annotating with Spacy
#1. Download and load the 'en_core_web_trf' model
spacy_initialize(
  model = "en_core_web_trf",
  condaenv = "base"
)

#2. Annotate tokens with POS tags
tokens_tagged <- spacy_parse(Judgmentscorpus, pos = TRUE)


# Apply spacy_parse() to one single document
n58pdfcorpus <- as.character(Judgmentscorpus[[5]])
taggedn58pdfcorpus<-spacy_parse(n58pdfcorpus, pos = TRUE)


#2. Split the documents in your corpus if their number of words outnumbered the limit
library(quanteda)
Judgmentstokens <- tokens(Judgmentscorpus) #Get the tokens file for your corpus

# Step 2.1.: Define the maximum sequence length
maxlen <- 500

# Step 2.2.: Split documents into sub-documents
sub_docs <- lapply(Judgmentscorpus, function(doc) {
  # Tokenize the document
  doc_tokens <- tokens(doc)
  
  # Split the tokens into smaller sub-documents
  split_tokens <- tokens_ngrams(doc_tokens, n = maxlen, concatenator = " ")
  
  return(split_tokens)
})

# Step 2.3: POS tag each sub-document using spacyr
pos_sub_docs <- lapply(sub_docs, function(sub_doc) {
  # Convert sub-doc to character
  sub_text <- as.character(sub_doc)
  
  # POS tag the sub-document
  pos_sub_doc <- spacy_parse(sub_text, pos = TRUE)
  
  return(pos_sub_doc)
})

#CAUTION: This requires too much memory
