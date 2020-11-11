#   File:     pdf_word_freq.r
#   Author:   Copyright (C) RJ Podeschi, 2020
#   Date:     10/4/20
#
#   Purpose:  To analyze text within a set of PDF files
#
#   Sources:  https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/

#     This program is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; either version 2 of the License, or any later version.

#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.

#     You should have received a copy of the GNU General Public License along
#     with this program; if not, write to RJ Podeschi at rpodeschi@millikin.edu.

#     ------- Begin Code --------


#   install.packages("pdftools")
#   In Fedora, you will need to first install the Popplar dev lib:
#     sudo dnf install poppler-cpp-devel
#   install.packages"tm"
  
library(pdftools)
library(tm)
library(ggplot2)

# Set working directory 
setwd("/home/rpodeschi/Dropbox/R-Projects/pdf_analysis/data/")

# add all pdf files from the data directory to files vector
files <- list.files(pattern = "pdf$")

# Use lapply in conjunction with pdf_text (from pdftools) to create a list object
# for each journal article in the files vector. The full text from each journal is now
# in a list element for each journal
journals <- lapply(files, pdf_text)

# Not particularly helpful, but this lets you know how many pages are in each PDF
lapply(journals, length)

# Using the tm library (text mining), create a corpus (database of text) from the 
# files list object.
# * Not using anything from journals object below. Starting over with files object.

corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))

# Depending on the PDF, you might end up with some junk still in there like " and -
# Get rid of it by manually removing the puncuation
myStopWords <- c(stopwords("english"), "also", "can", "one", "use", "the", "for", "make", "what",
               "this", "doi", "two")
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeWords, myStopWords)
corp <- tm_map(corp, removePunctuation, ucp = TRUE)
corp <- tm_map(corp, removeNumbers)

# Let's create a term-document matrix (TDM). TDM stores counts of terms for each document.
# Need to get rid of all the junk, so remove punctuation, words like "the", conver to
# lower case, stem words (word roots), remove numbers, and limit to at least 3 occurences.
journals.tdm <- TermDocumentMatrix(corp,
                                   control =
                                     list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(3, Inf))))

# Look at what we came up with (just the first 10 terms)
inspect(journals.tdm[1:10,])

# Quickly find some summary statistics
findFreqTerms(journals.tdm, lowfreq = 100, highfreq = Inf)

# See the counts of the above in a matrix for each PDF
freqTermsPerPDF <- findFreqTerms(journals.tdm, lowfreq = 150, highfreq = Inf)

# Put it into a matrix for easier viewing
as.matrix(journals.tdm[freqTermsPerPDF,])

# Look at total counts for those words and then sum them across the rows
freqTermsPerPDF.tdm <- as.matrix(journals.tdm[freqTermsPerPDF,])

# Write this to a CSV to we can use it in Excel and get into a table
write.csv(freqTermsPerPDF.tdm, "freqTermsPerPDF.csv")

sort(apply(freqTermsPerPDF.tdm, 1, sum), decreasing = TRUE)

# Compute a score based on the number of matching terms
tm_term_score(journals.tdm, 
              c("assist", "access", "learn", "student", "educ", "disabl", "technolog", "studi", "group",
                "particip", "support", "social", "univers", "cours", "research", "use", "higher", "need"))

# Compute a score based on just technology
tm_term_score(journals.tdm, c("technolog"))

tm_term_score(journals.tdm, c("assist", "support"))

# Bring in studies.csv with journal metadata and merge with freqTermsPerPDF
studies <- read.csv("studies.csv", stringsAsFactors = FALSE)

# Transpose freqTermsPerPDF.tdm so columns become rows
freqTermsPerPDF_forJoin <- t(freqTermsPerPDF.tdm)
# I couldn't figure out how to rename the author column, so I did it in Excel - see line 113 on bringing that in.
# colnames(freqTermsPerPDF_forJoin)[0] = "author"

freqTermsTranspose <- read.csv("freqTermsTranspose.csv", stringsAsFactors = FALSE)

# Join data from freqTerms and studies
joined <- merge(freqTermsTranspose, studies, by.x = "Word", by.y = "Study", all.x = TRUE, all.y = TRUE)

# Write out to a CSV so I can take it into Excel and create PivotTables because it's quicker for me right now.
write.csv(joined, "freqTermsJoined.csv")

# Create a Wordcloud from the PDFs
library(wordcloud)

# Wordcloud doesn't like the TDM, so we'll need to rebuild corp just for the Wordcloud
corpWC <- tm_map(corp, removeWords, stopwords(kind = "SMART"))
corpWC <- tm_map(corpWC, removeWords, myStopWords)
corpWC <- tm_map(corpWC, removePunctuation)
corpWC <- tm_map(corpWC, stripWhitespace)
corpWC <- tm_map(corpWC,PlainTextDocument)

# Generate the WordCloud
wordcloud(corp, scale=c(4,0.5), max.words=50, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
