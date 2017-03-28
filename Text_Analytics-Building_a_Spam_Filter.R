#' ---	
#' title: 'Text Analytics: Building A Spam Filter'	
#' author: "Godfred Somua-Gyimah"	
#' date: "February 20, 2017"	
#' output:	
#'   pdf_document:	
#'     toc: yes	
#'     toc_depth: 3	
#'   html_document:	
#'     toc: yes	
#'     toc_depth: '3'	
#' ---	
#' 	
#' # INTRODUCTION	
#' 	
#' The increasing volume of unsolicited bulk e-mail (also known as spam) has generated the need for reliable anti-spam filters. Machine learning techniques now a days are used to automatically filter the spam e-mail at very successful rates.	
#' 	
#' In this project, we will attempt to build a spam detection engine which meets current industry standards. So our model should be able to classify a given e-mail as either spam or not with a considerably high level of accuracy. We will be downloading data from the University of Campinas, Brazil website for our spam analysis.	
#' 	
#' 	
#' 	
knitr::opts_chunk$set(echo = TRUE)	
#' 	
#' 	
#' &nbsp;	
#' &nbsp;	
#' 	
#' **Note**: In order to run this demo, the following R packages must be installed in your R environment:	
#' 	
#' - magrittr: forward pipe operator	
#' - stringr: string manipulation	
#' - dplyr: Data Wrangling	
#' - tm : text mining	
#' - ggplot2 : data visualization	
#' 	
#' 	
#' 	
#' \newpage	
#' 	
#' 	
#' 	
# Clean the environment	
rm(list = ls())	
library(magrittr)	
	
#' 	
#' 	
#' # 1. Download, Read and Transform Raw Data	
#' 	
#' The movie taglines are stored in a flat txt file. It is not a tabular data structure, so we cannot use read methods such as read.csv() to directly read in the raw data. In this case, we need to read all lines into a character vector. Then we can further extract information from this charactor vector to create a data frame.	
#' 	
#' 	
# We will download the txt file using download.file()	
# and unzip it and read it line by line.	
# This file is unstructured so we can't use read.csv	
# Read the text file line by line	
	
temp <- tempfile()	
URL <- "http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/smsspamcollection.zip"	
download.file(URL, temp) #destfile = "./spam.zip", method="curl")	
unz(temp, "SMSSpamCollection.txt")	
	
con <- file("SMSSpamCollection.txt",open="r")	
line <- readLines(con)	
close(con)	
	
unlink(temp)	
	
#' 	
#' 	
#' 	
# Show the structure of line	
str(line)	
	
#' 	
#' 	
#' Show the first 20 lines of the character vector.	
#' 	
head(line, 20)	
#' 	
#' 	
#' 	
# Remove all white space using trimws	
line <- line[trimws(line)!=""]	
	
head(line, 20)	
#' 	
#' 	
#' Now, let's extract important information from the character vector and construct a data frame to represent the dataset.	
#' 	
#' First, we can use grep() with regular expression to extract the index of all documents. Each document starts with either "ham' or "spam". (Note: "^" means starting )	
#' 	
#' 	
ID <- grep("^(ham|spam)", line, value = FALSE)	
# value = FALSE returns index numbers, rather than actual cell values	
	
#' 	
#' 	
#' 	
#' 	
#' 	
# Create a data frame	
df <- data.frame(ID)	
head(df)	
#' 	
#' 	
#' 	
# Extract Class and store them in the data frame	
df$full_text <- grep("{1,2}am", line, value = TRUE) #line[df$index]	
head(df)	
#' 	
#' 	
#' 	
#' 	
# load the stringr package and create a column for the text	
library(stringr)	
df$Text <- gsub("^[a-z]{1,4}.\\t", "", df$full_text)	
	
#' 	
#' 	
#' 	
#' 	
#' 	
# Add a column in the data frame to store tagline text	
df$Class <- str_extract(df$full_text, "[a-z]{1,4}")	
head(df)	
#' 	
#' 	
#' 	
#' 	
# Now, remove the full_text column	
	
df <- subset(df, select = -full_text)	
#' 	
#' 	
#' 	
#' 	
# Check the tail of the data frame	
tail(df)	
#' 	
#' 	
#' 	
# Remove the line object from the current environment	
#rm(line)	
#' 	
#' 	
#' Plot frequency of messages by category	
#' 	
# Load ggplot2 package	
library(ggplot2)	
#' 	
#' 	
#' 	
#' 	
ggplot(data = df, aes(x = Class)) +	
  geom_bar(fill = "grey", colour = "black") +	
  labs(x = "Class", y = "Number of Messages")	
#' 	
#' 	
#' 	
#' # 2. Text Analysis	
#' 	
#' # 2.1. Aggragate to Year Level	
#' 	
#' We choose to analyze movies released during a 40-year from 1974 through 2013.	
#' 	
#' The unit of analysis is at year level, thus we need to reate an aggregate tagline_text collection for each year of interest.	
#' 	
#' 	
# Load dplyr package	
library(dplyr)	
#' 	
#' 	
#' # 2.2. Create Document Objects	
#' 	
# Load NLP and text mining (tm) packages	
library(NLP)	
library(tm)	
#' 	
#' 	
#' 	
#' 	
# Creating Corpus	
	
SMS_Corpus <- Corpus(VectorSource(df$Text))	
	
document.collection <- Corpus(VectorSource(df$Text))	
#' 	
#' 	
#' 	
#' # 2.3. Textual Data Transformation	
#' 	
#' 	
library(SnowballC)	
#' 	
#' 	
#' 	
# strip whitspace from the documents in the collection	
document.collection <- tm_map(document.collection, stripWhitespace)	
	
# convert uppercase to lowercase in the document collection	
document.collection <- tm_map(document.collection, content_transformer(tolower))	
	
# remove numbers from the document collection	
document.collection <- tm_map(document.collection, removeNumbers)	
	
# remove punctuation from the document collection	
document.collection <- tm_map(document.collection, removePunctuation)	
	
# Convert to Plain Text Document	
document.collection <- tm_map(document.collection, PlainTextDocument)	
	
# using a standard list, remove English stopwords from the document collection	
document.collection <- tm_map(document.collection,removeWords, stopwords("english"))	
	
# Stem	
document.collection <- tm_map(document.collection, stemDocument, language = "english")  	
	
inspect(document.collection)	
#' 	
#' 	
#' # 2.4 Creating Word Cloud 	
#' 	
# Create a word cloud to visualize term frequencies	
library(wordcloud)	
library(RColorBrewer)	
	
wordcloud(document.collection,	
          max.words =200,	
          random.order = FALSE,	
          colors = brewer.pal(7, "Dark2")	
          )	
	
#' 	
#' 	
#' # 2.4. Quantify Textual Data	
#' 	
#' We use the bag of words approach. First, let's create a terms-documents matrix to quantify the textual data.	
#' 	
#' 	
# Create terms-documents matrix as stated in the Home work	
tdm <- TermDocumentMatrix(document.collection)	
#' 	
#' 	
#' Then we can remove sparse terms from the matrix.	
#' 	
#' 	
examine.tdm <- removeSparseTerms(tdm, sparse = 0.90)	
top.words <- Terms(examine.tdm)	
print(top.words) 	
#' 	
#' There are no words with high sparsity (in this case, 55%)	
#' 	
#' 	
# Inspect the term-document matrix	
inspect(tdm[, 1:10])	
#' 	
#' 	
#' 	
#' 	
# To use documents as rows and term features as columns, 	
# we need to create documents-term-matrix	
# Tokenizing the corpus...	
dtm <- DocumentTermMatrix(document.collection)	
#' 	
#' 	
#' 	
#' 	
# Create indices of spam and ham for word cloud visualization	
	
spam_indices <- which(df$Class== "spam")	
ham_indices <- which(df$Class== "ham")	
	
spam_indices[1:3]	
#' 	
#' 	
ham_indices[1:3]	
	
#' 	
#' 	
	
wordcloud(document.collection[ham_indices],	
          colors = brewer.pal(8, "Dark2"),	
          min.freq=30)	
	
#' 	
#' 	
#' 	
#' 	
	
wordcloud(document.collection[spam_indices],	
          colors = brewer.pal(7, "Dark2"),	
          min.freq=2)	
	
#' 	
#' It appears some of the most common words in the spam messages were 'receive', 'free', 'win', 'prize', 'won'etc.	
#' 	
#' 	
#' # 2.5 Building a Spam Filter	
#' 	
#' ## 2.5.1 Dividing the data into train and test sets	
#' 	
# Divide corpus into training and test data.	
# Use 70% for training and 30% for testing	
	
sms_raw_train <- df[1:3902,]	
sms_raw_test <- df[3903:5574,]	
	
# and the document-term-matrix and clean corpus	
sms_dtm_train <- dtm[1:3902,]	
sms_dtm_test <- dtm[3903:5574,]	
sms_corpus_train <- document.collection[1:3902]	
sms_corpus_test <- document.collection[3903:5574]	
	
#' 	
#' 	
#' 	
# Separate training data into ham and spam	
	
ham <- subset(sms_raw_train, Class== "ham")	
spam <- subset(sms_raw_train, Class== "spam")	
	
#' 	
#' 	
#' To increase the predictive power of the classifier, we will only include frequently occuring words.	
#' 	
#' 	
# Set word frequency to 6 or more.	
	
freq_words <- findFreqTerms(sms_dtm_train, 6)	
length(freq_words)	
	
#' 	
#' Now, we will create document-term matrices using only the high frequency words	
#' 	
sms_train <- DocumentTermMatrix(sms_corpus_train, control=list(dictionary = freq_words))	
	
sms_test <- DocumentTermMatrix(sms_corpus_test, control=list(dictionary = freq_words))	
	
#' 	
#' 	
#' Convert count information to "Yes" and "No"	
#' 	
#' Naive Bayes classification needs present or absent info on each word in a message. We have counts of occurances. Convert the document-term matrices.	
#' 	
#' 	
# Changing variables to binary classes	
convert_counts <- function(x) {	
  y <- ifelse(x > 0, 1,0)	
  y <- factor(y, levels=c(0,1))	
  y	
	
}	
	
# Convert document-term matrices	
sms_trains<- apply(sms_train, 2, convert_counts)	
sms_tests<- apply(sms_test, 2, convert_counts)	
	
#' 	
#' 	
#' 	
# Changing variables to binary classes for the raw data	
#  where 1 <- ham and 0 <- spam	
	
sms_raw_train$newClass <- ifelse(sms_raw_train$Class== "ham", 1,0)	
	
sms_raw_test$newClass <- ifelse(sms_raw_test$Class== "ham", 1,0)	
	
#' 	
#' 	
#' 	
#' 	
#' ## 2.5.2 Naive Bayes Classifier	
#' 	
#' 	
# We'll use a Naive Bayes classifier provided in the package e1071	
library(e1071)	
nb_classifier <- naiveBayes(sms_trains, factor(sms_raw_train$Class))	
class(nb_classifier)	
	
#' 	
#' 	
#' ## Model Evaluation	
#' We will evaluate the performance of the classifier on the test data using predict()	
#' 	
#' 	
	
nb_test_pred <- predict(nb_classifier, newdata=sms_tests)	
	
#' 	
#' 	
#' Generate the confusion matrix for the SMS_Naive_Bayes Classifier	
#' 	
# Show confusion matrix	
table(nb_test_pred, sms_raw_test$Class)	
	
#' 	
#' Show all performance metrics	
#' 	
#' 	
library(caret)	
conf.mat <- confusionMatrix(nb_test_pred, sms_raw_test$Class)	
conf.mat$overall	
	
#' 	
#' 	
#' 	
	
conf.mat	
	
#' 	
#' 	
#' 	
#' Therefore, from the confusion matrix, our spam filter is able to correctly classify e-mails 97.8% of the time which is great.	
