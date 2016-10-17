library(ggplot2)
library(tm)
library(qdap)
library(rJava)
library(RWekajars)
library(RWeka) # Install JAVA before installing this
library(dplyr)
library(wordcloud)
library(stringr)
library(bigmemory)
library(slam)

# Open Connection
con_twitter <- file("en_US.twitter.txt", "r") 
con_news <- file("en_US.news.txt", "r") 
con_blogs <- file("en_US.blogs.txt", "r") 

# Readlines From Connection
news <- readLines("en_US.news.txt")
twitter <- readLines("en_US.twitter.txt")
blogs <- readLines("en_US.blogs.txt")

# Close Connections
close(con_twitter)
close(con_news)
close(con_blogs)

# Sampling Data

news_data <- sample(news, length(news)*0.5, replace = F)
twitter_data <- sample(twitter, length(twitter)*0.5, replace = F)
blogs_data <- sample(blogs, length(blogs)*0.5, replace = F)

rm("news")
rm("twitter")
rm("blogs")

# Convert into vectorsource

news.vec <- VectorSource(news_data)
twitter.vec <- VectorSource(twitter_data)
blogs.vec <- VectorSource(blogs_data)

# Make a Corpus

news.corpus <- Corpus(news.vec)
twitter.corpus <- Corpus(twitter.vec)
blogs.corpus <- Corpus(blogs.vec)

# Clean corpus (remove punctuations, numbers and convert into lower letters)

clean <- function(x) {
        
        data.corpus <- tm_map(x, content_transformer(removePunctuation))
        data.corpus <- tm_map(data.corpus, content_transformer(removeNumbers)) # remove numbers
        data.corpus <- tm_map(data.corpus, content_transformer(tolower)) # remove punctuation
        data.corpus
}
        
news.clean.corpus <- clean(news.corpus)
twitter.clean.corpus <- clean(twitter.corpus)
blogs.clean.corpus <- clean(blogs.corpus)

# Extract N-grams

UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

# This give the same thing as tdm1gram)
#one.g_Tokenizer <- NGramTokenizer(data.corpus, Weka_control(min = 1, max = 1))
#one_gram <- data.frame(table(one.g_Tokenizer))
#sd = TermDocumentMatrix(data.corpus, control = list(one.g_Tokenizer))

#two.g_Tokenizer <- NGramTokenizer(data.corpus, Weka_control(min = 2, max = 2))   
#two_gram <- data.frame(table(two.g_Tokenizer))

options(mc.cores=1)

#tdm1gram <- TermDocumentMatrix(data.corpus, control = list(tokenize = UnigramTokenizer))
#tdm1gram <- removeSparseTerms(tdm1gram,0.98)

# NEWS #

news.tdm2gram <- TermDocumentMatrix(news.clean.corpus, control = list(tokenize = BigramTokenizer))
news.tdm2gram <- removeSparseTerms(news.tdm2gram, sparse = 0.9997)
news.tdm2gram <- rollup(news.tdm2gram, 2, na.rm=TRUE, FUN = sum)

news.tdm3gram <- TermDocumentMatrix(news.clean.corpus, control = list(tokenize = TrigramTokenizer))
news.tdm3gram <- removeSparseTerms(news.tdm3gram, sparse = 0.9999)
news.tdm3gram <- rollup(news.tdm3gram, 2, na.rm=TRUE, FUN = sum)

news.tdm4gram <- TermDocumentMatrix(news.clean.corpus, control = list(tokenize = QuadgramTokenizer))
news.tdm4gram <- removeSparseTerms(news.tdm4gram, sparse = 0.99995)
news.tdm4gram <- rollup(news.tdm4gram, 2, na.rm=TRUE, FUN = sum)


# TWITTER #

twitter.tdm2gram <- TermDocumentMatrix(twitter.clean.corpus, control = list(tokenize = BigramTokenizer))
twitter.tdm2gram <- removeSparseTerms(twitter.tdm2gram, sparse = 0.9999)
twitter.tdm2gram <- rollup(twitter.tdm2gram, 2, na.rm=TRUE, FUN = sum)

twitter.tdm3gram <- TermDocumentMatrix(twitter.clean.corpus, control = list(tokenize = TrigramTokenizer))
twitter.tdm3gram<- removeSparseTerms(twitter.tdm3gram, sparse = 0.99993)
twitter.tdm3gram <- rollup(twitter.tdm3gram, 2, na.rm=TRUE, FUN = sum)

twitter.tdm4gram <- TermDocumentMatrix(twitter.clean.corpus, control = list(tokenize = QuadgramTokenizer))
twitter.tdm4gram<- removeSparseTerms(twitter.tdm4gram, sparse = 0.99999)
twitter.tdm4gram <- rollup(twitter.tdm4gram, 2, na.rm=TRUE, FUN = sum)

# BLOGS $

blogs.tdm2gram <- TermDocumentMatrix(blogs.clean.corpus, control = list(tokenize = BigramTokenizer))
blogs.tdm2gram<- removeSparseTerms(blogs.tdm2gram, sparse = 0.9997)
blogs.tdm2gram <- rollup(blogs.tdm2gram, 2, na.rm=TRUE, FUN = sum)

blogs.tdm3gram <- TermDocumentMatrix(blogs.clean.corpus, control = list(tokenize = TrigramTokenizer))
blogs.tdm3gram<- removeSparseTerms(blogs.tdm3gram, sparse = 0.9997)
blogs.tdm3gram <- rollup(blogs.tdm3gram, 2, na.rm=TRUE, FUN = sum)

blogs.tdm4gram <- TermDocumentMatrix(blogs.clean.corpus, control = list(tokenize = QuadgramTokenizer))
blogs.tdm4gram<- removeSparseTerms(blogs.tdm4gram, sparse = 0.9997)
blogs.tdm4gram <- rollup(blogs.tdm4gram, 2, na.rm=TRUE, FUN = sum)

# Convert Term Document Matrix into Data Frame


tdm2.to.df <- function(x) {
        df2gram <- as.data.frame(inspect(x)) # Converts td2gram to a data frame
        colnames(df2gram) <- c("num")
        df2gram[c('predictor', 'prediction')] <- subset(str_match(row.names(df2gram), "(.*) ([^ ]*)"), select=c(2,3))
        df2gram <- subset(df2gram, select=c('predictor', 'prediction', 'num'))
        df2gram <- df2gram[order(df2gram$predictor,-df2gram$num),]
        row.names(df2gram) <- NULL
        df2gram
}

tdm3.to.df <- function(x) {
        df3gram <- as.data.frame(inspect(x)) # Converts td2gram to a data frame
        colnames(df3gram) <- c("num")
        df3gram[c('predictor', 'prediction')] <- subset(str_match(row.names(df3gram), "(.*) ([^ ]*)"), select=c(2,3))
        df3gram <- subset(df3gram, select=c('predictor', 'prediction', 'num'))
        df3gram <- df3gram[order(df3gram$predictor,-df3gram$num),]
        row.names(df3gram) <- NULL
        df3gram
}

tdm4.to.df <- function(x) {
        df4gram <- as.data.frame(inspect(x)) # Converts td2gram to a data frame
        colnames(df4gram) <- c("num")
        df4gram[c('predictor', 'prediction')] <- subset(str_match(row.names(df4gram), "(.*) ([^ ]*)"), select=c(2,3))
        df4gram <- subset(df4gram, select=c('predictor', 'prediction', 'num'))
        df4gram <- df4gram[order(df4gram$predictor,-df4gram$num),]
        row.names(df4gram) <- NULL
        df4gram
}


# NEWS #

news2gram <- tdm2.to.df(news.tdm2gram)
news3gram <- tdm3.to.df(news.tdm3gram)
news4gram <- tdm4.to.df(news.tdm4gram)


# TWITTER #

twitter2gram <- tdm2.to.df(twitter.tdm2gram)
twitter3gram <- tdm3.to.df(twitter.tdm3gram)
twitter4gram <- tdm4.to.df(twitter.tdm4gram)


# BLOGS #

blogs2gram <- tdm2.to.df(blogs.tdm2gram)
blogs3gram <- tdm3.to.df(blogs.tdm3gram)
blogs4gram <- tdm4.to.df(blogs.tdm4gram) 


# Merger News, Twitter and Blogs Files #

# Two Grams #

test1 <- merge(news2gram,twitter2gram, by.x = c("predictor","prediction"),by.y = c("predictor","prediction"),all = TRUE)
two.gram <- merge(test1, blogs2gram, by.x = c("predictor","prediction"),by.y = c("predictor","prediction"),all = TRUE )

# Three Grams #

test1 <- merge(news3gram,twitter3gram, by.x = c("predictor","prediction"),by.y = c("predictor","prediction"),all = TRUE)
three.gram <- merge(test1, blogs3gram, by.x = c("predictor","prediction"),by.y = c("predictor","prediction"),all = TRUE )

# Quad Grams #

test1 <- merge(news4gram,twitter4gram, by.x = c("predictor","prediction"),by.y = c("predictor","prediction"),all = TRUE)
quad.gram <- merge(test1, blogs4gram, by.x = c("predictor","prediction"),by.y = c("predictor","prediction"),all = TRUE )

# Sum All Rows to get total frequency #

sum.all.rows <- function(x) {
        
        x$num.x[is.na(x$num.x)] <- 0
        x$num.y[is.na(x$num.y)] <- 0
        x$num[is.na(x$num)] <- 0
        x$total <- x$num.x + x$num.y + x$num
        x<- x[,c("predictor","prediction","total")]
        
}

two.gram <- sum.all.rows(two.gram)
three.gram <- sum.all.rows(three.gram)
quad.gram <- sum.all.rows(quad.gram)


save(two.gram, file="two.gram.Rdata")
save(three.gram, file="three.gram.Rdata")
save(quad.gram, file="quad.gram.Rdata")

