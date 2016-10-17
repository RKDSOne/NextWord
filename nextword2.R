library(ggplot2)
library(tm)
library(qdap)
library(rJava)
library(RWekajars)
library(RWeka) # Install JAVA before installing this
library(dplyr)
library(wordcloud)
library(stringr)

load("two.gram.Rdata")
load("three.gram.Rdata")
load("quad.gram.Rdata")


bigramPredict <- function(x) {
        xclean <- removeNumbers(removePunctuation(tolower(x)))
        xs <- tail(strsplit(xclean, " ")[[1]], 1)
        sd <- two.gram[two.gram$predictor == xs,]
        sd <- sd[order(-sd$total),]
        df_plot <- sd[1:10,]
        df_plot <- df_plot[complete.cases(df_plot),]
        #assign('df_plot',df_plot,envir=parent.frame())
        df_plot
}

trigramPredict <- function(x) {
        xclean <- removeNumbers(removePunctuation(tolower(x)))
        sd <- three.gram[three.gram$predictor == xclean,]
        sd <- sd[order(-sd$total),]
        df_plot <- sd[1:10,]
        df_plot <- df_plot[complete.cases(df_plot),]
        #assign('df_plot',df_plot,envir=parent.frame())
        df_plot
}

quadgramPredict <- function(x) {
        xclean <- removeNumbers(removePunctuation(tolower(x)))
        sd <- quad.gram[quad.gram$predictor == xclean,]
        sd <- sd[order(-sd$total),]
        df_plot <- sd[1:10,]
        df_plot <- df_plot[complete.cases(df_plot),]
        #assign('df_plot',df_plot,envir=parent.frame())
        df_plot
}

predict <- function(x) {
        if (wc(x) == 1) {
                df_plot <- bigramPredict(x)
        }
        if (wc(x) == 2) {
                df_plot <- trigramPredict(x)
        }
        if (wc(x) == 3) {
                df_plot <- quadgramPredict(x)
        }
        if (wc(x) > 3) {
                return("Prediction can't be made.")
        }

        list <- df_plot[,'prediction']
        cat(list,sep = "       ")
        
}

predict2 <- function(x) {
        if (wc(x) == 1) {
                df_plot <- bigramPredict(x)
        }
        if (wc(x) == 2) {
                df_plot <- trigramPredict(x)
                if (dim(df_plot)[1] == 0) {
                        df_plot <- bigramPredict(x)
                }
        }
        if (wc(x) == 3) {
                df_plot <- quadgramPredict(x)
                if (dim(df_plot)[1] == 0) {
                        df_plot <- trigramPredict(x)
                }
                if (dim(df_plot)[1] == 0) {
                        df_plot <- bigramPredict(x)
                }
        }
        if (wc(x) > 3) {
                df_plot <- quadgramPredict(x)
                if (dim(df_plot)[1] == 0) {
                        df_plot <- trigramPredict(x)
                }
                if (dim(df_plot)[1] == 0) {
                        df_plot <- bigramPredict(x)
                }
        }
        
        df_plot
        
}

plot <- ggplot(df) + geom_bar(aes(x=prediction,y=total),stat="identity")
        
        ggplot(df_plot[1:4,], aes(prediction,total)) + geom_point()


