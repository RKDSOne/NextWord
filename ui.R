library(shiny)
library(pushoverr)
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

fluidPage(
        titlePanel("Next Word"),

        sidebarLayout(
                sidebarPanel(
                
                textInput("text", "Enter Text", "Welcome!")
                
        ),
        mainPanel(
                tabsetPanel(type = "tabs",  
                            tabPanel("Next Word", 
                                     h2('And the most probable next words are'),
                                     verbatimTextOutput('text1'),
                                     plotOutput('plot2')),
                            
                            tabPanel("Data",
                                     h2('Dataset'),
                                     p("In this capstone I will be applying data science in the area of natural language processing (NLP).
                                       The dataset can be downloaded from"), 
                                     a(hraf = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", 
                                       "NLP Dataset"),
                                     h3(""),
                                     p("Large databases comprising of text in a target language are commonly used when generating language models for various purposes. 
                                       In this exercise, I will use the English database. There are three other databases in German, Russian and Finnish."),
                                     h3(""),
                                     strong("Complete R code for data analysis and Shiny App can be found"),a(hraf = "https://github.com/sdasadia", 
                                                                                                         "Here")
                                     
                            ),
                                      
                            tabPanel("Algorithm",
                                     h2('n-gram Algorithm'),
                                     p("In the fields of computational linguistics and probability, 
                                        an n-gram is a contiguous sequence of n items from a given sequence of text or speech. 
                                        The items can be phonemes, syllables, letters, words or base pairs according to the application. 
                                        The n-grams typically are collected from a text or speech corpus. 
                                        When the items are words, n-grams may also be called shingles."),
                                     img(src = "a.png",height = 400, width = 600)
                                     )
                )
        
)
)
)