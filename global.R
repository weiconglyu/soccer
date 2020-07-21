lib <- c('shinydashboard', 'shinyjs', 'htmltools', 'DT', 'RSQLite', 'DBI',
         'tidyverse', 'randomForest', 'imputeMissings')
for (pkg in lib)
    if (!require(pkg, character.only = T))
        install.packages(pkg)

library(shiny)
library(shinyjs)
library(htmltools)
library(DT)
library(RSQLite)
library(DBI)

load('predict.RData')
con <- dbConnect(SQLite(), 'soccer.sqlite')

addAll <- function(a, team = F) {
    c(setNames(-1, if (team) '(All Teams)' else '(All Leagues)'), a)
}
