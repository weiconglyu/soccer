library(shiny)
library(shinyjs)
library(htmltools)
library(DT)
library(RSQLite)
library(DBI)

load('predict.RData')
con <- dbConnect(SQLite(), 'soccer.sqlite')

addAll <- function(a, team = F) {
    if (length(a) > 0 && a[[1]] < 0)
        a
    else
        c(setNames(-1, if (team) '(All Teams)' else '(All Leagues)'), a)
}
