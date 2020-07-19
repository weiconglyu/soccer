library(shiny)
library(DT)
library(RSQLite)
library(DBI)

con <- dbConnect(SQLite(), 'soccer.sqlite')
