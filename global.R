library(shiny)
library(DT)
library(RSQLite)
library(DBI)

con <- dbConnect(SQLite(), 'soccer.sqlite')

league <- dbGetQuery(con, '
SELECT leagueID, leagueName
FROM League
')
league.list <- with(league, as.list(setNames(leagueID, leagueName)))

