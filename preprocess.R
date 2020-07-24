library(RSQLite)
library(DBI)
library(tidyverse)
library(randomForest)
library(imputeMissings)
set.seed(1234567)

getDate <- function(date) {
    sapply(strsplit(date, ' '), function(v) { v[1] })
}

# original data is from https://www.kaggle.com/hugomathien/soccer/data
input <- dbConnect(SQLite(), 'database.sqlite')
output <- dbConnect(SQLite(), 'soccer.sqlite')
dbListTables(input)

# Generate League
dbGetQuery(input, 'SELECT * FROM Country', n = 5)
dbGetQuery(input, 'SELECT * FROM League', n = 5)
dbWriteTable(output, 'League', overwrite = T, dbGetQuery(input, '
SELECT L.id AS leagueID, L.name AS leagueName, C.name AS country
FROM League L, Country C
WHERE L.country_id = C.id
ORDER BY leagueID
'))


# Generate Team
dbGetQuery(input, 'SELECT * FROM Team', n = 5)
dbWriteTable(output, 'Team', overwrite = T, dbGetQuery(input, '
SELECT DISTINCT team_api_id AS teamID, team_long_name AS teamName,
                team_short_name AS teamShortName, league_id AS leagueID
FROM Team T, Match M
WHERE T.team_api_id = M.home_team_api_id
ORDER BY teamID
'))


# Generate TeamInfo
dbGetQuery(input, 'SELECT * FROM Team_Attributes', n = 5)
teamInfo <- dbGetQuery(input, '
SELECT *
FROM Team_Attributes
GROUP BY team_api_id
HAVING MAX(date)
') %>%
    select(-c(id, team_fifa_api_id)) %>%
    mutate(date = getDate(date))
dbWriteTable(output, 'TeamInfo', overwrite = T, teamInfo)
dbExecute(output, '
ALTER TABLE TeamInfo
RENAME COLUMN team_api_id TO teamID
')
dbExecute(output, '
ALTER TABLE TeamInfo
RENAME COLUMN date TO teamInfoDate
')


# Generate Player
dbGetQuery(input, 'SELECT * FROM Player', n = 5)
query <- '
SELECT DISTINCT home_player_%d AS playerID, season, home_team_api_id AS teamID
FROM Match
WHERE playerID IS NOT NULL
UNION
SELECT DISTINCT away_player_%d AS playerID, season, away_team_api_id AS teamID
FROM Match
WHERE playerID IS NOT NULL
'
player <- dbGetQuery(input, paste0('
SELECT player_api_id AS playerID, player_name AS playerName, birthday, height, weight, teamID
FROM Player NATURAL JOIN (
SELECT playerID AS player_api_id, teamID
FROM (',
do.call(paste, c(lapply(1:11, function(k) {
    sprintf(query, k, k)
}), sep = 'UNION')), ')
GROUP BY playerID
HAVING MAX(season)
)
ORDER BY playerID
')) %>%
    mutate(birthday = getDate(birthday))
dbWriteTable(output, 'Player', overwrite = T, player)


# Generate PlayerInfo
dbGetQuery(input, 'SELECT * FROM Player_Attributes', n = 5)
playerInfo <- dbGetQuery(input, '
SELECT *
FROM Player_Attributes
GROUP BY player_api_id
HAVING MAX(date)
') %>%
    select(-c(id, player_fifa_api_id)) %>%
    mutate(date = getDate(date))
dbWriteTable(output, 'PlayerInfo', overwrite = T, playerInfo)
dbExecute(output, '
ALTER TABLE PlayerInfo
RENAME COLUMN player_api_id TO playerID
')
dbExecute(output, '
ALTER TABLE PlayerInfo
RENAME COLUMN date TO playerInfoDate
')


# Generate Match
dbGetQuery(input, 'SELECT * FROM Match', n = 5)
match <- dbGetQuery(input, paste('
SELECT match_api_id AS matchID, season, date, stage,
       home_team_api_id AS homeTeamID, away_team_api_id AS awayTeamID,
       home_team_goal AS homeTeamGoal, away_team_goal AS awayTeamGoal,',
paste0('home_player_', 1:11, ' AS homePlayerID_', 1:11, collapse = ', '), ', ',
paste0('away_player_', 1:11, ' AS awayPlayerID_', 1:11, collapse = ', '), '
FROM Match
ORDER BY matchID
')) %>%
    mutate(date = getDate(date))
dbWriteTable(output, 'Match', overwrite = T, match)


# Prepare data for prediction
dbGetQuery(input, 'SELECT * FROM Match', n = 5)
dbGetQuery(input, 'SELECT * FROM Team_Attributes', n = 5)
home <- dbGetQuery(input, '
SELECT match_api_id as matchID, TA.*
FROM Match M
     LEFT OUTER JOIN Team_Attributes TA ON M.home_team_api_id = TA.team_api_id
GROUP BY matchID
HAVING MIN(ABS(JULIANDAY(M.date) - JULIANDAY(TA.date)))
') %>%
    select(-c(id, team_fifa_api_id, team_api_id, date))
m <- ncol(home)
colnames(home)[2:m] <- paste0('x', 1:(m - 1))
away <- dbGetQuery(input, '
SELECT match_api_id as matchID, TA.*
FROM Match M
     LEFT OUTER JOIN Team_Attributes TA ON M.away_team_api_id = TA.team_api_id
GROUP BY matchID
HAVING MIN(ABS(JULIANDAY(M.date) - JULIANDAY(TA.date)))
') %>%
    select(-c(id, team_fifa_api_id, team_api_id, date))
m <- ncol(away)
colnames(away)[2:m] <- paste0('y', 1:(m - 1))
match <- dbGetQuery(input, '
SELECT match_api_id AS matchID, home_team_goal AS homeTeamGoal, away_team_goal AS awayTeamGoal
FROM Match
') %>%
    left_join(home, by = 'matchID') %>%
    left_join(away, by = 'matchID') %>%
    select(-matchID)
m <- ncol(match)
imputeMatch <- imputeMissings::compute(match[, 3:m])
match <- cbind(match[, 1:2], impute(match[, 3:m], imputeMatch))
homeGoal <- randomForest(homeTeamGoal ~ ., select(match, -awayTeamGoal), ntree = 20)
awayGoal <- randomForest(awayTeamGoal ~ ., select(match, -homeTeamGoal), ntree = 20)

save(list = c('imputeMatch', 'homeGoal', 'awayGoal'), file = 'predict.RData')
dbDisconnect(input)
dbDisconnect(output)
