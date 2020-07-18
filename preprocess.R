source('global.R')

getDate <- function(date) {
    sapply(strsplit(date, ' '), function(v) { v[1] })
}

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
                league_id AS leagueID, team_short_name AS teamShortName
FROM Team T, Match M
WHERE T.team_api_id = M.home_team_api_id
ORDER BY teamID
'))


# Generate TeamInfo
dbGetQuery(input, 'SELECT * FROM Team_Attributes', n = 5)
teamInfo <- dbGetQuery(input, '
SELECT *
FROM Team_Attributes
')[, c(-1, -2, -4)]
dbWriteTable(output, 'TeamInfo', overwrite = T, teamInfo)
dbExecute(output, '
ALTER TABLE TeamInfo
RENAME COLUMN team_api_id TO teamID
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
SELECT player_api_id AS playerID, player_name AS playerName, teamID, birthday, height, weight
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
'))
player$birthday <- getDate(player$birthday)
dbWriteTable(output, 'Player', overwrite = T, player)


# Generate PlayerInfo
dbGetQuery(input, 'SELECT * FROM Player_Attributes', n = 5)
playerInfo <- dbGetQuery(input, '
SELECT *
FROM Player_Attributes
GROUP BY player_api_id
HAVING MAX(date)
')[, c(-1, -2)]
playerInfo$date <- getDate(playerInfo$date)
dbWriteTable(output, 'PlayerInfo', overwrite = T, playerInfo)
dbExecute(output, '
ALTER TABLE PlayerInfo
RENAME COLUMN player_api_id TO playerID
')


# Generate play
dbGetQuery(input, 'SELECT * FROM Match', n = 5)
query <- '
SELECT match_api_id AS matchID, home_team_api_id AS teamID, home_player_%d AS playerID
FROM MATCH
WHERE playerID IS NOT NULL
UNION
SELECT match_api_id AS matchID, away_team_api_id AS teamID, away_player_%d AS playerID
FROM MATCH
WHERE playerID IS NOT NULL
'
dbWriteTable(output, 'play', overwrite = T, dbGetQuery(input, paste0('
SELECT *
FROM (',
do.call(paste, c(lapply(1:11, function(k) {
    sprintf(query, k, k)
}), sep = 'UNION')), ')'
)))


# Generate Match
dbGetQuery(input, 'SELECT * FROM Match', n = 5)
match <- dbGetQuery(input, paste0('
SELECT match_api_id AS matchID, season, date, home_team_api_id AS homeTeamID, away_team_api_id AS awayTeamID,
       home_team_goal AS homeTeamGoal, away_team_goal AS awayTeamGoal
FROM Match
'))
match$date <- getDate(match$date)
dbWriteTable(output, 'Match', overwrite = T, match)

dbDisconnect(input)
dbDisconnect(output)
