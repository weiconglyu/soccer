library(tidyverse)

getTeam <- function(leagueID) {
    with(dbGetQuery(con, paste('SELECT *
                                FROM Team',
                               if (leagueID != -1) paste0('WHERE leagueID = ', leagueID),
                               'ORDER BY teamName, teamShortName')),
         as.list(setNames(teamID, paste0(teamName, ' (', teamShortName, ')'))))
}

shinyServer(function(input, output, session) {
    view.table <- NULL
    match.table <- reactiveVal(NULL)
    
    observeEvent(input$`view-table_cell_edit`, {
        info <- input$`view-table_cell_edit`
        view.table.name <- colnames(view.table)
        dbExecute(con, paste('UPDATE', input$`view-table-select`,
                             'SET', view.table.name[info$col + 1], paste0('= "', info$value, '"'),
                             'WHERE', view.table.name[1], '=', view.table[info$row, 1]))
        view.table <<- editData(view.table, info, 'view-table', rownames = F)
        
        league <<- with(dbGetQuery(con, 'SELECT leagueID, leagueName
                                         FROM League
                                         ORDER BY leagueName'),
                        as.list(setNames(leagueID, leagueName)))
        updateSelectInput(session, 'team-info-select', choices = addAll(league),
                          selected = input$`team-info-select`)
        updateSelectizeInput(session, 'player-info-select-league', choices = league,
                             selected = input$`player-info-select-league`)
        updateSelectizeInput(session, 'player-info-select-team',
                             choices = addAll(getTeam(input$`player-info-select-league`), T),
                             selected = input$`player-info-select-team`)
        updateSelectizeInput(session, 'match-info-home-league', choices = addAll(league),
                             selected = input$`match-info-home-league`)
        updateSelectizeInput(session, 'match-info-home-team',
                             choices = getTeam(input$`match-info-home-league`),
                             selected = input$`match-info-home-team`)
        updateSelectizeInput(session, 'match-info-away-league', choices = addAll(league),
                             selected = input$`match-info-away-league`)
        updateSelectizeInput(session, 'match-info-away-team',
                             choices = getTeam(input$`match-info-away-league`),
                             selected = input$`match-info-away-team`)
    })
    
    output$`view-table` <- renderDT({
        DT <- datatable({
            view.table <<- dbGetQuery(con, paste('SELECT *
                                                  FROM', input$`view-table-select`))
        }, rownames = F, selection = 'none', editable = T,# extensions = c('Editor', 'Buttons'),
        options = list(lengthMenu = c(5, 10, 15, 30, 50), pageLength = 15,
                       scrollX = T, scrollY = T, scrollCollapse = T),
                       # dom = 'lf<"datatable-buttons"B>rtip')
                       # buttons = c('copy', 'CSV'))
        )
        if (input$`view-table-select` == 'Player')
            DT %>% formatRound('height', 2, mark = '')
        else
            DT
    })
    
    output$`team-info` <- renderDT({
        input$`view-table_cell_edit`
        leagueID <- input$`team-info-select`
        dbGetQuery(con, paste('SELECT *
                               FROM Team
                                    NATURAL LEFT OUTER JOIN League
                                    NATURAL LEFT OUTER JOIN TeamInfo',
                              if (leagueID != -1) paste0('WHERE leagueID = ', leagueID),
                              'ORDER BY TeamID'))
    }, rownames = F, selection = 'none', server = F, extensions = c('Select', 'Buttons'),
    options = list(lengthMenu = c(5, 10, 15, 30, 50), pageLength = 15, scrollX = T, scrollY = T, scrollCollapse = T,
                   select = 'multi', dom = 'lf<"datatable-buttons"B>rtip',
                   buttons = list('selectAll', 'selectNone',
                                  list(extend = 'copy', text = 'Copy to clipboard', title = NULL),
                                  list(extend = 'csv', text = 'Save as CSV', title = 'export')))
    )
    
    observeEvent(input$`player-info-select-league`, {
        updateSelectizeInput(session, 'player-info-select-team',
                             #choices = getTeam(input$`player-info-select-league`))
                             choices = addAll(getTeam(input$`player-info-select-league`), T))
    })
    
    output$`player-info` <- renderDT(datatable({
        input$`view-table_cell_edit`
        leagueID <- input$`player-info-select-league`
        teamID <- input$`player-info-select-team`
        dbGetQuery(con, paste('SELECT *
                               FROM Player
                                    NATURAL LEFT OUTER JOIN Team
                                    NATURAL LEFT OUTER JOIN League
                                    NATURAL LEFT OUTER JOIN PlayerInfo',
                               if (teamID != -1)
                                   paste0('WHERE teamID = ', teamID)
                               else if (leagueID != -1)
                                   paste0('WHERE leagueID = ', leagueID),
                               'ORDER BY playerID'))
    }, rownames = F, selection = 'none', extensions = c('Select', 'Buttons'),
    options = list(lengthMenu = c(5, 10, 15, 30, 50), pageLength = 15, scrollX = T, scrollY = T, scrollCollapse = T,
                   select = 'multi', dom = 'lf<"datatable-buttons"B>rtip',
                   buttons = list('selectAll', 'selectNone',
                                  list(extend = 'copy', text = 'Copy to clipboard', title = NULL),
                                  list(extend = 'csv', text = 'Save as CSV', title = 'export')))
    ) %>% formatRound('height', 2, mark = ''), server = F)
    
    observeEvent(input$`match-info-home-league`, {
        teams <- getTeam(input$`match-info-home-league`)
        team <- input$`match-info-home-team` 
        updateSelectizeInput(session, 'match-info-home-team', choices = teams,
                             selected = if (team %in% teams) team else teams[[1]])
    })
    
    observeEvent(input$`match-info-away-league`, {
        teams <- getTeam(input$`match-info-away-league`)
        team <- input$`match-info-away-team` 
        updateSelectizeInput(session, 'match-info-away-team', choices = teams,
                             selected = if (team %in% teams) team else teams[[1]])
    })
    
    observeEvent(input$`match-info-swap`, {
        league.home <- input$`match-info-home-league`
        league.away <- input$`match-info-away-league`
        team.home <- input$`match-info-home-team`
        team.away <- input$`match-info-away-team`
        
        updateSelectizeInput(session, 'match-info-home-league', selected = league.away)
        updateSelectizeInput(session, 'match-info-away-league', selected = league.home)
        updateSelectizeInput(session, 'match-info-home-team', choices = getTeam(league.away), selected = team.away)
        updateSelectizeInput(session, 'match-info-away-team', choices = getTeam(league.home), selected = team.home)
    })
    
    output$`player-info` <- renderDT({
        input$`view-table_cell_edit`
        leagueID <- input$`player-info-select-league`
        teamID <- input$`player-info-select-team`
        dbGetQuery(con, paste('SELECT *
                               FROM Player
                                    NATURAL LEFT OUTER JOIN Team
                                    NATURAL LEFT OUTER JOIN League
                                    NATURAL LEFT OUTER JOIN PlayerInfo',
                              if (teamID != -1)
                                  paste0('WHERE teamID = ', teamID)
                              else if (leagueID != -1)
                                  paste0('WHERE leagueID = ', leagueID),
                              'ORDER BY playerID'))
    }, rownames = F, selection = 'none', extensions = c('Select', 'Buttons'), server = F,
    options = list(lengthMenu = c(5, 10, 15, 30, 50), pageLength = 15, scrollX = T, scrollY = T, scrollCollapse = T,
                   select = 'multi', dom = 'lf<"datatable-buttons"B>rtip',
                   buttons = list('selectAll', 'selectNone',
                                  list(extend = 'copy', text = 'Copy to clipboard', title = NULL),
                                  list(extend = 'csv', text = 'Save as CSV', title = 'export')))
    )
    
    output$`match-info` <- renderDT({
        input$`view-table_cell_edit`
        homeID <- input$`match-info-home-team`
        awayID <- input$`match-info-away-team`
        query <- 'NATURAL LEFT OUTER JOIN (
                  SELECT PlayerID AS %sPlayerID_%d, playerName AS %sPlayerName_%d
                  FROM Player
                  )'
        match.table(dbGetQuery(con, paste('SELECT *
                                           FROM Match',
                                          do.call(paste, c(lapply(1:11, function(k) {
                                              sprintf(query, 'home', k, 'home', k)
                                          }))),
                                          do.call(paste, c(lapply(1:11, function(k) {
                                              sprintf(query, 'away', k, 'away', k)
                                          }))),
                                          'WHERE homeTeamID = ', homeID, 'AND awayTeamID = ', awayID,
                                          'ORDER BY matchID')))
        match.table()
    }, rownames = F, selection = 'none', extensions = c('Select', 'Buttons'), server = F,
    options = list(lengthMenu = c(1, 2, 4, 8, 10), pageLength = 4, scrollX = T, scrollY = T, scrollCollapse = T,
                   select = 'multi', dom = 'lf<"datatable-buttons"B>rtip',
                   buttons = list('selectAll', 'selectNone',
                                  list(extend = 'copy', text = 'Copy to clipboard', title = NULL),
                                  list(extend = 'csv', text = 'Save as CSV', title = 'export')))
    )
    
    output$`match-plot` <- renderPlot({
        home <- dbGetQuery(con, paste('SELECT *
                                       FROM TeamInfo
                                       WHERE TeamID = ', input$`match-info-home-team`)) %>%
            select(-c(teamID, teamInfoDate))
        m <- ncol(home)
        colnames(home)[1:m] <- paste0('x', 1:m)
        if (nrow(home) == 0)
            home[1, ] <- rep(NA, m)
        
        away <- dbGetQuery(con, paste('SELECT *
                                       FROM TeamInfo
                                       WHERE TeamID = ', input$`match-info-away-team`)) %>%
            select(-c(teamID, teamInfoDate))
        m <- ncol(away)
        colnames(away)[1:m] <- paste0('y', 1:m)
        if (nrow(away) == 0)
            away[1, ] <- rep(NA, m)
        
        match <- impute(cbind(home, away), imputeMatch)
        home <- predict(homeGoal, match)
        away <- predict(awayGoal, match)
        
        goal <- match.table() %>%
            select(Season = season, Home = homeTeamGoal, Away = awayTeamGoal) %>%
            rbind(list(Season = 'Prediction', Home = home, Away = away)) %>%
            pivot_longer(Home:Away, names_to = 'Team', values_to = 'Goal') %>%
            mutate(Team = factor(Team, levels = c('Home', 'Away')))
        n <- nrow(goal)
        g <- ggplot(goal, aes(Season, Goal, color = Team, group = Team)) +
            geom_point(shape = 1, stroke = 2, size = 2.5) +
            geom_text(aes(label = c(rep(NA, nrow(goal) - 2), round(c(home, away), 2))),
                      size = 5.5, family = 'Palatino', fontface = 'bold', hjust = -0.4, show.legend = F) +
            theme(text = element_text(size = 16, family = 'Palatino'))
        if (n >= 4)
            g +
            geom_line(data = goal[1:(n - 2), ], size = 1.2) +
            geom_line(data = goal[(n - 3):n, ], size = 1.2, linetype = 'longdash')
        else
            g
    })
    
    runjs('$("body").resize()')
})
