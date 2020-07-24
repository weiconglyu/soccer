library(tidyverse)
library(randomForest)
library(imputeMissings)

getTeam <- function(leagueID) {
    tryCatch({
        teams <- with(dbGetQuery(con, paste('SELECT *
                                             FROM Team',
                                            if (leagueID != -1) paste0('WHERE leagueID = ', leagueID),
                                            'ORDER BY teamName, teamShortName')),
                      as.list(setNames(teamID, ifelse(teamShortName == '', teamName, paste0(teamName, ' (', teamShortName, ')')))))
        if (length(teams) == 0) list(`(No Team)` = -2) else teams
    }, error = function(e) {
        list()
    })
}

shinyServer(function(input, output, session) {
    tables <- reactiveValues(view = NULL, match = NULL)
    
    updateSelect <- function(inputId, choices) {
        selected <- if (input[[inputId]] %in% choices) input[[inputId]] else choices[1]
        updateSelectInput(session, inputId, choices = choices, selected = selected)
    }
    
    updateSelectize <- function(inputId, choices) {
        selected <- if (input[[inputId]] %in% choices) input[[inputId]] else choices[1]
        updateSelectizeInput(session, inputId, choices = choices, selected = selected)
    }
    
    observeEvent(input$`view-table_cell_edit`, {
        info <- input$`view-table_cell_edit`
        names <- colnames(tables$view)
        dbExecute(con, paste('UPDATE', input$`view-table-select`,
                             'SET', names[info$col + 1], paste0('= "', info$value, '"'),
                             'WHERE', names[1], '=', tables$view[info$row, 1]))
        tables$view <- editData(tables$view, info, 'view-table', rownames = F, clearSelection = 'none')
    })
    
    observeEvent(tables$view, {
        league <<- with(dbGetQuery(con, 'SELECT leagueID, leagueName
                                         FROM League
                                         ORDER BY leagueName'),
                        as.list(setNames(leagueID, leagueName)))
        if (length(league) == 0)
            league <<- list(`(No League)` = -2)
        updateSelect('team-info-select', addAll(league))
        updateSelectize('player-info-select-league', league)
        updateSelectize('player-info-select-team', addAll(getTeam(input$`player-info-select-league`), T))
        updateSelectize('match-info-home-league', addAll(league))
        updateSelectize('match-info-home-team', getTeam(input$`match-info-home-league`))
        updateSelectize('match-info-away-league', addAll(league))
        updateSelectize('match-info-away-team', getTeam(input$`match-info-away-league`))
    })
    
    output$`view-table` <- renderDT({
        input$`view-table-select`
        isolate({
            tables$view <- dbGetQuery(con, paste('SELECT *
                                                  FROM', input$`view-table-select`))
        })
    }, rownames = F, selection = 'single', editable = 'cell', extensions = 'Buttons',
    options = list(lengthMenu = c(5, 10, 12, 30, 50), pageLength = 12, scrollX = T, scrollY = T, scrollCollapse = T,
                   searchHighlight = T, dom = 'lf<"datatable-buttons"B>rtip',
                   buttons = list(list(extend = 'collection', text = 'Append a row',
                                       action = JS('function() {
                                                        Shiny.setInputValue("view-table-append", true, {priority: "event"});
                                                    }')),
                                  list(extend = 'collection', text = 'Delete selected row',
                                       action = JS('function() {
                                                        Shiny.setInputValue("view-table-delete", true, {priority: "event"});
                                                    }')),
                                  list(extend = 'collection', text = 'Reset current table',
                                       action = JS('function() {
                                                        Shiny.setInputValue("view-table-reset", true, {priority: "event"});
                                                    }')))),
    callback = JS('$("#view-table .datatable-buttons button").get(1).classList.add("disabled");')
    )
    
    observeEvent(input$`view-table-append`, {
        ID <- max(tables$view[, 1]) + 1
        table <- input$`view-table-select`
        names <- colnames(tables$view)
        blank <- grep('name$', names, T)
        dbExecute(con, paste('INSERT INTO', table, '(', paste(names[c(1, blank)], collapse = ','), ')',
                             'VALUES (', paste(c(ID, rep('""', length(blank))), collapse = ','), ')'))
        n <- nrow(tables$view) + 1
        tables$view[n, ] <- NA
        tables$view[n, 1] <- ID
        tables$view[n, blank] <- ''
        proxy <- dataTableProxy('view-table')
        replaceData(proxy, tables$view, rownames = F, resetPaging = F)
        selectRows(proxy, n)
        # clearSearch(proxy)
        # selectPage(proxy, (nrow(tables$view) + input$`view-table_state`$length) %/% input$`view-table_state`$length)
        runjs('$("#view-table table").DataTable().page("last").draw("page");')
    })
    
    observeEvent(input$`view-table-delete`, {
        row <- input$`view-table_rows_selected`
        dbExecute(con, paste('DELETE FROM', input$`view-table-select`,
                             'WHERE', colnames(tables$view)[1], '=', tables$view[row, 1]))
        tables$view <- tables$view[-row, , drop = F]
        replaceData(dataTableProxy('view-table'), tables$view, rownames = F, resetPaging = F)
    })
    
    observeEvent(input$`view-table-reset`, {
        backup <- dbConnect(SQLite(), 'backup.sqlite')
        tables$view <- dbGetQuery(backup, paste('SELECT *
                                                 FROM', input$`view-table-select`))
        dbDisconnect(backup)
        dbWriteTable(con, input$`view-table-select`, overwrite = T, tables$view)
        replaceData(dataTableProxy('view-table'), tables$view, rownames = F)
    })
    
    observe({
        input$`view-table-select`
        row <- input$`view-table_rows_selected`
        runjs(paste0('$("#view-table .datatable-buttons button").get(1).classList.',
                     if (length(row) == 0) 'add' else 'remove',
                     '("disabled");'))
    })
    
    output$`team-info` <- renderDT({
        tables$view
        leagueID <- input$`team-info-select`
        dbGetQuery(con, paste('SELECT *
                               FROM Team
                                    NATURAL LEFT OUTER JOIN League
                                    NATURAL LEFT OUTER JOIN TeamInfo',
                              if (leagueID != -1) paste0('WHERE leagueID = ', leagueID),
                              'ORDER BY TeamID'))
    }, rownames = F, selection = 'none', server = F, extensions = c('Select', 'Buttons'),
    options = list(lengthMenu = c(5, 10, 12, 30, 50), pageLength = 12, scrollX = T, scrollY = T, scrollCollapse = T,
                   select = 'multi', searchHighlight = T, dom = 'lf<"datatable-buttons"B>rtip',
                   buttons = list('selectAll', 'selectNone',
                                  list(extend = 'copy', text = 'Copy to clipboard', title = NULL),
                                  list(extend = 'csv', text = 'Save as CSV', title = 'export')))
    )
    
    observeEvent(input$`player-info-select-league`, {
        updateSelectize('player-info-select-team', addAll(getTeam(input$`player-info-select-league`), T))
    })
    
    output$`player-info` <- renderDT({
        tables$view
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
    }, rownames = F, selection = 'none', server = F, extensions = c('Select', 'Buttons'), 
    options = list(lengthMenu = c(5, 10, 12, 30, 50), pageLength = 12, scrollX = T, scrollY = T, scrollCollapse = T,
                   select = 'multi', searchHighlight = T, dom = 'lf<"datatable-buttons"B>rtip',
                   buttons = list('selectAll', 'selectNone',
                                  list(extend = 'copy', text = 'Copy to clipboard', title = NULL),
                                  list(extend = 'csv', text = 'Save as CSV', title = 'export')))
    )
    
    observeEvent(input$`match-info-home-league`, {
        updateSelectize('match-info-home-team', getTeam(input$`match-info-home-league`))
    })
    
    observeEvent(input$`match-info-away-league`, {
        updateSelectize('match-info-away-team', getTeam(input$`match-info-away-league`))
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
    
    output$`match-info` <- renderDT({
        tables$view
        homeID <- input$`match-info-home-team`
        awayID <- input$`match-info-away-team`
        query <- 'NATURAL LEFT OUTER JOIN (
                  SELECT PlayerID AS %sPlayerID_%d, playerName AS %sPlayerName_%d
                  FROM Player
                  )'
        tables$match <- dbGetQuery(con, paste('SELECT *
                                               FROM Match',
                                              do.call(paste, c(lapply(1:11, function(k) {
                                                  sprintf(query, 'home', k, 'home', k)
                                              }))),
                                              do.call(paste, c(lapply(1:11, function(k) {
                                                  sprintf(query, 'away', k, 'away', k)
                                              }))),
                                              'WHERE homeTeamID = ', homeID, 'AND awayTeamID = ', awayID,
                                              'ORDER BY matchID'))
    }, rownames = F, selection = 'none', server = F, extensions = c('Select', 'Buttons'),
    options = list(lengthMenu = c(1, 2, 3, 5, 10), pageLength = 2, scrollX = T, scrollY = T, scrollCollapse = T,
                   select = 'multi', searchHighlight = T, dom = 'lf<"datatable-buttons"B>rtip',
                   buttons = list('selectAll', 'selectNone',
                                  list(extend = 'copy', text = 'Copy to clipboard', title = NULL),
                                  list(extend = 'csv', text = 'Save as CSV', title = 'export')))
    )
    
    output$`match-plot` <- renderPlot({
        tables$view
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
        
        goal <- tables$match %>%
            select(Season = season, Home = homeTeamGoal, Away = awayTeamGoal) %>%
            rbind(list(Season = 'Prediction', Home = home, Away = away)) %>%
            pivot_longer(Home:Away, names_to = 'Team', values_to = 'Goal') %>%
            mutate(Team = factor(Team, levels = c('Home', 'Away')))
        n <- nrow(goal)
        g <- ggplot(goal, aes(Season, Goal, color = Team, group = Team)) +
            geom_point(shape = 1, stroke = 2, size = 2.5) +
            geom_text(aes(label = c(rep('', nrow(goal) - 2), sprintf('%.2f', c(home, away)))),
                      size = 5.5, family = 'Palatino', fontface = 'bold', hjust = -0.4, show.legend = F) +
            theme(text = element_text(size = 16, family = 'Palatino'))
        if (n >= 4)
            g +
            geom_line(data = goal[1:(n - 2), ], size = 1.2) +
            geom_line(data = goal[(n - 3):n, ], size = 1.2, linetype = 'longdash')
        else
            g
    })
    
    runjs('$(document).ready(function() {
               $("body").resize();
          });')
})
