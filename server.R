getTeam <- function(leagueID) {
    with(dbGetQuery(con, paste('SELECT *
                                FROM Team',
                               if (leagueID != -1) paste0('WHERE leagueID = ', leagueID),
                               'ORDER BY teamName, teamShortName')),
         as.list(setNames(teamID, paste0(teamName, ' (', teamShortName, ')'))))
}

shinyServer(function(input, output, session) {
    view.table <- NULL
    
    output$`view-table` <- renderDT({
        view.table <<- dbGetQuery(con, paste('SELECT *
                                         FROM', input$`view-table-select`))
    }, rownames = F, selection = 'none', editable = T,
    options = list(lengthMenu = c(5, 10, 15, 30, 50), pageLength = 15,
                   scrollX = T, scrollY = T, scrollCollapse = T)
    )
    
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
        updateSelectInput(session, 'team-info-select', choices = c(`(All)` = -1, league),
                          selected = input$`team-info-select`)
        updateSelectizeInput(session, 'player-info-select-league', choices = c(`(All)` = -1, league),
                             selected = input$`player-info-select-league`)
        updateSelectizeInput(session, 'player-info-select-team', choices = c(`(All)` = -1, getTeam(input$`player-info-select-league`)),
                             selected = input$`player-info-select-team`)
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
    }, rownames = F, selection = 'none',
    options = list(lengthMenu = c(5, 10, 15, 30, 50), pageLength = 15,
                   scrollX = T, scrollY = T, scrollCollapse = T)
    )
    
    observeEvent(input$`player-info-select-league`, {
        updateSelectizeInput(session, 'player-info-select-team',
                             choices = c(`(All)` = -1, getTeam(input$`player-info-select-league`)))
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
    }, rownames = F, selection = 'none',
    options = list(lengthMenu = c(5, 10, 15, 30, 50), pageLength = 15,
                   scrollX = T, scrollY = T, scrollCollapse = T)
    )
})
