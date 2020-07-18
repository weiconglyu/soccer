shinyServer(function(input, output, session) {
    output$`view-table` <- renderDT({
        dbGetQuery(con, paste0('SELECT * FROM ', input$`view-table-select`))
    }, options = list(lengthMenu = c(5, 10, 15, 30, 50), pageLength = 15,
                      scrollX = T, scrollY = T, scrollCollapse = T),
       rownames = F, selection = 'none')
    
    output$`team-info` <- renderDT({
        leagueID <- input$`team-info-select`
        dbGetQuery(con, paste0('SELECT * FROM Team NATURAL JOIN TeamInfo',
                               if (leagueID == -1) '' else paste0(' WHERE leagueID = ', leagueID)))
    }, options = list(lengthMenu = c(5, 10, 15, 30, 50), pageLength = 15,
                      scrollX = T, scrollY = T, scrollCollapse = T),
    rownames = F, selection = 'none')
})
