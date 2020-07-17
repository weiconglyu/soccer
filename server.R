library(shiny)
library(DT)
library(RSQLite)
library(DBI)

shinyServer(function(input, output, session) {
    con <- dbConnect(SQLite(), 'database.sqlite')
    # updateSelectizeInput(session, 'view-table-select', choices = dbListTables(con))
    
    output$`view-table` <- renderDT({
        table <- input$`view-table-select`
        query <- if (table == 'League')
            'SELECT L.id, L.name, C.name AS country
             FROM League L, Country C
             WHERE L.country_id = C.id'
        else
            paste0('SELECT * FROM ', table)
        res <- dbSendQuery(con, query)
        result <- dbFetch(res)
        dbClearResult(res)
        result
    }, options = list(lengthMenu = c(5, 10, 15, 30, 50), pageLength = 15),
       rownames = F, selection = 'none')
})
