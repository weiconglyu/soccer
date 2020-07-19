library(shinydashboard)

league <- with(dbGetQuery(con, 'SELECT leagueID, leagueName
                                FROM League
                                ORDER BY leagueName'),
               as.list(setNames(leagueID, leagueName)))

dashboardPage(
    title = 'Visualization of European Professional Football and Match Result Prediction',
    dashboardHeader(title = 'European Soccer'),
    dashboardSidebar(
        hr(),
        sidebarMenu(id = 'tabs',
                    menuItem('View/Edit Raw Tables', tabName = 'view', icon = icon('table'), selected = T),
                    menuItem('Team Information', tabName = 'team', icon = icon('users')),
                    menuItem('Player Information', tabName = 'player', icon = icon('running'))),
        hr(),
        conditionalPanel('input.tabs == "view"',
                         fluidRow(
                             column(12,
                                    selectInput('view-table-select', 'Table:',
                                                c('League', 'Team', 'TeamInfo', 'Player', 'PlayerInfo', 'Match'),
                                                selectize = F, size = 6)
                             )
                         )
        ),
        conditionalPanel('input.tabs == "team"',
                         fluidRow(
                             column(12,
                                    selectInput('team-info-select', 'League:', c(`(All)` = -1, league), selected = -1,
                                                selectize = F, size = length(league) + 1)
                             )
                         )
        ),
        conditionalPanel('input.tabs == "player"',
                         fluidRow(
                             column(12,
                                    selectizeInput('player-info-select-league', 'League:', c(`(All)` = -1, league), selected = -1),
                                    selectizeInput('player-info-select-team', 'Team:', NULL)
                             )
                         )
        )
    ),
    
    dashboardBody(
        tags$head(
            tags$link(rel = 'stylesheet', type = 'text/css', href = 'ui.css')
        ),
        tabItems(
            tabItem('view',
                    fluidPage(
                        DTOutput('view-table')
                    )
            ),
            tabItem('team',
                    fluidPage(
                        DTOutput('team-info')
                    )
            ),
            tabItem('player',
                    fluidPage(
                        DTOutput('player-info')
                    )
            )
        )
    ),
)
