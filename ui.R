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
                    menuItem('Raw Tables', tabName = 'view', icon = icon('table'), selected = T),
                    menuItem('Teams', tabName = 'team', icon = icon('users')),
                    menuItem('Players', tabName = 'player', icon = icon('running')),
                    menuItem('Matches', tabName = 'match', icon = icon('hourglass-half'))),
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
                                    selectInput('team-info-select', 'League:', addAll(league), selected = -1,
                                                selectize = F, size = length(league) + 1)
                             )
                         )
        ),
        conditionalPanel('input.tabs == "player"',
                         fluidRow(
                             column(12,
                                    selectizeInput('player-info-select-league', 'League:', league, selected = -1),
                                    selectizeInput('player-info-select-team', 'Team:', NULL)
                             )
                         )
        ),
        conditionalPanel('input.tabs == "match"',
                         fluidRow(
                             column(12,
                                    strong('Home Team:', style = 'margin-left: 10px'),
                                    span(class = 'nobottom', selectizeInput('match-info-home-league', NULL, addAll(league), selected = -1)),
                                    span(class = 'notop', selectizeInput('match-info-home-team', NULL, NULL)),
                                    #hr(class = 'sep'),
                                    strong('Away Team:', style = 'margin-left: 10px'),
                                    span(class = 'nobottom', selectizeInput('match-info-away-league', NULL, addAll(league), selected = -1)),
                                    span(class = 'notop', selectizeInput('match-info-away-team', NULL, NULL)),
                                    #hr(class = 'sep'),
                                    div(class = 'inline',
                                        actionButton('match-info-swap', 'Swap'),
                                        #actionButton('match-info-submit', 'Submit')
                                    )
                             )
                         ))
    ),
    
    dashboardBody(
        useShinyjs(),
        includeCSS('ui.css'),
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
            ),
            tabItem('match',
                    fluidPage(
                        DTOutput('match-info'),
                        p(),
                        plotOutput('match-plot')
                    )
            )
        )
    ),
)
