library(shinydashboard)

dashboardPage(
    title = 'Visualization of European Professional Football and Match Result Prediction',
    dashboardHeader(title = 'European Soccer'),
    dashboardSidebar(
        hr(),
        sidebarMenu(id = 'tabs',
                    menuItem('View Tables', tabName = 'view', icon = icon('table'), selected = T),
                    menuItem('Team Information', tabName = 'team', icon = icon('users'))),
        hr(),
        conditionalPanel('input.tabs == "view"',
                         fluidRow(
                             column(12,
                                    selectInput('view-table-select', 'Table:', c('League', 'Team', 'Player'),
                                                selectize = F, size = 3)
                             )
                         )
        ),
        conditionalPanel('input.tabs == "team"',
                         fluidRow(
                             column(12,
                                    selectInput('team-info-select', 'League:', c(`(All)` = -1, league.list),
                                                selectize = F, size = nrow(league) + 1)
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
            )
        )
    ),
)
