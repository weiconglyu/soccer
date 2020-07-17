library(shiny)
library(shinydashboard)
library(DT)

dashboardPage(
    title = 'Visualization of European Professional Football and Match Result Prediction',
    dashboardHeader(title = 'European Soccer'),
    dashboardSidebar(
        hr(),
        sidebarMenu(id = 'tabs',
                    menuItem('View Tables', tabName = 'view', icon = icon('table'), selected = T),
                    menuItem('Team Info', tabName = 'team', icon = icon('users'))),
        hr(),
        conditionalPanel('input.tabs == "view"',
                         fluidRow(
                             column(1),
                             column(10,
                                    selectizeInput('view-table-select', 'Table:', c('League', 'Team', 'Player'))
                             )
                         )
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem('view',
                fluidPage(
                    DTOutput('view-table')
                )
            )
        )
    ),
)
