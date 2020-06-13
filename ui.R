library(DT)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)

library(dplyr)
library(ggplot2)
library(plotly)

ui <- navbarPage(
  theme = shinytheme('flatly'),
  title = "Step-up Challenge 2020",
  id = 'navbar',
  collapsible = TRUE,
  tabPanel(
    title = 'Login',
    uiOutput('user_ui')
  ),
  navbarMenu(
    title = 'My Data',
    tabPanel(
      title = 'Data Entry',
      dateInput(
        'date',
        'Date of Activity',
        value = Sys.Date(),
        format = 'DD, d MM',
        min = START_DATE,
        max = END_DATE
      ),
      tags$div(
        dateButton('Back', 'angle-left'),
        dateButton('Forwards', 'angle-right'),
        style = 'margin-bottom: 2px'
      ),
      tags$div(
        dateButton('Yesterday', 'caret-left'),
        dateButton('Today', 'caret-down'),
        style = 'margin-bottom: 15px'
      ),
      numericInput(
        'num_steps',
        'Number of Steps',
        value = 0,
        min = 0,
        width = '100%'
      ),
      numericInput(
        'num_cycle',
        'Number of Minutes Cycling',
        value = 0,
        min = 0,
        width = '100%'
      ),
      numericInput(
        'num_swim',
        'Number of Minutes Swimming',
        value = 0,
        min = 0,
        width = '100%'
      ),
      actionButton(
        'upload',
        "Upload",
        icon = icon('upload'),
        width = '100%'
      )
    ),
    tabPanel(
      title = 'History',
      tabsetPanel(
        id = 'history_tab',
        tabPanel(
          title = "Plot",
          plotOutput('history_plot')
        ),
        tabPanel(
          title = "Table",
          dataTableOutput('history_table')
        )
      )
    )
  ),
  navbarMenu(
    title = 'Comparisions',
    tabPanel(
      title = 'Individual Comparison',
      sidebarLayout(
        sidebarPanel(
          uiOutput('user_picker_ui'),
          conditionalPanel(
            condition = 'input.individual_tab == "Time Series"',
            tags$hr(),
            checkboxInput(
              'individual_cummulative_steps',
              'Cummulative Steps',
              value = FALSE
            )
          ),
        ),
        mainPanel(
          tabsetPanel(
            id = 'individual_tab',
            tabPanel(
              title = "Time Series",
              plotlyOutput('individual_comparison_time_series')
            ),
            tabPanel(
              "Leaderboard",
              dataTableOutput('individual_comparison_leaderboard')
            )
          )
        )
      )
    ),
    tabPanel(
      title = 'Team Comparison',
      sidebarLayout(
        sidebarPanel(
          pickerInput(
            'teams',
            "Teams",
            teams,
            selected = teams,
            multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE
            )
          ),
          conditionalPanel(
            condition = 'input.teams_tab == "Time Series"',
            tags$hr(),
            checkboxInput(
              'team_cummulative_steps',
              'Cummulative Steps',
              value = FALSE
            )
          ),
        ),
        mainPanel(
          tabsetPanel(
            id = 'teams_tab',
            tabPanel(
              title = "Time Series",
              plotlyOutput('team_comparison_time_series')
            ),
            tabPanel(
              "Leaderboard",
              dataTableOutput('team_comparison_leaderboard')
            )
          )
        )
      )
    )
  ),
  tabPanel(
    title = 'Winners'
  ),
  # Stylesheets and scripts
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  useShinyjs(),
  extendShinyjs(text = cookie_js),
  tags$head(tags$script(src = "js/js.cookie.js"))
)
