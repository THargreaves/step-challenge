library(DT)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)

library(dplyr)
library(plotly)

ui <- navbarPage(
  theme = shinytheme('flatly'),
  title = "Step-up Challenge 2020",
  id = 'navbar',
  collapsible = TRUE,
  tabPanel(
    #### ACCOUNT ####
    title = 'Account',
    uiOutput('account_ui')
  ),
  navbarMenu(
    title = 'My Data',
    tabPanel(
      #### DATA ENTRY ####
      title = 'Data Entry',
      dateInput(
        'activity_date',
        'Date of Activity',
        value = max(START_DATE, Sys.Date()),
        format = 'DD, d MM',
        min = START_DATE,
        max = END_DATE,
        width = '100%'
      ),
      tags$div(
        actionButton('back',
                     "Back",
                     icon('angle-left'),
                     class = 'date-button'),
        actionButton('forwards',
                     "Forwards",
                     icon('angle-right'),
                     class = 'date-button'),
        class = 'date-button-top-row'
      ),
      tags$div(
        actionButton('yesterday',
                     "Yesterday",
                     icon('caret-left'),
                     class = 'date-button'),
        actionButton('today',
                     "Today",
                     icon('caret-down'),
                     class = 'date-button'),
        class = 'date-button-bottom-row'
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
      uiOutput(
        'feature_toggle_ui'
      ),
      actionButton(
        'upload',
        "Upload",
        icon = icon('upload'),
        width = '100%'
      )
    ),
    tabPanel(
      #### HISTORY ####
      title = 'History',
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            condition = 'input.history_tab == "Plot"',
            checkboxInput(
              'apply_feature_multiplier',
              'Apply Feature Multiplier',
              value = TRUE
            ),
            checkboxInput(
              'show_average',
              'Show Daily Average',
              value = FALSE
            )
          ),
          conditionalPanel(
            condition = 'input.history_tab == "Table"',
            checkboxInput(
              'colour_rows',
              'Colour Rows',
              value = FALSE
            ),
          )
        ),
        mainPanel(
          tabsetPanel(
            id = 'history_tab',
            tabPanel(
              title = "Plot",
              plotlyOutput('history_plot', height = '600px')
            ),
            tabPanel(
              title = "Table",
              dataTableOutput('history_table')
            )
          )
        )
      )
    )
  ),
  navbarMenu(
    title = 'Comparisions',
    tabPanel(
      #### INDIVIDUAL COMPARISON ####
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
              plotlyOutput('individual_comparison_time_series', height = '600px')
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
      #### TEAM COMPARISON ####
      title = 'Team Comparison',
      sidebarLayout(
        sidebarPanel(
          uiOutput('teams_selector_ui'),
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
              plotlyOutput('team_comparison_time_series', height = '600px')
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
    #### WINNERS ####
    title = 'Winners',
    sidebarLayout(
      sidebarPanel(
        uiOutput('week_selector_ui')
      ),
      mainPanel(
        tabsetPanel(
          id = 'winners_tab',
          tabPanel(
            title = "Plot",
            textOutput('winners_text'),
            plotlyOutput('winners_plot', height = '600px')
          ),
          tabPanel(
            "Table",
            dataTableOutput('winners_table')
          )
        )
      )
    )
  ),
  # Stylesheets and scripts
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  useShinyjs(),
  extendShinyjs(text = cookie_js),
  tags$head(tags$script(src = "js/js.cookie.js"))
)
