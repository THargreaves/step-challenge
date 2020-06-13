library(DT)
library(shiny)
library(shinyjs)
library(shinyWidgets)

library(Cairo)
library(dplyr)
library(ggplot2)
library(magrittr)
library(plotly)
library(tidyr)

server <- function(input, output, session) {

  # Connect to database
  db_cfg <- fromJSON('db_config.json')
  conn <- dbConnect(
    MariaDB(),
    host = db_cfg$host,
    dbname = db_cfg$dbname,
    user = db_cfg$user,
    password = db_cfg$password
  )

  # Collect team names
  team_tbl <- tbl(conn, 'team') %>%
    select(team_id, team_name) %>%
    collect()
  teams <- team_tbl$team_id
  names(teams) <- team_tbl$team_name

  # Collect user names
  user_tbl <- tbl(conn, 'user') %>%
    transmute(user_id = user_id,
              name = str_c(first_name, last_name, sep = ' ')) %>%
    collect()
  users <- user_tbl$user_id
  names(users) <- user_tbl$name

  # App state reactive values
  state <- reactiveValues(
    user = NULL,
    wait = FALSE,
    new_id = TRUE,
    overwrite_id = NULL,
    cookie_id = NULL,
    submit = 0
  )

  # Login/Sign up UI
  output$user_ui <- renderUI({
    if (is.null(state$user)) {
      tagList(
        checkboxInput(
          'new_user',
          "I am a new user",
          value = FALSE
        ),
        uiOutput('login_ui')
      )
    } else {
      tagList(
        tags$p(tags$b(paste(
          "You are signed in as",
          state$user$first_name,
          state$user$last_name
        ))),
        actionButton(
          'sign_out',
          "Log Out",
          icon = icon('sign-out-alt'),
          width = '100%'
        )
      )
    }
  })

  output$login_ui <- renderUI({
    if (input$new_user) {
      tagList(
        textInput(
          'first_name',
          "First Name",
          width = '100%'
        ),
        textInput(
          'last_name',
          "Last Name",
          width = '100%'
        ),
        selectInput(
          'team',
          "Team",
          choices = c('', teams),
          width = '100%'
        ),
        actionButton(
          'sign_up',
          "Sign Up",
          icon = icon('user-plus'),
          width = '100%'
        )
      )
    } else {
      t <- tagList(
        numericInput(
          'user_id',
          "User ID",
          value = 0,
          min = 0,
          width = '100%'
        ),
        checkboxInput(
          'remember_me',
          "Remember Me (For 30 Days)",
          width = '100%'
        ),
        actionButton(
          'log_in',
          "Log In",
          icon = icon('sign-in-alt'),
          width = '100%'
        )
      )
      state$wait <- FALSE
      t
    }
  })

  observeEvent(input$sign_out, {
    js$rmcookie()
    state$cookie_id <- NULL
    state$user <- NULL
  })
  observeEvent(c(input$log_in, state$cookie_id), {
    req(!is.null(state$cookie_id) ||
          (!is.null(input$log_in) && input$log_in > 0))
    search <- isolate(ifelse(is.null(state$cookie_id),
                             input$user_id,
                             state$cookie_id))
    match <- tbl(conn, 'user') %>%
      filter(user_id == search) %>%
      collect()

    if (nrow(match) == 0) {
      sendSweetAlert(
        session,
        title = "User ID Could Not Be Found",
        text = paste("The user ID you entered doesn't exist. Please either",
                     "correct it to a valid ID or sign-up to this app"),
        type = 'error'
      )
    } else {
      state$user <- list(
        user_id = search,
        first_name = match$first_name,
        last_name = match$last_name
      )

      if (is.null(state$cookie_id)) {
        if (input$remember_me) {
          js$setcookie(input$user_id)
        }
      }
    }
  })
  observeEvent(input$sign_up, {
    if (input$first_name == "" || input$last_name == "" || input$team == "") {
      sendSweetAlert(
        session,
        title = "Required Fields Can't Be Empty",
        text = paste("Please make sure that you have filled in each field",
                     "before submitting"),
        type = 'error'
      )
      return()
    }

    search_fname <- isolate(input$first_name)
    search_lname <- isolate(input$last_name)
    match <- tbl(conn, 'user') %>%
      filter(first_name == search_fname,
             last_name == search_lname) %>%
      collect()

    if (nrow(match) == 0) {
      tryCatch({
        next_id <- dbGetQuery(conn, paste(
          'SELECT AUTO_INCREMENT',
          'FROM information_schema.TABLES',
          'WHERE TABLE_NAME = "user"'))$AUTO_INCREMENT

        new_user <- data.frame(
          first_name = search_fname,
          last_name = search_lname,
          team_id = as.integer(input$team),
          stringsAsFactors = FALSE
        )
        dbWriteTable(conn, 'user', new_user, append = TRUE, row.names = FALSE)

        sendSweetAlert(
          session,
          title = "Sign Up Was Successful",
          text = paste0("Your user ID is ", next_id,
                        ". Please keep hold of this for future log-ins"),
          type = 'success'
        )

        updateCheckboxInput(session, 'new_user', value = FALSE)
        state$wait = TRUE
        state$new_id = next_id
        updateTextInput(session, 'user_id', value = next_id)
      }, error = function(e) {
        sendSweetAlert(
          session,
          title = "An Error Occured When Creating A New User",
          text = paste("Please contact Tim Hargreaves quoting the error:",
                       e$message),
          type = 'error'
        )
      })
    } else {
      sendSweetAlert(
        session,
        title = "User Already Exists",
        text = paste("It appears that a user with this name already exists.",
                     "If this is your account, you can use the ID",
                     match$user_id,
                     "to log in. If you are attempting to correct your team",
                     "or you think there's been a mistake, please contact",
                     "Tim Hargreaves"),
        type = 'error'
      )
    }
  })
  observeEvent(state$wait, {
    req(state$new_id)
    req(!state$wait)
    updateTextInput(session, 'user_id', value = state$new_id)
    state$new_id <- FALSE
  })

  # Check for cookie
  observe({
    js$getcookie()
    if (!is.null(input$jscookie) && input$jscookie != "") {
      state$cookie_id <- as.integer(input$jscookie)
    }
  })

  # Data Entry
  observeEvent(input$back, {
    updateDateInput(session, 'date', value = input$date - 1)
  })
  observeEvent(input$forwards, {
    updateDateInput(session, 'date', value = input$date + 1)
  })
  observeEvent(input$yesterday, {
    updateDateInput(session, 'date', value = Sys.Date() - 1)
  })
  observeEvent(input$today, {
    updateDateInput(session, 'date', value = Sys.Date())
  })
  observeEvent(input$date, {
    if (input$date == START_DATE) {
      disable('back')
    } else {
      enable('back')
    }
    if (input$date == END_DATE) {
      disable('forwards')
    } else {
      enable('forwards')
    }
    if (input$date == Sys.Date() - 1) {
      disable('yesterday')
    } else {
      enable('yesterday')
    }
    if (input$date == Sys.Date()) {
      disable('today')
    } else {
      enable('today')
    }
  })
  observeEvent(input$upload, {
    match <- tbl(conn, 'activity') %>%
      filter(user_id == !!state$user$user_id,
             date == !!input$date) %>%
      collect()

    if (nrow(match) > 0) {
      confirmSweetAlert(
        session,
        'duplicate_confirm',
        title = "Duplicate Date",
        text = paste("The date you have entered already has activity uploaded",
                     "for it. If you continue to submit, the current activity",
                     "will be overwritten with these new values"),
        type = 'warning',
        btn_labels = c('Cancel', 'Continue')
      )
      state$overwrite_id <- match$activity_id
    } else {
      state$overwrite_id <- NULL
      state$submit <- state$submit + 1
    }
  })

  observeEvent(input$duplicate_confirm, {
    req(!is.null(input$duplicate_confirm))
    if (input$duplicate_confirm) {
      state$submit <- state$submit + 1
    }
  })

  observeEvent(state$submit, {
    req(state$submit > 0)
    tryCatch({
      if (is.null(state$overwrite_id)) {
        new_activity <- data.frame(
          user_id = state$user$user_id,
          date = input$date,
          num_steps = input$num_steps,
          num_cycle = input$num_cycle,
          num_swim = input$num_swim
        )
        dbWriteTable(conn, 'activity', new_activity,
                     append = TRUE, row.names = FALSE)
      } else {
        dbSendQuery(conn, paste0(
          "UPDATE activity ",
          "SET num_steps=", input$num_steps,
          ", num_cycle=", input$num_cycle,
          ", num_swim=", input$num_swim,
          " WHERE activity_id=", state$overwrite_id
        ))
      }
      sendSweetAlert(
        session,
        title = "Submission Was Successful",
        text = paste0("Your activities were successfully submitted"),
        type = 'success'
      )
    }, error = function(e) {
      sendSweetAlert(
        session,
        title = "An Error Occured When Uploading Activity",
        text = paste("Please contact Tim Hargreaves quoting the error:",
                     e$message),
        type = 'error'
      )
    })
  })

  # Control tabset visibility
  observe({
    if (is.null(state$user)) {
      hideTab('navbar', 'My Data')
      hideTab('navbar', 'Individual Comparison')
    } else {
      showTab('navbar', 'My Data')
      showTab('navbar', 'Individual Comparison')
    }
  })

  history_tbl <- reactive({
    req(state$user)
    tbl(conn, 'activity') %>%
      filter(user_id == !!state$user$user_id) %>%
      mutate(
        num_cycle = num_cycle * CYCLE_STEP_EQUIV,
        num_swim = num_swim * SWIM_STEP_EQUIV
      ) %>%
      select(-activity_id, -user_id) %>%
      collect()
  })

  output$history_plot <- renderPlot({
    history_gathered <- history_tbl() %>%
      gather(key = 'activity', value = 'step_equivalent',
             num_steps:num_swim)

    step_average <- history_gathered %>%
      group_by(date) %>%
      summarise(total_step_equivalent = sum(step_equivalent),
                .groups = 'drop') %>%
      summarise(avg_step_equivalent = mean(total_step_equivalent)) %>%
      extract2('avg_step_equivalent')

    history_gathered %>%
      filter(step_equivalent > 0) %>%
      ggplot(aes(x = date, y = step_equivalent, fill = activity)) +
        geom_col(position = 'stack') +
        geom_hline(yintercept = step_average, linetype = 'dashed') +
        annotate('text', y = step_average, x = START_DATE,
                 label = 'Average', vjust = -0.5, hjust = -.1) +
        labs(
          x = 'Date',
          y = 'Step Equivalent',
          fill = 'Activity'
        ) +
        coord_cartesian(xlim = c(START_DATE, END_DATE), expand = FALSE) +
        theme(legend.position = 'bottom')
  })

  output$history_table <- renderDataTable({
    history_tbl() %>%
      fix_column_names()
  })

  output$user_picker_ui <- renderUI({
    pickerInput(
      'users',
      "Users",
      users[users != state$user$user_id],
      selected = c(),
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE
      )
    )
  })

  individual_tbl <- reactive({
    tbl(conn, 'activity') %>%
      select(user_id, date, num_steps) %>%
      left_join(tbl(conn, 'user'), by = 'user_id') %>%
      filter(user_id %in% c(!!state$user$user_id, !!input$users)) %>%
      mutate(name = str_c(first_name, last_name, sep = ' ')) %>%
      group_by(name, date) %>%
      summarise(
        num_entries = n(),
        total_steps = sum(num_steps, na.rm = TRUE),
        average_steps = mean(num_steps, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      collect()
  })

  output$individual_comparison_time_series <- renderPlotly({
    req(length(individual_tbl()) > 0)
    p <- individual_tbl() %>%
      group_by(name) %>%
      mutate(
        total_steps = case_when(
          input$individual_cummulative_steps ~ cumsum(total_steps),
          TRUE ~ total_steps
        )
      ) %>%
      ggplot(aes(x = date, y = total_steps, col = name)) +
      geom_line() +
      labs(
        x = 'Date',
        y = paste0('Total Steps', ifelse(input$individual_cummulative_steps,
                                         ' (Cummulative)', '')),
        col = 'User'
      ) +
      xlim(START_DATE, min(Sys.Date(), END_DATE))
    ggplotly(p, dynamicTicks = TRUE)
  })

  team_tbl <- reactive({
    req(length(input$teams) > 0)
    tbl(conn, 'activity') %>%
      select(user_id, date, num_steps) %>%
      left_join(tbl(conn, 'user'), by = 'user_id') %>%
      filter(team_id %in% !!input$teams) %>%
      left_join(tbl(conn, 'team'), by = 'team_id') %>%
      group_by(team_name, date) %>%
      summarise(
        num_entries = n(),
        total_steps = sum(num_steps, na.rm = TRUE),
        average_steps = mean(num_steps, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      collect()
  })

  output$team_comparison_time_series <- renderPlotly({
    req(length(team_tbl()) > 0)
    p <- team_tbl() %>%
      group_by(team_name) %>%
      mutate(
        total_steps = case_when(
          input$team_cummulative_steps ~ cumsum(total_steps),
          TRUE ~ total_steps
        )
      ) %>%
      ggplot(aes(x = date, y = total_steps, col = team_name)) +
        geom_line() +
        labs(
          x = 'Date',
          y = paste0('Total Steps', ifelse(input$team_cummulative_steps,
                                           ' (Cummulative)', '')),
          col = 'Team'
        )
    ggplotly(p, dynamicTicks = TRUE)
  })

  output$team_comparison_leaderboard <- renderDataTable({
    req(length(team_tbl()) > 0)
    team_tbl() %>%
      group_by(team_name) %>%
      summarise_at(vars(num_entries, total_steps, average_steps), sum) %>%
      ungroup() %>%
      fix_column_names()
  })
}
