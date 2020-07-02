library(DT)
library(shiny)
library(shinyjs)
library(shinyWidgets)

library(dplyr)
library(ggplot2)
library(jsonlite)
library(magrittr)
library(plotly)
library(RMySQL)
library(tidyr)

#### TODO: Multiplier not added to teams ####

server <- function(input, output, session) {

  # Connect to database
  db_cfg <- fromJSON('db_config.json')
  conn <- dbConnect(
    MySQL(),
    host = db_cfg$host,
    dbname = db_cfg$dbname,
    user = db_cfg$user,
    password = db_cfg$password
  )

  # Queue disconnect
  session$onSessionEnded(function() {dbDisconnect(conn)})

  # Collect team names
  team_tbl <- tbl(conn, 'team') %>%
    select(team_id, manager) %>%
    arrange(manager) %>%
    collect()
  teams <- team_tbl$team_id
  names(teams) <- team_tbl$manager

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

  # Login UI
  output$account_ui <- renderUI({
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
          "Manager",
          choices = c('No Manager' = -1L, teams),
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
        tags$p(tags$i(paste("If you have forgotten your ID, attempt to create",
                            "a new user with your name, at which point you",
                            "will be reminded of your existing ID."))),
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
      if (!is.null(state$cookie_id)) {
        js$rmcookie()
        state$cookie_id <- NULL
        state$user <- NULL
      }
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
    if (input$first_name == "" || input$last_name == "") {
      sendSweetAlert(
        session,
        title = "All Fields Are Required",
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
          'WHERE TABLE_NAME = "user"',
          'AND TABLE_SCHEMA =',
          paste0('"', db_cfg$dbname, '"')))$AUTO_INCREMENT

        new_user <- tibble(
          first_name = search_fname,
          last_name = search_lname,
          team_id = ifelse(input$team == -1, NA, input$team)
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
    updateDateInput(session, 'activity_date', value = input$activity_date - 1)
  })
  observeEvent(input$forwards, {
    updateDateInput(session, 'activity_date', value = input$activity_date + 1)
  })
  observeEvent(input$yesterday, {
    updateDateInput(session, 'activity_date', value = Sys.Date() - 1)
  })
  observeEvent(input$today, {
    updateDateInput(session, 'activity_date', value = Sys.Date())
  })
  observe({
    if (input$activity_date <= START_DATE) {
      disable('back')
    } else {
      enable('back')
    }
    if (input$activity_date >= END_DATE) {
      disable('forwards')
    } else {
      enable('forwards')
    }
    if (input$activity_date == Sys.Date() - 1) {
      disable('yesterday')
    } else if (Sys.Date() - 1 < START_DATE | Sys.Date() - 1 > END_DATE) {
      disable('yesterday')
    } else {
      enable('yesterday')
    }
    if (input$activity_date == Sys.Date()) {
      disable('today')
    } else if (Sys.Date() < START_DATE | Sys.Date() > END_DATE) {
      disable('today')
    } else {
      enable('today')
    }
  })

  # See https://www.ttested.com/input-permanence-with-shiny/
  current_feature <- reactiveVal(FALSE)
  observeEvent(input$feature, {
    current_feature(input$feature)
  })

  output$feature_toggle_ui <- renderUI({
    week <- as.integer(input$activity_date - START_DATE) %/% 7 + 1
      checkboxInput(
        'feature',
        paste0("Participated in Featured Activity (",
               ifelse(is.na(FEATURES[week]), "TBC", FEATURES[week]),
               ")"),
        value = current_feature()
      )
  })

  observeEvent(input$upload, {
    match <- tbl(conn, 'activity') %>%
      filter(user_id == !!state$user$user_id,
             date == !!input$activity_date) %>%
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
    if (input$duplicate_confirm) {
      state$submit <- state$submit + 1
    } else {
      sendSweetAlert(
        session,
        title = "Upload Cancelled",
        text = "The existing database entry has not been altered",
        type = 'error'
      )
    }
  }, ignoreNULL = TRUE)

  observeEvent(state$submit, {
    req(state$submit > 0)
    tryCatch({
      if (is.null(state$overwrite_id)) {
        new_activity <- data.frame(
          user_id = state$user$user_id,
          date = input$activity_date,
          raw_steps = input$num_steps,
          raw_cycling = input$num_cycle,
          raw_swimming = input$num_swim,
          feature = as.integer(input$feature)
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
      for (id in c('num_steps', 'num_cycle', 'num_swim')) {
        updateNumericInput(session, id, value = 0)
      }
      updateCheckboxInput(session, 'feature', value = FALSE)
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
    input$upload
    req(state$user)
    tbl(conn, 'activity') %>%
      filter(user_id == !!state$user$user_id) %>%
      select(-activity_id, -user_id) %>%
      collect() %>%
      mutate(
        date = as.Date(date),
        equiv_steps = raw_steps,
        equiv_cycling = raw_cycling * CYCLE_STEP_EQUIV,
        equiv_swimming = raw_swimming * SWIM_STEP_EQUIV
      )
  })

  output$history_plot <- renderPlotly({
      multiplier <- history_tbl() %>%
        select(date, feature) %>%
        mutate(week = as.integer(date - START_DATE) %/% 7 + 1) %>%
        group_by(week) %>%
        mutate(feature = case_when(
          input$apply_feature_multiplier ~ any(feature),
          TRUE ~ FALSE)) %>%
        ungroup() %>%
        select(date, feature)

    history_gathered <- history_tbl() %>%
      select(-feature) %>%
      inner_join(multiplier, by = 'date') %>%
      mutate(raw_feature = case_when(
        feature ~ (equiv_steps + equiv_cycling + equiv_swimming) *
            (MULTIPLIER - 1),
        TRUE ~ 0
      ), equiv_feature = raw_feature) %>%
      select(-feature) %>%
      pivot_longer(
        -date,
        names_to = c('.value', 'activity'),
        names_sep = '_'
      ) %>%
      mutate(
        activity = factor(activity,
                          levels = c('steps', 'cycling', 'swimming', 'feature'))
        )

    step_average <- history_gathered %>%
      group_by(date) %>%
      summarise(total_step_equivalent = sum(equiv),
                .groups = 'drop') %>%
      summarise(avg_step_equivalent = mean(total_step_equivalent)) %>%
      extract2('avg_step_equivalent')

    p <- history_gathered %>%
      filter(equiv > 0) %>%
      mutate(text = str_c(
        "Date: ", date, "\n",
        "Activity: ", activity, "\n",
        case_when(
          activity %in% c('steps', 'feature') ~ '',
          TRUE ~ str_c("Minutes: ", as.character(raw), "\n")
        ),
        "Step Equivalent: ", equiv
      )) %>%
      ggplot(aes(x = date, y = equiv, fill = activity, text = text)) +
        geom_col(position = position_stack(reverse = TRUE),
                 alpha = 0.5, col = 'black') +
        scale_y_continuous(expand = c(0, 0)) +
        scale_fill_manual(values = c(
          '#F8766D', '#7CAE00', '#00BFC4', '#CF9400'
        )) +
        labs(
          x = 'Date',
          y = 'Step Equivalent',
          fill = 'Activity'
        ) +
        theme(axis.text.x = element_text(angle = 90))

    if (input$show_average) {
      p <- p + geom_hline(yintercept = step_average, linetype = 'dashed')
    }

    ggplotly(p, tooltip = "text")
  })

  output$history_table <- renderDataTable({
    history_tbl() %>%
      fix_column_names() %>%
      datatable(options = list(scrollX = TRUE, scrollCollapse = TRUE),
                rownames = FALSE)
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
        liveSearch = TRUE,
        noneSelectedText = 'Nobody Selected'
      )
    )
  })

  individual_tbl <- reactive({
    tbl(conn, 'activity') %>%
      left_join(tbl(conn, 'user'), by = 'user_id') %>%
      filter(user_id %in% c(!!state$user$user_id, !!input$users)) %>%
      collect() %>%
      mutate(
        date = as.Date(date),
        equiv_steps = raw_steps,
        equiv_cycling = raw_cycling * CYCLE_STEP_EQUIV,
        equiv_swimming = raw_swimming * SWIM_STEP_EQUIV,
        name = str_c(first_name, last_name, sep = ' '),
        week = as.integer(date - START_DATE) %/% 7 + 1
      ) %>%
      group_by(name, week) %>%
      mutate(feature = case_when(
        input$apply_feature_multiplier ~ any(feature),
        TRUE ~ FALSE)) %>%
      ungroup() %>%
      mutate(
        equiv_feature = round((equiv_steps + equiv_cycling + equiv_swimming) *
          (MULTIPLIER - 1))
      )
  })

  output$individual_comparison_time_series <- renderPlotly({
    req(nrow(individual_tbl()) > 0)
    p <- individual_tbl() %>%
      group_by(name, date) %>%
      summarise(
        total_steps = sum(equiv_steps + equiv_cycling + equiv_swimming +
                            equiv_feature),
        .groups = 'drop'
      ) %>%
      group_by(name) %>%
      mutate(
        total_steps = case_when(
          input$individual_cummulative_steps ~ cumsum(total_steps),
          TRUE ~ total_steps
        )
      ) %>%
      mutate(text = str_c(
        "Date: ", date, "\n",
        case_when(
          input$team_cummulative_steps ~ "Cummulative Step Equivalent: ",
          TRUE ~ "Step Equivalent: "
        ), format(total_steps, big.mark = ","), "\n",
        "Participant: ", name
      )) %>%
      ggplot(aes(x = date, y = total_steps, col = name, text = text, group = name)) +
      geom_line() +
      labs(
        x = 'Date',
        y = paste0('Total Steps', ifelse(input$individual_cummulative_steps,
                                         ' (Cummulative)', '')),
        col = 'User'
      )
    ggplotly(p, tooltip = 'text')
  })

  output$individual_comparison_leaderboard <- renderDataTable({
    req(nrow(individual_tbl()) > 0)
    individual_tbl() %>%
      group_by(name) %>%
      summarise(
        total_steps = sum(equiv_steps + equiv_cycling + equiv_swimming + equiv_feature),
        num_entries = n(),
        avg_steps = round(mean(equiv_steps + equiv_cycling + equiv_swimming + equiv_feature)),
        .groups = 'drop'
      ) %>%
      fix_column_names() %>%
      datatable(options = list(scrollX = TRUE, scrollCollapse = TRUE),
                rownames = FALSE)
  })

  output$teams_selector_ui <- renderUI({
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
    )
  })

  team_tbl <- reactive({
    input$upload
    req(length(input$teams) > 0)
    tbl(conn, 'activity') %>%
      left_join(tbl(conn, 'user'), by = 'user_id') %>%
      filter(team_id %in% !!input$teams) %>%
      inner_join(tbl(conn, 'team'), by = 'team_id') %>%
      collect() %>%
      mutate(
        date = as.Date(date),
        equiv_steps = raw_steps,
        equiv_cycling = raw_cycling * CYCLE_STEP_EQUIV,
        equiv_swimming = raw_swimming * SWIM_STEP_EQUIV,
        week = as.integer(date - START_DATE) %/% 7 + 1
      ) %>%
      group_by(first_name, last_name, week) %>%
      mutate(feature = case_when(
        input$apply_feature_multiplier ~ any(feature),
        TRUE ~ FALSE)) %>%
      ungroup() %>%
      mutate(
        equiv_feature = round((equiv_steps + equiv_cycling + equiv_swimming) *
          (MULTIPLIER - 1))
      )
  })

  output$team_comparison_time_series <- renderPlotly({
    req(nrow(team_tbl()) > 0)
    p <- team_tbl() %>%
      group_by(manager, date) %>%
      summarise(
        total_steps = sum(equiv_steps + equiv_cycling + equiv_swimming + equiv_feature),
        .groups = 'drop'
      ) %>%
      group_by(manager) %>%
      mutate(
        total_steps = case_when(
          input$team_cummulative_steps ~ cumsum(total_steps),
          TRUE ~ total_steps
        )
      ) %>%
      mutate(text = str_c(
        "Date: ", date, "\n",
        case_when(
          input$team_cummulative_steps ~ "Cummulative Step Equivalent: ",
          TRUE ~ "Step Equivalent: "
        ), format(total_steps, big.mark = ","), "\n",
        "Team Manager: ", manager
      )) %>%
      ggplot(aes(x = date, y = total_steps, col = manager, text = text, group = manager)) +
        geom_line() +
        labs(
          x = 'Date',
          y = paste0('Total Steps', ifelse(input$team_cummulative_steps,
                                           ' (Cummulative)', '')),
          col = 'Team'
        )
    ggplotly(p, tooltip = 'text', dynamicTicks = TRUE)
  })

  output$team_comparison_leaderboard <- renderDataTable({
    req(nrow(team_tbl()) > 0)
    team_tbl() %>%
      group_by(manager, size) %>%
      summarise(
        total_steps = sum(equiv_steps + equiv_cycling + equiv_swimming + equiv_feature),
        num_entries = n(),
        avg_steps = round(mean(equiv_steps + equiv_cycling + equiv_swimming + equiv_feature)),
        participation_rate = round(100 * num_entries /
          ((as.integer(max(min(Sys.Date(), END_DATE), START_DATE) - START_DATE) + 1))),
        .groups = 'drop'
      ) %>%
      mutate(participation_rate = str_c(round(participation_rate / size), "%")) %>%
      fix_column_names() %>%
      datatable(options = list(scrollX = TRUE, scrollCollapse = TRUE),
                rownames = FALSE)
  })

  output$week_selector_ui <- renderUI({
    week <- as.integer(min(END_DATE,
                           max(Sys.Date(), START_DATE)) - START_DATE) %/% 7 + 1
    sliderInput(
      'week',
      "Week",
      min = 1,
      max = week,
      value = week,
      step = 1
    )
  })

  winners_tbl <- reactive({
    req(input$week)
    tbl(conn, 'activity') %>%
      left_join(tbl(conn, 'user'), by = 'user_id') %>%
      collect() %>%
      mutate(
        date = as.Date(date),
        equiv_steps = raw_steps,
        equiv_cycling = raw_cycling * CYCLE_STEP_EQUIV,
        equiv_swimming = raw_swimming * SWIM_STEP_EQUIV,
        name = str_c(first_name, last_name, sep = ' '),
        week = as.integer(date - START_DATE) %/% 7 + 1
      ) %>%
      filter(week %in% c(input$week, max(1, input$week - 1))) %>%
      group_by(name, week) %>%
      mutate(feature = case_when(
        input$apply_feature_multiplier ~ any(feature),
        TRUE ~ FALSE)) %>%
      group_by(week) %>%
      mutate(
        equiv_feature = round((equiv_steps + equiv_cycling + equiv_swimming) *
          (MULTIPLIER - 1))
      ) %>%
      group_by(name, week) %>%
      summarise(
        total_steps = sum(equiv_steps + equiv_cycling +
                            equiv_swimming + equiv_feature),
        n = n(),
        .groups = 'drop'
      ) %>%
      filter(input$week == 1 | week != input$week | n < 6) %>%
      select(-n) %>%
      mutate(week = factor(case_when(
        week == input$week ~ 'current',
        TRUE ~ 'previous'
      ), levels = c('previous', 'current'))) %>%
      spread(key = 'week', value = 'total_steps', drop = FALSE) %>%
      filter(input$week == 1 | !is.na(previous), !is.na(current))
  })

  output$winners_text <- renderText({
    req(input$week)
    if (input$week > 1) {
      paste("NB: To qualify for a prize in weeks 2-6, you must have not won",
            "a prize before and you must have had at least six entries in the",
            "previous week.")
    }
  })

  output$winners_plot <- renderPlotly({
    req(input$week)
    if (input$week == 1) {
      plot_tbl <- winners_tbl() %>%
        mutate(metric = current)
    } else {
      plot_tbl <- winners_tbl() %>%
        mutate(metric = current / previous)
    }

    p <- plot_tbl %>%
      mutate(
        name = reorder(name, -metric)
      ) %>%
      mutate(text = str_c(
        "Name: ", name, "\n",
        ifelse(input$week == 1, "Total Steps: ", "Step Ratio: "), metric,
        ifelse(input$week == 1, "", "\nCurrent Steps: "),
        ifelse(input$week == 1, "", current),
        ifelse(input$week == 1, "", "\nPrevious Steps: "),
        ifelse(input$week == 1, "", previous)
      )) %>%
      ggplot(aes(x = name, y = metric, fill = name, text = text)) +
        geom_col(alpha = 0.5, col = 'black', show.legend = FALSE) +
        labs(
          x = 'Name',
          y = ifelse(input$week == 1, 'Total Weekly Steps', 'Step Ratio'),
          col = 'Team'
        ) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(axis.text.x = element_text(angle = 90))

    ggplotly(p, tooltip = 'text') %>%
      layout(showlegend = FALSE)
  })

  output$winners_table <- renderDataTable({
    req(input$week)
    if (input$week == 1) {
      table_tbl <- winners_tbl() %>%
        select(name, total = current)
    } else {
      table_tbl <- winners_tbl() %>%
        mutate(ratio = current / previous) %>%
        select(name, previous, current, ratio)
    }

    table_tbl %>%
    fix_column_names() %>%
      datatable(options = list(scrollX = TRUE, scrollCollapse = TRUE),
                rownames = FALSE)
  })
}
