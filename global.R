library(conflicted)
library(ggplot2)
library(jsonlite)
library(dplyr)
library(RMariaDB)
library(stringr)

# Manage conflicts
conflict_prefer('dataTableOutput', 'DT', quiet = TRUE)
conflict_prefer('filter', 'dplyr', quiet = TRUE)
conflict_prefer('renderDataTable', 'DT', quiet = TRUE)
conflict_prefer('show', 'shinyjs', quiet = TRUE)

# Constants
START_DATE = as.Date('2020-06-01')
END_DATE = as.Date('2020-06-30')
CYCLE_STEP_EQUIV = 100
SWIM_STEP_EQUIV = 203

# Theming
theme_set(theme_classic())

# Setup Javascript
if (!file.exists('www/js.cookie.js')) {
  download.file(
    url = 'https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js',
    destfile = 'www/js.cookie.js'
  )}
addResourcePath("js", "www")
cookie_js <- '
  shinyjs.getcookie = function(params) {
    var cookie = Cookies.get("id");
    if (typeof cookie !== "undefined") {
      Shiny.onInputChange("jscookie", cookie);
    } else {
      var cookie = "";
      Shiny.onInputChange("jscookie", cookie);
    }
  }
  shinyjs.setcookie = function(params) {
    Cookies.set("id", escape(params), { expires: 30 });
  }
  shinyjs.rmcookie = function(params) {
    Cookies.remove("id");
    Shiny.onInputChange("jscookie", "");
  }
'

# Connect to database
db_cfg <- fromJSON('db_config.json')
conn <- dbConnect(
  MariaDB(),
  host = db_cfg$host,
  dbname = db_cfg$dbname,
  user = db_cfg$user,
  password = db_cfg$password
)

# Queue disconnect on stop
onStop(function() {
  dbDisconnect(conn)
})

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

# Helper functions
fix_column_names <- function(df) {
  df %>%
    rename_all(function(c) {
      c %>%
        str_replace_all('(?<=^|_)id(?=$|_)', 'ID') %>%
        str_replace_all('_', ' ') %>%
        gsub('(?<=^| )([a-z])', '\\U\\1\\E', ., perl = TRUE)
    })
}

dateButton <- function(title, icon) {
  actionButton(
    tolower(title),
    title,
    icon = icon(icon),
    width = 'calc(50% - 2px)',
    style = 'padding: 4px; font-size: 80%'
  )
}
