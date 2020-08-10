library(conflicted)
library(ggplot2)
library(dplyr)
library(stringr)

# Manage conflicts
conflict_prefer('dataTableOutput', 'DT', quiet = TRUE)
conflict_prefer('filter', 'dplyr', quiet = TRUE)
conflict_prefer("layout", "plotly", quiet = TRUE)
conflict_prefer('renderDataTable', 'DT', quiet = TRUE)
conflict_prefer('show', 'shinyjs', quiet = TRUE)

# Constants
START_DATE <- as.Date('2020-06-29')
END_DATE <- as.Date('2020-08-09')
CYCLE_STEP_EQUIV <- 100
SWIM_STEP_EQUIV <- 203
FEATURES <- c('Walking', 'Running', 'Cycling',
              'Yoga', 'Zumba', 'Strength Workout')
MULTIPLIER <- 1.2
WINNERS <- tibble(
  'user_id' = c(34L, 91L, 74L, 8L, 30L, 54L, 22L, 67L, 76L, 75L, 40L),
  'win_week' = c(1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L)
)

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
