

# global.R - Shared functions and initialization
library(shiny)
library(DT)
library(shinyjs)
library(httr)
library(jsonlite)

# Initialize or load survey configurations
SURVEYS_FILE <- "survey_configs.json"

init_surveys <- function() {
  if (!file.exists(SURVEYS_FILE)) {
    jsonlite::write_json(list(), SURVEYS_FILE, auto_unbox = TRUE, pretty = TRUE)
  }
}

load_surveys <- function() {
  if (file.exists(SURVEYS_FILE)) {
    fromJSON(SURVEYS_FILE)
  } else {
    list()
  }
}

save_surveys <- function(surveys) {
  jsonlite::write_json(surveys, SURVEYS_FILE, auto_unbox = TRUE, pretty = TRUE)
}

# Save uploaded files
save_upload <- function(file, survey_id, file_type) {
  if (is.null(file)) return(NULL)
  
  # Create directory for survey if it doesn't exist
  dir_path <- file.path("survey_data", survey_id)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  
  # Save file
  file_path <- file.path(dir_path, paste0(file_type, "_", file$name))
  file.copy(file$datapath, file_path, overwrite = TRUE)
  
  return(file_path)
}

# Initialize on startup
init_surveys()