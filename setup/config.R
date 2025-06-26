library(here)
library(tibble)

#--------------------
# Project Structure 
#--------------------
PROJECT_CONFIG <- list(
  # Base directories
  dirs = list(
    base = here(),
    data = here("data"),
    raw = here("data", "raw"),
    processed = here("data", "processed"),
    metadata = here("data", "metadata")
  ),
  
  # Data source subdirectories
  data_sources = list(
    legislative = list(
      raw = here("data", "raw", "legislative"),
      processed = here("data", "processed", "legislative")
    ),
    district = list(
      raw = here("data", "raw", "district"),
      processed = here("data", "processed", "district")
    ),
    advocacy = list(
      raw = here("data", "raw", "advocacy"),
      processed = here("data", "processed", "advocacy")
    ),
    social_media = list(
      raw = here("data", "raw", "social_media"),
      processed = here("data", "processed", "social_media")
    )
  ),
  
  # Time range for data collection
  years = 2019:2024,
  
  # States included in analysis
  states = list(
    pilot = "UT",
    full = c("UT", "FL", "AL", "TX")
  )
)

#--------------------
# Pilot State Configuration (Utah)
#--------------------
UTAH_CONFIG <- list(
  state = list(
    abbreviation = "UT",
    name = "Utah",
    chamber = "HOUSE",
    committee = list(
      name = "House Education Committee",
      abbreviation = "EDUC"
    )
  ),
  
  # Legislative website structure
  urls = list(
    committee = "https://le.utah.gov/committee/committee.jsp",
    audio_base = "https://le.utah.gov/av/"
  )
)

#--------------------
# Web Scraping Settings
#--------------------
SCRAPING_CONFIG <- list(
  # Request settings
  request = list(
    delay = 3,  # Seconds between requests
    timeout = 30,  # Request timeout in seconds
    max_retries = 3,
    user_agent = "Research Project Bot (your@email.edu)"
  ),
  
  # File types to download
  file_types = c("mp4", "wav")
)

#--------------------
# File Naming Convention
#--------------------
FILENAME_CONFIG <- list(
  # Pattern for legislative audio files
  legislative = list(
    pattern = "{state}_{year}_{month}_{day}_{chamber}_{committee}_{id}",
    padding = list(
      id = 4  # Number of digits for ID padding
    )
  ),
  
  # Patterns for other data sources (to be expanded)
  district = list(
    pattern = "{state}_{district}_{year}_{month}_{day}_{id}"
  ),
  
  advocacy = list(
    pattern = "{state}_{org}_{year}_{month}_{day}_{id}"
  )
)

#--------------------
# Metadata Configuration
#--------------------
METADATA_CONFIG <- list(
  # Schema for legislative meetings
  legislative_cols = list(
    meeting_id = "character",
    state = "character",
    chamber = "character",
    committee = "character",
    committee_abbrev = "character",
    date = "Date",
    meeting_title = "character",
    meeting_topics = "character",
    bills_discussed = "character",
    original_url = "character",
    audio_filename = "character",
    download_timestamp = "POSIXct",
    file_size_mb = "numeric",
    duration_minutes = "numeric",
    transcription_status = "character",
    notes = "character"
  ),
  
  # Valid values for categorical fields
  valid_values = list(
    transcription_status = c(
      "pending",
      "in_progress",
      "completed",
      "failed",
      "needs_review"
    ),
    state_abbrev = c("UT", "FL", "AL", "TX")
  )
)

#--------------------
# Processing Settings
#--------------------
PROCESSING_CONFIG <- list(
  # Whisper transcription settings
  whisper = list(
    model = "base",
    language = "en",
    task = "transcribe",
    batch_size = 16
  ),
  
  # Picovoice settings
  picovoice = list(
    min_speakers = 1,
    max_speakers = 10
  )
)

#--------------------
# Utility Functions
#--------------------
# Function to validate project structure
validate_project_structure <- function() {
  # Check core directories exist
  for (dir in unlist(PROJECT_CONFIG$dirs)) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      message(sprintf("Created directory: %s", dir))
    }
  }
  
  # Check data source directories exist
  for (source in names(PROJECT_CONFIG$data_sources)) {
    for (dir in unlist(PROJECT_CONFIG$data_sources[[source]])) {
      if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
        message(sprintf("Created directory: %s", dir))
      }
    }
  }
}

# Function to get path for a data file
get_data_path <- function(source, state, filename, type = "raw") {
  base_dir <- PROJECT_CONFIG$data_sources[[source]][[type]]
  file.path(base_dir, state, filename)
}

# Function to get metadata file path
get_metadata_path <- function(filename) {
  file.path(PROJECT_CONFIG$dirs$metadata, filename)
}

# Export all configs
CONFIG <- list(
  project = PROJECT_CONFIG,
  utah = UTAH_CONFIG,
  scraping = SCRAPING_CONFIG,
  filename = FILENAME_CONFIG,
  metadata = METADATA_CONFIG,
  processing = PROCESSING_CONFIG
)

# Validate project structure when config is sourced
validate_project_structure()

