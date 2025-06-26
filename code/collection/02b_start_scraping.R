# 02b_start_scraping.R
# Quick Start Script for Utah Legislative Data Scraping
# School Choice Policy Discourse Analysis Project

# This script provides a streamlined workflow to get started with web scraping

# Load required libraries
required_packages <- c("rvest", "httr", "dplyr", "purrr", "stringr", 
                      "lubridate", "jsonlite", "xml2", "readr")

# Check and install missing packages
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
}

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Source project configuration from setup folder
config_paths <- c(
  "../../setup/config.R",        # From code/collection/ to setup/
  "../setup/config.R",           # From code/ to setup/  
  "setup/config.R",              # From project root to setup/
  "config.R"                     # If running from setup folder
)

config_found <- FALSE
for(path in config_paths) {
  if(file.exists(path)) {
    source(path)
    cat("✓ Found and loaded config.R from:", path, "\n")
    config_found <- TRUE
    break
  }
}

if(!config_found) {
  cat("✗ config.R not found in setup folder from current location.\n")
  cat("Current working directory:", getwd(), "\n")
  cat("Tried these paths:\n")
  for(path in config_paths) {
    cat("  -", path, "(exists:", file.exists(path), ")\n")
  }
  cat("\nPlease ensure you're running from the correct directory or that setup/config.R exists.\n")
  stop("Cannot find config.R in setup folder.")
}

# Check if directory structure exists
if(!dir.exists(CONFIG$project$dirs$data)) {
  stop("Project directories not found. Please run 01_directory_setup.Rmd first.")
}

cat("╔══════════════════════════════════════════════════════════╗\n")
cat("║              Utah Legislative Data Scraper               ║\n")
cat("║            School Choice Policy Discourse                ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

# Interactive workflow function
start_scraping_workflow <- function() {
  cat("Welcome to the Utah Legislative Data Scraping Workflow!\n\n")
  
  repeat {
    cat("What would you like to do?\n")
    cat("1. Inspect Utah legislative website structure\n")
    cat("2. Test scraper with one year of data\n") 
    cat("3. Run pilot scrape (2022-2024 + sample downloads)\n")
    cat("4. Run full scrape (2019-2024)\n")
    cat("5. Check existing data\n")
    cat("6. Exit\n\n")
    
    choice <- readline(prompt = "Enter your choice (1-6): ")
    
    switch(choice,
      "1" = {
        cat("\n=== Website Structure Inspection ===\n")
        if(!file.exists("02a_utah_site_inspector.R")) {
          cat("Site inspector script not found. Please ensure 02a_utah_site_inspector.R is in your project directory.\n")
        } else {
          source("02a_utah_site_inspector.R")
          run_site_inspection()
        }
      },
      
      "2" = {
        cat("\n=== Testing Scraper ===\n")
        if(!file.exists("02_utah_web_scraper_clean.R")) {
          cat("Main scraper script not found. Please ensure 02_utah_web_scraper.R is in your project directory.\n")
        } else {
          source("02_utah_web_scraper_clean.R")
          cat("Testing with 2024 data...\n")
          test_meetings <- utah_scraper$scrape_all_meetings(2024, 2024)
          
          if(nrow(test_meetings) > 0) {
            cat("✓ Test successful! Found", nrow(test_meetings), "meetings.\n")
            print(head(test_meetings))
            
            save_test <- readline(prompt = "Save test results? (y/n): ")
            if(tolower(save_test) == "y") {
              utah_scraper$save_metadata(test_meetings, "test_meetings.csv")
            }
          } else {
            cat("✗ Test failed. No meetings found. Check website structure.\n")
          }
        }
      },
      
      "3" = {
        cat("\n=== Pilot Scrape ===\n")
        if(!file.exists("02_utah_web_scraper_clean.R")) {
          cat("Main scraper script not found. Please ensure 02_utah_web_scraper.R is in your project directory.\n")
        } else {
          source("02_utah_web_scraper_clean.R")
          
          confirm <- readline(prompt = "This will scrape 2022-2024 data and download sample audio files. Continue? (y/n): ")
          if(tolower(confirm) == "y") {
            pilot_results <- utah_scraper$run_pilot_scrape()
            cat("Pilot scrape completed!\n")
          }
        }
      },
      
      "4" = {
        cat("\n=== Full Scrape ===\n")
        if(!file.exists("02_utah_web_scraper_clean.R")) {
          cat("Main scraper script not found. Please ensure 02_utah_web_scraper.R is in your project directory.\n")
        } else {
          source("02_utah_web_scraper_clean.R")
          
          cat("WARNING: This will scrape ALL years (2019-2024) and may take significant time.\n")
          cat("It will also download ALL available audio files, which could be many GB.\n")
          confirm <- readline(prompt = "Are you sure you want to continue? (y/n): ")
          
          if(tolower(confirm) == "y") {
            cat("Starting full scrape...\n")
            
            # Scrape all meetings
            all_meetings <- utah_scraper$scrape_all_meetings(2019, 2024)
            
            if(nrow(all_meetings) > 0) {
              # Save metadata
              utah_scraper$save_metadata(all_meetings, "utah_complete_metadata.csv")
              
              # Ask about audio downloads
              audio_confirm <- readline(prompt = "Download all audio files? This may take hours and use significant disk space. (y/n): ")
              if(tolower(audio_confirm) == "y") {
                utah_scraper$download_audio_files(all_meetings, max_downloads = Inf)
              }
              
              cat("Full scrape completed!\n")
            } else {
              cat("No meetings found. Please check scraper configuration.\n")
            }
          }
        }
      },
      
      "5" = {
        cat("\n=== Checking Existing Data ===\n")
        check_existing_data()
      },
      
      "6" = {
        cat("Goodbye!\n")
        break
      },
      
      {
        cat("Invalid choice. Please enter 1-6.\n\n")
      }
    )
    
    cat("\n")
  }
}

# Function to check existing scraped data
check_existing_data <- function() {
  data_dir <- file.path(CONFIG$project$data_sources$legislative$raw, "UT")
  
  # Check for metadata files
  metadata_files <- list.files(data_dir, pattern = "\\.csv$|\\.rds$", full.names = TRUE)
  
  if(length(metadata_files) > 0) {
    cat("Found", length(metadata_files), "metadata file(s):\n")
    for(file in metadata_files) {
      cat("  -", basename(file), "(", round(file.size(file)/1024, 1), "KB )\n")
    }
    
    # Try to load the most recent CSV file
    csv_files <- metadata_files[str_detect(metadata_files, "\\.csv$")]
    if(length(csv_files) > 0) {
      latest_csv <- csv_files[which.max(file.mtime(csv_files))]
      
      tryCatch({
        meetings <- read.csv(latest_csv)
        cat("\nLatest metadata summary:\n")
        cat("  Total meetings:", nrow(meetings), "\n")
        cat("  Date range:", min(meetings$meeting_date, na.rm = TRUE), "to", 
            max(meetings$meeting_date, na.rm = TRUE), "\n")
        cat("  Meetings with audio:", sum(meetings$audio_url != "", na.rm = TRUE), "\n")
      }, error = function(e) {
        cat("Error reading metadata file:", e$message, "\n")
      })
    }
  } else {
    cat("No metadata files found.\n")
  }
  
  # Check for audio files
  audio_dir <- file.path(data_dir, "audio")
  if(dir.exists(audio_dir)) {
    audio_files <- list.files(audio_dir, pattern = "\\.(wav|mp3|m4a)$", full.names = TRUE)
    
    if(length(audio_files) > 0) {
      total_size_gb <- sum(file.size(audio_files)) / 1024^3
      cat("\nAudio files:\n")
      cat("  Count:", length(audio_files), "\n")
      cat("  Total size:", round(total_size_gb, 2), "GB\n")
      
      # Show sample filenames
      cat("  Sample files:\n")
      sample_files <- head(basename(audio_files), 5)
      for(file in sample_files) {
        cat("    -", file, "\n")
      }
      if(length(audio_files) > 5) {
        cat("    ... and", length(audio_files) - 5, "more\n")
      }
    } else {
      cat("\nNo audio files found.\n")
    }
  } else {
    cat("\nAudio directory does not exist.\n")
  }
  
  # Check for download logs
  log_files <- list.files(data_dir, pattern = "download_log.*\\.csv$", full.names = TRUE)
  if(length(log_files) > 0) {
    cat("\nDownload logs:\n")
    for(log_file in log_files) {
      tryCatch({
        log_data <- read.csv(log_file)
        successful <- sum(log_data$download_success, na.rm = TRUE)
        total <- nrow(log_data)
        cat("  -", basename(log_file), ":", successful, "/", total, "successful downloads\n")
      }, error = function(e) {
        cat("  -", basename(log_file), "(error reading file)\n")
      })
    }
  }
}

# Function for quick setup validation
validate_setup <- function() {
  cat("=== Setup Validation ===\n")
  
  issues <- c()
  
  # Check config
  if(!exists("CONFIG")) {
    issues <- c(issues, "CONFIG object not found - config.R may not have loaded properly")
  }
  
  # Check directory structure
  if(!dir.exists(CONFIG$project$dirs$data)) {
    issues <- c(issues, "Data directories not created")
  }
  
  # Check required packages
  missing_pkgs <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  if(length(missing_pkgs) > 0) {
    issues <- c(issues, paste("Missing packages:", paste(missing_pkgs, collapse = ", ")))
  }
  
  # Check scraper scripts
  if(!file.exists("02_utah_web_scraper_clean.R")) {
    issues <- c(issues, "Main scraper script not found")
  }
  
  if(!file.exists("02a_utah_site_inspector.R")) {
    issues <- c(issues, "Site inspector script not found (optional)")
  }
  
  if(length(issues) == 0) {
    cat("✓ Setup validation passed! Ready to start scraping.\n")
    return(TRUE)
  } else {
    cat("✗ Setup issues found:\n")
    for(issue in issues) {
      cat("  -", issue, "\n")
    }
    cat("\nPlease resolve these issues before proceeding.\n")
    return(FALSE)
  }
}

# Print welcome message and instructions
cat("Quick Start Script Loaded!\n\n")

cat("Available functions:\n")
cat("• start_scraping_workflow() - Interactive guided workflow\n")
cat("• validate_setup() - Check if everything is configured properly\n") 
cat("• check_existing_data() - Review any previously scraped data\n\n")

# Auto-validate setup
if(validate_setup()) {
  cat("Ready to start! Run start_scraping_workflow() to begin.\n")
} else {
  cat("Please fix setup issues first.\n")
}