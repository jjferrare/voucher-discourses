# 02_utah_web_scraper_clean.R
# Utah Legislative Data Web Scraper with MP4 Discovery
# School Choice Policy Discourse Analysis Project
# Updated with precise meeting ID ranges for House and Senate Education Committees

# Load required libraries
library(rvest)
library(httr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(jsonlite)
library(xml2)

# Source configuration from setup folder
config_paths <- c("../../setup/config.R", "../setup/config.R", "setup/config.R", "config.R")
config_found <- FALSE
for(path in config_paths) {
  if(file.exists(path)) {
    source(path)
    config_found <- TRUE
    break
  }
}
if(!config_found) {
  stop("config.R not found in setup folder. Please ensure setup/config.R exists.")
}

# Create Utah-specific scraper functions
utah_scraper <- list()

# Define exact meeting IDs for House and Senate Education Committees
utah_scraper$meeting_ids <- list(
  house = list(
    "2019" = c(16088, 16108, 16136, 16148, 16176, 16201, 16214, 16252, 16272, 16291, 16319, 16331, 16367, 16389, 16415),
    "2020" = c(16726, 16756, 16778, 16802, 16819, 16849, 16870, 16899, 16929, 16949, 16963, 16985, 17019, 17041, 17078),
    "2021" = c(17327, 17373, 17399, 17419, 17441, 17472, 17485, 17506, 17529, 17548, 17569, 17591),
    "2022" = c(17925, 17947, 17974, 17992, 18020, 18039, 18061, 18084, 18119, 18134, 18151, 18176),
    "2023" = c(18488, 18510, 18535, 18555, 18575, 18591, 18627, 18642, 18670, 18681, 18713, 18735, 18760),
    "2024" = c(19010, 19034, 19053, 19096, 19165, 19167, 19170, 19187, 19172, 19175, 19178, 19181, 19185)
  ),
  senate = list(
    "2019" = c(16085, 16101, 16130, 16157, 16196, 16244, 16276, 16337, 16356, 16385, 16401, 16413),
    "2020" = c(16747, 16770, 16844, 16879, 16922, 16942, 16965, 16999, 17031, 17057, 17076),
    "2021" = c(17319, 17368, 17392, 17410, 17437, 17471, 17500, 17517, 17545, 17566, 17598),
    "2022" = c(17895, 17920, 17939, 17964, 17989, 18012, 18048, 18076, 18109, 18122, 18145, 18169),
    "2023" = c(18482, 18506, 18528, 18551, 18593, 18617, 18630, 18660, 18693, 18707, 18733, 18747),
    "2024" = c(19027, 19057, 19066, 19120, 19266, 19267, 19268, 19269, 19270, 19271, 19272, 19273)
  )
)


# Function to construct Utah legislative committee URLs
utah_scraper$build_committee_urls <- function(year, chamber = "house", committee = "education") {
  committee_code <- if(tolower(chamber) == "house") "HSTEDU" else "SSTEDU"
  committee_url <- paste0("https://le.utah.gov/committee/committee.jsp?year=", year, "&com=", committee_code)
  return(committee_url)
}

# Function to check if a meeting ID belongs to Education Committee
utah_scraper$is_education_committee <- function(mtg_id) {
  test_url <- paste0("https://le.utah.gov/av/committeeArchive.jsp?mtgID=", mtg_id)
  
  tryCatch({
    response <- HEAD(test_url, timeout(5))
    
    if(status_code(response) == 200) {
      page <- read_html(test_url)
      page_text <- html_text(page)
      
      # Check for Education committee indicators
      education_indicators <- c(
        "Education", "HSTEDU", "SSTEDU", 
        "House Education", "Senate Education",
        "Public Education", "Higher Education"
      )
      
      is_education <- any(str_detect(page_text, education_indicators))
      
      if(is_education) {
        # Determine which chamber
        chamber <- if(str_detect(page_text, "House|HSTEDU")) "house" else "senate"
        return(list(is_education = TRUE, chamber = chamber))
      }
    }
    
    return(list(is_education = FALSE, chamber = NA))
    
  }, error = function(e) {
    return(list(is_education = FALSE, chamber = NA, error = e$message))
  })
}

# Function to get Education Committee meetings by year and chamber using exact IDs
utah_scraper$get_education_meetings_by_exact_ids <- function(year, chamber = "both") {
  year_str <- as.character(year)
  
  cat("=== Finding", toupper(chamber), "Education Committee Meetings for", year, "===\n")
  
  if(!year_str %in% names(utah_scraper$meeting_ids$house)) {
    cat("❌ No meeting IDs defined for year", year, "\n")
    return(tibble())
  }
  
  chambers_to_process <- if(chamber == "both") c("house", "senate") else chamber
  all_meetings <- tibble()
  
  for(current_chamber in chambers_to_process) {
    meeting_ids <- utah_scraper$meeting_ids[[current_chamber]][[year_str]]
    
    cat("\nProcessing", toupper(current_chamber), "Education Committee:\n")
    cat("  ", length(meeting_ids), "known meeting IDs:", paste(meeting_ids, collapse = ", "), "\n")
    
    chamber_meetings <- tibble()
    
    for(i in seq_along(meeting_ids)) {
      id <- meeting_ids[i]
      cat("    Testing meeting", i, "of", length(meeting_ids), "(ID:", id, ")...")
      
      test_url <- paste0("https://le.utah.gov/av/committeeArchive.jsp?mtgID=", id)
      
      # Retry logic for connection issues
      max_retries <- 3
      retry_count <- 0
      success <- FALSE
      
      while(retry_count < max_retries && !success) {
        tryCatch({
          response <- HEAD(test_url, timeout(10))
          
          if(status_code(response) == 200) {
            page <- read_html(test_url)
            page_text <- html_text(page)
            
            # Close the connection explicitly
            try(close(response), silent = TRUE)
            
            # More flexible Education committee detection
            education_indicators <- c(
              "Education", "HSTEDU", "SSTEDU", 
              "House Education", "Senate Education",
              "Public Education", "Higher Education",
              "education"  # lowercase version
            )
            
            is_education <- any(str_detect(tolower(page_text), tolower(education_indicators)))
            
            # More flexible chamber detection
            is_correct_chamber <- if(current_chamber == "house") {
              str_detect(tolower(page_text), "house|hstedu")
            } else {
              str_detect(tolower(page_text), "senate|sstedu")
            }
            
            # For debugging - let's see what we found
            if(!is_education || !is_correct_chamber) {
              cat(" ⚠️ DEBUG - Education:", is_education, "Chamber:", is_correct_chamber, "\n")
              # Show a snippet of page text for debugging
              snippet <- substr(page_text, 1, 200)
              cat("     Page snippet:", snippet, "\n")
            }
            
            if(is_education && is_correct_chamber) {
              cat(" ✓ VERIFIED\n")
              
              # Extract meeting date if possible
              meeting_date <- NA
              date_match <- str_extract(page_text, "\\d{1,2}/\\d{1,2}/\\d{4}")
              if(!is.na(date_match)) {
                meeting_date <- date_match
              }
              
              chamber_meetings <- bind_rows(chamber_meetings, tibble(
                meeting_index = nrow(chamber_meetings) + 1,
                link_text = "Audio/Video",
                mtg_id = as.character(id),
                committee_archive_url = test_url,
                year = year,
                chamber = current_chamber,
                committee = "education",
                meeting_date = meeting_date,
                verified = TRUE
              ))
            } else {
              cat(" ✗ Not Education Committee or Wrong Chamber\n")
            }
            
            success <- TRUE  # Mark as successful
            
          } else {
            # Close the connection for non-200 responses too
            try(close(response), silent = TRUE)
            cat(" ✗ Not accessible (", status_code(response), ")\n")
            success <- TRUE  # Don't retry for 404s, etc.
          }
          
        }, error = function(e) {
          retry_count <- retry_count + 1
          if(retry_count < max_retries) {
            cat(paste(" ⚠️ Connection error, retry", retry_count, "of", max_retries - 1, "..."))
            Sys.sleep(2)  # Wait 2 seconds before retry
          } else {
            cat(" ✗ Connection failed after", max_retries, "attempts:", e$message, "\n")
            success <- TRUE  # Stop retrying
          }
        })
        
        # Force garbage collection every 10 requests
        if(i %% 10 == 0) {
          gc()
        }
      }
      
      Sys.sleep(0.3)  # Slightly longer delay between requests
    }
    
    cat("  Verified", nrow(chamber_meetings), "Education committee meetings for", 
        toupper(current_chamber), "in", year, "\n")
    
    all_meetings <- bind_rows(all_meetings, chamber_meetings)
  }
  
  cat("\nTotal verified Education committee meetings for", year, ":", nrow(all_meetings), "\n")
  
  if(nrow(all_meetings) > 0) {
    cat("Summary by chamber:\n")
    summary_by_chamber <- all_meetings %>% 
      group_by(chamber) %>% 
      summarise(
        count = n(),
        id_list = paste(sort(as.numeric(mtg_id)), collapse = ", "),
        .groups = "drop"
      )
    
    for(i in 1:nrow(summary_by_chamber)) {
      cat("  ", toupper(summary_by_chamber$chamber[i]), ":", summary_by_chamber$count[i], 
          "meetings (IDs:", summary_by_chamber$id_list[i], ")\n")
    }
  }
  
  return(all_meetings)
}

# Function to analyze the MP4 filename pattern
utah_scraper$analyze_filename_pattern <- function() {
  cat("=== Analyzing Utah MP4 Filename Pattern ===\n")
  
  # Example: rE120_V214_022324_02.mp4
  example_file <- "rE120_V214_022324_02.mp4"
  
  cat("Example filename:", example_file, "\n")
  cat("Pattern analysis:\n")
  cat("  - Prefix: 'r' (possibly 'recording')\n")
  cat("  - Room/Committee: 'E120' (likely Education committee room)\n")
  cat("  - Video ID: 'V214' (video sequence number)\n")
  cat("  - Date: '022324' (likely MM/DD/YY format = 02/23/24)\n")
  cat("  - Part: '02' (meeting part/segment)\n")
  cat("  - Extension: '.mp4'\n\n")
  
  # Parse the date
  date_part <- "022324"
  month <- substr(date_part, 1, 2)
  day <- substr(date_part, 3, 4)
  year <- paste0("20", substr(date_part, 5, 6))
  
  cat("Parsed date:", month, "/", day, "/", year, "\n")
  
  return(list(
    pattern = "r[ROOM]_V[VIDEO_ID]_[MMDDYY]_[PART].mp4",
    base_url = "https://le.utah.gov/MP4Video/HiRes/",
    example_date = paste0(year, "-", month, "-", day)
  ))
}

# Function to extract video information from committee archive page source (updated for audio)
utah_scraper$extract_video_info <- function(mtg_id) {
  archive_url <- paste0("https://le.utah.gov/av/committeeArchive.jsp?mtgID=", mtg_id)
  
  cat("Extracting video info from:", archive_url, "\n")
  
  tryCatch({
    page <- read_html(archive_url)
    page_source <- as.character(page)
    
    # Look for MP4 file references in the page source (both video and audio)
    mp4_matches <- str_extract_all(page_source, "r[A-Z0-9]+_[V]?[0-9]+_[0-9]+_[0-9]+\\.mp4")[[1]]
    
    # Look for both MP4Video and MP4Audio directory references
    mp4_dir_matches <- str_extract_all(page_source, "/MP4(Video|Audio)/[^\"'\\s]+\\.mp4")[[1]]
    
    cat("MP4 filename patterns found:", length(mp4_matches), "\n")
    cat("MP4 directory paths found:", length(mp4_dir_matches), "\n")
    
    if(length(mp4_matches) > 0) {
      cat("MP4 filenames:\n")
      for(file in mp4_matches) {
        cat("  -", file, "\n")
      }
    }
    
    if(length(mp4_dir_matches) > 0) {
      cat("MP4 directory paths:\n")
      for(path in mp4_dir_matches) {
        cat("  -", path, "\n")
      }
    }
    
    return(list(
      mtg_id = mtg_id,
      mp4_filenames = mp4_matches,
      mp4_paths = mp4_dir_matches
    ))
    
  }, error = function(e) {
    cat("Error extracting video info:", e$message, "\n")
    return(list(mtg_id = mtg_id, error = e$message))
  })
}

# Function to construct direct MP4 URLs (updated with exact room codes by year)
utah_scraper$construct_mp4_urls <- function(meeting_id, meeting_date = NULL, year = NULL, chamber = NULL) {
  cat("=== Constructing MP4 URLs for Meeting", meeting_id, "===\n")
  
  # Determine URL pattern based on year and chamber
  if(!is.null(year) && year <= 2020) {
    # 2019-2020: Both chambers use MP4Audio with different room codes
    base_url <- "https://le.utah.gov/MP4Audio/"
    suffix <- ".mp4"
    if(!is.null(chamber) && chamber == "senate") {
      room_code <- "E215"
      cat("Using 2019-2020 SENATE AUDIO URLs (E215) for year", year, "\n")
    } else {
      room_code <- "W030"
      cat("Using 2019-2020 HOUSE AUDIO URLs (W030) for year", year, "\n")
    }
  } else {
    # 2021+ All use MP4Video with 240p suffix but different room codes
    base_url <- "https://le.utah.gov/MP4Video/"
    suffix <- "_240p.mp4"
    
    # Determine room code by year and chamber
    if(!is.null(chamber) && chamber == "senate") {
      if(year >= 2023) {
        room_code <- "E210"  # Senate 2023-2024
      } else {
        room_code <- "E215"  # Senate 2021-2022
      }
      cat("Using SENATE LOW QUALITY (240p) URLs with room", room_code, "for year", year, "\n")
    } else {
      # House room codes by year
      if(year == 2021) {
        room_code <- "O211"
      } else if(year == 2022) {
        room_code <- "E120"
      } else {  # 2023-2024
        room_code <- "W030"
      }
      cat("Using HOUSE LOW QUALITY (240p) URLs with room", room_code, "for year", year, "\n")
    }
  }
  
  # If we have meeting date, construct the filename
  if(!is.null(meeting_date)) {
    # Convert date to MMDDYY format
    if(is.character(meeting_date)) {
      meeting_date <- as.Date(meeting_date)
    }
    
    date_formatted <- format(meeting_date, "%m%d%y")
    
    # Video ID ranges - different patterns by year and chamber
    if(!is.null(year) && year <= 2020) {
      # 2019-2020: Simple numbering (no "V" prefix)
      video_ids <- sprintf("%d", 100:130)
    } else if(!is.null(chamber) && chamber == "senate" && year <= 2022) {
      # 2021-2022 Senate: Simple numbering
      video_ids <- sprintf("%d", 200:220)
    } else if(!is.null(chamber) && chamber == "senate" && year >= 2023) {
      # 2023-2024 Senate: Different pattern
      video_ids <- sprintf("V%d", 15:25)  # V15 to V25
    } else {
      # All House 2021+: "V" prefix
      video_ids <- sprintf("V%03d", 200:220)  # V200 to V220
    }
    
    # Try different parts
    parts <- sprintf("%02d", 1:3)  # 01 to 03
    
    possible_urls <- c()
    
    for(vid_id in video_ids) {
      for(part in parts) {
        filename <- paste0("r", room_code, "_", vid_id, "_", date_formatted, "_", part, suffix)
        full_url <- paste0(base_url, filename)
        possible_urls <- c(possible_urls, full_url)
      }
    }
    
    cat("Generated", length(possible_urls), "possible URLs\n")
    cat("Sample URLs:\n")
    for(url in head(possible_urls, 3)) {
      cat("  -", url, "\n")
    }
    
    return(possible_urls)
  } else {
    cat("No meeting date provided - cannot construct specific URLs\n")
    return(c())
  }
}

# Function to test if MP4 URLs are accessible
utah_scraper$test_mp4_urls <- function(urls) {
  cat("=== Testing MP4 URL Accessibility ===\n")
  
  results <- map_dfr(head(urls, 15), function(url) {  # Test first 15 only
    cat("Testing:", basename(url), "...")
    
    tryCatch({
      response <- HEAD(url, timeout(5))
      status <- status_code(response)
      
      if(status == 200) {
        content_length <- headers(response)$`content-length`
        cat(" ✓ FOUND (", content_length, " bytes)\n")
        
        tibble(
          url = url,
          filename = basename(url),
          accessible = TRUE,
          status_code = status,
          file_size = content_length
        )
      } else {
        cat(" ✗ (", status, ")\n")
        tibble(
          url = url,
          filename = basename(url),
          accessible = FALSE,
          status_code = status,
          file_size = NA
        )
      }
    }, error = function(e) {
      cat(" ✗ Error\n")
      tibble(
        url = url,
        filename = basename(url),
        accessible = FALSE,
        status_code = NA,
        file_size = NA,
        error = e$message
      )
    })
  })
  
  accessible_files <- results %>% filter(accessible == TRUE)
  
  if(nrow(accessible_files) > 0) {
    cat("\n🎉 Found", nrow(accessible_files), "accessible MP4 files!\n")
    print(accessible_files)
  } else {
    cat("\n❌ No accessible MP4 files found in test URLs\n")
  }
  
  return(results)
}

# Updated MP4 discovery function (all use 240p in MP4Video for 2021+)
utah_scraper$discover_mp4_files <- function(mtg_id, test_date = "2024-02-23", year = NULL, chamber = NULL) {
  cat("=== Direct MP4 Discovery for Meeting", mtg_id, "===\n")
  
  # Step 1: Extract any video info from the page source
  video_info <- utah_scraper$extract_video_info(mtg_id)
  
  # Step 2: If we found filenames in the source, use those
  if(length(video_info$mp4_filenames) > 0) {
    cat("\n✓ Found MP4 filenames in page source!\n")
    
    # Use low quality for all years and chambers
    if(!is.null(year) && year <= 2020) {
      # 2019-2020: Both chambers use MP4Audio (no 240p suffix)
      direct_urls <- paste0("https://le.utah.gov/MP4Audio/", video_info$mp4_filenames)
      cat("Using 2019-2020 AUDIO URLs (no 240p) for year", year, "chamber", chamber, "\n")
    } else {
      # 2021+ All chambers use MP4Video with 240p suffix
      direct_urls <- paste0("https://le.utah.gov/MP4Video/", 
                           gsub("\\.mp4$", "_240p.mp4", video_info$mp4_filenames))
      cat("Using", year, toupper(chamber), "LOW QUALITY (240p) URLs\n")
    }
    
    cat("Testing discovered URLs:\n")
    for(url in direct_urls) {
      cat("  -", url, "\n")
    }
    
    test_results <- utah_scraper$test_mp4_urls(direct_urls)
    return(test_results)
  }
  
  # Step 3: If no filenames found, try constructing based on date pattern
  cat("\n⚠️ No MP4 filenames found in page source. Trying pattern matching...\n")
  possible_urls <- utah_scraper$construct_mp4_urls(mtg_id, test_date, year, chamber)
  
  if(length(possible_urls) > 0) {
    test_results <- utah_scraper$test_mp4_urls(possible_urls)
    return(test_results)
  }
  
  return(tibble())
}

# Function to save meeting metadata
utah_scraper$save_metadata <- function(meetings_df, filename = NULL) {
  if(is.null(filename)) {
    filename <- paste0("utah_meetings_metadata_", Sys.Date(), ".csv")
  }
  
  filepath <- file.path(CONFIG$project$data_sources$legislative$raw, "UT", filename)
  dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
  
  write.csv(meetings_df, filepath, row.names = FALSE)
  cat("Metadata saved to:", filepath, "\n")
  
  return(filepath)
}

# Fixed download function with better file verification
utah_scraper$download_mp4_files <- function(mp4_data, download_dir = NULL, max_downloads = NULL, timeout_minutes = 30) {
  cat("=== Downloading MP4 Files ===\n")
  
  # Set up download directory
  if(is.null(download_dir)) {
    download_dir <- file.path(CONFIG$project$data_sources$legislative$raw, "UT", "audio")
  }
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Filter to accessible files only
  downloadable_files <- mp4_data %>% 
    filter(accessible == TRUE) %>%
    distinct(url, .keep_all = TRUE)  # Remove duplicates
  
  if(nrow(downloadable_files) == 0) {
    cat("❌ No accessible MP4 files to download\n")
    return(tibble())
  }
  
  # Limit downloads if specified
  if(!is.null(max_downloads) && nrow(downloadable_files) > max_downloads) {
    cat("Limiting downloads to", max_downloads, "files\n")
    downloadable_files <- downloadable_files[1:max_downloads, ]
  }
  
  cat("Downloading", nrow(downloadable_files), "MP4 files to:", download_dir, "\n")
  cat("Timeout set to:", timeout_minutes, "minutes per file\n\n")
  
  download_results <- map_dfr(1:nrow(downloadable_files), function(i) {
    file_info <- downloadable_files[i, ]
    url <- file_info$url
    original_filename <- file_info$filename
    
    # Create standardized filename with meeting ID and chamber info
    mtg_id <- file_info$mtg_id
    chamber <- if("chamber" %in% names(file_info)) file_info$chamber else "unknown"
    year <- if("year" %in% names(file_info)) file_info$year else "unknown"
    
    committee_code <- if(chamber == "house") "HSTEDU" else if(chamber == "senate") "SSTEDU" else "EDU"
    
    file_extension <- tools::file_ext(original_filename)
    new_filename <- paste0("UT_", committee_code, "_", year, "_meeting_", mtg_id, "_", original_filename)
    
    local_filepath <- file.path(download_dir, new_filename)
    
    cat("Downloading", i, "of", nrow(downloadable_files), ":", new_filename, "\n")
    cat("  URL:", url, "\n")
    
    # Get expected file size for progress tracking
    expected_size_bytes <- as.numeric(file_info$file_size)
    expected_size_mb <- round(expected_size_bytes / 1024^2, 1)
    cat("  Expected size:", expected_size_mb, "MB\n")
    
    start_time <- Sys.time()
    
    # Skip if file already exists and is correct size
    if(file.exists(local_filepath)) {
      existing_size <- file.size(local_filepath)
      if(abs(existing_size - expected_size_bytes) < (expected_size_bytes * 0.05)) {
        cat("  ✓ File already exists with correct size, skipping\n")
        actual_size_mb <- round(existing_size / 1024^2, 1)
        
        return(tibble(
          mtg_id = mtg_id,
          chamber = chamber,
          year = year,
          original_filename = original_filename,
          new_filename = new_filename,
          local_filepath = local_filepath,
          download_success = TRUE,
          file_size_mb = actual_size_mb,
          expected_size_mb = expected_size_mb,
          download_time_minutes = 0,
          download_date = Sys.Date(),
          source_url = url,
          note = "File already existed"
        ))
      }
    }
    
    tryCatch({
      # Set longer timeout and download
      old_timeout <- getOption("timeout")
      options(timeout = timeout_minutes * 60)  # Convert to seconds
      
      download.file(url, local_filepath, mode = "wb", quiet = FALSE)
      
      # Restore original timeout
      options(timeout = old_timeout)
      
      end_time <- Sys.time()
      download_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
      
      # Wait a moment for file system to catch up
      Sys.sleep(1)
      
      # Verify download with retry logic
      max_retries <- 5
      file_found <- FALSE
      actual_file_size <- 0
      
      for(retry in 1:max_retries) {
        if(file.exists(local_filepath)) {
          actual_file_size <- file.size(local_filepath)
          if(actual_file_size > 0) {
            file_found <- TRUE
            break
          }
        }
        cat("    Retry", retry, "- waiting for file...\n")
        Sys.sleep(2)  # Wait 2 seconds between retries
      }
      
      if(file_found) {
        actual_size_mb <- round(actual_file_size / 1024^2, 1)
        
        # Check if download completed successfully (allow 5% variance)
        size_match <- abs(actual_file_size - expected_size_bytes) < (expected_size_bytes * 0.05)
        
        if(size_match || actual_file_size >= expected_size_bytes * 0.90) {  # At least 90% of expected size
          cat("  ✓ Downloaded successfully:", actual_size_mb, "MB in", round(download_time, 1), "minutes\n")
          
          tibble(
            mtg_id = mtg_id,
            chamber = chamber,
            year = year,
            original_filename = original_filename,
            new_filename = new_filename,
            local_filepath = local_filepath,
            download_success = TRUE,
            file_size_mb = actual_size_mb,
            expected_size_mb = expected_size_mb,
            download_time_minutes = download_time,
            download_date = Sys.Date(),
            source_url = url
          )
        } else {
          cat("  ⚠️ Partial download:", actual_size_mb, "MB of", expected_size_mb, "MB expected\n")
          
          tibble(
            mtg_id = mtg_id,
            chamber = chamber,
            year = year,
            original_filename = original_filename,
            new_filename = new_filename,
            local_filepath = local_filepath,
            download_success = FALSE,
            file_size_mb = actual_size_mb,
            expected_size_mb = expected_size_mb,
            download_time_minutes = download_time,
            download_date = Sys.Date(),
            source_url = url,
            error = "Partial download - file incomplete"
          )
        }
      } else {
        cat("  ✗ Download failed - file not found after", max_retries, "retries\n")
        
        tibble(
          mtg_id = mtg_id,
          chamber = chamber,
          year = year,
          original_filename = original_filename,
          new_filename = new_filename,
          download_success = FALSE,
          file_size_mb = 0,
          expected_size_mb = expected_size_mb,
          download_time_minutes = download_time,
          error = paste("File not found after", max_retries, "retries"),
          source_url = url
        )
      }
      
    }, error = function(e) {
      cat("  ✗ Download error:", e$message, "\n")
      
      # Check if partial file exists
      partial_size_mb <- 0
      if(file.exists(local_filepath)) {
        partial_size_mb <- round(file.size(local_filepath) / 1024^2, 1)
        cat("    Partial file size:", partial_size_mb, "MB\n")
      }
      
      tibble(
        mtg_id = mtg_id,
        chamber = chamber,
        year = year,
        original_filename = original_filename,
        new_filename = new_filename,
        download_success = FALSE,
        file_size_mb = partial_size_mb,
        expected_size_mb = expected_size_mb,
        error = e$message,
        source_url = url
      )
    })
  })
  
  # Summary
  successful_downloads <- sum(download_results$download_success, na.rm = TRUE)
  total_size_mb <- sum(download_results$file_size_mb, na.rm = TRUE)
  
  cat("\n📊 Download Summary:\n")
  cat("Successful downloads:", successful_downloads, "/", nrow(downloadable_files), "\n")
  cat("Total size downloaded:", round(total_size_mb, 1), "MB\n")
  cat("Files saved to:", download_dir, "\n")
  
  # Show failed downloads
  failed_downloads <- download_results %>% filter(download_success == FALSE)
  if(nrow(failed_downloads) > 0) {
    cat("\n❌ Failed downloads:\n")
    for(i in 1:nrow(failed_downloads)) {
      cat("  -", failed_downloads$original_filename[i], ":", failed_downloads$error[i], "\n")
    }
  }
  
  # Save download log
  download_log_path <- file.path(download_dir, paste0("download_log_", Sys.Date(), ".csv"))
  write.csv(download_results, download_log_path, row.names = FALSE)
  cat("Download log saved to:", download_log_path, "\n")
  
  return(download_results)
}

# Multi-year workflow for complete data collection with precise ranges
utah_scraper$complete_multi_year_workflow <- function(years = 2019:2024, chamber = "both") {
  cat("╔══════════════════════════════════════════════════════════╗\n")
  cat("║      COMPLETE MULTI-YEAR MP4 WORKFLOW (PRECISE RANGES)  ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")
  
  all_results <- list()
  total_meetings <- 0
  total_mp4s <- 0
  
  for(year in years) {
    cat("\n", rep("=", 60), "\n")
    cat("PROCESSING YEAR:", year, "\n")
    cat(rep("=", 60), "\n")
    
    # Get meetings for this year using exact IDs
    meetings <- utah_scraper$get_education_meetings_by_exact_ids(year, chamber)
    
    if(nrow(meetings) > 0) {
      cat("\nDiscovering MP4 files for", nrow(meetings), "meetings in", year, "...\n")
      
      year_mp4_results <- tibble()
      
      for(i in 1:nrow(meetings)) {
        mtg_id <- meetings$mtg_id[i]
        chamber_info <- meetings$chamber[i]
        cat("  Processing", year, chamber_info, "meeting", i, "of", nrow(meetings), "(ID:", mtg_id, ")\n")
        
        chamber_info <- meetings$chamber[i]
        mp4_results <- utah_scraper$discover_mp4_files(mtg_id, year = year, chamber = chamber_info)
        
        if(nrow(mp4_results) > 0) {
          mp4_results$meeting_index <- i
          mp4_results$mtg_id <- mtg_id
          mp4_results$year <- year
          mp4_results$chamber <- chamber_info
          year_mp4_results <- bind_rows(year_mp4_results, mp4_results)
        }
        
        Sys.sleep(0.5)
      }
      
      # Save year-specific results
      if(nrow(year_mp4_results) > 0) {
        utah_scraper$save_metadata(year_mp4_results, paste0("mp4_files_", year, ".csv"))
        utah_scraper$save_metadata(meetings, paste0("meetings_metadata_", year, ".csv"))
        
        accessible_count <- sum(year_mp4_results$accessible, na.rm = TRUE)
        cat("\n📊", year, "SUMMARY:\n")
        cat("  Meetings found:", nrow(meetings), "\n")
        
        # Summary by chamber
        chamber_summary <- meetings %>% 
          group_by(chamber) %>% 
          summarise(count = n(), .groups = "drop")
        
        for(i in 1:nrow(chamber_summary)) {
          cat("    ", toupper(chamber_summary$chamber[i]), ":", chamber_summary$count[i], "meetings\n")
        }
        
        cat("  MP4 files found:", nrow(year_mp4_results), "\n")
        cat("  Accessible files:", accessible_count, "\n")
        
        total_meetings <- total_meetings + nrow(meetings)
        total_mp4s <- total_mp4s + accessible_count
        
        all_results[[as.character(year)]] <- list(
          meetings = meetings,
          mp4_results = year_mp4_results
        )
      }
    } else {
      cat("No Education committee meetings found for", year, "\n")
    }
  }
  
  # Overall summary
  cat("\n", rep("=", 60), "\n")
  cat("🎉 COMPLETE PROJECT SUMMARY\n")
  cat(rep("=", 60), "\n")
  cat("Years processed:", paste(years, collapse = ", "), "\n")
  cat("Total meetings found:", total_meetings, "\n")
  cat("Total accessible MP4 files:", total_mp4s, "\n")
  
  # Create master dataset
  if(length(all_results) > 0) {
    master_mp4_data <- map_dfr(all_results, function(year_data) {
      year_data$mp4_results
    })
    
    master_meetings_data <- map_dfr(all_results, function(year_data) {
      year_data$meetings
    })
    
    utah_scraper$save_metadata(master_mp4_data, "master_mp4_dataset_all_years.csv")
    utah_scraper$save_metadata(master_meetings_data, "master_meetings_dataset_all_years.csv")
    
    cat("\n📁 Files saved:\n")
    cat("  - Individual year files: mp4_files_YYYY.csv, meetings_metadata_YYYY.csv\n")
    cat("  - Master datasets: master_mp4_dataset_all_years.csv, master_meetings_dataset_all_years.csv\n")
    
    # Final summary by chamber and year
    cat("\n📊 DETAILED BREAKDOWN:\n")
    summary_table <- master_meetings_data %>%
      group_by(year, chamber) %>%
      summarise(meetings = n(), .groups = "drop") %>%
      arrange(year, chamber)
    
    print(summary_table)
  }
  
  return(all_results)
}

# Complete workflow with automatic downloads using precise ranges
utah_scraper$complete_workflow_with_downloads <- function(years = 2024, chamber = "both", download_files = TRUE, max_downloads = NULL) {
  cat("╔══════════════════════════════════════════════════════════╗\n")
  cat("║    COMPLETE UTAH WORKFLOW WITH DOWNLOADS (PRECISE)      ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")
  
  all_results <- list()
  total_downloads <- tibble()
  
  for(year in years) {
    cat("🔍 PROCESSING YEAR:", year, "\n")
    
    # Step 1: Find meetings using exact IDs
    meetings <- utah_scraper$get_education_meetings_by_exact_ids(year, chamber)
    
    if(nrow(meetings) == 0) {
      cat("❌ No meetings found for", year, "\n")
      next
    }
    
    # Step 2: Discover MP4 files
    cat("\n🎬 Discovering MP4 Files for", nrow(meetings), "meetings...\n")
    all_mp4_data <- tibble()
    
    for(i in 1:nrow(meetings)) {
      mtg_id <- meetings$mtg_id[i]
      chamber_info <- meetings$chamber[i]
      cat("  Processing", chamber_info, "meeting", i, "of", nrow(meetings), "(ID:", mtg_id, ")\n")
      
      chamber_info <- meetings$chamber[i]
      mp4_results <- utah_scraper$discover_mp4_files(mtg_id, year = year, chamber = chamber_info)
      
      if(nrow(mp4_results) > 0) {
        mp4_results$mtg_id <- mtg_id
        mp4_results$chamber <- chamber_info
        mp4_results$year <- year
        all_mp4_data <- bind_rows(all_mp4_data, mp4_results)
      }
      
      Sys.sleep(0.5)
    }
    
    # Step 3: Save metadata
    cat("\n💾 Saving Metadata for", year, "...\n")
    if(nrow(all_mp4_data) > 0) {
      utah_scraper$save_metadata(all_mp4_data, paste0("utah_mp4_metadata_", year, ".csv"))
      utah_scraper$save_metadata(meetings, paste0("utah_meetings_metadata_", year, ".csv"))
    }
    
    # Step 4: Download files (optional)
    year_downloads <- tibble()
    if(download_files && nrow(all_mp4_data) > 0) {
      cat("\n⬇️ Downloading MP4 Files for", year, "...\n")
      
      if(!is.null(max_downloads)) {
        cat("Note: Downloads limited to", max_downloads, "files for testing\n")
      }
      
      year_downloads <- utah_scraper$download_mp4_files(all_mp4_data, max_downloads = max_downloads)
      total_downloads <- bind_rows(total_downloads, year_downloads)
    } else if(!download_files) {
      cat("\n⏭️ Skipping Downloads for", year, "(download_files = FALSE)\n")
    }
    
    # Store results for this year
    all_results[[as.character(year)]] <- list(
      meetings = meetings,
      mp4_data = all_mp4_data,
      downloads = year_downloads
    )
    
    # Year summary
    cat("\n📊", year, "SUMMARY:\n")
    cat("Meetings found:", nrow(meetings), "\n")
    cat("MP4 files discovered:", nrow(all_mp4_data), "\n")
    cat("Accessible files:", sum(all_mp4_data$accessible, na.rm = TRUE), "\n")
    
    if(nrow(year_downloads) > 0) {
      cat("Files downloaded:", sum(year_downloads$download_success, na.rm = TRUE), "\n")
      cat("Total size:", round(sum(year_downloads$file_size_mb, na.rm = TRUE), 1), "MB\n")
    }
  }
  
  # Final summary across all years
  cat("\n", rep("=", 60), "\n")
  cat("🎉 FINAL SUMMARY ACROSS ALL YEARS\n")
  cat(rep("=", 60), "\n")
  
  total_meetings_all <- sum(sapply(all_results, function(x) nrow(x$meetings)))
  total_mp4s_all <- sum(sapply(all_results, function(x) sum(x$mp4_data$accessible, na.rm = TRUE)))
  total_downloads_all <- sum(total_downloads$download_success, na.rm = TRUE)
  total_size_all <- sum(total_downloads$file_size_mb, na.rm = TRUE)
  
  cat("Years processed:", paste(years, collapse = ", "), "\n")
  cat("Total meetings found:", total_meetings_all, "\n")
  cat("Total accessible MP4 files:", total_mp4s_all, "\n")
  
  if(nrow(total_downloads) > 0) {
    cat("Total files downloaded:", total_downloads_all, "\n")
    cat("Total size downloaded:", round(total_size_all, 1), "MB\n")
    
    # Save master download log
    master_download_log <- file.path(CONFIG$project$data_sources$legislative$raw, "UT", "audio", "master_download_log.csv")
    write.csv(total_downloads, master_download_log, row.names = FALSE)
    cat("Master download log saved to:", master_download_log, "\n")
  }
  
  return(all_results)
}

# Function for batch downloading from saved metadata
utah_scraper$download_from_metadata <- function(metadata_file, max_downloads = NULL) {
  cat("=== Downloading from Saved Metadata ===\n")
  
  # Load metadata
  if(file.exists(metadata_file)) {
    mp4_data <- read.csv(metadata_file)
    cat("Loaded", nrow(mp4_data), "records from", metadata_file, "\n")
    
    # Download the files
    download_results <- utah_scraper$download_mp4_files(mp4_data, max_downloads = max_downloads)
    
    return(download_results)
  } else {
    cat("❌ Metadata file not found:", metadata_file, "\n")
    return(tibble())
  }
}

# Convenience function to test with single year
utah_scraper$test_single_year <- function(year = 2024, chamber = "both") {
  cat("🧪 Testing with single year:", year, "\n")
  cat("Chamber:", chamber, "\n\n")
  
  # Just find meetings without downloads
  meetings <- utah_scraper$get_education_meetings_by_exact_ids(year, chamber)
  
  if(nrow(meetings) > 0) {
    cat("\n✅ Test successful! Found", nrow(meetings), "meetings\n")
    cat("Meeting IDs:", paste(meetings$mtg_id, collapse = ", "), "\n")
    
    # Test MP4 discovery on first meeting
    if(nrow(meetings) > 0) {
      cat("\n🎬 Testing MP4 discovery on first meeting...\n")
      test_mp4 <- utah_scraper$discover_mp4_files(meetings$mtg_id[1])
      
      if(nrow(test_mp4) > 0) {
        cat("✅ MP4 discovery test successful!\n")
        accessible_count <- sum(test_mp4$accessible, na.rm = TRUE)
        cat("Found", accessible_count, "accessible MP4 files\n")
      }
      
      return(list(meetings = meetings, mp4_test = test_mp4))
    }
  } else {
    cat("❌ Test failed - no meetings found\n")
    return(list())
  }
}

cat("Utah Legislative Web Scraper with Exact Meeting IDs loaded!\n")
cat("🎯 Quick test: utah_scraper$test_single_year(2024)\n")
cat("🎯 Find meetings only: utah_scraper$get_education_meetings_by_exact_ids(2024, 'both')\n")
cat("🎯 Complete workflow: utah_scraper$complete_multi_year_workflow(2019:2024)\n")
cat("🎯 With downloads: utah_scraper$complete_workflow_with_downloads(2024, max_downloads=2)\n")