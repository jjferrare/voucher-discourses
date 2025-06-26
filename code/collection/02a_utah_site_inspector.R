# 02a_utah_site_inspector.R
# Utah Legislative Website Structure Inspector
# This script helps analyze the actual structure of the Utah legislative website
# to refine our scraping approach

library(rvest)
library(httr)
library(xml2)
library(dplyr)

# Function to explore Utah legislative website structure
inspect_utah_site <- function() {
  cat("=== Utah Legislative Website Inspector ===\n\n")
  
  # Base URLs to explore - CORRECTED for actual Utah legislative website
  base_urls <- c(
    "https://le.utah.gov",
    "https://le.utah.gov/committee/committee.jsp?year=2024&com=HSTEDU",  # House Education 2024
    "https://le.utah.gov/committee/committee.jsp?year=2023&com=HSTEDU",  # House Education 2023
    "https://le.utah.gov/committee/committee.jsp?year=2022&com=HSTEDU",  # House Education 2022
    "https://le.utah.gov/committee/committee.jsp?year=2021&com=HSTEDU",  # House Education 2021
    "https://le.utah.gov/committee/committee.jsp?year=2020&com=HSTEDU"   # House Education 2020
  )
  
  for(url in base_urls) {
    cat("Inspecting:", url, "\n")
    cat(rep("-", 50), "\n")
    
    tryCatch({
      page <- read_html(url)
      
      # Get page title
      title <- page %>% html_node("title") %>% html_text()
      cat("Page Title:", str_trim(title), "\n")
      
      # Look for navigation or committee links
      nav_links <- page %>% 
        html_nodes("a") %>% 
        html_attr("href") %>%
        .[str_detect(., "committee|education")] %>%
        head(10)
      
      if(length(nav_links) > 0) {
        cat("Committee-related links found:\n")
        for(link in nav_links) {
          cat("  -", link, "\n")
        }
      }
      
      # Look for meeting or audio links
      meeting_links <- page %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        .[str_detect(., "meeting|audio|video|minutes|agenda")] %>%
        head(5)
      
      if(length(meeting_links) > 0) {
        cat("Meeting-related links found:\n")
        for(link in meeting_links) {
          cat("  -", link, "\n")
        }
      }
      
      # Look for date patterns
      date_text <- page %>%
        html_nodes("*") %>%
        html_text() %>%
        .[str_detect(., "\\d{1,2}/\\d{1,2}/\\d{4}|\\d{4}-\\d{2}-\\d{2}|January|February|March|April|May|June|July|August|September|October|November|December")] %>%
        head(3)
      
      if(length(date_text) > 0) {
        cat("Date patterns found:\n")
        for(dt in date_text) {
          cat("  -", str_trim(str_sub(dt, 1, 100)), "\n")
        }
      }
      
    }, error = function(e) {
      cat("Error accessing", url, ":", e$message, "\n")
    })
    
    cat("\n")
    Sys.sleep(1)  # Be respectful
  }
}

# Function to analyze a specific committee page structure
analyze_committee_page <- function(url) {
  cat("=== Detailed Committee Page Analysis ===\n")
  cat("URL:", url, "\n\n")
  
  tryCatch({
    page <- read_html(url)
    
    # Get all CSS classes used on the page
    all_classes <- page %>%
      html_nodes("*[class]") %>%
      html_attr("class") %>%
      str_split("\\s+") %>%
      unlist() %>%
      unique() %>%
      sort()
    
    cat("CSS Classes found (first 20):\n")
    print(head(all_classes, 20))
    
    # Look for meeting-related classes
    meeting_classes <- all_classes[str_detect(all_classes, "meeting|committee|agenda|minutes|audio|video")]
    if(length(meeting_classes) > 0) {
      cat("\nMeeting-related classes:\n")
      print(meeting_classes)
    }
    
    # Get all IDs
    all_ids <- page %>%
      html_nodes("*[id]") %>%
      html_attr("id") %>%
      unique() %>%
      sort()
    
    cat("\nIDs found:\n")
    print(all_ids)
    
    # Look for table structures
    tables <- page %>% html_nodes("table")
    if(length(tables) > 0) {
      cat("\nFound", length(tables), "table(s)\n")
      
      # Analyze first table
      if(length(tables) >= 1) {
        first_table <- tables[[1]]
        headers <- first_table %>% 
          html_nodes("th, thead td") %>% 
          html_text(trim = TRUE)
        
        if(length(headers) > 0) {
          cat("First table headers:\n")
          print(headers)
        }
        
        # Get first few rows
        rows <- first_table %>%
          html_nodes("tr") %>%
          head(3)
        
        cat("First few table rows:\n")
        for(i in seq_along(rows)) {
          row_text <- rows[[i]] %>% html_text(trim = TRUE)
          cat("Row", i, ":", str_sub(row_text, 1, 100), "...\n")
        }
      }
    }
    
    # Look for lists
    lists <- page %>% html_nodes("ul, ol")
    if(length(lists) > 0) {
      cat("\nFound", length(lists), "list(s)\n")
      
      # Check first list
      if(length(lists) >= 1) {
        first_list_items <- lists[[1]] %>%
          html_nodes("li") %>%
          html_text(trim = TRUE) %>%
          head(5)
        
        cat("First list items:\n")
        for(item in first_list_items) {
          cat("  -", str_sub(item, 1, 80), "...\n")
        }
      }
    }
    
  }, error = function(e) {
    cat("Error analyzing page:", e$message, "\n")
  })
}

# Function to test different URL patterns
test_url_patterns <- function() {
  cat("=== Testing URL Patterns ===\n\n")
  
  # Correct URL patterns based on actual Utah legislative website
  url_patterns <- list(
    # Current year patterns - CORRECTED
    list(
      pattern = "https://le.utah.gov/committee/committee.jsp?year=2024&com=HSTEDU",
      description = "2024 House Education Committee"
    ),
    list(
      pattern = "https://le.utah.gov/committee/committee.jsp?year=2023&com=HSTEDU", 
      description = "2023 House Education Committee"
    ),
    list(
      pattern = "https://le.utah.gov/committee/committee.jsp?year=2022&com=HSTEDU",
      description = "2022 House Education Committee"
    ),
    list(
      pattern = "https://le.utah.gov/committee/committee.jsp?year=2021&com=HSTEDU",
      description = "2021 House Education Committee"
    ),
    list(
      pattern = "https://le.utah.gov/committee/committee.jsp?year=2020&com=HSTEDU",
      description = "2020 House Education Committee"
    ),
    list(
      pattern = "https://le.utah.gov/committee/committee.jsp?year=2019&com=HSTEDU",
      description = "2019 House Education Committee"
    )
  )
  
  working_urls <- c()
  
  for(pattern_info in url_patterns) {
    url <- pattern_info$pattern
    desc <- pattern_info$description
    
    cat("Testing:", desc, "\n")
    cat("URL:", url, "\n")
    
    tryCatch({
      response <- GET(url)
      status <- status_code(response)
      
      if(status == 200) {
        cat("✓ SUCCESS (Status: 200)\n")
        working_urls <- c(working_urls, url)
        
        # Quick content check
        page <- read_html(url)
        title <- page %>% html_node("title") %>% html_text()
        cat("  Title:", str_trim(title), "\n")
        
      } else {
        cat("✗ FAILED (Status:", status, ")\n")
      }
      
    }, error = function(e) {
      cat("✗ ERROR:", e$message, "\n")
    })
    
    cat("\n")
    Sys.sleep(1)  # Be respectful
  }
  
  cat("=== Summary ===\n")
  cat("Working URLs found:\n")
  for(url in working_urls) {
    cat("  ✓", url, "\n")
  }
  
  return(working_urls)
}

# Function to extract and test audio/video links
test_media_links <- function(committee_url) {
  cat("=== Testing Media Links ===\n")
  cat("From:", committee_url, "\n\n")
  
  tryCatch({
    page <- read_html(committee_url)
    
    # Look for various types of media links
    all_links <- page %>% html_nodes("a") %>% html_attr("href")
    
    # Filter for potential media links
    media_patterns <- c(
      "\\.mp3", "\\.wav", "\\.m4a",  # Audio
      "\\.mp4", "\\.avi", "\\.mov",  # Video  
      "audio", "video", "recording", "stream"  # General media terms
    )
    
    media_links <- all_links[str_detect(all_links, paste(media_patterns, collapse = "|"))]
    
    if(length(media_links) > 0) {
      cat("Potential media links found:\n")
      for(link in media_links) {
        # Test if link works
        full_url <- if(str_starts(link, "http")) link else paste0("https://le.utah.gov", link)
        
        cat("Testing:", full_url, "\n")
        tryCatch({
          response <- HEAD(full_url)
          status <- status_code(response)
          content_type <- headers(response)$`content-type`
          
          cat("  Status:", status)
          if(!is.null(content_type)) cat(" | Type:", content_type)
          cat("\n")
          
        }, error = function(e) {
          cat("  Error:", e$message, "\n")
        })
        
        Sys.sleep(0.5)  # Be respectful
      }
    } else {
      cat("No obvious media links found.\n")
      cat("This might mean:\n")
      cat("1. Media is embedded differently\n")
      cat("2. Links are generated dynamically\n") 
      cat("3. Different URL structure is used\n")
    }
    
  }, error = function(e) {
    cat("Error testing media links:", e$message, "\n")
  })
}

# Main inspection workflow
run_site_inspection <- function() {
  cat("╔══════════════════════════════════════════════╗\n")
  cat("║        Utah Legislative Site Inspector       ║\n") 
  cat("╚══════════════════════════════════════════════╝\n\n")
  
  # Step 1: General site exploration
  cat("STEP 1: General Site Structure\n")
  inspect_utah_site()
  
  # Step 2: Test URL patterns
  cat("\nSTEP 2: URL Pattern Testing\n")
  working_urls <- test_url_patterns()
  
  # Step 3: Detailed analysis of working URLs
  if(length(working_urls) > 0) {
    cat("\nSTEP 3: Detailed Page Analysis\n")
    analyze_committee_page(working_urls[1])  # Analyze the first working URL
    
    # Step 4: Test for media links
    cat("\nSTEP 4: Media Link Testing\n")
    test_media_links(working_urls[1])
  }
  
  cat("\n", rep("=", 50), "\n", sep = "")
  cat("Site inspection complete!\n")
  cat("Use the findings to adjust the scraper functions.\n")
}

# Print usage instructions
cat("Utah Legislative Site Inspector loaded!\n")
cat("Run: run_site_inspection() to start comprehensive analysis\n")
cat("Or use individual functions:\n")
cat("  - inspect_utah_site()\n")
cat("  - test_url_patterns()\n") 
cat("  - analyze_committee_page(url)\n")
cat("  - test_media_links(url)\n")