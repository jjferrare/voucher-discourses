# School Choice Policy Discourse Analysis

This repository contains code and documentation for analyzing policy discourses around school choice before, during, and after the COVID-19 pandemic.

## Project Overview

This project examines changes in policy discourses concerning private school choice as they unfolded within and between states prior to, during, and after the COVID-19 pandemic. The analysis incorporates multiple data sources:

- State legislative committee meetings
- School board meetings (from DistrictView)
- Policy documents from advocacy organizations
- Social media data (Twitter and Facebook)

## Current Status

The project is in its pilot phase, focusing on state legislative committee meetings from Utah. The pilot will establish procedures and code infrastructure that will be expanded to include additional states and data sources.

## Repository Structure

```
project_root/
├── setup/                      # Setup and configuration files
│   ├── config.R               # Project configuration
│   └── 01_directory_setup.Rmd # Initial setup script
├── data/                      # Data directory (not tracked in git)
│   ├── raw/                   # Raw data by source
│   │   ├── legislative/       # Legislative meeting recordings
│   │   ├── district/         # School board meetings
│   │   ├── advocacy/         # Policy documents
│   │   └── social_media/     # Twitter/Facebook data
│   ├── processed/            # Processed data by source
│   └── metadata/             # Metadata tracking
├── code/                      # Analysis and processing scripts
│   ├── collection/           # Data collection scripts
│   ├── processing/           # Data processing scripts
│   └── analysis/             # Analysis scripts
└── docs/                      # Documentation
    └── pilot/                # Pilot phase documentation
```

## Setup Instructions

### Prerequisites

Required R packages:
```r
install.packages(c(
  "tidyverse",  # Data manipulation and visualization
  "fs",         # File system operations
  "here",       # Project relative paths
  "glue"        # String interpolation
))
```

### Initial Setup

1. Clone this repository:
```bash
git clone https://github.com/yourusername/yourrepo.git
cd yourrepo
```

2. Open the project in RStudio:
   - File → Open Project
   - Navigate to the cloned repository
   - Select the .Rproj file

3. Run the setup script:
   - Open setup/01_directory_setup.Rmd
   - Run all chunks to create directory structure and initialize metadata files

4. Verify setup:
   - Check console output for any errors
   - Verify directory structure matches expected layout
   - Confirm metadata files were created successfully

## Data Collection

### Legislative Meetings
- Current focus: Utah House Education Committee meetings
- Date range: 2019-2024
- Audio format: MP4
- Metadata tracking includes:
  - Meeting dates
  - Topics discussed
  - Bills considered
  - Download status

## Analysis Pipeline

1. Data Collection
   - Web scraping of legislative websites
   - Audio file downloads
   - Metadata extraction

2. Processing
   - Audio transcription (Whisper)
   - Speaker diarization (Picovoice Falcon)
   - Text preprocessing

3. Analysis
   - Topic modeling
   - Discourse network analysis
   - Temporal analysis

## Contributing

1. Create a new branch for each feature
2. Follow existing code style and documentation practices
3. Update documentation as needed
4. Submit pull requests for review

## Contact

Joe Ferrare: jferrare@uw.edu

## License



## Acknowledgments


