# LRW Chinook Summary Project - Development Summary

## Project Overview
Successfully developed an automated data pipeline and interactive Shiny dashboard for Lostine River Weir Chinook salmon monitoring, transitioning from a reactive multi-year selector to a focused current-year dashboard.

## Key Achievements

### 🤖 **Automated Data Pipeline**
- **GitHub Actions workflow** downloads FINS data nightly (6pm-6am PT)
- **API integration** with proper authentication and rate limiting
- **Error logging** and automated data processing
- **Time restrictions respected** (avoid 11pm-3am maintenance window)

### 📊 **Interactive Shiny Dashboard**
- **Non-reactive design** for better performance (loads current year at startup)
- **Dynamic file paths** that work from both root directory (Shiny) and documents/ (Quarto)
- **Error handling** with graceful degradation for missing data
- **Clean UI** with current year focus, removing complex year selection

### 📄 **Integrated Reporting**
- **Quarto document** generates professional PDF reports
- **Consistent parameter naming** (`trap.year` format across all functions)
- **Automated report generation** with dynamic data inclusion

## Critical Solutions Implemented

### **File Structure Organization**
```
project/
├── app.R                           # Shiny dashboard (root directory)
├── documents/LRW-Weekly-Chinook-Summary.qmd
├── R/report_helpers.R              # Core functions
├── github_scripts/                 # API scripts (isolated from Shiny)
│   ├── get_fins_data.R
│   └── nightly_fins_download.R
└── data/
    ├── TrappingData.csv
    └── yearly_estimates.csv
```

### **Dynamic Path Resolution**
**Problem**: Functions needed to work from both Shiny app (root) and Quarto (documents/)
**Solution**: Path detection in all functions:
```r
if (basename(getwd()) == "documents") {
  path <- "../data/file.csv"     # Quarto context
} else {
  path <- "data/file.csv"        # Shiny context
}
```

### **Parameter Naming Consistency**
**Problem**: Mixed parameter naming (`trap_year` vs `trap.year`)
**Solution**: Standardized all functions to use `trap.year` format:
- `get_trap_data(trap.year = year)`
- `prepare_megadf(trap.year = year)`
- `sumGRSMEbrood(trap.year = year)`

### **API Isolation**
**Problem**: API scripts causing environment variable errors in Shiny
**Solution**: Moved API scripts to `github_scripts/` folder, away from auto-sourced R/ directory

## Key Functions Developed

### **Data Processing**
- `get_trap_data()`: Load/clean FINS data with dynamic paths
- `load_yearly_estimates()`: Get current forecasts with path detection
- `calculate_dispositions()`: Generate disposition summaries
- `prepare_megadf()`: Combine catch and flow data for visualization

### **Visualization & Reporting**
- `generate_lrw_megaplot()`: Historical comparison plots with dynamic output paths
- `safe_flextable()`: Handle tables with no-data scenarios
- `sumGRSMEdisp()`: Disposition summary tables
- `sumGRSMEbrood()`: Broodstock collection summaries

## Technical Decisions

### **Simplified Architecture**
- **Non-reactive Shiny app**: Loads current year data once at startup
- **Removed download functionality**: Eliminated Quarto/Shiny compatibility issues
- **Focus on current year**: Simplified UI by removing year selection

### **Error Handling Strategy**
- **Graceful degradation**: Show "no data available" messages instead of errors
- **File existence checks**: Verify data files before processing
- **API isolation**: Prevent local development issues with missing environment variables

### **Data Management Decisions**
- **Removed redundant .rda backups**: Eliminated `fins_data.rda` creation in `get_trap_data()` 
  - Reasoning: CSV source + GitHub backup + nightly refresh provides sufficient data protection
  - Result: Cleaner data flow and reduced storage complexity

## Package Dependencies
```r
# Core Shiny packages
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# Data processing
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# Visualization & reporting
library(ggplot2)
library(flextable)

# Domain-specific
library(cuyem)  # NPT package for data cleaning
```

## Configuration Requirements

### **GitHub Actions Secrets**
- `FINS_API_KEY`: Required for FINS database access

### **Time Restrictions**
- API calls: 6pm - 6am PT only
- Avoid: 11pm - 3am PT (maintenance)
- Rate limit: 24 calls/day per module

### **File Requirements**
- `data/TrappingData.csv`: Current FINS data
- `data/yearly_estimates.csv`: Annual forecasts and targets

## Lessons Learned

### **Shiny Development**
1. **Non-reactive is better** for single-purpose dashboards
2. **Quarto rendering in Shiny** has compatibility issues on Windows
3. **File path detection** essential for multi-context functions

### **R Package Integration**
1. **cuyem package functions** require careful parameter matching
2. **Dynamic sourcing** can cause unexpected API calls
3. **Environment isolation** important for development vs production

### **Data Pipeline**
1. **GitHub Actions** excellent for automated data collection
2. **Time restrictions** must be strictly observed for API access
3. **Error logging** crucial for monitoring automated processes

## Next Steps for Project Enhancement

### **Immediate Opportunities**
- Add refresh button to reload current data
- Implement email notifications for download failures
- Add data validation checks

### **Future Features**
- Multi-species support (steelhead, bull trout)
- Historical trend analysis across years
- Advanced filtering options
- Data export capabilities

## Contact Information
- **Project Lead**: Brian Simmons (brians@nezperce.org)
- **Organization**: Nez Perce Tribe - Joseph Field Office
- **Technical Contact**: Neal Espinosa (neale@nezperce.org)

---
*Development completed: July 2025*
*Status: Production ready for current year monitoring*