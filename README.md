# Lostine River Weir - Chinook Summary Dashboard

An automated data pipeline and interactive dashboard for Lostine River Weir Chinook salmon monitoring data.

## Project Structure

```
LRW-update/
├── .github/
│   └── workflows/
│       └── download_fins_data.yml    # Nightly data download automation
├── github_scripts/
│   ├── get_fins_data.R              # FINS API functions
│   └── nightly_fins_download.R      # Download script for GitHub Actions
├── documents/
│   └── LRW-Weekly-Chinook-Summary.qmd # Quarto report template
├── R/
│   ├── report_helpers.R             # Core functions and data processing
│   ├── sumGRSMEdisp.R              # Disposition summary functions
│   └── sumGRSMEbrood.R             # Broodstock summary functions
├── data/
│   ├── TrappingData.csv            # FINS data (auto-updated nightly)
│   └── yearly_estimates.csv        # Annual forecasts and targets
├── logs/
│   └── fins_download.log           # Download activity logs
├── www/
│   └── npt_joseph.png              # NPT logo for reports
├── app.R                           # Shiny dashboard application
└── README.md                       # This file
```

## Features

### 🤖 Automated Data Pipeline
- **Nightly downloads** from FINS API using GitHub Actions
- **Automatic data cleaning** and processing
- **Error logging** and monitoring

### 📊 Interactive Dashboard
- **Real-time data visualization** of current year catches
- **Disposition summaries** for hatchery and natural-origin fish
- **Interactive plots** with historical comparisons
- **River flow integration** from USGS data

### 📄 Professional Reporting
- **Quarto-based PDF reports** with publication-ready formatting
- **Automated report generation** with current data
- **Consistent formatting** matching NPT standards

## Quick Start

### Running the Dashboard Locally

1. **Install required R packages:**
```r
install.packages(c("shiny", "shinydashboard", "DT", "plotly", 
                   "readr", "dplyr", "ggplot2", "lubridate"))
```

2. **Set up your environment:**
   - Ensure `data/TrappingData.csv` exists
   - Verify `data/yearly_estimates.csv` is current

3. **Launch the dashboard:**
```r
shiny::runApp()
```

### Generating PDF Reports

```r
quarto render documents/LRW-Weekly-Chinook-Summary.qmd --to pdf
```

## Data Sources

- **FINS Database**: Fish trapping and disposition data
- **USGS Station 13330000**: Lostine River flow data
- **NPT Annual Estimates**: Return forecasts and brood goals

## Configuration

### GitHub Actions Setup
1. Set repository secret: `FINS_API_KEY`
2. Enable Actions in repository settings
3. Verify workflow permissions (read/write)

### Time Restrictions
- API calls only allowed 6pm - 6am PT
- Avoid 11pm - 3am PT (maintenance window)
- Maximum 24 calls per day per module

## Key Functions

### Data Processing
- `get_trap_data()`: Load and clean FINS data
- `calculate_dispositions()`: Summarize fish dispositions
- `prepare_megadf()`: Combine catch and flow data

### Visualization
- `generate_lrw_megaplot()`: Create historical comparison plots
- `safe_flextable()`: Handle tables with no-data scenarios

### Reporting
- `load_yearly_estimates()`: Get current forecasts
- `sumGRSMEdisp()`: Disposition summary tables
- `sumGRSMEbrood()`: Broodstock collection summaries

## Data Validation

Use `R/diagnostic_utilities.R` for troubleshooting data discrepancies:

- `quick_consistency_check(data, year)` - Daily validation
- `validate_data_consistency(data, year)` - Full investigation
- Individual debug functions for specific calculations

## Maintenance

### Regular Tasks
- Review `logs/fins_download.log` for API issues
- Update `yearly_estimates.csv` when new forecasts available
- Monitor GitHub Actions for failed downloads

### Troubleshooting
- **API errors**: Check time restrictions and rate limits
- **Missing data**: Verify nightly download succeeded
- **Plot issues**: Check USGS data availability

## Development Notes

- **Non-reactive design**: Dashboard loads current year data at startup
- **Dynamic file paths**: Functions work from both Shiny app and Quarto
- **Error handling**: Graceful degradation when data unavailable
- **Consistent naming**: All functions use `trap.year` parameter format

## Contact

**Project Lead**: Brian Simmons  
**Organization**: Nez Perce Tribe - Joseph Field Office  
**Email**: brians@nezperce.org

---

*Last updated: July 2025*