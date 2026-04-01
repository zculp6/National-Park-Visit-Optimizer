# 🏞️ National Parks Route Planner

An interactive R Shiny application for planning optimal road trips across U.S. National Parks. Plan your journey with intelligent routing, cost estimation, and real-time visualizations.

![R](https://img.shields.io/badge/R-4.0+-blue.svg)
![Shiny](https://img.shields.io/badge/Shiny-Dashboard-brightgreen.svg)

**🌐 [Try the Live Demo](https://zculp.shinyapps.io/national-park-visit-optimizer/)**

## 🌟 Features

### Trip Planning
- **Interactive Map Interface**: Visualize all 63 U.S. National Parks with real-time mapping
- **Smart Route Optimization**: Automatically calculates the most efficient route between selected parks
- **Flexible Travel Modes**: Choose between driving and flying for each segment
- **Custom Start/End Points**: Begin and end your journey from any U.S. ZIP code

### Cost Estimation
- **Comprehensive Cost Analysis**: 
  - Gas prices by state (updated March 2026 data)
  - Flight cost estimates based on distance and seasonality
  - Park entrance fees (vehicle, person, and motorcycle options)
  - Accommodation costs with configurable lodging budgets
- **Real-time Calculations**: Dynamic cost updates as you modify your route
- **Budget Planning**: Total trip cost broken down by category

### Route Customization
- **Park Visit Duration**: Customize how long you spend at each park
- **Travel Mode Override**: Manually set drive vs. fly for individual segments
- **Multi-stop Itineraries**: Visit multiple parks in a single optimized trip
- **Date-based Planning**: Calculate costs based on your travel dates with seasonal pricing

### Advanced Features
- **Airport Integration**: Automatic selection of nearest major/medium airports
- **Territory Support**: Includes flights to American Samoa and U.S. Virgin Islands
- **Distance Options**: Toggle between miles and kilometers
- **Export Capabilities**: Download your itinerary as CSV or detailed route maps

## 📋 Prerequisites
 
### Quick Start - No Installation Required! 🚀
 
**Want to try it now?** Visit the live application:  
**👉 [https://zculp.shinyapps.io/national-park-visit-optimizer/](https://zculp.shinyapps.io/national-park-visit-optimizer/)**
 
No installation or setup required - start planning your National Parks adventure immediately!

### Running Locally
 
If you want to run the application on your own machine or contribute to development:
 
#### Required R Packages
```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "tidyverse",
  "leaflet",
  "sf",
  "geosphere",
  "igraph",
  "httr",
  "DT",
  "shinyWidgets",
  "jsonlite",
  "rvest",
  "dplyr",
  "stringr",
  "lubridate",
  "googleway",
  "htmltools",
  "shinyjs",
  "rsconnect"
))
```
 
### API Keys
- **Google Maps API Key**: Required for routing functionality
  - Set as environment variable: `GOOGLE_MAPS_KEY`
  - Obtain from [Google Cloud Console](https://console.cloud.google.com/)
 
### Data Files
The following CSV files must be present in the application directory:
- `df_2.csv` - Additional National Parks data (from [Kaggle](https://www.kaggle.com/datasets/thedevastator/the-united-states-national-parks?select=df_2.csv))
- `us-airports.csv` - Airport location data
- `us_territory_flights.csv` - Flight route data for territories
- `uszips.csv` - U.S. ZIP code coordinates (from [SimpleMaps](https://simplemaps.com/data/us-zips))
- `Public Use Statistics.csv` - National park visitor statistics (from NPS)
The following data is found using the NPS API:
- `nps_activities_by_park.csv` (get_activities.R) - National park activities
- `national_parks_images.csv` (get_park_images.R) - National park images
- `nps_park_details.csv` (get_park_info.R) - National parks information and statistics
- `nps_all_coordinates.csv` (get_park_outline.R) - National parks coordinates
- `nps_things_to_do.csv` (get_activities.R) - National park things to do
- `park_cost.csv` (get_park_info.R) - National park entrance fees
- `parkboundaries.csv` and `all_park_boundaries.json` (get_park_boundaries.R) - National park boundaries

 
## 🚀 Installation
 
### 1. Clone the Repository
```bash
git clone https://github.com/yourusername/national-parks-planner.git
cd national-parks-planner
```
 
### 2. Set Up Environment Variables
Create a `.Renviron` file in your project directory:
```
GOOGLE_MAPS_KEY=your_api_key_here
```
 
### 3. Install Dependencies
```r
# In R console
source("install_dependencies.R")
```
 
### 4. Run the Application
```r
# In R console
shiny::runApp()
```
 
Or from terminal:
```bash
R -e "shiny::runApp('app.R')"
```

## 📖 Usage

### Basic Trip Planning

1. **Select Your Parks**
   - Check the boxes next to parks you want to visit
   - Parks appear on the interactive map with details

2. **Set Your Location**
   - Enter your starting ZIP code
   - Optionally enter a different ending ZIP code
   - Click "Calculate Route"

3. **Customize Your Trip**
   - Adjust visit duration for each park (in hours)
   - Override travel modes (Auto/Drive/Flight) for specific segments
   - Set your travel dates for accurate seasonal pricing

4. **Review Costs**
   - View total trip cost breakdown
   - See individual costs: gas, flights, entrance fees
   - Adjust parameters to optimize your budget

5. **Export Your Plan**
   - Download itinerary as CSV
   - Save route details for reference

### Advanced Options

#### Travel Mode Customization
- **Auto**: Automatically selects drive or fly based on distance
- **Drive**: Forces driving route for the segment
- **Flight**: Forces flight route with airport transfers

#### Cost Parameters
- **Fuel Efficiency**: Adjust MPG for your vehicle (default: 25 MPG)
- **Number of People**: Calculate per-person costs for flights and entrance fees
- **Vehicle Type**: Calculate parking costs for vehicle type
- **Travel Dates**: Estimate travel costs based on time period
- **Airline Preference**: Get potential flight options limited to specific airlines 

## 🗺️ Data Sources

- **National Park Data**: [National Park Service API](https://www.nps.gov/subjects/developer/api-documentation.htm)
- **ZIP Code Data**: [SimpleMaps US ZIP Codes Database](https://simplemaps.com/data/us-zips)
- **Airport Data**: [Airport Data](https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat)
- **Gas Prices**: [Gas Prices](https://fuelinsights.gasbuddy.com/Home/US/)
- **Park Boundaries**: Geospatial data from NPS

## 🛠️ Technical Architecture

### Key Components

1. **Routing Engine**
   - Traveling Salesman Problem (TSP) solver using igraph
   - Google Maps API integration for real routes
   - Smart mode selection based on distance thresholds

2. **Cost Calculation**
   - Dynamic gas pricing by state
   - Flight estimation using distance + seasonality factors
   - Entrance fee lookup with vehicle/person options

3. **Caching System**
   - Nearest airport caching for performance
   - OSRM route caching to minimize API calls
   - Park boundary code resolution

4. **UI/UX**
   - Responsive Leaflet maps with custom markers
   - Real-time route visualization
   - Interactive cost breakdowns
   - Mobile-friendly dashboard layout

### Data Processing Functions

- `normalize_park_code()`: Standardizes park code formats
- `find_nearest_airport()`: Locates closest major airport
- `estimate_flight_cost()`: Calculates airfare with seasonal adjustments
- `compute_route()`: TSP solver with multi-modal transportation
- `route_with_geometry()`: Adds geographic routing data

## 📊 Cost Calculation Details

### Gas Cost Formula
```
Total Gas Cost = (Distance in miles / MPG) × (Gas price per gallon)
```
- Uses state-specific gas prices
- Accounts for vehicle fuel efficiency
- Calculates round-trip for there-and-back journeys

### Flight Cost Formula
```
Base Fare + (Distance × Rate per Mile) × Seasonal Multiplier × Airline Factor
```
- **Seasonal Multipliers**: 
  - Winter (Dec-Jan): 1.40
  - Summer (Jun-Aug): 1.30
  - Spring Break (Mar-Apr): 1.20
  - Low Season (Feb, Nov): 0.85
- **Airline Factors**: Varies by carrier (budget vs. major airlines)

## 🌐 Deployment
 
This application is deployed on shinyapps.io and accessible at:  
**[https://zculp.shinyapps.io/national-park-visit-optimizer/](https://zculp.shinyapps.io/national-park-visit-optimizer/)**
 
### Deploying Your Own Instance
 
To deploy your own version to shinyapps.io:
 
```r
# Install rsconnect if not already installed
install.packages("rsconnect")
 
# Set up your shinyapps.io account
rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
 
# Deploy the application
rsconnect::deployApp()
```

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

### Development Setup
1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 🙏 Acknowledgments

- National Park Service for park data and boundary information
- SimpleMaps for comprehensive ZIP code database
- Google Maps Platform for routing services
- R Shiny community for excellent documentation and packages

## 📧 Links

- **Live Application**: [https://zculp.shinyapps.io/national-park-visit-optimizer/](https://zculp.shinyapps.io/national-park-visit-optimizer/)
- **Project Link**: [https://github.com/yourusername/national-parks-planner](https://github.com/yourusername/national-parks-planner)

## 🐛 Known Issues

- Some territory airports may have limited flight data
- Seasonal pricing is estimated and may not reflect actual fares
- Route optimization assumes consistent travel speeds
- Park boundary data may not include all recent park additions

---

**Made with ❤️ for National Park enthusiasts**
