pacman::p_load(shiny, sf, raster, spatstat, tmap, bslib, readr, tidyverse)

mpsz_sf <- st_read(dsn = "data/geospatial", 
                   layer = "MP14_SUBZONE_WEB_PL")

hawker_centre_sf <- read_csv("data/aspatial/updated_hawker_centres.csv")
hawker_centre_sf <- st_as_sf(hawker_centre_sf, coords = c("Longitude", "Latitude"), crs = 4326)

sg_sf <- st_read(dsn = "data/", 
                 layer = "CostalOutline")

main_island_name <- "SINGAPORE - MAIN ISLAND"
sg_sf <- sg_sf[sg_sf$COSTAL_NAM == main_island_name, ]

target_crs = st_crs(sg_sf)
hawker_centre_sf <- st_transform(hawker_centre_sf, target_crs)

mpsz_sf <- st_intersection(mpsz_sf, sg_sf)

mpsz <- as_Spatial(mpsz_sf)
hawker_centre <- as_Spatial(hawker_centre_sf)
sg <- as_Spatial(sg_sf)

hawker_centre_sp <- as(hawker_centre, "SpatialPoints")
sg_sp <- as(sg, "SpatialPolygons")

hawker_centre_ppp <- as.ppp(hawker_centre_sp)

# Define UI
ui <- fluidPage(
  titlePanel("Geospatial Analytics Shiny Application"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Select Region:", choices = c("All", unique(mpsz_sf$REGION_N))),
      uiOutput("location_select"),
      selectInput(inputId = "kernel",
                  label = "Select Kernel:",
                  choices = list("Gaussian" = "gaussian",
                                 "Epanechnikov" = "epanechnikov",
                                 "Quartic" = "quartic",
                                 "Disc" = "disc"),
                  selected = "gaussian"),
      sliderInput("bandwidth", "Select Bandwidth (Sigma):", min = 0.01, max = 12, value = 0.5, step = 0.1)
    ),
    mainPanel(
      plotOutput("kde_plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Function to filter data based on region and location selections
  filter_data <- function(region, location) {
    #if region is "All" - ppp for the whole Singapore
    if(region == "All") {
      if(is.null(location) || location == "All") {
        hawker_centre_SG_ppp_km <- rescale(hawker_centre_SG_ppp, 1000, "km")
        sg_owin <- as(sg_sp, "owin")
        return(hawker_centre_SG_ppp_km)
      } 
    } else {
      #if region is not "All", and location is "All" - ppp for a specific region (e.g. NORTH)
      if(is.null(location) || location == "All") {
        selected_region = mpsz[mpsz@data$REGION_N == region,] 
        selected_region_sp = as(selected_region, "SpatialPolygons") 
        selected_region_owin = as(selected_region_sp, "owin") 
        hawker_centre_selected_region_ppp = hawker_centre_ppp[selected_region_owin] 
        hawker_centre_selected_region_ppp_km = rescale(hawker_centre_selected_region_ppp, 1000, "km") 
        return(hawker_centre_selected_region_ppp_km)
      #if region is not "All", and location is not "All" - ppp for a specific planning area (e.g. Ang Mo Kio)
      } else {
        selected_area = mpsz[mpsz@data$PLN_AREA_N == location,]
        selected_area_sp = as(selected_area, "SpatialPolygons") 
        selected_area_owin = as(selected_area_sp, "owin") 
        hawker_centre_selected_area_ppp = hawker_centre_ppp[selected_area_owin] 
        hawker_centre_selected_area_ppp_km = rescale(hawker_centre_selected_area_ppp, 1000, "km") 
        return(hawker_centre_selected_area_ppp_km)
      }
    }
  }
  
  # Function to compute KDE based on user-selected parameters
  compute_kde <- function(data, kernel, sigma) {
    kde <- density(data, sigma = sigma, edge = TRUE, kernel = kernel)
    return(kde)
  }
  
  # Render the density plot based on user-selected parameters
  output$kde_plot <- renderPlot({
    # Get user-selected parameters
    region <- input$region
    location <- input$location
    kernel <- input$kernel
    sigma <- input$bandwidth
    
    # Filter data based on region and location selections
    filtered_data <- filter_data(region, location)
    
    # Compute KDE based on user-selected parameters
    kde <- compute_kde(filtered_data, kernel, sigma)
    
    # Plot the density map
    plot(kde, main = paste("Kernel Density Estimation using",kernel, "kernel"))
  })
  
  # Render the location select input based on the region selection
  observeEvent(input$region, {
    region_choice <- input$region
    
    # Get the locations for the selected region
    locations <- unique(mpsz_sf$PLN_AREA_N[mpsz_sf$REGION_N == region_choice])
    
    # If "All" regions selected or no specific region is selected, set location select input to NULL
    if(region_choice == "All" || is.null(region_choice)) {
      output$location_select <- renderUI({})
    } else {
      # Add "All" option to the locations dropdown
      locations <- c("All", locations)
      
      # Render the location select input
      output$location_select <- renderUI({
        selectInput("location", "Select Location:", choices = locations)
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

