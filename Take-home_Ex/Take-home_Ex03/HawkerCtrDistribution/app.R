library(shiny)
library(sf)
library(sp)
library(spatstat)
library(tmap)
library(readr)

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

hawker_centre <- as_Spatial(hawker_centre_sf)
hawker_centre_sp <- as(hawker_centre, "SpatialPoints")


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

# Define server logic
server <- function(input, output, session) {
  
  # Function to filter data based on region and location selections
  filter_data <- reactive({
    region <- input$region
    location <- input$location
    kernel <- input$kernel
    bandwidth <- input$bandwidth
    
    # Your data filtering and processing logic here
    if(region == "All") {
      sg_owin <- as.owin(mpsz_sf)
      hawker_centre_ppp <- as.ppp(hawker_centre_sf)
      hawker_centre_SG_ppp <- hawker_centre_ppp[sg_owin] 
      hawker_centre_SG_ppp_km <- rescale(hawker_centre_SG_ppp, 1000, "km")
      return(hawker_centre_SG_ppp_km)
    } else {
      if(is.null(location) || location == "All") {
        selected_region <- mpsz_sf[mpsz_sf$REGION_N == region,]
        selected_region_owin <- as.owin(selected_region) 
        hawker_centre_ppp <- as.ppp(hawker_centre_sf)
        hawker_centre_selected_region_ppp <- hawker_centre_ppp[selected_region_owin] 
        hawker_centre_selected_region_ppp_km <- rescale(hawker_centre_selected_region_ppp, 1000, "km") 
        return(hawker_centre_selected_region_ppp_km)
      } else {
        selected_area <- mpsz_sf[mpsz_sf$PLN_AREA_N == location,]
        selected_area_owin <- as.owin(selected_area)
        hawker_centre_ppp <- as.ppp(hawker_centre_sf)
        hawker_centre_selected_area_ppp <- hawker_centre_ppp[selected_area_owin] 
        hawker_centre_selected_area_ppp_km <- rescale(hawker_centre_selected_area_ppp, 1000, "km") 
        return(hawker_centre_selected_area_ppp_km)
      }
    }
  })
  
  # Output for dynamic location select input
  output$location_select <- renderUI({
    region <- input$region
    if(region == "All") {
      return(NULL)
    } else {
      locations <- unique(mpsz_sf$PLN_AREA_N[mpsz_sf$REGION_N == region])
      selectInput("location", "Select Location:", choices = c("All", locations))
    }
  })
  
  # Output for KDE plot
  output$kde_plot <- renderPlot({
    data <- filter_data()
    if(!is.null(data)) {
      plot(density(data, sigma = input$bandwidth, kernel = input$kernel), main = "Kernel Density Estimation")
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)