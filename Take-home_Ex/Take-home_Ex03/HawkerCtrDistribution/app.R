pacman::p_load(shiny, maptools, sf, raster, spatstat, tmap, readr)

mpsz_sf <- st_read(dsn = "data/geospatial", 
                   layer = "MP14_SUBZONE_WEB_PL")

hawker_centre_sf <- read_csv("data/aspatial/updated_hawker_centres.csv")
hawker_centre_sf <- st_as_sf(hawker_centre_sf, coords = c("Longitude", "Latitude"), crs = 4326)

sg_sf <- st_read(dsn = "data/", 
                 layer = "CostalOutline")

target_crs = st_crs(sg_sf)
hawker_centre_sf <- st_transform(hawker_centre_sf, target_crs)

mpsz <- as_Spatial(mpsz_sf)
hawker_centre <- as_Spatial(hawker_centre_sf)
sg <- as_Spatial(sg_sf)

hawker_centre_sp <- as(hawker_centre, "SpatialPoints")
sg_sp <- as(sg, "SpatialPolygons")

hawker_centre_ppp <- as.ppp(hawker_centre_sp)

hawker_centre_SG_ppp_km <- rescale(hawker_centre_SG_ppp, 1000, "km")

sg_owin <- as(sg_sp, "owin")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Kernel Density"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
