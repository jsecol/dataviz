#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#### JS: latest published 8/22/19 (added basemaps toggle)

# 8/23 = trying to zoom based on list work: no need to setView, just filter prisons_poly

## see for options: https://rstudio.github.io/leaflet/json.html

library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(htmltools)

ui <- dashboardPage(
    dashboardHeader(title = "US Prison Facilities"),
    dashboardSidebar(
    ),
    dashboardBody(
        leafletOutput(outputId = "mymap1", width ="100%", height = "600px")
    )
)

server <- 

    
    function(input, output, session) {
        
    prisons_poly <- st_read("shapes/prison_bdys_ll.geojson") %>% 
        filter(STATE == "MA")
    
    poly_popuptext <- paste0("<b>Security Level: %s</b>")
    
        
    output$mymap1 <- renderLeaflet({
        leaflet() %>%
            addTiles(group = "Open Streetmap") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
            addLayersControl(
                baseGroups = c("Open Streetmap", "World Imagery"),
                options = layersControlOptions(collapsed = FALSE)) %>% 
#            setView(lng = -72.94, lat = 41.32, 14) %>% 
            addPolygons(data = prisons_poly, label= ~prisons_poly$NAME, 
                        popup = ~sprintf(poly_popuptext, htmlEscape(SECURELVL))) %>% 
            addSearchOSM() # %>% 
#            addSearchGoogle(apikey = YOURAPIKEY)
    })
    
    # this has nothing to do with shinyapps.io though, just use the 5-min idle option in account to
    # minimize time used (and maybe bad practice)
    session$onSessionEnded(stopApp)
    
}

shinyApp(ui = ui, server = server)


# library(shiny)
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)

############
# # Misc.
#            addProviderTiles(providers$Esri.WorldStreetMap) %>% 
