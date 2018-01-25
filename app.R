#
#  Shiny Web application 
#  This application allow user to enter their travel date, 
#  pickup time, pickup location, drop off location. It will
#  use the predictive model to predict the time to travel. 
#

require(shiny)
require(shinyTime)
require(leaflet)
require(googleway)
require(xtable)

source("model.R")
#devtools::install_github("dkahle/ggmap")
google_api_key <- "AIzaSyClVIejhNNNALZwT98TYes1Jw1i76nnpss"

# Define UI for application that draws a histogram
ui <- bootstrapPage(
   
   # Application title
   titlePanel("New York Taxi Trip Time Prediction Application"),
   
   mainPanel(
       leafletOutput("pickupMapPlot"),
       dataTableOutput('result')
   ),
   # Sidebar with a slider input for number of bins 
   sidebarPanel ( 
      dateInput("p1", "pickup_date:"), value= as.character(now())
      ,
      timeInput("t1", "pickup_time", value = strptime("12:34:56", "%H:%M:%S"))
      ,
      textInput("pa1", "pickup_address:", value="1, 30 street, New York, NY")
      ,
      textInput("dr1", "dropoff_address:", value="1, 80 street, new York NY")
      ,
      numericInput("ps1", "number_of_passenger:", value=1)
      # Show a plot of the generated distribution
      ,
      selectInput("modelInput", "choose a prediction model",
                  choices= c("eXtreme Gradiant Boost (xgb)",
                             "linear"), multiple=FALSE, selected="linear")
      ,
      submitButton()
      
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    address1 <- as.data.frame(as.matrix(cbind(40.77422,-73.95214), nrow=1,ncol=2))
    address2 <- as.data.frame(as.matrix(cbind(40.77422,-73.95214), nrow=1,ncol=2))
    
    getInput <- reactive({
        print("ractive, straddress")
       
        straddress <- input$pa1 
        straddress2 <- input$dr1
        print(straddress)
        print(straddress2)
        if (length(straddress) > 0)
        {
            address1 <- google_geocode(straddress, key=google_api_key)$results
        }
        if (length(straddress2) > 0)
        {
            address2 <- google_geocode(straddress2, key=google_api_key)$results
        }
        
        ret <- as.data.frame(
            as.matrix(cbind(address1$geometry$location$lat, address1$geometry$location$lng,
                            address2$geometry$location$lat, address2$geometry$location$lng,
                            address1$formatted_address, address2$formatted_address), 
                      nrow=1,ncol=6))
        names(ret) <- c("pickup_latitude", 
                        "pickup_longitude", 
                        "dropoff_latitude", 
                        "dropoff_longitude",
                        "pickup_address",
                        "dropoff_address")
        print("dimension of ret in reactive `")
        print(dim(ret))
        print(ret)
        ret <- ret %>%
            mutate(pickup_latitude = as.numeric(as.character(pickup_latitude))) %>%
            mutate(pickup_longitude = as.numeric(as.character(pickup_longitude))) %>%
            mutate(dropoff_latitude = as.numeric(as.character(dropoff_latitude))) %>%
            mutate(dropoff_longitude = as.numeric(as.character(dropoff_longitude)))
        
        ret$passenger_count <- input$ps1
        pickup_time <- input$t1
        pickup_date <- input$p1
        ret$pickup_datetime <- 
            paste(as.character(pickup_date),
                  strftime(as.POSIXlt(pickup_time), format="%H:%M:%S"))
        if (is.na(ret$pickup_datetime) | is.na(ret$pickup_datetime))
        {
            ret$pickup_datetime <- as.character(now())
        }
        ret$trip_distance <- distGeo(
            as.matrix(cbind(ret$pickup_longitude, ret$pickup_latitude), ncol=2),
            as.matrix(cbind(ret$dropoff_longitude, ret$dropoff_latitude), ncol=2))
        
        ret$trip_distance <- round(ret$trip_distance, digits=2)
        ret$dropoff_datetime <- NA
        ret$vendor_id <- 1
        ret$id <- 1
        ret$store_and_fw_flg <- "N"
        ret$trip_duration <- NA
        ret <- modify_test_ds(ret)
        print(ret)
        ret
        
    })
    
    output$result <- renderDataTable({
        d <- getInput() %>% 
            dplyr::select(pickup_address, dropoff_address,
                          trip_distance)
        t <- round(predictTripTime(getInput())$trip_duration, digits=2)
        ret <- as.data.frame(as.matrix(cbind(d, t),ncol=4))
        names(ret) <- c("pickup_addr", "dropoff_addr", "trip_dist", "estimate_trip_duration")
        ret
    }, options = list(lengthChange = TRUE, paging=FALSE, searching=FALSE)
    )
    
    output$pickupMapPlot <- renderLeaflet({
        m <- getMinMaxLatLng()
        leaflet() %>%
            addTiles() %>%
            setView(lng=m[1, 1], lat=m[1,3],zoom=10)
    })
    
    observe({
        
        inputAddr <- getInput()
        print(paste(inputAddr$pickup_latitude, inputAddr$pickup_longitude,
                    class(inputAddr$pickup_latitude)))
        leafletProxy("pickupMapPlot", data=inputAddr) %>%
            clearMarkers() %>%
            addMarkers(lat=~pickup_latitude, lng=~pickup_longitude, popup=~inputAddr$pickup_address) %>%
            addMarkers(lat=~dropoff_latitude, lng=~dropoff_longitude,popup=~inputAddr$dropoff_address)
        
    })
            
}

# Run the application 
shinyApp(ui = ui, server = server)


