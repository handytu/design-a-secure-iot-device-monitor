# 0mk3_design_a_secure.R

# Load necessary libraries
library(shiny)
library(ggplot2)
library(reshape2)
library scales)

# Define the UI for the monitor dashboard
ui <- fluidPage(
  titlePanel("Secure IoT Device Monitor"),
  sidebarLayout(
    sidebarPanel(
      selectInput("device", "Select Device:", c("Device 1", "Device 2", "Device 3")),
      sliderInput("time_range", "Time Range (hours):", 1, 24, 6)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Device Status", tableOutput("device_status")),
        tabPanel("Sensor Readings", plotOutput("sensor_readings")),
        tabPanel("Alerts", tableOutput("alerts"))
      )
    )
  )
)

# Define the server logic for the monitor dashboard
server <- function(input, output) {
  # Establish a secure connection to the IoT device data stream
  conn <- openssl::ssl_connect("https://iot-device-data-stream.com/api/v1/data")
  
  # Define a reactive expression to fetch device data
  device_data <- reactive({
    req(input$device)
    req(input$time_range)
    # Query the data stream using the selected device and time range
    data <- getURL(conn, path = "/devices/{{input$device}}/data", 
                    query = list(start_time = Sys.time() - input$time_range * 3600, 
                                   end_time = Sys.time()))
    # Parse the JSON response
    data <- jsonlite::fromJSON(data, simplifyDataFrame = TRUE)
    # Process the data (e.g., filter, aggregate, transform)
    data <- data[, c(" timestamp", "temperature", "humidity")]
    data$timestamp <- as.POSIXct(data$timestamp, tz = "UTC")
    data
  })
  
  # Define output for device status
  output$device_status <- renderTable({
    device_data() %>% 
      group_by(device) %>% 
      summarise(status = "Online", 
                 last_seen = max(timestamp))
  })
  
  # Define output for sensor readings
  output$sensor_readings <- renderPlot({
    data <- device_data()
    ggplot(data, aes(x = timestamp, y = temperature, color = device)) + 
      geom_line() + 
      labs(x = "Time", y = "Temperature (°C)", color = "Device")
  })
  
  # Define output for alerts
  output$alerts <- renderTable({
    device_data() %>% 
      filter(humidity > 80) %>% 
      group_by(device) %>% 
      summarise(alerts = "High Humidity Alert", 
                 last_seen = max(timestamp))
  })
}

# Create the Shiny app
shinyApp(ui = ui, server = server)