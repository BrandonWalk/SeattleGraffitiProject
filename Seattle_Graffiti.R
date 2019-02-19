require("tidyverse")
require("rsconnect")
require("leaflet")
require("lubridate")
require("shiny")
crimes <- read_csv("Seattle_Police_Department_911_Incident_Response.csv")
crimes$`Event Clearance Date` <- mdy_hms(crimes$`Event Clearance Date`,tz="America/Los_Angeles")
crimes <- crimes[which(!is.na(crimes$Longitude)),]
crimes <- crimes[which(!is.na(crimes$`Event Clearance Date`)),]
crimes$`Event Clearance Description` <- str_to_title(crimes$`Event Clearance Description`)
crimes$mColors <- ifelse(crimes$`Event Clearance Description`=="Gang Graffiti","blue","red")

ui <- fluidPage(
  titlePanel("Does Gang Graffiti Predict Crime?"),
  h4("Gang graffiti in blue, selected crime in red."),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("inputDate",
                     label = 'Date range input: yyyy-mm-dd',
                     start=Sys.Date()-240,end=Sys.Date()-226),
      selectInput('inputCrime', 'Select Crimes', str_to_title(sort(unique(crimes$`Event Clearance Description`)[-73],decreasing = FALSE)), selectize=TRUE),
      sliderInput('inputRadius',"Radius:",min=0,max=1,step=0.1,value=0.5),
      helpText("Selected crime will appear in red and will dislplay events 2 weeks following the selected date range. Increasing the radius obviously increases the chance the selected crimes is considered \"near\" graffiti, but too large a circle renders the circle useless in nailing down a location for crimes.")),
    mainPanel(leafletOutput("map"))
  )
)

server <- function(input,output){
  selectedCrimes <- reactive({input$inputCrime})
  selectedDates <- reactive({input$inputDate})
  selectedRadius <- reactive({input$inputRadius})
  output$map <- renderLeaflet({
    start<-selectedDates()[1]
    end<-selectedDates()[2]
    radMult <- selectedRadius()
    crimes %>% filter(`Event Clearance Description`==selectedCrimes() | `Event Clearance Description`=="Gang Graffiti") %>% filter(`Event Clearance Date`<=end+14 & `Event Clearance Date`>=start) %>% filter((`Event Clearance Description`=="Gang Graffiti" & `Event Clearance Date`<=end) | (`Event Clearance Description`==selectedCrimes() & `Event Clearance Date`>end)) %>% leaflet() %>% addProviderTiles(providers$Stamen.TonerLite) %>% addCircles(color=~mColors,weight=0,radius=~ifelse(`Event Clearance Description`=="Gang Graffiti", 1000*radMult, 50),popup=~paste("Date:",date(`Event Clearance Date`),"<br/>","CAD Event Number:", `CAD Event Number`))})
}

shinyApp(ui=ui,server=server)