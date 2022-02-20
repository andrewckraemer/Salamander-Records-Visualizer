
library(sf)
library(dplyr)
library(tigris) 
library(plotly) 
library(geodist)

# Data pre-processing
mander_data_full<-read.csv('mander_data.csv')
mander_named<-mander_data_full[!(mander_data_full $species==""), ] #clean data - only include records identified to species
mander_non_na <- mander_named %>% filter_at(vars(decimalLatitude, decimalLongitude, year),all_vars(!is.na(.))) #clean data - only include records with lat, long, and year
sallylisttoprint<-names(table(mander_non_na$species))

# User Interface
ui <- fluidPage(
  
  # Application title
  headerPanel("Salamander Records Visualizer"),
  h5("This app uses GBIF data (www.gbif.org; February 2022) to plot species distributions for each salamander species recorded in the United States"),
  br(),
  
  # Sidebar for input data 
  sidebarLayout(
    sidebarPanel(
      h4(selectInput('specieschoice', 'Species List',c(sallylisttoprint),selected="Plethodon cinereus")),
      br(),
      radioButtons("yearswitch","",c("All"="all","Since 1990"="modern","Since 2010"="recent")),
      br(),
      tags$h4("The most recent observation is:"),
      h4(textOutput("year"))
    ),
    # Show a map of the data
    mainPanel(
      plotOutput("plot"),
    )
  ),
  tags$a(href="https://github.com/andrewckraemer/Salamander-Records-Visualizer","Andrew Kraemer's R script for this widget")
)


# 'Under The Hood'
server <- function(input, output,session) {

  # subset a single species
  observe({
    specieschoice<-input$specieschoice
    yearswitch<-switch(input$yearswitch,all=1500,modern=1990,recent=2010)
    mander_yearsub<-subset(mander_non_na,mander_non_na$year>yearswitch)
    mander_data<-filter(mander_yearsub, species == specieschoice)
    mostrecent<-max(mander_data$year)
    output$year<-renderText(mostrecent)
    countymap <- counties(cb = TRUE,resolution='20m')
    longlat<-data.frame(mander_data$decimalLongitude, mander_data$decimalLatitude)
    colnames(longlat)<-c('long','lat')
  
    latlong_sf <- longlat %>%
      st_as_sf(coords = c("long", "lat"), crs = st_crs(countymap))
  
  # count salamander records per county
    data_sf_summary <- countymap %>% 
      mutate(counts = lengths(st_intersects(., latlong_sf))) 
    data_sf_summary$counts<-ifelse(data_sf_summary$counts>0,log(data_sf_summary$counts),data_sf_summary$counts) #log-transform counts
  
  # plot
    output$plot <- renderPlot({
      p<- ggplot(data_sf_summary) +
      geom_sf(aes(fill=counts),size=0.07) +
      scale_fill_distiller("low - abundance - high", palette="Spectral",labels =c(),guide=guide_colourbar(title.position='top')) +
      ggtitle(specieschoice)+
      coord_sf(xlim = c(-127, -68), ylim = c(22, 55)) +
      #ylab('Latitude')+xlab('Longitude')+
      theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),axis.text.x=element_blank(),
            axis.title.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_blank(), 
            legend.direction='horizontal',legend.position =c(0.25,0.2),plot.title = element_text(size = 20, face = "italic"))
     p
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
