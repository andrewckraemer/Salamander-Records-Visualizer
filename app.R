
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
  titlePanel("Salamander Records Visualizer"),
  
  # Sidebar for input data 
  sidebarLayout(
    sidebarPanel(
      h4(selectInput('specieschoice', 'Species List',c(sallylisttoprint))),
      br(),
      tags$h4("Most Recent Observation"),
      #verbatimTextOutput("year",placeholder=F)
      h4(textOutput("year"))
    ),
    # Show a map of the data
    mainPanel(
    plotOutput("plot"),
    )
  )
)


# 'Under The Hood'
server <- function(input, output,session) {

  # subset a single species
  observe({specieschoice<-input$specieschoice
  mander_data<-filter(mander_non_na, species == specieschoice)
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
      geom_sf(aes(fill=counts),size=0.01) +
      scale_fill_distiller("Relative Abundance", palette="Spectral",labels = c()) +
      ggtitle(specieschoice)+
      coord_sf(xlim = c(-127, -68), ylim = c(20, 55)) +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                             panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position =c(0.15,0.2),plot.title = element_text(size = 20, face = "italic"))
    p
    #   })
  })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)