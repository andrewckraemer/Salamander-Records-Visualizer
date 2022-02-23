library(tidyverse)
library(sf)
library(dplyr)
library(tigris) 
library(plotly) 
library(cdlTools)
library(nngeo)

## Data pre-processing
mander_data_full<-read.csv('mander_data.csv')
protectedareas<-read.csv('protectedareas.csv')
protectedareas_sf <- protectedareas %>% 
  st_as_sf(coords = c("long", "lat"))
sallylisttoprint<-names(table(mander_data_full$species))
statesf<-states(resolution='20m')
countysf <- counties(cb = TRUE,resolution='20m')
topCOstate<-st_read("topcountyperstate.shp")
options(tigris_use_cache = TRUE)
st_crs(protectedareas_sf) <- st_crs(countysf)


## User Interface
ui <- fluidPage(
  
  # Application title
  titlePanel("Salamander Records Visualizer"),
  h5("This app uses Global Biodiversity Information Facility data (www.gbif.org) to plot distributions for each salamander species recorded in the United States. Data downloaded February 2022."),
  br(),
  
  # Sidebar for input data 
  sidebarLayout(
    sidebarPanel(
      h4(selectInput('specieschoice', 'Where is my favorite salamander?',c(sallylisttoprint),selected="Plethodon cinereus")),
      h6(div(HTML("<em>see Species Distributions tab</em>"))),
      radioButtons("yearswitch","",c("All"="all","Since 1990"="modern","Since 2010"="recent")),
      br(),
      tags$h5("Average observations each year:"),
      h5(textOutput("avgyear")),
      tags$h5("The most recent observation is:"),
      h5(textOutput("year")),
      hr(style = "border-top: 1px solid #000000;"),
      h4(selectInput('statechoice', 'If I visit this state, where can I find the greatest density of salamanders?', state.name,selected="Wisconsin")),
      h4(textOutput("bestcounty")),
      h5(textOutput("neighborprotectedarea")),
      br(),
      h6(div(HTML("<em>see Salamander Diversity tab</em>")))
    ),
    # Show a map of the data
    mainPanel(
      tabsetPanel(
        br(),
        br(),
        tabPanel("Species Distributions",plotOutput("plotdistribution")),
        tabPanel('Salamander Diversity',plotOutput("plothotspots"))
      )  
    )
  ),
  tags$a(href="https://github.com/andrewckraemer/Salamander-Records-Visualizer","Andrew Kraemer's R script for this app")
)


# 'Under The Hood'
server <- function(input, output,session) {

  # subset a single species
  observe({
    specieschoice<-input$specieschoice
    yearswitch<-switch(input$yearswitch,all=1500,modern=1990,recent=2010)
    mander_yearsub<-subset(mander_data_full,mander_data_full$year>yearswitch)
    mander_data<-filter(mander_yearsub, species == specieschoice)
    mostrecent_temp<-max(mander_data$year)
    mostrecent<-ifelse(mostrecent_temp==-Inf,"Why would this be here, anyway...?",mostrecent_temp)
    output$year<-renderText(mostrecent)
    
    meanyear.temp<-nrow(mander_data)/(max(mander_data$year)-min(mander_data$year))
    meanyear<-ifelse(meanyear.temp==Inf,"No records :(",meanyear.temp)
    output$avgyear<-renderText(meanyear)
    
    # count salamander SPECIES CHOICE records per county
    longlat<-data.frame(mander_data$longitude, mander_data$latitude)
    colnames(longlat)<-c('long','lat')
    latlong_sf <- longlat %>%
      st_as_sf(coords = c("long", "lat"), crs = st_crs(countysf))
    data_sf_summary <- countysf %>% 
      mutate(counts = lengths(st_intersects(countysf,latlong_sf))) 
    data_sf_summary$counts<-ifelse(data_sf_summary$counts>0,log(data_sf_summary$counts),data_sf_summary$counts) #log-transform counts

    #find county with most records from chosen state
    statechoice<-input$statechoice
    stateboundary<-subset(statesf,statesf$NAME==statechoice)
    countystatefp<-fips(statechoice,to='FIPS')
    topcounty<-subset(topCOstate, topCOstate$STATEFP==sprintf("%02d", countystatefp))
    topcountyname<-paste(topcounty$NAME,"County")
    output$bestcounty<-renderText(topcountyname)

    #find a great place to look
    cents<-st_centroid(topcounty)
    neighbor<-st_nn(cents, protectedareas_sf)
    neighborprotectedarea<-strsplit(protectedareas_sf$parkname[neighbor[[1]]], "-")[[1]][1]
    output$neighborprotectedarea <-renderText(neighborprotectedarea)
	  protectedlonglat<-protectedareas_sf[neighbor[[1]],]
	  st_crs(protectedlonglat) <- st_crs(countysf)
	  
	  #count salamander diversity per county in chosen state
    statemanders<-subset(mander_data_full, mander_data_full$state==statechoice)
    statecounties<-subset(countysf, countysf $STATEFP==sprintf("%02d", countystatefp))
    statemanderslonglat<-data.frame(statemanders$longitude, statemanders$latitude)
    colnames(statemanderslonglat)<-c('long','lat')
    statemanders_sf <- statemanderslonglat %>%
      st_as_sf(coords = c("long", "lat"), crs = st_crs(statecounties))
      
      whichpolygon<- st_intersects(statecounties, statemanders_sf)
      countycounts<-NULL
      for (i in 1:length(whichpolygon)){
      tempcount<-length(table(statemanders$species[whichpolygon[[i]]]))
      countycounts<-c(countycounts,tempcount)
      }
      statemanders_sf_summary<-cbind(statecounties,countycounts)
      
  # plotdistribution
    output$plotdistribution<- renderPlot({
      q<- ggplot(data_sf_summary) +
      geom_sf(aes(fill=counts),size=0.07) +
      scale_fill_distiller("absent - common", palette="Spectral",labels =c(),guide=guide_colourbar(title.position='top')) +
      ggtitle(specieschoice)+
      #geom_sf_label(data=protectedlonglat, aes(label = neighborprotectedarea),nudge_x = -10, nudge_y = -1.5)+
      coord_sf(xlim = c(-127, -68), ylim = c(22, 50)) +
      theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),axis.text.x=element_blank(),
            axis.title.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_blank(), 
            legend.direction='horizontal',legend.position =c(0.15,0.2),plot.title = element_text(size = 20, face = "italic"))
     q
    })
    output$plothotspots<- renderPlot({
      p<- ggplot(data_sf_summary) +
      geom_sf(fill="#008ac2",size=0.07) +
      geom_sf(data= statemanders_sf_summary, aes(fill=countycounts),size=0.07) +
        #geom_sf(data=stateboundary,size=0.3,color="goldenrod",fill=NA)+
        #geom_sf(data=topCOstate,size=0.3,color="darkblue",fill=NA)+
        #geom_sf(data=statemanders_sf_summary,size=0.3,fill="darkblue")+
        geom_sf(data=protectedlonglat, size = 2, shape = 8, color = "darkmagenta")+
        scale_fill_distiller("low - high", limits=c(0,30),palette="Spectral",labels =c(),guide=guide_colourbar(title.position='top')) +
        ggtitle(paste("Salamander Diversity of", statechoice))+
        #geom_sf_label(data=protectedlonglat, aes(label = neighborprotectedarea),nudge_x = -10, nudge_y = -1.5)+
        coord_sf(xlim = c(-127, -68), ylim = c(22, 50)) +
        theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(),axis.text.x=element_blank(),
              axis.title.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_blank(), 
              legend.direction='horizontal',legend.position =c(0.15,0.2),plot.title = element_text(size = 20, face = "italic"))
      p
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
