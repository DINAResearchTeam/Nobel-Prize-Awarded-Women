
##############################################################################
### LOAD PACKAGES ############################################################
##############################################################################

# required packages
packages = c("tidyverse","sf","leaflet","leafpop","geosphere","readxl")
# use this function to check if each package is on the local machine
# if a package is installed, it will be loaded
# if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE,repos='http://cran.rediris.es')
  }
  library(x, character.only = TRUE)
})
#verify they are loaded
search()
rm(list = ls())

##############################################################################
### LOAD DATA AND INITIALIZE VARIABLES #######################################
##############################################################################

if(1)
{
  ## Read excel file 
  women <- read_excel("WOMEN.xlsx")
  women <- subset(women, !is.na(women$latStudyWork))
  
  # create a matrix with the Prizes values
  steamm <- matrix("", ncol = 1, nrow = length(women$Name))
  for (i in 1:length(steamm))
  {
    if(women$Physics[i] > 0){steamm[i] <- str_c(steamm[i], "Physics (", women$Physics[i], ") ")}
    if(women$Chemistry[i] > 0){steamm[i] <- str_c(steamm[i], "Chemistry (", women$Chemistry[i], ") ")}
    if(women$PhysiologyOrMedicine[i] > 0){steamm[i] <- str_c(steamm[i], "Physiology or Medicine (", women$PhysiologyOrMedicine[i], ") ")}
    if(women$Literature[i] > 0){steamm[i] <- str_c(steamm[i], "Literature (", women$Literature[i], ") ")}
    if(women$Peace[i] > 0){steamm[i] <- str_c(steamm[i], "Peace (", women$Peace[i], ") ")}
    if(women$EconomicSciences[i] > 0){steamm[i] <- str_c(steamm[i], "Economic Sciences (", women$EconomicSciences[i], ") ")}
  }
  
  # Create the textual content to show in the popups
  content <- paste("<style> div.leaflet-popup-content {width:auto !important;}</style>",
                   sep = "<br/>",
                   str_c("<b><a href=",women$WikiAddress," target='_blank'>",women$Name,"</a></b>"),
                   "<b><a>Born:</a></b>",
                   str_c(women$BornPlace," (",women$BornYear,")"),
                   "<b><a>Study/Work:</a></b>",
                   women$StudyWorkPlace,
                   "<b><a>Known for:</a></b>",
                   women$KnownFor,
                   "<b><a>Nobel Prize for:</a></b>",
                   steamm,
                   "<b></b>"
  )
  
  
  # Settings
  circleMarkerSize = 7 # Size of the circular markers
  imageWidth = 150 # Width of the images for the popups
  lineSmooth = 48 # smoothness for the connections (do not change)
  color1 = "#FF00C3"
  #color2 = "#C300FF"
  color2 = "#5395b5"
  # https://rampgenerator.com/
  paleta = colorRampPalette(c(color1, color2))
  myColor49_1_2 = paleta(49)
}



##############################################################################
### CREATE THE WEB MAP WITH LEAFLET ##########################################
##############################################################################

if(1)
{
  mapaweb <- leaflet(options = leafletOptions(minZoom = 3,
                                              maxZoom = 16))
  # Add the default base map (Open Street Maps)
  mapaweb <- addTiles(mapaweb, group = "Map 1") 
  # Add other base maps 
  mapaweb <- addProviderTiles(mapaweb,provider = providers$Stamen.TonerLite, group = "Map 2")
  mapaweb <- addProviderTiles(mapaweb,provider = providers$Esri.WorldStreetMap, group = "Map 3" )
  mapaweb <- addProviderTiles(mapaweb,provider = providers$Stamen.Watercolor, group = "Map 4" )
  
  # Add minimap
  mapaweb <- addMiniMap(mapaweb,tiles = providers$Stamen.TonerLite, toggleDisplay = TRUE)
  
  # Add a button to zoom = 3
  mapaweb <- addEasyButtonBar(mapaweb,
                              easyButton(
                                icon = "fa-globe", title = "Zoom to Level 3",
                                onClick = JS("function(btn, map){ map.setZoom(3);}"))
  )
  
  # Add settings for the view when loading the map
  mapaweb <- setView(mapaweb,lng = -3.00, lat = 39.00, zoom = 3) 
  
  # Add markers: born
  mapaweb <- addCircleMarkers(mapaweb,                          
                              women$lngBorn,
                              women$latBorn,
                              popup = content,
                              color = color1,
                              radius = circleMarkerSize,
                              group = "Born")
  mapaweb <- addPopupImages(mapaweb,
                            women$WikiPicture,
                            group = "Born", width = imageWidth)
  
  # Add markers: died/worked
  mapaweb <- addCircleMarkers(mapaweb,                          
                              women$lngStudyWork,
                              women$latStudyWork,
                              popup = content,
                              popupOptions = popupOptions(
                                maxWidth = imageWidth*1.2),
                              color = color2,
                              radius = circleMarkerSize,
                              group = "StudyWork")
  mapaweb <- addPopupImages(mapaweb,
                            women$WikiPicture,
                            group = "StudyWork", width = imageWidth)
  
  
  for (i in 1:length(women$lngBorn))
  {
    
    if(women$latBorn[i] != women$latStudyWork[i] & women$lngBorn[i] != women$lngStudyWork[i] ){
      #calculate the intermediate values between two points, following a circle
      connections <-gcIntermediate(c(women$lngBorn[i],women$latBorn[i]),
                                   c(women$lngStudyWork[i],women$latStudyWork[i]),
                                   n=lineSmooth,# Choose the number of interpolated points (smoothness)
                                   addStartEnd = T) 
      #this code is needed to paint the connections with a color map
      lat <- connections[,2]
      long <- connections[,1]
      lat2 <- rep(lat, times=matrix(2,nrow = length(lat), ncol = 1))
      lat2 <- lat2[2:(length(lat2)-1)]
      long2 <- rep(long, times=matrix(2,nrow = length(long), ncol = 1))
      long2 <- long2[2:(length(long2)-1)]
      group <- matrix(1:(length(lat)-1))
      group2 <- rep(group, times=matrix(2,nrow = length(group), ncol = 1))
      group2f <- factor(group2)
      header <- c("lat", "long", "group")
      col<-plyr::mapvalues(group2f,from=group,to=myColor49_1_2) # add colors
      df <- data.frame(lat2, long2, group2f,col)
      
      # draw each connection as a polyline:
      for( group in levels(df$group2f))
      {
        mapaweb <- addPolylines(mapaweb, lng=~long2,lat=~lat2,
                                data=df[df$group2f==group,],
                                color=~col,
                                opacity=1,
                                weight = 1,
                                smoothFactor = 0,
                                label = as.character(women$Name[i]),
                                group = "connections")
      }
    }
    
  }
  
  # Add controls to select the base map and other layers
  mapaweb <- addLayersControl(mapaweb,
                              baseGroups = c("Map 1", "Map 2", "Map 3", "Map 4"),
                              overlayGroups = c("Born","StudyWork","connections"),
                              options = layersControlOptions(collapsed = FALSE) )
  
  # View the map
  mapaweb
}



