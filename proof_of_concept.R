library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(igraph)
library(scales)

# data manipulation & prep ------------------------------------------------


# load organizations csv  
geo_data <- read_csv("geo_all.csv") 

# define palette for circle markers -- one color for all 
pal <- colorFactor(rep('blue', 26), domain = codes_list)


# network definition ------------------------------------------------------


# map definition ---------------------------------------------------------


map <- leaflet(data = geo_data, options = leafletOptions(minZoom = 11, maxZoom = 18)) %>%
  
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = 'Grayscale') %>% 
  addTiles(group = 'Color') %>% 

  # restrict boundaries to around Baltimore (doesn't seem to work as expected)
  fitBounds(lng1 = min(geo_data$lon) - 0.11, 
            lat1 = min(geo_data$lat) - 0.11,
            lng2 = max(geo_data$lon) + 0.11, 
            lat2 = max(geo_data$lat) + 0.11) %>% 
  
  # set default view to downtown Baltimore
  setView(lng = -76.62, lat = 39.29, zoom = 12) %>%  
  
  # addCircleMarkers(data = geo_data, ~lon, ~lat, stroke = FALSE, radius = ~log(ASSET_AMT + 100), 
  #                  fillOpacity = .5, popup = ~ paste(          NAME,        "<br/>",
  #                                                  "Address:", STREET,      "<br/>",
  #                                                  "Assets:", ASSET_AMT,    "<br/>",
  #                                                  "Income:", INCOME_AMT,   "<br/>",
  #                                                  "Revenue:", REVENUE_AMT, "<br/>",
  #                                                  "Category:", codes),
  # 
  #                  group="circles") %>% 
  # 

  addMarkers(clusterOptions = markerClusterOptions(),
             data = geo_data,
             ~lon,
             ~lat,
             popup = ~ paste(NAME, "<br/>",
                             "Address:", STREET, "<br/>",
                             "Assets:", scales::comma(ASSET_AMT), "<br/>",
                             "Income:", scales::comma(INCOME_AMT), "<br/>",
                             "Revenue:", scales::comma(REVENUE_AMT), "<br/>",
                             "Category:", codes),
             group="circles") %>% 
                                                                                                   

  addResetMapButton() %>% 
  
  addMarkers(data = geo_data, ~lon, ~lat, label = geo_data$NAME, group = 'blanks', 
             icon = makeIcon( 
               iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
               iconWidth = 1, iconHeight = 1)) %>% 
  
  addSearchFeatures(targetGroups = 'blanks',
                    options = searchFeaturesOptions(zoom=17,
                                                    openPopup = TRUE,
                                                    firstTipSubmit = TRUE,
                                                    autoCollapse = TRUE,
                                                    hideMarkerOnCollapse = FALSE)) %>% 
  
  
  # Add user controls to toggle groups displayed
  addLayersControl(
  baseGroups = c('Grayscale', 'Color'),
  options = layersControlOptions(collapsed = TRUE)
  ) %>%    
  
  addControl("<P><B>Hint!</B><br> Search for your organization with the magnifying glass button.</P>",
             position='bottomright')
   

map

# map export --------------------------------------------------------------


# save map as html document 
sav.file <- "/Users/jbjrV/OneDrive/Code for Baltimore/index.html"
saveWidget(map, file=sav.file, selfcontained = F)
