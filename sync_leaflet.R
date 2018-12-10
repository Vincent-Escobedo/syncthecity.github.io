library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(igraph)
library(sp)

# data manipulation & prep ------------------------------------------------


# load organizations csv  
geo_data <- read_csv("geo_all.csv") 

# define lists of rollup groups
AC_list <- c('Animal-Related',
             'Arts, Culture and Humanities', 	
             'Civil Rights, Social Action, Advocacy',
             'Mutual/Membership Benefit Organizations, Other',
             'Recreation, Sports, Leisure, Athletics',
             'Religion-Related, Spiritual Development')

CFH_list <- c('Diseases, Disorders, Medical Disciplines',
              'Health - General and Rehabilitative',
              'Human Services - Multipurpose and Other',
              'Medical Research',
              'Mental Health, Crisis Intervention',
              'Social Science Research Institutes, Services')

CS_list <- c('Crime, Legal-Related',
             'International, Foreign Affairs and National Security',
             'Public Safety, Disaster Preparedness and Relief')

EY_list <- c('Educational Institutions and Related Activities',
             'Youth Development')

HCD_list <- c('Community Improvement, Capacity Building',
              'Housing, Shelter',
              'Public, Society Benefit - Multipurpose and Other')

S_list <- c('Environmental Quality, Protection and Beautification',
            'Food, Agriculture and Nutrition',
            'Science and Technology Research Institutes, Services')

WED_list <- c('Employment, Job-Related',
              'Philanthropy, Voluntarism and Grantmaking Foundations')

# add variable for rollup group to geo data
geo_data <- geo_data %>% 
  mutate(group = ifelse(codes %in% AC_list, 'Arts and Culture',
                        
                 ifelse(codes %in% CFH_list, 'Children and Family Health', 
                               
                 ifelse(codes %in% CS_list, 'Crime and Safety', 
                                      
                 ifelse(codes %in% EY_list, 'Education and Youth', 
                                             
                 ifelse(codes %in% HCD_list, 'Housing and Community Development',
                                                    
                 ifelse(codes %in% S_list, 'Sustainability',
                                                           
                 ifelse(codes %in% WED_list, 'Workforce and Economic Development',
                                                                  
                 'Unknown'
         ))))))))

# create table of codes for map definition
codes <- table(geo_data$codes) %>% 
  as.data.frame()

# create list of codes for legend and filter 
codes_list <- c('Animal-Related',
                'Arts, Culture and Humanities', 	
                'Civil Rights, Social Action, Advocacy',
                'Community Improvement, Capacity Building',
                'Crime, Legal-Related',
                'Diseases, Disorders, Medical Disciplines',
                'Educational Institutions and Related Activities',
                'Employment, Job-Related',
                'Environmental Quality, Protection and Beautification',
                'Food, Agriculture and Nutrition',
                'Health - General and Rehabilitative',
                'Housing, Shelter',
                'Human Services - Multipurpose and Other',
                'International, Foreign Affairs and National Security',
                'Medical Research',
                'Mental Health, Crisis Intervention',
                'Mutual/Membership Benefit Organizations, Other',
                'Philanthropy, Voluntarism and Grantmaking Foundations',
                'Public Safety, Disaster Preparedness and Relief',
                'Public, Society Benefit - Multipurpose and Other',
                'Recreation, Sports, Leisure, Athletics',
                'Religion-Related, Spiritual Development',
                'Science and Technology Research Institutes, Services',
                'Social Science Research Institutes, Services',
                'Youth Development',
                'Unknown')


# define palette for circle markers -- one color for all 
pal <- colorFactor(rep('blue', 26), domain = codes_list)


# network definition ------------------------------------------------------

# define connections (edges) between organizations -- from and to
edges <- geo_data %>% 
  mutate(from = NAME) %>% 
  # randomly select an organization node for each organization to work with 
  mutate(to = sample(geo_data$NAME, 3550, replace = TRUE)) %>% 
  select('from', 'to')

vertices <- geo_data %>% 
  select('NAME', 'lon', 'lat')

vertices_u <- vertices %>% 
  unique() 

g <- graph.data.frame(edges, directed = FALSE, vertices = vertices)

data <- edges %>% 
  select('from', 'to')

v <- unique(c(data[,1], data[,2])) #Define v from both columns in data
v <- na.omit(v)
e <- na.omit(data)


g <- graph.data.frame(e, directed = FALSE)
plot(g, vertex.label = NA, vertex.size = 3)

# map definition ---------------------------------------------------------


map <- leaflet(data = geo_data, options = leafletOptions(minZoom = 11, maxZoom = 18)) %>%
  
  addTiles(group = 'Color') %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = 'Grayscale') %>% 
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'Dark') %>% 
  
  # restrict boundaries to around Baltimore (doesn't seem to work as expected)
  fitBounds(lng1 = min(geo_data$lon) - 0.11, 
            lat1 = min(geo_data$lat) - 0.11,
            lng2 = max(geo_data$lon) + 0.11, 
            lat2 = max(geo_data$lat) + 0.11) %>% 
  
  # set default view to downtown Baltimore
  setView(lng= -76.62, lat=39.29,zoom=12) %>% 
  
  ## Animal-Related group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Animal-Related',],~lon, ~lat, stroke=FALSE,
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up window with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Animal-Related") %>% 
  
  ## Arts, Culture, & Humanities group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Arts, Culture and Humanities',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Arts, Culture and Humanities") %>% 
  
  ## Civil Rights, Social Action, Advocacy group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Civil Rights, Social Action, Advocacy',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Civil Rights, Social Action, Advocacy") %>% 
  
  ## Community Improvement, Capacity Building group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Community Improvement, Capacity Building',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Community Improvement, Capacity Building") %>% 
  
  ## Crime, Legal-Related group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Crime, Legal-Related',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Crime, Legal-Related") %>% 
  
  ## Diseases, Disorders, Medical Disciplines group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Diseases, Disorders, Medical Disciplines',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Diseases, Disorders, Medical Disciplines") %>% 
  
  ## Educational Institutions and Related Activities group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Educational Institutions and Related Activities',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Educational Institutions and Related Activities") %>% 
  
  ## Employment, Job-Related group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Employment, Job-Related',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Employment, Job-Related") %>% 
  
  ## Environmental Quality, Protection and Beautification group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Environmental Quality, Protection and Beautification',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Environmental Quality, Protection and Beautification") %>% 
  
  ## Food, Agriculture and Nutrition group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Food, Agriculture and Nutrition',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Food, Agriculture and Nutrition") %>% 
  
  ## Health - General and Rehabilitative group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Health - General and Rehabilitative',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Health - General and Rehabilitative") %>% 
  
  ## Housing, Shelter group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Housing, Shelter',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Housing, Shelter") %>% 
  
  ## Human Services - Multipurpose and Other group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Human Services - Multipurpose and Other',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Human Services - Multipurpose and Other") %>% 
  
  ## International, Foreign Affairs and National Security group
  addCircleMarkers(data=geo_data[geo_data$codes == 'International, Foreign Affairs and National Security',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="International, Foreign Affairs and National Security") %>% 
  
  ## Medical Research group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Medical Research',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Medical Research") %>% 
  
  ## Mental Health, Crisis Intervention group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Mental Health, Crisis Intervention',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Mental Health, Crisis Intervention") %>%
  
  ## Mutual/Membership Benefit Organizations, Other group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Mutual/Membership Benefit Organizations, Other',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Mutual/Membership Benefit Organizations, Other") %>%
  
  ## Philanthropy, Voluntarism and Grantmaking Foundations group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Philanthropy, Voluntarism and Grantmaking Foundations',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Philanthropy, Voluntarism and Grantmaking Foundations") %>%
  
  ## Public Safety, Disaster Preparedness and Relief group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Public Safety, Disaster Preparedness and Relief',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Public Safety, Disaster Preparedness and Relief") %>%
  
  ## Public, Society Benefit - Multipurpose and Other group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Public, Society Benefit - Multipurpose and Other',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Public, Society Benefit - Multipurpose and Other") %>%
  
  ## Recreation, Sports, Leisure, Athletics group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Recreation, Sports, Leisure, Athletics',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Recreation, Sports, Leisure, Athletics") %>%
  
  ## Religion-Related, Spiritual Development group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Religion-Related, Spiritual Development',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Religion-Related, Spiritual Development") %>%
  
  ## Science and Technology Research Institutes, Services group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Science and Technology Research Institutes, Services',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Science and Technology Research Institutes, Services") %>%
  
  ## Social Science Research Institutes, Services group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Social Science Research Institutes, Services',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Social Science Research Institutes, Services") %>%
  
  ## Youth Development group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Youth Development',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Youth Development") %>%
  
  ## Unknown group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Unknown',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), 
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes, "<br/>",
                                   "Grouping:", group),
                   
                   group="Unknown") %>% 
  
  # Add user controls to toggle groups displayed
  addLayersControl(
    baseGroups = c('Color', 'Grayscale', 'Dark'),
    overlayGroups = codes_list,
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  
  # Add map reset button
  addResetMapButton() %>% 
  
  # Add search feature
  addSearchFeatures(targetGroups = codes_list,
                                   options = searchFeaturesOptions(zoom = 14,
                                                                   openPopup = TRUE,
                                                                   firstTipSubmit = TRUE,
                                                                   autoCollapse = TRUE,
                                                                   hideMarkerOnCollapse = TRUE)) %>% 
  
  # Hide all groups by default 
  hideGroup(codes_list)

map

# map export --------------------------------------------------------------


# save map as html document 
sav.file <- "/Users/jbjrV/OneDrive/Code for Baltimore/index.html"
saveWidget(map, file=sav.file, selfcontained = F)
