library(tidyverse)
library(leaflet)
library(htmlwidgets)


# data manipulation & prep ------------------------------------------------


# load organizations csv  
geo_data <- read_csv("geo_all.csv") 

# create list of codes for legend and filter 
codes_list <- c('Animal-Related',
                'Arts, Culture and Humanities', 	
                'Civil Rights, Social Action, Advocacy',
                'Mutual/Membership Benefit Organizations, Other',
                'Recreation, Sports, Leisure, Athletics',
                'Religion-Related, Spiritual Development',
                
                'Diseases, Disorders, Medical Disciplines',
                'Health - General and Rehabilitative',
                'Human Services - Multipurpose and Other',
                'Medical Research',
                'Mental Health, Crisis Intervention',
                'Social Science Research Institutes, Services',
                
                'Crime, Legal-Related',
                'International, Foreign Affairs and National Security',
                'Public Safety, Disaster Preparedness and Relief',
                
                'Educational Institutions and Related Activities',
                'Youth Development',
                
                'Community Improvement, Capacity Building',
                'Housing, Shelter',
                'Public, Society Benefit - Multipurpose and Other',
                
                'Environmental Quality, Protection and Beautification',
                'Food, Agriculture and Nutrition',
                'Science and Technology Research Institutes, Services',
                
                'Employment, Job-Related',
                'Philanthropy, Voluntarism and Grantmaking Foundations',
                
                'Unknown')

groupslist <- c('Arts and Culture',
                'Children and Family Health',
                'Crime and Safety',
                'Education and Youth',
                'Housing and Community Development',
                'Sustainability',
                'Workforce and Economic Development',
                'Unknown')

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


# define colors for each group
AC <-  "#008080"   # Teal - Arts and Culture
CFH <-"#ff0000"    # Red - Children and Family Health
CS <- "#f47742"    # Orange - Crime and Safety
EY <- "#7cc176"    # Green - Education and Youth
HCD <- "#928bd6"   # Lavendar - Housing and Community Development 
S <- "#dd8dcc"     # Pink - Sustainability
WED <- "#d6c031"   # Yellow - Workforce and Economic Development
U <- "#000000"     # Black - Unknown

# define palette for circle markers
pal <- colorFactor(c(rep(AC, 6),
                     rep(CFH, 6),
                     rep(CS, 3),
                     rep(EY, 2),
                     rep(HCD, 3),
                     rep(S, 3),
                     rep(WED, 2),
                     U), domain= codes_list)

#pal <- colorFactor(c(AC, CFH, CS, EY, HCD, S, WED, U), domain= groupslist)

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
  
  # add legend to bottom right of map
  # addLegend(
  #   title="Tax-Exempt Organizations in Baltimore",
  #   position = 'bottomright',
  #   #colors = c(AR,ACH, U),
  #   colors = rep('blue', 26),
  #   labels = codes_list)  %>%
  
# add legend to bottom left of map
addLegend(
  position = 'bottomleft',
  colors = AC,
  group = 'Arts and Culture',
  labels = 'Arts and Culture')  %>%
  
  addLegend(
    position = 'bottomleft',
    colors = CFH,
    group = 'Children and Family Health',
    labels = 'Children and Family Health')  %>%
  
  addLegend(
    position = 'bottomleft',
    colors = CS,
    group = 'Crime and Safety',
    labels = 'Crime and Safety')  %>%
  
  addLegend(
    position = 'bottomleft',
    colors = EY,
    group = 'Education and Youth',
    labels = 'Education and Youth')  %>%
  
  addLegend(
    position = 'bottomleft',
    colors = HCD,
    group = 'Housing and Community Development',
    labels = 'Housing and Community Development')  %>%
  
  addLegend(
    position = 'bottomleft',
    colors = S,
    group = 'Sustainability',
    labels = 'Sustainability')  %>%
  
  addLegend(
    position = 'bottomleft',
    colors = WED,
    group = 'Workforce and Economic Development',
    labels = 'Workforce and Economic Development')  %>%
  
  addLegend(
    position = 'bottomleft',
    colors = U,
    group = 'Unknown',
    labels = 'Unknown')  %>%
  
  
  
  ## Animal-Related group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Animal-Related',],~lon, ~lat, stroke=FALSE,
                 radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                 #create pop-up window with information for each marker
                 popup = ~ paste(NAME, "<br/>",
                                 "Address:", STREET,"<br/>",
                                 "Assets:", ASSET_AMT, "<br/>",
                                 "Income:", INCOME_AMT, "<br/>",
                                 "Revenue:", REVENUE_AMT, "<br/>",
                                 "Category:", codes),
                 
                 group="Arts and Culture") %>% 
  
  ## Arts, Culture, & Humanities group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Arts, Culture and Humanities',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Arts and Culture") %>% 
  
  ## Civil Rights, Social Action, Advocacy group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Civil Rights, Social Action, Advocacy',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Arts and Culture") %>% 
  
  ## Community Improvement, Capacity Building group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Community Improvement, Capacity Building',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Housing and Community Development") %>% 
  
  ## Crime, Legal-Related group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Crime, Legal-Related',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Crime and Safety") %>% 
  
  ## Diseases, Disorders, Medical Disciplines group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Diseases, Disorders, Medical Disciplines',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Children and Family Health") %>% 
  
  ## Educational Institutions and Related Activities group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Educational Institutions and Related Activities',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Education and Youth") %>% 
  
  ## Employment, Job-Related group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Employment, Job-Related',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Workforce and Economic Development") %>% 
  
  ## Environmental Quality, Protection and Beautification group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Environmental Quality, Protection and Beautification',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Sustainability") %>% 
  
  ## Food, Agriculture and Nutrition group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Food, Agriculture and Nutrition',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Sustainability") %>% 
  
  ## Health - General and Rehabilitative group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Health - General and Rehabilitative',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Children and Family Health") %>% 
  
  ## Housing, Shelter group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Housing, Shelter',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Housing and Community Development") %>% 
  
  ## Human Services - Multipurpose and Other group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Human Services - Multipurpose and Other',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Children and Family Health") %>% 
  
  ## International, Foreign Affairs and National Security group
  addCircleMarkers(data=geo_data[geo_data$codes == 'International, Foreign Affairs and National Security',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Crime and Safety") %>% 
  
  ## Medical Research group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Medical Research',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Children and Family Health") %>% 
  
  ## Mental Health, Crisis Intervention group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Mental Health, Crisis Intervention',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Children and Family Health") %>%
  
  ## Mutual/Membership Benefit Organizations, Other group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Mutual/Membership Benefit Organizations, Other',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Arts and Culture") %>%
  
  ## Philanthropy, Voluntarism and Grantmaking Foundations group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Philanthropy, Voluntarism and Grantmaking Foundations',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Workforce and Economic Development") %>%
  
  ## Public Safety, Disaster Preparedness and Relief group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Public Safety, Disaster Preparedness and Relief',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Crime and Safety") %>%
  
  ## Public, Society Benefit - Multipurpose and Other group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Public, Society Benefit - Multipurpose and Other',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Housing and Community Development") %>%
  
  ## Recreation, Sports, Leisure, Athletics group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Recreation, Sports, Leisure, Athletics',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Arts and Culture") %>%
  
  ## Religion-Related, Spiritual Development group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Religion-Related, Spiritual Development',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Arts and Culture") %>%
  
  ## Science and Technology Research Institutes, Services group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Science and Technology Research Institutes, Services',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Sustainability") %>%
  
  ## Social Science Research Institutes, Services group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Social Science Research Institutes, Services',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Children and Family Health") %>%
  
  ## Youth Development group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Youth Development',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Education and Youth") %>%
  
  ## Unknown group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Unknown',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), 
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Unknown") %>% 

  # Add user controls to toggle groups displayed
  addLayersControl(
    baseGroups = c('Color', 'Grayscale', 'Dark'),
    overlayGroups = groupslist,
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  
  # Hide all groups by default 
  hideGroup(groupslist)
  
map

# map export --------------------------------------------------------------


# save map as html document 
sav.file <- "/Users/jbjrV/OneDrive/Code for Baltimore/index.html"
saveWidget(map, file=sav.file, selfcontained = F)
