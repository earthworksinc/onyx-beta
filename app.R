setwd("C:/Users/Windows11/Desktop/3605d7afc8f446428159165712d19a96")


if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("USGS-R/inlmisc", dependencies = TRUE)


library(leaflet)
library(inlmisc)
library(mongolite)
library(raster)
library(ggplot2)
library(shiny)

color_palette <- ("inferno")
if.na <- #808080
bins <- 10
variance <- colorBin(color_palette, bins, na.color = if.na)


rm(ui)
rm(server)


ui <- navbarPage(tags$a(href='http://thearthworks.com',
                        tags$img(src='EWLogo.png',height='24',width='30')), id="nav",
                 
                 tabPanel("Map",
                          
                          div(class="outer",
                              
                              tags$head(
                                includeCSS("style.css"),
                                includeScript("gomap.js")
                              ),  
                              
                              leafletOutput("map", width="100%", height="100%"),
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = FALSE, top = "auto", left = 35, right = "auto", bottom = 35,
                                            width = 600, height = "auto",
                                            h5(textOutput("Well_Name")),
                                            h6(textOutput("UWI")),
                                            h6(textOutput("Operator")),
                                            #selectInput("Formation", " ",
                                             #           choices=c("HALFWAY",
                                              #                    "MONTNEY",
                                               #                   "BLUESKY",
                                                #                  "DOIG PHOSPHATE-MONTNEY",
                                                 #                 "DEBOLT",
                                                  #                "BALDONNEL",
                                                   #               "DOIG")),
                                            plotOutput("plot", height=300, width = 550)
                              )
                          )
                 ),
                 tabPanel("Data",
                          
                          dataTableOutput ("tabledata"),
                 ),
                 tabPanel("About",
                          h2("Onyx"),
                          h4("Onyx is a web application designed to leverage the capabilities of a cloud database integrated with geospatial systems and machine learning methods. Current version of Onyx presents the Gas production in the Montney Shale Gas resource in NorthEast British Columbia, Canada.
                             The application enables a geographic view to the database with on-spot analytics. Furthermore, the option to view and download the tabular data enables the users to expand thier interpretation and analysis workflows."),
                          h2("Montney Shale Gas Resource"),
                          h4("Spread across Alberta and British Columbia, the Montney Shale Gas resource is a Lower Triassic Age formation with a gas production rate of 0.9 to 4 million cubic feet of gas per day. 615 drilling wells including horizontal, directional and fracked wells are mapped.
                             The production decline curve for water, oil and gas using op-spot analytics present the change in production of each well, overtime. A geospatial map defines the zones with high production rates for the volumetric production of Gas, based on the dataset."),
                          h2("Application Features"),
                          tags$img(src='Map.png'),
                          tags$img(src='Data.png')
                          
                          
                          
                 )
                 
)


server <- function(input, output, session) {
  
  
  
  IDW_raster <- raster(paste0("IDW_Grid_raster.tif"))
  IDW_contour <- rasterToContour(IDW_raster)
  IDW_points <- rasterToPoints(IDW_raster)
  
  
  
  
  
  Index_Mongo <- mongo(collection = "index", 
                       db = "geo", 
                       url = "mongodb+srv://earthworks:123abcABS@mvp.fggzy.mongodb.net/myFirstDatabase?retryWrites=true&w=majority")
  
  
  Ind = reactive({
    Index_Mongo$find("{}")
  })
  
  Prod_Mongo <- mongo(collection = "production", 
                      db = "geo",
                      url = "mongodb+srv://earthworks:123abcABS@mvp.fggzy.mongodb.net/myFirstDatabase?retryWrites=true&w=majority")
  
  
  Prod = reactive({
    Prod_Mongo$find("{}")
  })
  
  
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      attributionControl=FALSE))%>%setView(lng=-122.3394083 , lat=57.02672778,  zoom = 10)%>%
      addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", 
               attribution = 'Google', 
               group = "Sattelite Imagery")%>%
      addRasterImage(IDW_raster,
                     opacity = 0.7,
                     col = colorBin(palette = "GnBu", 
                                    bins = 50,
                                    alpha = 1,
                                    domain = NULL,  
                                    na.color = NA,
                                    pretty = TRUE),
                     group = "Geostatistical Map")%>%
      addPolylines(data=IDW_contour,
                   col="black", 
                   weight = 0.5,
                   group = "Geostatistical Map")%>%
      addLegend("bottomright", 
                pal = colorBin(palette = "GnBu", 
                               IDW_points,
                               bins = 5), 
                value = IDW_points, 
                title = "Gas Production e3m3",
                group = "Geostatistical Map", 
                labFormat = labelFormat(digit = 1 ) )%>%
      addAwesomeMarkers(
        data = Ind(), 
        lng =~Longitude, 
        lat = ~Latitude, 
        label = ~Well_Name,
        #color = ~variance(click_data()$Gas_prod_vol),
        clusterOptions = markerClusterOptions(),
        #clusterOptions = markerClusterOptions(
        #  iconCreateFunction=JS("function (cluster) {    
    #var markers = cluster.getAllChildMarkers();
    #var childCount = cluster.getChildCount();
    #var p = 0; 
    #for (i = 0; i < markers.length; i++) {
    #  if(markers[i].options.col === 'red'){
    #    p = 3;
    #    break;
    #  }
    #}
    #if(p === 1){
    #  c = 'rgba(195, 196, 192, 0.7);'
    #} else {
    #  c = 'rgb(195, 196, 192, 1);'
    #}
    #return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(5, 5)});
  #}"),
          #showCoverageOnHover = FALSE,
          #spiderfyOnMaxZoom = TRUE,
          #spiderLegPolylineOptions = list(weight = 0, 
          #                                color = "#222", 
          #                                opacity = 0.5),
          #maxClusterRadius = 5,
        #  
        #  
        #),
        
        layerId = ~Wa_num,
        group = "Well Locations",
        popup = ~paste("Operator:", Operator,"<br/>", "Well ID:", UWI, "<br/>", "<b/>" , Well_Name))%>%
      addLayersControl(overlayGroups = c( "Well Locations"), 
                       baseGroups = c("Sattelite Imagery"), options = layersControlOptions())%>%
      addEasyButtonBar(
        easyButton(
          icon = htmltools::HTML("fa-crosshairs"),
          onClick = JS("function(btn, map){map.setZoom(9);}")))%>%
      hideGroup("Legend")%>%
      AddSearchButton(group = "Well Locations", zoom = 15, textPlaceholder = "Site Name")%>%
      addSimpleGraticule(interval = 1, 
                         group = "Graticule")
    
    
    
  })
  
  click_data <- reactive({
    click_Wa_num <- input$map_marker_click$id
    Prod()[Prod()$Wa_num %in% click_Wa_num,]
  })
  
  
  click_data2 <- reactive({
    click_Wa_num <- input$map_marker_click$id
    Ind()[Ind()$Wa_num %in% click_Wa_num,]
  })
  
  
  output$plot=renderPlot({
    ggplot(data=click_data(),aes(x=Prod_period, y=Gas_prod_vol))+
      geom_line(col="#EEB868")+
      geom_point(aes(x=Prod_period, y=Gas_prod_vol), col="#EEB868", size=3)+
      geom_line(aes(x=Prod_period, y=Oil_prod_vol), col="#EF767A")+
      geom_point(aes(x=Prod_period, y=Oil_prod_vol), col="#EF767A", size=3)+
      geom_line(aes(x=Prod_period, y=Water_prod_vol), col="#2e7d36")+
      geom_point(aes(x=Prod_period, y=Water_prod_vol), col="#2e7d36", size=3)+
      geom_point(aes(x=Prod_period, y=Water_prod_vol, col="#2e7d36"))+
      labs(color='Production')+
      scale_colour_manual(values=c("Oil"="#EF767A","Gas"="#EEB868","Water"="#2e7d36"))+
      theme(
        legend.background = element_rect(), 
        legend.position = "top",
        legend.spacing = unit(4.0, 'cm'),
        legend.key.width = unit(5, "line"),
        plot.title = element_text(size=13, margin = margin(b=5, t=10), hjust = 0.5),
        plot.subtitle = element_text(size = 11, margin = margin(b=0), hjust = 0.5),
        axis.text.x= element_text(color = 'black', size = 09, angle = 90, margin = margin(b=15)),
        axis.text.y= element_text(color = 'black', size = 09,angle = 0, margin = margin(l=15)),
        axis.title.y = element_text(color = 'black', size=11, margin = margin(t=15, l=15)),
        legend.text = element_text(color = 'black', size=10),
        panel.grid.minor = element_line(colour = 'grey', linetype = 'dashed', size=0.5),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dashed', size=0.1),
        panel.background = element_blank()
      )+
      labs(x = "Production Period", y = "Production Volume e3m3")
  })
  
  
  
  output$UWI=renderText({
    unique(click_data2()$UWI)
  })
  
  output$Operator=renderText({
    unique(click_data2()$Operator)
  })
  
  output$Well_Name=renderText({
    unique(click_data2()$Well_Name)
  })
  
  output$Formation=renderText({
    unique(click_data()$Formtn_code)
  })
  
  output$tabledata <- renderDataTable({
    as.data.frame(Prod())
    
  })
  
}


shinyApp(ui=ui, server=server)






