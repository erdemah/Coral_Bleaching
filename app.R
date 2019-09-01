#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# ui.R
library(shiny)
library(leaflet)

ui <- fluidPage( 
  
  # Application title
  titlePanel("Coral Bleaching Over The Years"),
  
  fluidRow( column (2,
                    selectInput("variable", "Coral Type", 
                                c("Blue Corals" = "blue corals", 
                                  "Hard Corals" = "hard corals",
                                  "Sea Fans" = "sea fans",
                                  "Sea Pens" = "sea pens",
                                  "Soft Corals" = "soft corals",
                                  "All Types" = "all")),
                    selectInput("variable2", "Smoother", 
                                c("lm" = "lm", 
                                  "glm" = "glm",
                                  "loess" = "loess")),
                    selectInput("variable3", "Map", 
                                c("NatGeo" = "Esri.NatGeoWorldMap", 
                                  "Terrain" = "Stamen.Terrain",
                                  "DeLorme" = "Esri.DeLorme",
                                  "Wikimedia" = "Wikimedia"))
  ),
  column(10,
         leafletOutput("mymap")),
  hr(),
  plotOutput("bleachingPlot"),
  hr()
  )
)

#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# server.R
library(shiny)
library(ggplot2)
library(leaflet)


#server logic
server <- function(input, output) {
  
  coral_data <- read.csv("assignment-02-data-formated.csv")
  coral_data$value <- as.numeric(sub("%","",coral_data$value)) 
  coral_data <- coral_data[order(coral_data$latitude),]
  rownames(coral_data) = 1:nrow(coral_data)
  coral_data$location = factor(coral_data$location, levels=unique(coral_data$location))
  
  #rendering leaflet
  output$mymap <- renderLeaflet({ # create leaflet map
    if (input$variable != "all"){
      server_data <- coral_data[coral_data$coralType == input$variable,]
    }
    else{
      server_data <- coral_data
    }
    pal = colorFactor(palette = c("blue","green", "yellow", "red","purple"),
                      domain=server_data$coralType)
    
    map1 <- leaflet(data=server_data) %>% 
      addProviderTiles(input$variable3) %>% 
      addCircleMarkers(lng = ~ longitude, lat=~ latitude, color = ~ pal(coralType),
                       label = paste(server_data$location),radius = ~value/20)%>% #value/20 to shrink size
      addLegend(position = "bottomright", # position where the legend should appear
                pal = pal, # pallete color
                values = ~coralType,
                title = "Coral Type", # title of the legend
                opacity = 0.8 # Opacity of legend
      )
  })
  
  output$bleachingPlot <- reactivePlot(function() {
    # check for the input variable
    if (input$variable != "all") {
      server_data <- coral_data[coral_data$coralType == input$variable,]
      plot1 <- ggplot(server_data, aes(x=year,y=value)) + geom_point(aes(color=coralType), size=0.9,shape=9) + 
        facet_wrap(~location, nrow = 2) + 
        labs(x= "Years", y="Bleaching(%)",title = "Bleaching percentage on different coral type and site") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 0.1)) + 
        scale_color_discrete(name = "Coral Type") + 
        theme(plot.title = element_text(lineheight=0.1, hjust = 0.5, color = "Blue")) +
        theme(legend.position="bottom") +
        scale_y_continuous(breaks=seq(0,100,20)) + 
        geom_line(stat = "smooth", method = input$variable2, aes(x=year,y=value), color = "blue", alpha = 0.6, size = 0.5) + 
        geom_smooth(method = input$variable2, color = NA, alpha = 0.4)
      
      print(plot1)
    }
    else{
      server_data <- coral_data
      plot1 <- ggplot(server_data, aes(x=year,y=value)) + geom_point(aes(color=coralType), size=0.9,shape=9) + 
        facet_grid(coralType~location) + 
        labs(x= "Years", y="Bleaching(%)",title = "Bleaching percentage on different coral type and site") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 0.1)) + 
        scale_color_discrete(name = "Coral Type") + 
        theme(plot.title = element_text(lineheight=0.1, hjust = 0.5, color = "Blue")) +
        theme(legend.position="bottom") +
        scale_y_continuous(breaks=seq(0,100,20)) + 
        geom_line(stat = "smooth", method = input$variable2, aes(x=year,y=value), color = "blue", alpha = 0.6, size = 0.5) + 
        geom_smooth(method = input$variable2, color = NA, alpha = 0.4)
      
      print(plot1)
    }
    
  })
  
}
shinyApp(ui = ui, server = server)
