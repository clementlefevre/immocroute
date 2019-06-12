library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000), ]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile), ]

function(input, output, session) {
  
  data <- reactive({
    print(input$variable)
    bretagne@data[, input$variable]
    
  })
  colorpal <-
    reactive({
      colorQuantile("Greens", data(), n = 5)
    })
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <-
    renderLeaflet({
      pal <- colorpal()
      leaflet(bretagne)    %>% addProviderTiles(providers$OpenStreetMap.HOT) %>% addPolygons(
                                                                                             color = "#444444",
                                                                                             weight = .2,
                                                                                             smoothFactor = 0.5,
                                                                                             layerId = bretagne$INSEE_COM,
                                                                                             #opacity = 1.0,
                                                                                             fillOpacity = 0.6,
                                                                                             label = ~ paste(
                                                                                               NOM_COM,
                                                                                               " - ",
                                                                                               input$variable,
                                                                                               " ",
                                                                                               round(data(), 0),
                                                                                               ":",
                                                                                               CNT_OFFERS,
                                                                                               " offers"
                                                                                             ),
                                                                                             fillColor = ~ pal(data()),
                                                                                             highlightOptions = highlightOptions(
                                                                                               color = "white",
                                                                                               weight = 2,
                                                                                               bringToFront = TRUE
                                                                                             )
      )
      
      
    })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE, ])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(
      zipdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2]
    )
  })
  
  
  
  
  
  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <-
    hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  
  output$price <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(df.selected()) == 0)
      return(NULL)
    
    p <-
      ggplot(df.selected(), aes(surfaceHabitable, prix_m2, color = age)) + geom_point()
    p
    
  })
  
  output$age <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(df.selected()) == 0)
      return(NULL)
    
    #print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
    p <-
      ggplot(df.selected(), aes(age, prix_m2, color = surfaceHabitable)) + geom_point()
    p
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  
  # Show a popup at the given location

  
  # When map is clicked, show a popup with city info
  observe({
    
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  df.selected <- eventReactive(input$map_shape_click, {
    df.clicked <-
      data.immo %>% filter(inseeCommune == input$map_shape_click$id)
    df.clicked
  })
  
  
  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(
        Action = paste(
          '<a class="go-map" href="" data-lat="',
          Lat,
          '" data-long="',
          Long,
          '" data-zip="',
          Zipcode,
          '"><i class="fa fa-crosshairs"></i></a>',
          sep = ""
        )
      )
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
