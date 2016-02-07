library(shiny)
library(dygraphs)
library(dplyr)
library(readr)
library(lubridate)
library(xts)
library(wcUtils)
library(sp)
library(leaflet)
library(shinyjs)

shinyServer(function(input, output, session) {
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  
  upload_ptt <- reactiveValues(
    dat_files = NULL,
    zipfile = NULL,
    ptt = NULL
  )
  
  observe({
    input$clearPTT
    upload_ptt$zipfile <- NULL
    upload_ptt$ptt <- NULL
    upload_ptt$dat_files <- NULL
  })
  
  observeEvent(input$zipfile, {
    upload_ptt$zipfile <- input$zipfile
    upload_ptt$ptt <- strsplit(input$zipfile$name,'\\.')[[1]][1]
    upload_ptt$dat_files <- unzip(input$zipfile$datapath,
                          exdir = tempdir())
  })

  ptt <- reactive({
    if(!is.null(upload_ptt$ptt)){
      return(upload_ptt$ptt)
    } else {
      return(NULL)
    }
  })
  
  dat_files <- reactive({
    if(!is.null(upload_ptt$dat_files)) {
    return(upload_ptt$dat_files)
    } else return(NULL)
  })
  
  histos <- reactive({
    req(dat_files())
    histo_file <- dat_files()[grep("*-Histos.csv", 
                                            dat_files())]
    res <- wcUtils::read_histos(histo_file)
    return(res)
  })
  
  locs <- reactive({
    req(dat_files())
    locs_fastgps <- dat_files()[grep(paste0("*",ptt(),"-1-Locations.csv"), 
                                   dat_files())]
    locs_data <- dat_files()[grep(paste0("*",ptt(),"-Locations.csv"), 
                                 dat_files())]
    locs_fastgps <- try(read.csv(locs_fastgps))
    locs_data <- try(read.csv(locs_data))
    
    if (class(locs_fastgps) != "try-error") {
      return(locs_fastgps)
    } else {
      return(locs_data)
    }
  })
  
  behav_file <- reactive({
    req(dat_files())
    behav_file <- try(dat_files()[grep("*-Behavior.csv", dat_files())])
    if (class(behav_file) == "try-error") {
      return(NULL)
    } else {
      return(behav_file)
    }
  })
  
  output$zipFileInput <- renderUI({
    input$clearPTT
    fileInput('zipfile', NULL, width="60%")
  })
  
  output$summary <- renderText({
    return(paste("Uploaded file: ", upload_ptt$zipfile$name))
  })
  
  output$timelines <- renderDygraph({
    validate(need(histos(), "No Histos File Detected"))

      t <- histos() %>%
        wcUtils::tidyTimelines(.) %>%
        select(datadatetime, percent_dry) %>%
        arrange(datadatetime)
      
      t <- data.frame(datadatetime =
                        seq(min(t$datadatetime),
                            max(t$datadatetime), 
                            by = '1 hour')) %>% 
        left_join(t, by = "datadatetime")
      
      t <- xts(t, t$datadatetime)
      t <- t[, "percent_dry"]
      
      dygraph(t, main = "Percent Dry", group = 'wc_plots') %>%
        dySeries(
          "percent_dry",
          label = "Percent Dry",
          fillGraph = TRUE,
          stepPlot = TRUE,
          color = "#D55E00",
          drawPoints = FALSE,
          strokeWidth = 0
        ) %>%
        dyOptions(useDataTimezone = TRUE,
                  fillAlpha = 0.75) %>%
        dyAxis("y", label = "Percent Dry per Hour",
               valueRange = c(0, 101)) %>%
        dyAxis("x", label = "Time") %>%
        dyRangeSelector()
  })
  
  output$dives <- renderDygraph({
    validate(need(behav_file(),"No Behavior File Detected"))
      d <- read.csv(behav_file()) %>%
        filter(What != "Message") %>%
        mutate(
          start_dt =
            lubridate::parse_date_time(Start,
                                       "H!:M!:S! d!-m!-Y!"),
          DepthMin = ifelse(What == "Surface", 0, DepthMin)
        ) %>%
        select(start_dt, DepthMin) %>%
        arrange(start_dt)
      
      d <- xts(d, d$start_dt)
      d <- d[, "DepthMin"]
      
      p <- dygraph(d, main = "Dive Behavior", group = 'wc_plots') %>%
        dySeries(
          "DepthMin",
          label = "Depth",
          fillGraph = TRUE,
          stepPlot = TRUE,
          color = "#0071BC",
          drawPoints = FALSE,
          strokeWidth = 0
        ) %>%
        dyOptions(useDataTimezone = TRUE,
                  fillAlpha = 0.75) %>%
        dyAxis("y", label = "Dive Depth (m)") %>%
        dyAxis("x", label = "Time")
      
      msg <- read.csv(behav_file()) %>%
        filter(What == "Message") %>%
        mutate(
          start_dt =
            lubridate::parse_date_time(Start,
                                       "H!:M!:S! d!-m!-Y!"),
          end_dt =
            lubridate::parse_date_time(End,
                                       "H!:M!:S! d!-m!-Y!")
        ) %>% select(start_dt, end_dt) %>% arrange(start_dt)
      
      for (i in 1:nrow(msg)) {
        p <- p %>% dyShading(from = msg$start_dt[i], to = msg$end_dt[i])
      }
      return(p)
  })
  
  output$dive_histos_vis <- reactive({
      validate(need(histos(),"No Histos File Detected"))
      h <- histos()[["histos"]] %>%
        filter(histtype == 'DiveDepth')
      if (nrow(h) < 1) {
        return(0)
      } else {
        return(1)
      }
  })
  
  output$dive_behav_vis <- reactive({
    t <- try(read.csv(behav_file()))
    if (class(t) == "try-error") {
      return(0)
    } else {
      return(1)
    }
  })
  
  output$dive_histos <- renderDygraph({
    validate(need(histos(),"No Histos File Detected"))
      h <- histos()[["histos"]] %>%
        filter(histtype == 'DiveDepth')
      if (nrow(h) < 1) {
        return(invisible())
      } else {
        nbins <- max(h$numbins)
        bin_string <- paste0("bin1:bin", nbins)
        
        h <- h %>% select_(.dots = c("date", bin_string))
        h <- xts(h, h$date)
        h <- h[, -which(names(h) %in% c("date", "datadatetime"))]
        
        cols <- colorRampPalette(rev(viridis(nbins)))
        
        dygraph(h, main = "Dive Depth Histograms", group = 'wc_plots') %>%
          dyOptions(
            useDataTimezone = TRUE,
            stepPlot = TRUE,
            fillGraph = TRUE,
            fillAlpha = 0.75,
            colors = cols(nbins)
          ) %>%
          dyAxis("y", label = "Number of Dives") %>%
          dyAxis("x", label = "Time")
      }
  })
  
  output$locations <- renderLeaflet({
    validate(need(locs(),"No Locations File Detected"))
      locs <- locs() %>%
        mutate(loc_dt = lubridate::parse_date_time(Date,
                                                   "H!:M!:S! d!-m!-Y!")) %>%
        select(loc_dt, Latitude, Longitude)
      if (!is.null(input$longlat_date_window)) {
        locs <- filter(
          locs,
          loc_dt >= input$longlat_date_window[[1]] &
            loc_dt <= input$longlat_date_window[[2]]
        )
      }
      coordinates(locs) <- ~ Longitude + Latitude
      proj4string(locs) <- CRS('+proj=longlat +ellps=WGS84')
      leaflet(locs) %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addCircleMarkers(
          radius = 2,
          stroke = FALSE,
          color = "#e31c3d",
          fillOpacity = 0.5
        )
  })
  
  output$longlat <- renderDygraph({
    req(upload_ptt$zipfile)
    if (!is.null(input$dygraph_date_window)) {
      strftime(input$dygraph_date_window[[1]], "%d %b %Y")
    }
      locs <- read.csv(unz(upload_ptt$zipfile$datapath,
                           paste0(ptt(), "-Locations.csv"))) %>%
        mutate(loc_dt = lubridate::parse_date_time(Date,
                                                   "H!:M!:S! d!-m!-Y!")) %>%
        select(loc_dt, Longitude, Latitude)
      locs <- xts(locs, locs$loc_dt)
      locs <- locs[, -which(names(locs) %in% c("loc_dt"))]
      dygraph(locs, main = "Geographic Coordinates",
              group = 'wc_plots') %>%
        dyOptions(useDataTimezone = TRUE, drawPoints = TRUE) %>%
        dySeries("Latitude", axis = "y2") %>%
        dyAxis("y", label = "long", drawGrid = FALSE) %>%
        dyAxis("y2", label = "lat", drawGrid = FALSE) %>%
        dyAxis("x", label = "Time", drawGrid = TRUE) %>%
        dyLegend(width = 400) %>%
        dyRangeSelector()

  })
  
  outputOptions(output, 'dive_histos_vis', suspendWhenHidden = FALSE)
  outputOptions(output, 'dive_behav_vis', suspendWhenHidden = FALSE)
})
