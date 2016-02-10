library(shiny)
library(dygraphs)
library(dplyr)
library(readr)
library(lubridate)
library(xts)
library(wcUtils)
library(sp)
library(leaflet)
library(viridis)
library(shinyjs)

shinyServer(function(input, output, session) {
  
  input_dat <- reactiveValues(
    dat_files = NULL,
    zipfile = NULL,
    ptt = NULL,
    data_load = FALSE,
    select_deployid = FALSE,
    id = NULL
  )
  
  observeEvent(input$zipfile, {
    input_dat$zipfile <- input$zipfile
    input_dat$ptt <- strsplit(input$zipfile$name,'\\.')[[1]][1]
    input_dat$dat_files <- unzip(input$zipfile$datapath,
                          exdir = tempdir())
    input_dat$data_load <- TRUE
  })

  observeEvent(input$getWCdata,{
    with(input_dat,{
      dat_files = NULL
      zipfile = NULL
      ptt = NULL
      data_load = FALSE
      select_deployid = FALSE
      id = NULL
    })
    input_dat$ptt <- as.character(input$ptt_integer)
    res <- wcUtils::wcPOST(keyfile = input$keyfile$datapath)
    ptt_dat <- wcUtils::wcGetPttID(res,ptt=as.character(input_dat$ptt))
    if(length(ptt_dat$ids) == 1) {
      input_dat$id <- ptt_dat$id
    } else if(nrow(subset(ptt_dat$df,!is.na(deployid))) == 1) {
      input_dat$id <- ptt_dat$df$id[which(!is.na(ptt_dat$df$deployid))]
      warning('multiple deployments identified; returning only with deployid')
    } else {
      warning('more than one deployid found for this PTT')
      input_dat$select_deployid <- TRUE
      input_dat$id <- NULL
    }
    if(!is.null(input_dat$id)) {
      input_dat$zipfile <- wcUtils::wcGetZip(id=input_dat$id,
                                             keyfile=input$keyfile$datapath)
      input_dat$dat_files <- unzip(input_dat$zipfile,
                                   exdir = tempdir())
      input_dat$data_load <- TRUE
    }
  })
  
  output$app_mode <- reactive({
    ifelse(input_dat$data_load,"show_results","input_only")
  })
  outputOptions(output, "app_mode", suspendWhenHidden = FALSE)
  
  output$current_ptt <- renderText({
    paste(input_dat$ptt)
  })
  
  dat_files <- reactive({
    return(input_dat$dat_files)
  })
  
  histos <- reactive({
    req(dat_files())
    histo_file <- dat_files()[grep("*-Histos.csv", 
                                            dat_files())]
    res <- wcUtils::read_histos(histo_file)
    return(res)
  })
  
  all_locs <- reactive({
    req(dat_files())
    message(paste('examining location data for ptt',input_dat$ptt))
    locs_fastgps <- dat_files()[grep(paste0("*",input_dat$ptt,"-1-Locations.csv"), 
                                   dat_files())]
    locs_data <- dat_files()[grep(paste0("*",input_dat$ptt,"-Locations.csv"), 
                                 dat_files())]
    locs_fastgps <- try(read.csv(locs_fastgps))
    locs_data <- try(read.csv(locs_data))
    
    if (class(locs_fastgps) != "try-error") {
      message(paste('using fastgps data for ptt',input_dat$ptt))
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
    fileInput('zipfile', 'Upload Zip File')
  })
  
  output$keyFileInput <- renderUI({
    fileInput('keyfile', 'Upload Key File')
  })
  
  output$pttInput <- renderUI({
    numericInput('ptt_integer', 'PTT ID Integer',max=999999,min=1111, value=NA)
  })
  
  output$getDataButton <- renderUI({
    actionButton('getWCdata',label="Get Data from Wildlife Computers")
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
  outputOptions(output, 'dive_histos_vis', suspendWhenHidden = FALSE)
  
  output$dive_behav_vis <- reactive({
    t <- try(read.csv(behav_file()))
    if (class(t) == "try-error") {
      return(0)
    } else {
      return(1)
    }
  })
  outputOptions(output, 'dive_behav_vis', suspendWhenHidden = FALSE)
  
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
  
  locs <- reactive({
    if(!is.null(input$timelines_date_window)) {
      locs <- all_locs() %>%
        mutate(loc_dt = lubridate::parse_date_time(Date,
                                                   "H!:M!:S! d!-m!-Y!")) %>%
        select(loc_dt, Latitude, Longitude) %>% 
      filter(loc_dt >= input$timelines_date_window[[1]] &
               loc_dt <= input$timelines_date_window[[2]])
    } else {
      locs <- all_locs() %>%
        mutate(loc_dt = lubridate::parse_date_time(Date,
                                                   "H!:M!:S! d!-m!-Y!")) %>%
        select(loc_dt, Latitude, Longitude)
    }
  })
  
  output$date_range <- renderText({
    if (!is.null(input$timelines_date_window))
      paste(
      strftime(input$timelines_date_window[[1]], "%d %b %Y"),
      "to",
      strftime(input$timelines_date_window[[2]], "%d %b %Y")
      )
  })

  output$locations <- renderLeaflet({
    req(locs())
      locs <- locs()
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
  
  # output$longlat <- renderDygraph({
  #   req(input_dat$zipfile)
  #   if (!is.null(input$dygraph_date_window)) {
  #     strftime(input$dygraph_date_window[[1]], "%d %b %Y")
  #   }
  #     locs <- read.csv(unz(input_dat$zipfile$datapath,
  #                          paste0(input_dat$ptt, "-Locations.csv"))) %>%
  #       mutate(loc_dt = lubridate::parse_date_time(Date,
  #                                                  "H!:M!:S! d!-m!-Y!")) %>%
  #       select(loc_dt, Longitude, Latitude)
  #     locs <- xts(locs, locs$loc_dt)
  #     locs <- locs[, -which(names(locs) %in% c("loc_dt"))]
  #     dygraph(locs, main = "Geographic Coordinates",
  #             group = 'wc_plots') %>%
  #       dyOptions(useDataTimezone = TRUE, drawPoints = TRUE) %>%
  #       dySeries("Latitude", axis = "y2") %>%
  #       dyAxis("y", label = "long", drawGrid = FALSE) %>%
  #       dyAxis("y2", label = "lat", drawGrid = FALSE) %>%
  #       dyAxis("x", label = "Time", drawGrid = TRUE) %>%
  #       dyLegend(width = 400) %>%
  #       dyRangeSelector()
  # 
  # })
  
})
