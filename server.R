library(shiny)
library(dygraphs)
library(dplyr)
library(readr)
library(lubridate)
library(xts)
library(wcUtils)
library(rgdal)
library(sp)
library(leaflet)
library(viridis)
library(shinyjs)
library(gtools)

shinyServer(function(input, output, session) {
  useShinyjs(html = TRUE)
  shinyjs::hide("map",anim=FALSE)
  shinyjs::hide("map-data-header", anim = FALSE)
  shinyjs::hide("dive-data",anim = FALSE)
  shinyjs::hide("dive-data-header",anim = FALSE)
  shinyjs::hide("spinner",anim = FALSE)
  shinyjs::hide("time-mismatch-warning",anim = FALSE)
  
  input_dat <- reactiveValues(
    dat_files = NULL,
    zipfile = NULL,
    ptt = NULL,
    select_deployid = FALSE,
    id = NULL
  )
  
  observeEvent(input$zipfile, {
    shinyjs::hide("map",anim = FALSE)
    shinyjs::hide("map-data-header",anim = FALSE)
    shinyjs::hide("dive-data",anim = FALSE)
    shinyjs::hide("dive-data-header",anim = FALSE)
    shinyjs::toggle("spinner",anim = FALSE)
    input_dat$zipfile <- input$zipfile
    input_dat$ptt <- strsplit(input$zipfile$name,'\\.')[[1]][1]
    input_dat$dat_files <- unzip(input$zipfile$datapath,
                          exdir = tempdir())
    
    updateNumericInput(session,inputId="ptt_integer",value=input_dat$ptt)
    shinyjs::show("map-data-header",anim = FALSE)
    shinyjs::show("map",anim = FALSE)
    shinyjs::show("dive-data-header",anim = FALSE)
    shinyjs::show("dive-data",anim = FALSE)
    shinyjs::toggle("spinner",anim = FALSE)
  })

  observeEvent(input$getWCdata,{
    shinyjs::toggle("spinner",anim = FALSE)
    shinyjs::hide("map",anim = FALSE)
    shinyjs::hide("map-data-header",anim = FALSE)
    shinyjs::hide("dive-data",anim = FALSE)
    shinyjs::hide("dive-data-header",anim = FALSE)
    with(input_dat,{
      dat_files = NULL
      zipfile = NULL
      ptt = NULL
      select_deployid = FALSE
      id = NULL
    })
    
    input_dat$ptt <- as.character(input$ptt_integer)
    res <- wcUtils::wcPOST(keyfile = input$keyfile$datapath)
    ptt_dat <- wcUtils::wcGetPttID(res,ptt=as.character(input_dat$ptt))
    if(length(ptt_dat$ids) == 1) {
      input_dat$id <- ptt_dat$id
    } else if(nrow(subset(ptt_dat$df,!is.na(deployid) & deployid != "")) == 1) {
      input_dat$id <- ptt_dat$df$id[which(!is.na(ptt_dat$df$deployid) & ptt_dat$df$deployid != "")]
    } else {
      input_dat$select_deployid <- TRUE
      input_dat$id <- NULL
    }
    if(!is.null(input_dat$id)) {
      input_dat$zipfile <- wcUtils::wcGetZip(id=input_dat$id,
                                             keyfile=input$keyfile$datapath)
      input_dat$dat_files <- unzip(input_dat$zipfile,
                                   exdir = tempdir())
    }
    shinyjs::show("map-data-header",anim=FALSE)
    shinyjs::show("map",anim=FALSE)
    shinyjs::show("dive-data-header",anim = FALSE)
    shinyjs::show("dive-data",anim = FALSE)
    shinyjs::toggle("spinner",anim = FALSE)
  })
  
  histos <- reactive({
    req(input_dat$dat_files)
    histo_file <- input_dat$dat_files[grep("*-Histos.csv", 
                                            input_dat$dat_files)]
    res <- wcUtils::read_histos(histo_file)
    return(res)
  })
  
  msgs_hourly <- reactive({
    req(input_dat$dat_files)
    msg_file <- input_dat$dat_files[grep("*-All.csv",
                                          input_dat$dat_files)]
    msg_dt <- wcUtils::read_allmsg(msg_file) %>% 
      dplyr::select(msg_date) %>% 
      dplyr::mutate(msg_hour = lubridate::floor_date(msg_date,"hour")) %>%
      dplyr::group_by(msg_hour) %>% 
      dplyr::summarise(msg_count=n())
    
    msg_seq <- data.frame(msg_hour = 
                            seq(min(msg_dt$msg_hour),
                                max(msg_dt$msg_hour + lubridate::hours(1)),
                                by='1 hour'))
    
    msg_dt <- msg_seq %>% dplyr::left_join(msg_dt, by='msg_hour') %>% 
      dplyr::mutate(msg_count = ifelse(is.na(msg_count),0,msg_count)) %>% 
      dplyr::arrange(msg_hour)
    return(msg_dt)
  })
  
  all_locs <- reactive({
    req(input_dat$dat_files)
    locs_fastgps <- 
      input_dat$dat_files[grep(paste0("*",input_dat$ptt,"-[0-9]+-Locations.csv"), 
                                   input_dat$dat_files)]
    locs_data <- 
      input_dat$dat_files[grep(paste0("*",input_dat$ptt,"-Locations.csv"), 
                                 input_dat$dat_files)]
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
    req(input_dat$dat_files)
    behav_file <- try(input_dat$dat_files[grep("*-Behavior.csv", input_dat$dat_files)])
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
    numericInput('ptt_integer', 'PTT ID Integer',
                 max=999999,min=1111, value=NA)
  })
  
  output$getDataButton <- renderUI({
    actionButton('getWCdata',label="Get Data from Wildlife Computers")
  })
  
  output$msgs_per_hour <- renderDygraph({
    m <- xts(msgs_hourly(),msgs_hourly()$msg_hour)
    m <- m[,"msg_count"]
    m$msg_count <- as.integer(m$msg_count)
    
    dygraph(m, main = "Argos Messages Received", group = 'wc_plots') %>%
      dySeries(
        "msg_count",
        label = "Message Count",
        fillGraph = TRUE,
        stepPlot = TRUE,
        color = "#11AA99",
        drawPoints = FALSE,
        strokeWidth = 0
      ) %>%
      dyOptions(useDataTimezone = TRUE,
                fillAlpha = 0.75) %>%
      dyAxis("y", label = "Messages per Hour") %>%
      dyAxis("x", label = "Time") %>% 
      dyRangeSelector()
  })
  
  output$timelines <- renderDygraph({
    validate(need(histos(), "No Histos File Detected"))

      t <- try(histos() %>%
        wcUtils::tidyTimelines(.) %>%
        select(datadatetime, percent_dry) %>%
        arrange(datadatetime))
      if(class(t) == 'try-error') {
        return(NULL)
      }
      
      t_days <- unique(lubridate::floor_date(t$datadatetime,"day"))
      day_two <- t_days[which.min(t_days) + 1]
      day_two_bins <- filter(t, floor_date(datadatetime,"day") == day_two)
      
      time_diffs <- diff(day_two_bins$datadatetime)
      if(sum(diff(as.numeric(time_diffs))) == 0) {
        t_diff <- as.numeric(time_diffs[1])
        if(t_diff == 1) {
          t_diff <- 60
        } else if(t_diff == 20 ) {
          t_diff <- t_diff
        } else {
          stop("timeline bin other than 20 or 60 min bins detected")
        }
      } else {
        stop("timeline time diffs are unequal")
      }
      
      t <- data.frame(datadatetime =
                        seq(min(t$datadatetime),
                            max(t$datadatetime), 
                            by = paste(t_diff,'min'))) %>% 
        left_join(t, by = "datadatetime")
      
      t_dat_times <- filter(t,!is.na(percent_dry)) %>% 
        mutate(start_dt = datadatetime,
               end_dt = datadatetime + lubridate::minutes(t_diff)) %>% 
        select(start_dt,end_dt)
      
      t <- xts(t, t$datadatetime)
      t <- t[, "percent_dry"]
      
      p <- dygraph(t, main = "Percent Dry", group = 'wc_plots') %>%
        dySeries(
          "percent_dry",
          label = "Percent Dry",
          fillGraph = TRUE,
          stepPlot = TRUE,
          color = "#999933",
          drawPoints = FALSE,
          strokeWidth = 0
        ) %>%
        dyOptions(useDataTimezone = TRUE,
                  fillAlpha = 0.75) %>%
        dyAxis("y", label = paste("Percent Dry per",t_diff,"min."),
               valueRange = c(0, 101)) %>%
        dyAxis("x", label = "Time")%>% 
        dyRangeSelector()
      
      for (i in 1:nrow(t_dat_times)) {
        p <- p %>% dyShading(from = t_dat_times$start_dt[i], 
                             to = t_dat_times$end_dt[i])
      }
      return(p)
  })
  
  output$dives <- renderDygraph({
    validate(need(behav_file(),"No Behavior File Detected"))
    if (is.null(behav_file())) {
      return()
    }

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
          color = "#992288",
          drawPoints = FALSE,
          strokeWidth = 0
        ) %>%
        dyOptions(useDataTimezone = TRUE,
                  fillAlpha = 0.75) %>%
        dyAxis("y", label = "Dive Depth (m)") %>%
        dyAxis("x", label = "Time") %>% 
        dyRangeSelector()
      
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
  
  output$timelines_vis <- reactive({
    h <- histos()[["histos"]] %>%
      filter(histtype %in% c('Percent','1Percent','TwentyMinTimeline'))
    if (nrow(h) < 1) {
      return(0)
    } else {
      return(1)
    }
  })
  outputOptions(output, 'timelines_vis', suspendWhenHidden = FALSE)
  
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
      
      non_standard_hours <- 
        as.numeric(names(table(lubridate::hour(h$date)))[which(
          table(lubridate::hour(h$date)) < 5)])
      h <- h %>% filter(!lubridate::hour(date) %in% non_standard_hours)
      
      time_bins <- sort(unique(lubridate::hour(h$date)))
      
      time_diffs <- diff(time_bins)
      if(sum(diff(time_diffs)) == 0) {
        t_diff <- time_diffs[1]
      } else {
        stop("dive histo bin time diffs are unequal")
      }
      
      h <- data.frame(date =
                        seq(min(h$date),
                            max(h$date + lubridate::hours(t_diff)), 
                            by = paste(t_diff,'hours'))) %>% 
        left_join(h, by = "date")
      
      h_dat_times <- filter(h,!is.na(bin1)) %>% 
        mutate(start_dt = date,
               end_dt = date + lubridate::hours(t_diff)) %>% 
        select(start_dt,end_dt)
      
      h <- h %>% select_(.dots = c("date", bin_string)) %>% 
        tidyr::gather(bin,num_dives,num_range("bin",1:nbins)) %>% 
        mutate(bin=factor(bin,levels=gtools::mixedsort(unique(bin))))
      
      h <- h %>% mutate(num_dives = ifelse(is.na(num_dives),0,num_dives))
      
      cols <- colorRampPalette(rev(c("#781C81","#413B93","#4065B1","#488BC2","#55A1B1","#63AD99","#7FB972","#B5BD4C","#D9AD3C","#E68E34","#E6642C","#D92120")))
      
      h <- tidyr::spread(h, key=bin,value=num_dives)
      h <- xts(h, h$date)
      h <- h[, -which(names(h) %in% c("date", "datadatetime"))]
      
      p <- dygraph(h, main = "Dive Depth Histograms", 
                   group = 'wc_plots') %>%
        
        dyOptions(
          useDataTimezone = TRUE,
          strokeWidth = 2,
          stepPlot = TRUE,
          fillGraph = TRUE,
          fillAlpha = 0.05,
          colors = cols(nbins)
        ) %>%
        dyAxis("y", label = "Number of Dives") %>%
        dyAxis("x", label = "Time") %>% 
        dyRangeSelector()
      
      for (i in 1:nrow(h_dat_times)) {
        p <- p %>% dyShading(from = h_dat_times$start_dt[i], 
                             to = h_dat_times$end_dt[i])
      }

      return(p)
    }
  })
  
  locs <- reactive({
    if(!is.null(input$msgs_per_hour_date_window)) {
      locs <- all_locs() %>%
        mutate(loc_dt = lubridate::parse_date_time(Date,
                                                   "H!:M!:S! d!-m!-Y!"),
               Type = as.factor(Type)) %>%
        select(loc_dt, Latitude, Longitude, Type) %>% 
      filter(loc_dt >= input$msgs_per_hour_date_window[[1]] &
               loc_dt <= input$msgs_per_hour_date_window[[2]])
    } else {
      locs <- all_locs() %>%
        mutate(loc_dt = lubridate::parse_date_time(Date,
                                                   "H!:M!:S! d!-m!-Y!"),
               Type = as.factor(Type)) %>%
        select(loc_dt, Latitude, Longitude, Type)
    }

    coordinates(locs) <- ~ Longitude + Latitude
    proj4string(locs) <- CRS("+init=epsg:4326")
    #reproject to get proper wrapping
    locs <- sp::spTransform(locs,CRS("+proj=longlat +lon_wrap=180"))
    return(locs)
  })
  
  locs_start <- reactive({
    l_start <- subset(locs(),loc_dt == min(locs()$loc_dt))
    return(l_start)
  })
  
  locs_end <- reactive({
    l_end <- subset(locs(),loc_dt == max(locs()$loc_dt))
    return(l_end)
  })
  
  track <- reactive({
    req(locs())
    SpatialLines(list(Lines(list(Line(locs())), "id")))
  })
  
  output$locations <- renderLeaflet({
    esri_wrld_ocean <- "http://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}"
    esri_wrld_ocean_ref <- "http://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Reference/MapServer/tile/{z}/{y}/{x}"
    esri_wrld_ocean_attr <- "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"
    
    req(locs())
      pal <- colorFactor(c("#F19320","#762A83","#EE3333"), domain = c("Argos", "GPS","User"))

      leaflet(locs()) %>%
        addTiles(urlTemplate = esri_wrld_ocean,
                         attribution = esri_wrld_ocean_attr
                         ) %>%
        addTiles(urlTemplate = esri_wrld_ocean_ref,
                 attribution = esri_wrld_ocean_attr) %>% 
        addPolylines(data=track(), weight=2, color='black',
                     group="Trackline") %>%
        addCircleMarkers(
          radius = 3,
          stroke = FALSE,
          color = ~pal(Type),
          fillOpacity = 1
        ) %>% 
        addCircleMarkers(
          data=locs_start(),
          radius=5,
          stroke=TRUE,
          color='#117733',
          fill=FALSE,
          opacity=1,
          popup=~loc_dt) %>%
        addCircleMarkers(
          data=locs_end(),
          radius=5,
          stroke=TRUE,
          color='#882255',
          fill=FALSE,
          opacity=1,
          popup=~loc_dt) %>%
        addLegend(pal=pal,
                  values=~Type,
                  title = "Location Type",opacity=1)
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
