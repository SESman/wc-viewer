library(shiny)
library(dygraphs)
library(leaflet)

shinyUI(fluidPage(
  tags$head(
    tags$style(".clearButton {font-size:12px;}")
  ),
  # Application title
  titlePanel("Wildlife Computers Data Viewer"),
  tabsetPanel(
    tabPanel("Input Data",
             fluidRow(p()),
             fluidRow(
               column(6,
                      wellPanel(
                        h3("Upload a ZIP File"),
                        uiOutput('zipFileInput'),
                        HTML("<button id='clearPTT' class='action-button clearButton'>Clear File</button>"),p(),
                        p(
                          'Upload a *.zip file downloaded from the Wildlife Computers Data Portal. Only one deployment/file can be explored at a time.'
                        ),
                        hr(),
                        p('The folowing tag and data types are supported:'),
                        tags$ul(
                          tags$li('SPLASH'),
                          tags$li('SPOT'),
                          tags$li('SPLASH/FastGPS')
                        ),
                        tags$ul(
                          tags$li('Argos & FastGPS Locations'),
                          tags$li('Hourly Percent Timelines'),
                          tags$li('Dive Behavior Messages'),
                          tags$li('Dive Depth Histograms')
                        )
                      )),
               column(6,
                 wellPanel(      
                 h3("Get Data by PTT ID"),
                 textInput('ptt_text', 'Enter a PTT Identifier'),
                 fileInput('keyfile', 'Upload KeyFile'),
                 p(
                   "If you have enabled web services on your Wildlife Computers Data Portal account, you can provide a PTT identifier and a web key file. The application will pull data directly from the portal."
                 ),
                 hr(),
                 h5("Web Services Security"),
                 p(
                   "When logged into the data portal, go to Account Setting and then Web Services Security. There you can add a key pair. The pair consists of both an Access Key and a Secret Key."
                 ),
                 h5("Formated KeyFile"),
                 p(
                   "The keyfile must be a JSON formated file with two fields: wcAccesskey, wcSecretKey"
                 )
               ))
             )),
    tabPanel(
      "Behavior Plots",
      conditionalPanel(
        "output.dive_behav_vis!=0",
        dygraphOutput("dives", height = "300px")
      ),
      conditionalPanel(
        "output.dive_histos_vis!=0",
        dygraphOutput("dive_histos", height = "300px")
      ),
      dygraphOutput("timelines", height = "250px")
    ),
    tabPanel(
      "Location Map",
      p(),
      leafletOutput("locations"),
      p(),
      dygraphOutput("longlat", height = "275px")
    )
  )
))
