library(shiny)
library(leaflet)
library(dygraphs)

function(req) {
  htmlTemplate("www/index.html",
               spinnerIcon = icon(name="spinner",class="fa-spin fa-fw"))
}
