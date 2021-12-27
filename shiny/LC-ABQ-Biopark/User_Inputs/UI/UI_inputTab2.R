list( 
  
  h4("Occurrence Remarks"),
  #change to non radio button
  checkboxInput( "occurrenceRemarks_introduced","Remove potentially introduced records with the remarks in fields listed below?", 1),
  
  checkboxGroupInput("occurrenceRemarks_cull", "Occurrence Remarks:",
                     c("Cult" = "cult",
                       "Garden" = "garden")),
  h4("AOO, EOO toggle"),
  
  
  splitLayout(
    numericInput("is.restricted.aoo.cutoff", "Restricted AOO Cutoff", value =
                   500),
    numericInput("is.restricted.eoo.cutoff", "Restricted EOO Cutoff", value =
                   40000)
  ),
  textAreaInput("eoo_just", "EOO Justification",
                value = "EOO calculated based on GBIF.org (2020) occurrence records trimmed to include only the species' native range as defined by Plants of the World Online, NatureServe, and VASCAN. The lower value is derived from the same data with the 10% of points furthest from the species' centroid removed.",
                400, 150),
  
  textAreaInput("aoo_just", "AOO Justification",
                value = "AOO AOO was calculated based on publicly available point data from GBIF. The reported upper value is calculated based on the number of 2 km x 2 km grid cells occupied by points occurring within WGSRPD regions where the species is confirmed to be native. The lower value is derived from the same data with the 10% of points furthest from the species' centroid removed. Both figures should be considered underestimates given incomplete sampling of the species.",
                400, 220),
  
  textAreaInput(
    "is.restricted.justification",
    "Restricted Justification",
    value = "Records of the species are abundant and there is currently no indication that the species would qualify for listing under a threatened category based on its distribution.",
    400,
    100
  )
  #is restricted just
)