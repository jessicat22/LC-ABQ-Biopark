list(
checkboxInput("del_year", "Remove records with no associated date?", 1),
        checkboxInput("sens", "Flag occurrence records as sensitive?", 0),
        checkboxInput("inat", "Include iNaturalist records?", 1),


    checkboxInput("unerctanty.tolerance", "Uncertanty Tolerance",1),
    h5("Default Codes"),
    splitLayout(
        numericInput("presence_code", "Presence Code", 1, 0, 6, 1),
        numericInput("origin_code", "Origin Code", 1, 0, 6, 1),
        numericInput("seasonal_code","Seasonal Code",1,0,5,1)
    ),
        numericInput("outlier_threshold","Outlier Threshold",.9,0,1,.01),

    br(),
    tags$hr(),
   br(),

    radioButtons("pop.data.qual", "Population data qual:",
                 c("Good" = "good", "Medium" = "medium",
                   "Poor" = "poor", "Unkown"= "unkown", "NA"= "na"),
                 (selected = "unkown"),(inline = TRUE)),

    splitLayout(
        radioButtons("precision_method", "Precision Method:",
                     c("Single" = "single", "Double" = "Double"), (selected = "single"),(inline = TRUE)),

        checkboxInput("throttle.points","Throttle Points",1)
    ),
   br(),
   tags$hr(),
   br(),

    sliderInput("min_decimals",label = h5("Minimum Decimals:"),
                min = 0, max = 10, value = 4),

    sliderInput("uncertainty_tolerance", label = h5("Uncertainty Tolerance"),
                min = 1, max = 10000, value = 5000),

    sliderInput("throttle_level", label = h5("Throttle Level"),
                min = 250, max = 750, value = 500)
)