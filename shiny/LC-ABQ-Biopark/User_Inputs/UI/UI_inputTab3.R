list(
h5("Threats"),
    checkboxInput("nothreats", "No Threats", 1),
    checkboxInput("threats.unknown", "Threats Unknown", 0),
    #threats.narrative	rationale.text	threats.unknown	threats.text	nothreats
    textAreaInput("threats.narrative", "Threats Narrative",
                  value = "The species has a large range, and numerous records of the species are available. At present, no threats likely to drive the species into a threatened category are known.",
                  400, 100),
    textAreaInput("rationale.text", "Rationale Text",
                  value = "The species has a large range, and there is currently no indication of widespread population decline. It is therefore listed as Least Concern",
                  400, 100),
    textAreaInput("threats.text", "Threats Text",
                  value = "There are no known significant threats to the species at this time",
                  400, 50),
    textAreaInput("pop.narrative", "Population Narrative Text",
                  value = "The species has a large range, and numerous records of the species are available. At present, no threats likely to drive the species into a threatened category are known.	",
                  400, 100)
)