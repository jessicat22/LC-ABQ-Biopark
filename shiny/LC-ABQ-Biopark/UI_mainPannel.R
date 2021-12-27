list(
h4(
  "ABQ BioPark’s LC Pipeline is intended to streamline the processing
               of data for widely distributed species in preparation for submission
               of Least Concern assessments to the IUCN Red List. The script is
               intended to scrape a variety of sources for data, parse point data,
               and return spreadsheets formatted for upload using SIS Connect.
               The script is not intended to be a substitute for human assessors
               and all assessments generated using the tool will require review
               according to the regular processes outlined in the IUCN Red List Rules of Procedure."
),

#taxonomy
h4("Contents of Taxonomy File:"),
tableOutput("contentsOfTaxonomy"),

#darwincore
h4("Contents of Optional DarwinCore File:"),
tableOutput("DarwinCore"),

h4("Contents of allfields"),
tableOutput("all_fields"))