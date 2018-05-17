#SOS UI Script
require("shiny")

fluidPage(
  titlePanel("SOS: Occupancy Modeling by Camera Trap Data"),
  tabsetPanel(
    #Upload Tab ----
    tabPanel(
      title = "1) Upload Data",
      fluidRow(
        column(8,
               HTML("
<br><center><font color=\"darkred\">Uploaded files must be spreadsheets in csv format.</font></center>
<h4>Each row represents an observation, and the following columns must be present:</h4>
<ul>
  <li>\"<b>Independent</b>\", with a value of \"Yes\" indicating that the observation is independent of others (to prevent autocorrelation). We recommend considering observations independent only if the individuals in the photograph have not been in an independent photo at that location in the previous 30 minutes.</li>
  <li>\"<b>Species</b>\", which holds the <i><u>species name</u></i> of the animal in the observation (e.g: \"<i>Canis latrans</i>\", \"Sevengill Shark\", or \"<i>Ectopistes migratorius</i>\").</li>
  <li>\"<b>Survey.Name</b>\", which contains the <u>site</u> and the <u>season</u> in which the study took place (e.g: \"Site1 Summer 2018\", \"ABC Site, Fall 2010\", \"Spring 2008 My Favorite Valley\").</li>
</ul>
<br>
<h4>The following columns are optional, but can be used to model covariates if present:</h4>
<ul>
  <li>\"<b>Date</b>\", which gives the date of the observation in M/D/YYYY format (e.g: June 13, 2012 would be given as 6/13/2012).</li>
  <li>\"<b>Elevation</b>\", which gives the altitude of the camera trap location relative to sea level (or some other common reference level; values are normalized in internal calculations).
</ul>
             ")
        ),
        column(4,
               wellPanel(
                 fileInput(
                   inputId = "updata",
                   label = "Upload your own data to use with the webapp:",
                   accept = ".csv"
                 ),
                 div(
                   "Once the file has finished uploading, press the button below in order to import the data into the app:"
                 ),
                 fluidRow(column(
                   12,
                   align = "center",
                   actionButton(
                     inputId = "dataButton",
                     label = "Generate dataset",
                     width = "100%"
                   )
                 ))
               ),
               HTML("The source code for this app is available <u><a href=\"https://github.com/rbotts/Occupancy\">here</a></u> on Github."),
               div(style = "height:20px"),
               wellPanel(radioButtons(inputId = "mooringCheck", label = HTML("<b>Are you using our data?</b>"), choiceNames = c("No", "Yes"), choiceValues = c(FALSE, TRUE)))
        )
      )
      ),
    #Variable/Covariate Tab ----
    tabPanel(
      title = "2) Variable and Covariate Selection",
      fluidRow(
        column(4,
          uiOutput("speciesSelect") #A checkbox group that allows selecting which species to be analyzed
        ),
        column(4,
          uiOutput("siteCovSelect"), #A checkbox group that allows selecting which covariates to be analyzed as a function of site
          uiOutput("obsCovSelect") #A checkbox group that allows selecting which covariates to be analyzed as a function of observation
        ),
        column(4,
               uiOutput("yearlyCovSelect"), #A checkbox group that allows selecting which covariates to be analyzed as a function of year
               fluidRow(column(
                 12,
                 HTML("Once you've selected all the variables and covariates you like, press the following button to run the Occupancy analysis. Note that this may take a few minutes to complete, especially if using a large dataset."),
                 actionButton(
                   inputId = "goButton",
                   label = "Go!",
                   width = "100%"
                 )
               ))
        )
      )
    )
  )
)
