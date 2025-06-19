# This file contains the user interface for Multipass CCS Refiner
library(shinyjs)
#####Defining User Interface (UI) #####
# The UI is divided into 7 fold-able code sections. First the overall interface and the others for each tab in the GUI.
# Begin by selecting a theme, setting some sizes and creating color names for aesthetics.
# HTML code is used to increase flexibility for styling options. 
ui <- fluidPage(theme = shinythemes::shinytheme("yeti"), #----
                tags$head(
                  tags$style(HTML("
                    html, body {
                      width: 100%;
                      margin: 0;
                      padding: 0;
                      box-sizing: border-box;
                    }
                    .background-darkblue,
                    .background-green,
                    .background-tan,
                    .citation {
                      width: 100%;
                      box-sizing: border-box;
                    }
                  "))
                ),
                # Create the app title
                navbarPage(
                  "Multipass CCS Refiner",
                  
                  # Introduction Page UI ----
                  tabPanel("Introduction",
                           
                           # Create three drop down boxes with specified colors. 
                           tags$style(HTML("
             .background-darkblue {
               background-color: #9da2ae; /* Dark blue background */
               padding: 15px;
               border-radius: 5px;
               margin-bottom: 15px;
               width: 100%;
               box-sizing: border-box;
               border: 2px solid black; /* Black border with 2px thickness */
             }
             .background-green {
               background-color: #d3d9d4; /* green background */
               padding: 15px;
               border-radius: 5px;
               margin-bottom: 15px;
               width: 100%;
               box-sizing: border-box;
               border: 2px solid black; /* Black border with 2px thickness */
             }
             .background-tan {
               background-color: #fefaf7; /* tan background */
               padding: 15px;
               border-radius: 5px;
               margin-bottom: 15px;
               width: 100%;
               box-sizing: border-box;
               border: 2px solid black; /* Black border with 2px thickness */
             }
             .citation {
               background-color: #fefaf7; /* tan background */
               padding: 10px;
               border-left: 5px solid #000000; /* Black left border */
               margin-bottom: 10px;
               width: 100%;
               box-sizing: border-box;
             }
             /* Style for toggle buttons */
             .toggle-btn {
               font-size: 18px;  /* size */
               padding: 5px 10px;
               margin-left: 10px;
               margin-top: -5px;
             }
             /* Flexbox container for header and button */
             .header-container {
               display: flex;
               justify-content: space-between;
               align-items: center;
             }
             /* Style for the icons to make them larger */
             .toggle-btn i {
               font-size: 16px;
             }
           ")),
                           
                           # Enable shinyjs
                           shinyjs::useShinyjs(),
                           
                           div(class = "background-darkblue", 
                               # Flex container with the header and toggle button
                               div(class = "header-container", 
                                   # header font size is changed by h. h1() is largest followed by h2() and h3() etc.
                                   h1("Background"), 
                                   # Action button next to the header
                                   actionButton("toggle_background", label = NULL, icon = icon("caret-down"), class = "btn btn-info toggle-btn", style = "background-color: black; color: white; border: none;")
                               ),
                               hidden(
                                 div(id = "background_content",
                                     p("Ion mobility spectrometry (IMS) is a powerful analytical technique for separating gas-phase ions based on the frequency and intensity of their collisions with an inert buffer gas. Ion mobility measurements are commonly reported as collision cross section (CCS) values, which, for metabolites, can assist in compound annotation."),
                                     
                                     p("Cyclic ion mobility (cIM) is a form of IMS that relies on traveling wave (TW) ion optics and enhances separation by circulating ions through a 1 m closed-loop buffer gas region. In cIM, ion flow through the drift region is controlled by altering the direction of a TW at an ion optic juncture. Changing the direction of the TW during separation introduces perturbations in the ions’ velocity, affecting the measured arrival time and the calculated CCS values. This phenomenon has prompted calibration strategies to correct errors observed in multi-pass periodic drift times."),
                                     
                                     p("Lin and Costello demonstrated that a linear relationship based on ion path length can be used to calculate a perturbation-corrected multi-pass drift time from measurable variables ",
                                       a("Lin and Costello (Anal. Chem. 2024)", href = "https://doi.org/10.1021/acs.analchem.4c01758", target = "_blank"),
                                       ". This approach is advantageous because it eliminates the need for carefully selected arrival times and accounts for the underlying principles behind the ions’ velocity perturbations."),
                                     
                                     p("This approach requires measurements at multiple separation time points which, without automation, can increase post-processing time. Additionally, changes in TW conditions, mass range, ionization mode or system and ambient conditions can introduce variability in multipass arrival times, emphasizing the need for rapid automated multipass CCS re-calibration."),
                                     
                                     p("The Multipass CCS Refiner app provides a streamlined solution for calibrating multi-pass CCS measurements. The application allows users to upload, calibrate, and visualize results with minimal manual preprocessing. Its intuitive interface, built-in formatting tools, and automated calculations are designed to reduce the time and programming expertise required to implement multipass CCS refinement."),
                                     
                                     p("The Multipass CCS Refiner was developed using R version 4.5.0 (R Core Team, 2025), RStudio version 2025.05.1+513 (Posit, 2025), and Shiny version 1.10.0 (Posit, 2024). The full source code is available on ", 
                                       a("GitHub", href = "https://github.com/facundof2016/Multipass-CCS-Refiner", target = "_blank"), 
                                       " and can be run locally by cloning the repository, installing the required packages, and launching the application by opening the App.R script and pressing the 'Run App' button within the RStudio interface. A complete list of package dependencies is provided in the introduction section of the code. A video tutorial demonstrating installation and use of the Multipass CCS Refiner is available on ",
                                       a("YouTube", href = "https://www.youtube.com/watch?v=AXrbocbQGY8&t=0s", target = "_blank"), ".")
                                 )
                               )
                               
                           ),
                           
                           # Using the App section.
                           div(class = "background-green", 
                               # Flex container with the header and toggle button
                               div(class = "header-container", 
                                   # Large header for "Using the App"
                                   h1("Using the Application"), 
                                   # Smaller action button next to the header
                                   actionButton("toggle_using_app", label = NULL, icon = icon("caret-down"), class = "btn btn-info toggle-btn", style = "background-color: black; color: white; border: none;")
                               ),
                               hidden(
                                 #Add the text.
                                 div(id = "using_app_content",
                                     p(""),
                                     p("The Multipass CCS Refiner allows users to create multi-pass calibration curves,", 
                                       "calculate experimental collisional cross section (CCS) values and download their results.",
                                       "To assist with automation the largest peak in an arrival time distribution (ATD) is located and fitted.",
                                       "While splitting ATDs into separate peaks can be done manually, the Multipass CCS Refiner offers an in app alternative.",
                                       "Example documents to assist with proper formatting can be downloaded below:"),
                                     p(""),
                                     br(),
                                     #Add the first example file that users can download. The download handler is in the server.
                                     fluidRow(
                                       column(9, p(HTML("<strong>Creating a Calibration Curve</strong>"))),
                                       column(2,uiOutput("exampleCalibrationUI"))
                                     ),
                                     # Add text explaining how users can upload their files.
                                     p("Calibration curves are constructed in the Create Calibration Curve tab using a standard", 
                                       "with known CCS values. ATDs collected on cIM", 
                                       "spectrometers can be copied and pasted directly from Mass Lynx to .xlsx", 
                                       "files with the separation time placed in a merged cell above the data columns. Each analyte", 
                                       "should be placed in its own separate tab labeled with the word analyte followed by a unique", 
                                       "compound name. Sheets containing data should start with the by-pass followed by the single-pass", 
                                       "ATD with 0 or 0.01 and 2 or 2.0 headers respectively. All following ATDs can be conducted at", 
                                       "any separation time and placed in any order within the sheet."), 
                                     
                                     p(
                                       "The source, ionization mode, standard, mass range, and TW conditions are optional fields for saving curve information. ",
                                       "Analyte names and theoretical CCS values are required to run a calibration; they should be comma-separated and listed in the order in which they appear in the .xlsx file. ",
                                       "A calibration file can be uploaded by using the browse function to navigate to the file's location. ",
                                       "A series of error checkers within the program identifies improperly formatted data before analysis begins. ",
                                       "After successfully running a calibration, power curves for both the single-pass and multi-pass results will be displayed at the bottom of the page. ",
                                       "A file containing arrival time calculations and linear plots for each standard in the mixture can be downloaded locally using the 'Download All Plots' button. ",
                                       "A successful calibration can then be saved to any calibration curve slot using the 'Save' button."
                                     ),
                                     br(),
                                     # Add the second button to download the experimental example.
                                     fluidRow(
                                       column(9, p(HTML("<strong>Processing Experimental Data</strong>"))),
                                       column(2,uiOutput("exampleExperimentUI")),
                                     ),
                                     
                                     p(
                                       "Running experimental data requires that a calibration has been successfully run and saved. ",
                                       "Users can select whether or not they wish to compare experimental data to known CCS values. ",
                                       "Analyte names and CCS values should be comma-separated and placed in the same order as the tabs in the uploaded .xlsx file, following the same format as the calibration curve. ",
                                       "After a successful run, a brief summary of results will be displayed in a data table at the bottom of the page, ",
                                       "and a detailed summary of results can be downloaded for all or individual analytes."
                                     ),
                                     br(),
                                     # Add the third button to download the multiple peaks example.
                                     fluidRow(
                                       column(9, p(HTML("<strong>Separating ATDs</strong>"))),
                                       column(2, uiOutput("exampleMultiPeakATDUI"))
                                     ),
                                     p(
                                       "One alternative to manually separating ATDs or performing additional slicing experiments is provided in the Multiple Peak ATD tab. ",
                                       "ATDs can be uploaded directly from MassLynx as .xlsx files, with arrival time data in the first column and intensity data in the second column. ",
                                       "The raw data can be plotted to assist with providing initial guess values to locate all peaks within an ATD. ",
                                       "Data within a specified standard deviation of the fitted arrival time is retained, and data points outside this range are generated from the fitted Gaussian model. ",
                                       "Distributions from all fitted peaks can be copied as reconstructed ATDs."
                                     )
                                     
                                 )
                               )
                           ),
                           
                           # Citations Section
                           div(class = "background-tan", 
                               
                               div(class = "header-container", 
                                   # Large header for "Citations"
                                   h1("Citations"), 
                                   # Smaller action button next to the header
                                   actionButton("toggle_citations", label = NULL, icon = icon("caret-down"), class = "btn btn-info toggle-btn", style = "background-color: black; color: white; border: none;")
                               ),
                               hidden(
                                 div(id = "citations_content",
                                     tags$h4("Literature Citations"),
                                     tags$ul(
                                       tags$li("Xia, C., Mernie, E., Zaia, J., Costello, C.E., & Lin, C. (2024). Accurate Collisional Cross Section Measurement by Multipass Cyclic Ion Mobility Spectrometry. Analytical Chemistry, 96(29), 11959–11968. ", 
                                               a("https://doi.org/10.1021/acs.analchem.4c01758", href="https://doi.org/10.1021/acs.analchem.4c01758", target="_blank")),
                                       tags$li("Righetti, L., Dreolin, N., Celma, A., McCullagh, M., Barknowitz, G., Sancho, J.V., & Dall’Asta, C. (2020). Travelling Wave Ion Mobility-Derived Collision Cross Section for Mycotoxins: Investigating Interlaboratory and Interplatform Reproducibility. Journal of Agricultural and Food Chemistry, 68(39), 10937–10943. ",
                                               a("https://doi.org/10.1021/acs.jafc.0c04498", href="https://doi.org/10.1021/acs.jafc.0c04498", target="_blank"))
                                     ),
                                     
                                     tags$h4("Software Citations"),
                                     tags$ul(
                                       tags$li("R Core Team (2024). R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing. ", 
                                               a("https://www.R-project.org/", href="https://www.R-project.org/", target="_blank")),
                                       tags$li("Posit team (2024). RStudio: Integrated Development Environment for R. Posit Software, PBC. ", 
                                               a("https://posit.co", href="https://posit.co", target="_blank")),
                                       tags$li("Chang W., Cheng J., Allaire J., et al. (2024). shiny: Web Application Framework for R. R package version 1.10.0. ", 
                                               a("https://CRAN.R-project.org/package=shiny", href="https://CRAN.R-project.org/package=shiny", target="_blank")),
                                       tags$li("Sievert C., Cheng J., Aden-Buie G. (2024). bslib: Custom 'Bootstrap' Themes for shiny and rmarkdown. R package version 0.8.0. ", 
                                               a("https://CRAN.R-project.org/package=bslib", href="https://CRAN.R-project.org/package=bslib", target="_blank")),
                                       tags$li("Chang W. (2021). shinythemes: Themes for Shiny. R package version 1.2.0. ", 
                                               a("https://CRAN.R-project.org/package=shinythemes", href="https://CRAN.R-project.org/package=shinythemes", target="_blank")),
                                       tags$li("Wickham H. (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag."),
                                       tags$li("Wickham H., Bryan J. (2023). readxl: Read Excel Files. R package version 1.4.3. ", 
                                               a("https://CRAN.R-project.org/package=readxl", href="https://CRAN.R-project.org/package=readxl", target="_blank")),
                                       tags$li("Dragulescu A., Arendt C. (2020). xlsx: Read, Write, Format Excel Files. R package version 0.6.5. ", 
                                               a("https://CRAN.R-project.org/package=xlsx", href="https://CRAN.R-project.org/package=xlsx", target="_blank")),
                                       tags$li("Attali D. (2021). shinyjs: Easily Improve the User Experience of Shiny Apps. R package version 2.1.0. ", 
                                               a("https://CRAN.R-project.org/package=shinyjs", href="https://CRAN.R-project.org/package=shinyjs", target="_blank")),
                                       tags$li("Bailey S. (2015). shinyBS: Twitter Bootstrap Components for Shiny. R package version 0.61. ", 
                                               a("https://CRAN.R-project.org/package=shinyBS", href="https://CRAN.R-project.org/package=shinyBS", target="_blank")),
                                       tags$li("Xie Y., Cheng J., Tan X. (2023). DT: A Wrapper of DataTables. R package version 0.31. ", 
                                               a("https://CRAN.R-project.org/package=DT", href="https://CRAN.R-project.org/package=DT", target="_blank")),
                                       tags$li("Garnier S. (2021). viridisLite: Colorblind-Friendly Color Maps. R package version 0.4.0. ", 
                                               a("https://CRAN.R-project.org/package=viridisLite", href="https://CRAN.R-project.org/package=viridisLite", target="_blank"))
                                     )
                                 )
                               )
                           ),
                  ),  # End of the Introduction tab.
                  
                  
                  # Create Calibration Curves Tab UI----
                  tabPanel("Create Calibration Curves", 
                           
                           h1("Calibration"),
                           textOutput("calibrationStatus"), 
                           
                           # First row: source and standard Used
                           fluidRow(
                             column(6, selectInput("source", "Source: ", c("ESI", "nano-ESI","DESI", "APCI", "Other"))),
                             column(6, textInput("standard", "Standard Used: "))
                           ),
                           
                           # Second row: Mode and Mass Range
                           fluidRow(
                             column(6, selectInput("mode", "Ionization Mode: ", c("Positive", "Negative"))),
                             column(6, div(
                               style = "display: flex; align-items: center;",
                               textInput("massRange", "Mass Range: "), 
                               tags$span(" m/z")
                             ))
                           ),
                           fluidRow(
                             column(6, textInput("twConditions", "Traveling Wave Conditions: ")),
                           ),
                           # Third row: Analytes and CCS values
                           fluidRow(
                             column(6, textAreaInput("calibrationAnalyteNames", "Analyte Names (comma separated): ")),
                             column(6, textAreaInput("calibrationCCS", "Theoretical CCS Values (comma separated): "))
                           ),
                           # Fourth row: File upload and save to slot
                           fluidRow(
                             column(6, fileInput("calibrationFile", "Upload File Path/Name: ", accept = ".xlsx")),
                             column(6, selectInput("saveToSlot", "Save to Slot: ", c("Curve 1","Curve 2","Curve 3"))),
                           ),
                           # Fifth row: Run, Save and Download buttons. The download handlers and action processing are in the server.
                           fluidRow(
                             column(1, actionButton("runCalibration","Run")), 
                             column(1, actionButton("Save","Save")), 
                             column(7, uiOutput("disabledDownloadCalibrationCurves", style = "width: 96.25%; text-align: right;")),
                             column(2, uiOutput("disabledDownloadAllCalibrationPlots", style = "width: 100%; text-align: left;")),
                           ),
                           br(),
                           #Tab panel for displaying the calibration curves on the GUI.
                           tabsetPanel(
                             tabPanel("Single-Pass Curve", plotOutput("singlePassCalibrationCurve")),
                             tabPanel("Multi-Pass Curve", plotOutput("MultiPassCalibrationCurve"))
                           )
                           
                  ),# End of tab 2 Create Calibration Curves.
                  # Test New Analytes Tab UI----
                  tabPanel("Process Data", 
                           
                           h1("Calculate and Compare CCS Values"),
                           textOutput("analytesStatus"),
                           # Rows 1-3 ask for general information similar to the create calibration curve tab.
                           fluidRow(
                             column(6, selectInput("curveSlot", "Calibration Curve: ", c("Curve 1","Curve 2"))),
                             column(6, selectInput("knownCCSValues", "Compare to Known CCS Values: ", c("Yes","No"))),
                           ),
                           fluidRow(
                             column(6, textAreaInput("processDataAnalyteNames","Analyte Names (comma separated): ")),
                             column(6, uiOutput("disabledDataCCSValues")), # Enabled if the user has CCS values to compare.
                           ),
                           fileInput("usersDataFile", "Upload File: ", accept = ".xlsx"),
                           fluidRow(
                             # Row Four has the run action button and download button that is disabled until a run is completed.
                             column(1, actionButton("runData", "Run")),
                             column(2, uiOutput("disabledAllDataPlots", style = "text-align: right; margin-left: 10px;")),
                           ),
                           br(),
                           # The actions below are hidden until a run is completed. 
                           fluidRow(     
                             column(4, uiOutput("analyteDropdownMenu")),
                           ),
                           
                           fluidRow(
                             column(3, uiOutput("disabledDownloadIndividualPlots")),
                           ),
                           br(),
                           DT::DTOutput("CCSResultsDT"),
                           
                           textOutput("status")
                           
                  ), # End of tab 3 Process Data.
                  # Multiple Peak ATD Tab UI----
                  tabPanel("Multiple Peak ATD", 
                           
                           tabPanel("Multiple Peak ATD", 
                                    
                                    h1("Separate and Reconstruct ATDs"),
                                    textOutput("multiATDStatus"),
                                    
                                    # Ask use for file upload and the location of their data.
                                    fluidRow(
                                      column(3, fileInput("usersATDFile", "Upload File: ", accept = ".xlsx")),
                                      column(3, uiOutput("sheetNameUI")),
                                      column(3, numericInput("startRow","Data Start Row: ", 2, min=1, max=50)),
                                      column(3, numericInput("startColumn","Data Start Column: ", 1, min=1, max=50))
                                    ),
                                    # Button to generate plots.
                                    fluidRow(
                                      column(2, actionButton("plotRawData", "Plot Raw Data"))
                                    ),
                                    # Plot the raw data
                                    fluidRow(
                                      column(11, plotOutput("rawDataPlot"))
                                    ),
                                    # Add as many peaks as needed up to 10 peaks in one ATD.
                                    fluidRow(
                                      column(3, actionButton("generatePeaks", "Add More Peaks")),
                                      column(3, actionButton("deleteLastpeak", "Delete Last Peak"))
                                    ),
                                    # Generate button to view fitted Gausians overtop original data.
                                    uiOutput("generateFittedPlots"),
                                    fluidRow(
                                      column(3, actionButton("generate", "Generate"))
                                    ),
                                    # Plot output for ATD triggered when generate is pressed. 
                                    fluidRow(
                                      column(12, plotOutput("userATD"))
                                    ),
                                    # Dropdown menu to copy one peak distribution at a time followed by a copy button.
                                    fluidRow(
                                      column(3, selectInput("peakSelection", "Select Peak to Copy", choices = NULL)),
                                      column(6, numericInput("sigmaRange", "Keep Data within ±σ of μ", 3, min = 1, max = 20))
                                    ),
                                    fluidRow(
                                      column(3, actionButton("copytoClipboard", "Copy Distribution"))
                                    ),
                                    # Peaks are copied with arrival time and intensity headers.
                                    br(),
                                    textOutput("status")
                           )
                           
                  ), # End of tab 4 Multiple Peak ATD
                  # Calibration Curve 1 Tab UI----
                  # Calibration Curve 1 and 2 are essentially identical and exist so users can store 2 curves at once.
                  # For example one in positive mode and one in negative mode or perhaps using two different mass ranges.
                  tabPanel("Calibration Curve 1",
                           mainPanel(
                             h1("Calibration Curve 1"),
                             verbatimTextOutput("source1"),
                             verbatimTextOutput("mode1"),
                             verbatimTextOutput("standard1"),
                             verbatimTextOutput("massRange1"),
                             verbatimTextOutput("twConditions1"),
                             verbatimTextOutput("singlePassEquationCurve1"),
                             verbatimTextOutput("multiPassEquationCurve1"),
                             actionButton("delete1","Delete Curve"),
                             tabsetPanel(
                               tabPanel("Single Pass Curve", plotOutput("singlePassCalibrationCurve1")),
                               tabPanel("Multipass Curve", plotOutput("multiPassCalibrationCurve1"))
                             )
                           )
                  ), # End of tab 5 Calibration Curve 1.
                  # Calibration Curve 2 Tab UI----
                  tabPanel("Calibration Curve 2",
                           mainPanel(
                             h1("Calibration Curve 2"),
                             verbatimTextOutput("source2"),
                             verbatimTextOutput("mode2"),
                             verbatimTextOutput("standard2"),
                             verbatimTextOutput("massRange2"),
                             verbatimTextOutput("twConditions2"),
                             verbatimTextOutput("singlePassEquationCurve2"),
                             verbatimTextOutput("multiPassEquationCurve2"),
                             actionButton("delete2","Delete Curve"),
                             tabsetPanel(
                               tabPanel("Single Pass Curve", plotOutput("singlePassCalibrationCurve2")),
                               tabPanel("Multipass Curve", plotOutput("MultiPassCalibrationCurve2"))
                             )
                           )
                  ), # End of tab 6 Calibration Curve 2.
                  # Calibration Curve 2 Tab UI----
                  tabPanel("Calibration Curve 3",
                           mainPanel(
                             h1("Calibration Curve 3"),
                             verbatimTextOutput("source3"),
                             verbatimTextOutput("mode3"),
                             verbatimTextOutput("standard3"),
                             verbatimTextOutput("massRange3"),
                             verbatimTextOutput("twConditions3"),
                             verbatimTextOutput("singlePassEquationCurve3"),
                             verbatimTextOutput("multiPassEquationCurve3"),
                             actionButton("delete3","Delete Curve"),
                             tabsetPanel(
                               tabPanel("Single Pass Curve", plotOutput("singlePassCalibrationCurve3")),
                               tabPanel("Multipass Curve", plotOutput("MultiPassCalibrationCurve3"))
                             )
                           )
                  ), # End of tab 7 Calibration Curve 3.
                  # Settings Tab UI----
                  # Setting for decimal places have min and max values to avoid errors. Changeable manually or with arrow buttons.
                  tabPanel("Settings",
                           
                           h1("Settings"),
                           fluidPage(
                             column(6, numericInput("powerCurveDecimals", "Power Curve Decimals: ", 4, min=3, max=8)),
                             column(6, numericInput("linearPlotDecimals", "Linear Plot Decimals: ", 4, min=2, max=5)),
                           ),
                           fluidPage(
                             column(6, numericInput("CCSDecimals", "CCS Value Decimals: ", 2, min=0, max=5)),
                             column(6, numericInput("errorDecimals", "CCS Error Decimals: ", 2, min=1, max=5)),
                           ),
                           fluidPage(
                             column(6, numericInput("ATDDecimals", "ATD Decimals: ", 4, min=1, max=7))
                           )
                  ), # End of tab 8 Settings.
                ) # Close the navbarPage
) # Close the fluidPage

##### End of Defining UI #####