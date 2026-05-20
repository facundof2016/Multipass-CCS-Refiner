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
                        .background-orange {
                          background-color: #f9c784; /* Orange background */
                          padding: 15px;
                          border-radius: 5px;
                          margin-bottom: 15px;
                          width: 100%;
                          box-sizing: border-box;
                          border: 2px solid black;
                        }
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
                       /* Make icons larger */
                       .toggle-btn i {
                         font-size: 16px;
                       }
           ")),
                     # Additional Announcements Tab tags
                     tags$style(HTML("
                        .announcement-card {
                          background-color: #ffffff;
                          border-left: 6px solid #000000;
                          border-radius: 10px;
                          padding: 16px 20px;
                          margin-bottom: 18px;
                          box-shadow: 0 2px 8px rgba(0,0,0,0.12);
                        }
                      
                        .announcement-header {
                          font-weight: 700;
                          margin-top: 0;
                          margin-bottom: 10px;
                        }
                      
                        .announcement-summary {
                          background-color: #fff7e6;
                          border: 1px solid #f0b85a;
                          border-radius: 10px;
                          padding: 16px 20px;
                          margin-bottom: 18px;
                        }
                      
                        .announcement-link {
                          font-weight: bold;
                          text-decoration: underline;
                        }
                      
                        .announcement-tag {
                          display: inline-block;
                          background-color: #000000;
                          color: #ffffff;
                          border-radius: 999px;
                          padding: 3px 10px;
                          font-size: 0.85em;
                          margin-bottom: 8px;
                        }
                      ")),
                           
                           # Enable shinyjs
                           shinyjs::useShinyjs(),
                           # ---------------- Announcements Panel ----------------
                           div(class = "background-orange",
                               # Header + toggle button
                               div(class = "header-container",
                                   h1("Announcements"),
                                   actionButton(
                                     "toggle_announcements", 
                                     label = NULL, 
                                     icon = icon("caret-down"), 
                                     class = "btn btn-info toggle-btn",
                                     style = "background-color: black; color: white; border: none;"
                                   )
                               ),
                               
                               # Collapsible content
                               hidden(
                                 div(
                                   id = "announcements_content",
                                   
                                   div(
                                     class = "announcement-card",
                                     h4(class = "announcement-header", "New in the May 2026 Update"),
                                     p("This release adds calibration export/reupload, built-in reference tables, and enhanced calibration corrections for multipass CCS workflows.")
                                   ),
                                   
                                   div(
                                     class = "announcement-card",
                                     span(class = "announcement-tag", "New Feature"),
                                     h4(class = "announcement-header", "💾 Calibration Curve Export and Reupload"),
                                     p("Calibration curves can now be exported as ", code(".rds"), 
                                       " files and reuploaded in future sessions. This allows users to save completed calibration curves and reload them later without rerunning the full calibration workflow."),
                                     p("Calibration curves can be exported from the", strong("Saved Calibration Curves"), "tab and reimported into the", strong("Create Calibration Curves"), " tab for use within the local or online versions.")
                                   ),
                                   
                                   div(
                                     class = "announcement-card",
                                     span(class = "announcement-tag", "New Feature"),
                                     h4(class = "announcement-header", "📚 Calibrant Reference Tables"),
                                     p("Reference tables for Major Mix and SpheriCal Low Mass standards have been added to the application to streamline calibration workflows and reduce manual data entry."),
                                     p("These tables include CCS, m/z, and molecular weight values, allowing users to quickly select reference standards in the",
                                       strong("Create Calibration Curves"), "tab."),
                                     p("When using a built-in calibrant library, ionization mode and mass range are automatically populated when saving a calibration curve based on the calibrants selected."),
                                     
                                     p(strong("To add a custom hard-coded calibrant library:")),
                                     p("Code can be duplicated and edited from the existing calibrant tables using the following steps. This requires access to the application source code and is intended for local users."),
                                     
                                     tags$ol(
                                       tags$li(strong("Server:"), " Create a new calibrant data frame using the required internal columns: ",
                                               code("name"), ", ", code("CCS"), ", ", code("mz"), ", ", code("MW"), ", and ", code("z"),
                                               ", with optional columns such as ", code("formula"), " or ", code("adduct"),
                                               ". Column names are case-sensitive and must match exactly."),
                                       tags$li(strong("Server:"), " Add an ", code("attr()"), " call immediately after the data frame definition to specify ionization mode, for example: ",
                                               code('attr(my_df, "ion_mode") <- "Positive"'),
                                               ". This allows the application to auto-populate the ionization mode when the library is selected."),
                                       tags$li(strong("Server:"), " Add the new data frame to the ", code("calibrantTables"),
                                               " list so it can be accessed by the application."),
                                       tags$li(strong("Server:"), " Add a matching case to the ",
                                               code("observeEvent(input$calibrantSource)"), " block so the calibrant checkbox selector populates correctly when the new library is selected."),
                                       tags$li(strong("Server:"), " Create a matching server-side table output using ", code("DT::renderDT()"), " and ", code("formatCalibrantTable()"), "."),
                                       tags$li(strong("UI:"), " Add the new library as a named choice in the ", code("calibrantSource"), " selectInput."),
                                       tags$li(strong("UI:"), " Add a matching tab or table output using ", code("DT::DTOutput()"), " in the Calibrant Reference Tables tab."),
                                       tags$li(strong("UI:"), " Use ", code("HTML()"), " in the tab title if superscripts or other scientific formatting are needed.")
                                     )
                                   ),
                                   
                                   div(
                                     class = "announcement-card",
                                     span(class = "announcement-tag", "Updated Methods"),
                                     h4(class = "announcement-header", "🧪 Calibration Adjustments"),
                                     p("The application now includes an additional calibration workflow accounting for mass-dependent flight times, ion charge state, and reduced mass when calculating corrected multipass CCS values."),
                                     p("This workflow allows users to apply an enhanced duty cycle (EDC) delay coefficient to correct for mass-dependent flight time contributions."),
                                     p("The EDC delay coefficient can be found in the Typhoon folder in MassLynx and is generally between 1.4 and 1.6."),
                                     tags$p(
                                       a("Read more", 
                                         href = "https://www.nature.com/articles/nprot.2008.78", 
                                         target = "_blank", 
                                         style = "font-weight: bold; text-decoration: underline;"
                                       )
                                     )
                                   ),
                                   
                                   div(
                                     class = "announcement-card",
                                     span(class = "announcement-tag", "Compatibility Fix"),
                                     h4(class = "announcement-header", "⚠️ ggplot2 Update Notice"),
                                     p("An update to ggplot2 (v3.5.0 and above) introduced changes to how mathematical expressions, ",
                                       code("expression()"), " and ", code("bquote()"), 
                                       " are handled inside plot text annotations. These changes affected local offline users who updated to ggplot2 v3.5.0 and above."),
                                     p("The online version of the application remains fully functional and was not affected by this update."),
                                     
                                     h5("✔️ What has been fixed"),
                                     tags$ul(
                                       tags$li("All text annotations using mathematical formatting have been rewritten using character strings."),
                                       tags$li("All subtitles and labels have been updated to ensure compatibility with future ggplot2 releases.")
                                     ),
                                     
                                     h5("How to fix your local installation"),
                                     p("If your local version of the Multipass CCS Refiner was crashing after uploading calibration files, please download the most recent version from GitHub using the link below."),
                                     p("After updating, restart RStudio and ensure you are using the latest versions of R and ggplot2.")
                                   ),
                                   
                                   div(
                                     class = "announcement-card",
                                     h4(class = "announcement-header", "Download the Latest Version from GitHub"),
                                     tags$p(
                                       "👉 ", 
                                       a("Multipass CCS Refiner", 
                                         href = "https://github.com/facundof2016/Multipass-CCS-Refiner", 
                                         target = "_blank", 
                                         style = "font-weight: bold; text-decoration: underline;"
                                       )
                                     )
                                   )
                                 )
                               )
                             ),
                           # ---------------- End Announcements Panel ----------------
                           div(class = "background-darkblue", 
                               # Flex container with the header and toggle button
                               div(class = "header-container", 
                                   # header font size is changed by h. h1() is largest followed by h2() and h3() etc.
                                   h1("Background"), 
                                   # Action button next to the header
                                   actionButton("toggle_background", label = NULL, icon = icon("caret-down"), class = "btn btn-info toggle-btn", style = "background-color: black; color: white; border: none;")
                               ),
                               hidden(
                                 div(
                                   id = "background_content",
                                   div(
                                     h4(strong("Ion Mobility Spectrometry and CCS")),
                                     p(
                                       "Ion mobility spectrometry (IMS) separates gas-phase ions on a millisecond timescale based on differences in size, shape, and charge, which influence collision rates with an inert buffer gas. ",
                                       "IMS measurements are commonly reported as collision cross section (CCS) values, which can support compound annotation. ",
                                       "Drift tube IMS is the classical IMS platform and enables ion mobilities to be measured directly for CCS calculation. ",
                                       "The rapid adoption of IMS has led to the development of additional platforms with resolving powers in the hundreds, including cyclic IMS (cIMS) spectrometers."
                                     ),
                                     p(
                                       "cIMS uses traveling wave electric fields to circulate ions through a closed-loop separation region, extending ion path length and improving resolution. ",
                                       "However, the use of an oscillating electric field requires CCS measurements to be calculated using ions of known mobility. ",
                                       "In addition, changing the direction of the traveling wave during ion ejection can introduce velocity perturbations that affect measured arrival times and, ultimately, calculated CCS values."
                                     )
                                   ),
                                   br(),
                                   
                                   div(
                                     h4(strong("Perturbation-Corrected Multipass Calibration")),
                                     p(
                                       "Lin and Costello demonstrated that a linear relationship based on ion path length can be used to calculate a perturbation-corrected multipass drift time from measurable variables ",
                                       a(
                                         "Lin and Costello (Anal. Chem. 2024)",
                                         href = "https://doi.org/10.1021/acs.analchem.4c01758",
                                         target = "_blank"
                                       ),
                                       ".",
                                       "This approach eliminates the need for carefully selected arrival times and provides a practical framework for correcting multipass periodic drift times."
                                     ),
                                   ),
                                   br(),
                                   
                                   div(
                                     h4(strong("Multipass CCS Refiner")),
                                     p(
                                       "The Multipass CCS Refiner streamlines multipass CCS calibration by allowing users to upload calibration data, generate corrected drift times, build calibration curves, calculate CCS values, and visualize results with minimal manual preprocessing.",
                                       "The app is designed to reduce post-processing time and make multipass CCS refinement more accessible to users without requiring extensive programming experience.",
                                       "The Multipass CCS Refiner was developed using R version 4.6.0, RStudio version 2026.04.0, and Shiny version 1.10.0. ",
                                       "The full source code is available on ",
                                       a(
                                         "GitHub",
                                         href = "https://github.com/facundof2016/Multipass-CCS-Refiner",
                                         target = "_blank"
                                       ),
                                       ", and a video tutorial demonstrating installation and use is available on ",
                                       a(
                                         "YouTube",
                                         href = "https://www.youtube.com/watch?v=AXrbocbQGY8&t=0s",
                                         target = "_blank"
                                       ),
                                       "."
                                     )
                                   )
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
                                     p("The Multipass CCS Refiner allows users to create multipass calibration curves,", 
                                       "calculate experimental collisional cross section (CCS) values and download their results.",
                                       "To assist with automation the largest peak in an arrival time distribution (ATD) is located and fitted.",
                                       "While splitting ATDs into separate peaks can be done manually, the Multipass CCS Refiner offers an in app alternative.",
                                       "Example documents to assist with proper formatting can be downloaded below:"),
                                     p(""),
                                     br(),
                                     #Add the first example file that users can download. The download handler is in the server.
                                     p(HTML("<strong>Creating a Calibration Curve</strong>")),
                                     div(
                                       style = "display: flex; flex-wrap: wrap; gap: 8px; margin-bottom: 10px;",
                                       uiOutput("exampleCalibrationUI1"),
                                       uiOutput("exampleCalibrationUI2"),
                                       uiOutput("exampleRDSUI1"),
                                       uiOutput("exampleRDSUI2")
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
                                       "After successfully running a calibration, power curves for both the single-pass and multipass results will be displayed at the bottom of the page. ",
                                       "A file containing arrival time calculations and linear plots for each standard in the mixture can be downloaded locally using the 'Download All Plots' button. ",
                                       "A successful calibration can then be saved to any calibration curve slot using the 'Save' button."
                                     ),
                                     br(),
                                     # Add the second button to download the experimental example.
                                     p(HTML("<strong>Processing Experimental Data</strong>")),
                                     div(
                                       style = "display: flex; flex-wrap: wrap; gap: 8px; margin-bottom: 10px;",
                                       uiOutput("exampleExperimentUI1"),
                                       uiOutput("exampleExperimentUI2")
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
                                     p(HTML("<strong>Separating ATDs</strong>")),
                                     div(
                                       style = "display: flex; flex-wrap: wrap; gap: 8px; margin-bottom: 10px;",
                                       uiOutput("exampleMultiPeakATDUI")
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
                                               a("https://doi.org/10.1021/acs.jafc.0c04498", href="https://doi.org/10.1021/acs.jafc.0c04498", target="_blank")),
                                       tags$li("Moran-Garrido, M., Taha, A.Y., Gaudioso, Á., Ledesma, M.D., & Barbas, C. (2025). Development of an Oxylipin Library Using Liquid Chromatography-Ion Mobility Quadrupole Time-of-Flight: Application to Mouse Brain Tissue. Analytical Chemistry, 97(6), 3643–3650. ",
                                               a("https://doi.org/10.1021/acs.analchem.4c06265",
                                                 href = "https://doi.org/10.1021/acs.analchem.4c06265",
                                                 target = "_blank")),
                                       
                                       tags$li("da Silva, K.M., Wölk, M., Nepachalovich, P., Iturrospe, E., Covaci, A., van Nuijs, A.L.N., & Fedorova, M. (2023). Investigating the Potential of Drift Tube Ion Mobility for the Analysis of Oxidized Lipids. Analytical Chemistry, 95(36), 13566–13574. ",
                                               a("https://doi.org/10.1021/acs.analchem.3c02213",
                                                 href = "https://doi.org/10.1021/acs.analchem.3c02213",
                                                 target = "_blank")),
                                       tags$li("Ruotolo, B., Benesch, J., Sandercock, A., Hyung, S.J., Robinson, C.V. (2008). Ion mobility–mass spectrometry analysis of large protein complexes. Nature Protocols, 3, 1139–1152. ",
                                               a("https://doi.org/10.1038/nprot.2008.78",
                                                 href = "https://doi.org/10.1038/nprot.2008.78",
                                                 target = "_blank")),
                                       tags$li("Bush, M.F., Hall, Z., Giles, K., Hoyes, J., Robinson, C.V., & Ruotolo, B.T. (2010). Collision Cross Sections of Proteins and Their Complexes: A Calibration Framework and Database for Gas-Phase Structural Biology. Analytical Chemistry, 82(22), 9557–9565. ",
                                               a("https://doi.org/10.1021/ac1022953", href = "https://doi.org/10.1021/ac1022953", target = "_blank"))
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
                                       tags$li("Wickham H. (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag.", 
                                               a("https://cran.r-project.org/web/packages/ggplot2/index.html", href="https://cran.r-project.org/web/packages/ggplot2/index.html", target="_blank")),
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
                           br(),
                           h3("Import Saved Calibration"),
                           
                           fluidRow(
                             column(
                               4,
                               fileInput(
                                 "importCalibrationFile",
                                 "Upload calibration (.rds)",
                                 accept = ".rds"
                               )
                             ),
                             column(
                               4,
                               selectInput(
                                 "importToSlot",
                                 "Import to slot:",
                                 choices = c("Curve 1", "Curve 2", "Curve 3"),
                                 selected = "Curve 1"
                               )
                             ),
                             column(
                               4,
                               actionButton(
                                 "importCalibrationBtn",
                                 "Import Calibration",
                                 class = "btn-primary"
                               )
                             )
                           ),
                           br(),
                           
                           h3("Create New Calibration"),
                           
                           ## --- TOP ROW: Method + Auto-fill ---
                           fluidRow(
                             column(
                               6,
                               selectInput(
                                 "calibrationMethod",
                                 "Calibration Method:",
                                 c("Xia, Lin & Costello",
                                   "Xia + Bush & Ruotolo")
                               )
                             ),
                             column(
                               6,
                               checkboxInput(
                                 "useCalibrantMetadata",
                                 "Use Built-In Calibrant Data",
                                 value = FALSE
                               )
                             )
                           ),
                           br(),
                           
                           ## --- Calibrant Library + Checkbox Selection ---
                           conditionalPanel(
                             condition = "input.useCalibrantMetadata === true",
                             fluidRow(
                               column(
                                 6,
                                 selectInput(
                                   "calibrantSource",
                                   "Calibrant Library:",
                                   choices = c(
                                     "Major Mix Positive Ion Mode"    = "major_pos",
                                     "Major Mix Negative Ion Mode"    = "major_neg",
                                     "Spherical Positive Ion Mode"    = "spherical"
                                   )
                                 )
                               ),
                               column(
                                 6,
                                 uiOutput("calibrantSelectorUI")
                               )
                             ),
                             br()
                           ),
                           
                           
                           ## --- Source + TW Conditions (now next to each other) ---
                           fluidRow(
                             column(6, selectInput("source", "Source: ",
                                                   c("ESI", "nano-ESI", "DESI", "APCI", "Other"))),
                             column(6, textInput("twConditions", "Traveling Wave Conditions:"))
                           ),
                           br(),
                           
                           ## --- MODE (manual only) ---
                           conditionalPanel(
                             condition = "input.calibrationMethod === 'Xia + Bush & Ruotolo'",
                             fluidRow(
                               column(
                                 6,
                                 selectInput("ruotoloGas", "Drift Gas:",
                                             c("Nitrogen", "Helium"), selected = "Nitrogen")
                               ),
                               column(
                                 6,
                                 numericInput("ruotolo_c",
                                              "EDC Delay Coefficient:",
                                              value = 1.41, min = 0, step = 0.01)
                               )
                             ),
                             
                             ## Manual-only Ruotolo m/z, MW, and charge inputs
                             conditionalPanel(
                               condition = "input.useCalibrantMetadata === false",
                               
                               fluidRow(
                                 column(
                                   6,
                                   selectInput(
                                     "ruotoloCharge",
                                     "Charge State Magnitude:",
                                     choices = c(1, 2, 3),
                                     selected = 1
                                   )
                                 )
                               ),
                               
                               fluidRow(
                                 column(6, textInput("ruotolo_mz", "m/z Values (comma separated):")),
                                 column(6, textInput("ruotolo_MW", "Molecular Weights (Da, comma separated):"))
                               )
                             ),
                             br()
                           ),
                           
                           ## --- ANALYTE NAMES + CCS (manual only) ---
                           conditionalPanel(
                             condition = "input.useCalibrantMetadata === false",
                             fluidRow(
                               column(6, textAreaInput("calibrationAnalyteNames",
                                                       "Standard Names (comma separated):")),
                               column(6, textAreaInput("calibrationCCS",
                                                       "Theoretical CCS Values (comma separated):"))
                             )
                           ),
                           br(),
                           
                           ## --- Upload Files + Save Slot ---
                           fluidRow(
                             column(6, fileInput("calibrationFile",
                                                 "Upload File Path/Name:", accept = ".xlsx")),
                             column(6, selectInput("saveToSlot", "Save to Slot:",
                                                   c("Curve 1", "Curve 2", "Curve 3")))
                           ),
                           br(),
                           
                           ## --- Run, Save, Downloads ---
                           fluidRow(
                             column(1, actionButton("runCalibration","Run")), 
                             column(1, actionButton("Save","Save")), 
                             column(7, uiOutput("disabledDownloadCalibrationCurves",
                                                style = "width: 96.25%; text-align: right;")),
                             column(2, uiOutput("disabledDownloadAllCalibrationPlots",
                                                style = "width: 100%; text-align: left;"))
                           ),
                           br(),
                           
                           ## --- Xia Plots ---
                           tabsetPanel(
                             tabPanel("Single-Pass Curve", plotOutput("singlePassCalibrationCurve")),
                             tabPanel("Multi-Pass Curve",  plotOutput("MultiPassCalibrationCurve"))
                           ),
                           br(),
                           
                           ## --- Ruotolo Plots & Summary ---
                           conditionalPanel(
                             condition = "input.calibrationMethod === 'Xia + Bush & Ruotolo'",
                             h2("Bush & Ruotolo Calibration Results"),
                             tabsetPanel(
                               tabPanel("Log–Log Ruotolo Fit", plotOutput("ruotoloPlot")),
                               tabPanel("Ruotolo Summary", tableOutput("ruotoloSummary"))
                             )
                           )
                  ),# End of tab 2 Create Calibration Curves.
                  # Test New Analytes Tab UI----
                  tabPanel("Process Data", 
                           
                           h1("Calculate and Compare CCS Values"),
                           textOutput("analytesStatus"),
                           # Rows 1-3 ask for general information similar to the create calibration curve tab.
                           fluidRow(
                             column(6, selectInput("curveSlot", "Calibration Curve: ", c("Curve 1","Curve 2","Curve 3"))),
                             column(6, selectInput("knownCCSValues", "Compare to Known CCS Values: ", c("Yes","No"))),
                           ),
                           conditionalPanel(
                             condition = "output.curveHasRuotolo",
                             h3("Bush & Ruotolo Inputs for Unknown Analytes"),
                             fluidRow(
                               column(
                                 6,
                                 textInput(
                                   "ruotolo_mz_unknown",
                                   "Analyte m/z values (comma separated):"
                                 )
                               ),
                               column(
                                 6,
                                 textInput(
                                   "ruotolo_MW_unknown",
                                   "Analyte MW values (Da, comma separated):"
                                 )
                               ),
                             ),
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
                           # The plot below is an optional comparison plot between accounting for 
                           # charge, reduced mass and size dependent drift times (Ruotolo) and only using the 
                           # multipass method (Xia).  
                           # br(),
                           # h3("Comparison Plot (Xia vs Bush–Ruotolo)"),
                           # plotOutput("comparisonPlotRU"),
                           
                           
                           textOutput("status")
                           
                  ), # End of tab 3 Process Data.
                  # Multiple Peak ATD Tab UI----
                  tabPanel("Peak Resolution", 
                                    
                            h1("Plot and Fit ATDs for Peak Resolution Calculation"),
                            textOutput("multiATDStatus"),
                            
                            # Ask use for file upload and the location of their data.
                            fluidRow(
                              column(3, fileInput("usersATDFile", "Upload File: ", accept = ".xlsx")),
                              column(3, uiOutput("sheetNameUI")),
                              column(3, numericInput("startRow","Data Start Row: ", 1, min=1, max=50)),
                              column(3, numericInput("startColumn","Data Start Column: ", 1, min=1, max=50))
                            ),
                            # Button to generate plots.
                            fluidRow(
                              column(2, actionButton("plotRawData", "Plot Raw Data"))
                            ),
                            # Plot the raw data
                            fluidRow(
                              column(12, plotOutput("rawDataPlot"))
                            ),
                            # Add as many peaks as needed up to 10 peaks in one ATD.
                            fluidRow(
                              column(3, actionButton("autoGuessPeaks","Auto-Guess Peaks")),
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
                            # Rp and Rpp table
                            hr(),
                            h2("Single Peak Resolution"),
                            DT::DTOutput("peakMetricsTable"),
                            
                            hr(),
                            h2("Two Peak Resolution"),
                            DT::DTOutput("rppTable"),
                            
                            textOutput("status")
                           
                  ), # End of tab 4 Multiple Peak ATD
                  # Calibration Curves Tab UI----
                  # Calibration Curves are essentially identical and exist so users can store multiple curves at once.
                  # For example one in positive mode and one in negative mode or perhaps using different mass ranges or calibrants.
                  tabPanel(
                    "Saved Calibration Curves",
                    
                    h1("Saved Calibration Curves"),
                    
                    selectInput(
                      "whichCurve",
                      "Select Calibration Curve:",
                      choices = c("Curve 1", "Curve 2", "Curve 3"),
                      selected = "Curve 1"
                    ),
                    # Button to export curves as RDS files to reupload later
                    column(
                      3,
                      conditionalPanel(
                        condition = "output.canExportCalibration",
                        downloadButton("exportCalibration", "Export Calibration")
                      )
                    ),
                    br(),
                    # ================= CURVE 1 =================
                    conditionalPanel(
                      condition = "input.whichCurve === 'Curve 1'",
                      
                      h1("Calibration Curve 1"),
                      verbatimTextOutput("source1"),
                      verbatimTextOutput("mode1"),
                      verbatimTextOutput("standard1"),
                      verbatimTextOutput("massRange1"),
                      verbatimTextOutput("twConditions1"),
                      verbatimTextOutput("singlePassEquationCurve1"),
                      verbatimTextOutput("multiPassEquationCurve1"),
                      verbatimTextOutput("ruotoloInfo1"),
                      
                      fluidRow(
                        column(3, actionButton("delete1", "Delete Curve"))
                      ),
                      
                      tabsetPanel(
                        tabPanel("Single Pass Curve",
                                 plotOutput("singlePassCalibrationCurve1")),
                        tabPanel("Multipass Curve",
                                 plotOutput("multiPassCalibrationCurve1")),
                        tabPanel("Ruotolo Log–Log Fit",
                                 conditionalPanel(
                                   condition = "output.hasRuotolo1",
                                   plotOutput("ruotoloPlot1")
                                 )),
                        tabPanel("Ruotolo Summary Table",
                                 conditionalPanel(
                                   condition = "output.hasRuotolo1",
                                   tableOutput("ruotoloSummary1")
                                 ))
                      )
                    ),
                    
                    # ================= CURVE 2 =================
                    conditionalPanel(
                      condition = "input.whichCurve === 'Curve 2'",
                      
                      h1("Calibration Curve 2"),
                      verbatimTextOutput("source2"),
                      verbatimTextOutput("mode2"),
                      verbatimTextOutput("standard2"),
                      verbatimTextOutput("massRange2"),
                      verbatimTextOutput("twConditions2"),
                      verbatimTextOutput("singlePassEquationCurve2"),
                      verbatimTextOutput("multiPassEquationCurve2"),
                      verbatimTextOutput("ruotoloInfo2"),
                      
                      fluidRow(
                        column(3, actionButton("delete2", "Delete Curve"))
                      ),
                      
                      tabsetPanel(
                        tabPanel("Single Pass Curve",
                                 plotOutput("singlePassCalibrationCurve2")),
                        tabPanel("Multipass Curve",
                                 plotOutput("multiPassCalibrationCurve2")),
                        tabPanel("Ruotolo Log–Log Fit",
                                 conditionalPanel(
                                   condition = "output.hasRuotolo2",
                                   plotOutput("ruotoloPlot2")
                                 )),
                        tabPanel("Ruotolo Summary Table",
                                 conditionalPanel(
                                   condition = "output.hasRuotolo2",
                                   tableOutput("ruotoloSummary2")
                                 ))
                      )
                    ),
                    
                    # ================= CURVE 3 =================
                    conditionalPanel(
                      condition = "input.whichCurve === 'Curve 3'",
                      
                      h1("Calibration Curve 3"),
                      verbatimTextOutput("source3"),
                      verbatimTextOutput("mode3"),
                      verbatimTextOutput("standard3"),
                      verbatimTextOutput("massRange3"),
                      verbatimTextOutput("twConditions3"),
                      verbatimTextOutput("singlePassEquationCurve3"),
                      verbatimTextOutput("multiPassEquationCurve3"),
                      verbatimTextOutput("ruotoloInfo3"),
                      
                      fluidRow(
                        column(3, actionButton("delete3", "Delete Curve"))
                      ),
                      
                      tabsetPanel(
                        tabPanel("Single Pass Curve",
                                 plotOutput("singlePassCalibrationCurve3")),
                        tabPanel("Multipass Curve",
                                 plotOutput("multiPassCalibrationCurve3")),
                        tabPanel("Ruotolo Log–Log Fit",
                                 conditionalPanel(
                                   condition = "output.hasRuotolo3",
                                   plotOutput("ruotoloPlot3")
                                 )),
                        tabPanel("Ruotolo Summary Table",
                                 conditionalPanel(
                                   condition = "output.hasRuotolo3",
                                   tableOutput("ruotoloSummary3")
                                 ))
                      )
                    )
                  ), # End of Saved Calibration Curves tab
                  
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
                  ), # End of Settings.
                  tabPanel("Calibrant Reference Tables",
                           
                           h1("Calibrant Reference"),
                           
                           tabsetPanel(
                             
                             tabPanel("Major Mix [M + H]⁺",
                                      DT::DTOutput("calibrantMajorMixPos")
                             ),
                             
                             tabPanel("Major Mix [M − H]⁻",
                                      DT::DTOutput("calibrantMajorMixNeg")
                             ),
                             
                             tabPanel("Spherical Low Mass",
                                      DT::DTOutput("calibrantSpherical")
                             )
                           )
                  ), # End of Calibrant References Tab
                ) # Close the navbarPage
) # Close the fluidPage

##### End of Defining UI #####
