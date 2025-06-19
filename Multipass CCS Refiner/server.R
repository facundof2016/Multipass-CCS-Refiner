# This file contains the server for Multipass CCS Refiner
#library(readxl)
#library(xlsx)
#library(ggplot2) 
#library(grid)
#library(DT)
source("functions.R")
##### Define server function #####
server <- function(input, output, session) {
  # Visibility toggles for intro of the UI ----
  observeEvent(input$toggle_background, {
    toggle("background_content")  
  })
  observeEvent(input$toggle_using_app, {
    toggle("using_app_content")  
  })
  observeEvent(input$toggle_citations, {
    toggle("citations_content")  
  })# End visibility toggle creation.
  
  # Create reactive values ----
  # All reactive values will change whenever the user changes them in the GUI.
  # Values to store files which change upon file upload.
  calibrationFile <- reactiveVal(NULL)
  unknownFile <- reactiveVal(NULL)
  
  # Equations from the plot_power_curve function for single and multipass calibrations. Can be reset by the user.
  singlePassEquationCurve1 <- reactiveVal(NULL)
  multiPassEquationCurve1 <- reactiveVal(NULL) # Curves saved to Calibration Curve 1 tab.
  singlePassEquationCurve2 <- reactiveVal(NULL)
  multiPassEquationCurve2 <- reactiveVal(NULL) # Curves saved to Calibration Curve 2 tab.
  singlePassEquationCurve3 <- reactiveVal(NULL)
  multiPassEquationCurve3 <- reactiveVal(NULL) # Curves saved to Calibration Curve 2 tab.
  
  # Reactive values to store curve expressions
  singlePassCurveExpression <- reactiveVal()
  multiPassCurveExpression <- reactiveVal()
  
  # Boolean values to track when calibrations and data analyses are completed. Used to show status messages on screen. 
  calibrationComplete <- reactiveVal(FALSE)
  processDataComplete <- reactiveVal(FALSE)
  
  # Curves can be saved or deleted by the user.
  savedCal1 <- reactiveVal(FALSE) 
  savedCal2 <- reactiveVal(FALSE)
  savedCal3 <- reactiveVal(FALSE)
  
  # Plot storage which is reset if the user runs a new calibration or process new data. 
  reactivePlots <- reactiveValues(
    allCalibrationPlots = list(),        
    powerCurvePlots = list(), 
    allProcessDataPlots = list()
  )
  
  # Reactive values for calibration curve information. These can be saved and deleted by the user. 
  calibrationInfo <- reactiveValues(
    curve1 = list(source = NULL, mode = NULL, standard = NULL, massRange = NULL, twConditions = NULL, singlePassEquationCurve1 = NULL, multiPassEquationCurve1 = NULL),
    curve2 = list(source = NULL, mode = NULL, standard = NULL, massRange = NULL, twConditions = NULL, singlePassEquationCurve2 = NULL, multiPassEquationCurve2 = NULL),
    curve3 = list(source = NULL, mode = NULL, standard = NULL, massRange = NULL, twConditions = NULL, singlePassEquationCurve3 = NULL, multiPassEquationCurve3 = NULL)
  )
  # Reactive object for multiple arrival time distributions and nls models generated from them
  multiplePeakReactive <- reactiveVal(NULL)
  modelsReactive <- reactiveVal(list()) 
  # End reactive variable creation.
  
  # Observe Events for File Uploads ----
  # Code executed when a calibration file is uploaded.
  observeEvent(input$calibrationFile, {
    if (grepl("\\.xlsx$", input$calibrationFile$name)) {
      calibrationFile(input$calibrationFile$datapath)
      
    } else { # This error will not trigger because of upload requirements. Left here if users change the code.
      showNotification("Please upload a valid .xlsx file.", type = "error")
    }
  })
  # Observe Event for User Data File Upload
  observeEvent(input$usersDataFile, {
    if (grepl("\\.xlsx$", input$usersDataFile$name)) {
      unknownFile(input$usersDataFile$datapath)
      
    } else { # This error will not trigger because of upload requirements. Left here if users change the code.
      showNotification("Please upload a valid .xlsx.", type = "error")
    }
  }) # End file uploading observer.
  
  # Save Button Observers ----
  # Code executed when the save button is pressed. This depends on whether Curve 1 2 or 3 are selected in the GUI.
  # This code works with the rendering calibration curves below to show the saved results in the GUI.
  observeEvent(input$Save, {
    if (!calibrationComplete()) {
      errorMessage <- paste("Error: Please complete a calibration run prior to saving.")
      output$calibrationStatus <- renderText(errorMessage)
      showNotification(errorMessage, type = "error")
      return()
    }
    
    if (input$saveToSlot == "Curve 1") {
      # Store metadata and plots in calibrationInfo
      calibrationInfo$curve1 <- list(
        source = input$source,
        mode = input$mode,
        standard = input$standard,
        massRange = input$massRange,
        twConditions = input$twConditions,
        singlePassEquationCurve1 = singlePassEquationCurve1(),
        multiPassEquationCurve1 = multiPassEquationCurve1(),
        plots = reactivePlots$powerCurvePlots
      )
      
      # Display equations
      output$singlePassEquationCurve1 <- renderText({ 
        paste("Single Pass Equation: ", calibrationInfo$curve1$singlePassEquationCurve1) 
      })
      output$multiPassEquationCurve1 <- renderText({ 
        paste("Multi Pass Equation: ", calibrationInfo$curve1$multiPassEquationCurve1) 
      })
      
      # Display plots using renderPlot
      output$singlePassCalibrationCurve1 <- renderPlot({
        req(calibrationInfo$curve1$plots)
        calibrationInfo$curve1$plots$Single
      })
      output$multiPassCalibrationCurve1 <- renderPlot({
        req(calibrationInfo$curve1$plots)
        calibrationInfo$curve1$plots$Multi
      })
      
      savedCal1(TRUE)
    }
    
    if (input$saveToSlot == "Curve 2") {
      calibrationInfo$curve2 <- list(
        source = input$source,
        mode = input$mode,
        standard = input$standard,
        massRange = input$massRange,
        twConditions = input$twConditions,
        singlePassEquationCurve2 = singlePassEquationCurve2(),
        multiPassEquationCurve2 = multiPassEquationCurve2(),
        plots = reactivePlots$powerCurvePlots
      )
      
      output$singlePassEquationCurve2 <- renderText({ 
        paste("Single Pass Equation: ", calibrationInfo$curve2$singlePassEquationCurve2) 
      })
      output$multiPassEquationCurve2 <- renderText({ 
        paste("Multi Pass Equation: ", calibrationInfo$curve2$multiPassEquationCurve2) 
      })
      
      output$singlePassCalibrationCurve2 <- renderPlot({
        req(calibrationInfo$curve2$plots)
        calibrationInfo$curve2$plots$Single
      })
      output$MultiPassCalibrationCurve2 <- renderPlot({
        req(calibrationInfo$curve2$plots)
        calibrationInfo$curve2$plots$Multi
      })
      
      savedCal2(TRUE)
    }
    if (input$saveToSlot == "Curve 3") {
      calibrationInfo$curve3 <- list(
        source = input$source,
        mode = input$mode,
        standard = input$standard,
        massRange = input$massRange,
        twConditions = input$twConditions,
        singlePassEquationCurve3 = singlePassEquationCurve3(),
        multiPassEquationCurve3 = multiPassEquationCurve3(),
        plots = reactivePlots$powerCurvePlots
      )
      
      output$singlePassEquationCurve3 <- renderText({ 
        paste("Single Pass Equation: ", calibrationInfo$curve3$singlePassEquationCurve3) 
      })
      output$multiPassEquationCurve3 <- renderText({ 
        paste("Multi Pass Equation: ", calibrationInfo$curve3$multiPassEquationCurve3) 
      })
      
      output$singlePassCalibrationCurve3 <- renderPlot({
        req(calibrationInfo$curve3$plots)
        calibrationInfo$curve3$plots$Single
      })
      output$MultiPassCalibrationCurve3 <- renderPlot({
        req(calibrationInfo$curve3$plots)
        calibrationInfo$curve3$plots$Multi
      })
      
      savedCal3(TRUE)
    }
    showNotification(
      paste("Check the Calibration", as.character(input$saveToSlot), "tab to see your results"), 
      type = "message"
    )
    output$calibrationStatus <- renderText("Plot Saved!")
  }) # End save button observer.
  
  # Calibration Curves 1 and 2 Tabs Rendering ----
  # This code is executed with the save button observer to show results in the GUI.
  output$mode1 <- renderText({ paste("Mode: ", calibrationInfo$curve1$mode) })
  output$source1 <- renderText({ paste("Source: ",calibrationInfo$curve1$source)})
  output$standard1 <- renderText({ paste("Standard: ", calibrationInfo$curve1$standard) })
  output$massRange1 <- renderText({ paste("Mass Range: ", calibrationInfo$curve1$massRange, " m/z") })
  output$twConditions1 <- renderText({ paste("TW Conditions: ", calibrationInfo$curve1$twConditions) })
  output$singlePassEquationCurve1 <- renderText({ paste("Single Pass Equation: ", calibrationInfo$singlePassCalibrationCurve1) })
  output$multiPassEquationCurve1 <- renderText({ paste("Multi Pass Equation: ", calibrationInfo$multiPassCalibrationCurve1) })
  
  output$mode2 <- renderText({ paste("Mode: ", calibrationInfo$curve2$mode) })
  output$source2 <- renderText({ paste("Source: ",calibrationInfo$curve2$source)})
  output$standard2 <- renderText({ paste("Standard: ", calibrationInfo$curve2$standard) })
  output$massRange2 <- renderText({ paste("Mass Range: ", calibrationInfo$curve2$massRange, " m/z") })
  output$twConditions2 <- renderText({ paste("TW Conditions: ", calibrationInfo$curve2$twConditions) })
  output$singlePassEquationCurve2 <- renderText({ paste("Single Pass Equation: ", calibrationInfo$singlePassCalibrationCurve2) })
  output$multiPassEquationCurve2 <- renderText({ paste("Multi Pass Equation: ", calibrationInfo$MultiPassCalibrationCurve2) })
  
  output$mode3 <- renderText({ paste("Mode: ", calibrationInfo$curve3$mode) })
  output$source3 <- renderText({ paste("Source: ",calibrationInfo$curve3$source)})
  output$standard3 <- renderText({ paste("Standard: ", calibrationInfo$curve3$standard) })
  output$massRange3 <- renderText({ paste("Mass Range: ", calibrationInfo$curve3$massRange, " m/z") })
  output$twConditions3 <- renderText({ paste("TW Conditions: ", calibrationInfo$curve3$twConditions) })
  output$singlePassEquationCurve3 <- renderText({ paste("Single Pass Equation: ", calibrationInfo$singlePassCalibrationCurve3) })
  output$multiPassEquationCurve3 <- renderText({ paste("Multi Pass Equation: ", calibrationInfo$MultiPassCalibrationCurve3) })
  # End Calibration Curves rendering.
  
  # Delete Button Observers ----
  # Code executed to clear data when delete is pressed. Reset values to starting NULL values.
  observeEvent(input$delete1, {
    calibrationInfo$curve1 <- list(mode = NULL, standard = NULL, massRange = NULL,Curvetp1 = NULL, Curvetpp = NULL)
    output$singlePassCalibrationCurve1 <- renderImage({
      NULL
    }, deleteFile = TRUE)
    output$multiPassCalibrationCurve1 <- renderImage({
      NULL
    }, deleteFile = TRUE)
  })
  observeEvent(input$delete2, {
    calibrationInfo$curve2 <- list(mode = NULL, standard = NULL, massRange = NULL,Curvetp1 = NULL, Curvetpp = NULL)
    output$singlePassCalibrationCurve2 <- renderImage({
      NULL
    }, deleteFile = TRUE)
    output$MultiPassCalibrationCurve2 <- renderImage({
      NULL
    }, deleteFile = TRUE)
  }) 
  observeEvent(input$delete3, {
    calibrationInfo$curve3 <- list(mode = NULL, standard = NULL, massRange = NULL,Curvetp1 = NULL, Curvetpp = NULL)
    output$singlePassCalibrationCurve3 <- renderImage({
      NULL
    }, deleteFile = TRUE)
    output$MultiPassCalibrationCurve3 <- renderImage({
      NULL
    }, deleteFile = TRUE)
  })
  # End delete button observer.
  
  # Run Calibration Observer ----
  # This is the code that is executed when the user presses the run botton in the calibration tab.
  # This code works similarly to the code for the run button in the Process Data tab.
  # Both segments begin with a series of error checkers to check for incorrect information and communicate findings to the user.
  # Errors are reported at the top and bottom of the tab.
  observeEvent(input$runCalibration, {
    # Clear any old plots and calibration curves before running
    reactivePlots$powerCurvePlots <- list()
    reactivePlots$allCalibrationPlots <- list()
    
    # Validate user inputs by checking for null values. Make sure a file is present.
    if (is.null(calibrationFile()) || calibrationFile() == "") {
      errorMessage <- paste("Error: Please upload a file before running calibration.")
      output$calibrationStatus <- renderText(errorMessage)
      showNotification(errorMessage, type = "error")
      return()
    }
    
    # Check to make sure required analyte names and CCS values are entered.
    if (input$calibrationAnalyteNames == "" || input$calibrationCCS == "" || 
        is.na(input$calibrationAnalyteNames) || is.na(input$calibrationCCS)) {
      errorMessage <- paste("Error: Enter analyte names and CCS values before running calibration.")
      output$calibrationStatus <- renderText(errorMessage)
      showNotification(errorMessage, type = "error")
      return()
    }
    
    # Save the entered names and CCS values. Spaces in the user entry are ignored and values must be comma separated.
    calibrationAnalyteNames <- list()
    calibrationAnalyteNames <- unlist(strsplit(input$calibrationAnalyteNames, ",\\s*"))
    calibrationCCS <- as.numeric(unlist(strsplit(input$calibrationCCS, ",\\s*")))
    
    # Check if CCS values are numeric.
    if (any(is.na(calibrationCCS))) {
      errorMessage <- paste("Error: Some CCS values could not be interpreted as numeric. Please ensure all values are numbers and comma separated.")
      output$calibrationStatus <- renderText(errorMessage)
      showNotification(errorMessage, type = "error")
      return()  # or return something else, depending on what you want to happen
    }
    
    # Ensure the length of CCS values and analyte names entered match.
    if (length(calibrationAnalyteNames) != length(calibrationCCS)) {
      errorMessage <- paste("Error: The number of analytes and corresponding CCS values do not match.")
      output$calibrationStatus <- renderText(errorMessage)
      showNotification(errorMessage, type = "error")
      return()
    }
    
    # Grab the sheets for processing based on if they contain the name Analyte or analyte in the tab. This is so other tabs can exist in the files uploaded.
    calibrationFilePath <- normalizePath(calibrationFile())  # Normalize file path.
    allSheetNames <- excel_sheets(calibrationFilePath)
    ATDSheets <- allSheetNames[grepl("Analyte", allSheetNames, ignore.case = TRUE)]
    
    # Create an error if the number of sheets in the excel doc is less than the number of analytes entered.
    if (length(calibrationAnalyteNames) != length(ATDSheets)) {
      errorMessage <- paste("Error: The number of analytes entered is different than the number of processable sheets. Start tab names with Analyte. Please see the supporting information for an example document.")
      output$calibrationStatus <- renderText(errorMessage)
      showNotification(errorMessage, type = "error")
      return()
    }
    
    #Create an error checker to ensure separation times in the file are numeric and start with the bypass and single pass ATDs.
    for (i in seq_along(ATDSheets)){
      separationTimesRaw <- read.xlsx(calibrationFilePath, colClasses = "numeric", sheetName = ATDSheets[i], startRow = 1, endRow = 1, fillMergedCells = TRUE, as.data.frame = FALSE)
      separationTimesRaw <- as.vector(separationTimesRaw)
      separationTimes <- as.numeric(separationTimesRaw[!is.na(separationTimesRaw)])
      # Check if separationTimes contains only numeric values
      if (!all(is.finite(separationTimes))) {
        errorMessage <- paste("Error: Please ensure all separation times in your excel are numeric and do not contain ms or other labels. Check sheet", ATDSheets[i])
        output$calibrationStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
      # Check that the bypass time is first and labeled properly
      if (separationTimes[1] != 0 && separationTimes[1] != 0.01) {
        errorMessage <- paste("Error: Please ensure the bypass ATD is first in all sheets and has a 0 or 0.01 separation time label. Check sheet", ATDSheets[i])
        output$calibrationStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
      # Check that the single pass time is second and labeled properly.
      if (separationTimes[2] != 2) {
        errorMessage <- paste("Error: Please ensure the single pass ATD is second in all sheets and has a 2 separation time label. Check sheet", ATDSheets[i])
        output$calibrationStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
    }# End of error checking. The observer is continued below.
    
    # Tell the user that calculations have started. This is updated upon the completion of each analyte's calculations.
    output$calibrationStatus <- renderText("Calculating... Please Wait.")
    
    # Initialize local variables
    allTp1 <- numeric() # Stores the single pass periodic drift time.
    allTpp <- numeric() # Stores the perturbed periodic drift time or the y-intercept of our linear plots.
    calibrationPlotsTemp <- list() # Stores temporary plots.
    
    # Start calculation process with progress indicator.
    withProgress(message = "Calculating... Please wait", value = 0, {
      
      results <- lapply(seq_along(ATDSheets), function(i) {
        incProgress(1 / length(ATDSheets), detail = paste("Processing:", calibrationAnalyteNames[i]))
        
        # Process each sheet and store its results
        result <- process_sheet(calibrationFilePath, ATDSheets[i], calibrationAnalyteNames[i], input$linearPlotDecimals, list())
        
        # Add plots to calibrationPlotsTemp
        calibrationPlotsTemp <<- c(calibrationPlotsTemp, result$plotList)
        #print(paste("calibrationPlotsTemp after appending:", length(calibrationPlotsTemp))) # Print statement for debugging.
        
        return(result)
      })
      
      # Update reactivePlots with the accumulated calibrationPlotsTemp
      reactivePlots$allCalibrationPlots <- calibrationPlotsTemp
      
      # Save drift times and calculate calibration curves
      
      allTp1 <- sapply(results, function(x) x$driftTimes$tp1)
      allTpp <- sapply(results, function(x) x$driftTimes$tpp)
      
      # Generate and display single pass calibration curves
      resultSingle <- plot_power_curve(calibrationCCS, allTp1, "Single", input$powerCurveDecimals)
      singlePassCurveExpression(resultSingle$curveExpression)
      singlePassEquationCurve1(resultSingle$equation)
      singlePassEquationCurve2(resultSingle$equation)
      singlePassEquationCurve3(resultSingle$equation)
      output$singlePassCalibrationCurve <- renderPlot({ resultSingle$p })
      
      
      resultMulti <- plot_power_curve(calibrationCCS, allTpp, "Multi", input$powerCurveDecimals)
      multiPassCurveExpression(resultMulti$curveExpression)
      multiPassEquationCurve1(resultMulti$equation)
      multiPassEquationCurve2(resultMulti$equation)
      multiPassEquationCurve3(resultMulti$equation)
      output$MultiPassCalibrationCurve <- renderPlot({ resultMulti$p })
    
      # Store power curves
      reactivePlots$powerCurvePlots <- list(
        Single = resultSingle$p,
        Multi = resultMulti$p
      )
      
      # Notify the user that calibration is complete
      calibrationComplete(TRUE)
      output$calibrationStatus <- renderText("Calibration Completed!")
      showNotification("Calibration Completed!",type = "message")
    })
  }) # End run calibration observer.
  
  #Run Data Observer ----
  observeEvent(input$runData, {
    # The Run Data observer works similarly to the Run Calibration observer but needs to calculate additional values and does not create power curves.
    # Check if the user has first run and saved a calibration
    if ((input$saveToSlot == "Curve 1" && !savedCal1()) || (input$saveToSlot == "Curve 2" && !savedCal2()) || (input$saveToSlot == "Curve 3" && !savedCal3())) {
      errorMessage <- paste("Error: Please ensure a calibration curve has been saved to the slot you are trying to use.")
      output$analytesStatus <- renderText(errorMessage)
      showNotification(errorMessage, type = "error")
      return()
    }
    
    # Check if the user has uploaded a file
    if (is.null(unknownFile()) || unknownFile() == "") {
      errorMessage <- paste("Error: Please upload a file before running calibration.")
      output$analytesStatus <- renderText(errorMessage)
      showNotification(errorMessage, type = "error")
      return()
    }
    
    # Normalize the uploaded file path
    dataFilePath <- normalizePath(unknownFile())
    
    # Check if Analytes and CCS values are provided and valid for CCS option set to yes
    if(input$knownCCSValues == "Yes"){
      if (input$processDataAnalyteNames == "" || input$processDataCCSTheoretical == "" || 
          is.na(input$processDataAnalyteNames) || is.na(input$processDataCCSTheoretical)) {
        errorMessage <- paste("Error: Enter analyte names and CCS values before running.")
        output$analytesStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
    }
    
    # Capture user inputs for Analyte names and CCS values
    dataAnalyteNames <- list()
    dataAnalyteNames <- unlist(strsplit(input$processDataAnalyteNames, ",\\s*"))
    
    if(input$knownCCSValues == "Yes"){
      dataCCSValues <- as.numeric(unlist(strsplit(input$processDataCCSTheoretical, ",\\s*")))  # Convert to numeric
    }
    
    # Check if all CCS values are numeric if they are entered by the user.
    if(input$knownCCSValues == "Yes"){
      if(any(is.na(dataCCSValues))){
        errorMessage("Error: Please ensure all CCS values are numeric and comma separated.")
        output$analytesStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
    }
    
    # Check if the number of analytes is equal to the number of CCS values if they are entered.
    if(input$knownCCSValues == "Yes"){
      # Check if the number of analytes matches the number of CCS values
      if (length(dataAnalyteNames) != length(dataCCSValues)) {
        errorMessage <- paste("Error: The number of analytes and corresponding CCS values do not match.")
        output$analytesStatus <- renderText(errorMessage)
        showNotification(errorMesage, type = "error")
        return()
      }
    }
    
    # Check if Analytes are provided and valid for CCS set to No.
    if(input$knownCCSValues == "No"){
      if (input$processDataAnalyteNames == "" || is.na(input$processDataAnalyteNames)) {
        errorMessage <- paste("Error: Enter analyte names before running.")
        output$analytesStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
    }
    
    # Get the sheet names for data tabs
    allSheetNames <- excel_sheets(dataFilePath)
    ATDSheets <- allSheetNames[grepl("Analyte", allSheetNames, ignore.case = TRUE)]
    
    # Create an error if the number of analyte names does not equal the number of processable sheets. 
    if (length(dataAnalyteNames) != length(ATDSheets)) {
      errorMessage <- paste("Error: The number of analytes entered does not equal the number of processable sheets. Start tab names with Analyte. Please see the supporting information for an example document.")
      output$calibrationStatus <- renderText(errorMessage)
      showNotification(errorMessage, type = "error")
      return()
    }
    
    # Create an error checker to ensure separation times in the files are numeric and start with the bypass and single pass ATDs.
    for (i in seq_along(ATDSheets)){
      separationTimesRaw <- read.xlsx(dataFilePath, colClasses = "numeric", sheetName = ATDSheets[i], startRow = 1, endRow = 1, fillMergedCells = TRUE, as.data.frame = FALSE)
      separationTimes <- as.numeric(separationTimesRaw[!is.na(separationTimesRaw)])
      if (!all(is.finite(separationTimes))) {
        errorMessage <- paste("Error: Please ensure all separation times in your excel are numeric and do not contain ms or other labels. Check sheet", ATDSheets[i])
        output$calibrationStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
      
      if (separationTimes[1] != 0 && separationTimes[1] != 0.01) {
        errorMessage <- paste("Error: Please ensure the bypass ATD is first in all sheets and has a 0 or 0.01 separation time label. Check sheet", ATDSheets[i])
        output$calibrationStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
      
      if (separationTimes[2] != 2) {
        errorMessage <- paste("Error: Please ensure the single pass ATD is second in all sheets and has a 2 separation time label. Check sheet", ATDSheets[i])
        output$calibrationStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
    }
    
    # Create temp lists to store all plots and individual analyte plots
    dataPlotsTemp <- list()
    individualAnalytesTemp <- list()
    
    # Get the Calibration curve selected by the user.
    if (input$curveSlot == "Curve 1") {
      singleEquationString <- singlePassEquationCurve1()
      multiEquationString  <- multiPassEquationCurve1()
    } else if (input$curveSlot == "Curve 2") {
      singleEquationString <- singlePassEquationCurve2()
      multiEquationString  <- multiPassEquationCurve2()
    } else if (input$curveSlot == "Curve 3") {
      singleEquationString <- singlePassEquationCurve3()
      multiEquationString  <- multiPassEquationCurve3()
    }
    
    # Strip spaces before parsing
    singleEquationString <- gsub("\\s+", "", singleEquationString)
    multiEquationString  <- gsub("\\s+", "", multiEquationString)
    
    # Error checking
    cat("Single equation string: '", singleEquationString, "'\n")
    cat("Multi equation string: '", multiEquationString, "'\n")
    
    # Generate expressions for CCS calculation
    singleCoeffs <- extract_coefficients_from_string(singleEquationString)
    multiCoeffs  <- extract_coefficients_from_string(multiEquationString)
    
    singlePassEquation <- substitute(a * t^b, list(a = singleCoeffs$a, b = singleCoeffs$b))
    multiPassEquation  <- substitute(a * t^b, list(a = multiCoeffs$a,  b = multiCoeffs$b))
    
    # Create a few more local variables to store calculated drift times and percent differences.
    dataTp1 <- numeric(length(dataAnalyteNames))
    dataTpp <- numeric(length(dataAnalyteNames))
    percentDifferenceTp1 <- numeric(length(dataAnalyteNames))
    percentDifferenceTpp <- numeric(length(dataAnalyteNames))
    
    # Tell the user the app is running.
    output$analytesStatus <- renderText("Running... Please wait.")
    
    # Use withProgress to update the progress
    withProgress(message = "Running calculations...", value = 0, {
      
      # Process each of the sheets in ATDSheets and pass them to the process sheet function.
      for (i in seq_along(ATDSheets)) {
        incProgress(1 / length(ATDSheets), detail = paste("Processing:", dataAnalyteNames[i]))
        
        # Process each sheet and store its results
        result <- process_sheet(dataFilePath, ATDSheets[i], dataAnalyteNames[i], input$linearPlotDecimals, list())
        
        # Add all plots in process sheet to dataPlotsTemp
        dataPlotsTemp <- c(dataPlotsTemp, result$plotList)
        
        # Add each analyte to its own list so users can download only a single analyte if they wish
        individualAnalytesTemp[[dataAnalyteNames[i]]] <-  result$plotList
        
        # Calculate CCS, render the plots and add them to the full and individual plot lists
        if(input$knownCCSValues == "Yes") {
          knownCCSTp1Result <- calculate_ccs_with_comparison(dataAnalyteNames[i], result$driftTimes$tp1, dataCCSValues[i], singlePassEquation, TRUE, input$CCSDecimals)
          knownCCSTppResult <- calculate_ccs_with_comparison(dataAnalyteNames[i], result$driftTimes$tpp, dataCCSValues[i], multiPassEquation, FALSE, input$CCSDecimals)
          
          dataPlotsTemp <- c(dataPlotsTemp, knownCCSTp1Result$plotObject[1])
          individualAnalytesTemp[[dataAnalyteNames[i]]] <- c(individualAnalytesTemp[[dataAnalyteNames[i]]],knownCCSTp1Result$plotObject[1])
          
          dataPlotsTemp <- c(dataPlotsTemp, knownCCSTppResult$plotObject[1])
          individualAnalytesTemp[[dataAnalyteNames[i]]] <-  c(individualAnalytesTemp[[dataAnalyteNames[i]]],knownCCSTppResult$plotObject[1])
          
          # Directly assign calculated CCS values to the respective vectors
          dataTp1[i] <- knownCCSTp1Result$calculatedCCS
          dataTpp[i] <- knownCCSTppResult$calculatedCCS
          
          # Assign percent differences
          percentDifferenceTp1[i] <- knownCCSTp1Result$percentDifferences
          percentDifferenceTpp[i] <- knownCCSTppResult$percentDifferences
        } 
        else {
          unknownCCSTp1Result <- calculate_ccs_without_comparison(dataAnalyteNames[i], result$driftTimes$tp1, singlePassEquation, TRUE, input$CCSDecimals)
          unknownCCSTppResult <- calculate_ccs_without_comparison(dataAnalyteNames[i], result$driftTimes$tpp, multiPassEquation, FALSE, input$CCSDecimals)
          
          dataPlotsTemp <- c(dataPlotsTemp, unknownCCSTp1Result$plotObject[1])
          individualAnalytesTemp[[dataAnalyteNames[i]]] <- c(individualAnalytesTemp[[dataAnalyteNames[i]]],unknownCCSTp1Result$plotObject[1])
          
          dataPlotsTemp <- c(dataPlotsTemp, unknownCCSTppResult$plotObject[1])
          individualAnalytesTemp[[dataAnalyteNames[i]]] <- c(individualAnalytesTemp[[dataAnalyteNames[i]]],unknownCCSTppResult$plotObject[1])
          
          # Directly assign calculated CCS values to the respective vectors
          dataTp1[i] <- unknownCCSTp1Result$calculatedCCS
          dataTpp[i] <- unknownCCSTppResult$calculatedCCS
        }
      }
      
      # Update reactivePlots with the accumulated dataPlotsTemp
      reactivePlots$allProcessDataPlots <- dataPlotsTemp
      
      # Generate the CCS results table
      if (input$knownCCSValues == "Yes") {
        CCSResultsDT <- data.frame(
          analyte = dataAnalyteNames,
          acceptedCCS = format(round(dataCCSValues, input$CCSDecimals), nsmall = input$CCSDecimals),
          singlePassCCS = format(round(dataTp1, input$CCSDecimals), nsmall = input$CCSDecimals),
          percentDifferenceSinglePass = ifelse(is.na(percentDifferenceTp1), NA, format(round(percentDifferenceTp1, input$errorDecimals), nsmall = input$errorDecimals)),
          multiPassCCS = format(round(dataTpp, input$CCSDecimals), nsmall = input$CCSDecimals),
          percentDifferenceMultiplePass = ifelse(is.na(percentDifferenceTpp), NA, format(round(percentDifferenceTpp, input$errorDecimals), nsmall = input$errorDecimals))
        )
        customColNames <- c("Analyte", "Accepted CCS", "Single Pass CCS", "Percent Difference", "Multi Pass CCS", "Percent Difference")
      } else {
        CCSResultsDT <- data.frame(
          analyte = dataAnalyteNames,
          singlePassCCS = format(round(dataTp1, input$CCSDecimals), nsmall = input$CCSDecimals),
          multiPassCCS = format(round(dataTpp, input$CCSDecimals), nsmall = input$CCSDecimals)
        )
        customColNames <- c("Analyte", "Single Pass CCS", "Multi Pass CCS")
      }
      
      # Render the CCS results as a DT (without buttons)
      output$CCSResultsDT <- DT::renderDT({
        datatable(CCSResultsDT, colnames = customColNames, rownames = FALSE, escape = FALSE)
      })
      
      # Render the dropdown menu dynamically based on analytes
      output$analyteDropdownMenu <- renderUI({
        selectInput("selectedAnalyte", "Choose an Analyte to Download", 
                    choices = CCSResultsDT$analyte,
                    selected = CCSResultsDT$analyte[1])  # Default to the first analyte
      })
      
      # Handle download action based on the selected analyte
      observeEvent(input$selectedAnalyte, {
        sanitizedAnalyte <- sanitize_names(input$selectedAnalyte)
        
        # Define download handler for the selected analyte
        output$downloadPlot <- downloadHandler(
          filename = function() {
            paste0(sanitizedAnalyte, "_plot.pdf")  # Dynamic filename based on analyte name
          },
          content = function(file) {
            # Retrieve plot data for the selected analyte
            plotData <- individualAnalytesTemp[[input$selectedAnalyte]]
            
            if (!is.null(plotData)) {
              pdf(file, width = 6, height = 4)
              par(mar = c(5, 5, 4, 2))
              for (plotObject in plotData) {
                print(plotObject)  # Print each plot to the PDF
              }
              dev.off()
            }
          }
        )
      })
      
      
      # Update status when done
      processDataComplete(TRUE)
      output$analytesStatus <- renderText("Completed!")
      showNotification("Completed!", type = "message")
    })
  }) # End of process data observer
  
  # The next section of code is for the Multiple Peak ATD tab. 
  # Start with a file upload and sheet name observers and user sheet and column information. 
  # Read the uploaded file and process the data
  observeEvent(input$usersATDFile, {
    req(input$usersATDFile)
    
    # Get the sheet names from the uploaded file. Data is not loaded until user hits plots Raw Data.
    sheets <- excel_sheets(input$usersATDFile$datapath)
    
    # Update sheet name dropdown menu
    output$sheetNameUI <- renderUI({
      selectInput("sheetName", "Sheet Name:", choices = sheets)
    })
  })
  
  # Sheet selection and load data when Plot Raw Data is clicked.
  # Plot Raw Data will do nothing until data is loaded into the app.
  observeEvent(input$plotRawData, {
    req(input$usersATDFile, input$sheetName, input$startRow, input$startColumn)
    
    # Read the data from the selected sheet and the specified range.
    # First column is arrival time and second is intensity. 
    start_col <- input$startColumn
    end_col <- start_col + 1
    
    data <- read_xlsx(input$usersATDFile$datapath, 
                      sheet = input$sheetName, 
                      range = cell_limits(c(input$startRow, start_col), c(NA, end_col)))
    
    # Store the data reactively so it can be easily changed in the app. 
    multiplePeakReactive(data)
    
    # Get the arrival times and intensities from the selected columns
    arrivalTimes <- data[[1]]
    intensities <- data[[2]]  
    
    # Throw an error if there are missing values this will prevent an error from plotting.
    if (any(is.na(arrivalTimes)) | any(is.na(intensities))) {
      errorMessage <- paste("Error: This data appears to have missing values or may contain row or column headers that are non-numeric.")
      output$calibrationStatus <- renderText(errorMessage)
      showNotification(errorMessage, type = "error")
      return()
    }
    
    # Check lengths of arrivalTimes and intensities so that they are the same. 
    if (length(arrivalTimes) != length(intensities)) {
      errorMessage <- paste("Error: Mismatch in the number of rows between arrival times and intensities.")
      showNotification(errorMessage, type = "error")
      return()
    }
    
    # Create raw data plot (Top plot in the Multiple Peak ATD tab)
    output$rawDataPlot <- renderPlot({
      startTick <- floor(min(arrivalTimes) / 0.5) * 0.5
      
      par(
        mar = c(5, 6, 2, 2),            
        cex.lab = 1.6,                   
        cex.axis = 1.6,                  
        las = 1                          
      )
      
      plot(
        arrivalTimes, intensities, 
        type = "l", col = "black", 
        xlab = "Arrival Time (ms)",
        ylab = "",
        xaxt = "n"                      
      )
      
      axis(1, at = seq(startTick, max(arrivalTimes), by = 1.0), cex.axis = 1.6)
    })
  })
  
  # Reactive value to store the number of peaks
  numPeaks <- reactiveVal(1)
  
  # When the "Add More Peaks" button is clicked, increase the number of peaks
  observeEvent(input$generatePeaks, {
    if(numPeaks() < 10) {
      numPeaks(numPeaks() + 1)
      
    }else{
      errorMessage <- paste("Error: The maximum number of peaks (10) has been reached.")
      showNotification(errorMessage, type = "error")
      return()
    }
  })
  
  # Reactive value for deleting a peak
  observeEvent(input$deleteLastpeak, {
    if(numPeaks() > 1) {
      numPeaks(numPeaks() - 1)
    }
  })
  
  # Dynamically generate input fields based on the number of peaks
  output$generateFittedPlots <- renderUI({
    peakInputs <- list()
    
    # Loop to generate a set of inputs for each peak
    for (i in 1:numPeaks()) {
      peakInputs[[i]] <- fluidRow(
        column(3, numericInput(paste0("userAmplitude", i), 
                               paste("Peak", i, "Intensity Guess"), 10000, min = 1000)),
        column(3, numericInput(paste0("userArrivalTime", i), 
                               paste("Peak", i, "μ Guess"), 10, min = 10)),
        column(3, numericInput(paste0("userStd", i), 
                               paste("Peak", i, "σ Guess"), 0.1, min = 0))
      )
    }
    
    # Return the list of UI elements
    do.call(tagList, peakInputs)
  })
  
  # Observe event for generating fitted Gaussian distributions to multiple peaks
  observeEvent(input$generate, {
    data <- multiplePeakReactive()
    arrivalTimes <- data[[1]]
    intensities <- data[[2]]
    
    # Create and fill a list with all the guess information for the peaks.
    peakGuesses <- list()
    for (i in 1:numPeaks()) {
      peakGuesses[[i]] <- list(
        amplitude = input[[paste0("userAmplitude", i)]],
        arrivalTime = input[[paste0("userArrivalTime", i)]],
        stdDev = input[[paste0("userStd", i)]]
      )
    }
    
    # Generate models for each peak and store models
    models <- calculate_multiple_arrival_times(arrivalTimes, intensities, peakGuesses)
    modelsReactive(models)
    
    # Debugging print statement to make sure models are being generated.
    #print(models)  
    
    # Render the plot
    output$userATD <- renderPlot({
      plot_multiple_gaussians(arrivalTimes, intensities, models, 
                              input$ATDDecimals,
                              highlighted_peak = as.numeric(input$peakSelection),
                              sigmaRange = input$sigmaRange)
    })
  })
  # Trigger a plot update anytime the selected peak or sigmaRange is changed.
  # Prevents the user from having to keep pressing generate.
  observeEvent(c(input$peakSelection, input$sigmaRange), {
    models <- modelsReactive()
    data <- multiplePeakReactive()
    
    # Safely check that everything is defined before trying to plot
    if (!is.null(models) && !is.null(data) && 
        !is.null(input$peakSelection) && !is.null(input$sigmaRange)) {
      
      selectedPeak <- as.numeric(input$peakSelection)
      
      # Additional safety check in case selectedPeak is NA or invalid
      if (!is.na(selectedPeak) && length(models) >= selectedPeak && selectedPeak > 0) {
        arrivalTimes <- data[[1]]
        intensities <- data[[2]]
        
        output$userATD <- renderPlot({
          plot_multiple_gaussians(arrivalTimes, intensities, models, 
                                  input$ATDDecimals,
                                  highlighted_peak = selectedPeak,
                                  sigmaRange = input$sigmaRange)
        })
      }
    }
  })
  # Observer for copying reconstructed ATDs.
  # Fits ATDs using user specified guess values and retains values within a specified standard deviation.
  # Points outside of the specified standard deviation are reconstructed using the fitted distribution.
  observeEvent(input$copytoClipboard, {
    if (interactive()) {
      models <- modelsReactive()
      data <- multiplePeakReactive()
      selectedPeak <- as.numeric(input$peakSelection)
      
      if (is.null(models) || length(models) < selectedPeak || is.null(models[[selectedPeak]])) {
        showNotification("Invalid peak selection or models are not generated.", type = "error")
        return()
      }
      
      model <- models[[selectedPeak]]
      arrivalTimes <- data[[1]]
      intensities <- data[[2]]
      
      # Automatically determine min and max arrival times for plotting
      arrivalTimeMin <- min(arrivalTimes)
      arrivalTimeMax <- max(arrivalTimes)
      
      mu <- coef(model)["mu"]
      sigma <- coef(model)["sigma"]
      A <- coef(model)["A"]
      
      # Define the region of raw data to keep in the distribution
      lowerBound <- mu - (input$sigmaRange * sigma)
      upperBound <- mu + (input$sigmaRange * sigma)
      
      # Allow users to extract values based on standard deviation from mu.
      filteredIndices <- which((arrivalTimes >= lowerBound) & (arrivalTimes <= upperBound))
      filteredTimes <- arrivalTimes[filteredIndices]
      filteredIntensities <- intensities[filteredIndices]
      
      # Locate the maximum intensity value
      maxIndex <- which.max(filteredIntensities)
      
      # Initialize vectors to store the final decreasing values
      finalTimes <- c(filteredTimes[maxIndex])
      finalIntensities <- c(filteredIntensities[maxIndex])
      
      # Process left side (before the peak)
      for (i in (maxIndex - 1):1) {
        finalTimes <- c(finalTimes, filteredTimes[i])
        finalIntensities <- c(finalIntensities, filteredIntensities[i])
      }
      
      # Process right side (after the peak)
      for (i in (maxIndex + 1):length(filteredIntensities)){
        finalTimes <- c(finalTimes, filteredTimes[i])
        finalIntensities <- c(finalIntensities, filteredIntensities[i])
      }
      
      # Generate time and intensity values outside the kept values
      generatedTimesLeft <- seq(arrivalTimeMin, lowerBound, length.out = 50)
      generatedTimesRight <- seq(upperBound, arrivalTimeMax, length.out = 50)
      
      # Combine and generate intensities
      generatedTimes <- c(generatedTimesLeft, generatedTimesRight)
      generatedIntensities <- A * exp(-((generatedTimes - mu)^2) / (2 * sigma^2))
      
      # Combine original decreasing points with generated points
      combinedTimes <- c(finalTimes, generatedTimes)
      combinedIntensities <- c(finalIntensities, generatedIntensities)
      
      # Replace negative intensity values with zero
      combinedIntensities[combinedIntensities < 0] <- 0
      
      # Round small values to zero
      combinedIntensities[combinedIntensities < 1e-4] <- 0
      
      # Remove scientific notation
      result <- data.frame(
        arrivalTimeCombinedData = format(combinedTimes, scientific = FALSE),
        Intensity = format(combinedIntensities, scientific = FALSE)
      )
      
      # Sort by Arrival Time
      result <- result[order(as.numeric(result$arrivalTimeCombinedData)), ]
      
      # Remove headers
      resultHeaderless <- paste(as.character(result[, 1]), as.character(result[, 2]), sep = "\t")
      
      # Copy data to clipboard without row numbers
      clipr::write_clip(resultHeaderless, row.names = FALSE)
      
      showNotification("Filtered and generated peak data copied to clipboard in order!", type = "message")
    }
  })
  
  observeEvent(modelsReactive(), {
    models <- modelsReactive()
    
    if (!is.null(models) && length(models) > 0) {
      peakLabels <- paste("Peak", seq_along(models))
      
      updateSelectInput(session, "peakSelection", 
                        choices = setNames(seq_along(models), peakLabels))
    } else {
      updateSelectInput(session, "peakSelection", choices = NULL)
    }
  })
  
  # Rendering of Dynamic Download Buttons and Textboxes----
  # Starting with the download buttons for the example data.
  # Example calibration file.
  output$exampleCalibrationUI <- renderUI({
    downloadButton("exampleCalibrationFile", "Calibration Example")
  })
  # Example experimental file
  output$exampleExperimentUI <- renderUI({
    downloadButton("exampleExperimentFile", "Experiment Example")
  })
  output$exampleMultiPeakATDUI <- renderUI({
    downloadButton("exampleMultiPeakATDFile", "Separation Example")
  })
  
  # The download calibration curves button with rendering when calibration is complete.
  output$disabledDownloadCalibrationCurves <- renderUI({
    if (!calibrationComplete()) {
      downloadButton("downloadCalibrationCurves", "Download Curves", disabled = TRUE)
    } else {
      downloadButton("downloadCalibrationCurves", "Download Curves")
    }
  })
  
  # The all plots button on the create calibration curve tab with rendering when calibration is complete.
  output$disabledDownloadAllCalibrationPlots <- renderUI({
    if (!calibrationComplete()) {
      downloadButton("downloadAllCalibrationPlots", "Download All Plots", disabled = TRUE)
    } else {
      downloadButton("downloadAllCalibrationPlots", "Download All Plots")
    }
  })
  
  # Textbox area for entering known CCS values which only appears if the user has data to enter. 
  output$disabledDataCCSValues <- renderUI({
    if (input$knownCCSValues == "No") {
      return(NULL)
    } else {
      textAreaInput("processDataCCSTheoretical", "Theoretical CCS Values (comma separated): ", "")
    }
  })
  
  # The all plots button for the Process Data tab with rendering when data processing is complete. 
  output$disabledAllDataPlots <- renderUI({
    if (!processDataComplete()) {
      downloadButton("downloadAllDataPlots", "Download All Plots", disabled = TRUE)
    } else {
      downloadButton("downloadAllDataPlots", "Download All Plots", diabled = FALSE)
    }
  })
  
  # Hide the individual button download for dataprocessing until the run is complete.
  output$disabledDownloadIndividualPlots <- renderUI({
    if (processDataComplete()) {
      # Show the download button after the data is processed
      downloadButton("downloadPlot", "Selected Plots")
    } else {
      # Hide the download button until the data is processed
      return(NULL)
    }
  }) # End rendering of dynamic buttons and text boxes
  
  # Download Button Handlers ----
  # Create download handlers for example files
  output$exampleCalibrationFile <- downloadHandler(
    filename = function() {
      paste("MajorMix Calibration Curve Example", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("www/MajorMix Calibration Curve Example.xlsx", file)
    }
  )
  # Download handler for example experiment file
  output$exampleExperimentFile <- downloadHandler(
    filename = function() {
      paste("SPLASH Lipid Standards Example", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("www/SPLASH Lipids Example.xlsx", file)
    }
  )
  output$exampleMultiPeakATDFile <- downloadHandler(
    filename = function() {
      paste("Prostaglandins Multipeak ATD Example", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("www/Multiple Peak ATD Example.xlsx", file)
    }
  )
  # Download handler for all plots in the calibration curves tab.
  output$downloadAllCalibrationPlots <- downloadHandler(
    filename = function() {
      paste("All_Calibration_Plots_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 6, height = 4)
      
      # Print linear calibration plots
      if (!is.null(reactivePlots$allCalibrationPlots)) {
        for (plotObject in reactivePlots$allCalibrationPlots) {
          print(plotObject)
        }
      }
      
      # Print power curve plots
      if (!is.null(reactivePlots$powerCurvePlots)) {
        for (plotObject in reactivePlots$powerCurvePlots) {
          print(plotObject)
        }
      }
      
      dev.off()
    }
  )
  
  # Download handler for just the calibration curves
  output$downloadCalibrationCurves <- downloadHandler(
    filename = function() {
      paste("Calibration_Curves_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 6, height = 6)
      
      if (!is.null(reactivePlots$powerCurvePlots)) {
        for (plotObject in reactivePlots$powerCurvePlots) {
          print(plotObject)
        }
      }
      
      dev.off()
    }
  )
  
  # Download handler for all plots in the data processing tab
  output$downloadAllDataPlots <- downloadHandler(
    filename = function() {
      paste("All_Analytes_Plots_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 6, height = 4)
      par(mar = c(5, 5, 4, 2)) 
      if (!is.null(reactivePlots$allProcessDataPlots)) {
        for (plotObject in reactivePlots$allProcessDataPlots) {
          print(plotObject)
        }
      }
      
      dev.off()
    }
  ) # End of download button handlers.
}# End of server
#####End of Defining Server#####