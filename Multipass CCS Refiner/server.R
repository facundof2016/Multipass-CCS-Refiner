# This file contains the server for Multipass CCS Refiner
#library(readxl)
#library(xlsx)
#library(ggplot2) 
#library(grid)
#library(DT)
source("functions.R")

`%||%` <- function(a, b) if (!is.null(a)) a else b

##### Define server function #####
server <- function(input, output, session) {
  # Visibility toggles for intro of the UI ----
  observeEvent(input$toggle_announcements, {
    shinyjs::toggle("announcements_content")
  })
  observeEvent(input$toggle_background, {
    toggle("background_content")  
  })
  observeEvent(input$toggle_using_app, {
    toggle("using_app_content")  
  })
  observeEvent(input$toggle_citations, {
    toggle("citations_content")  
  })# End visibility toggle creation.
  
  # Default Calibration Information
  calibrants_majorMix_pos <- data.frame(
    name = c(
      "Sulfadimethoxide","Val-Tyr-Val","Terfenadine","Polyalanine n7",
      "Leucine Enkephalin","Polyalanine n8","Reserpine","Polyalanine n9",
      "Polyalanine n10","Polyalanine n11","Polyalanine n12","Polyalanine n13",
      "Polyalanine n14","Polyalanine n15","Polyalanine n16"
    ),
    formula = c(
      "C12H14N4O4S","C19H29N3O5","C32H41NO2","C21H37N7O8",
      "C28H37N5O7","C24H42N8O9","C33H40N2O9","C27H47N9O10",
      "C30H52N10O11","C33H57N11O12","C36H62N12O13","C39H67N13O14",
      "C42H72N14O15","C45H77N15O16","C48H82N16O17"
    ),
    CCS = c(
      168.4,191.7,228.7,211.0,229.8,228.0,252.3,243.0,
      256.0,271.0,282.0,294.0,306.0,321.5,333.6
    ),
    mz = c(
      311.0808521,380.2179975,472.321006,516.2776377,
      556.276575,587.3147515,609.2806573,658.3518652,
      729.388979,800.4260928,871.4632066,942.5003204,
      1013.537434,1084.574548,1155.611662
    ),
    MW = c(
      310.0736,379.2107,471.3137,515.2703,
      555.2693,586.3074,608.2734,657.3445,
      728.3816,799.4187,870.4559,941.4930,
      1012.5301,1083.5672,1154.6043
    ),
    z = rep(1, 15)
  )
  calibrants_majorMix_neg <- data.frame(
    name = c(
      "Sulfadimethoxide","Val-Tyr-Val", "Leucine Enkephalin","Polyalanine n8","Reserpine",
      "Polyalanine n9","Polyalanine n10","Polyalanine n11","Polyalanine n12","Polyalanine n13",
      "Polyalanine n14","Polyalanine n15"
    ),
    formula = c(
      "C12H14N4O4S","C19H29N3O5","C28H37N5O7","C24H42N8O9","C33H40N2O9","C27H47N9O1","C30H52N10O11",
      "C33H57N11O12","C36H62N12O13","C39H67N13O14","C42H72N14O15","C45H77N15O16"
    ),
    CCS = c(
      170.1,192.5,225.3,227.7,265.2,242.1,255.9,268.5,280.2,294.6,308.8,322.4
    ),
    mz = c(
      309.065751,378.202896,554.261474,585.3001985,607.265556,656.3373123,
      727.3744261,798.4115399,869.4486537,940.4857675,1011.522881,1082.559995
    ),
    MW = c(
      310.0736,379.2107,555.2693,586.3074,608.2734,657.3445,
      728.3816,799.4187,870.4559,941.4930,1012.5301,1083.5672
    ),
    z = rep(1, 12)
  )
  calibrants_spherical <- data.frame(
    name = c(
      "PFS-1022 [Sodiated]", "PFS-2012 [Protonated]", "PFS-2012 [Sodiated]", "PFS-1032 [Protonated]", 
      "PFS-1032 [Sodiated]", "PFS-1042 [Protonated]", "PFS-1042 [Sodiated]", "PFS-1052 [Protonated]",
      "PFS-1052 [Sodiated]","PFS-3012 [Sodiated]", "PFS-1062 [Protonated]", "PFS-1062 [Sodiated]",
      "PFS-2032 [Protonated]", "PFS-2032 [Sodiated]"
    ),
    formula = c(
      "C12H22O8","C17H30O10","C17H30O10","C20H36O12","C20H36O12","C25H44O16",
      "C25H44O16","C30H52O20","C30H52O20","C37H62O22","C40H70O25",
      "C40H70O25","C50H84O30","C50H84O30"
      ),
    CCS = c(
      160.65, 180.88, 185.39, 197.50,
      198.51, 218.56, 220.65, 245.51,
      246.40, 270.24, 284.51, 287.23,
      315.77, 317.41
    ),
    mz = c(
      317.121, 395.191, 417.173, 469.228, 
      491.210, 601.270, 623.252, 733.312,
      755.294, 881.362, 951.428, 973.410,
      1165.512, 1187.494
    ),
    MW = c(
      294.131, 394.184, 394.184, 468.221,
      468.221, 600.263, 600.263, 732.305,
      732.305, 858.373, 950.451, 950.451,
      1164.505, 1164.505
    ),
    z = rep(1, 14),
    adduct = c(
      "[M + Na]<sup>+</sup>", "[M + H]<sup>+</sup>",
      "[M + Na]<sup>+</sup>", "[M + H]<sup>+</sup>",
      "[M + Na]<sup>+</sup>", "[M + H]<sup>+</sup>",
      "[M + Na]<sup>+</sup>", "[M + H]<sup>+</sup>",
      "[M + Na]<sup>+</sup>", "[M + Na]<sup>+</sup>",
      "[M + H]<sup>+</sup>", "[M + Na]<sup>+</sup>",
      "[M + H]<sup>+</sup>", "[M + Na]<sup>+</sup>"
    )
  )
  calibrantTables <- list(
    major_pos = calibrants_majorMix_pos,
    major_neg = calibrants_majorMix_neg,
    spherical = calibrants_spherical
  )
  
  attr(calibrantTables$major_pos, "ion_mode") <- "Positive"
  attr(calibrantTables$major_neg, "ion_mode") <- "Negative"
  attr(calibrantTables$spherical, "ion_mode") <- "Positive"
  # Format reference tables
  formatCalibrantTable <- function(df) {
    
    # Format numeric columns only if they exist
    if ("mz" %in% names(df)) {
      df$mz <- sprintf("%.4f", df$mz)
    }
    
    if ("MW" %in% names(df)) {
      df$MW <- sprintf("%.4f", df$MW)
    }
    
    if ("CCS" %in% names(df)) {
      df$CCS <- sprintf("%.2f", df$CCS)
    }
    
    if ("z" %in% names(df)) {
      df$z <- as.integer(df$z)
    }
    
    # Rename columns only if they exist
    names(df)[names(df) == "name"]    <- "Name"
    names(df)[names(df) == "formula"] <- "Formula"
    names(df)[names(df) == "mz"]      <- "m/z"
    names(df)[names(df) == "MW"]      <- "Monoisotopic Mass (Da)"
    names(df)[names(df) == "adduct"]  <- "Adduct"
    
    df
  }
  # Render Tables to the Calibrant Reference Tables Tab.
  output$calibrantMajorMixPos <- DT::renderDT({
    DT::datatable(
      formatCalibrantTable(calibrants_majorMix_pos),
      escape = FALSE,
      rownames = FALSE,
      options = list(pageLength = 16)
    )
  })
  
  output$calibrantMajorMixNeg <- DT::renderDT({
    DT::datatable(
      formatCalibrantTable(calibrants_majorMix_neg),
      escape = FALSE,
      rownames = FALSE,
      options = list(pageLength = 16)
    )
  })
  
  output$calibrantSpherical <- DT::renderDT({
    DT::datatable(
      formatCalibrantTable(calibrants_spherical),
      escape = FALSE,
      rownames = FALSE,
      options = list(pageLength = 14)
    )
  })
  # Create reactive values ----
  # All reactive values will change whenever the user changes them in the GUI.
  # Values to store files which change upon file upload.
  # File uploads
  calibrationFile <- reactiveVal(NULL)
  unknownFile     <- reactiveVal(NULL)
  
  # Calibration equations (Xia)
  singlePassEquationCurve1 <- reactiveVal(NULL)
  multiPassEquationCurve1  <- reactiveVal(NULL)
  
  singlePassEquationCurve2 <- reactiveVal(NULL)
  multiPassEquationCurve2  <- reactiveVal(NULL)
  
  singlePassEquationCurve3 <- reactiveVal(NULL)
  multiPassEquationCurve3  <- reactiveVal(NULL)
  
  # Curve expressions
  singlePassCurveExpression <- reactiveVal()
  multiPassCurveExpression  <- reactiveVal()
  
  # Completion flags
  calibrationComplete <- reactiveVal(FALSE)
  processDataComplete <- reactiveVal(FALSE)
  
  # Saved-state flags
  savedCal1 <- reactiveVal(FALSE)
  savedCal2 <- reactiveVal(FALSE)
  savedCal3 <- reactiveVal(FALSE)
  
  # Global plot storage
  reactivePlots <- reactiveValues(
    allCalibrationPlots = list(),
    powerCurvePlots     = list(),
    allProcessDataPlots = list()
  )
  
  # Ruotolo reactive storage (computed each run)
  ruotoloA   <- reactiveVal(NULL)
  ruotoloX   <- reactiveVal(NULL)
  ruotoloGasRV <- reactiveVal(NULL)
  ruotoloCRV   <- reactiveVal(NULL)
  ruotoloMZRV  <- reactiveVal(NULL)
  ruotoloMWRV  <- reactiveVal(NULL)
  ruotoloZRV   <- reactiveVal(NULL)
  ruotoloCCSRV <- reactiveVal(NULL)
  ruotoloPlotRV <- reactiveVal(NULL)
  ruotoloSummaryRV <- reactiveVal(NULL)
  
  # Master storage for saved curves
  calibrationInfo <- reactiveValues(
    curve1 = NULL,
    curve2 = NULL,
    curve3 = NULL
  )
  
  # ---- Multiple Peak ATD: Resolution storage ----
  peakMetricsRV     <- reactiveVal(NULL)   # per-peak metrics table
  rppRV             <- reactiveVal(NULL)   # pairwise Rpp table
  
  # Reactive object for multiple arrival time distributions and nls models generated from them
  multiplePeakReactive <- reactiveVal(NULL)
  modelsReactive <- reactiveVal(list())
  
  # Does the currently selected curve have Ruotolo calibration?
  output$curveHasRuotolo <- reactive({
    
    slot <- switch(input$curveSlot,
                   "Curve 1" = "curve1",
                   "Curve 2" = "curve2",
                   "Curve 3" = "curve3")
    
    info <- calibrationInfo[[slot]]
    
    # TRUE when both A and X exist
    !is.null(info$ruotolo_A) && !is.null(info$ruotolo_X)
  })
  
  outputOptions(output, "curveHasRuotolo", suspendWhenHidden = FALSE)
  
  # The code below is for features not released in the application
  # currentCalibration <- reactive({
  #   
  #   cat("ENTER currentCalibration()\n")
  #   
  #   # 1. calibrationComplete
  #   cat("calibrationComplete():", calibrationComplete(), "\n")
  #   if (!hasCalibration()) {
  #     cat("RETURN NULL: no calibration coefficients available\n")
  #     return(NULL)
  #   }
  #   
  #   
  #   # 2. calibration method
  #   cat("input$calibrationMethod:", input$calibrationMethod, "\n")
  #   
  #   # ---- Xia branch ----
  #   if (grepl("Xia", input$calibrationMethod) &&
  #       !grepl("Ruotolo", input$calibrationMethod)) {
  #     
  #     cat("ENTER Xia branch\n")
  #     
  #     expr <- multiPassCurveExpression()
  #     cat("multiPassCurveExpression is NULL:", is.null(expr), "\n")
  #     
  #     if (is.null(expr)) {
  #       cat("RETURN NULL: curve expression is NULL\n")
  #       return(NULL)
  #     }
  #     
  #     coeffs <- extract_power_coefficients(expr)
  #     cat("Extracted a, b:", coeffs$a, coeffs$b, "\n")
  #     
  #     return(list(
  #       method = "Xia",
  #       a = coeffs$a,
  #       b = coeffs$b
  #     ))
  #   }
  #   
  #   # ---- Ruotolo branch ----
  #   if (grepl("Ruotolo", input$calibrationMethod)) {
  #     
  #     cat("ENTER Ruotolo branch\n")
  #     
  #     if (is.null(ruotoloA()) || is.null(ruotoloX())) {
  #       cat("RETURN NULL: Ruotolo coefficients missing\n")
  #       return(NULL)
  #     }
  #     
  #     return(list(
  #       method  = "Ruotolo",
  #       A       = ruotoloA(),
  #       X       = ruotoloX(),
  #       mz      = ruotoloMZRV(),
  #       MW      = ruotoloMWRV(),
  #       z       = ruotoloZRV(),
  #       gas     = ruotoloGasRV(),
  #       c_param = ruotoloCRV()
  #     ))
  #   }
  #   
  #   cat("RETURN NULL: No matching calibration method\n")
  #   NULL
  # })
  # 
  # 
  # ccsRppRV <- reactive({
  #   
  #   cal <- currentCalibration()
  #   df  <- peakMetricsRV()
  #   
  #   if (is.null(cal) || is.null(df) || nrow(df) < 2) return(NULL)
  #   
  #   metrics <- lapply(seq_len(nrow(df)), function(i) {
  #     compute_peak_metrics_ccs(df$mu_ms[i], df$FWHM_ms[i], cal)
  #   })
  #   
  #   out <- list(); k <- 1
  #   for (i in 1:(length(metrics) - 1)) {
  #     for (j in (i + 1):length(metrics)) {
  #       
  #       A <- metrics[[i]]
  #       B <- metrics[[j]]
  #       if (is.null(A) || is.null(B)) next
  #       
  #       out[[k]] <- data.frame(
  #         PeakA = i,
  #         PeakB = j,
  #         CCS_A = A$CCS,
  #         CCS_B = B$CCS,
  #         Rpp_CCS = compute_Rpp_ccs(A, B)
  #       )
  #       k <- k + 1
  #     }
  #   }
  #   
  #   do.call(rbind, out)
  # })
  # 
  # hasCalibration <- reactive({
  #   
  #   # Xia curves
  #   if (!is.null(multiPassCurveExpression())) return(TRUE)
  #   
  #   # Ruotolo
  #   if (!is.null(ruotoloA()) && !is.null(ruotoloX())) return(TRUE)
  #   
  #   FALSE
  # })
  # 
  # ccsPeakMetricsRV <- reactive({
  #   
  #   cal <- currentCalibration()
  #   df  <- peakMetricsRV()
  #   
  #   if (is.null(cal) || is.null(df)) return(NULL)
  #   
  #   out <- lapply(seq_len(nrow(df)), function(i) {
  #     m <- compute_peak_metrics_ccs(
  #       mu_t   = df$mu_ms[i],
  #       fwhm_t = df$FWHM_ms[i],
  #       cal    = cal
  #     )
  #     if (is.null(m)) return(NULL)
  #     
  #     data.frame(
  #       Peak = df$Peak[i],
  #       CCS = m$CCS,
  #       FWHM_CCS = m$FWHM_CCS,
  #       Rp_CCS = m$Rp_CCS
  #     )
  #   })
  #   
  #   do.call(rbind, out)
  # })
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
      msg <- "Error: Please complete a calibration run prior to saving."
      showNotification(msg, type = "error")
      output$calibrationStatus <- renderText(msg)
      return()
    }
    
    # Determine curve slot ("curve1", "curve2", "curve3")
    slot <- switch(input$saveToSlot,
                   "Curve 1" = "curve1",
                   "Curve 2" = "curve2",
                   "Curve 3" = "curve3")
    
    slotIndex <- substr(slot, 6, 6)
    
    # Adding the standard information if a calibrant from the refernce list was selected.
    
    # Build standard/calibrant label for saved curve metadata
    standardLabel <- if (isTRUE(input$useCalibrantMetadata)) {
      
      if (!is.null(input$selectedCalibrants) && length(input$selectedCalibrants) > 0) {
        
        calibrantSourceLabel <- switch(
          input$calibrantSource,
          "major_pos" = "Major Mix [M + H]+",
          "major_neg" = "Major Mix [M - H]-",
          "spherical" = "SpheriCal Low Mass",
          input$calibrantSource
        )
        
        paste0(
          calibrantSourceLabel,
          ": ",
          paste(input$selectedCalibrants, collapse = ", ")
        )
        
      } else {
        "Built-in calibrants selected, but names unavailable"
      }
      
    } else {
      
      if (!is.null(input$standard) && nzchar(input$standard)) {
        input$standard
      } else {
        "Not specified"
      }
    }

    # Pre-calculate dfCal for mass range and ion mode derivation
    dfCal <- if (isTRUE(input$useCalibrantMetadata)) {
      getSelectedCalibrantDF(input, calibrantTables)
    } else {
      NULL
    }
    
    # Build base save list
    saveList <- list(
      source      = input$source,
      mode = if (isTRUE(input$useCalibrantMetadata)) {
        attr(calibrantTables[[input$calibrantSource]], "ion_mode") %||% "Not specified"
      } else {
        "Not specified"
      },
      standard    = standardLabel,
      massRange = if (isTRUE(input$useCalibrantMetadata) && !is.null(dfCal) && "mz" %in% names(dfCal)) {
        paste0(round(min(dfCal$mz), 1), " - ", round(max(dfCal$mz), 1))
      } else {
        "Not specified"
      },
      twConditions = input$twConditions,
      
      singlePassEq = get(paste0("singlePassEquationCurve", slotIndex))(),
      multiPassEq  = get(paste0("multiPassEquationCurve",   slotIndex))(),
      
      plots = reactivePlots$powerCurvePlots,
      
      # Ruotolo fields (may remain NULL if not enhanced mode)
      ruotolo_A     = NULL,
      ruotolo_X     = NULL,
      ruotolo_gas   = NULL,
      ruotolo_c     = NULL,
      ruotolo_mz    = NULL,
      ruotolo_MW    = NULL,
      ruotolo_z     = NULL,
      ruotolo_CCS   = NULL,
      ruotolo_plot  = NULL,
      ruotolo_summary = NULL
    )
    
    # Fill Ruotolo info if enhanced calibration
    if (input$calibrationMethod == "Xia + Bush & Ruotolo") {
      
      saveList$ruotolo_A       <- ruotoloA()
      saveList$ruotolo_X       <- ruotoloX()
      saveList$ruotolo_gas     <- ruotoloGasRV()
      saveList$ruotolo_c       <- ruotoloCRV()
      saveList$ruotolo_mz      <- ruotoloMZRV()
      saveList$ruotolo_MW      <- ruotoloMWRV()
      saveList$ruotolo_z       <- ruotoloZRV()
      saveList$ruotolo_CCS     <- ruotoloCCSRV()
      saveList$ruotolo_plot    <- ruotoloPlotRV()
      saveList$ruotolo_summary <- ruotoloSummaryRV()
    }
    
    # Save to master storage
    calibrationInfo[[slot]] <- saveList
    
    # Mark saved
    if (slot == "curve1") savedCal1(TRUE)
    if (slot == "curve2") savedCal2(TRUE)
    if (slot == "curve3") savedCal3(TRUE)
    
    # Render Xia curves
    output[[paste0("singlePassCalibrationCurve", slotIndex)]] <- renderPlot({
      req(saveList$plots$Single)
      saveList$plots$Single
    })
    output[[paste0("multiPassCalibrationCurve", slotIndex)]] <- renderPlot({
      req(saveList$plots$Multi)
      saveList$plots$Multi
    })
    
    # Render Ruotolo if present
    output[[paste0("ruotoloPlot", slotIndex)]] <- renderPlot({
      req(saveList$ruotolo_plot)
      saveList$ruotolo_plot
    })
    output[[paste0("ruotoloSummary", slotIndex)]] <- renderTable({
      req(saveList$ruotolo_summary)
      saveList$ruotolo_summary
    })
    
    showNotification(
      paste("Calibration saved to", input$saveToSlot),
      type = "message"
    )
    output$calibrationStatus <- renderText("Plot Saved!")
  }) # End save button observer
  
  
  # Calibration Curves Save Tabs Rendering ----
  # This code is executed with the save button observer to show results in the GUI.
  # ================================================================
  #     SAVED CALIBRATION CURVE RENDERING (CURVES 1, 2, 3)
  # ================================================================
  
  # Helper for Ruotolo info text
  buildRuotoloInfo <- function(info) {
    if (is.null(info$ruotolo_A) || is.null(info$ruotolo_X)) {
      return("Bush & Ruotolo: Not available for this curve.")
    }
    
    paste0(
      "Bush & Ruotolo Calibration: Ω' = ",
      signif(info$ruotolo_A, 4), " · t'^{", signif(info$ruotolo_X, 4), "}  | ",
      "Gas: ", info$ruotolo_gas,
      ", c = ", signif(info$ruotolo_c, 4)
    )
  }

  # CURVE 1
  output$source1     <- renderText({ paste("Source:",      calibrationInfo$curve1$source) })
  output$mode1       <- renderText({ paste("Mode:",        calibrationInfo$curve1$mode) })
  output$standard1   <- renderText({ paste("Standard:",    calibrationInfo$curve1$standard) })
  output$massRange1  <- renderText({ paste("Mass Range:",  calibrationInfo$curve1$massRange, "m/z") })
  output$twConditions1 <- renderText({ paste("TW Conditions:", calibrationInfo$curve1$twConditions) })
  
  output$singlePassEquationCurve1 <- renderText({
    paste("Single Pass Equation:", calibrationInfo$curve1$singlePassEq)
  })
  output$multiPassEquationCurve1 <- renderText({
    paste("Multi Pass Equation:", calibrationInfo$curve1$multiPassEq)
  })
  
  # Ruotolo info
  output$ruotoloInfo1 <- renderText({ buildRuotoloInfo(calibrationInfo$curve1) })
  
  # Ruotolo plot + summary
  output$ruotoloPlot1 <- renderPlot({
    req(calibrationInfo$curve1$ruotolo_plot)
    calibrationInfo$curve1$ruotolo_plot
  })
  output$ruotoloSummary1 <- renderTable({
    req(calibrationInfo$curve1$ruotolo_summary)
    calibrationInfo$curve1$ruotolo_summary
  })
  
  output$hasRuotolo1 <- reactive({
    !is.null(calibrationInfo$curve1$ruotolo_plot)
  })
  outputOptions(output, "hasRuotolo1", suspendWhenHidden = FALSE)
  
  # CURVE 2
  output$source2     <- renderText({ paste("Source:", calibrationInfo$curve2$source) })
  output$mode2       <- renderText({ paste("Mode:",   calibrationInfo$curve2$mode) })
  output$standard2   <- renderText({ paste("Standard:", calibrationInfo$curve2$standard) })
  output$massRange2  <- renderText({ paste("Mass Range:", calibrationInfo$curve2$massRange, "m/z") })
  output$twConditions2 <- renderText({ paste("TW Conditions:", calibrationInfo$curve2$twConditions) })
  
  output$singlePassEquationCurve2 <- renderText({
    paste("Single Pass Equation:", calibrationInfo$curve2$singlePassEq)
  })
  output$multiPassEquationCurve2 <- renderText({
    paste("Multi Pass Equation:", calibrationInfo$curve2$multiPassEq)
  })
  
  output$ruotoloInfo2 <- renderText({ buildRuotoloInfo(calibrationInfo$curve2) })
  
  output$ruotoloPlot2 <- renderPlot({
    req(calibrationInfo$curve2$ruotolo_plot)
    calibrationInfo$curve2$ruotolo_plot
  })
  output$ruotoloSummary2 <- renderTable({
    req(calibrationInfo$curve2$ruotolo_summary)
    calibrationInfo$curve2$ruotolo_summary
  })
  
  output$hasRuotolo2 <- reactive({
    !is.null(calibrationInfo$curve2$ruotolo_plot)
  })
  outputOptions(output, "hasRuotolo2", suspendWhenHidden = FALSE)
  
  # CURVE 3
  output$source3     <- renderText({ paste("Source:", calibrationInfo$curve3$source) })
  output$mode3       <- renderText({ paste("Mode:",   calibrationInfo$curve3$mode) })
  output$standard3   <- renderText({ paste("Standard:", calibrationInfo$curve3$standard) })
  output$massRange3  <- renderText({ paste("Mass Range:", calibrationInfo$curve3$massRange, "m/z") })
  output$twConditions3 <- renderText({ paste("TW Conditions:", calibrationInfo$curve3$twConditions) })
  
  output$singlePassEquationCurve3 <- renderText({
    paste("Single Pass Equation:", calibrationInfo$curve3$singlePassEq)
  })
  output$multiPassEquationCurve3 <- renderText({
    paste("Multi Pass Equation:", calibrationInfo$curve3$multiPassEq)
  })
  
  output$ruotoloInfo3 <- renderText({ buildRuotoloInfo(calibrationInfo$curve3) })
  
  output$ruotoloPlot3 <- renderPlot({
    req(calibrationInfo$curve3$ruotolo_plot)
    calibrationInfo$curve3$ruotolo_plot
  })
  output$ruotoloSummary3 <- renderTable({
    req(calibrationInfo$curve3$ruotolo_summary)
    calibrationInfo$curve3$ruotolo_summary
  })
  
  output$hasRuotolo3 <- reactive({
    !is.null(calibrationInfo$curve3$ruotolo_plot)
  })
  outputOptions(output, "hasRuotolo3", suspendWhenHidden = FALSE)
  # End Calibration Curves rendering.
  
  # Import calibration RDS file 
  observeEvent(input$importCalibrationBtn, {
    
    # ---- Basic checks ----
    if (is.null(input$importCalibrationFile)) {
      showModal(modalDialog(
        title = "No file selected",
        "Please upload a .rds calibration file before importing.",
        easyClose = TRUE
      ))
      return()
    }
    
    # ---- Read file safely ----
    imported_obj <- tryCatch(
      readRDS(input$importCalibrationFile$datapath),
      error = function(e) e
    )
    
    if (inherits(imported_obj, "error")) {
      showModal(modalDialog(
        title = "Import failed",
        paste("Unable to read calibration file:", imported_obj$message),
        easyClose = TRUE
      ))
      return()
    }
    
    # ---- Validate structure ----
    validation <- validate_imported_calibration(imported_obj)
    
    if (!isTRUE(validation)) {
      showModal(modalDialog(
        title = "Invalid calibration file",
        validation,
        easyClose = TRUE
      ))
      return()
    }
    
    # ---- Determine target slot ----
    slot <- switch(input$importToSlot,
                   "Curve 1" = "curve1",
                   "Curve 2" = "curve2",
                   "Curve 3" = "curve3")
    
    slotIndex <- substr(slot, 6, 6)
    
    # ---- Overwrite confirmation if needed ----
    if (!is.null(calibrationInfo[[slot]])) {
      showModal(modalDialog(
        title = paste("Overwrite", input$importToSlot, "?"),
        paste(
          input$importToSlot,
          "already contains a calibration.",
          "Do you want to overwrite it?"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirmImportCalibration", "Overwrite", class = "btn-danger")
        )
      ))
      
      # Store object temporarily
      importedCalibrationPending <<- list(
        slot = slot,
        slotIndex = slotIndex,
        payload = imported_obj$payload
      )
      
      return()
    }
    
    # ---- No overwrite needed → apply immediately ----
    apply_imported_calibration(
      slot = slot,
      slotIndex = slotIndex,
      payload = imported_obj$payload
    )
  })
  
  observeEvent(input$confirmImportCalibration, {
    
    req(importedCalibrationPending)
    
    apply_imported_calibration(
      slot      = importedCalibrationPending$slot,
      slotIndex = importedCalibrationPending$slotIndex,
      payload   = importedCalibrationPending$payload
    )
    
    importedCalibrationPending <<- NULL
    removeModal()
  })
  
  apply_imported_calibration <- function(slot, slotIndex, payload) {
    
    # ----------------------------------------------------
    # 1. Restore into calibrationInfo
    # ----------------------------------------------------
    calibrationInfo[[slot]] <<- payload
    
    # Mark slot as saved
    switch(slot,
           curve1 = savedCal1(TRUE),
           curve2 = savedCal2(TRUE),
           curve3 = savedCal3(TRUE))
    
    # ----------------------------------------------------
    # 2. Re-ADD CCS-CRITICAL REACTIVES
    # ----------------------------------------------------
    
    ## Xia / power-law calibration
    if (!is.null(payload$multiPassEq)) {
      
      # Restore equation strings (you already rely on these elsewhere)
      multiPassEquationCurve1(payload$multiPassEq)
      singlePassEquationCurve1(payload$singlePassEq)
      
      # Convert equation string -> curveExpression
      mp_expr <- equation_string_to_curve_expr(payload$multiPassEq)
      sp_expr <- equation_string_to_curve_expr(payload$singlePassEq)
      
      # Populate reactives
      multiPassCurveExpression(mp_expr)
      singlePassCurveExpression(sp_expr)
    }
    
    ## Ruotolo calibration (if present)
    if (!is.null(payload$ruotolo_A) && !is.null(payload$ruotolo_X)) {
      
      ruotoloA(payload$ruotolo_A)
      ruotoloX(payload$ruotolo_X)
      
      ruotoloGasRV(payload$ruotolo_gas %||% "Nitrogen")
      ruotoloCRV(payload$ruotolo_c %||% 1.41)
      
      ruotoloMZRV(payload$ruotolo_mz)
      ruotoloMWRV(payload$ruotolo_MW)
      ruotoloZRV(payload$ruotolo_z)
      ruotoloCCSRV(payload$ruotolo_CCS)
      
      if (!is.null(payload$ruotolo_plot))
        ruotoloPlotRV(payload$ruotolo_plot)
      
      if (!is.null(payload$ruotolo_summary))
        ruotoloSummaryRV(payload$ruotolo_summary)
    }
    
    # Optional but safe: mark calibration available
    calibrationComplete(TRUE)
    
    # ----------------------------------------------------
    # 3. Re-render plots
    # ----------------------------------------------------
    output[[paste0("singlePassCalibrationCurve", slotIndex)]] <- renderPlot({
      req(payload$plots$Single)
      payload$plots$Single
    })
    
    output[[paste0("multiPassCalibrationCurve", slotIndex)]] <- renderPlot({
      req(payload$plots$Multi)
      payload$plots$Multi
    })
    
    if (!is.null(payload$ruotolo_plot)) {
      output[[paste0("ruotoloPlot", slotIndex)]] <- renderPlot({
        payload$ruotolo_plot
      })
      
      output[[paste0("ruotoloSummary", slotIndex)]] <- renderTable({
        payload$ruotolo_summary
      })
    }
    
    showNotification(
      paste("Calibration imported into", toupper(slot)),
      type = "message"
    )
  }
  
  # Delete Button Observers ----
  # Code executed to clear data when delete is pressed. Reset values to starting NULL values.
  observeEvent(input$delete1, {
    calibrationInfo$curve1 <- NULL
    savedCal1(FALSE)
    
    output$singlePassCalibrationCurve1 <- renderPlot({})
    output$multiPassCalibrationCurve1  <- renderPlot({})
    output$ruotoloPlot1                <- renderPlot({})
    output$ruotoloSummary1             <- renderTable(NULL)
  })
  
  observeEvent(input$delete2, {
    calibrationInfo$curve2 <- NULL
    savedCal2(FALSE)
    
    output$singlePassCalibrationCurve2 <- renderPlot({})
    output$multiPassCalibrationCurve2  <- renderPlot({})
    output$ruotoloPlot2                <- renderPlot({})
    output$ruotoloSummary2             <- renderTable(NULL)
  })
  
  observeEvent(input$delete3, {
    calibrationInfo$curve3 <- NULL
    savedCal3(FALSE)
    
    output$singlePassCalibrationCurve3 <- renderPlot({})
    output$multiPassCalibrationCurve3  <- renderPlot({})
    output$ruotoloPlot3                <- renderPlot({})
    output$ruotoloSummary3             <- renderTable(NULL)
  })# End delete button observers.
  
  # Run Calibration Observer ----
  # This is the code that is executed when the user presses the run button in the calibration tab.
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
    
    # --- GET ANALYTE NAMES + CCS (manual or from calibrants) ---
    
    calibrationAnalyteNames <- NULL
    calibrationCCS <- NULL
    
    if (!isTRUE(input$useCalibrantMetadata)) {
      # Manual mode: validate text inputs
      if (input$calibrationAnalyteNames == "" || input$calibrationCCS == "" || 
          is.na(input$calibrationAnalyteNames) || is.na(input$calibrationCCS)) {
        errorMessage <- "Error: Enter analyte names and CCS values before running calibration, or enable 'Use Built-In Calibrant Data'."
        output$calibrationStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
      
      # Parse manual entries
      calibrationAnalyteNames <- unlist(strsplit(input$calibrationAnalyteNames, ",\\s*"))
      calibrationCCS <- as.numeric(unlist(strsplit(input$calibrationCCS, ",\\s*")))
      
    } else {
      # Auto-fill mode: get names + CCS from selected calibrants
      dfCal <- getSelectedCalibrantDF(input, calibrantTables)
      
      if (is.null(dfCal) || nrow(dfCal) == 0) {
        errorMessage <- "Error: No calibrants selected. Please select at least one calibrant or disable 'Use Built-In Calibrant Data'."
        output$calibrationStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
      
      calibrationAnalyteNames <- dfCal$name
      calibrationCCS <- dfCal$CCS
    }
    
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
      errorMessage <- paste("Error: The number of calibrants entered is different than the number of processable excel sheets. Start tab names with Analyte. Please see the supporting information for an example document.")
      output$calibrationStatus <- renderText(errorMessage)
      showNotification(errorMessage, type = "error")
      return()
    }
    
    # Create an error checker to ensure separation times in the file are numeric and start with the bypass and single pass ATDs.
    for (i in seq_along(ATDSheets)){
      separationTimesRaw <- readxl::read_excel(calibrationFilePath, sheet = ATDSheets[i], col_names = FALSE, col_types = "numeric", n_max = 1)
      separationTimes <- as.numeric(separationTimesRaw[1, ][!is.na(separationTimesRaw[1, ])])
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
      p_single <- resultSingle$p
      
      resultMulti <- plot_power_curve(calibrationCCS, allTpp, "Multi", input$powerCurveDecimals)
      multiPassCurveExpression(resultMulti$curveExpression)
      multiPassEquationCurve1(resultMulti$equation)
      multiPassEquationCurve2(resultMulti$equation)
      multiPassEquationCurve3(resultMulti$equation)
      output$MultiPassCalibrationCurve <- renderPlot({ resultMulti$p })
      p_multi <- resultMulti$p
      
      # --------------------------------------------------------------
      # --- BUSH & RUOTOLO CALIBRATION (only when selected) ---
      # --------------------------------------------------------------
      if (input$calibrationMethod == "Xia + Bush & Ruotolo") {
        cat("----- ENTER RUOTOLO BLOCK -----\n")
        # 1) Prepare metadata
        ruotolo_meta <- prepare_ruotolo_metadata(
          calibrantSource    = input$calibrantSource,
          selectedCalibrants = input$selectedCalibrants,
          useBuiltIn         = input$useCalibrantMetadata,
          mz_input           = input$ruotolo_mz,
          MW_input           = input$ruotolo_MW,
          z_input            = input$ruotoloCharge,
          gas_input          = input$ruotoloGas,
          c_param            = input$ruotolo_c,
          calibrantTables    = calibrantTables
        )
        
        # Fill CCS if manual
        if (is.null(ruotolo_meta$CCS_lit)) {
          ruotolo_meta$CCS_lit <- calibrationCCS
        }
        
        # Extract metadata
        tpp_vec <- allTpp
        CCS_vec <- ruotolo_meta$CCS_lit
        mz_vec  <- ruotolo_meta$mz
        MW_vec  <- ruotolo_meta$MW
        z_vec   <- ruotolo_meta$z
        gas     <- ruotolo_meta$gas
        c_param <- ruotolo_meta$c_param
        
        cat("Calling run_ruotolo_calibration()...\n")
        # 2) Run Ruotolo Model
        ruotolo_result <- run_ruotolo_calibration(
          tpp      = tpp_vec,
          CCS_lit  = CCS_vec,
          mz       = mz_vec,
          MW       = MW_vec,
          z        = z_vec,
          gas      = gas,
          c_param  = c_param,
          decimals = input$linearPlotDecimals
        )
        
        cat("Ruotolo returned A:", ruotolo_result$A, "X:", ruotolo_result$X, "\n")
        
        # Add results to the reactive values for sending to calibration curves saved tab
        ruotoloPlotRV(ruotolo_result$plot)
        ruotoloSummaryRV(ruotolo_result$summary)
        
        # 3) Render Plot
        output$ruotoloPlot <- renderPlot({
          ruotolo_result$plot
        })
        
        # 4) Build Ruotolo summary with names
        summary_df <- ruotolo_result$summary
        summary_df$Name <- calibrationAnalyteNames
        summary_df <- summary_df[, c("Name", setdiff(names(summary_df), "Name"))]
        
        output$ruotoloSummary <- renderTable(summary_df)
        
        # 5) Store constants for saving
        ruotoloA(ruotolo_result$A)
        ruotoloX(ruotolo_result$X)
        
        ruotoloGasRV(gas)
        ruotoloCRV(c_param)
        ruotoloMZRV(mz_vec)
        ruotoloMWRV(MW_vec)
        ruotoloZRV(z_vec)
        ruotoloCCSRV(CCS_vec)
      }
      
      
      # Store power curves
      if (input$calibrationMethod == "Xia + Bush & Ruotolo") {
        reactivePlots$powerCurvePlots <- list(
          Single  = p_single,
          Multi   = p_multi,
          Ruotolo = ruotolo_result$plot
        )
      } else {
        reactivePlots$powerCurvePlots <- list(
          Single = p_single,
          Multi  = p_multi
        )
      }
      
      # Notify the user that calibration is complete
      calibrationComplete(TRUE)
      output$calibrationStatus <- renderText("Calibration Completed!")
      showNotification("Calibration Completed!",type = "message")
    })
  }) # End run calibration observer.
  
  #Run Data Observer ----
  observeEvent(input$runData, {
    # 1) Check calibration curve is saved -------------------------------
    currentSaved <- switch(input$curveSlot,
                           "Curve 1" = savedCal1(),
                           "Curve 2" = savedCal2(),
                           "Curve 3" = savedCal3()
    )
    
    if (!isTRUE(currentSaved)) {
      errorMessage <- "Error: Please ensure the selected calibration curve has been saved."
      output$analytesStatus <- renderText(errorMessage)
      showNotification(errorMessage, type = "error")
      return()
    }
    
    # 2) Check data file present ----------------------------------------
    if (is.null(unknownFile()) || unknownFile() == "") {
      errorMessage <- "Error: Please upload a file before processing analytes."
      output$analytesStatus <- renderText(errorMessage)
      showNotification(errorMessage, type = "error")
      return()
    }
    
    # Normalize the uploaded file path
    dataFilePath <- normalizePath(unknownFile())
    
    # 3) Analyte names + optional CCS -----------------------------------
    if (input$knownCCSValues == "Yes") {
      if (input$processDataAnalyteNames == "" ||
          input$processDataCCSTheoretical == "" ||
          is.na(input$processDataAnalyteNames) ||
          is.na(input$processDataCCSTheoretical)) {
        errorMessage <- "Error: Enter analyte names and CCS values before running."
        output$analytesStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
    } else {
      if (input$processDataAnalyteNames == "" || is.na(input$processDataAnalyteNames)) {
        errorMessage <- "Error: Enter analyte names before running."
        output$analytesStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
    }
    
    dataAnalyteNames <- unlist(strsplit(input$processDataAnalyteNames, ",\\s*"))
    
    if (input$knownCCSValues == "Yes") {
      dataCCSValues <- as.numeric(unlist(strsplit(input$processDataCCSTheoretical, ",\\s*")))
      
      if (any(is.na(dataCCSValues))) {
        errorMessage <- "Error: Please ensure all CCS values are numeric and comma separated."
        output$analytesStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
      
      if (length(dataAnalyteNames) != length(dataCCSValues)) {
        errorMessage <- "Error: The number of analytes and CCS values do not match."
        output$analytesStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
    }
    
    # 4) Determine if current curve has Ruotolo calibration -------------
    slotInfo <- switch(input$curveSlot,
                       "Curve 1" = calibrationInfo$curve1,
                       "Curve 2" = calibrationInfo$curve2,
                       "Curve 3" = calibrationInfo$curve3
    )
    print("DEBUG Ruotolo calibration slot contents:")
    print(slotInfo)
    ruotoloAvailable <- !is.null(slotInfo$ruotolo_A) && !is.null(slotInfo$ruotolo_X)
    
    
    # 5) If Ruotolo available, parse Ruotolo unknown inputs -------------
    ruotolo_mz_unknown <- ruotolo_MW_unknown <- NULL
    
    if (ruotoloAvailable) {
      if (input$ruotolo_mz_unknown == "" || input$ruotolo_MW_unknown == "" ||
          is.na(input$ruotolo_mz_unknown) || is.na(input$ruotolo_MW_unknown)) {
        errorMessage <- "Error: Please enter m/z and MW values for Ruotolo CCS calculation."
        output$analytesStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
      
      ruotolo_mz_unknown <- as.numeric(unlist(strsplit(input$ruotolo_mz_unknown, ",\\s*")))
      ruotolo_MW_unknown <- as.numeric(unlist(strsplit(input$ruotolo_MW_unknown, ",\\s*")))
      
      if (any(is.na(ruotolo_mz_unknown)) || any(is.na(ruotolo_MW_unknown))) {
        errorMessage <- "Error: Ruotolo m/z and MW values must be numeric and comma separated."
        output$analytesStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
      
      if (length(ruotolo_mz_unknown) != length(dataAnalyteNames) ||
          length(ruotolo_MW_unknown) != length(dataAnalyteNames)) {
        errorMessage <- "Error: The number of Ruotolo m/z and MW values must match the number of analytes."
        output$analytesStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
    }
    
    # 6) Check analyte sheets in file -----------------------------------
    allSheetNames <- excel_sheets(dataFilePath)
    ATDSheets <- allSheetNames[grepl("Analyte", allSheetNames, ignore.case = TRUE)]
    
    if (length(dataAnalyteNames) != length(ATDSheets)) {
      errorMessage <- paste(
        "Error: The number of analytes entered does not equal the number of processable excel sheets.",
        "Start tab names with 'Analyte'. Please see the supporting information for an example document."
      )
      output$calibrationStatus <- renderText(errorMessage)
      showNotification(errorMessage, type = "error")
      return()
    }
    
    # Validate separation times (bypass, single-pass)
    for (i in seq_along(ATDSheets)) {
      separationTimesRaw <- readxl::read_excel(dataFilePath, sheet = ATDSheets[i], col_names = FALSE, col_types = "numeric", n_max = 1)
      separationTimes <- as.numeric(separationTimesRaw[1, ][!is.na(separationTimesRaw[1, ])])
      
      if (!all(is.finite(separationTimes))) {
        errorMessage <- paste(
          "Error: Please ensure all separation times in your excel are numeric and do not contain ms or other labels. Check sheet",
          ATDSheets[i]
        )
        output$calibrationStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
      
      if (separationTimes[1] != 0 && separationTimes[1] != 0.01) {
        errorMessage <- paste(
          "Error: Please ensure the bypass ATD is first in all sheets and has a 0 or 0.01 separation time label. Check sheet",
          ATDSheets[i]
        )
        output$calibrationStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
      
      if (separationTimes[2] != 2) {
        errorMessage <- paste(
          "Error: Please ensure the single pass ATD is second in all sheets and has a 2 separation time label. Check sheet",
          ATDSheets[i]
        )
        output$calibrationStatus <- renderText(errorMessage)
        showNotification(errorMessage, type = "error")
        return()
      }
    }
    
    # 7) Prepare plot lists, arrays, and equations ----------------------
    dataPlotsTemp <- list()
    individualAnalytesTemp <- list()
    
    # Pick Xia curves from selected slot
    if (input$curveSlot == "Curve 1") {
      singleEquationString <- singlePassEquationCurve1()
      multiEquationString  <- multiPassEquationCurve1()
    } else if (input$curveSlot == "Curve 2") {
      singleEquationString <- singlePassEquationCurve2()
      multiEquationString  <- multiPassEquationCurve2()
    } else {
      singleEquationString <- singlePassEquationCurve3()
      multiEquationString  <- multiPassEquationCurve3()
    }
    
    singleEquationString <- gsub("\\s+", "", singleEquationString)
    multiEquationString  <- gsub("\\s+", "", multiEquationString)
    
    singleCoeffs <- extract_coefficients_from_string(singleEquationString)
    multiCoeffs  <- extract_coefficients_from_string(multiEquationString)
    
    singlePassEquation <- substitute(a * t^b, list(a = singleCoeffs$a, b = singleCoeffs$b))
    multiPassEquation  <- substitute(a * t^b, list(a = multiCoeffs$a,  b = multiCoeffs$b))
    
    dataTp1 <- numeric(length(dataAnalyteNames))
    dataTpp <- numeric(length(dataAnalyteNames))
    percentDifferenceTp1 <- rep(NA_real_, length(dataAnalyteNames))
    percentDifferenceTpp <- rep(NA_real_, length(dataAnalyteNames))
    
    # Ruotolo result vectors
    ruotoloCCS <- rep(NA_real_, length(dataAnalyteNames))
    percentDifferenceRuotolo <- rep(NA_real_, length(dataAnalyteNames))
    
    output$analytesStatus <- renderText("Running... Please wait.")
    
    # 8) Main loop ------------------------------------------------------
    withProgress(message = "Running calculations...", value = 0, {
      for (i in seq_along(ATDSheets)) {
        incProgress(1 / length(ATDSheets), detail = paste("Processing:", dataAnalyteNames[i]))
        
        result <- process_sheet(
          dataFilePath,
          ATDSheets[i],
          dataAnalyteNames[i],
          input$linearPlotDecimals,
          list()
        )
        
        dataPlotsTemp <- c(dataPlotsTemp, result$plotList)
        individualAnalytesTemp[[dataAnalyteNames[i]]] <- result$plotList
        
        # Xia: CCS calculations
        if (input$knownCCSValues == "Yes") {
          knownCCSTp1Result <- calculate_ccs_with_comparison(
            dataAnalyteNames[i], result$driftTimes$tp1,
            dataCCSValues[i], singlePassEquation, TRUE, input$CCSDecimals
          )
          knownCCSTppResult <- calculate_ccs_with_comparison(
            dataAnalyteNames[i], result$driftTimes$tpp,
            dataCCSValues[i], multiPassEquation, FALSE, input$CCSDecimals
          )
          
          dataPlotsTemp <- c(dataPlotsTemp, knownCCSTp1Result$plotObject[1])
          individualAnalytesTemp[[dataAnalyteNames[i]]] <- c(
            individualAnalytesTemp[[dataAnalyteNames[i]]],
            knownCCSTp1Result$plotObject[1]
          )
          
          dataPlotsTemp <- c(dataPlotsTemp, knownCCSTppResult$plotObject[1])
          individualAnalytesTemp[[dataAnalyteNames[i]]] <- c(
            individualAnalytesTemp[[dataAnalyteNames[i]]],
            knownCCSTppResult$plotObject[1]
          )
          
          dataTp1[i] <- knownCCSTp1Result$calculatedCCS
          dataTpp[i] <- knownCCSTppResult$calculatedCCS
          
          percentDifferenceTp1[i] <- knownCCSTp1Result$percentDifferences
          percentDifferenceTpp[i] <- knownCCSTppResult$percentDifferences
        } else {
          unknownCCSTp1Result <- calculate_ccs_without_comparison(
            dataAnalyteNames[i], result$driftTimes$tp1,
            singlePassEquation, TRUE, input$CCSDecimals
          )
          unknownCCSTppResult <- calculate_ccs_without_comparison(
            dataAnalyteNames[i], result$driftTimes$tpp,
            multiPassEquation, FALSE, input$CCSDecimals
          )
          
          dataPlotsTemp <- c(dataPlotsTemp, unknownCCSTp1Result$plotObject[1])
          individualAnalytesTemp[[dataAnalyteNames[i]]] <- c(
            individualAnalytesTemp[[dataAnalyteNames[i]]],
            unknownCCSTp1Result$plotObject[1]
          )
          
          dataPlotsTemp <- c(dataPlotsTemp, unknownCCSTppResult$plotObject[1])
          individualAnalytesTemp[[dataAnalyteNames[i]]] <- c(
            individualAnalytesTemp[[dataAnalyteNames[i]]],
            unknownCCSTppResult$plotObject[1]
          )
          
          dataTp1[i] <- unknownCCSTp1Result$calculatedCCS
          dataTpp[i] <- unknownCCSTppResult$calculatedCCS
        }
        
        # Ruotolo CCS for unknown (if available)
        if (ruotoloAvailable) {
          ru_res <- calculate_ruotolo_ccs_unknown(
            tpp   = result$driftTimes$tpp,
            mz    = ruotolo_mz_unknown[i],
            MW    = ruotolo_MW_unknown[i],
            z = slotInfo$ruotolo_z[1],
            A       = slotInfo$ruotolo_A,
            X       = slotInfo$ruotolo_X,
            gas     = slotInfo$ruotolo_gas,
            c_param = slotInfo$ruotolo_c
          )
          
          ruotoloCCS[i] <- ru_res$CCS
          
          if (input$knownCCSValues == "Yes") {
            percentDifferenceRuotolo[i] <- percent_diff(ruotoloCCS[i], dataCCSValues[i])
          }
        }
      }
      
      reactivePlots$allProcessDataPlots <- dataPlotsTemp
      
      # 9) Build CCS results table --------------------------------------
      if (input$knownCCSValues == "Yes") {
        if (ruotoloAvailable) {
          CCSResultsDT <- data.frame(
            analyte = dataAnalyteNames,
            acceptedCCS = format(round(dataCCSValues, input$CCSDecimals), nsmall = input$CCSDecimals),
            singlePassCCS = format(round(dataTp1, input$CCSDecimals), nsmall = input$CCSDecimals),
            percentDifferenceSinglePass = ifelse(
              is.na(percentDifferenceTp1), NA,
              format(round(percentDifferenceTp1, input$errorDecimals), nsmall = input$errorDecimals)
            ),
            multiPassCCS = format(round(dataTpp, input$CCSDecimals), nsmall = input$CCSDecimals),
            percentDifferenceMultiplePass = ifelse(
              is.na(percentDifferenceTpp), NA,
              format(round(percentDifferenceTpp, input$errorDecimals), nsmall = input$errorDecimals)
            ),
            ruotoloCCS = format(round(ruotoloCCS, input$CCSDecimals), nsmall = input$CCSDecimals),
            percentDifferenceRuotolo = ifelse(
              is.na(percentDifferenceRuotolo), NA,
              format(round(percentDifferenceRuotolo, input$errorDecimals), nsmall = input$errorDecimals)
            )
          )
          customColNames <- c(
            "Analyte", "Accepted CCS",
            "Single Pass CCS", "Percent Difference (From Accepted)",
            "Xia Multipass CCS",  "Percent Difference (From Accepted)",
            "Xia + Bush & Ruotolo Multipass CCS",  "Percent Difference (From Accepted)"
          )
        } else {
          CCSResultsDT <- data.frame(
            analyte = dataAnalyteNames,
            acceptedCCS = format(round(dataCCSValues, input$CCSDecimals), nsmall = input$CCSDecimals),
            singlePassCCS = format(round(dataTp1, input$CCSDecimals), nsmall = input$CCSDecimals),
            percentDifferenceSinglePass = ifelse(
              is.na(percentDifferenceTp1), NA,
              format(round(percentDifferenceTp1, input$errorDecimals), nsmall = input$errorDecimals)
            ),
            multiPassCCS = format(round(dataTpp, input$CCSDecimals), nsmall = input$CCSDecimals),
            percentDifferenceMultiplePass = ifelse(
              is.na(percentDifferenceTpp), NA,
              format(round(percentDifferenceTpp, input$errorDecimals), nsmall = input$errorDecimals)
            )
          )
          customColNames <- c(
            "Analyte", "Accepted CCS",
            "Single Pass CCS", "Percent Difference (From Accepted)",
            "Multi Pass CCS",  "Percent Difference (From Accepted)"
          )
        }
      } else {
        if (ruotoloAvailable) {
          CCSResultsDT <- data.frame(
            analyte = dataAnalyteNames,
            singlePassCCS = format(round(dataTp1, input$CCSDecimals), nsmall = input$CCSDecimals),
            multiPassCCS = format(round(dataTpp, input$CCSDecimals), nsmall = input$CCSDecimals),
            ruotoloCCS   = format(round(ruotoloCCS, input$CCSDecimals), nsmall = input$CCSDecimals)
          )
          customColNames <- c(
            "Analyte", "Single Pass CCS", "Xia Multi Pass CCS", "Xia + Bush & Ruotolo CCS"
          )
        } else {
          CCSResultsDT <- data.frame(
            analyte = dataAnalyteNames,
            singlePassCCS = format(round(dataTp1, input$CCSDecimals), nsmall = input$CCSDecimals),
            multiPassCCS = format(round(dataTpp, input$CCSDecimals), nsmall = input$CCSDecimals)
          )
          customColNames <- c("Analyte", "Single Pass CCS", "Xia Multi Pass CCS")
        }
      }
      
      # 10) Render DT + UI dropdown + download handler ------------------
      output$CCSResultsDT <- DT::renderDT({
        datatable(CCSResultsDT, colnames = customColNames, rownames = FALSE, escape = FALSE)
      })
      
      # --- RuoTolo comparison plot ---
      output$comparisonPlotRU <- renderPlot({
        req(ruotoloAvailable)   # only plot if Ruotolo calibration present
        
        # Build a dataframe for comparison
        compDF <- data.frame(
          analyte = dataAnalyteNames,
          Xia_CCS = dataTpp,
          Ruotolo_CCS = ruotoloCCS
        )
        
        # Remove any analytes that failed to compute
        compDF <- compDF[is.finite(compDF$Xia_CCS) & is.finite(compDF$Ruotolo_CCS), ]
        req(nrow(compDF) > 0)
        
        # 1:1 identity line limits
        xyMin <- min(c(compDF$Xia_CCS, compDF$Ruotolo_CCS), na.rm = TRUE)
        xyMax <- max(c(compDF$Xia_CCS, compDF$Ruotolo_CCS), na.rm = TRUE)
        
        ggplot(compDF, aes(x = Xia_CCS, y = Ruotolo_CCS, label = analyte)) +
          geom_abline(slope = 1, intercept = 0, color = "gray40", linetype = "dashed") +
          geom_point(size = 3, color = "blue") +
          geom_text(vjust = -0.7, size = 4) +
          coord_equal(xlim = c(xyMin, xyMax), ylim = c(xyMin, xyMax)) +
          labs(
            title = "Xia vs Bush–Ruotolo CCS Comparison",
            x = expression("Xia CCS (Å"^2*")"),
            y = expression("Ruotolo CCS (Å"^2*")")
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 20, hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12)
          )
      })
      
      output$analyteDropdownMenu <- renderUI({
        selectInput(
          "selectedAnalyte",
          "Choose an Analyte to Download",
          choices = CCSResultsDT$analyte,
          selected = CCSResultsDT$analyte[1]
        )
      })
      
      observeEvent(input$selectedAnalyte, {
        sanitizedAnalyte <- sanitize_names(input$selectedAnalyte)
        
        output$downloadPlot <- downloadHandler(
          filename = function() {
            paste0(sanitize_names(input$selectedAnalyte), "_plot.pdf")
          },
          content = function(file) {
            
            withProgress(message = "Preparing selected analyte plots...", value = 0, {
              
              plotData <- individualAnalytesTemp[[input$selectedAnalyte]]
              totalPlots <- length(plotData)
              
              if (is.null(plotData) || totalPlots == 0) {
                showNotification("No plots are available for the selected analyte.", type = "warning")
                return()
              }
              
              pdf(file, width = 6, height = 4)
              on.exit(dev.off(), add = TRUE)
              
              for (i in seq_along(plotData)) {
                incProgress(1 / totalPlots, detail = paste("Adding plot", i, "of", totalPlots))
                print(plotData[[i]])
              }
            })
            
            showNotification("Selected analyte plot download is ready.", type = "message")
          }
        )
      })
      
      processDataComplete(TRUE)
      output$analytesStatus <- renderText("Completed!")
      showNotification("Completed!", type = "message")
    })
    
  })  # End runData observer
  
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
    
    start_row <- as.numeric(input$startRow)
    start_col <- as.numeric(input$startColumn)
    end_col   <- start_col + 1
    
    # Validate row/column inputs before reading
    if (is.na(start_row) || is.na(start_col) || start_row < 1 || start_col < 1) {
      errorMessage <- "Error: Start row and start column must be numeric values greater than or equal to 1."
      showNotification(errorMessage, type = "error")
      output$calibrationStatus <- renderText(errorMessage)
      return()
    }
    
    # Read the selected arrival time/intensity columns
    data <- tryCatch({
      readxl::read_xlsx(
        input$usersATDFile$datapath,
        sheet = input$sheetName,
        range = readxl::cell_limits(c(start_row, start_col), c(NA, end_col)),
        col_names = FALSE
      )
    }, error = function(e) {
      showNotification(
        paste("Error reading ATD file:", e$message),
        type = "error"
      )
      return(NULL)
    })
    
    if (is.null(data)) return()
    
    # Confirm that two columns were actually read
    if (ncol(data) < 2) {
      errorMessage <- paste0(
        "Error: The selected range did not contain two columns. ",
        "Check that the start column contains arrival time values and the next column contains intensity values."
      )
      showNotification(errorMessage, type = "error")
      output$calibrationStatus <- renderText(errorMessage)
      return()
    }
    
    if (nrow(data) == 0) {
      errorMessage <- "Error: The selected range did not contain any rows. Check the selected sheet and start row."
      showNotification(errorMessage, type = "error")
      output$calibrationStatus <- renderText(errorMessage)
      return()
    }
    
    # Keep only the first two columns in case readxl pulls extra columns
    data <- data[, 1:2]
    names(data) <- c("arrivalTime", "intensity")
    
    # Convert to numeric
    arrivalTimes <- suppressWarnings(as.numeric(data$arrivalTime))
    intensities  <- suppressWarnings(as.numeric(data$intensity))
    
    # Remove rows that are fully blank
    keep <- !(is.na(arrivalTimes) & is.na(intensities))
    arrivalTimes <- arrivalTimes[keep]
    intensities  <- intensities[keep]
    
    # Validate numeric data
    if (length(arrivalTimes) == 0 || length(intensities) == 0) {
      errorMessage <- "Error: No numeric arrival time/intensity data were found in the selected range."
      showNotification(errorMessage, type = "error")
      output$calibrationStatus <- renderText(errorMessage)
      return()
    }
    
    if (any(is.na(arrivalTimes)) || any(is.na(intensities))) {
      errorMessage <- paste(
        "Error: This data appears to contain missing values or non-numeric row/column headers.",
        "Try increasing the start row so the selected range begins at the first numeric data row."
      )
      showNotification(errorMessage, type = "error")
      output$calibrationStatus <- renderText(errorMessage)
      return()
    }
    
    if (length(arrivalTimes) != length(intensities)) {
      errorMessage <- "Error: Mismatch in the number of rows between arrival times and intensities."
      showNotification(errorMessage, type = "error")
      output$calibrationStatus <- renderText(errorMessage)
      return()
    }
    
    # Store clean numeric data reactively
    cleanData <- data.frame(
      arrivalTime = arrivalTimes,
      intensity = intensities
    )
    
    multiplePeakReactive(cleanData)
    
    # Create raw data plot
    output$rawDataPlot <- renderPlot({
      startTick <- floor(min(arrivalTimes) / 0.5) * 0.5
      
      par(
        mar = c(5, 8, 2, 2),
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
    
    # ---- Per-peak metrics (μ, σ, FWHM, Rp) ----
    metrics <- lapply(models, get_peak_metrics)
    metrics <- metrics[!vapply(metrics, is.null, logical(1))]
    
    if (length(metrics) > 0) {
      peakMetricsRV(
        do.call(rbind, lapply(seq_along(metrics), function(i) {
          data.frame(
            Peak = i,
            mu_ms = metrics[[i]]$mu,
            sigma_ms = metrics[[i]]$sigma,
            FWHM_ms = metrics[[i]]$FWHM,
            Rp_time = metrics[[i]]$Rp_time,
            row.names = NULL
          )
        }))
      )
    } else {
      peakMetricsRV(NULL)
    }
    
    # ---- Pairwise Rpp (time domain) ----
    rppRV(compute_all_Rpp(models))
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
  # Example calibration files.
  output$exampleCalibrationUI1 <- renderUI({
    downloadButton("exampleCalibrationFile1", "SpheriCal Example .xlsx")
  })
  output$exampleCalibrationUI2 <- renderUI({
    downloadButton("exampleCalibrationFile2", "Major Mix Example .xlsx")
  })
  output$exampleRDSUI1 <- renderUI({
    downloadButton("exampleRDSFile1", "SpheriCal Example.rds")
  })
  output$exampleRDSUI2 <- renderUI({
    downloadButton("exampleRDSFile2", "Major Mix Example.rds")
  })
  
  # Example experimental files
  output$exampleExperimentUI1 <- renderUI({
    downloadButton("exampleExperimentFile1", "Eicosanoid Oxylipins.xlsx")
  })
  output$exampleExperimentUI2 <- renderUI({
    downloadButton("exampleExperimentFile2", "SPLASH Mix.xlsx")
  })
  output$exampleMultiPeakATDUI <- renderUI({
    downloadButton("exampleMultiPeakATDFile", "Prostaglandins A2, B2 & J2.xlsx")
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
  # Download handler for calibration files
  output$exampleCalibrationFile1 <- downloadHandler(
    filename = function() {
      paste("SpheriCal Example", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("www/SpheriCal Example.xlsx", file)
    }
  )
  
  output$exampleCalibrationFile2 <- downloadHandler(
    filename = function() {
      paste("MajorMix Calibration Curve Example", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("www/MajorMix Calibration Curve Example.xlsx", file)
    }
  )
  
  output$exampleRDSFile1 <- downloadHandler(
    filename = function() {
      "SpheriCal_Calibration_Sodium Adducts_350ms_18V_2026-05-05.rds"
    },
    content = function(file) {
      file.copy("www/SpheriCal_Calibration_Sodium Adducts_350ms_18V_2026-05-05.rds", file)
    }
  )
  
  output$exampleRDSFile2 <- downloadHandler(
    filename = function() {
      "Major_Mix_Positive_Calibration_2026-05-17.rds"
    },
    content = function(file) {
      file.copy("www/Major_Mix_Positive_Calibration_2026-05-17.rds", file)
    }
  )
  
  # Download handler for experimental files
  output$exampleExperimentFile1 <- downloadHandler(
    filename = function() {
      paste("Oxylipins Example", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("www/Oxylipins Example.xlsx", file)
    }
  )
  output$exampleExperimentFile2 <- downloadHandler(
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
      file.copy("www/Prostaglandins ABJ2 Multiple Peak ATD Example.xlsx", file)
    }
  )
  # Download handler for all plots in the calibration curves tab.
  output$downloadAllCalibrationPlots <- downloadHandler(
    filename = function() {
      paste("All_Calibration_Plots_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      
      withProgress(message = "Preparing calibration plots for download...", value = 0, {
        
        allPlots <- c(
          reactivePlots$allCalibrationPlots,
          reactivePlots$powerCurvePlots
        )
        
        totalPlots <- length(allPlots)
        
        if (totalPlots == 0) {
          showNotification("No calibration plots are available to download.", type = "warning")
          return()
        }
        
        pdf(file, width = 6, height = 4)
        on.exit(dev.off(), add = TRUE)
        
        for (i in seq_along(allPlots)) {
          incProgress(1 / totalPlots, detail = paste("Adding plot", i, "of", totalPlots))
          print(allPlots[[i]])
        }
      })
      
      showNotification("Calibration plot download is ready.", type = "message")
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
      
      withProgress(message = "Preparing analyte plots for download...", value = 0, {
        
        allPlots <- reactivePlots$allProcessDataPlots
        totalPlots <- length(allPlots)
        
        if (totalPlots == 0) {
          showNotification("No analyte plots are available to download.", type = "warning")
          return()
        }
        
        pdf(file, width = 6, height = 4)
        on.exit(dev.off(), add = TRUE)
        
        for (i in seq_along(allPlots)) {
          incProgress(1 / totalPlots, detail = paste("Adding plot", i, "of", totalPlots))
          print(allPlots[[i]])
        }
      })
      
      showNotification("Analyte plot download is ready.", type = "message")
    }
  ) # End of download button handlers.
  # Observer for selecting given calibrants
  
  observeEvent(input$calibrantSource, {
    
    df <- switch(input$calibrantSource,
                 "major_pos" = calibrants_majorMix_pos,
                 "major_neg" = calibrants_majorMix_neg,
                 "spherical" = calibrants_spherical,
                 "custom"     = NULL)
    
    # Built-in calibrants
    if (!is.null(df)) {
      output$calibrantSelectorUI <- renderUI({
        checkboxGroupInput(
          "selectedCalibrants",
          "Select Calibrants to Include:",
          choices = as.list(df$name),
          selected = df$name
        )
      })
    }
    
    # Custom option (optional future expansion)
    else {
      output$calibrantSelectorUI <- renderUI({
        list(
          textAreaInput("custom_mz", "m/z (comma separated):"),
          textAreaInput("custom_MW", "MW (comma separated):"),
          textAreaInput("custom_charge", "Charge (comma separated):")
        )
      })
    }
  })
  
  observeEvent(input$calibrationMethod, {
    updateCheckboxInput(session, "useCalibrantMetadata", value = FALSE)
  })
  
  # ---- Export Saved Calibration Curve Metadata----
  # This code is used for creating JSON files of calibrations so they can be reuploaded to the application without rerunning the calibration
  output$exportCalibration <- downloadHandler(
    
    filename = function() {
      slot <- input$whichCurve
      date_str <- format(Sys.Date(), "%Y-%m-%d")
      paste0("Multipass_CCS_Calibration_", slot, "_", date_str, ".rds")
    },
    
    content = function(file) {
      
      slotInfo <- switch(input$whichCurve,
                         "Curve 1" = calibrationInfo$curve1,
                         "Curve 2" = calibrationInfo$curve2,
                         "Curve 3" = calibrationInfo$curve3
      )
      
      req(slotInfo)
      
      # Optional: add metadata wrapper
      export_payload <- list(
        type        = "Multipass_CCS_Refiner_Calibration",
        exported_on = Sys.time(),
        curve_slot  = input$whichCurve,
        payload     = slotInfo
      )
      
      saveRDS(export_payload, file)
    }
  )
  
  
  # Allows button to activate in the UI for exporting curves metadata.
  output$canExportCalibration <- reactive({
    slot <- switch(input$whichCurve,
                   "Curve 1" = calibrationInfo$curve1,
                   "Curve 2" = calibrationInfo$curve2,
                   "Curve 3" = calibrationInfo$curve3)
    !is.null(slot)
  })
  outputOptions(output, "canExportCalibration", suspendWhenHidden = FALSE)
  
  # Render Rp and Rpp time domain tables
  # Change this to match the rounding in the settings
  output$peakMetricsTable <- DT::renderDT({
    req(peakMetricsRV())
    
    df <- peakMetricsRV()
    
    # Format numeric columns to four decimals
    num_cols <- vapply(df, is.numeric, logical(1))
    df[num_cols] <- lapply(df[num_cols], function(x) {
      formatC(x, format = "f", digits = 4)
    })
    
    # Keep Peak as an integer
    if ("Peak" %in% names(df)) {
      df$Peak <- as.character(as.integer(as.numeric(df$Peak)))
    }
    
    DT::datatable(
      df,
      rownames = FALSE,
      escape = FALSE,
      colnames = c(
        "Peak",
        "Arrival Time, &mu; (ms)",
        "&sigma; (ms)",
        "FWHM (ms)",
        "R<sub>p</sub>"
      ),
      options = list(pageLength = 10)
    )
  })
  
  # Create Dataframe for Two Peak Resolution
  output$rppTable <- DT::renderDT({
    req(rppRV())
    
    df <- rppRV()
    
    # Format numeric columns to four decimals
    num_cols <- vapply(df, is.numeric, logical(1))
    df[num_cols] <- lapply(df[num_cols], function(x) {
      formatC(x, format = "f", digits = 4)
    })
    
    # Keep peak identifiers as integers
    if ("PeakA" %in% names(df)) {
      df$PeakA <- as.character(as.integer(as.numeric(df$PeakA)))
    }
    
    if ("PeakB" %in% names(df)) {
      df$PeakB <- as.character(as.integer(as.numeric(df$PeakB)))
    }
    
    DT::datatable(
      df,
      rownames = FALSE,
      escape = FALSE,
      colnames = c(
        "Peak A",
        "Peak B",
        "Arrival Time Peak A, &mu;<sub>A</sub> (ms)",
        "FWHM Peak A (ms)",
        "Arrival Time Peak B, &mu;<sub>B</sub> (ms)",
        "FWHM Peak B (ms)",
        "&Delta;&mu; (ms)",
        "FWHM Sum (ms)",
        "R<sub>pp</sub>"
      ),
      options = list(pageLength = 10)
    )
  })

  observeEvent(input$autoGuessPeaks, {
    
    data <- multiplePeakReactive()
    req(data)
    
    arrivalTimes <- data[[1]]
    intensities  <- data[[2]]
    
    guesses <- auto_guess_peaks(
      arrivalTimes,
      intensities,
      min_rel_height = 0.10,
      min_separation = 0.3
    )
    
    if (!length(guesses)) {
      showNotification("No peaks detected above threshold.", type = "warning")
      return()
    }
    
    # Update number of peaks
    numPeaks(length(guesses))
    
    # Populate inputs
    for (i in seq_along(guesses)) {
      updateNumericInput(session,
                         paste0("userAmplitude", i),
                         value = guesses[[i]]$amplitude)
      
      updateNumericInput(session,
                         paste0("userArrivalTime", i),
                         value = guesses[[i]]$arrivalTime)
      
      updateNumericInput(session,
                         paste0("userStd", i),
                         value = guesses[[i]]$stdDev)
    }
    
    showNotification(
      paste("Auto-detected", length(guesses), "peaks"),
      type = "message"
    )
  })
  
}# End of server
#####End of Defining Server#####
