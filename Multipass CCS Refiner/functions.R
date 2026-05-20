# This file contains the functions used in Multipass CCS Refiner
library(readxl) 
library(xlsx)
library(DT)
library(ggplot2) 
library(grid)
library(viridisLite)

#####Defining Functions#####
# Functions are defined as verbs using snake case.
# Variables are defined with CamelCase using nouns.

# Function for using standard references table. Done to lower user input requirements
getSelectedCalibrantDF <- function(input, calibrantTables) {
  
  df <- calibrantTables[[ input$calibrantSource ]]
  if (is.null(df)) return(NULL)
  if (is.null(input$selectedCalibrants)) return(NULL)
  df[df$name %in% input$selectedCalibrants, ]
}
# Calculate precise arrival time from an arrival time distribution (ATD).
# Gaussian distributions are fit using non-linear least squares regression (nls).
calculate_arrival_time <- function(arrivalTimes,intensities) {
  
  # Provide a starting value near the maximum of the ATD.
  amplitudeMax <- max(intensities)
  amplitudeGuess <- 1.1*amplitudeMax
  guessLocation <- which(intensities == amplitudeMax)[1]
  muGuess <- arrivalTimes[guessLocation]
  
  # The sigma or standard deviation will change based peak broadening.
  # I have chosen 0.2 as a starting place.
  sigmaGuess <- 0.2
  
  # Add error catching in the event of a singular convergence error. 
  # This may occur if initial estimates are off or if data is noisy.
  # Fit intensities to a Gaussian using our starting parameters.
  if (!is.na(muGuess) && is.numeric(muGuess)) { 
    tryCatch({
      modelATD <- nls(intensities ~ A * exp(-((arrivalTimes - mu)^2) / (2 * sigma^2)), 
                      start = list(A = amplitudeGuess, mu = muGuess, sigma = sigmaGuess), 
                      algorithm = 'port',
                      control = nls.control(warnOnly = TRUE))
    }, error = function(e) {
      cat("Singular convergence error encountered. Trying again with different initial guesses.\n")
      amplitudeGuess <- amplitudeGuess * runif(1, 0.8, 1.2)
      muGuess <- muGuess * runif(1, 0.9, 1.1)
      sigmaGuess <- sigmaGuess * runif(1, 0.9, 1.1)
      modelATD <- nls(intensities ~ A * exp(-((arrivalTimes - mu)^2) / (2 * sigma^2)), 
                      start = list(A = amplitudeGuess, mu = muGuess, sigma = sigmaGuess), 
                      algorithm = "port",
                      control = nls.control(warnOnly = TRUE))   
    })
  }
  return(modelATD)
}

# This function plots the fitted Gaussian distribution over the ATD.
# The number of passes for an ion packet is calculated in the process_sheet function.
plot_gaussian_arrival <- function(arrivalTimes, intensities, modelATD, separationTime, passes, decimals) {
  
  # Create a data frame from arrivalTimes and intensities
  data <- data.frame(
    arrivalTimes = arrivalTimes,
    intensities = intensities,
    fittedValues = predict(modelATD),
    passes = passes,
    separationTime = separationTime
  )
  
  # Extract muCoefficient from the model
  muCoefficient <- coef(modelATD)["mu"]
  
  # Build title and subtitle as plain character strings
  titleText <- paste0(
    "Arrival time: ",
    format(round(muCoefficient, decimals), nsmall = decimals)
  )
  
  subtitleText <- paste0(
    "Separation Time (ms): ", separationTime,
    " | Passes: ", passes
  )
  
  # Create the plot using ggplot2
  p <- ggplot(data, aes(x = arrivalTimes, y = intensities)) +
    geom_point(aes(x = arrivalTimes, y = intensities), 
               shape = 21,  
               color = "black",  
               fill = "white",   
               size = 4) +  
    geom_line(aes(x = arrivalTimes, y = fittedValues), color = "red", linewidth = 1.25) +  
    labs(
      x = "Arrival Time (ms)",
      y = "",
      title = titleText,
      subtitle = subtitleText
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, hjust = 0.5, vjust = 2),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      panel.background = element_rect(fill = "white", color = "black"),  
      panel.border = element_rect(color = "black", linewidth = 0.5, fill=NA),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title = element_text(size = 14),
      plot.margin = margin(t = 40, r = 20, b = 40, l = 20),
      panel.grid = element_blank()
    )
  
  # Return the muCoefficient and the plot object
  print(p)
  return(list(muCoefficient = muCoefficient, plotObject = p))
}

# Similar function to calculate_arrival_time but built to accept user input guess values.
# This function to built to assist with reconstructing points when ATD's are partially overlapping.
calculate_multiple_arrival_times <- function(
    arrivalTimes,
    intensities,
    peakGuesses,
    window_sigma = 3
) {
  
  n_peaks <- length(peakGuesses)
  models  <- vector("list", n_peaks)
  
  # ---- 1. Fit strongest peaks first ----
  order_idx <- order(
    vapply(peakGuesses, function(p) p$amplitude, numeric(1)),
    decreasing = TRUE
  )
  peakGuesses_ord <- peakGuesses[order_idx]
  
  # ---- 2. Initialize residual signal ----
  residual <- intensities
  
  for (k in seq_along(peakGuesses_ord)) {
    
    i <- order_idx[k]  # original peak index
    
    amplitudeGuess <- peakGuesses_ord[[k]]$amplitude
    muGuess        <- peakGuesses_ord[[k]]$arrivalTime
    sigmaGuess     <- max(abs(peakGuesses_ord[[k]]$stdDev), 0.02)
    
    # ---- 3. Local fitting window ----
    window <- abs(arrivalTimes - muGuess) <= (window_sigma * sigmaGuess)
    if (sum(window) < 8) {
      models[[i]] <- NULL
      next
    }
    
    xw <- arrivalTimes[window]
    yw <- residual[window]
    
    # ---- 4. Fit Gaussian to residual ----
    tryCatch({
      
      modelATD <- nls(
        yw ~ A * exp(-((xw - mu)^2) / (2 * sigma^2)),
        start = list(
          A     = amplitudeGuess,
          mu    = muGuess,
          sigma = sigmaGuess
        ),
        lower = c(0,      muGuess - 0.5, 0.02),
        upper = c(Inf,    muGuess + 0.5, Inf),
        algorithm = "port",
        control = nls.control(
          warnOnly = TRUE,
          maxiter  = 200
        )
      )
      
      models[[i]] <- modelATD
      
      # ---- 5. Subtract fitted peak from residual ----
      co <- coef(modelATD)
      fitted_full <- co["A"] * exp(-((arrivalTimes - co["mu"])^2) / (2 * co["sigma"]^2))
      
      residual <- residual - fitted_full
      residual[residual < 0] <- 0
      
    }, error = function(e) {
      message("Error fitting peak ", i, ": ", e$message)
      models[[i]] <- NULL
    })
  }
  
  models
}


# Plotting function for users to see where Gaussian distributions are being constructed for their peaks.
plot_multiple_gaussians <- function(
    arrivalTimes,
    intensities,
    models,
    decimals,
    highlighted_peak = NULL,
    sigmaRange = NULL
) {
  
  data <- data.frame(arrivalTimes = arrivalTimes, intensities = intensities)
  
  p <- ggplot(data, aes(x = arrivalTimes, y = intensities)) +
    geom_point(shape = 21, color = "black", fill = "white", size = 3) +
    labs(x = "Arrival Time (ms)", y = "") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA),
      axis.text = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      panel.grid = element_blank()
    )
  
  peakColors <- c(
    "#440154", "#1f9e89", "#6ece58", "#482878", "#000000",
    "#3e4989", "#26828e", "#35b779", "#b5de2b", "#fde725"
  )
  
  for (i in seq_along(models)) {
    if (is.null(models[[i]])) next
    
    fittedValues <- try(
      predict(
        models[[i]],
        newdata = data.frame(xw = arrivalTimes)
      ),
      silent = TRUE
    )
    
    if (inherits(fittedValues, "try-error") ||
        length(fittedValues) != length(arrivalTimes)) next
    
    muCoefficient <- coef(models[[i]])["mu"]
    
    peakData <- data.frame(
      arrivalTimes = arrivalTimes,
      fittedValues = fittedValues,
      peak = factor(i)
    )
    
    p <- p +
      geom_line(
        data = peakData,
        aes(x = arrivalTimes, y = fittedValues, color = peak),
        linewidth = 1.25
      ) +
      annotate(
        "text",
        x = muCoefficient + 0.5,
        y = max(fittedValues, na.rm = TRUE),
        label = paste0(
          "μ", i, ": ",
          formatC(muCoefficient, format = "f", digits = decimals)
        ),
        hjust = -0.25,
        vjust = 1,
        size = 8
      )
  }
  
  if (!is.null(highlighted_peak) &&
      !is.null(sigmaRange) &&
      !is.null(models[[highlighted_peak]])) {
    
    mu    <- coef(models[[highlighted_peak]])["mu"]
    sigma <- coef(models[[highlighted_peak]])["sigma"]
    
    lowerBound <- mu - sigmaRange * sigma
    upperBound <- mu + sigmaRange * sigma
    
    p <- p +
      geom_vline(xintercept = c(lowerBound, upperBound),
                 linetype = "dashed", color = "red", linewidth = 1)
  }
  
  p + scale_color_manual(values = peakColors, name = "Peak Number")
}

# Function to create linear plots to obtain perturbation corrected periodic drift times. 
# All ATDs across all separation times will be used to make these plots.
plot_linear_chart <- function(x, y, analyteName, t1, bypassTime, decimals) {
  # Prepare data frame and fit linear model.
  df <- data.frame(x = as.numeric(x), y = as.numeric(y))
  fit <- lm(y ~ x, data = df)
  
  # Numeric coefficients
  intercept <- coef(fit)[1]
  slope <- coef(fit)[2]
  r2 <- summary(fit)$r.squared
  tp1 <- t1 - bypassTime
  
  # Formatted text versions for display
  interceptText <- format(round(intercept, decimals), nsmall = decimals)
  slopeText <- format(round(slope, decimals), nsmall = decimals)
  r2Text <- format(round(r2, decimals), nsmall = decimals)
  tp1Text <- format(round(tp1, decimals), nsmall = decimals)
  
  # Create the x and y labels
  xLabel = expression(paste(t[s], " / n"))
  yLabel = expression(paste("(", t[nd], " - ", t[s], ") / n"))
  
  # Set text labels for equations and other annotations that will display on the plots.
  equationLabel <- paste0("y == ", slopeText, " * x + ", interceptText)
  rSquaredLabel <- paste0("R^2 == ", r2Text)
  tppLabel <- paste0("t[p1] == ", tp1Text)
  tp1Label <- paste0("t[pp] == ", interceptText)
  
  
  # Create the plot using ggplot2
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(analyteName) +
    theme(plot.title = element_text(size = 22, hjust = 0.5)) +
    labs(x = xLabel, y = yLabel) +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.background = element_rect(fill = "white", color = "black"),
      panel.border = element_rect(color = "black", linewidth = 0.5,fill=NA),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
      panel.grid.minor = element_line(color = "lightgray", linewidth = 0.1),
      plot.margin = margin(2, 1, 1, 1, "mm")
    ) +
    
    annotate(
      "text",
      x     = max(df$x),
      y     = max(df$y),
      label = equationLabel,
      hjust = 1, vjust = 1, size = 5,
      parse = TRUE
    ) +
    
    annotate(
      "text",
      x     = max(df$x),
      y     = 0.85 * max(df$y),
      label = rSquaredLabel,
      hjust = 1, vjust = 1, size = 5,
      parse = TRUE
    ) +
    
    # Place tp1 in the lower-left corner.
    annotate(
      "text",
      x     = min(df$x),
      y     = 0.25 * max(df$y),
      label = tp1Label,
      hjust = 0, vjust = 0, size = 5,
      parse = TRUE
    ) +
    
    # Place tpp annotation below tp1.
    annotate(
      "text",
      x     = min(df$x),
      y     = 0.10 * max(df$y),
      label = tppLabel,
      hjust = 0, vjust = 0, size = 5,
      parse = TRUE
    )
  
  
  # Suppressing is.na(x) language warning from pasting formatted subscripts as text.
  # This is done in an effort to make the plots publication quality.
  suppressWarnings({print(p)
    return(list(tpp = coef(fit)[1], plotObject = p))
  })
}

# process_sheet accepts user data and calls from above functions to process it.
# This function is also where the automated pass counter is implemented.
process_sheet <- function(filePath, sheetName, analyteName, decimals, plotList) {
  # Start by importing excel files. This is why formatting is important.
  featureData <- read_excel(filePath, sheet = sheetName, col_types = "numeric", skip = 2, col_names = FALSE)
  separationTimesRaw <- readxl::read_excel(filePath, sheet = sheetName, col_names = FALSE, col_types = "numeric", n_max = 1)
  separationTimes <- as.numeric(separationTimesRaw[1, ][!is.na(separationTimesRaw[1, ])])
  
  # Create numeric vectors and variables needed.
  exactArrivals <- numeric()
  n <- numeric()
  x <- numeric()
  y <- numeric()
  bypassTime <- 0
  t1 <- 0
  
  # There are 2 columns for each ATD so the pass counter increases by 2.
  for (j in seq(1, ncol(featureData) - 1, by = 2)) {
    arrivalIntensityPair <- na.omit(featureData[complete.cases(featureData), j:(j + 1)])
    # Converting lists into vectors. 
    arrivalTimes <- unlist(arrivalIntensityPair[, 1])
    intensities <- unlist(arrivalIntensityPair[, 2])
    
    # Call calculate_arrival_time to fit a Gaussian model.
    modelATD <- calculate_arrival_time(arrivalTimes, intensities)
    
    # Update storage for the arrival times with each pass.
    exactArrivals <- c(exactArrivals, coef(modelATD)["mu"])
    
    # Below is the pass counter which operates on a linear relationship.
    # This relationship is based on the time it takes to complete one pass.
    # Roughly whole numbers are obtained from the ratio of total drift and single pass times. 
    
    # First ATD or bypass at 0.01 ms separation time.
    if(j == 1){
      bypassTime = as.numeric(exactArrivals[1])
      n[1] = 0
    }
    # Second ATD or single pass at 2 ms separation time.
    if(j==3){
      t1 = as.numeric(exactArrivals[2])
      n[2] = 1
      x <- c(x, separationTimes[2] / n[2])
      y <- c(y, (t1 - bypassTime - separationTimes[2]) / n[2])
    }
    
    #For all passes beyond single pass.
    if (j > 3) {
      tnp <- as.numeric(exactArrivals[((j+1)/2)])
      n[(j+1)/2] <- round((tnp - bypassTime)/(t1 - bypassTime)) # This is a rounded approximation of the number of passes an analyte undergoes. May be adjusted in individual use cases.
      # Save x and y to make linear plots later, x=ts/n and y=(tn-ts-t0)/n
      x <- c(x, separationTimes[((j+1)/2)] / n[((j+1)/2)])
      y <- c(y, (tnp - separationTimes[((j+1)/2)] - bypassTime) / n[((j+1)/2)])
    }
    
    # Plot the Gaussian distribution over top the data and store the plot to a list.
    gaussianResult <- plot_gaussian_arrival(arrivalTimes, intensities, modelATD, separationTimes[((j + 1) / 2)], n[((j+1)/2)], decimals)
    plotList[[paste(analyteName,"seperation time",separationTimes[((j + 1) / 2)],sep="_")]] <- gaussianResult$plotObject
    
  }
  
  # Once all ATDs have been processed we can generate the linear plots and store them within the plot list.
  linearResult <- plot_linear_chart(x, y, analyteName, t1, bypassTime, decimals)
  plotList[[paste(analyteName, "linear", sep = "_")]] <- linearResult$p  # Store linear chart plot
  
  # Return both the drift times and the list of plots
  driftTimes <- list(tp1 = (t1 - bypassTime), tpp = linearResult$tpp)
  return(list(driftTimes = driftTimes, plotList = plotList))
}

# Function for fitting power curves from the calibration data.
# This function uses single pass and perturbed periodic drift times to create power curves.
# Accepted CCS values from a standard are required to construct these curves.
plot_power_curve <- function(CCS,exactArrivals,singleOrMulti,decimals) {
  
  # Ensure all inputs are numeric.
  CCS <- as.numeric(as.character(CCS))
  exactArrivals <- as.numeric(as.character(exactArrivals))
  
  # Check for NA values after conversion.
  if (any(is.na(CCS)) || any(is.na(exactArrivals))) {
    stop("CCS and exactArrivals must be numeric and cannot contain NA values.")
  }
  
  # Check for non-positive values
  if (any(CCS <= 0) || any(exactArrivals <= 0)) {
    stop("CCS and exactArrivals must be positive numbers.")
  }
  
  # Fit the data to a power law function.
  powerLawModel <- lm(log(CCS) ~ log(exactArrivals))
  predictedLogCCS <- predict(powerLawModel)
  predictedCCS <- exp(predictedLogCCS)
  
  # Create an xLabel for the plot
  if(singleOrMulti == "Single"){
    xLabel = expression(paste("Single-Pass ", t[p1], " (ms)"))
  }
  else {
    xLabel = expression(paste("Perturbed ", t[pp], " (ms)"))
  }
  
  yLabel = expression("CCS (" * "Å"^2 * ")")
  
  # Create a data frame for ggplot2.
  df <- data.frame(exactArrivals = exactArrivals, CCS = CCS)
  
  # Coefficients for display and return
  a <- exp(coef(powerLawModel)[1])
  b <- coef(powerLawModel)[2]
  aText <- format(round(a, decimals), nsmall = decimals)
  bText <- format(round(b, decimals), nsmall = decimals)
  
  # Equation as a plain text string (for returning)
  equation <- paste(
    "CCS =",
    format(round(a, decimals), nsmall = decimals),
    "* t^",
    format(round(b, decimals), nsmall = decimals)
  )
  
  # Equation label in parse-able form for the plot
  equationLabel <- paste0(
    "CCS == ", aText, " * t[p]^", bText
  )
  
  # Plot the data and the power law fit.
  p <- ggplot(df, aes(x = exactArrivals, y = CCS)) +
    geom_point() +
    geom_line(aes(y= predictedCCS), color = "red") + 
    labs(x = xLabel, y = yLabel) +
    
    # Add a border around the plot and panel.
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      panel.border = element_rect(color = "black", linewidth = 0.5,fill=NA),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_line(color = "lightgray", linewidth = 0.25),
      panel.grid = element_line(color = "lightgray", linewidth = 0.5),
    )+
    
    # Add the equation as an annotation in the plot at a location based on the data.
    annotate(
      "text",
      x     = median(df$exactArrivals),
      y     = sort(df$CCS, partial = 2)[2],
      label = equationLabel,
      hjust = 0,
      vjust = 1,
      size  = 5,
      parse = TRUE
    )
  
  print(p)
  curveExpression <- substitute(
    a * t^b,
    list(a = a, b = b)
  )
  # Return coefficients of the power law model and plot object.
  return(list(
    equation = equation, 
    p = p, 
    curveExpression = curveExpression
  ))
}

# Function for calculating the percent difference
percent_diff <- function(x, y) {
  abs(round(100 * (x - y) / ((x+y)/2), 2))
}
# This function will calculate CCS values and output a plot of each experimental analyte.
calculate_ccs_with_comparison <- function(dataAnalyteName, periodicTime, acceptedCCS, curveExpression, singleOrMulti, decimals) {
  # Check and convert character string into expression if needed
  if (is.character(curveExpression)) {
    cat("curveExpression was a character, converting to expression...\n")
    coeffs <- extract_coefficients_from_string(curveExpression)
    curveExpression <- substitute(a * t^b, list(a = coeffs$a, b = coeffs$b))
  }
  
  # Confirm curveExpression is a call object
  cat("curveExpression class:", class(curveExpression), "\n")
  print(curveExpression)
  
  # Extract coefficients for plotting
  coeffs <- extract_power_coefficients(curveExpression)
  a <- coeffs$a
  b <- coeffs$b
  cat("Extracted coefficients - a:", a, ", b:", b, "\n")
  
  # Set axis labels
  xLabel <- if (singleOrMulti) {
    expression(paste("Single-Pass ", t[p1], " (ms)"))
  } else {
    expression(paste("Perturbed ", t[pp], " (ms)"))
  }
  yLabel <- expression("CCS (" * "Å"^2 * ")")
  
  # Initialize output
  calculatedCCS <- numeric(1)
  percentDifference <- numeric(1)
  plots <- list()
  
  tryCatch({
    center <- periodicTime
    calculatedCCS <- eval(curveExpression, list(t = center))
    percentDifference <- percent_diff(calculatedCCS, acceptedCCS)
    
    cat("Calculated CCS for", dataAnalyteName, ":", calculatedCCS, "\n")
    cat("Percent Difference for", dataAnalyteName, ":", percentDifference, "\n")
    
    # Determine plot range
    yMin <- 50
    yBuffer <- 0.1 * calculatedCCS
    yMax <- max(400, calculatedCCS + yBuffer)
    yLimits <- c(yMin, yMax)
    
    xMin <- (yLimits[1] / a)^(1 / b)
    xMax <- (yLimits[2] / a)^(1 / b)
    xLimits <- c(xMin, xMax)
    
    # Label
    labelText <- paste0(
      "Calculated CCS: ", format(round(calculatedCCS, decimals), nsmall = decimals), "\n",
      "Percent Difference: ", format(round(percentDifference, decimals), nsmall = decimals), "%"
    )
    
    plotData <- data.frame(
      x = as.numeric(center),
      y = as.numeric(calculatedCCS),
      label = as.character(labelText),
      row.names = NULL
    )
    
    # Plot
    p <- ggplot(plotData, aes(x = x, y = y)) +
      geom_point(color = "blue", size = 3) +
      annotate("text", x = mean(xLimits), y = yLimits[1] + 0.05 * diff(yLimits),
               label = plotData$label, size = 4, hjust = 0.5, vjust = 0) +
      stat_function(fun = function(t) eval(curveExpression, list(t = t)),
                    color = "blue", xlim = xLimits) +
      coord_cartesian(ylim = yLimits) +
      labs(title = dataAnalyteName, x = xLabel, y = yLabel) +
      theme(
        plot.title = element_text(size = 22, hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)
      )
    
    plots[[1]] <- p
    print(p)
    cat("plot created")
  }, error = function(e) {
    cat("Error processing", dataAnalyteName, ":", e$message, "\n")
  })
  
  
  return(list(
    calculatedCCS = calculatedCCS,
    percentDifferences = percentDifference,
    plotObject = plots
  ))
}

# This is the same as calculate_ccs_with_comparison but without accepted CCS values. 
calculate_ccs_without_comparison <- function(dataAnalyteName, periodicTime, curveExpression, singleOrMulti, decimals) {
  # Set x-axis label based on pass type
  xLabel <- if (singleOrMulti) {
    expression(paste("Single-Pass ", t[p1], " (ms)"))
  } else {
    expression(paste("Perturbed ", t[pp], " (ms)"))
  }
  
  yLabel <- expression("CCS (" * "Å"^2 * ")")
  
  # Initialize outputs
  calculatedCCS <- numeric(1)
  plots <- list()
  
  # Extract power law coefficients a and b
  coeffs <- extract_power_coefficients(curveExpression)
  a <- coeffs$a
  b <- coeffs$b
  
  tryCatch({
    center <- periodicTime
    calculatedCCS <- eval(curveExpression, list(t = center))
    
    cat("Calculated CCS for", dataAnalyteName, ":", calculatedCCS, "\n")
    
    # Determine limits
    yMin <- 50
    yBuffer <- 0.1 * calculatedCCS
    yMax <- max(400, calculatedCCS + yBuffer)
    yLimits <- c(yMin, yMax)
    
    xMin <- (yLimits[1] / a)^(1 / b)
    xMax <- (yLimits[2] / a)^(1 / b)
    xLimits <- c(xMin, xMax)
    
    labelText <- paste0(
      "Calculated CCS: ", format(round(calculatedCCS, decimals), nsmall = decimals)
    )
    
    plotData <- data.frame(
      x = center,
      y = calculatedCCS,
      label = labelText
    )
    
    p <- ggplot(plotData, aes(x = x, y = y)) +
      geom_point(color = "blue", size = 3) +
      annotate("text", x = mean(xLimits), y = yLimits[1] + 0.05 * diff(yLimits),
               label = plotData$label, size = 4, hjust = 0.5, vjust = 0) +
      stat_function(fun = function(t) eval(curveExpression, list(t = t)),
                    color = "blue", xlim = xLimits) +
      coord_cartesian(ylim = yLimits) +
      labs(title = dataAnalyteName, x = xLabel, y = yLabel) +
      theme(
        plot.title = element_text(size = 22, hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)
      )
    
    plots[[1]] <- p
    print(p)
    
  }, error = function(e) {
    cat("Error processing", dataAnalyteName, ":", e$message, "\n")
  })
  
  
  return(list(
    calculatedCCS = calculatedCCS,
    plotObject = plots
  ))
}

# Compute CCS for unknown analytes using Bush & Ruotolo model
calculate_ruotolo_ccs_unknown <- function(tpp, mz, MW, z, A, X, gas = "Nitrogen", c_param = 1.41) {
  
  # Calculate reduced mass
  gas_mass <- ifelse(gas == "Nitrogen", 28.0134, 4.0026)
  mu <- (MW * gas_mass) / (MW + gas_mass)
  
  # Compute tD'
  tD_prime <- tpp - 0.001 * c_param * sqrt(mz)
  
  # Compute normalized CCS Omega'
  Omega_prime <- A * (tD_prime^X)
  
  # Convert normalized CCS back to real CCS
  CCS <- Omega_prime / sqrt(mu) * abs(z)
  
  return(list(
    CCS = CCS,
    tD_prime = tD_prime,
    Omega_prime = Omega_prime
  ))
}



extract_coefficients_from_string <- function(equationString) {
  # Log the input string for debugging
  #cat("Trying to extract coefficients from string: '", equationString, "'\n")
  
  # Remove 'CCS=' or similar
  equationString <- gsub("CCS\\s*=\\s*", "", equationString)
  equationString <- gsub("\\s+", "", equationString)  # <-- critical
  
  # Match pattern: a * t^b
  match <- regexec("([0-9.eE+-]+)\\*t\\^([0-9.eE+-]+)", equationString)
  parts <- regmatches(equationString, match)[[1]]
  
  # Error catching for equation format
  if (length(parts) < 3) {
    warning(paste("Malformed equation string:", equationString))
    stop(paste("Malformed equation string:", equationString))
  }
  
  a <- as.numeric(parts[2])
  b <- as.numeric(parts[3])
  
  if (is.na(a) || is.na(b)) {
    stop("Error: Failed to convert coefficients to numeric.")
  }
  
  return(list(a = a, b = b))
}
# Extract coefficients from power curves
extract_power_coefficients <- function(expr) {
  if (!is.call(expr)) {
    stop("curveExpression must be a call (e.g., a * t^b)")
  }
  
  # Expecting: a * t^b
  if (expr[[1]] != as.name("*")) {
    stop("curveExpression must be of the form: a * t^b")
  }
  
  aPart <- expr[[2]]
  powerPart <- expr[[3]]
  
  if (!is.call(powerPart) || powerPart[[1]] != as.name("^")) {
    stop("curveExpression must be of the form: a * t^b")
  }
  
  tVar <- powerPart[[2]]
  bPart <- powerPart[[3]]
  
  # Check that it's a * t^b exactly
  if (!identical(tVar, as.name("t"))) {
    stop("curveExpression must use 't' as the variable")
  }
  
  a <- eval(aPart)
  b <- eval(bPart)
  
  return(list(a = a, b = b))
}

# Function to sanitize the analyte names so they can be read by R.
sanitize_names <- function(analyte) {
  gsub("[[:punct:]/\\]", "_", analyte)
}
# Prepare user entered data when the Ruotolo method is selected.
prepare_ruotolo_metadata <- function(
    calibrantSource,
    selectedCalibrants,
    useBuiltIn,
    mz_input,
    MW_input,
    z_input,
    gas_input,
    c_param,
    calibrantTables
) {
  cat("---- ENTER prepare_ruotolo_metadata() ----\n")
  cat("useBuiltIn:", useBuiltIn, "\n")
  cat("calibrantSource:", calibrantSource, "\n")
  
  if (isTRUE(useBuiltIn)) {
    cat("Using built-in calibrants\n")
    
    df <- calibrantTables[[calibrantSource]]
    cat("Loaded calibrant table rows:", nrow(df), "\n")
    
    df <- df[df$name %in% selectedCalibrants, ]
    cat("After filtering, rows:", nrow(df), "\n")
    
    return(list(
      CCS_lit = df$CCS,
      mz      = df$mz,
      MW      = df$MW,
      z       = abs(as.numeric(df$z)),
      gas     = "Nitrogen",
      c_param = c_param
    ))
  }
  
  # Manual mode
  cat("Manual mode\n")
  cat("mz_input:", mz_input, "\n")
  cat("MW_input:", MW_input, "\n")
  cat("z_input:", z_input, "\n")
  
  mz_vec <- as.numeric(unlist(strsplit(mz_input, ",\\s*")))
  MW_vec <- as.numeric(unlist(strsplit(MW_input, ",\\s*")))
  z_vec  <- as.numeric(z_input)
  
  if (any(is.na(mz_vec))) {
    stop("Manual Ruotolo m/z values must be numeric and comma separated.")
  }
  
  if (any(is.na(MW_vec))) {
    stop("Manual Ruotolo MW values must be numeric and comma separated.")
  }
  
  if (is.na(z_vec)) {
    stop("Manual Ruotolo charge state must be numeric.")
  }
  
  if (length(mz_vec) != length(MW_vec)) {
    stop("Manual Ruotolo m/z and MW inputs must contain the same number of values.")
  }
  
  return(list(
    CCS_lit = NULL,  # filled later
    mz      = mz_vec,
    MW      = MW_vec,
    z       = rep(abs(z_vec), length(mz_vec)),
    gas     = gas_input,
    c_param = c_param
  ))
}

run_ruotolo_calibration <- function(
    tpp,
    CCS_lit,
    mz,
    MW,
    z,
    gas = c("Nitrogen", "Helium"),
    c_param = 1.41,
    decimals = 4
) {
  cat("---- ENTER run_ruotolo_calibration() ----\n")
  cat("length(tpp):", length(tpp), "\n")
  cat("length(CCS_lit):", length(CCS_lit), "\n")
  cat("length(mz):", length(mz), "\n")
  cat("length(MW):", length(MW), "\n")
  cat("length(z):", length(z), "\n")
  
  gas <- match.arg(gas)
  
  
  tD_prime <- tpp - 0.001 * c_param * sqrt(mz)
  cat("Min tD':", min(tD_prime), "\n")
  
  gas_mass <- ifelse(gas == "Nitrogen", 28.0134, 4.0026)
  mu <- (MW * gas_mass) / (MW + gas_mass)
  
  Omega_prime <- CCS_lit * sqrt(mu) / abs(z)
  cat("Omega_prime OK? min:", min(Omega_prime), "\n")
  
  log_tD_prime <- log(tD_prime)
  log_Omega_prime <- log(Omega_prime)
  
  fit <- lm(log_Omega_prime ~ log_tD_prime)
  A <- exp(coef(fit)[1])
  X <- coef(fit)[2]
  
  # fitted curve
  log_Omega_prime_fit <- predict(fit)
  
  df_plot <- data.frame(
    log_tD_prime        = log_tD_prime,
    log_Omega_prime     = log_Omega_prime,
    log_Omega_prime_fit = log_Omega_prime_fit
  )
  
  r2 <- summary(fit)$r.squared
  r2Text <- format(round(r2, decimals), nsmall = decimals)
  
  A_text <- format(round(A, decimals), nsmall = decimals)
  X_text <- format(round(X, decimals), nsmall = decimals)
  
  eq_label <- paste0(
    "ln(Omega*\"'\") == ln(", A_text, ") + ",
    X_text, " * ln(t[D]*\"'\")"
  )
  
  r2_label <- paste0("R^2 == ", r2Text)
  
  # Compute layout values
  x_left  <- min(df_plot$log_tD_prime)
  y_top   <- max(df_plot$log_Omega_prime)
  y_range <- y_top - min(df_plot$log_Omega_prime)
  
  # Build Ruotolo plot
  
  # First make a basic ggplot without text
  p_base <- ggplot(df_plot, aes(x = log_tD_prime, y = log_Omega_prime)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, color = "blue") +
    labs(
      x = expression(ln(t[D]^"'" * " (ms)")),
      y = expression(ln(Omega*"'"*""))
    ) +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x  = element_text(size = 14),
      axis.text.y  = element_text(size = 14),
      panel.background = element_rect(fill = "white", color = "black"),
      panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
      panel.grid.minor = element_line(color = "lightgray", linewidth = 0.1),
      plot.margin = margin(10, 10, 10, 10, "pt")
    )
  
  # === Build so we can read actual axis limits after smoothing ===
  gb <- ggplot_build(p_base)
  x_rng <- gb$layout$panel_params[[1]]$x.range
  y_rng <- gb$layout$panel_params[[1]]$y.range
  
  # Upper-left placement coordinates
  x_left <- x_rng[1] + 0.02 * diff(x_rng)
  y_top  <- y_rng[2] - 0.02 * diff(y_rng)
  
  # Equation & R² labels
  eq_label <- paste0(
    "ln(Omega*\"'\" ) == ln(", A_text, ") + ",
    X_text, " * ln(t[D]*\"'\")"
  )
  
  r2_label <- paste0("R^2 == ", r2Text)
  
  # Final plot with annotation
  p_ruotolo <- p_base +
    annotate("text",
             x = x_left,
             y = y_top,
             label = eq_label,
             parse = TRUE,
             size = 5,
             hjust = 0,
             vjust = 1) +
    annotate("text",
             x = x_left,
             y = y_top - 0.10 * diff(y_rng),
             label = r2_label,
             parse = TRUE,
             size = 5,
             hjust = 0,
             vjust = 1)
  
  print(p_ruotolo)
  
  summary_df <- data.frame(
    tpp          = tpp,
    tD_prime     = tD_prime,
    CCS_lit      = CCS_lit,
    mz           = mz,
    MW           = MW,
    z            = z,
    mu           = mu,
    Omega_prime  = Omega_prime,
    Omega_prime_fit = exp(log_Omega_prime_fit),
    Residual_percent = 100 * (exp(log_Omega_prime_fit) - Omega_prime) / Omega_prime
  )
  
  return(list(
    A = A,
    X = X,
    fit = fit,
    plot = p_ruotolo,
    summary = summary_df
  ))
}

# Calibration Import/Export Helpers (RDS) Not currently active
# MPCCS_CAL_SCHEMA_VERSION <- "mpccs_cal_v1"
# 
# # Small helper to check a ggplot-like object not currently used. 
# # .is_plot_object <- function(x) {
# #   inherits(x, "gg") || inherits(x, "ggplot")
# # }
# 
# # Validate the structure of a saved calibration object (read from .rds)
# # Returns TRUE if valid; otherwise returns a character error message.
# validate_calibration_rds_object <- function(obj) {
#   if (is.null(obj)) return("Calibration file is empty (NULL).")
#   if (!is.list(obj)) return("Calibration file is not a list.")
#   
#   if (is.null(obj$schema_version) || !is.character(obj$schema_version) || length(obj$schema_version) != 1) {
#     return("Missing or invalid 'schema_version' in calibration file.")
#   }
#   if (obj$schema_version != MPCCS_CAL_SCHEMA_VERSION) {
#     return(paste0(
#       "Unsupported calibration file schema: '", obj$schema_version,
#       "'. Expected '", MPCCS_CAL_SCHEMA_VERSION, "'."
#     ))
#   }
#   
#   if (is.null(obj$slot_payload) || !is.list(obj$slot_payload)) {
#     return("Missing or invalid 'slot_payload' in calibration file.")
#   }
#   
#   payload <- obj$slot_payload
#   
#   # Required fields used by your app in Process Data + Saved Curves
#   required_fields <- c("singlePassEq", "multiPassEq", "plots")
#   missing <- setdiff(required_fields, names(payload))
#   if (length(missing) > 0) {
#     return(paste("Missing required fields:", paste(missing, collapse = ", ")))
#   }
#   
#   if (!is.character(payload$singlePassEq) || length(payload$singlePassEq) != 1) {
#     return("singlePassEq must be a single string.")
#   }
#   if (!is.character(payload$multiPassEq) || length(payload$multiPassEq) != 1) {
#     return("multiPassEq must be a single string.")
#   }
#   
#   if (!is.list(payload$plots)) return("plots must be a list.")
#   if (is.null(payload$plots$Single) || is.null(payload$plots$Multi)) {
#     return("plots must include at least $Single and $Multi.")
#   }
#   
#   # Not strictly required, but helps catch corrupted files early
#   if (!.is_plot_object(payload$plots$Single)) return("plots$Single does not look like a ggplot object.")
#   if (!.is_plot_object(payload$plots$Multi))  return("plots$Multi does not look like a ggplot object.")
#   
#   # Optional Ruotolo fields (only validate types if present)
#   optional_numeric_single <- c("ruotolo_A", "ruotolo_X", "ruotolo_c")
#   for (nm in optional_numeric_single) {
#     if (!is.null(payload[[nm]]) && !(is.numeric(payload[[nm]]) && length(payload[[nm]]) == 1)) {
#       return(paste0(nm, " must be a single numeric value when present."))
#     }
#   }
#   if (!is.null(payload$ruotolo_gas) && !(is.character(payload$ruotolo_gas) && length(payload$ruotolo_gas) == 1)) {
#     return("ruotolo_gas must be a single string when present.")
#   }
#   
#   optional_numeric_vec <- c("ruotolo_mz", "ruotolo_MW", "ruotolo_z", "ruotolo_CCS")
#   for (nm in optional_numeric_vec) {
#     if (!is.null(payload[[nm]]) && !is.numeric(payload[[nm]])) {
#       return(paste0(nm, " must be numeric when present."))
#     }
#   }
#   
#   if (!is.null(payload$ruotolo_plot) && !.is_plot_object(payload$ruotolo_plot)) {
#     return("ruotolo_plot does not look like a ggplot object when present.")
#   }
#   if (!is.null(payload$ruotolo_summary) && !is.data.frame(payload$ruotolo_summary)) {
#     return("ruotolo_summary must be a data.frame when present.")
#   }
#   
#   TRUE
# }
# 
# # Read + validate a calibration .rds file.
# # Returns a list: list(ok=TRUE, payload=<slot_payload>, meta=<obj>) or list(ok=FALSE, error="...")
# read_calibration_rds <- function(path) {
#   obj <- tryCatch(readRDS(path), error = function(e) e)
#   if (inherits(obj, "error")) {
#     return(list(ok = FALSE, error = paste("Failed to read .rds:", obj$message)))
#   }
#   
#   valid <- validate_calibration_rds_object(obj)
#   if (!isTRUE(valid)) {
#     return(list(ok = FALSE, error = valid))
#   }
#   
#   list(ok = TRUE, payload = obj$slot_payload, meta = obj)
# }
# 
# # Build a calibration object ready to write with saveRDS().
# # You pass the slot payload you already store in calibrationInfo$curveX
# build_calibration_rds_object <- function(slot_payload) {
#   if (is.null(slot_payload) || !is.list(slot_payload)) {
#     stop("slot_payload must be a list.")
#   }
#   
#   list(
#     schema_version = MPCCS_CAL_SCHEMA_VERSION,
#     created_at     = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
#     slot_payload   = slot_payload
#   )
# }

validate_imported_calibration <- function(obj) {
  
  # Top-level checks
  if (!is.list(obj)) {
    return("Imported file is not a valid R object.")
  }
  
  if (is.null(obj$type) || obj$type != "Multipass_CCS_Refiner_Calibration") {
    return("File is not a valid Multipass CCS Refiner calibration.")
  }
  
  if (is.null(obj$payload) || !is.list(obj$payload)) {
    return("Calibration payload is missing or corrupted.")
  }
  
  payload <- obj$payload
  
  # Required fields inside payload
  required_fields <- c(
    "singlePassEq",
    "multiPassEq",
    "plots"
  )
  
  missing <- setdiff(required_fields, names(payload))
  if (length(missing) > 0) {
    return(
      paste(
        "Calibration is missing required fields:",
        paste(missing, collapse = ", ")
      )
    )
  }
  
  # Optional: sanity check plots
  if (!is.list(payload$plots) ||
      !all(c("Single", "Multi") %in% names(payload$plots))) {
    return("Calibration plots are missing or malformed.")
  }
  
  TRUE
}

# Functions for Rp and Rpp calculations
# ---- FWHM / Rp / Rpp (time domain) ----
FWHM_from_sigma <- function(sigma) {
  2 * sqrt(2 * log(2)) * sigma   # 2.355·σ
}

Rp_time <- function(mu, FWHM) {
  mu / FWHM
}

get_peak_metrics <- function(mdl) {
  if (is.null(mdl)) return(NULL)
  mobj <- if (!is.null(mdl$model)) mdl$model else mdl
  co <- try(coef(mobj), silent = TRUE)
  if (inherits(co, "try-error")) return(NULL)
  
  mu    <- unname(co["mu"])
  sigma <- abs(unname(co["sigma"]))
  FWHM  <- FWHM_from_sigma(sigma)
  
  list(
    mu = mu,
    sigma = sigma,
    FWHM = FWHM,
    Rp_time = Rp_time(mu, FWHM)
  )
}

compute_Rpp_time <- function(mdlA, mdlB) {
  A <- get_peak_metrics(mdlA)
  B <- get_peak_metrics(mdlB)
  if (is.null(A) || is.null(B)) return(NA_real_)
  
  if (B$mu < A$mu) { tmp <- A; A <- B; B <- tmp }
  
  1.18 * (B$mu - A$mu) / (A$FWHM + B$FWHM)
}

compute_all_Rpp <- function(models) {
  if (is.null(models) || length(models) < 2) {
    return(data.frame(Message = "Fit at least two peaks to compute Rpp."))
  }
  
  idx <- which(vapply(models, function(m) !is.null(m), logical(1)))
  
  if (length(idx) < 2) {
    return(data.frame(Message = "Fit at least two peaks to compute Rpp."))
  }
  
  out <- list()
  k <- 1
  
  # Only loop through valid unique peak pairs
  for (i in seq_len(length(idx) - 1)) {
    for (j in seq(i + 1, length(idx))) {
      
      a <- idx[i]
      b <- idx[j]
      
      A <- get_peak_metrics(models[[a]])
      B <- get_peak_metrics(models[[b]])
      
      if (is.null(A) || is.null(B)) next
      
      if (B$mu < A$mu) {
        tmp <- A
        A <- B
        B <- tmp
        pa <- b
        pb <- a
      } else {
        pa <- a
        pb <- b
      }
      
      out[[k]] <- data.frame(
        PeakA = pa,
        PeakB = pb,
        tdA_ms = A$mu,
        FWHM_A_ms = A$FWHM,
        tdB_ms = B$mu,
        FWHM_B_ms = B$FWHM,
        delta_t_ms = B$mu - A$mu,
        FWHM_sum_ms = A$FWHM + B$FWHM,
        Rpp = 1.18 * (B$mu - A$mu) / (A$FWHM + B$FWHM),
        row.names = NULL
      )
      
      k <- k + 1
    }
  }
  
  if (length(out) == 0) {
    return(data.frame(Message = "Not enough valid fits to compute Rpp."))
  }
  
  do.call(rbind, out)
}
# This function is the earlier version of the function defined below.
# auto_guess_peaks <- function(x, y, min_rel_height = 0.10, smooth_k = 7) {
#   
#   # Smooth signal to suppress noise
#   y_smooth <- stats::filter(y, rep(1/smooth_k, smooth_k), sides = 2)
#   y_smooth[is.na(y_smooth)] <- y[is.na(y_smooth)]
#   
#   # Find local maxima
#   pk <- findpeaks(
#     y_smooth,
#     minpeakheight = min_rel_height * max(y_smooth, na.rm = TRUE)
#   )
#   
#   if (is.null(pk)) return(list())
#   
#   # pk columns: height, index, left base, right base
#   guesses <- lapply(seq_len(nrow(pk)), function(i) {
#     idx <- pk[i, 2]
#     
#     mu <- x[idx]
#     A  <- pk[i, 1]
#     
#     # crude sigma estimate from half-height width
#     half_height <- A / 2
#     left  <- max(which(y_smooth[1:idx] <= half_height), 1)
#     right <- min(idx + which(y_smooth[idx:length(y_smooth)] <= half_height)[1], length(x))
#     
#     sigma <- abs(x[right] - x[left]) / 2.355
#     sigma <- ifelse(is.finite(sigma) && sigma > 0, sigma, diff(range(x))/50)
#     
#     list(mu = mu, A = A, sigma = sigma)
#   })
#   
#   guesses
# }
# 
# fit_single_peak_local <- function(x, y, guess, k = 3) {
#   
#   win <- (x >= (guess$mu - k * guess$sigma)) &
#     (x <= (guess$mu + k * guess$sigma))
#   
#   xw <- x[win]
#   yw <- y[win]
#   
#   if (length(xw) < 10) return(NULL)
#   
#   tryCatch({
#     minpack.lm::nlsLM(
#       yw ~ A * exp(-((xw - mu)^2) / (2 * sigma^2)),
#       start = list(A = guess$A, mu = guess$mu, sigma = guess$sigma),
#       control = minpack.lm::nls.lm.control(maxiter = 200)
#     )
#   }, error = function(e) NULL)
# }

auto_guess_peaks <- function(arrivalTimes,
                             intensities,
                             min_rel_height = 0.10,
                             min_separation = 0.3) {
  
  n <- length(intensities)
  if (n < 5) return(list())
  
  maxI <- max(intensities, na.rm = TRUE)
  thresh <- min_rel_height * maxI
  
  # --- Find local maxima ---
  idx <- which(
    intensities > thresh &
      intensities > c(-Inf, intensities[-n]) &
      intensities > c(intensities[-1], -Inf)
  )
  
  if (!length(idx)) return(list())
  
  # --- Enforce minimum separation ---
  keep <- logical(length(idx))
  last_mu <- -Inf
  
  for (i in seq_along(idx)) {
    mu_i <- arrivalTimes[idx[i]]
    if (mu_i - last_mu >= min_separation) {
      keep[i] <- TRUE
      last_mu <- mu_i
    }
  }
  
  idx <- idx[keep]
  if (!length(idx)) return(list())
  
  # --- Build guesses ---
  guesses <- vector("list", length(idx))
  
  for (i in seq_along(idx)) {
    mu <- arrivalTimes[idx[i]]
    A  <- intensities[idx[i]]
    
    # crude σ estimate using local width
    left  <- max(which(arrivalTimes < mu & intensities < A/2), na.rm = TRUE)
    right <- min(which(arrivalTimes > mu & intensities < A/2), na.rm = TRUE)
    
    if (is.finite(left) && is.finite(right)) {
      sigma <- abs(arrivalTimes[right] - arrivalTimes[left]) / 2.355
    } else {
      sigma <- 0.15
    }
    
    guesses[[i]] <- list(
      amplitude   = A,
      arrivalTime = mu,
      stdDev      = max(sigma, 0.05)
    )
  }
  
  guesses
}
# Code for features not in this version of the application
# ccs_xia <- function(t, a, b) {
#   a * t^b
# }
# 
# dccs_dt_xia <- function(t, a, b) {
#   a * b * t^(b - 1)
# }
# 
# ccs_ruotolo <- function(tpp, A, X, mz, MW, z, gas = "Nitrogen", c_param = 1.41) {
#   gas_mass <- ifelse(gas == "Nitrogen", 28.0134, 4.0026)
#   mu_red <- (MW * gas_mass) / (MW + gas_mass)
#   
#   tD_prime <- tpp - 0.001 * c_param * sqrt(mz)
#   Omega_prime <- A * tD_prime^X
#   
#   Omega_prime * abs(z) / sqrt(mu_red)
# }
# 
# dccs_dt_ruotolo <- function(tpp, A, X, mz, MW, z, gas = "Nitrogen", c_param = 1.41) {
#   gas_mass <- ifelse(gas == "Nitrogen", 28.0134, 4.0026)
#   mu_red <- (MW * gas_mass) / (MW + gas_mass)
#   
#   tD_prime <- tpp - 0.001 * c_param * sqrt(mz)
#   
#   abs(z) / sqrt(mu_red) * A * X * tD_prime^(X - 1)
# }
# 
# compute_peak_metrics_ccs <- function(mu_t, fwhm_t, cal) {
#   
#   if (cal$method == "Xia") {
#     ccs_mu <- ccs_xia(mu_t, cal$a, cal$b)
#     slope  <- dccs_dt_xia(mu_t, cal$a, cal$b)
#     
#   } else if (cal$method == "Ruotolo") {
#     ccs_mu <- ccs_ruotolo(
#       mu_t, cal$A, cal$X,
#       cal$mz, cal$MW, cal$z,
#       cal$gas, cal$c_param
#     )
#     slope <- dccs_dt_ruotolo(
#       mu_t, cal$A, cal$X,
#       cal$mz, cal$MW, cal$z,
#       cal$gas, cal$c_param
#     )
#   } else {
#     return(NULL)
#   }
#   
#   fwhm_ccs <- abs(slope) * fwhm_t
#   rp_ccs   <- ccs_mu / fwhm_ccs
#   
#   list(
#     CCS = ccs_mu,
#     FWHM_CCS = fwhm_ccs,
#     Rp_CCS = rp_ccs
#   )
# }
# 
# compute_Rpp_ccs <- function(A, B) {
#   
#   if (B$CCS < A$CCS) {
#     tmp <- A; A <- B; B <- tmp
#   }
#   
#   1.18 * (B$CCS - A$CCS) / (A$FWHM_CCS + B$FWHM_CCS)
# }
# 
   equation_string_to_curve_expr <- function(eq_string) {
     ab <- extract_coefficients_from_string(eq_string)
     substitute(a * t^b, list(a = ab$a, b = ab$b))
   }
# End of Defining Functions
