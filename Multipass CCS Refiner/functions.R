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
      title = paste("Arrival time: ", format(round(muCoefficient, decimals), nsmall = 5)),
      subtitle = bquote(t[plain(s)] ~ "(ms): " ~ .(separationTime)~ " | " ~Passes:  .(passes))
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
calculate_multiple_arrival_times <- function(arrivalTimes, intensities, peakGuesses) {
  models <- list()
  
  for (i in 1:length(peakGuesses)) {
    amplitudeGuess <- peakGuesses[[i]]$amplitude
    muGuess <- peakGuesses[[i]]$arrivalTime
    sigmaGuess <- peakGuesses[[i]]$stdDev
    
    lowerMu <- muGuess - 1.5
    upperMu <- muGuess + 1.5
    
    tryCatch({
      modelATD <- nls(intensities ~ A * exp(-((arrivalTimes - mu)^2) / (2 * sigma^2)), 
                      start = list(A = amplitudeGuess, mu = muGuess, sigma = sigmaGuess),
                      lower = c(0, lowerMu, 0.01),
                      upper = c(Inf, upperMu, 10),
                      algorithm = 'port',
                      control = nls.control(warnOnly = TRUE))
      models[[i]] <- modelATD
    }, error = function(e) {
      print(paste("Error fitting model for peak", i, ":", e$message))  # Debug print
      models[[i]] <- NULL
    })
  }
  return(models)
}

# Plotting function for users to see where Gaussian distributions are being constructed for their peaks.
plot_multiple_gaussians <- function(arrivalTimes, intensities, models, decimals,highlighted_peak = NULL, sigmaRange = NULL) {
  data <- data.frame(arrivalTimes = arrivalTimes, intensities = intensities)
  
  # Initialize the plot.
  p <- ggplot(data, aes(x = arrivalTimes, y = intensities)) +
    geom_point(shape = 21, color = "black", fill = "white", size = 3) +
    labs(x = "Arrival Time (ms)", y = "") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA),
      axis.text.x = element_text(size = 16),        
      axis.text.y = element_text(size = 16),        
      axis.title.x = element_text(size = 16),
      panel.grid = element_blank()
    )
  
  
  # Define colors for multiple peaks. Choosing colorblind friendly colors.
  peakColors <- c("#440154", "#1f9e89", "#6ece58", "#482878", "#000000", "#3e4989", "#26828e",
                  "#35b779", "#b5de2b", "#fde725") 
  
  # Add fitted Gaussians to the plot using a for loop to add any number of peaks.
  for (i in seq_along(models)) {
    if (!is.null(models[[i]])) {
      fittedValues <- predict(models[[i]])
      muCoefficient <- coef(models[[i]])["mu"]
      
      peakData <- data.frame(arrivalTimes = arrivalTimes, fittedValues = fittedValues, peak = i)
      
      # Add Gaussian curve for each peak
      p <- p + geom_line(data = peakData, aes(x = arrivalTimes, y = fittedValues, color = as.factor(peak)), 
                         linewidth = 1.25) +
        annotate("text", x = muCoefficient + 0.5, y = max(fittedValues), 
                 label = paste(" μ", i, ": ", round(muCoefficient, decimals)," ms"), hjust = -0.25, vjust = 1, size = 8)
    }
  }
  # Create vertical lines to show the user what data they will keep.
  if (!is.null(highlighted_peak) && !is.null(sigmaRange)) {
    if (!is.null(models[[highlighted_peak]])) {
      mu <- coef(models[[highlighted_peak]])["mu"]
      sigma <- coef(models[[highlighted_peak]])["sigma"]
      lowerBound <- mu - (sigmaRange * sigma)
      upperBound <- mu + (sigmaRange * sigma)
      
      p <- p + 
        geom_vline(xintercept = lowerBound, linetype = "dashed", color = "red", linewidth = 1) +
        geom_vline(xintercept = upperBound, linetype = "dashed", color = "red", linewidth = 1) +
        annotate("text", x = lowerBound, y = 0.75*max(intensities), label = paste0("-", sigmaRange, "σ"),
                 hjust = 1.1, vjust = 0.5, color = "red", size = 8) +
        annotate("text", x = upperBound, y = 0.75*max(intensities), label = paste0("+", sigmaRange, "σ"),
                 hjust = -0.1, vjust = 0.5, color = "red", size = 8)
    }
  }
  p <- p + scale_color_manual(values = peakColors, name = "Peak Number")
  
  return(p)
}
# Function to create linear plots to obtain perturbation corrected periodic drift times. 
# All ATDs across all separation times will be used to make these plots.
plot_linear_chart <- function(x, y, analyteName, t1, bypassTime, decimals) {
  # Prepare data frame and fit linear model.
  df <- data.frame(x = as.numeric(x), y = as.numeric(y))
  fit <- lm(y ~ x, data = df)
  
  #Save the coefficients of the linear model
  interceptCoefficient <- format(round(coef(fit)[1], decimals),nsmall = decimals)
  slopeCoefficient <- format(round(coef(fit)[2], decimals), nsmall = decimals)
  
  # Create the x and y labels
  xLabel = expression(paste(t[s], " / n"))
  yLabel = expression(paste("(", t[nd], " - ", t[s], ") / n"))
  
  # Set text labels for equations and other annotations that will display on the plots.
  equationLabel <- bquote(y == .(slopeCoefficient) * x + .(interceptCoefficient))  # Using bquote for formatting
  rSquaredLabel <- bquote(R^2 == .(format(round(summary(fit)$r.squared, decimals)),nsmall = decimals))  # Format R^2 using bquote
  
  # Create formatted expressions for tpp and tp1 so that they appear with proper subscripts.
  tppLabel <- bquote(t[pp] == .(interceptCoefficient))  # Subscript for tpp
  tp1Label <- bquote(t[p1] == .(format(round(t1 - bypassTime, decimals)),nsmall = decimals))  # Subscript for tp1
  
  
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
    
    annotate("text", 
             x = max(df$x), y = max(df$y),  # Place equation in the upper right.
             label = equationLabel, 
             hjust = 1, vjust = 1, size = 5) +
    
    annotate("text", 
             x = max(df$x), y = 0.85*max(df$y),  # Position R^2 below the equation.
             label = rSquaredLabel, 
             hjust = 1, vjust = 1, size = 5) +
    
    # Place tp1 in the lower-left corner.
    annotate("text", 
             x = min(df$x), y = 0.25*max(df$y),
             label = tp1Label,
             hjust = 0, vjust = 0, size = 5) +
    
    # Place tpp annotation below tp1.
    annotate("text", 
             x = min(df$x), y = 0.1*max(df$y),
             label = tppLabel,
             hjust = 0, vjust = 0, size = 5)
  
  
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
  separationTimesRaw <- read.xlsx(filePath, colClasses = "numeric", sheetName = sheetName, startRow = 1, endRow = 1, fillMergedCells = TRUE, as.data.frame = FALSE)
  separationTimes <- as.numeric(separationTimesRaw[!is.na(separationTimesRaw)])
  
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
  
  # Pre-compute coefficients to be displayed on the plots.
  powerLawCoefficient1 <- format(round(exp(coef(powerLawModel)[1]), decimals), nsmall = decimals)
  powerLawCoefficient2 <- format(round(coef(powerLawModel)[2], decimals), nsmall = decimals)
  
  # Create the equation using an expression and insert the pre-computed coefficients.
  equation <- paste("CCS =", format(round(exp(coef(powerLawModel)[1]), decimals), nsmall = decimals), "* t^", format(round(coef(powerLawModel)[2], decimals), nsmall = decimals))
  equation_plot_format <- bquote(
    CCS == .(powerLawCoefficient1) * t[p]^.(powerLawCoefficient2)
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
    annotation_custom(
      grob = textGrob(equation_plot_format, 
                      hjust = 0, vjust = 1, gp = gpar(fontsize = 20)), 
      xmin = median(df$exactArrivals), xmax = median(df$exactArrivals), 
      ymin = sort(df$CCS, partial = 2)[2], ymax = sort(df$CCS, partial = 2)[2]
    )
  
  print(p)
  curveExpression <- substitute(a * t^b, list(a = exp(coef(powerLawModel)[1]), b = coef(powerLawModel)[2]))
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
      y_min <- 50
      y_buffer <- 0.1 * calculatedCCS
      y_max <- max(400, calculatedCCS + y_buffer)
      y_limits <- c(y_min, y_max)
      
      x_min <- (y_limits[1] / a)^(1 / b)
      x_max <- (y_limits[2] / a)^(1 / b)
      x_limits <- c(x_min, x_max)
      
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
        annotate("text", x = mean(x_limits), y = y_limits[1] + 0.05 * diff(y_limits),
                 label = plotData$label, size = 4, hjust = 0.5, vjust = 0) +
        stat_function(fun = function(t) eval(curveExpression, list(t = t)),
                      color = "blue", xlim = x_limits) +
        coord_cartesian(ylim = y_limits) +
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
      cat("Error processing", dataAnalyteNames, ":", e$message, "\n")
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
      y_min <- 50
      y_buffer <- 0.1 * calculatedCCS
      y_max <- max(400, calculatedCCS + y_buffer)
      y_limits <- c(y_min, y_max)
      
      x_min <- (y_limits[1] / a)^(1 / b)
      x_max <- (y_limits[2] / a)^(1 / b)
      x_limits <- c(x_min, x_max)
      
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
        annotate("text", x = mean(x_limits), y = y_limits[1] + 0.05 * diff(y_limits),
                 label = plotData$label, size = 4, hjust = 0.5, vjust = 0) +
        stat_function(fun = function(t) eval(curveExpression, list(t = t)),
                      color = "blue", xlim = x_limits) +
        coord_cartesian(ylim = y_limits) +
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

extract_coefficients_from_string <- function(equationString) {
  # Log the input string for debugging
  #cat("Trying to extract coefficients from string: '", equationString, "'\n")
  
  # Remove 'CCS=' or similar
  equationString <- gsub("CCS\\s*=\\s*", "", equationString)
  
  # Match pattern: a * t^b
  match <- regexec("([0-9.eE+-]+)\\*t\\^([0-9.eE+-]+)", equationString)
  parts <- regmatches(equationString, match)[[1]]
  
  # Error catching for equation format
  if (length(parts) < 3) {
    warning(paste("Malformed equation string:", equationString))
    stop("Error: Equation must be in the form 'a * t^b'")
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
  
  a_part <- expr[[2]]
  pow_part <- expr[[3]]
  
  if (!is.call(pow_part) || pow_part[[1]] != as.name("^")) {
    stop("curveExpression must be of the form: a * t^b")
  }
  
  t_var <- pow_part[[2]]
  b_part <- pow_part[[3]]
  
  # Check that it's a * t^b exactly
  if (!identical(t_var, as.name("t"))) {
    stop("curveExpression must use 't' as the variable")
  }
  
  a <- eval(a_part)
  b <- eval(b_part)
  
  return(list(a = a, b = b))
}

# Function to sanitize the analyte names so they can be read by R.
sanitize_names <- function(analyte) {
  gsub("[[:punct:]/\\]", "_", analyte)
}
# End of Defining Functions