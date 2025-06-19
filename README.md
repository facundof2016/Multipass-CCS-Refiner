# Multipass CCS Refiner

The **Multipass CCS Refiner** enables users to perform multipass CCS calibration in cyclic ion mobility (cIM) instruments following the **Lin and Costello approach** ([DOI: 10.1021/acs.analchem.4c01758](https://pubs.acs.org/doi/10.1021/acs.analchem.4c01758)). This approach reduces the need for carefully selected separation times and enables accurate multipass CCS determinations under user-defined separation conditions while accounting for artifacts underlying fluctuations in ion drift times.

The application is structured into four core modules in the form of .R scripts:
1. **functions.R** for data processing, plotting and calculations.
2. **ui.R** for data input.
3. **server.R** for integrating user input with backend operations.
4. **app.R** for combining all elements for real-time interactive execution via the web or locally.

The **Multipass CCS Refiner** was developed using:
- **R version 4.5.0** (R Core Team, 2025)
- **RStudio version 2025.05.1+513** (Posit, 2025)
- **Shiny version 1.10.0** (Posit, 2024)

---

# Available Formats

The **Multipass CCS Refiner** is available online and locally.

## 1. Running the App Online

If you prefer not to worry about setup or installation, you can access the **Multipass CCS Refiner** directly through the hosted version on **Posit**. This is ideal for quick usage and small-scale analysis.

**Access the app here**:  
[Multipass CCS Refiner](https://ericgier.shinyapps.io/Multipass-CCS-Refiner/)

**Note**:
- No packages or environments are required for online use.
- The app will disconnect after approximately **15 minutes** of inactivity.

## 2. Running the App Locally

If you prefer to run the Multipass CCS Refiner locally, use the application for longer periods of inactivity, or modify the code for your own workflow, follow the steps below to download the code from GitHub and run the application on your machine.

### Prerequisites
- **R version 4.5.0** or later must be installed on your computer.  
  Download R from the official R Project website: [https://cran.r-project.org/bin/windows/base/](https://cran.r-project.org/bin/windows/base/)
  
- **RStudio** (version 2025.05.1+513 or later) is recommended to open and run the app.  
  Download RStudio from [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/).

### Install Required Packages
The app depends on several R packages, which can be installed via the R console:

install.packages(c("shiny", "shinythemes", "bslib", "shinyjs", "shinyBS", "DT", "readxl", "xlsx", "ggplot2", "grid", "viridisLite"))

### Clone or Download the Repository
1. Go to the GitHub repository:  
   [Multipass CCS Refiner Github Homepage](https://github.com/facundof2016/Multipass-CCS-Refiner)

2. Use the **green "Code" button** to clone the repository or download it as a ZIP file and extract it.

### Open and Run the Application
1. Open **RStudio** and navigate to the folder where you cloned or downloaded the repository.
2. Launch the application by opening the `App.R` script and clicking the **"Run App"** button within the RStudio interface.

---

# Uploading Data
Data upload functionality is designed for exporting ATDs from **MassLynx** (Waters Inc., Wilmslow, U.K.).
The application includes downloadable example file templates for creating calibration curves, processing experimental data, and separating multiple peak ATDs. For the best results the authors recommend downloading the example files and editing them directly with your data. The templates can be downloaded from either:
- The **“Using the Application”** section within the app's **Introduction tab**, or
- Directly from GitHub in the **Example Data** folder.

## Creating a Calibration Curve

Calibration curves are constructed in the **Create Calibration Curve** tab using a standard with known CCS values. ATDs can be copied and pasted directly from **Mass Lynx** to `.xlsx` files, with the separation time placed in a merged cell above the data columns.

**Data Format**:
- Each feature should be placed in its own separate tab labeled with the word "analyte" followed by a unique compound name.
- Sheets containing data should start with the by-pass, followed by the single-pass ATD with `0` or `0.01` and `2` or `2.0` headers respectively. Following ATDs can be conducted at any separation time and placed in any order within the sheet.

**Required Fields**:
- Analyte names and theoretical CCS values are required to run a calibration; they should be comma-separated and listed in the order in which they appear in the `.xlsx` file.
- A calibration file can be uploaded by using the browse function to navigate to the file's location.

**After calibration**:
- Power curves for both single-pass and multi-pass results will be displayed at the bottom of the page.
- A file containing arrival time calculations and linear plots for each standard in the mixture can be downloaded using the **'Download All Plots'** button.
- The calibration can then be saved to any available calibration curve slot using the **'Save'** button.

## Example Calibration File Information

**Analyte Names**:  
Sulfadimethoxide, Val-Tyr-Val, Terfenadine, Polyalanine (n7), Leucine Enkephalin, Polyalanine (n8), Resperine, Polyalanine (n9), Polyalanine (n10), Polyalanine (n11), Polyalanine (n12), Polyalanine (n13), Polyalanine (n14)

**CCS Values**:  
168.4, 191.7, 228.7, 211.0, 229.8, 228.0, 252.3, 243.0, 256.0, 271.0, 282.0, 294.0, 306.0

---

## Processing Experimental Data

Running experimental data requires that a calibration has been successfully run and saved. Users can select whether to compare experimental data to known CCS values.

**Data Format**:
- Analyte names and CCS values should be comma-separated and placed in the same order as the tabs in the uploaded `.xlsx` file, following the same format as the calibration curve.

**After processing**:
- A summary of results will be displayed in a data table at the bottom of the page.
- A detailed summary of results can be downloaded for all or individual analytes.

### Example File Experimental Information

**Analyte Names**:  
d7-PC (15:0-18:1), d7-LysoPC (18:1), d7-PE (15:0-18:1), d7-LysoPE (18:1), d7-LysoPE (18:1) M+Na, d7-LysoPC (18:1) M+Na

**CCS Values**:  
285.4, 235.0, 273.5, 217.8, 224.3, 238.2

---

### Separating Multiple Peak ATDs

The **Multiple Peak ATD** tab provides an alternative to manually separating ATDs or performing additional slicing experiments.

- **Upload ATDs**: ATDs can be uploaded directly from **MassLynx** as `.xlsx` files, with arrival time data in the first column and intensity data in the second column.
- **Fitting**: The raw data can be plotted to assist with providing initial guess values to locate all peaks within an ATD.
- **Reconstructed ATDs**: Data within a specified standard deviation of the fitted arrival time is retained, and data points outside this range are generated from the fitted Gaussian model. The reconstructed ATD can be downloaded after the peaks are fitted.

---

## Video Tutorial

A **video tutorial** demonstrating how to use the components of the **Multipass CCS Refiner** is available here:  
[Watch the tutorial on YouTube](https://www.youtube.com/watch?v=AXrbocbQGY8&t=0s)

---

## Getting Help

If you have any questions about this application, please email:
- **Eric Gier** (egier3@gatech.edu)
- **Facundo Fernandez** (facundo.fernandez@chemistry.gatech.edu)

Or post to our GitHub page:  
[Multipass CCS Refiner GitHub](https://github.com/facundof2016/Multipass-CCS-Refiner/)
