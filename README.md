# Multipass CCS Refiner

The **Multipass CCS Refiner** enables users to perform multipass CCS calibration in cyclic ion mobility spectrometry (cIMS) instruments following the **Lin and Costello approach** ([DOI: 10.1021/acs.analchem.4c01758](https://pubs.acs.org/doi/10.1021/acs.analchem.4c01758)). This approach reduces the need for carefully selected separation times and enables accurate multipass CCS determinations under user-defined separation conditions while accounting for artifacts underlying fluctuations in ion drift times.

The application is structured into four core modules in the form of .R scripts:
1. **functions.R** for data processing, plotting and calculations.
2. **ui.R** for data input.
3. **server.R** for integrating user input with backend operations.
4. **app.R** for combining all elements for real-time interactive execution via the web or locally.

The **Multipass CCS Refiner** was developed using:
- **R version 4.5.2 (2025-10-31 ucrt)** (R Core Team, 2025)
- **RStudio version 2026.4.0.526** (Posit, 2026)
- **Shiny version 1.10.0** (Posit, 2024)

---

# What's New — May 2026 Update

## Calibration Curve Export and Import
Completed calibration curves can now be exported as `.rds` files and reimported in future sessions, eliminating the need to rerun calibrations. Curves can be exported from the **Saved Calibration Curves** tab and reimported in the **Create Calibration Curves** tab. Pre-built calibration files are available for download from the **Example Data** folder of this repository and from within the app:
- `Major_Mix_Positive_Calibration_2026-05-17.rds`
- `SpheriCal_Calibration_Sodium Adducts_350ms_18V_2026-05-05.rds`

## Built-In Calibrant Reference Tables
Reference tables for Major Mix and SpheriCal Low Mass standards have been added to streamline calibration workflows and reduce manual data entry. These tables include CCS, m/z, and molecular weight values, allowing users to quickly select reference standards directly in the **Create Calibration Curves** tab. When using a built-in calibrant library, ionization mode and mass range are automatically populated when saving a calibration curve based on the calibrants selected.

## Enhanced Calibration Method: Xia + Bush & Ruotolo
An additional calibration workflow based on [Ruotolo et al. (2008)](https://www.nature.com/articles/nprot.2008.78) and [Bush et al. (2010)](https://pubs.acs.org/doi/10.1021/ac1022953) has been added that accounts for mass-dependent flight times, ion charge state, and reduced mass when calculating corrected multipass CCS values. This workflow applies an EDC delay coefficient to correct for mass-dependent flight time contributions. The EDC delay coefficient can be found in the Typhoon folder in MassLynx and is generally between 1.4 and 1.6.

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
- **R version 4.5.2 (2025-10-31 ucrt)** or later must be installed on your computer.  
  Download R from the official R Project website: [https://cran.r-project.org/bin/windows/base/](https://cran.r-project.org/bin/windows/base/)

- **RStudio version 2026.4.0.526** or later is recommended to open and run the app.  
  Download RStudio from [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/).

### Install Required Packages
The app depends on several R packages, which can be installed by copying the following line directly into the R console:

```r
install.packages(c("shiny", "shinythemes", "bslib", "shinyjs", "shinyBS", "DT", "readxl", "ggplot2", "viridisLite", "clipr"))
```

### Clone or Download the Repository
1. Go to the GitHub repository:  
   [Multipass CCS Refiner GitHub Homepage](https://github.com/facundof2016/Multipass-CCS-Refiner)

2. Use the **green "Code" button** to clone the repository or download it as a ZIP file and extract it.

### Open and Run the Application
1. Open **RStudio** and navigate to the folder where you cloned or downloaded the repository.
2. Launch the application by opening the `app.R` script and clicking the **"Run App"** button within the RStudio interface.

---

# Uploading Data

Data upload functionality is designed for exporting ATDs from **MassLynx** (Waters Inc., Wilmslow, U.K.). The application includes downloadable example file templates for creating calibration curves, processing experimental data, and separating multiple peak ATDs. For the best results the authors recommend downloading the example files and editing them directly with your data. The templates can be downloaded from either:
- The **"Using the Application"** section within the app's **Introduction tab**, or
- Directly from GitHub in the **Example Data** folder.

## Creating a Calibration Curve

Calibration curves are constructed in the **Create Calibration Curve** tab using a standard with known CCS values. ATDs can be copied and pasted directly from **MassLynx** to `.xlsx` files, with the separation time placed in a merged cell above the data columns.

**Data Format**:
- Each feature should be placed in its own separate tab labeled with the word "analyte" followed by a unique compound name.
- Sheets containing data should start with the by-pass, followed by the single-pass ATD with `0` or `0.01` and `2` or `2.0` headers respectively. Following ATDs can be conducted at any separation time and placed in any order within the sheet.

**Required Fields**:
- Analyte names and theoretical CCS values are required to run a calibration; they should be comma-separated and listed in the order in which they appear in the `.xlsx` file.
- A calibration file can be uploaded by using the browse function to navigate to the file's location.

**Calibration Methods**:
- **Xia, Lin & Costello** — power curve calibration using single-pass and multipass arrival times.
- **Xia + Bush & Ruotolo** — enhanced calibration that additionally corrects for mass-dependent flight times, ion charge state, and reduced mass. Requires m/z, molecular weight, and charge state for each calibrant, as well as the EDC delay coefficient from MassLynx.

**Using Built-In Calibrant Libraries**:
- Enable **"Use Built-In Calibrant Data"** to select from the Major Mix Positive, Major Mix Negative, or SpheriCal Low Mass reference tables directly in the app. Ionization mode and mass range will be auto-populated when saving the calibration curve.

**After Calibration**:
- Power curves for both single-pass and multipass results will be displayed at the bottom of the page.
- A file containing arrival time calculations and linear plots for each standard in the mixture can be downloaded using the **'Download All Plots'** button.
- The calibration can then be saved to any available calibration curve slot using the **'Save'** button.
- Saved calibrations can be exported as `.rds` files from the **Saved Calibration Curves** tab for use in future sessions.

## Example Calibration File Information

### Major Mix Positive Ion Mode (Xia, Lin & Costello Method)

**Analyte Names**:  
Sulfadimethoxide, Val-Tyr-Val, Terfenadine, Polyalanine (n7), Leucine Enkephalin, Polyalanine (n8), Resperine, Polyalanine (n9), Polyalanine (n10), Polyalanine (n11), Polyalanine (n12), Polyalanine (n13), Polyalanine (n14)

**CCS Values**:  
168.4, 191.7, 228.7, 211.0, 229.8, 228.0, 252.3, 243.0, 256.0, 271.0, 282.0, 294.0, 306.0

### SpheriCal Low Mass Sodium Adducts (Xia + Bush & Ruotolo Method)

**Analyte Names**:  
PFS-1022, PFS-2012, PFS-1032, PFS-1042, PFS-1052, PFS-3012, PFS-1062, PFS-2032

**m/z Values**:  
317.121, 417.173, 491.210, 623.252, 755.294, 881.362, 973.410, 1187.494

**Molecular Weights (Da)**:  
294.131, 394.184, 468.221, 600.263, 732.305, 858.373, 950.451, 1164.505

**CCS Values**:  
160.65, 185.39, 198.51, 220.65, 246.40, 270.24, 287.23, 317.41

**Pre-Built Calibration Files**:  
The following `.rds` calibration files are available in the **Example Data** folder and from within the app for users who wish to skip the calibration step:
- `Major_Mix_Positive_Calibration_2026-05-17.rds`
- `SpheriCal_Calibration_Sodium Adducts_350ms_18V_2026-05-05.rds`

---

## Processing Experimental Data

Running experimental data requires that a calibration has been successfully run and saved, or that a saved calibration `.rds` file has been imported. Users can select whether to compare experimental data to known CCS values.

**Data Format**:
- Analyte names and CCS values should be comma-separated and placed in the same order as the tabs in the uploaded `.xlsx` file, following the same format as the calibration curve.

**After Processing**:
- A summary of results will be displayed in a data table at the bottom of the page.
- A detailed summary of results can be downloaded for all or individual analytes.

### Example File Experimental Information

#### Oxylipin Example (Xia + Bush & Ruotolo Method)

**Analyte Names**:  
PGD1, PGE1, PGF2a, PGF2b, 14_15-EET, 15(R)-HETE, 15(S)-HETE, 5(S)-HETE, 8_9-ETE, 6-trans-LTB4, 5(S)_12(S)-DiHETE, 8(S)_15(S)-DiHETE, PGA1, PGD2, PGE2, LXA4, 14_15-EpETE, 15(S)HEPE, 15-oxo-ETE, 5-oxo-ETE, PGA2, PGB2, PGJ2

**m/z Values**:  
377.23, 377.23, 377.23, 377.23, 343.22, 343.22, 343.22, 343.22, 343.22, 359.22, 359.22, 359.22, 359.22, 375.21, 375.21, 375.21, 341.21, 341.21, 341.21, 341.21, 347.13, 347.13, 347.13

**Molecular Weights (Da)**:  
354.2406, 354.2406, 354.2406, 354.2406, 320.2351, 320.2351, 320.2351, 320.2351, 320.2351, 336.2300, 336.2300, 336.2300, 336.2300, 352.2250, 352.2250, 352.2250, 318.2195, 318.2195, 318.2195, 318.2195, 324.1362, 324.1362, 324.1362

**CCS Values**:  
195.41, 197.54, 203.83, 194.30, 184.87, 190.00, 190.00, 185.78, 183.84, 199.00, 190.56, 195.42, 197.10, 192.77, 196.07, 195.67, 181.62, 183.71, 190.33, 186.53, 195.57, 205.39, 187.73

Comparison CCS values are from [Moran-Garrido et al.](https://pubs.acs.org/doi/10.1021/acs.analchem.4c06265) and [da Silva et al.](https://pubs.acs.org/doi/10.1021/acs.analchem.3c02213)

#### Phospholipid Example (Xia, Lin & Costello Method)

**Analyte Names**:  
d7-PC (15:0-18:1), d7-LysoPC (18:1), d7-PE (15:0-18:1), d7-LysoPE (18:1), d7-LysoPE (18:1) M+Na, d7-LysoPC (18:1) M+Na

**CCS Values**:  
285.4, 235.0, 273.5, 217.8, 224.3, 238.2

---

## Separating Multiple Peak ATDs

The **Multiple Peak ATD** tab provides an alternative to manually separating ATDs or performing additional slicing experiments.

- **Upload ATDs**: ATDs can be uploaded directly from **MassLynx** as `.xlsx` files, with arrival time data in the first column and intensity data in the second column.
- **Fitting**: The raw data can be plotted to assist with providing initial guess values to locate all peaks within an ATD.
- **Reconstructed ATDs**: Data within a specified standard deviation of the fitted arrival time is retained, and data points outside this range are generated from the fitted Gaussian model. The reconstructed ATD can be downloaded after the peaks are fitted.

---

## Video Tutorial

A **video tutorial** demonstrating how to use the components of the **Multipass CCS Refiner** is available here:  
[Watch the tutorial on YouTube](https://www.youtube.com/watch?v=AXrbocbQGY8&t=0s)

> **Note**: This tutorial covers the original version of the application. Core workflows remain the same; new features are documented in the **What's New** section above and within the app's **Introduction tab**.

---

## Getting Help

If you have any questions about this application, please email:
- **Eric Gier** (egier3@gatech.edu)
- **Facundo Fernandez** (facundo.fernandez@chemistry.gatech.edu)

Or post to our GitHub page:  
[Multipass CCS Refiner GitHub](https://github.com/facundof2016/Multipass-CCS-Refiner/)
