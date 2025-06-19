# This file contains the higher level app folder for Multipass CCS Refiner

#### Introduction ####
# Welcome to the Multipass CCS Refiner: A Web Application for Fine Collisional Cross Section Calibration in Cyclic Ion Mobility.
# The purpose of this application is to assist users with collisional cross section (CCS) calibration in multiple pass cyclic ion mobility (cIM) experiments.
# This application implements the calibration approach described by Lin and Costello (doi.org/10.1021/acs.analchem.4c01758) in a user-friendly interface, enabling calibration of multi-pass CCS experiments online with higher accuracy.
# The authors have made every effort to comment this code fully and highly recommend reading the above paper to gain insight into the physical underpinnings of the Multipass CCS Refiner.
# Additionally, this application would not have been possible without the greater R community who have been cited in the introduction tab of the R Shiny application.

# For users inexperienced with coding or the R language this application is hosted online through Shinyapps.io 
# at https://ericgier.shinyapps.io/multipass_ccs_refiner_segmented_code/
# In order to use this application locally, this script can be saved to your computer and run by pressing the Run App button at the top of the console.
# Running locally will require the packages listed below. These can be downloaded by copying the line of code below directly into the R Console.
# install.packages("shiny"), install.packages("shinythemes"), install.packages("bslib"), install.packages("shinyjs"), install.packages("shinyBS"), install.packages("DT"), install.packages("xlsx"), install.packages("ggplot2"), install.packages("png"), install.packages("dplyr")
# Neither these packages nor R itself need to be installed to run this application online.

# Broadly, this code is divided into three sections. 
#  1. Defining Functions - Home to all calculation, plotting and file sanitation functions.
#  2. User Interface - The graphical user interface for collecting user input.
#  3. Server - The code responsible for connecting user input to the back-end of the application.

# This template has been greatly modified from the original made by Winston Chang.
# If you would like to make your own RShiny applications this is an excellent place to start.
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# End Introduction

#### Data Uploading ####

# File format is important for using this code. The authors have created example excel files with the appropriate format to ease user experience.
# Calibration and experiment data files require all analytes to be placed in separate tabs of an excel document.
# All tabs containing data must start with "Analyte" (not case sensitive) in order for the application to differentiate data tabs from additional tabs.
# Acceptable tab names include Analyte 1, Analyte Resperine, Analyte Polyalanine (n9) etc.. These names can include keys such as _,/,:,?,& etc. and will be cleaned as needed by the application. Cleaning will not impact output to the user.

# Arrival time distributions from MassLynx can be copied and pasted directly into excel or the example excel documents.
# Arrival time distributions must contain arrival times in the first column followed by the corresponding intensity in the second column. This is the default for MassLynx copy and paste.
# Headers above each arrival time intensity pair must be merged and contain a numerical separation time without units. Units are assumed to be milliseconds which are the default separation time units for cIM instruments.
# Analyte tabs must start with by-pass (0 or 0.01) followed by the single-pass (2 or 2.0) ms arrival times. 
# Separation times for all multi-pass analyses are up to the users discretion and can be listed in any order but must include the user-selected separation time in the merged header. 
# Error checkers exist within the application to identify the locations of incorrect cells and will notify users within the interface when issues are encountered. 

# Finally, using the example excel files and editing ATD data and separation times will mitigate the vast majority of potential errors when running Multipass CCS Refiner. 
# Our goal is to make using this application as fast and as user-friendly as possible!

# End of Data Uploading

# Handle the newest version of Rcpp from the packages.R script
# Many packages in RShiny are dependent on Rcpp so it is recommended to update before publishing an application online.
# The packages.R script will do this automatically before each upload but is not required for regular use.
# source("Rcpp.R")

#### Installing Packages and Importing Libraries ####
# There are a number of packages and libraries used throughout this program. If you have not already you will need to install the packages listed below.
# Packages can be installed by copying the line below directly into the console as a single or separate lines of code.
# install.packages("shiny"), install.packages("shinythemes"), install.packages("bslib"), install.packages("shinyjs"), install.packages("shinyBS"), install.packages("DT"), install.packages('xlsx'), install.packages("ggplot2"), install.packages("png"), install.packages("clipr"), install.packages("viridis")

# Some packages come with start up messages which I have chosen to suppress.
# Import Shiny libraries 
library(shiny)
library(shinythemes)
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(shinyjs))
library(shinyBS)

# Import libraries for general functionality. 
suppressPackageStartupMessages(library(DT))
library(readxl) 
library(xlsx)
library(DT)
library(ggplot2) 
library(grid)
library(viridisLite)

# End importing libraries and installing packages.
#### Clear Previous Data and Graphing Space ####
# Ensure all old variables and the graphing space are cleared prior to running.
rm(list = ls())
graphics.off()

# Optional toggle for warning messages
options(warn = -1) # set to 0 to see warning messages
# End Cleaning
#### Source external files for UI, server, and functions ####
source("functions.R")
source("ui.R")
source("server.R") 
# End sourcing

#### Create Shiny object ####
shinyApp(ui = ui, server = server)