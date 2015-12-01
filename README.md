# education-income
Final project for Harvard's Stat 186, Spring 2015.

Andy Shi and Diana Zhu.

## Introduction

The relationship between education and income has long been a topic of interest
among economists and education has been widely perceived as an effective way of
reducing income inequality. Our project seeks to investigate their relationship
using a cross-sectional dataset. 


## Data
We use the dataset, Research on Early Life and Aging Trends and Effects
(RELATE): A Cross-National Study. The data is available at:

http://www.icpsr.umich.edu/icpsrweb/DSDR/studies/34241


## Report

The report is available as the `writeup_final.pdf` file. To regenerate this pdf,
you will need to run the `education-income.R` code and compile the pdf using
LaTeX. 


## Running the Code

### Dependencies

The R code requires the `data.table` and the `car` packages. You can install
them with 

    install.packages(c("data.table", "car"))

### Directory Structure

The directory structure should be as follows:


    education-income
    |-- code
        |-- education-income.R
    |-- figure
    |-- ICPSR_34241
        |-- DS0001
            |-- 34241-00001-Data.rda

The folder `ICPSR_34241` and its subfolders/contents can be downloaded from the
link above. 

The figure directory holds the figures and needs to be created before running
the R code. 
