---
title:        "Aurora IncrementalQA"
# subtitle:     "..."
author:       "Jack Gregory"
date:         "29 September 2023"
---


# Introduction

This repository contains an R Shiny web application for Aurora IncrementalQA.  The main output is a plotly heatmap which assists with the evaluation of run-by-run and year-by-year changes within a research project.


# Instructions

The remainder of this readme file provides instructions for how to setup and run the app.

1. Install [R](https://cran.rstudio.com/).
2. If desired, install [RStudio](https://posit.co/download/rstudio-desktop/).
3. Add Rscript to PATH:
    a. Type "path" into the search bar and select "Edit the system environment variables".
    b. Click "Environment variables".
    c. Under "User variables" select "New...".
    d. Enter the following:
        - Variable name = "PATH"
        - Variable value = "path\to\Rscript.exe" (e.g., `C:\Program Files\R\R-4.3.0\bin\x64\`)
    e. Click "OK" three times.
    f. Restart your machine for the changes to take effect.
4. Setup an [ODBC connection to the DWH](https://auroraenergy.atlassian.net/wiki/spaces/AW/pages/3642130481/Data+Warehouse+Password+Update+Methods#ODBC).
5. Clone the IncrementalQA repository ...
6. Run start.bat file within your cloned repository.


# Extensions

- Write df.scn_user() as a csv and read csv to df.scn_user()
- Other variables (e.g., baseload prices, generation, capacity, etc.)

