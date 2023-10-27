# Introduction

This repository contains an R Shiny web application for Aurora IncrementalQA.  The main output is a plotly heatmap which assists with the evaluation of run-by-run and year-by-year changes within a research project.

At present, the app is a beta version and only plots capture prices.  However, in the future, it may be possible to add other outcome variables.  See [Extensions](#extensions) below.


# Instructions

The remainder of this readme file provides instructions for how to setup and run the app.

1. Install the latest version of [R](https://cran.rstudio.com/).
2. If desired, install [RStudio](https://posit.co/download/rstudio-desktop/).
3. Add Rscript to PATH:
   - Type "path" into the search bar and select "Edit the system environment variables".
   - Click "Environment variables".
   - Under "User variables" select "New...".
   - Enter the following:
      - Variable name = "PATH"
      - Variable value = "path\to\Rscript.exe" (e.g., `C:\Program Files\R\R-4.3.0\bin\x64\`)
   - Click "OK" three times.
4. Setup an [ODBC connection to the DWH](https://auroraenergy.atlassian.net/wiki/spaces/AW/pages/3642130481/Data+Warehouse+Password+Update+Methods#ODBC).
5. Clone the IncrementalQA repository = [https://github.com/jack-gregory/IncrementalQA.git](https://github.com/jack-gregory/IncrementalQA.git).
6. Run `start.bat` file within your cloned repository.
7. If the bat file does not run, restart your machine for the PATH changes to take effect and try again.


# Extensions

- [ ] Check warning/error messages when app is running
- [ ] Save scenarios by including upload & download functionality for df.scn_user() as a csv (see [here](https://mastering-shiny.org/action-transfer.html))
- [ ] Other variables (e.g., baseload prices, generation, capacity, etc.)


# Version Notes

## v0.1 -- 2023-10-09

- Initial version querying capture prices only.
- Inclusion of start.bat file to run from terminal.

