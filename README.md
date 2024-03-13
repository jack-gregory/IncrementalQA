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
5. Clone the IncrementalQA repository: [https://github.com/jack-gregory/IncrementalQA.git](https://github.com/jack-gregory/IncrementalQA.git).
6. Run `start.bat` file within your cloned repository.
7. If the bat file does not run, restart your machine for the PATH changes to take effect and try again.


# Extensions

- [ ] Simplify repetitive code.
- [ ] Move functions and SQL queries to source files. 
- [ ] Check warning/error messages when app is running
- [ ] Close connections and terminal when exiting (see Shiny functions `onStop()` and `onSessionEnded()` as well as [here](https://community.rstudio.com/t/closing-database-connection-when-closing-shiny-app/134910) and [here](https://stackoverflow.com/questions/65995120/how-to-close-the-connection-when-exiting-the-shiny-app))


# Version Notes

## v0.2 -- 2024-03-11

- Inclusion of scenario save functionality.
- Inclusion of additional output variables (i.e., baseload prices, capacity, and net generation).


## v0.1 -- 2023-10-09

- Initial version querying capture prices only.
- Inclusion of start.bat file to run from terminal.

