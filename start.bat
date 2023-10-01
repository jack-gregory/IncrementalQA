@echo off
echo "IncrementalQA Launcher ..."
set /p "odbc=Please enter ODBC name: "
Rscript -e "shiny::runApp('app.R', launch.browser=TRUE)" -o %odbc%
pause
