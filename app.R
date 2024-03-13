## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Aurora IncrementalQA
## Jack Gregory
## 29 September 2023
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script is a Shiny web application for Aurora IncrementalQA.  The main output is a plotly 
## heatmap which assists with the evaluation of run-by-run and year-by-year changes within a 
## research project.

## The application can be run by either:
##  - Clicking the "Run App" button in RStudio, which is at the top-right of the code window.
##  - Selecting all code and typing "CTRL + ENTER".
##  - Running "start.bat" in the local repository folder.


### START CODE ### 


# PREAMBLE ----------------------------------------------------------------------------------------

## Initiate 
## ... packages
pkgs <- c(
  "optparse",                                   # Command line arguments
  "shiny","shinythemes","shinybusy",            # Dashboard
  "fs",                                         # File system
  "vroom","assertr",                            # Data reading and writing
  "DBI","odbc",                                 # Data querying
  "tidyverse",                                  # Data wrangling
  "rhandsontable",                              # Dynamic tables
  "plotly"                                      # Dynamic plots
)
install.packages(setdiff(pkgs, rownames(installed.packages())))
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

## ... functions
theme_plot <- function() {
  ggplot2::theme_classic() +
  ggplot2::theme(strip.background = element_rect(fill="grey85", color=NA),
                 axis.line.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 panel.grid.major.y = element_line(color="grey85", linewidth=0.3),
                 panel.grid.minor.y = element_line(color="grey85", linewidth=0.3),
                 axis.line.x = element_line(linewidth=0.4),
                 axis.ticks.x = element_line(linewidth=0.4),
                 legend.position = "right",
                 plot.caption = element_text(hjust=0))
}

heatmap <- function(.data, .scn, .var, .iso3, .tech, .year) {
  
  ## Assertions
  stopifnot(
    is.data.frame(.data),
    is.data.frame(.scn),
    .var %in% c("Baseload Price","Capture Price","Capacity","Net Generation"),
    stringr::str_detect(.iso3, "^[A-Z]{3}$") && length(.iso3)==1,
    is.character(.tech) && length(.tech)==1,
    is.numeric(.year) && length(.year)==2
  )
  
  ## Define inputs
    ## ... variable
  var_label <- tolower(.var)
  var_sym <- gsub(" ", "", var_label)
  var <- dplyr::sym(var_sym)
  ## ... technology
  if (.var=="Baseload Price") {
    tech_filter <- dplyr::expr(TRUE)
    tech_label <- ""
  } else {
    tech_filter <- dplyr::expr(technology==.tech)
    tech_label <- .tech
  }
  ## ... unit symbol
  if (.var=="Capacity") {
    var_unit <- "GW"
  } else if (.var=="Net Generation") {
    var_unit <- "TWh"
  } else {
    var_unit <- "€"
  }
  
  .data |>
    dplyr::filter(region==.iso3) |>
    dplyr::filter(!!tech_filter) |>
    dplyr::filter(lubridate::year(date)>=.year[1] & lubridate::year(date)<=.year[2]) |>
    dplyr::left_join(dplyr::select(.scn, id, label), by=c("scenarioid"="id")) |>
    dplyr::relocate(label, .after=scenario) |>
    dplyr::arrange(date, label) |>
    dplyr::group_by(date) |>
    dplyr::mutate(scale = max(!!var, na.rm=TRUE) - min(!!var, na.rm=TRUE),
                  delta = !!var - dplyr::lag(!!var),
                  ndelta = delta/scale,
                  gap = dplyr::last(!!var) - dplyr::first(!!var),
                  ngap = gap/scale,
                  label = forcats::fct_recode(label, ALL=as.character(.scn$label[1])),
                  ndelta = ifelse(label=="ALL", ngap, ndelta),
                  text = ifelse(label=="ALL",
                                paste0(lubridate::year(date), "\n", label, "\n",
                                       formatC(round(gap/dplyr::first(!!var)*10^2, digits=2),
                                               format="f", digits=2), "%"),
                                paste0(lubridate::year(date), "\n", label, "\n",
                                       formatC(round(delta, digits=2), format="f", digits=2), var_unit))) |>
    ggplot2::ggplot(aes(x=label, y=date, fill=ndelta, text=text)) +
    ggplot2::geom_hline(yintercept=as.POSIXct("2028-01-01"), linewidth=0.8) +
    ggplot2::geom_tile(alpha=0.8) +
    ggplot2::scale_fill_gradient2(limits=c(-1,1), low="#de1a24", mid="grey98", high="#3f8f29", na.value="grey80") +
    ggplot2::labs(title=stringr::str_squish(paste(.iso3, tech_label, "normalized", var_label, "delta")),
                  y="",
                  x="",
                  fill="Norm. Δ",
                  caption="") +
    theme_plot() +
    ggplot2::theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5))
}

## ... command line arguments
## NB: See <https://www.r-bloggers.com/2015/09/passing-arguments-to-an-r-script-from-command-lines>
option_list <- list(
  make_option(c("-o", "--odbc"), type="character", default=NULL,
              help="ODBC connection name (i.e., '--odbc Redshift').", metavar="character")
)
opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)
if (is.null(opt$odbc)){
  print_help(opt_parser)
  stop("An ODBC connection name must be supplied!!", call.=FALSE)
}

# opt <- list(odbc="Redshift_2022")


## USER INTERFACE ---------------------------------------------------------------------------------
# Define UI for application that draws a heatmap

ui <- fluidPage(
  
  ## Define styles
  tags$head(
    ## Set navbar brand color
    tags$style(type="text/css", "span.navbar-brand{color: gold !important;}"),
    
    ## Set navbar tab color
    tags$style(type="text/css", "li a{color: gold !important;}"),
    
    ## Set slider color
    tags$style(
      HTML('/* changes the colour of the bars */
            .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
            background: gold !important;
            border-top: 1px solid gold !important;
            border-bottom: 1px solid gold !important;}

            /* changes the colour of the number tags */
            .irs-from, .irs-to, .irs-single { 
            background: gold !important; 
            color: black !important }'
      )
    )
  ),

  ## Add busy indicator
  add_busy_bar(color="gold", height="4px"),
  
  ## Define UI layout
  navbarPage("IncrementalQA", id="IncQA", theme=shinythemes::shinytheme("yeti"),
    
    ## Scenarios page
    tabPanel("Scenarios",
      sidebarLayout(
        sidebarPanel(
          ## Title
          h3(strong("IncrementalQA")),
          h4("Aurora Energy Research"),
          hr(),
          h6('An app producing a dynamic heatmap to compare incremental changes within a research project.'),
          ## Instructions
          h5(strong("Instructions")),
          h6('1. Import scenarios using one of the two methods below:'),
          h6(HTML("&emsp; <b>From DWH ...</b>")),
          h6(HTML('&emsp; a. Select all relevant EoS projects and click <em>Apply</em> to populate the scenarios dropdown.')),
          h6(HTML('&emsp; b. Select all relevant scenarios and click <em>Apply</em> to populate the table.')),
          h6(HTML("&emsp; <b>From file ...</b>")),
          h6(HTML('&emsp; a. Click <em>Upload</em> to import saved scenarios table.')),
          h6('2. Arrange the scenarios in the desired order by clicking and dragging rows.'),
          h6('3. Label the scenarios with a descriptive abbreviation or accept the generic ones supplied.'),
          h6('4. If desired, save scenarios table in a csv for future use.'),
          h6(HTML('5. Click <em>Query</em> to read the respective data from the DWH.')),
        ),
        mainPanel(
          fluidRow(
            column(10, offset=1,
              ## Project selectize
              selectizeInput("project",
                label=strong("Select project(s)"),
                choices=NULL,
                multiple=TRUE,
                width="100%"
              ),
              actionButton("project_button",
                label="  Apply",
                icon=icon("hand-pointer"),
                style="background-color: gold"
              ),
              hr(),
              ## Scenario selectize
              selectizeInput("scenario",
                label=strong("Select scenario(s)"),
                choices=NULL,
                multiple=TRUE,
                width="100%"
              ),
              actionButton("scenario_button",
                label="  Apply",
                icon=icon("hand-pointer"),
                style="background-color: gold"
              ),
              hr(),
              ## Scenario table
              h6(strong("Label and arrange scenarios")),
              rHandsontableOutput("table"),
              # verbatimTextOutput("text_table"),
              br(),
              fluidRow(
                column(4,
                  fileInput("upload_button",
                            label=NULL,
                            buttonLabel="Upload...",
                            # icon=icon("upload"),
                            # style="background-color: gold",
                            accept=c(".csv")
                  )
                ),
                column(1, offset=1,
                  uiOutput("download_button")
                )
              ),
              ## Query button
              hr(),
              uiOutput("query_button")#,
              # verbatimTextOutput("text_query")
            )
          )
        )
      )
    ),
    
    ## Heatmap page
    tabPanel("Heatmap",
      sidebarLayout(
        sidebarPanel(
          ## Instructions
          h5(strong("Instructions")),
          h6('1. Once the query is available locally, select the desired variable from its dropdown.'),
          h6('2. Select the desired region from its dropdown.'),
          h6('3. If necessary, select the desired technology from its dropdown.'),
          h6('4. Set the year range using its slider.'),
          h6(HTML('5. Click <em>Plot</em> to generate the heatmap.')),
          h6('6. Rinse and repeat.'),
          hr(),
          ## Variable select
          selectInput("var",
                      label=strong("Select variable"),
                      choices=c("Baseload Price","Capture Price","Capacity","Net Generation"),
                      selected=NULL
          ),
          ## Region select
          selectInput("iso3",
            label=strong("Select region"),
            choices=NULL,
            selected=NULL
          ),
          ## Technology select
          selectInput("tech",
                      label=strong("Select technology"),
                      choices=NULL,
                      selected=NULL
          ),
          ## Year double slider
          sliderInput("year",
            label=strong("Select year range"),
            min=2020,
            max=2060,
            value=c(2023,2060),
            step=1,
            round=TRUE,
            sep=""
          ),
          actionButton("plot_button",
            label="  Plot",
            icon=icon("chart-column"),
            style="background-color: gold"
          )
        ),
        mainPanel(
          fluidRow(
            column(10, offset=1,
              ## Heatmap plot
              plotlyOutput("heatmap",
                height="700px"
              )
            )
          )
        )
      )
    )
  )
)


## SERVER -----------------------------------------------------------------------------------------
# Define server logic required to draw a heatmap

server <- function(input, output, session, odbc_name=opt$odbc) {

  ## Connect to DWH
  con <- DBI::dbConnect(odbc::odbc(), odbc_name, timeout=60)
  
  ## Populate project selectize
  updateSelectizeInput(session=session, inputId="project", server=TRUE, choices=DBI::dbGetQuery(con, paste0("
    SELECT DISTINCT scn.project
    FROM            public.scenarios AS scn
    WHERE           scn.scenariotype='FULL_YEAR_RUN'
    ORDER BY        scn.project
  ")) |> dplyr::pull())
  
  ## Populate scenario selectize
  observeEvent(input$project_button, {
    updateSelectizeInput(session=session, inputId="scenario", server=TRUE, choices=DBI::dbGetQuery(con, paste0("
      SELECT DISTINCT scn.scenario
      FROM            public.scenarios AS scn
      WHERE           scn.project IN (", paste0("'", paste(input$project, collapse="', '"), "'"), ")
                      AND scn.scenariotype='FULL_YEAR_RUN'
      ORDER BY        scn.scenario
    ")) |> dplyr::pull())
  })
  
  ## Populate scenario table manually
  observeEvent(input$scenario_button, {
    ## Query DWH based on project and scenario user inputs
    df.scn <- DBI::dbGetQuery(con, paste0("
      SELECT DISTINCT scn.id,
                      scn.project,
                      scn.scenario
      FROM            public.scenarios AS scn
      WHERE           scn.project IN (", paste0("'", paste(input$project, collapse="', '"), "'"), ")
                      AND scn.scenario IN (", paste0("'", paste(input$scenario, collapse="', '"), "'"), ")
                      AND scn.scenariotype='FULL_YEAR_RUN'
      ORDER BY        scn.id
    "))
    
    ## Create table using rhandsontable
    output$table <- renderRHandsontable({
      rhandsontable(
          df.scn |>
            dplyr::mutate(label = paste0("RUN", dplyr::row_number())),
          manualRowMove=TRUE,
          stretchH="all"
        ) |>
        hot_col(col=seq(1,3), readOnly = TRUE) |>
        hot_cols(columnSorting=TRUE) |>
        hot_context_menu(allowRowEdit=FALSE, allowColEdit=FALSE, customOpts=list(items=c("remove_row")))
    })
    
    ## Display query button
    output$download_button <- renderUI({
      downloadButton("download",
                     label="  Download",
                     icon=icon("download"),
                     style="background-color: gold"
      )
    })
    
    output$download <- downloadHandler(
      filename = function() {
        paste0("scenarios_", format(Sys.time(), '%Y%m%d'), ".csv")
      },
      content = function(file) {
        vroom::vroom_write(df.scn_user(), file, delim=",")
      }
    )
    
    ## Display query button
    output$query_button <- renderUI({
      actionButton("query_button",
        label="  Query",
        icon=icon("database"),
        style="background-color: gold"
      )
    })
  })
  
  ## Populate scenario table from saved file
  observeEvent(input$upload_button, {
    if (fs::path_ext(input$upload_button$name)!="csv") {
      ## Prevent scenario table if incorrect file type
      showNotification("Invalid file type; please upload a csv file.", duration=NULL, type="error")
    } else {
      ## Upload scenario table
      df.scn <- vroom::vroom(input$upload_button$datapath, delim = ",", col_types="iccc")
      
      ## Dataframe assertions
      scn_check <- df.scn |>
        assertr::chain_start() %>%
        assertr::verify(ncol(.)==4) |>
        assertr::verify(assertr::has_only_names("id","project","scenario","label"), obligatory=TRUE) |>
        assertr::verify(is.numeric(id)) |>
        assertr::assert(is.character, -id) |>
        assertr::assert(assertr::is_uniq, id, scenario, label) |>
        assertr::chain_end(success_fun=assertr::success_logical, error_fun=assertr::error_logical)
      
      if (!scn_check) {
        ## Prevent scenario table if incorrect dataframe format
        showNotification("Invalid dataframe format; please upload a different csv file.", duration=NULL, type="error")
      } else {
        ## Create table using rhandsontable
        output$table <- renderRHandsontable({
          rhandsontable(
              df.scn,
              manualRowMove=TRUE,
              stretchH="all"
            ) |>
            hot_col(col=seq(1,3), readOnly = TRUE) |>
            hot_cols(columnSorting=TRUE) |>
            hot_context_menu(allowRowEdit=FALSE, allowColEdit=FALSE, customOpts=list(items=c("remove_row")))
        })
    
        ## Display query button
        output$download_button <- renderUI({
          downloadButton("download",
                         label="  Download",
                         icon=icon("download"),
                         style="background-color: gold"
          )
        })
        
        output$download <- downloadHandler(
          filename = function() {
            paste0("scenarios_", format(Sys.time(), '%Y%m%d'), ".csv")
          },
          content = function(file) {
            vroom::vroom_write(df.scn_user(), file, delim=",")
          }
        )
        
        ## Display query button
        output$query_button <- renderUI({
          actionButton("query_button",
                       label="  Query",
                       icon=icon("database"),
                       style="background-color: gold"
          )
        })
      }
    }
  })
  
  ## Make scenario table user input reactive
  df.scn_user <- reactiveVal()
  observeEvent(input$table, {
    df.scn_user(rhandsontable::hot_to_r(input$table))
    # output$text_table <- renderPrint(dplyr::pull(df.scn_user(), label))
  })
  
  ## Perform DWH queries
  df.bp <- NULL
  df.cp <- NULL
  df.gen <- NULL
  observeEvent(input$query_button, {
    if (nrow(dplyr::distinct(df.scn_user(), id))!=nrow(df.scn_user())) {
      ## Prevent query if duplicate scenarios
      showNotification("Remove duplicate scenarios before proceeding.", duration=NULL, type="error")
    } else if (nrow(dplyr::distinct(df.scn_user(), label))!=nrow(df.scn_user())) {
      ## Prevent query if duplicate labels
      showNotification("Set unique labels before proceeding.", duration=NULL, type="error")
    } else {
      ## Lock app while DWH query executes
      # show_modal_spinner(
      #   spin="fading-circle",
      #   color="gold",
      #   text="Querying DWH, please wait..."
      # )
      
      ## Convert labels to factor
      scn_labels <- dplyr::pull(df.scn_user(), label)
      df.scn_user(dplyr::mutate(df.scn_user(), label = factor(label, levels=scn_labels)))
      
      ## Perform DWH queries
      ## ... baseload prices
      df.bp <<- DBI::dbGetQuery(con, paste0("
          SELECT      dta.scenarioid,
                      scn.project,
                      scn.scenario,
                      dta.regionid,
                      reg.region,
                      dta.timeid,
                      tm.date,
                      dta.baseloadprice
          FROM        public.yearlyregion AS dta
          INNER JOIN  ( SELECT  *
                        FROM    public.scenarios 
                        WHERE   id IN (", paste0("'", paste(dplyr::pull(df.scn_user(), id), collapse="', '"), "'"), ")
                      ) AS scn ON dta.scenarioid=scn.id
          LEFT JOIN   public.regions AS reg on dta.regionid=reg.id
          LEFT JOIN   public.time AS tm ON dta.timeid=tm.id
        ;"))
      
      ## ... capture prices
      df.cp <<- DBI::dbGetQuery(con, paste0("
          SELECT      dta.scenarioid,
                      scn.project,
                      scn.scenario,
                      dta.regionid,
                      reg.region,
                      dta.technologyid,
                      tech.technologyfullname,
                      tech.technology,
                      dta.timeid,
                      tm.date,
                      dta.capturedprice AS captureprice
          FROM        public.yearlyregiontechnologyfinance AS dta
          INNER JOIN  ( SELECT  *
                        FROM    public.scenarios 
                        WHERE   id IN (", paste0("'", paste(dplyr::pull(df.scn_user(), id), collapse="', '"), "'"), ")
                      ) AS scn ON dta.scenarioid=scn.id
          LEFT JOIN   public.regions AS reg on dta.regionid=reg.id
          LEFT JOIN   public.technologies AS tech ON dta.technologyid=tech.id
          LEFT JOIN   public.time AS tm ON dta.timeid=tm.id
        ;"))
      
      ## ... capacity & net generation
      df.gen <<- DBI::dbGetQuery(con, paste0("
          SELECT      dta.scenarioid,
                      scn.project,
                      scn.scenario,
                      dta.regionid,
                      reg.region,
                      dta.technologyid,
                      tech.technologyfullname,
                      tech.technology,
                      dta.timeid,
                      tm.date,
                      dta.capacity,
                      dta.netproductionintwh AS netgeneration
          FROM        public.yearlyregiontechnologyoperations AS dta
          INNER JOIN  ( SELECT  *
                        FROM    public.scenarios 
                        WHERE   id IN (", paste0("'", paste(dplyr::pull(df.scn_user(), id), collapse="', '"), "'"), ")
                      ) AS scn ON dta.scenarioid=scn.id
          LEFT JOIN   public.regions AS reg on dta.regionid=reg.id
          LEFT JOIN   public.technologies AS tech ON dta.technologyid=tech.id
          LEFT JOIN   public.time AS tm ON dta.timeid=tm.id
        ;"))
      
      ## Release app
      # remove_modal_spinner()
      # output$text_query <- renderPrint(dplyr::slice(df.data(), 1:3))
      
      ## Populate region select
      updateSelectInput(session=session, inputId="iso3", selected=NULL,
                        choices=c(df.bp$region, df.cp$region, df.gen$region) |>
                          unique() |>
                          sort()
      )
      
      ## Populate technology select
      updateSelectInput(session=session, inputId="tech", selected=NULL,
                        choices=c(df.cp$technology, df.gen$technology) |>
                          unique() |>
                          sort()
      )
      
      ## Populate year double slider
      year_min <- c(df.bp$date, df.cp$date, df.gen$date) |>
        min() |>
        as.numeric() |>
        lubridate::year()
      year_max <- c(df.bp$date, df.cp$date, df.gen$date) |>
        max() |>
        as.numeric() |>
        lubridate::year()
      updateSliderInput(session=session, inputId="year",
                        min=year_min,
                        max=year_max,
                        value=c(year_min, year_max)
      )
      
      ## Move to "Heatmap" tab
      updateNavbarPage(session, inputId="IncQA",
                       selected="Heatmap")
    }
  })

  ## Build heatmap
  df.data <- reactiveVal()
  observeEvent(input$plot_button, {
    
    ## Add inputs list to prevent inadvertent heatmap changes
    l.choice <- list(
      var=input$var,
      iso3=input$iso3,
      tech=input$tech,
      year=input$year
    )
    
    ## Set reactive data
    if (l.choice$var=="Baseload Price") {
      df.data(df.bp)
    } else if (l.choice$var=="Capture Price") {
      df.data(df.cp)
    } else {
      df.data(df.gen)
    }
    
    ## Build plotly
    output$heatmap <- renderPlotly({
      plotly::ggplotly(heatmap(df.data(),
                               .scn=df.scn_user(),
                               .var=l.choice$var, 
                               .iso3=l.choice$iso3, 
                               .tech=l.choice$tech, 
                               .year=l.choice$year), 
                       tooltip="text")
    })
  })
}


## RUN APP ----------------------------------------------------------------------------------------
# Run the application 

shinyApp(ui=ui, server=server)


### END CODE ###

