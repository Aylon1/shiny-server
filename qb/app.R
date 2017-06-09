## app.R ##
library(shiny)
library(shinydashboard)
library(DT)
library(shinyBS)

if (.Platform$OS.type != "unix") {
  Sys.getenv("R_LIBS_USER")
  Sys.setenv(R_LIBS_USER = "C:/_Syncthing/code/R/win-library/3.2")
  Sys.getenv("R_LIBS_USER")
  .libPaths("C:/_Syncthing/code/R/win-library/3.2")
  
  setwd("C:/_Syncthing/code/R/Strategies/QuantBeaver")
} else {
  setwd("~/Desktop/sf_R_Data/Strategies/QuantBeaver")
  
  #if(!file.exists('log.txt'))
  #  writeLines(c(""), "log.txt")
}

source(paste(getwd(), "lib/1_qb_options.R", sep = "/"))

# http://stackoverflow.com/questions/36132204/reactive-radiobuttons-with-tooltipbs-in-shiny
radioTooltip <-
  function(id,
           choice,
           title,
           placement = "bottom",
           trigger = "hover",
           options = NULL) {
    options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
    options = paste0("{'", paste(
      names(options),
      options,
      sep = "': '",
      collapse = "', '"
    ), "'}")
    bsTag <- shiny::tags$script(shiny::HTML(
      paste0(
        "
        $(document).ready(function() {
        setTimeout(function() {
        $('input', $('#",
        id,
        "')).each(function(){
        if(this.getAttribute('value') == '",
        choice,
        "') {
        opts = $.extend(",
        options,
        ", {html: true});
        $(this.parentElement).tooltip('destroy');
        $(this.parentElement).tooltip(opts);
        }
        })
        }, 500)
        });
        "
        )
        ))
    htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
        }


qb_getInstTable <- function() {
  iTable <-
    dplyr::mutate(instrument.table()[, c("primary_id",
                                         "asset_type",
                                         "type",
                                         "msCategory",
                                         "msName",
                                         "name")], ifelse(name == "NULL", msName, name))
  iTable <-
    dplyr::mutate(iTable,
                  ifelse(asset_type == "Not Categorized", type, asset_type))
  colnames(iTable) <-
    c("ID",
      "drop3",
      "drop4",
      "Category",
      "drop1",
      "drop2",
      "Name",
      "aType")
  iTable$row <- seq.int(1, nrow(iTable), 1)
  #View(iTable)
  return(iTable[, c("row", "ID", "Name", "aType", "Category")])
}

iTable <- qb_getInstTable()

sidebar <- dashboardSidebar(width = 300,
                            ## Sidebar content
                            sidebarMenu(
                              menuItem(
                                "Asset Selection",
                                tabName = "tabAssetSelection",
                                icon = icon("open-file", lib = "glyphicon")
                              ),
                              menuItem("Portf. Setup",
                                       icon = icon("sliders"),
                                       fluidRow(column(8,
                                       selectInput(
                                         inputId = "portfolioSelector",
                                         label = "Portfolios",
                                         choices = c("Portf.1"=1),
                                         #selected = "EUR",
                                         multiple = FALSE,
                                         selectize = TRUE,
                                         width = '98%'
                                       )
                                       
                                       ),column(4,
                                                actionButton("buttonNewPortfolio", "+"))
                                       ),
                                       selectInput(
                                         inputId = "ccy",
                                         label = "Portf. currency",
                                         choices = c("USD", "EUR", "HUF"),
                                         selected = "EUR",
                                         multiple = FALSE,
                                         selectize = TRUE,
                                         width = '98%'
                                       ),
                                       menuItem("Timing", tabName = "menuTiming", icon = icon("calendar"),
                                                numericInput(
                                                  inputId = "lookback",
                                                  label = "Lookback period (yrs.)",
                                                  value = 8,
                                                  min = 1,
                                                  max = 20,
                                                  step = 1
                                                ),
                                                bsTooltip("lookback", "Years as integer. Sets the date range to: Today to x years in the past.",
                                                          "right", options = list(container = "body")),
                                                dateRangeInput(
                                                  inputId = "dateRange",
                                                  label = "Date Range",
                                                  start = "2009-01-01",
                                                  weekstart = 1,
                                                  width = '98%'
                                                ),
                                                radioButtons(
                                                  inputId = "periodicity",
                                                  label = "Periodicity",
                                                  choices = c("Monthly" = "month",
                                                              "Weekly" = "week")
                                                ),
                                                radioTooltip(id = "periodicity", choice = "month", title = "Monthly rebalancing period.", placement = "right", trigger = "hover"),
                                                radioTooltip(id = "periodicity", choice = "week", title = "Weekly rebalancing period.", placement = "right", trigger = "hover"),
                                                checkboxInput(
                                                  inputId = "periodicity_timing",
                                                  label = "Start of Period?",
                                                  value = FALSE
                                                ),
                                                bsTooltip("periodicity_timing", "If ticked, Portfolio rebalances at the start of week or month.",
                                                          "right", options = list(container = "body"))
                                       ),
                                       menuItem("BM & TAA", tabName = "bmTAA", icon = icon("random", lib = "glyphicon"),
                                                selectInput(
                                                  inputId = "bmSelector",
                                                  label = "Benchmark",
                                                  choices = NULL,
                                                  multiple = FALSE,
                                                  selectize = TRUE,
                                                  width = '98%'
                                                ),
                                                selectInput(
                                                  inputId = "signalTAASelector",
                                                  label = "TAA Signal Calculation",
                                                  choices = list('Ehlers' = c(EhlersAutoCorr = 1),
                                                              'QuantStrat' = c(QuantStrat = 2)),
                                                  multiple = FALSE,
                                                  selectize = TRUE,
                                                  width = '98%'
                                                ),
                                                bsTooltip("signalTAASelector", "EhlersAutoCorr -> https://quantstrattrader.wordpress.com/2017/02/15/ehlerss-autocorrelation-periodogram/, QuantStrat -> https://quantstrattrader.wordpress.com/2015/11/09/a-filter-selection-method-inspired-from-statistics/",
                                                          "right", options = list(container = "body")),
                                                selectInput(
                                                  inputId = "rankingSelector",
                                                  label = "Rank Assets by:",
                                                  choices = c(StrengthROC = 1, WeightedROC = 2),
                                                  multiple = FALSE,
                                                  selectize = TRUE,
                                                  width = '98%'
                                                )
                                                ),
                                       menuItem("Other", tabName = "other", icon = icon("microchip"),
                                                radioButtons(
                                                  inputId = "alignMatrix",
                                                  label = "Missing values",
                                                  choices = c(
                                                    "Delete Columns" = "long",
                                                    "Delete Rows" = "wide",
                                                    "Keep" = "none"
                                                  )
                                                ),
                                                radioTooltip(id = "alignMatrix", choice = "long", title = "Clomuns with missing values are deleted i.e. if the asset does not have long enough history.", placement = "right", trigger = "hover"),
                                                radioTooltip(id = "alignMatrix", choice = "wide", title = "Rows with missing values are removed i.e. keeping all assets but shortening the observed period.", placement = "right", trigger = "hover"),
                                                radioTooltip(id = "alignMatrix", choice = "none", title = "Keep = Missing Values are keept! Handle with care as may lead to incorrect results!", placement = "right", trigger = "hover"),
                                                checkboxInput(
                                                  inputId = "survivors",
                                                  label = "Survivors only?",
                                                  value = TRUE
                                                ),
                                                bsTooltip("survivors", "If ticked, exclude any assets that do not have a recent price.",
                                                          "right", options = list(container = "body"))
                                        )
                                       
                                       ),
                              menuItem(
                                "Dashboard",
                                tabName = "tabDashboard",
                                icon = icon("dashboard")
                              ),
                              menuItem("Correlations",
                                       tabName = "tabCorrelations",
                                       icon = icon("th"))
                            ))

body <-   ## Body content
  dashboardBody(
    tags$style(
      type = 'text/css',
      "col-sm-1, .col-sm-10, .col-sm-11, .col-sm-12, .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5, .col-sm-6, .col-sm-7, .col-sm-8, .col-sm-9 { !important; padding-left:1px; padding-right:1px; margin:0;}"
    ),
    tabItems(
      tabItem(tabName = "tabAssetSelection",
              fluidPage(fluidRow(
                column(12,
                       fluidRow(column(
                         6,
                         box(
                           title = "Assets",
                           collapsible = TRUE,
                           width = 12,
                           tags$style(
                             type = 'text/css',
                             ".dataTables_wrapper { font-size: 0.9em; line-height: 100%; padding:0; margin:0;}"
                           ),
                           tags$style(
                             type = 'text/css',
                             ".dataTables_wrapper input.form-control { font-size: 0.9em; height: 70%;}"
                           ),
                           tags$style(
                             type = 'text/css',
                             ".dataTables_wrapper table.dataTable tbody th, table.dataTable tbody td { font-size: 0.9em; padding:2px; margin:0px;}"
                           ),
                           DT::dataTableOutput('dtAllAssets')
                         )
                       ),
                       column(
                         6,
                         
                         box(
                           width = 12,
                           collapsible = TRUE,
                           title = "Portfolio Assets",
                           DT::dataTableOutput('portfAssets')
                           # column(
                           #   6,
                           #   selectInput(
                           #     inputId = "inputTicker",
                           #     label = NULL,
                           #     choices = c("Preset1", "Preset2", "Preset3", "Preset4")
                           #   )
                           # ),
                           # column(6),
                           # uiOutput("tabs")
                         ),
                         box(
                           width = 12,
                           collapsible = TRUE,
                           collapsed = T,
                           title = "Portfolio Configuration",
                           tabBox(
                             width = 12,
                             tabPanel(title = "Main"),
                             tabPanel(title = "Benchmark"),
                             tabPanel(title = "Debugging", 
                                      column(
                                        12#,
                                        # verbatimTextOutput("lookback"),
                                        # verbatimTextOutput("dateRangeText"),
                                        # verbatimTextOutput("ccy"),
                                        # verbatimTextOutput("periodicity"),
                                        # verbatimTextOutput("periodicity_timing"),
                                        # verbatimTextOutput("alignMatrix"),
                                        # verbatimTextOutput("survivors"),
                                        # verbatimTextOutput("portfolios"),
                                        # verbatimTextOutput("bmSelector"),
                                        # verbatimTextOutput("portfolioSelector")
                                      ))
                           )
                         )
                       )),
                       fluidRow(column(12)))
              ))),
      # First tab content
      tabItem(tabName = "tabDashboard",
              verbatimTextOutput("lookback"),
              verbatimTextOutput("dateRangeText"),
              verbatimTextOutput("ccy"),
              verbatimTextOutput("periodicity"),
              verbatimTextOutput("periodicity_timing"),
              verbatimTextOutput("alignMatrix"),
              verbatimTextOutput("survivors"),
              verbatimTextOutput("portfolios"),
              verbatimTextOutput("bmSelector"),
              verbatimTextOutput("portfolioSelector"),
              verbatimTextOutput("signalTAASelector"),
              "tabDashboard"),
      # Second tab content
      tabItem(tabName = "tabCorrelations",
              "tabCorrelations")
    )
  )

ui <- dashboardPage(
  dashboardHeader(
    titleWidth = "300",
    title = "QuantBeaver",
    dropdownMenu(
      type = "notifications",
      notificationItem(text = "5 new users today",
                       icon("users")),
      notificationItem(text = "12 items delivered",
                       icon("truck"),
                       status = "success"),
      notificationItem(
        text = "Server load at 86%",
        icon = icon("exclamation-triangle"),
        status = "warning"
      )
    ),
    dropdownMenu(
      type = "tasks",
      badgeStatus = "success",
      taskItem(value = 90, color = "green",
               "Documentation"),
      taskItem(value = 17, color = "aqua",
               "Project X"),
      taskItem(value = 75, color = "yellow",
               "Server deployment"),
      taskItem(value = 80, color = "red",
               "Overall project")
    )
  ),
  sidebar,
  body
)

server <- function(input, output, session) {
  # create storage
  portfolio <- list()
  portfolio[[session$token]] <- list()
  config <-
    list(
      ccy = "EUR",
      lookback = 8,
      from = as.character(seq(
        Sys.Date(),
        length = 2,
        by = paste0("-", 8, " years")
      )[2]),
      to = as.character(Sys.Date()),
      periodicity = "month",
      periodicity_timing = FALSE,
      alignMatrix = "long",
      survivors = TRUE,
      rIDs = NULL,
      bmSelector = NULL,
      signalTAASelector = 1,
      rankingSelector = 1
    )
  
  setPortfolioValue <- function(key, value) {
    #browser()
    if (is.null(input$portfolioSelector)) {
      return()
    } else {
      # initialize the config
      if (is.null(portfolio[[session$token]][[input$portfolioSelector]]))
        portfolio[[session$token]][[input$portfolioSelector]] <<- config
      
      if (key == "dateRange") {
        portfolio[[session$token]][[input$portfolioSelector]][["from"]] <<-
          value[1]
        portfolio[[session$token]][[input$portfolioSelector]][["to"]] <<-
          value[2]
      } else {
        portfolio[[session$token]][[input$portfolioSelector]][[paste0(key)]] <<-
          value
      }
    }
  }
  
  getPortfolioValue <- function(key, verbose = FALSE) {
    if (is.null(input$portfolioSelector)) {
      return()
    } else {
      if (verbose) {
        if (key == "dateRange") {
          return(paste0("input$", key, ": ", portfolio[[session$token]][[input$portfolioSelector]][["from"]], " to ", portfolio[[session$token]][[input$portfolioSelector]][["to"]]))
        } else {
          return(paste0("input$", key, ": ", portfolio[[session$token]][[input$portfolioSelector]][[paste0(key)]]))
        }
      } else {
        if (is.null(portfolio[[session$token]][[input$portfolioSelector]]))
          portfolio[[session$token]][[input$portfolioSelector]] <<- config
        
        if (is.null(portfolio[[session$token]][[input$portfolioSelector]][[paste0(key)]]))
          return(NULL)
        else
          return(portfolio[[session$token]][[input$portfolioSelector]][[paste0(key)]])
      }
    }
  }
  
  uiUpdateBM <- function(x=T){
    if (!is.null(getPortfolioValue("ticker")) & length(getPortfolioValue("ticker")) > 0) {
      bmItems <- getPortfolioValue("rIDs")
      names(bmItems) <- getPortfolioValue("ticker")
      
      updateSelectInput(session, "bmSelector", choices = bmItems, selected = getPortfolioValue("bmSelector"))
    }
  }
  
  uiUpdatePortfolioSelector <- function(){
    key <- as.vector(sapply(v$data, "[[", 1))
    val <- seq(1, length(key), 1)
    names(val) <- key

    updateSelectInput(session, "portfolioSelector", choices = val, selected = (as.numeric(input$buttonNewPortfolio) + 1))
  }
  
  uiUpdatePorfolioConfig <- function(){
    updateTextInput(session, "ccy", value = getPortfolioValue("ccy"))
    updateTextInput(session, "lookback", value = getPortfolioValue("lookback"))
    updateDateRangeInput(session, "dateRange", label = "dateRange", start = as.Date(getPortfolioValue("from")), end = as.Date(getPortfolioValue("to")))
    updateCheckboxInput(session, "periodicity_timing", value = getPortfolioValue("periodicity_timing"))
    updateCheckboxInput(session, "survivors", value = getPortfolioValue("survivors"))
    updateRadioButtons(session, "periodicity", selected = getPortfolioValue("periodicity"))
    updateRadioButtons(session, "alignMatrix", selected = getPortfolioValue("alignMatrix"))
    updateSelectInput(session, "signalTAASelector", selected = getPortfolioValue("signalTAASelector"))
    updateSelectInput(session, "rankingSelector", selected = getPortfolioValue("rankingSelector"))
    
  }
  
  # all assets datatable
  output$dtAllAssets <- DT::renderDataTable({
    DT::datatable(
      iTable,
      filter = 'top',
      rownames = F,
      options = list(columnDefs = list(list(
        visible = FALSE, targets = 0
      )))
    )
  })
  
  # a proxy to select the assets absed on the chosen portfolio
  proxy = dataTableProxy('dtAllAssets')
  
  # storage of selected rows in all asset table
  uiUpdatePortfolioAssets <- reactive({
    setPortfolioValue("rIDs", c(as.vector(unlist(iTable[input$dtAllAssets_rows_selected, 1]))))
    setPortfolioValue("ticker", c(as.vector(unlist(iTable[input$dtAllAssets_rows_selected, "ID"]))))
    
    # update bmSelector
    uiUpdateBM(F)
    
    if (!is.null(getPortfolioValue("rIDs")))
      dplyr::filter(iTable, row %in% getPortfolioValue("rIDs"))
    else
      NULL
  })
  
  # portfolio assets datatable
  output$portfAssets <- DT::renderDataTable({
    DT::datatable(
      uiUpdatePortfolioAssets(),
      filter = 'top',
      rownames = F,
      options = list(columnDefs = list(list(
        visible = FALSE, targets = 0
      )))
    )
  })
  
  # reactive value to trigger updates of the portfolio table
  v <-
    reactiveValues(data = list(list(
      Title = "Portf.1",
      Value = "1",
      Content = "Tab1 content"
    )))
  
  # observed button to create new portfolio
  observeEvent(input$buttonNewPortfolio, {
    v$data[[as.numeric(input$buttonNewPortfolio) + 1]] <-
      list(
        Title = paste0("Portf.", as.numeric(input$buttonNewPortfolio) + 1),
        Value = as.character(as.numeric(input$buttonNewPortfolio) + 1),
        Content = paste0("Portf.", as.numeric(input$buttonNewPortfolio) + 1)
      )
    
    uiUpdatePortfolioSelector()
  })
  
  # update all asset table with currently selected rows
  observeEvent(input$portfolioSelector, {
    #updateTabItems(session, "portfAssets", selected = as.character(input$portfolioSelector))
    if (!is.null(getPortfolioValue("rIDs")))
      proxy %>% selectRows(as.numeric(getPortfolioValue("rIDs")))
    else
      proxy %>% selectRows(0)
    
    # update portfolio asset data.table
    uiUpdatePortfolioAssets()
    
    # update portfolio config
    uiUpdatePorfolioConfig()
    
    # update bm data
    uiUpdateBM()
  })
  
  #source(file.path("server", "ui.asset.R"), local = TRUE)$value
  
  # Currency choice
  observe({
    setPortfolioValue("ccy", input$ccy)
  })
  
  # lookback choice
  observeEvent(input$lookback, {
    setPortfolioValue("lookback", as.numeric(input$lookback))
      
    setPortfolioValue("from", as.character(seq(
      as.Date(getPortfolioValue("to")),
      length = 2,
      by = paste0("-", input$lookback, " years")
    )[2]))
      
    updateDateRangeInput(
      session,
      "dateRange",
      label = "dateRange",
      start = as.Date(getPortfolioValue("from")),
      end = as.Date(getPortfolioValue("to"))
    )
  })
  
  # date range choice
  observe({
    setPortfolioValue("dateRange", input$dateRange)
  })
  
  # periodicity choice
  observe({
    setPortfolioValue("periodicity", input$periodicity)
  })
  
  # periodicity.timing choice
  observe({
    setPortfolioValue("periodicity_timing", input$periodicity_timing)
  })
  
  # BM choice
  observeEvent(input$bmSelector, {
    setPortfolioValue("bmSelector", input$bmSelector)
  })
  
  # TAA signal choice
  observeEvent(input$signalTAASelector, {
    setPortfolioValue("signalTAASelector", input$signalTAASelector)
  })
  
  # ranking choice
  observeEvent(input$rankingSelector, {
    setPortfolioValue("rankingSelector", input$rankingSelector)
  })
  
  # alignMatrix choice
  observe({
    setPortfolioValue("alignMatrix", input$alignMatrix)
  })
  
  # survivors choice
  observe({
    setPortfolioValue("survivors", input$survivors)
  })
  
  output$ccy <- renderText({
    input$ccy
    getPortfolioValue("ccy", T)
  })
  
  output$lookback <- renderText({
    input$lookback
    getPortfolioValue("lookback", T)
  })
  
  output$dateRangeText  <- renderText({
    input$dateRange
    getPortfolioValue("dateRange", T)
  })
  
  output$periodicity <- renderText({
    input$periodicity
    getPortfolioValue("periodicity", T)
  })
  
  output$periodicity_timing <- renderText({
    input$periodicity_timing
    getPortfolioValue("periodicity_timing", T)
  })
  
  output$bmSelector <- renderText({
    input$bmSelector
    getPortfolioValue("bmSelector", T)
  })
  
  output$alignMatrix <- renderText({
    input$alignMatrix
    getPortfolioValue("alignMatrix", T)
  })
  
  output$survivors <- renderText({
    input$survivors
    getPortfolioValue("survivors", T)
  })
  
  output$portfolioSelector <- renderText({
    input$portfolioSelector
    #getPortfolioValue("survivors", T)
  })
  
  # -------------------------------------------------------------
  output$portfData <- renderText({
    portf.data[[input$portfAssets]]$rIDs
  })
  
  output$x4 = renderPrint({
    ids <- input$dtAllAssets_rows_selected
    
    rIDs <- as.vector(unlist(iTable[ids, 1]))
    if (length(ids)) {
      cat('These rows were selected:\n\n')
      cat(rIDs, sep = ', ')
    }
  })
  
  # close app when browser window closed
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)