#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(dplyr)
library(rhandsontable)
library(reactable)
library(sparkline)
library(plotly)
library(MetaStan)
library(promises)
library(shinycssloaders)

# Options for Spinner
options(spinner.color="#FF6666", spinner.color.background="#ffffff", spinner.size=2)

res_pal <- function(x) rgb(colorRamp(c("#6699cc", "#ffb54d"))(x), maxColorValue = 255, alpha = 120)

bar_style <- function(width = 1, color = "lightblue") {
  # The linear-gradient stop position is 100% - width%
  position <- paste0(100 - width * 100, "%")
  list(
    background = sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, color),
    backgroundSize = "98% 88%",
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center"
  )
}

mbmap <- function(x = MBMA.stan,...) {
  
  ## Plot the observed probabilities
  r = as.vector(rbind(x$data_wide$r1, x$data_wide$r2,
                      x$data_wide$r3, x$data_wide$r4))
  dose = as.numeric(as.vector(rbind(x$data_wide$d1, x$data_wide$d2,
                                    x$data_wide$d3, x$data_wide$d4)))
  n = as.vector(rbind(x$data_wide$n1, x$data_wide$n2,
                      x$data_wide$n3, x$data_wide$n4))
  obs.probs = r/n
  
  mydata = data.frame(dose = dose,
                      y.obs = obs.probs,
                      y.samplesizes = n,
                      Study = gl(nrow(x$data_wide), 4,
                                 labels = letters[1:nrow(x$data_wide)]))
  
  Pred_probs_args = rep(NA, times = x$data$Npred)
  
  for(i in 1:x$data$Npred) {
    Pred_probs_args[i] = sprintf("Pred_probs[%i%s", i, "]")
  }
  
  pred.probs = x$fit_sum[Pred_probs_args, c(4, 5, 6, 7, 8)]
  
  x_data = data.frame(x$fit_sum)
  
  Preddata = data.frame(Pred_doses = x$data$Pred_doses,
                        y.pred =  pred.probs[,3],
                        y.lo =  pred.probs[,1],
                        y.lo25 =  pred.probs[,2],
                        y.hi =  pred.probs[,5],
                        y.hi75 =  pred.probs[,4])
  
  plot_ly() %>%
    # geom_ribbon
    add_trace(
      x = c(Preddata$Pred_doses, rev(Preddata$Pred_doses)),
      y = c(Preddata$y.lo25, rev(Preddata$y.hi75)),
      type = "scatter",
      mode = "none",
      fill = "tozeroy",
      fillcolor = "rgba(255, 102, 102, 0.4)") %>%
    add_trace(
      x = c(Preddata$Pred_doses, rev(Preddata$Pred_doses)),
      y = c(Preddata$y.lo, rev(Preddata$y.hi)),
      type = "scatter",
      mode = "none",
      fill = "tozeroy",
      fillcolor = "rgba(255, 102, 102, 0.2)") %>%
    # geom_line (dashed)
    add_trace(data = Preddata, x = ~Pred_doses, y = ~y.lo, name = "lower prediction",
              type = "scatter", mode = "lines", line = list(dash = "dash", color = "#ff6666", width=1)) %>%
    add_trace(data = Preddata, x = ~Pred_doses, y = ~y.hi, name = "upper prediction",
              type = "scatter", mode = "lines", line = list(dash = "dash", color = "#ff6666", width=1)) %>%
    add_trace(data = Preddata, x = ~Pred_doses, y = ~y.lo25, name = "25th prediction",
              type = "scatter", mode = "lines", line = list(dash = "dash", color = "#ff6666", width=1)) %>%
    add_trace(data = Preddata, x = ~Pred_doses, y = ~y.hi75, name = "75th prediction",
              type = "scatter", mode = "lines", line = list(dash = "dash", color = "#ff6666", width=1)) %>%
    # geom_line
    add_trace(data = Preddata, x = ~Pred_doses, y = ~y.pred, name = "50th prediction",
              type = "scatter", mode = "lines", line = list(color = "#ff6666")) %>%
    # geom_point
    add_trace(data = mydata, x = ~dose, y = ~y.obs, size = ~y.samplesizes, color = ~Study, name = "Observation",
              type = "scatter", mode = "markers", marker = list(symbol = "circle")) %>%
    
    # y-axis range
    layout(
      xaxis = list(title = "Dose"),
      yaxis = list(title = "Response Probability", range = c(0, 1))) %>%
    # no legends
    layout(showlegend = FALSE)
}

# Define UI for application that draws a histogram
ui <- bslib::page_fluid(
  shinyjs::useShinyjs(),
  theme = bs_theme(bootswatch = "minty",
                   base_font = font_google("Inter"),
                   code_font = font_google("JetBrains Mono"),
                   font_scale = 0.8
                   ),
  title = "Model-based meta-analysis platform",
  #sidebar = sidebar(
  #  title = "Whatever"
  #),
  br(),
  card(
    card_header("Data preparation"),
    shiny::fileInput(
      inputId = "udata",
      label = "Upload .csv file",
      width = "100%",
      accept = ".csv"
    ),
    "[Dataset preparation guide]",
    br(),
    "d: series of dose, r: responders in each series, n: sample size in each series",
    rHandsontableOutput("data_raw"),
    downloadButton("download_btn", "download modified table")
  ),
  card(
    card_header("Data summary"),
    checkboxInput("stu_grp", "grouping by study"),
    reactableOutput("data_stan")
  ),
  shiny::splitLayout(
    cellWidths = c("50%","50%"),
    card(
      card_header("Sample size by study"),
      plotlyOutput("data_pie")
    ),
    card(
      card_header("Histograms"),
      plotlyOutput("data_hist")
    )
  ),
  card(
    card_header("Model fit"),
    actionButton("fit", "fit data"),
    fluidRow(
      selectInput(
        "mods", "select models to fit",
        c("linear" = "linear",
          "log-linear" = "loglinear",
          "emax" = "emax",
          "sigmoidal" = "sigmoidal"),
        multiple = TRUE,
        selected = c("linear", "emax")
      ),
      sliderInput("iter", "no.iteration", ticks = FALSE,
                  min = 500, max = 2000, step = 100, value = 1000)  
    ),
    tableOutput("fit_res")
  ),
  card(
    card_header("Fit visualization"),
    uiOutput("plot_choice"),
    plotlyOutput("fit_plot")
  )
  

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {


  value <- shiny::reactiveValues()
  
  observeEvent(input$udata,{
    value$data <- read.csv(input$udata$datapath) # add upload data to value$data
  })
  
  output$data_raw <- renderRHandsontable({
    data <- value$data
    if(!is.null(data)){ rhandsontable(data) }
  })
  
  output$download_btn <- downloadHandler(
    filename = function() {
      # setname for data
      "data.csv"
    },
    content = function(file) {
      # generate file
      write.csv(rhandsontable::hot_to_r(input$data_raw), "data.csv",
                quote = FALSE, row.names = FALSE)
      # download generated file
      file.copy("data.csv", file)
    }
  )
  
  output$data_stan <- renderReactable({
    if(is.null(input$data_raw)){return()} # stop function if rhandsontable is not generated
    data_meta <- MetaStan::create_MetaStan_dat(
      dat = rhandsontable::hot_to_r(input$data_raw),
      armVars=c(dose="d", responders="r", sampleSize="n"),
      nArmsVar="nd"
    )
    data_meta_l <- data_meta$data_long %>% 
      filter(!is.na(sampleSize) & !is.na(responders)) %>% 
      mutate(ratio = round(responders/sampleSize*100,1))
    
    value$data_meta <- data_meta
    value$data_meta_l <- data_meta_l
    
    reactable(data_meta_l,
              pagination = FALSE,
              compact = TRUE,
              defaultExpanded = TRUE,
              bordered = TRUE,
              fullWidth = FALSE,
              outlined = TRUE,
              groupBy = if(input$stu_grp){"study"}else{NULL},
              columns = list(
                dose = colDef(
                  style = function(value) {
                    normalized <- (value - min(data_meta_l$dose)) / (max(data_meta_l$dose) - min(data_meta_l$dose))
                    color <- res_pal(normalized)
                    list(background = color)
                  }
                ),
                ratio = colDef(
                  name = "ratio in %",
                  style = function(value) {
                    bar_style(width = value/100, color = "#ddd")
                  }
                  )
                )
              )
  })
  
  output$data_pie <- renderPlotly({
    if(is.null(value$data_meta_l)){return()} # stop function if data_meta (long_form) is not generated
    plot_ly(value$data_meta_l, labels = ~study, values = ~sampleSize, type = "pie", opacity = 1)
  })
  output$data_hist <- renderPlotly({
    if(is.null(value$data_meta_l)){return()} # stop function if data_meta (long_form) is not generated
    data <- value$data_meta_l
    histograms <- lapply(names(data), function(col_name) {
      plot_ly(data, x = ~get(col_name), type = "histogram", name = col_name, nbinsx = 5, opacity = 0.6)
    })
    subplot_grid <- subplot(histograms, nrows = 3, margin = 0.05) %>%
      layout(
        bargap = 0.1,
        legend = list(orientation = "h")  # "h"는 수평 방향, y = -0.2는 아래로 -0.2의 위치에 배치
      )
    
  })
  
  
  observeEvent(input$fit, {
    data_meta <- value$data_meta
    # Model fitting
    
    withProgress(message = 'Fitting', value = 0, {
      incProgress(1/4, detail = "running linear model")
      ## 1. Linear (mu, alpha, tau)
      if("linear" %in% input$mods){
        value$linear <- MBMA_stan(
          data=data_meta, likelihood="binomial",
          dose_response="linear", 
          mu_prior = c(0, 10), 
          alpha_prior = c(0, 100),
          tau_prior_dist="half-normal", tau_prior=0.5,
          warmup = input$iter/2, iter = input$iter)
      }else{
        value$MBMAlinear <- NULL
      }
      incProgress(1/4, detail = "running log-linear model")
      ## 2. Log dose linear (mu, alpha, tau)
      if("loglinear" %in% input$mods){
        value$loglinear <- MBMA_stan(
          data=data_meta, likelihood="binomial",
          dose_response="log-linear", 
          mu_prior = c(0, 10), 
          alpha_prior = c(0, 100),
          tau_prior_dist="half-normal", tau_prior=0.5,
          warmup = input$iter/2, iter = input$iter)
      }else{
        value$MBMAloglinear <- NULL
      }
      incProgress(1/4, detail = "running emax model")
      ## 3. Emax (mu, Emax, ED50, tau)
      if("emax" %in% input$mods){
        value$emax <- MBMA_stan(
          data=data_meta, likelihood="binomial",
          dose_response="emax", 
          Emax_prior=c(0, 10), 
          ED50_prior_dist="functional",
          tau_prior_dist="half-normal", tau_prior=0.5,
          warmup = input$iter/2, iter = input$iter)
      }else{
        value$MBMAemax <- NULL
      }
      incProgress(1/4, detail = "running sigmoidal model")
      ## 4. Sigmoidal Emax (mu, Emax, ED50, gamma, tau)
      if("sigmoidal" %in% input$mods){
        value$sigmoidal <- MBMA_stan(
          data=data_meta, likelihood="binomial",
          dose_response="sigmoidal", 
          Emax_prior=c(0, 10), 
          ED50_prior_dist="functional",
          tau_prior_dist="half-normal", tau_prior=0.5,
          gamma_prior = c(1, 2),
          warmup = input$iter/2, iter = input$iter
        )  
      }else{
        value$MBMAsigmoidal <- NULL
      }
    })
    
    
    
    fit_list <- list(
      linear = value$linear$fit,
      loglinear = value$loglinear$fit,
      emax = value$emax$fit,
      sigmoidal = value$sigmoidal$fit
    )
    fit_list <- fit_list[!sapply(fit_list, is.null)] # remove null fit results
    
    
    
    output$fit_res <- renderTable({
      res_mat <- compare_MBMA(fit_list)
      mat_mname <- (res_mat %>% dimnames())[[1]]
      fit_res <- data.frame(model = mat_mname, res_mat, row.names = NULL) %>% 
        arrange(model) %>% 
        mutate(method = fit_list %>% names())
    })
    
    output$fit_plot <- renderPlotly({
      mbmap(value[[input$mplot]])
    })
    
  })
  
  output$plot_choice <- renderUI({
    selectInput(
      "mplot","choose method to plot",
      choices = input$mods
    )
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server) #%>% run_with_themer()
