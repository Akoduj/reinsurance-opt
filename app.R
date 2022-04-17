# Deploy with:
# library(rsconnect)
# rsconnect::deployApp('D:/02_Projects/R Projects/App-1')

library(shiny)
library(plotly)
library(DT)
source("helper.R")

genNumbers = source("helper.R")$value

userInput = rbind(c(10, 12, 14),
                         c(0.05, 0.075, 0.1),
                         c(10.25, 12.5, 15))
colnames(userInput) = c("LoB1", "LoB2", "LoB3")
rownames(userInput) = c("mean", "CV", "P")

userInputPremium_default = matrix(c(10.25, 12.5, 15), ncol = 3)
colnames(userInputPremium_default) = c("LoB1", "LoB2", "LoB3")
rownames(userInputPremium_default) = c("P")


test = 0

# Define UI ----
ui <- navbarPage("Risikomodellierung",
                 tabPanel("1: Monte Carlo Simulation",
                          
                          sidebarLayout(
                            sidebarPanel(
                              actionButton("update", "Generate Scenarios"),
                              br(),
                                         numericInput("n",
                                                      label = "Number of Scenarios",
                                                      value = 10000),
                                         fluidRow(
                                           column(6, offset=0,
                                                  numericInput("CoC_ins",
                                                               label = "Cost of Capital Insurer",
                                                               value = 0.1),
                                                  ),
                                           column(6, offset=0,
                                                  numericInput("CoC_reins",
                                                               label = "Cost of Capital Reinsurer",
                                                               value = 0.06),
                                           )
                                         ),
                                         
                                           h4(tags$b("Correlation Matrix")),
                                           fluidRow(
                                             column(4, offset=0, style = "margin-right:-15px",
                                                    textInput("c11",
                                                              label=NULL,
                                                              value=1,
                                                              width="60px"
                                                    )
                                             ),
                                             column(4, offset=0, style = "margin-right:-15px",
                                                    textInput("c12",
                                                              label=NULL,
                                                              value=0.75,
                                                              width="60px"),
                                                    textInput("c22",
                                                              label=NULL,
                                                              value=1,
                                                              width="60px")
                                             ),
                                             column(4, offset=0,
                                                    textInput("c13",
                                                              label=NULL,
                                                              value=-0.25,
                                                              width="60px"),
                                                    textInput("c23",
                                                              label=NULL,
                                                              value=0.25,
                                                              width="60px"),
                                                    textInput("c33",
                                                              label=NULL,
                                                              value=1,
                                                              width="60px")
                                             )
                                           ),
                                           h4(tags$b("Input values")),
                                           DTOutput('userInputTable')
                                         
                                         
                                         
                                         ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Simulation",
                                         h5(tags$b("Loss Distribution L")),
                                         DTOutput("sim_L"),
                                         h5(tags$b("Underwriting Result U")),
                                         DTOutput("sim_U")
                                         ),
                                tabPanel("Scenarios",
                                         fluidRow(
                                           column(3, h5("Random Numbers"),
                                                  tableOutput('corrTable')
                                           ),
                                           column(3, offset=1, h5("Loss"),
                                                  tableOutput('lossTable')
                                           ),
                                           column(3, offset=1, h5("P&L"),
                                                  tableOutput('PLTable')
                                           )

                                         )
                                         )
                              )
                              
                              
                              )
                            
                          )
                          ),
                 tabPanel("2: Without Reinsurance",
                          
                          fluidRow(
                            column(6, offset=0,
                                   h4("Undiversified"),
                                   h5(tags$b("Economic Profitability Z")),
                                   DTOutput("undiv_z_table"),
                                   h5(tags$b("Underwriting Result U")),
                                   DTOutput("undiv_u_table"),
                                   h5(tags$b("Capital Costs")),
                                   DTOutput("undiv_capCost_table")
                            ),
                            column(5, offset=1,
                                   h4("Diversified"),
                                   h5(tags$b("Risk Based Capital")),
                                   DTOutput("div_riskCap_table"),
                                   h5(tags$b("Economic Profitability Z")),
                                   DTOutput("div_z_table"),
                                   h5(tags$b("Underwriting Result U")),
                                   DTOutput("div_u_table"),
                                   h5(tags$b("Capital Costs")),
                                   DTOutput("div_capCost_table")
                            ),
                          )
                          ),
                 tabPanel("3: Reinsurance",
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("reins_risk_measure", "Reinsurance Risk Measure",
                                          c("Value at Risk",
                                            "Expected Shortfall"),
                                          selected = 1),
                              sliderInput("reins_risk_measure_alpha", "Risk Measure Alpha (%)",
                                          min = 1, max = 100, value = 99, step = 0.5),
                              selectInput("reins_p_fn_type", "Reinsurance Premium Principle",
                                          c("Expected Value Principle",
                                            "Standard Deviation Principle",
                                            "Cost of Capital Principle"),
                                          selected = "Standard Deviation Principle"),
                              sliderInput("reins_p_evp_lambda", "Expected Value surcharge (%)",
                                          min = 0, max = 100, value = 0, step = 1),
                              sliderInput("reins_p_sdp_alpha", "Standard deviation multiplier",
                                          min = 0, max = 20, value = 2.3, step = 0.1),
                              sliderInput("reins_coc", "Reinsurance Cost of Capital (%)",
                                          min = 0, max = 100, value = 10, step = 1)
                              
                              
                            ),
                            mainPanel(
                              tabsetPanel(id="reins_tabs",
                                tabPanel(
                                  "Effect of Reinsurance on CDF",
                                  
                                  plotOutput('reins_plot'),
                                  div(style = "margin-top:-20px"),
                                  fluidRow(
                                    column(3,
                                           h5(tags$b("Deductible")),
                                           div(style = "margin-bottom:-15px"),
                                           sliderInput("reins_ded1", "",
                                                       min = 0, max = 40, value = 10, step = 0.5),
                                           div(style = "margin-bottom:-25px"),
                                           sliderInput("reins_ded2", "",
                                                       min = 0, max = 40, value = 12.5, step = 0.5),
                                           div(style = "margin-bottom:-25px"),
                                           sliderInput("reins_ded3", "",
                                                       min = 0, max = 40, value = 14.5, step = 0.5),
                                           div(style = "margin-bottom:-15px"),
                                    ),
                                    
                                    column(3,
                                           h5(tags$b("Limit")),
                                           div(style = "margin-bottom:-15px"),
                                           sliderInput("reins_lim1", "",
                                                       min = 0, max = 40, value = 1.5, step = 0.5),
                                           div(style = "margin-bottom:-25px"),
                                           sliderInput("reins_lim2", "",
                                                       min = 0, max = 40, value = 2, step = 0.5),
                                           div(style = "margin-bottom:-25px"),
                                           sliderInput("reins_lim3", "",
                                                       min = 0, max = 40, value = 2.5, step = 0.5),
                                           div(style = "margin-bottom:-15px"),
                                    ),
                                    column(3,
                                           h5(tags$b("Premium")),
                                           br(), br(),
                                           div(style = "margin-top:-15px"),
                                           textOutput("reins_p1"),
                                           br(), br(), br(),
                                           div(style = "margin-top:-6px"),
                                           textOutput("reins_p2"),
                                           br(), br(), br(),
                                           div(style = "margin-top:-9px"),
                                           textOutput("reins_p3"),
                                    ),
                                    column(3,
                                           div(style = "margin-top:-10px"),
                                           br(),
                                           sliderInput("reins_bounds", "Plot Bounds",
                                                       min = 0, max = 50,
                                                       value = c(30,40),
                                                       step = 1),
                                    ),
                                  ),
                                ),
                                tabPanel(
                                  "Reinsurance Premium Surface Plots",
                                  br(),
                                  plotlyOutput("reins_p_plot"),
                                  br(),
                                  fluidRow(
                                    column(6,
                                      tags$b("X - Limit"),
                                      br(),
                                      tags$b("Y - Deductible"),
                                      br(),
                                      tags$b("Z - Premium"),
                                    ),
                                    column(6,
                                      selectInput(
                                        "reins_p_plot_lob",
                                        "Select LoB",
                                        c("LoB1", "LoB2", "LoB3"),
                                        selected = 1
                                      )
                                    )
                                  )
                                  
                                )
                              )
                            )
                          )
                          ),
                 tabPanel("4: Optimization",
                          
                          sidebarLayout(
                            sidebarPanel(
                              actionButton("opt_button", "Optimize"),
                              br(),
                              selectInput("opt_risk_measure", "Reinsurance Risk Measure",
                                          c("Value at Risk",
                                            "Expected Shortfall"),
                                          selected = 1),
                              sliderInput("opt_risk_measure_alpha", "Risk Measure Alpha (%)",
                                          min = 1, max = 100, value = 99, step = 0.5),
                              selectInput("opt_p_fn_type", "Reinsurance Premium Principle",
                                          c("Expected Value Principle",
                                            "Standard Deviation Principle",
                                            "Cost of Capital Principle"),
                                          selected = "Standard Deviation Principle"),
                              sliderInput("opt_p_evp_lambda", "Expected Value surcharge (%)",
                                          min = 0, max = 100, value = 0, step = 1),
                              sliderInput("opt_p_sdp_alpha", "Standard deviation multiplier",
                                          min = 0, max = 20, value = 2.3, step = 0.1),
                              sliderInput("opt_coc_ins", "Insurance Cost of Capital (%)",
                                          min = 0, max = 100, value = 10, step = 1),
                              sliderInput("opt_coc_reins", "Reinsurance Cost of Capital (%)",
                                          min = 0, max = 100, value = 10, step = 1),
                              
                              
                            ),
                            mainPanel(
                              fluidRow(
                                column(6,
                                  h4(tags$b("Insurer")),
                                  numericInput("opt_equity_ins", "Equity", value = 5),
                                  sliderInput("opt_solvency_target_ins", "Solvency Target (%)",
                                              min = 50, max = 150, value = 100, step = 1),
                                  h5(tags$b("Insurance Premium")),
                                  DTOutput('opt_premium_ins')
                                ),
                                column(6,
                                  h4(tags$b("Reinsurer")),
                                  numericInput("opt_equity_reins", "Equity", value = 5),
                                  sliderInput("opt_solvency_target_reins", "Solvency Target (%)",
                                              min = 50, max = 150, value = 100, step = 1),
                                  
                                ),
                              ),
                              
                              h4(tags$b("Optimization Result")),
                              fluidRow(
                                column(3,
                                  radioButtons("opt_target_var", "Variable to be optimized",
                                               c("Total Economic Profitability",
                                                 "Insurer Economic Profitability",
                                                 "Reinsurer Economic Profitability",
                                                 "Insurer Return on Risk Based Capital",
                                                 "Reinsurer Return on Risk Based Capital"),
                                               selected = "Total Economic Profitability"),
                                ),
                                column(9,
                                  DTOutput("opt_result_inp"),
                                  DTOutput("opt_result_out")
                                ),
                              ),
                              
                            )
                          
                          ),
                 ),
                 tabPanel("5: Analysis",
                          fluidRow(
                            column(5, style='padding:0px;',
                                   plotlyOutput("anal_plot1"),
                            ),
                            column(7, style='padding:0px;',
                                   plotlyOutput("anal_plot2"),
                            )
                          ),
                          fluidRow(
                            column(5, style='padding:0px;',
                                   plotlyOutput("anal_plot3"),
                            ),
                            column(7, style='padding:0px;',
                                   plotlyOutput("anal_plot4"),
                            )
                          ),
                          
                          #actionButton("opt_button", "Optimize")
                 ),
                          
)

# Define server logic ----
server <- function(input, output) {
  
  # Correlation matrix
  corr = eventReactive(input$update, {
    cbind(c(1, as.double(input$c12), as.double(input$c13)),
          c(as.double(input$c12), 1, as.double(input$c23)),
          c(as.double(input$c13), as.double(input$c23), 1))
  })
  
  
  
  userInputPrem = reactiveValues()
  
  
  ################
  # Simulation
  
  vals = reactiveValues()

  # Run simulation and get all the output values
  observeEvent(input$update, {
    result = runSimulation(n = input$n,
                           corr = corr(),
                           input = userInput,
                           CoC_ins = input$CoC_ins,
                           CoC_reins = input$CoC_reins)
    
    # Update Premium values on Optimization tab
    userInputPrem$premium = matrix(userInput[3,], ncol = 3)
    colnames(userInputPrem$premium) = c("LoB1", "LoB2", "LoB3")
    rownames(userInputPrem$premium) = c("P")
    
    
    # Monte Carlo
    vals$uniformNums = result["corrNum"]
    vals$lossNums = result["loss"]
    vals$P_L = result["p_l"]
    vals$u = as.data.frame(result["u"])
    
    # Without Reinsurance
    vals$undiv_z = as.data.frame(result["undiv_z"])
    vals$undiv_u = as.data.frame(result["undiv_u"])
    vals$undiv_capCost = as.data.frame(result["undiv_capCost"])
    
    # With Reinsurance
    vals$div_riskCap = as.data.frame(result["div_riskCap"])
    vals$div_z = as.data.frame(result["div_z"])
    vals$div_u = as.data.frame(result["div_u"])
    vals$div_capCost = as.data.frame(result["div_capCost"])

  })
  
  #####################
  # Optimization
  opt_vals = reactiveValues()
  
  observeEvent(input$opt_button, {
    # User input values
    if (is.null(userInputPrem$premium)) {
      premium_ins = userInputPremium_default
    } else {
      premium_ins = userInputPrem$premium
    }
    
    opt_risk_measure = input$opt_risk_measure
    opt_alpha = input$opt_risk_measure_alpha / 100
    
    p_type = input$opt_p_fn_type
    p_evp_lambda = input$opt_p_evp_lambda / 100
    p_sdp_alpha = input$opt_p_sdp_alpha
    
    coc_ins = input$opt_coc_ins / 100
    coc_reins = input$opt_coc_reins / 100
    
    equity_ins = input$opt_equity_ins
    equity_reins = input$opt_equity_reins
    
    solvency_target_ins = input$opt_solvency_target_ins / 100
    solvency_target_reins = input$opt_solvency_target_reins / 100
    
    opt_target = match(input$opt_target_var, c("Total Economic Profitability",
                       "Insurer Economic Profitability",
                       "Reinsurer Economic Profitability",
                       "Insurer Return on Risk Based Capital",
                       "Reinsurer Return on Risk Based Capital"))
    opt_target = opt_target - 1
    
    if (opt_target <= 2) {
      opt_dimension = 2
    } else {
      opt_target = opt_target + 2
      opt_dimension = 6
    }
    
    
    if (opt_risk_measure == "Value at Risk") {
      opt_risk_measure = "VaR"
    } else {
      opt_risk_measure = "ES"
    }
    if (p_type == "Expected Value Principle") {
      p_type = "EVP"
      p_args = p_evp_lambda
    } else if (p_type == "Standard Deviation Principle") {
      p_type = "SDP"
      p_args = p_sdp_alpha
    } else {
      p_type = "CoCP"
      p_args = c(coc_reins, opt_risk_measure, opt_alpha)
    }
    
    # Run the optimization
    # .... prem_ins : Reinsurance premium values (LoB number of dimensions)
    # .... coc_ins : Cost of Capital rate of insurer
    # .... coc_reins : Cost of Capital rate of reinsurer
    # .... prem_fn_type : Reinsurance Premium function type ("EVP", "SDP", "CoCP")
    # .... prem_fn_args : (lambda, alpha, [r_CoC, "ES"/"VaR", alpha])
    # .... alpha : Alpha value of risk measure of both insurer and reinsurer
    # .... risk_measure_reins : Risk measure of reinsurer ("ES", "VaR")
    # .... f_dim : Dimension of output vector
    # .... f_ind : Index of variable to be optimized
    # .... .... (Z_ins, Z_reins, RBC_ins, RBC_reins, RoRBC_ins, RoRBC_reins, p)
    # .... eq_ins : Total available equity Insurer
    # .... eq_reins : Total available equity Reinsurer
    # .... sol_tar_ins : Solvency target Insurer (equity / RBC)
    # .... sol_tar_reins : Solvency target Reinsurer (equity / RBC)
    # .... FE : Function evaluations for differential evolution (DE)
    # .... popSize : Population size for DE
    # .... s : s scalar for DE
    opt = runOptimization(prem_ins = premium_ins,
                          coc_ins = coc_ins,
                          coc_reins = coc_reins,
                          prem_fn_type = p_type,
                          prem_fn_args = p_args,
                          alpha = opt_alpha,
                          risk_measure_reins = opt_risk_measure,
                          f_dim = opt_dimension,
                          f_ind = opt_target,
                          eq_ins = equity_ins,
                          eq_reins = equity_reins,
                          sol_tar_ins = solvency_target_ins,
                          sol_tar_reins = solvency_target_reins)
    
    xOpt = opt$xOpt
    fOptAll = opt$fOptAll
    
    opt_input = matrix(c(xOpt, fOptAll[7:9]), ncol = 3, byrow = T)
    colnames(opt_input) = c("LoB1", "LoB2", "LoB3")
    rownames(opt_input) = c("Deductible", "Limit", "Premium")
    
    opt_output = matrix(fOptAll[1:6], ncol = 2, byrow = T)
    colnames(opt_output) = c("Insurer", "Reinsurer")
    rownames(opt_output) = c("Z", "RBC", "RoRBC")
    
    opt_vals$input = opt_input
    opt_vals$output = opt_output
  })
  
  #################
  # Reinsurance plot values
  reins_plot_vals = reactiveValues()
  reins_prem_plot_vals = reactiveValues()
  
  observe({
    # User input values
    ded = c(input$reins_ded1, input$reins_ded2, input$reins_ded3)
    lim = c(input$reins_lim1, input$reins_lim2, input$reins_lim3)
    
    reins_risk_measure = input$reins_risk_measure
    reins_alpha = input$reins_risk_measure_alpha / 100
    
    p_type = input$reins_p_fn_type
    p_evp_lambda = input$reins_p_evp_lambda / 100
    p_sdp_alpha = input$reins_p_sdp_alpha
    p_coc = input$reins_coc / 100
    
    tabs = input$reins_tabs
    
    isolate({
      if (reins_risk_measure == "Value at Risk") {
        reins_risk_measure = "VaR"
      } else {
        reins_risk_measure = "ES"
      }
      if (p_type == "Expected Value Principle") {
        p_type = "EVP"
        p_args = p_evp_lambda
      } else if (p_type == "Standard Deviation Principle") {
        p_type = "SDP"
        p_args = p_sdp_alpha
      } else {
        p_type = "CoCP"
        p_args = c(p_coc, reins_risk_measure, reins_alpha)
      }
      
      if (tabs == "Effect of Reinsurance on CDF") {
        plt = makeReinsPlot(ded, lim, p_type, p_args)
        p = as.numeric(plt$reins_p)
        reins_plot_vals$L_plot = ecdf(apply(plt$reins_plot_before, 1, sum))
        reins_plot_vals$L_plot_after = ecdf(apply(plt$reins_plot_after, 1, sum))
        reins_plot_vals$L_plot_reins = ecdf(apply(plt$reins_plot_reins, 1, sum))
        reins_plot_vals$reins_p1 = p[1]
        reins_plot_vals$reins_p2 = p[2]
        reins_plot_vals$reins_p3 = p[3]
      } else if (tabs == "Reinsurance Premium Surface Plots") {
        bounds = c(0, 10)
        plt = makeReinsPremPlot(bounds, p_type, p_args)
        
        reins_prem_plot_vals$x = plt$x
        reins_prem_plot_vals$y = plt$y
        reins_prem_plot_vals$z1 = plt$z1
        reins_prem_plot_vals$z2 = plt$z2
        reins_prem_plot_vals$z3 = plt$z3
      }
    })
  })
  
  # Change bounds if user adjusts them
  reins_plot_bounds = reactiveValues()
  
  observe({
    reins_bounds = input$reins_bounds
    
    reins_plot_bounds$min_x = reins_bounds[1]
    reins_plot_bounds$max_x = reins_bounds[2]
  })
  
  
  #################
  # Analysis
  
  # Dimensions: c("Z_ins", "Z_reins", "RBC_ins", "RBC_reins",
  # .... "RoRBC_ins", "RoRBC_reins", "p1", "p2", "p3",
  # .... "ded1", "ded2", "ded3", "lim1", "lim2", "lim3")
  anal_plot_vals = list()
  cols = palette(rainbow(6))
  # Insurer Optimization
  # Column 1 (Z_ins and Z_reins)
  anal_plot_vals$plot11 = getDataPlot2D(3, 1, 1)
  anal_plot_vals$plot11L = getDataPlot2D(3, 2, 1)
  
  anal_plot_vals$plot21 = getDataPlot2D(4, 1, 1)
  anal_plot_vals$plot21L = getDataPlot2D(4, 2, 1)
  
  anal_plot_vals$plot31 = getDataPlot2D(2, 1, 1)
  
  
  # Column 2 (RoRBC_ins and RoRBC_reins)
  anal_plot_vals$plot12 = getDataPlot2D(3, 5, 5)
  anal_plot_vals$plot12L = getDataPlot2D(3, 6, 5)
  
  anal_plot_vals$plot22 = getDataPlot2D(4, 5, 5)
  anal_plot_vals$plot22L = getDataPlot2D(4, 6, 5)
  
  anal_plot_vals$plot32 = getDataPlot2D(6, 5, 5)
  
  # Reinsurer Optimization
  # Column 3 (Z_ins and Z_reins)
  anal_plot_vals$plot13 = getDataPlot2D(3, 1, 2)
  anal_plot_vals$plot13L = getDataPlot2D(3, 2, 2)
  
  anal_plot_vals$plot23 = getDataPlot2D(4, 1, 2)
  anal_plot_vals$plot23L = getDataPlot2D(4, 2, 2)
  
  anal_plot_vals$plot33 = getDataPlot2D(2, 1, 2)
  
  # Column 4 (RoRBC_ins and RoRBC_reins)
  anal_plot_vals$plot14 = getDataPlot2D(3, 5, 6)
  anal_plot_vals$plot14L = getDataPlot2D(3, 6, 6)
  
  anal_plot_vals$plot24 = getDataPlot2D(4, 5, 6)
  anal_plot_vals$plot24L = getDataPlot2D(4, 6, 6)
  
  anal_plot_vals$plot34 = getDataPlot2D(6, 5, 6)
  
  
  
  #anal_plot_vals$plot12 = getDataPlot2D(3, 1, 2, F)
  #anal_plot_vals$plot22 = getDataPlot2D(3, 2, 2, F)
  
  #anal_plot_vals$plot11 = getDataPlot2D(1, 2, 2)
  #anal_plot_vals$plot11 = getDataPlot2D(1, 2, 2)
  
  
  
  
  ################
  # Monte Carlo Simulation
  output$userInputTable = renderDT(userInput,
                        editable = 'cell',
                        rownames = TRUE,
                        options = list(paging = F, searching = F)
                        )
  
  
  # Simulaton
  output$sim_L = renderDT(round(get_sim_L(userInput[1:2,]), 2),
                          editable = FALSE,
                          rownames = TRUE,
                          options = list(paging = F, searching = F))
  

  output$sim_U = renderDT(round(vals$u, 2),
                          editable = FALSE,
                          colnames = c("LoB1", "LoB2", "LoB3"),
                          rownames = TRUE,
                          options = list(paging = F, searching = F))
  
  # Scenarios
  output$corrTable = renderTable(colnames = FALSE, {
    vals$uniformNums
  })
  output$lossTable = renderTable(colnames = FALSE, {
    vals$lossNums
  })
  output$PLTable = renderTable(colnames = FALSE, {
    vals$P_L
  })
  
  # edit input values
  observeEvent(input$userInputTable_cell_edit, {
    userInput <<- editData(userInput, input$userInputTable_cell_edit, 'userInputTable')
    vals$sim_L <<- get_sim_L(userInput[1:2,])
    output$sim_L = renderDT(round(vals$sim_L, 2),
                            editable = FALSE,
                            rownames = TRUE,
                            options = list(paging = F, searching = F))
  })
  
  
  
  ################
  # Without Reinsurance
  # Undiversified
  output$undiv_z_table = renderDT(round(vals$undiv_z, 2),
                          editable = FALSE,
                          rownames = c("E[Z]", "RBC[Z]", "RoRBC[Z]"),
                          colnames = c("LoB1", "LoB2", "LoB3", "Total"),
                          options = list(paging = F, searching = F))
  output$undiv_u_table = renderDT(round(vals$undiv_u, 2),
                                  editable = FALSE,
                                  rownames = c("E[U]", "99% ES[U]"),
                                  colnames = c("LoB1", "LoB2", "LoB3", "Total"),
                                  options = list(paging = F, searching = F))
  output$undiv_capCost_table = renderDT(round(vals$undiv_capCost, 2),
                                  editable = FALSE,
                                  rownames = c("RBC[Z]", "CoC[Z]"),
                                  colnames = c("LoB1", "LoB2", "LoB3", "Total"),
                                  options = list(paging = F, searching = F))
  
  
  # Diversified
  output$div_riskCap_table = renderDT(round(vals$div_riskCap, 2),
                                  editable = FALSE,
                                  rownames = c("undiv", "diff", "div"),
                                  colnames = c("LoB1", "LoB2", "LoB3", "Total"),
                                  options = list(paging = F, searching = F))
  output$div_z_table = renderDT(round(vals$div_z, 2),
                                  editable = FALSE,
                                  rownames = c("E[Z]", "RBC[Z]", "RoRBC[Z]"),
                                  colnames = c("LoB1", "LoB2", "LoB3", "Total"),
                                  options = list(paging = F, searching = F))
  output$div_u_table = renderDT(round(vals$div_u, 2),
                                  editable = FALSE,
                                  rownames = c("E[U]", "99% ES[U]"),
                                  colnames = c("LoB1", "LoB2", "LoB3", "Total"),
                                  options = list(paging = F, searching = F))
  output$div_capCost_table = renderDT(round(vals$div_capCost, 2),
                                        editable = FALSE,
                                        rownames = c("RBC[Z]", "CoC[Z]"),
                                        colnames = c("LoB1", "LoB2", "LoB3", "Total"),
                                        options = list(paging = F, searching = F))
  
  
  
  ################
  # With Reinsurance
  x = c(1,2,3,4,5)
  y = c(1,2,3,4,5)
  z = rbind(
    c(0, 1, 0, 1, 0),
    c(1, 0, 1, 0, 1),
    c(0, 1, 0, 1, 0),
    c(1, 0, 1, 0, 1),
    c(0, 1, 0, 1, 0))
  
  #output$plot = renderPlotly({
  #  plot1 = plot_ly(
  #    x = vals$ins_x,
  #    y = vals$ins_y,
  #    z = vals$ins_z
  #    #z = matrix(1:12, ncol = 3), #vals$data, #
  #    #type = "surface"
  #  ) %>% add_surface()
  #})
  
  
  # Effect of Reinsurance on CDF
  output$reins_plot = renderPlot({
    min_x = reins_plot_bounds$min_x
    max_x = reins_plot_bounds$max_x
    p = plot(reins_plot_vals$L_plot, ylim=c(0,1),
             xlim=c(min_x, max_x),
             col="red", main="CDF (Loss including Premium payable to reinsurer)", xlab = "Loss")
    lines(reins_plot_vals$L_plot_after,lty=3,verticals=T, col="green")
    lines(reins_plot_vals$L_plot_reins,lty=3,verticals=T, col="blue")
    legend(max_x - 0.2*(max_x - min_x), 0.2, legend=c("CDF before", "CDF Insurer", "CDF Reinsurer"),
           col=c("red", "green", "blue"), lty=1:2, cex=0.8)
  })
  
  output$reins_p1 = renderText({reins_plot_vals$reins_p1})
  output$reins_p2 = renderText({reins_plot_vals$reins_p2})
  output$reins_p3 = renderText({reins_plot_vals$reins_p3})
  
  # Reinsurance Premium Plot
  output$reins_p_plot = renderPlotly({
    lob = input$reins_p_plot_lob
    if (lob == "LoB1") {
      z_ = reins_prem_plot_vals$z1
    } else if (lob == "LoB2") {
      z_ = reins_prem_plot_vals$z2
    } else {
      z_ = reins_prem_plot_vals$z3
    }
    plot1 = plot_ly(
      x = reins_prem_plot_vals$x,
      y = reins_prem_plot_vals$y,
      z = z_,
      scene = "scene1",
      type = "surface",
      showscale = TRUE
    )
    #plot2 = plot_ly(
    #  x = reins_prem_plot_vals$x,
    #  y = reins_prem_plot_vals$y,
    #  z = reins_prem_plot_vals$z2,
    #  scene = "scene2",
    #  type = "surface",
    #  showscale = FALSE
    #)
    #plot3 = plot_ly(
    #  x = reins_prem_plot_vals$x,
    #  y = reins_prem_plot_vals$y,
    #  z = reins_prem_plot_vals$z3,
    #  scene = "scene3",
    #  type = "surface",
    #  showscale = TRUE,
    #)
    # subplot and define scene
    #fig = subplot(plot1, plot2, plot3) 
    #fig = fig %>% layout(title = "Premium Surface Plots",
    #                     scene = list(domain=list(x=c(0,0.5),y=c(0.5,1))),
    #                     scene2 = list(domain=list(x=c(0.5,1),y=c(0.5,1))),
    #                     scene3 = list(domain=list(x=c(0,0.5),y=c(0,0.5)))
    #                     )
    
  })
  
  
  ################
  # Optimization
  
  output$opt_premium_ins = renderDT(if (is.null(userInputPrem$premium)) {
    userInputPremium_default
  } else {
    userInputPrem$premium
  },
                                   editable = 'cell',
                                   rownames = TRUE,
                                   options = list(paging = F, searching = F)
  )
  
  output$opt_result_inp = renderDT(signif(opt_vals$input, 5),
                                    editable = FALSE,
                                    rownames = TRUE,
                                    options = list(paging = F, searching = F)
  )
  
  output$opt_result_out = renderDT(signif(opt_vals$output, 5),
                                    editable = FALSE,
                                    rownames = TRUE,
                                    options = list(paging = F, searching = F)
  )
  
  ###############
  # Analysis
  output$anal_plot1 = renderPlotly({
    
    # Insurer Optimization
    p11 = anal_plot_vals$plot11
    p11L = anal_plot_vals$plot11L
    p12 = anal_plot_vals$plot12
    p12L = anal_plot_vals$plot12L
    p21 = anal_plot_vals$plot21
    p21L = anal_plot_vals$plot21L
    p22 = anal_plot_vals$plot22
    p22L = anal_plot_vals$plot22L
    
    #p31 = anal_plot_vals$plot31
    #p32 = anal_plot_vals$plot32
    
    # Reinsurer Optimization
    #p13 = anal_plot_vals$plot13
    #p13L = anal_plot_vals$plot13L
    #p14 = anal_plot_vals$plot14
    #p14L = anal_plot_vals$plot14L
    #p23 = anal_plot_vals$plot23
    #p23L = anal_plot_vals$plot23L
    #p24 = anal_plot_vals$plot24
    #p24L = anal_plot_vals$plot24L
    
    #p33 = anal_plot_vals$plot33
    #p34 = anal_plot_vals$plot34
    

    # Insurer Optimization
    # Column 1 (Z)
    plot11 = plot_ly(
      x = p11[,1],
      y = p11[,2],
      type = "scatter",
      mode = 'lines+markers',
      name = "Z Insurer",
      marker = list(color = cols[1]),
      line = list(color = cols[1]),
      showlegend = F
    ) %>%
      layout(xaxis = list(title = colnames(p11)[1]), yaxis = list(title = "Z")) %>%
      add_trace(y = p11L[,2], name = "Z Reinsurer", showlegend = F,
                marker = list(color = cols[2]),
                line = list(color = cols[2]))
    
    plot21 = plot_ly(
      x = p21[,1],
      y = p21[,2],
      type = "scatter",
      mode = 'lines+markers',
      showlegend = F,
      marker = list(color = cols[1]),
      line = list(color = cols[1]),
    ) %>%
      layout(xaxis = list(title = colnames(p21)[1]), yaxis = list(title = "Z")) %>%
      add_trace(y = p21L[,2], showlegend = F,
                marker = list(color = cols[2]),
                line = list(color = cols[2]))
    
    #plot31 = plot_ly(
    #  x = p31[,1],
    #  y = p31[,2],
    #  type = "scatter",
    #  mode = 'lines+markers',
    #  marker = list(color = cols[1]),
    #  line = list(color = cols[1]),
    #  showlegend = F
    #) %>%
    #  layout(xaxis = list(title = "Z Reinsurer"),
    #         yaxis = list(title = "Z Insurer"))
    
    
    # Column 2 (RoRBC)
    plot12 = plot_ly(
      x = p12[,1],
      y = p12[,2],
      type = "scatter",
      mode = 'lines+markers',
      name = "RoRBC Insurer",
      marker = list(color = cols[3]),
      line = list(color = cols[3]),
      showlegend = F
    ) %>%
      layout(xaxis = list(title = colnames(p12)[1]), yaxis = list(title = "RoRBC", range = list(-1, 1))) %>%
      add_trace(y = p12L[,2], name = "RoRBC Reinsurer", showlegend = F,
                marker = list(color = cols[4]),
                line = list(color = cols[4]))
    
    plot22 = plot_ly(
      x = p22[,1],
      y = p22[,2],
      type = "scatter",
      mode = 'lines+markers',
      marker = list(color = cols[3]),
      line = list(color = cols[3]),
      showlegend = F
    ) %>%
      layout(xaxis = list(title = colnames(p22)[1]), yaxis = list(title = "RoRBC", range = list(-1, 1))) %>%
      add_trace(y = p22L[,2], showlegend = F,
                marker = list(color = cols[4]),
                line = list(color = cols[4]))
    
    #plot32 = plot_ly(
    #  x = p32[,1],
    #  y = p32[,2],
    #  type = "scatter",
    #  mode = 'lines+markers',
    #  marker = list(color = cols[3]),
    #  line = list(color = cols[3]),
    #  showlegend = F
    #) %>%
    #  layout(xaxis = list(title = "RoRBC Reinsurer"),
    #         yaxis = list(title = "RoRBC Insurer"))
    
    
    # Reinsurer Optimization
    # Column 3 (Z)
    #plot13 = plot_ly(
    #  x = p13[,1],
    #  y = p13[,2],
    #  type = "scatter",
    #  mode = 'lines+markers',
    #  marker = list(color = cols[1]),
    #  line = list(color = cols[1]),
    #  showlegend = F
    #) %>%
    #  layout(xaxis = list(title = colnames(p13)[1]), yaxis = list(title = "Z")) %>%
    #  add_trace(y = p13L[,2], showlegend = F,
    #            marker = list(color = cols[2]),
    #            line = list(color = cols[2]))
    
    #plot23 = plot_ly(
    #  x = p23[,1],
    #  y = p23[,2],
    #  type = "scatter",
    #  mode = 'lines+markers',
    #  showlegend = F,
    #  marker = list(color = cols[1]),
    #  line = list(color = cols[1]),
    #) %>%
    #  layout(xaxis = list(title = colnames(p23)[1]), yaxis = list(title = "Z")) %>%
    #  add_trace(y = p23L[,2], showlegend = F,
    #            marker = list(color = cols[2]),
    #            line = list(color = cols[2]))
    
    #plot33 = plot_ly(
    #  x = p33[,1],
    #  y = p33[,2],
    #  type = "scatter",
    #  mode = 'lines+markers',
    #  marker = list(color = cols[1]),
    #  line = list(color = cols[1]),
    #  showlegend = F
    #) %>%
    #  layout(xaxis = list(title = "Z Reinsurer"),
    #         yaxis = list(title = "Z Insurer"))
    
    
    # Column 4 (RoRBC)
    #plot14 = plot_ly(
    #  x = p14[,1],
    #  y = p14[,2],
    #  type = "scatter",
    #  mode = 'lines+markers',
    #  marker = list(color = cols[3]),
    #  line = list(color = cols[3]),
    #  showlegend = F
    #) %>%
    #  layout(xaxis = list(title = colnames(p14)[1]), yaxis = list(title = "RoRBC", range = list(-1, 1))) %>%
    #  add_trace(y = p14L[,2], showlegend = F,
    #            marker = list(color = cols[4]),
    #            line = list(color = cols[4]))
    
    #plot24 = plot_ly(
    #  x = p24[,1],
    #  y = p24[,2],
    #  type = "scatter",
    #  mode = 'lines+markers',
    #  marker = list(color = cols[3]),
    #  line = list(color = cols[3]),
    #  showlegend = F
    #) %>%
    #  layout(xaxis = list(title = colnames(p22)[1]), yaxis = list(title = "RoRBC", range = list(-1, 1))) %>%
    #  add_trace(y = p24L[,2], showlegend = F,
    #            marker = list(color = cols[4]),
    #            line = list(color = cols[4]))
    
    #plot34 = plot_ly(
    #  x = p34[,1],
    #  y = p34[,2],
    #  type = "scatter",
    #  mode = 'lines+markers',
    #  marker = list(color = cols[3]),
    #  line = list(color = cols[3]),
    #  showlegend = F
    #) %>%
    #  layout(xaxis = list(title = "RoRBC Reinsurer"),
    #         yaxis = list(title = "RoRBC Insurer"))
    
    
    
    fig = subplot(plot11, plot12,
                  plot21, plot22,
                  nrows = 2, titleX = TRUE, titleY = TRUE, margin = 0.09)
    #fig = fig %>% layout(title = list(text = "Stacked Subplots"),
    #                     plot_bgcolor='#e5ecf6', 
    #                     xaxis = list( 
    #                       zerolinecolor = '#ffff', 
    #                       zerolinewidth = 2, 
    #                       gridcolor = 'ffff'), 
    #                     yaxis = list( 
    #                       zerolinecolor = '#ffff', 
    #                       zerolinewidth = 2, 
    #                       gridcolor = 'ffff')) 
    
    #fig = fig %>% layout(title = "Analysis Plots",
    #                     scene11 = list(domain = list(x = c(0, 0.5), y = c(0.5, 1))),
    #                     scene12 = list(domain = list(x = c(0.5, 1), y = c(0.5, 1))),
    #                     scene21 = list(domain = list(x = c(0, 0.5), y = c(0, 0.5))),
    #                     scene22 = list(domain = list(x = c(0.5, 1), y = c(0, 0.5))))
  })
  
  output$anal_plot2 = renderPlotly({
    
    # Reinsurer Optimization
    p13 = anal_plot_vals$plot13
    p13L = anal_plot_vals$plot13L
    p14 = anal_plot_vals$plot14
    p14L = anal_plot_vals$plot14L
    p23 = anal_plot_vals$plot23
    p23L = anal_plot_vals$plot23L
    p24 = anal_plot_vals$plot24
    p24L = anal_plot_vals$plot24L
    
    
    # Reinsurer Optimization
    # Column 3 (Z)
    plot13 = plot_ly(
      x = p13[,1],
      y = p13[,2],
      type = "scatter",
      mode = 'lines+markers',
      marker = list(color = cols[1]),
      line = list(color = cols[1]),
      name = "Z Insurer",
      showlegend = T
    ) %>%
      layout(xaxis = list(title = colnames(p13)[1]), yaxis = list(title = "Z")) %>%
      add_trace(y = p13L[,2], name = "Z Reinsurer", showlegend = T,
                marker = list(color = cols[2]),
                line = list(color = cols[2]))
    
    plot23 = plot_ly(
      x = p23[,1],
      y = p23[,2],
      type = "scatter",
      mode = 'lines+markers',
      showlegend = F,
      marker = list(color = cols[1]),
      line = list(color = cols[1]),
    ) %>%
      layout(xaxis = list(title = colnames(p23)[1]), yaxis = list(title = "Z")) %>%
      add_trace(y = p23L[,2], showlegend = F,
                marker = list(color = cols[2]),
                line = list(color = cols[2]))
    
    
    # Column 4 (RoRBC)
    plot14 = plot_ly(
      x = p14[,1],
      y = p14[,2],
      type = "scatter",
      mode = 'lines+markers',
      marker = list(color = cols[3]),
      line = list(color = cols[3]),
      name = "RoRBC Insurer",
      showlegend = T
    ) %>%
      layout(xaxis = list(title = colnames(p14)[1]), yaxis = list(title = "RoRBC", range = list(-1, 1))) %>%
      add_trace(y = p14L[,2], name = "RoRBC Reinsurer", showlegend = T,
                marker = list(color = cols[4]),
                line = list(color = cols[4]))
    
    plot24 = plot_ly(
      x = p24[,1],
      y = p24[,2],
      type = "scatter",
      mode = 'lines+markers',
      marker = list(color = cols[3]),
      line = list(color = cols[3]),
      showlegend = F
    ) %>%
      layout(xaxis = list(title = colnames(p24)[1]), yaxis = list(title = "RoRBC", range = list(-1, 1))) %>%
      add_trace(y = p24L[,2], showlegend = F,
                marker = list(color = cols[4]),
                line = list(color = cols[4]))

    
    fig = subplot(plot13, plot14,
                  plot23, plot24,
                  nrows = 2, titleX = TRUE, titleY = TRUE, margin = 0.09)
    
  })
  
  output$anal_plot3 = renderPlotly({
    
    # Insurer Optimization
    
    p31 = anal_plot_vals$plot31
    p32 = anal_plot_vals$plot32
    
    
    # Insurer Optimization
    plot31 = plot_ly(
      x = p31[,1],
      y = p31[,2],
      type = "scatter",
      mode = 'lines+markers',
      marker = list(color = cols[1]),
      line = list(color = cols[1]),
      showlegend = F
    ) %>%
      layout(xaxis = list(title = "Z Reinsurer"),
             yaxis = list(title = "Z Insurer"))
    
    plot32 = plot_ly(
      x = p32[,1],
      y = p32[,2],
      type = "scatter",
      mode = 'lines+markers',
      marker = list(color = cols[3]),
      line = list(color = cols[3]),
      showlegend = F
    ) %>%
      layout(xaxis = list(title = "RoRBC Reinsurer", range = c(0, 8)),
             yaxis = list(title = "RoRBC Insurer", range = c(-0.5, 0.5)))
    
    
    fig = subplot(plot31, plot32,
                  nrows = 1, titleX = TRUE, titleY = TRUE, margin = 0.09)
    

    
  })
  
  output$anal_plot4 = renderPlotly({
    
    # Reinsurer Optimization

    p33 = anal_plot_vals$plot33
    p34 = anal_plot_vals$plot34

    # Reinsurer Optimization
    # Column 3 (Z)
    plot33 = plot_ly(
      x = p33[,1],
      y = p33[,2],
      type = "scatter",
      mode = 'lines+markers',
      marker = list(color = cols[1]),
      line = list(color = cols[1]),
      showlegend = F
    ) %>%
      layout(xaxis = list(title = "Z Reinsurer"),
             yaxis = list(title = "Z Insurer"))
    
    
    # Column 4 (RoRBC)
    plot34 = plot_ly(
      x = p34[,1],
      y = p34[,2],
      type = "scatter",
      mode = 'lines+markers',
      marker = list(color = cols[3]),
      line = list(color = cols[3]),
      showlegend = F
    ) %>%
      layout(xaxis = list(title = "RoRBC Reinsurer", range = c(0, 8)),
             yaxis = list(title = "RoRBC Insurer", range = c(-0.5, 0.5)))
    
    
    
    
    fig = subplot(plot33, plot34,
                  nrows = 1, titleX = TRUE, titleY = TRUE, margin = 0.09)
    
  })
  
  ##################
}

# Run the app ----
shinyApp(ui = ui, server = server)