#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(plotly)
require(tidyverse)
library(mc2d)
library(shinythemes)


shinyUI(fluidPage(theme = shinytheme("slate"),
    tabsetPanel(
        tabPanel("Total Cost Simulation", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(numericInput("yearly_inflation", "Yearly Inflation Rate [%]", min = 0.00, value = 0), helpText("Please insert your yearly estimated inflation rate for the next five years."), 
                                  numericInput("yearly_cost_increase", "Yearly Cost Increase [%]", min = 0.00, value = 0), helpText("Please insert your yearly estimated cost increase rate for the next five years."),
                                  submitButton("Update & Simulate", icon("refresh")), helpText("Please update once after you inserted the values and twice to aggregate all costs.")),
                     mainPanel(
                         plotlyOutput("line_plot_total_cost"),
                         plotlyOutput("table_total_cost")
                     )
                 )
        ),
        tabPanel("Material Costs Simulation", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(numericInput("material_cost_min", "Minimum Material Costs [€]", min = 0.00, value = 0), helpText("Please insert your lowest extimated Material Costs (quarterly) for the next five years."),
                                  numericInput("material_cost_base", "Expected Material Costs [€]", min = 0.00, value = 0), helpText("Please insert your average extimated Material Costs (quarterly) for the next five years."),
                                  numericInput("material_cost_max", "Maximum Case Material Costs [€]", min = 0.00, value = 0), helpText("Please insert your highest extimated Material Costs (quarterly) for the next five years."),
                                  selectInput("material_cost_distribution", "Distribution", c("PERT Beta Distribution" = "PERT Beta", "Normal Distribution" = "Normal", "Triangular Distribution" = "Triangular")), helpText("Please choose the distribution which mimics the risk profile of your costs best. Use the graphs below."),
                                  submitButton("Update & Simulate", icon("refresh")), helpText(""),
                                  img(src = "distribution_plots.png", width = "100%")),
                     mainPanel(
                         plotlyOutput("line_plot_material_cost"),
                         plotlyOutput("table_material_cost")
                     )
                 )
        ),
        tabPanel("Personnel Costs Simulation", fluid = TRUE,
                 sidebarLayout(   
                     sidebarPanel(numericInput("personnel_cost_min", "Minimum Personnel Costs [€]", min = 0.00, value = 0), helpText("Please insert your lowest extimated Personnel Costs (quarterly) for the next five years."),
                                  numericInput("personnel_cost_base", "Expected Personnel Costs [€]", min = 0.00, value = 0), helpText("Please insert your average extimated Personnel Costs (quarterly) for the next five years."),
                                  numericInput("personnel_cost_max", "Maximum Personnel Costs [€]", min = 0.00, value = 0), helpText("Please insert your highest extimated Personnel Costs (quarterly) for the next five years."),
                                  selectInput("personnel_cost_distribution", "Distribution", c("PERT Beta Distribution" = "PERT Beta", "Normal Distribution" = "Normal", "Triangular Distribution" = "Triangular")), helpText("Please choose the distribution which mimics the risk profile of your costs best. Use the graphs below"),
                                  submitButton("Update & Simulate", icon("refresh")), helpText(""),
                                  img(src = "distribution_plots.png", width = "100%")),
                     mainPanel(
                         plotlyOutput("line_plot_personnel_cost"),
                         plotlyOutput("table_personnel_cost")
                     )
                 )
        ),
        tabPanel("Other Operating Costs Simulation", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(numericInput("otheroperating_cost_min", "Minimum Other Operating Costs [€]", min = 0.00, value = 0), helpText("Please insert your lowest extimated Other Operating Costs (quarterly) for the next five years."),
                                  numericInput("otheroperating_cost_base", "Expected Other Operating Costs [€]", min = 0.00, value = 0), helpText("Please insert your average extimated Other Operating Costs (quarterly) for the next five years."),
                                  numericInput("otheroperating_cost_max", "Maximum Other Operating Costs [€]", min = 0.00, value = 0), helpText("Please insert your highest extimated Other Operating Costs (quarterly) for the next five years."),
                                  selectInput("otheroperating_cost_distribution", "Distribution", c("PERT Beta Distribution" = "PERT Beta", "Normal Distribution" = "Normal", "Triangular Distribution" = "Triangular")), helpText("Please choose the distribution which mimics the risk profile of your costs best. Use the graphs below"),
                                  submitButton("Update & Simulate", icon("refresh")), helpText(""),
                                  img(src = "distribution_plots.png", width = "100%")),
                     mainPanel(
                         plotlyOutput("line_plot_otheroperating_cost"),
                         plotlyOutput("table_otheroperating_cost")
                     )
                 )
        ),
        tabPanel("Depreciation and Amortization ", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(numericInput("depreciation_amortization_min", "Minimum Depreciation and Amortization [€]", min = 0.00, value = 0), helpText("Please insert your lowest extimated Depreciation and Amortization (quarterly) for the next five years."),
                                  numericInput("depreciation_amortization_base", "Expected Depreciation and Amortization [€]", min = 0.00, value = 0), helpText("Please insert your average extimated Depreciation and Amortization (quarterly) for the next five years."),
                                  numericInput("depreciation_amortization_max", "Maximum Depreciation and Amortization [€]", min = 0.00, value = 0), helpText("Please insert your highest extimated Depreciation and Amortization (quarterly) for the next five years."),
                                  selectInput("depreciation_amortization_distribution", "Distribution", c("PERT Beta Distribution" = "PERT Beta", "Normal Distribution" = "Normal", "Triangular Distribution" = "Triangular")), helpText("Please choose the distribution which mimics the risk profile of your costs best. Use the graphs below"),
                                  submitButton("Update & Simulate", icon("refresh")), helpText(""),
                                  img(src = "distribution_plots.png", width = "100%")),
                     mainPanel(
                         plotlyOutput("line_plot_depreciation_amortization"),
                         plotlyOutput("table_depreciation_amortization")
                     )
                 )
        ),
        tabPanel("Interest Costs Simulation", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(numericInput("interests_cost_min", "Minimum Interest Costs [€]", min = 0.00, value = 0), helpText("Please insert your lowest extimated Interest Costs (quarterly) for the next five years."), 
                                  numericInput("interests_cost_base", "Expected Interest Costs [€]", min = 0.00, value = 0),helpText("Please insert your average extimated Interest Costs (quarterly) for the next five years."),
                                  numericInput("interests_cost_max", "Maximum Interest Costs [€]", min = 0.00, value = 0),helpText("Please insert your highest extimated Interest Costs (quarterly) for the next five years."),
                                  selectInput("interests_cost_distribution", "Distribution", c("PERT Beta Distribution" = "PERT Beta", "Normal Distribution" = "Normal", "Triangular Distribution" = "Triangular")), helpText("Please choose the distribution which mimics the risk profile of your costs best. Use the graphs below"),
                                  submitButton("Update & Simulate", icon("refresh")), helpText(""),
                                  img(src = "distribution_plots.png", width = "100%")),
                     mainPanel(
                         plotlyOutput("line_plot_interests_cost"),
                         plotlyOutput("table_interests_cost")
                     )
                 )
        )
    )
))