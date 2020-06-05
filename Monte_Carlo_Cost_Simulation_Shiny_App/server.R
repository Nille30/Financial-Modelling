#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(plotly)
require(tidyverse)
library(mc2d)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        #
        drift = reactive({(input$yearly_cost_increase + input$yearly_inflation) / 4 / 100})
        
        #Simulate Material Costs
        material_cost_data = reactive({
            if (input$material_cost_distribution == "PERT Beta") {
            material_cost_data = rpert(10000, min=input$material_cost_min, mode=input$material_cost_base, max=input$material_cost_max, shape=1)
            } else if (input$material_cost_distribution == "Normal") {
            material_cost_data = rnorm(1000, mean=mean(c(input$material_cost_min, input$material_cost_base, input$material_cost_max)), sd=sd(c(input$material_cost_min, input$material_cost_base, input$material_cost_max)))
            } else {
            material_cost_data = rtriang(1000, min=input$material_cost_min, mode=input$material_cost_base, max=input$material_cost_max)
            }
        })
        
        material_cost_paths = reactive({
            material_cost_t1 = sample((material_cost_data()[material_cost_data() >= quantile(material_cost_data(), 0.35) & material_cost_data() <= quantile(material_cost_data(), 0.65)]), 1)
            material_cost_vol = sd(material_cost_data()) / mean(material_cost_data()) #material_cost_volatility in percent
            quartersCount = 20 #Number of quarters which will be
            numberIterations = 400 #Number of iterations for each quarter
            material_cost_paths = matrix(material_cost_t1, numberIterations, quartersCount)
            for (d in 1:quartersCount ) {
                for (i in 1:numberIterations ) {
                    if (d>1) 
                        material_cost_paths[i,d] = material_cost_paths[i,d-1] * (1 + drift()/quartersCount+ (runif(1, 0, 1) - 0.5) * material_cost_vol * sqrt(1/quartersCount))
                }
            }
            material_cost_paths[is.nan( material_cost_paths)] = 0
            material_cost_paths 
        })


    output$line_plot_material_cost <- renderPlotly({
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        material_cost_av = rep(0,quartersCount)
        material_cost_avg = rep(0,quartersCount)
        material_cost_minv = rep(0,quartersCount)
        material_cost_maxv = rep(0,quartersCount)
        
        for (d in 1:quartersCount ) {
            material_cost_av[d]=median(material_cost_paths()[,d])
            material_cost_avg[d]=mean(material_cost_paths()[,d])
            material_cost_minv[d]=min(material_cost_paths()[,d])
            material_cost_maxv[d]=max(material_cost_paths()[,d])
        }
        
        x_data = factor(c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #X axis data ticks as factor
        x_data = factor(x_data, levels =c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #Leveling X axis data ticks to material_cost_avoid sorting
        x_axis = list(title="Quarter", fond=list(size=12)) #Attributes for X axis label
        y_axis = list(title="Costs per quarter [€]", fond=list(size=12)) #Attributes for Y axis label
        
        line_plot_material_cost = plot_ly()
        line_plot_material_cost = plot_ly(x = ~x_data, y = ~material_cost_avg, type="scatter", mode="lines", name="Base Case Scenario", line=list(color='rgb(0,0,255)', width=2))
        line_plot_material_cost = line_plot_material_cost %>% add_trace(y= ~material_cost_minv, mode="lines", name="Best Case Scenario", line=list(color='rgb(0,255,0)', width=2))
        line_plot_material_cost = line_plot_material_cost %>% add_trace(y= ~material_cost_maxv, mode="lines", name="Worst Case Scenario", line=list(color='rgb(255,0,0)', width=2))
        line_plot_material_cost = line_plot_material_cost %>% layout(title="5 Year Cost Simulation \n of Material Costs", legend = list(orientation = 'h', font=list(size=6)), xaxis=x_axis, yaxis=y_axis)
        
        #Plots a specific amount of simulations based on the modulo operation 
        for (i in 1:numberIterations) {
            #if (i %% 5 == 0) {
            line_plot_material_cost = line_plot_material_cost %>% add_trace(y= material_cost_paths()[i,], mode="lines", name= i, line=list(color='rgb(140,140,140)', width=0.5), showlegend=FALSE)
            #}
        } 
        line_plot_material_cost
    })
    
    output$table_material_cost = renderPlotly({
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        material_cost_av = rep(0,quartersCount)
        material_cost_avg = rep(0,quartersCount)
        material_cost_minv = rep(0,quartersCount)
        material_cost_maxv = rep(0,quartersCount)
        material_cost_q10 = rep(0,quartersCount)
        material_cost_q25 = rep(0,quartersCount)
        material_cost_q75 = rep(0,quartersCount)
        material_cost_q90 = rep(0,quartersCount)

        
        for (d in 1:quartersCount ) {
            material_cost_av[d]=median(material_cost_paths()[,d])
            material_cost_avg[d]=mean(material_cost_paths()[,d])
            material_cost_minv[d]=min(material_cost_paths()[,d])
            material_cost_maxv[d]=max(material_cost_paths()[,d])
            material_cost_q10[d]=quantile(material_cost_paths()[,d], probs=0.1, na.rm = TRUE)
            material_cost_q25[d]=quantile(material_cost_paths()[,d], probs=0.25, na.rm = TRUE)
            material_cost_q75[d]=quantile(material_cost_paths()[,d], probs=0.75, na.rm = TRUE)
            material_cost_q90[d]=quantile(material_cost_paths()[,d], probs=0.90, na.rm = TRUE)
        }
        
        x_data = factor(c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #X axis data ticks as factor
        x_data = factor(x_data, levels =c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #Leveling X axis data ticks to material_cost_avoid sorting
        x_axis = list(title="Quarter", fond=list(size=12)) #Attributes for X axis label
        y_axis = list(title="Costs per quarter [€]", fond=list(size=12))
        
        table_material_cost = data.frame(material_cost_minv,material_cost_avg,material_cost_maxv,material_cost_q10, material_cost_q25, material_cost_q75, material_cost_q90, row.names=x_data)
        names(table_material_cost) = c("Best Case Scenario", "Base Case Scenario", "Worst Case Scenario", "10th Quantile", "25th Quantile", "75th Quantile", "90th Quantile")
        table_material_cost = plot_ly(type='table', header = list(values = c("<b>Quarter</b>", names(table_material_cost)), align = c('left', rep('center', ncol(table_material_cost))), line = list(width = 1, color = 'black'),
                                                                  fill = list(color = 'rgb(39, 43, 48)'), font = list(family = "Arial", size = 14, color = "white")),
                                      cells = list(values = rbind(rownames(table_material_cost), t(as.matrix(unname(round(table_material_cost, digits=0))))),
                                                   align = c('left', rep('center', ncol(table_material_cost))), line = list(color = "black", width = 1), fill = list(color = c('rgb(39, 43, 48)', 'rgba(39, 43, 48, 0.65)')), font = list(family = "Arial", size = 12, color = c("white","black"))))
        
        
        
    })
    
    #Simulate Personnel Costs
    personnel_cost_data = reactive({
        if (input$personnel_cost_distribution == "PERT Beta") {
            personnel_cost_data = rpert(10000, min=input$personnel_cost_min, mode=input$personnel_cost_base, max=input$personnel_cost_max, shape=1)
        } else if (input$personnel_cost_distribution == "Normal") {
            personnel_cost_data = rnorm(1000, mean=mean(c(input$personnel_cost_min, input$personnel_cost_base, input$personnel_cost_max)), sd=sd(c(input$personnel_cost_min, input$personnel_cost_base, input$personnel_cost_max)))
        } else {
            personnel_cost_data = rtriang(1000, min=input$personnel_cost_min, mode=input$personnel_cost_base, max=input$personnel_cost_max)
        }
    })
    
    personnel_cost_paths = reactive({
        personnel_cost_vol = sd(personnel_cost_data()) / mean(personnel_cost_data()) #personnel_cost_volatility in percent
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        personnel_cost_t1 = sample((personnel_cost_data()[personnel_cost_data() >= quantile(personnel_cost_data(), 0.35) & personnel_cost_data() <= quantile(personnel_cost_data(), 0.65)]), 1)
        personnel_cost_paths = matrix(personnel_cost_t1, numberIterations, quartersCount)
        for (d in 1:quartersCount ) {
            for (i in 1:numberIterations ) {
                if (d>1) 
                    personnel_cost_paths[i,d] = personnel_cost_paths[i,d-1] * (1 + drift()/quartersCount+ (runif(1, 0, 1) - 0.5) * personnel_cost_vol * sqrt(1/quartersCount))
            }
        }
        personnel_cost_paths[is.nan(personnel_cost_paths)] = 0
        personnel_cost_paths 
    })
    
    
    output$line_plot_personnel_cost <- renderPlotly({
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        personnel_cost_av = rep(0,quartersCount)
        personnel_cost_avg = rep(0,quartersCount)
        personnel_cost_minv = rep(0,quartersCount)
        personnel_cost_maxv = rep(0,quartersCount)
        
        for (d in 1:quartersCount ) {
            personnel_cost_av[d]=median(personnel_cost_paths()[,d])
            personnel_cost_avg[d]=mean(personnel_cost_paths()[,d])
            personnel_cost_minv[d]=min(personnel_cost_paths()[,d])
            personnel_cost_maxv[d]=max(personnel_cost_paths()[,d])
        }
        
        x_data = factor(c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #X axis data ticks as factor
        x_data = factor(x_data, levels =c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #Leveling X axis data ticks to personnel_cost_avoid sorting
        x_axis = list(title="Quarter", fond=list(size=12)) #Attributes for X axis label
        y_axis = list(title="Costs per quarter [€]", fond=list(size=12)) #Attributes for Y axis label
        
        line_plot_personnel_cost = plot_ly()
        line_plot_personnel_cost = plot_ly(x = ~x_data, y = ~personnel_cost_avg, type="scatter", mode="lines", name="Base Case Scenario", line=list(color='rgb(0,0,255)', width=2))
        line_plot_personnel_cost = line_plot_personnel_cost %>% add_trace(y= ~personnel_cost_minv, mode="lines", name="Best Case Scenario", line=list(color='rgb(0,255,0)', width=2))
        line_plot_personnel_cost = line_plot_personnel_cost %>% add_trace(y= ~personnel_cost_maxv, mode="lines", name="Worst Case Scenario", line=list(color='rgb(255,0,0)', width=2))
        line_plot_personnel_cost = line_plot_personnel_cost %>% layout(title="5 Year Cost Simulation \n of Personnel Costs", legend = list(orientation = 'h', font=list(size=6)), xaxis=x_axis, yaxis=y_axis)
        
        #Plots a specific amount of simulations based on the modulo operation 
        for (i in 1:numberIterations) {
            #if (i %% 5 == 0) {
            line_plot_personnel_cost = line_plot_personnel_cost %>% add_trace(y= personnel_cost_paths()[i,], mode="lines", name= i, line=list(color='rgb(140,140,140)', width=0.5), showlegend=FALSE)
            #}
        } 
        line_plot_personnel_cost
    })
    
    output$table_personnel_cost = renderPlotly({
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        personnel_cost_av = rep(0,quartersCount)
        personnel_cost_avg = rep(0,quartersCount)
        personnel_cost_minv = rep(0,quartersCount)
        personnel_cost_maxv = rep(0,quartersCount)
        personnel_cost_q10 = rep(0,quartersCount)
        personnel_cost_q25 = rep(0,quartersCount)
        personnel_cost_q75 = rep(0,quartersCount)
        personnel_cost_q90 = rep(0,quartersCount)
        
        
        for (d in 1:quartersCount ) {
            personnel_cost_av[d]=median(personnel_cost_paths()[,d])
            personnel_cost_avg[d]=mean(personnel_cost_paths()[,d])
            personnel_cost_minv[d]=min(personnel_cost_paths()[,d])
            personnel_cost_maxv[d]=max(personnel_cost_paths()[,d])
            personnel_cost_q10[d]=quantile(personnel_cost_paths()[,d], probs=0.1, na.rm = TRUE)
            personnel_cost_q25[d]=quantile(personnel_cost_paths()[,d], probs=0.25, na.rm = TRUE)
            personnel_cost_q75[d]=quantile(personnel_cost_paths()[,d], probs=0.75, na.rm = TRUE)
            personnel_cost_q90[d]=quantile(personnel_cost_paths()[,d], probs=0.90, na.rm = TRUE)
        }
        
        x_data = factor(c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #X axis data ticks as factor
        x_data = factor(x_data, levels =c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #Leveling X axis data ticks to personnel_cost_avoid sorting
        x_axis = list(title="Quarter", fond=list(size=12)) #Attributes for X axis label
        y_axis = list(title="Costs per quarter [€]", fond=list(size=12))
        
        table_personnel_cost = data.frame(personnel_cost_minv,personnel_cost_avg,personnel_cost_maxv,personnel_cost_q10, personnel_cost_q25, personnel_cost_q75, personnel_cost_q90, row.names=x_data)
        names(table_personnel_cost) = c("Best Case Scenario", "Base Case Scenario", "Worst Case Scenario", "10th Quantile", "25th Quantile", "75th Quantile", "90th Quantile")
        table_personnel_cost = plot_ly(type='table', header = list(values = c("<b>Quarter</b>", names(table_personnel_cost)), align = c('left', rep('center', ncol(table_personnel_cost))), line = list(width = 1, color = 'black'),
                                                                  fill = list(color = 'rgb(39, 43, 48)'), font = list(family = "Arial", size = 14, color = "white")),
                                      cells = list(values = rbind(rownames(table_personnel_cost), t(as.matrix(unname(round(table_personnel_cost, digits=0))))),
                                                   align = c('left', rep('center', ncol(table_personnel_cost))), line = list(color = "black", width = 1), fill = list(color = c('rgb(39, 43, 48)', 'rgba(39, 43, 48, 0.65)')), font = list(family = "Arial", size = 12, color = c("white","black"))))
        
        
        
    })
    
    #Simulate Other Operating Costs
    otheroperating_cost_data = reactive({
        if (input$otheroperating_cost_distribution == "PERT Beta") {
            otheroperating_cost_data = rpert(10000, min=input$otheroperating_cost_min, mode=input$otheroperating_cost_base, max=input$otheroperating_cost_max, shape=1)
        } else if (input$otheroperating_cost_distribution == "Normal") {
            otheroperating_cost_data = rnorm(1000, mean=mean(c(input$otheroperating_cost_min, input$otheroperating_cost_base, input$otheroperating_cost_max)), sd=sd(c(input$otheroperating_cost_min, input$otheroperating_cost_base, input$otheroperating_cost_max)))
        } else {
            otheroperating_cost_data = rtriang(1000, min=input$otheroperating_cost_min, mode=input$otheroperating_cost_base, max=input$otheroperating_cost_max)
        }
    })
    
    otheroperating_cost_paths = reactive({
        otheroperating_cost_vol = sd(otheroperating_cost_data()) / mean(otheroperating_cost_data()) #otheroperating_cost_volatility in percent
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        otheroperating_cost_t1 = sample((otheroperating_cost_data()[otheroperating_cost_data() >= quantile(otheroperating_cost_data(), 0.35) & otheroperating_cost_data() <= quantile(otheroperating_cost_data(), 0.65)]), 1)
        otheroperating_cost_paths = matrix(otheroperating_cost_t1, numberIterations, quartersCount)
        for (d in 1:quartersCount ) {
            for (i in 1:numberIterations ) {
                if (d>1) 
                    otheroperating_cost_paths[i,d] = otheroperating_cost_paths[i,d-1] * (1 + drift()/quartersCount+ (runif(1, 0, 1) - 0.5) * otheroperating_cost_vol * sqrt(1/quartersCount))
            }
        }
        otheroperating_cost_paths[is.nan(otheroperating_cost_paths)] = 0
        otheroperating_cost_paths
    })
    
    
    output$line_plot_otheroperating_cost <- renderPlotly({
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        otheroperating_cost_av = rep(0,quartersCount)
        otheroperating_cost_avg = rep(0,quartersCount)
        otheroperating_cost_minv = rep(0,quartersCount)
        otheroperating_cost_maxv = rep(0,quartersCount)
        
        for (d in 1:quartersCount ) {
            otheroperating_cost_av[d]=median(otheroperating_cost_paths()[,d])
            otheroperating_cost_avg[d]=mean(otheroperating_cost_paths()[,d])
            otheroperating_cost_minv[d]=min(otheroperating_cost_paths()[,d])
            otheroperating_cost_maxv[d]=max(otheroperating_cost_paths()[,d])
        }
        
        x_data = factor(c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #X axis data ticks as factor
        x_data = factor(x_data, levels =c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #Leveling X axis data ticks to otheroperating_cost_avoid sorting
        x_axis = list(title="Quarter", fond=list(size=12)) #Attributes for X axis label
        y_axis = list(title="Costs per quarter [€]", fond=list(size=12)) #Attributes for Y axis label
        
        line_plot_otheroperating_cost = plot_ly()
        line_plot_otheroperating_cost = plot_ly(x = ~x_data, y = ~otheroperating_cost_avg, type="scatter", mode="lines", name="Base Case Scenario", line=list(color='rgb(0,0,255)', width=2))
        line_plot_otheroperating_cost = line_plot_otheroperating_cost %>% add_trace(y= ~otheroperating_cost_minv, mode="lines", name="Best Case Scenario", line=list(color='rgb(0,255,0)', width=2))
        line_plot_otheroperating_cost = line_plot_otheroperating_cost %>% add_trace(y= ~otheroperating_cost_maxv, mode="lines", name="Worst Case Scenario", line=list(color='rgb(255,0,0)', width=2))
        line_plot_otheroperating_cost = line_plot_otheroperating_cost %>% layout(title="5 Year Cost Simulation \n of Other Operating Costs", legend = list(orientation = 'h', font=list(size=6)), xaxis=x_axis, yaxis=y_axis)
        
        #Plots a specific amount of simulations based on the modulo operation 
        for (i in 1:numberIterations) {
            #if (i %% 5 == 0) {
            line_plot_otheroperating_cost = line_plot_otheroperating_cost %>% add_trace(y= otheroperating_cost_paths()[i,], mode="lines", name= i, line=list(color='rgb(140,140,140)', width=0.5), showlegend=FALSE)
            #}
        } 
        line_plot_otheroperating_cost
    })
    
    output$table_otheroperating_cost = renderPlotly({
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        otheroperating_cost_av = rep(0,quartersCount)
        otheroperating_cost_avg = rep(0,quartersCount)
        otheroperating_cost_minv = rep(0,quartersCount)
        otheroperating_cost_maxv = rep(0,quartersCount)
        otheroperating_cost_q10 = rep(0,quartersCount)
        otheroperating_cost_q25 = rep(0,quartersCount)
        otheroperating_cost_q75 = rep(0,quartersCount)
        otheroperating_cost_q90 = rep(0,quartersCount)
        
        
        for (d in 1:quartersCount ) {
            otheroperating_cost_av[d]=median(otheroperating_cost_paths()[,d])
            otheroperating_cost_avg[d]=mean(otheroperating_cost_paths()[,d])
            otheroperating_cost_minv[d]=min(otheroperating_cost_paths()[,d])
            otheroperating_cost_maxv[d]=max(otheroperating_cost_paths()[,d])
            otheroperating_cost_q10[d]=quantile(otheroperating_cost_paths()[,d], probs=0.1, na.rm = TRUE)
            otheroperating_cost_q25[d]=quantile(otheroperating_cost_paths()[,d], probs=0.25, na.rm = TRUE)
            otheroperating_cost_q75[d]=quantile(otheroperating_cost_paths()[,d], probs=0.75, na.rm = TRUE)
            otheroperating_cost_q90[d]=quantile(otheroperating_cost_paths()[,d], probs=0.90, na.rm = TRUE)
        }
        
        x_data = factor(c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #X axis data ticks as factor
        x_data = factor(x_data, levels =c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #Leveling X axis data ticks to otheroperating_cost_avoid sorting
        x_axis = list(title="Quarter", fond=list(size=12)) #Attributes for X axis label
        y_axis = list(title="Costs per quarter [€]", fond=list(size=12))
        
        table_otheroperating_cost = data.frame(otheroperating_cost_minv,otheroperating_cost_avg,otheroperating_cost_maxv,otheroperating_cost_q10, otheroperating_cost_q25, otheroperating_cost_q75, otheroperating_cost_q90, row.names=x_data)
        names(table_otheroperating_cost) = c("Best Case Scenario", "Base Case Scenario", "Worst Case Scenario", "10th Quantile", "25th Quantile", "75th Quantile", "90th Quantile")
        table_otheroperating_cost = plot_ly(type='table', header = list(values = c("<b>Quarter</b>", names(table_otheroperating_cost)), align = c('left', rep('center', ncol(table_otheroperating_cost))), line = list(width = 1, color = 'black'),
                                                                   fill = list(color = 'rgb(39, 43, 48)'), font = list(family = "Arial", size = 14, color = "white")),
                                       cells = list(values = rbind(rownames(table_otheroperating_cost), t(as.matrix(unname(round(table_otheroperating_cost, digits=0))))),
                                                    align = c('left', rep('center', ncol(table_otheroperating_cost))), line = list(color = "black", width = 1), fill = list(color = c('rgb(39, 43, 48)', 'rgba(39, 43, 48, 0.65)')), font = list(family = "Arial", size = 12, color = c("white","black"))))
        
        
        
    })
    
    #Simulate Depreciation and Amortization
    depreciation_amortization_data = reactive({
        if (input$depreciation_amortization_distribution == "PERT Beta") {
            depreciation_amortization_data = rpert(10000, min=input$depreciation_amortization_min, mode=input$depreciation_amortization_base, max=input$depreciation_amortization_max, shape=1)
        } else if (input$depreciation_amortization_distribution == "Normal") {
            depreciation_amortization_data = rnorm(1000, mean=mean(c(input$depreciation_amortization_min, input$depreciation_amortization_base, input$depreciation_amortization_max)), sd=sd(c(input$depreciation_amortization_min, input$depreciation_amortization_base, input$depreciation_amortization_max)))
        } else {
            depreciation_amortization_data = rtriang(1000, min=input$depreciation_amortization_min, mode=input$depreciation_amortization_base, max=input$depreciation_amortization_max)
        }
    })
    
    depreciation_amortization_paths = reactive({
        depreciation_amortization_vol = sd(depreciation_amortization_data()) / mean(depreciation_amortization_data()) #depreciation_amortization_volatility in percent
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        depreciation_amortization_t1 = sample((depreciation_amortization_data()[depreciation_amortization_data() >= quantile(depreciation_amortization_data(), 0.35) & depreciation_amortization_data() <= quantile(depreciation_amortization_data(), 0.65)]), 1)
        depreciation_amortization_paths = matrix(depreciation_amortization_t1, numberIterations, quartersCount)
        for (d in 1:quartersCount ) {
            for (i in 1:numberIterations ) {
                if (d>1) 
                    depreciation_amortization_paths[i,d] = depreciation_amortization_paths[i,d-1] * (1 + drift()/quartersCount+ (runif(1, 0, 1) - 0.5) * depreciation_amortization_vol * sqrt(1/quartersCount))
            }
        }
        depreciation_amortization_paths[is.nan(depreciation_amortization_paths)] = 0
        depreciation_amortization_paths
    })
    
    
    output$line_plot_depreciation_amortization <- renderPlotly({
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        depreciation_amortization_av = rep(0,quartersCount)
        depreciation_amortization_avg = rep(0,quartersCount)
        depreciation_amortization_minv = rep(0,quartersCount)
        depreciation_amortization_maxv = rep(0,quartersCount)
        
        for (d in 1:quartersCount ) {
            depreciation_amortization_av[d]=median(depreciation_amortization_paths()[,d])
            depreciation_amortization_avg[d]=mean(depreciation_amortization_paths()[,d])
            depreciation_amortization_minv[d]=min(depreciation_amortization_paths()[,d])
            depreciation_amortization_maxv[d]=max(depreciation_amortization_paths()[,d])
        }
        
        x_data = factor(c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #X axis data ticks as factor
        x_data = factor(x_data, levels =c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #Leveling X axis data ticks to depreciation_amortization_avoid sorting
        x_axis = list(title="Quarter", fond=list(size=12)) #Attributes for X axis label
        y_axis = list(title="Costs per quarter [€]", fond=list(size=12)) #Attributes for Y axis label
        
        line_plot_depreciation_amortization = plot_ly()
        line_plot_depreciation_amortization = plot_ly(x = ~x_data, y = ~depreciation_amortization_avg, type="scatter", mode="lines", name="Base Case Scenario", line=list(color='rgb(0,0,255)', width=2))
        line_plot_depreciation_amortization = line_plot_depreciation_amortization %>% add_trace(y= ~depreciation_amortization_minv, mode="lines", name="Best Case Scenario", line=list(color='rgb(0,255,0)', width=2))
        line_plot_depreciation_amortization = line_plot_depreciation_amortization %>% add_trace(y= ~depreciation_amortization_maxv, mode="lines", name="Worst Case Scenario", line=list(color='rgb(255,0,0)', width=2))
        line_plot_depreciation_amortization = line_plot_depreciation_amortization %>% layout(title="5 Year Cost Simulation \n of  Depreciation & Amortization", legend = list(orientation = 'h', font=list(size=6)), xaxis=x_axis, yaxis=y_axis)
        
        #Plots a specific amount of simulations based on the modulo operation 
        for (i in 1:numberIterations) {
            #if (i %% 5 == 0) {
            line_plot_depreciation_amortization = line_plot_depreciation_amortization %>% add_trace(y= depreciation_amortization_paths()[i,], mode="lines", name= i, line=list(color='rgb(140,140,140)', width=0.5), showlegend=FALSE)
            #}
        } 
        line_plot_depreciation_amortization
    })
    
    output$table_depreciation_amortization = renderPlotly({
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        depreciation_amortization_av = rep(0,quartersCount)
        depreciation_amortization_avg = rep(0,quartersCount)
        depreciation_amortization_minv = rep(0,quartersCount)
        depreciation_amortization_maxv = rep(0,quartersCount)
        depreciation_amortization_q10 = rep(0,quartersCount)
        depreciation_amortization_q25 = rep(0,quartersCount)
        depreciation_amortization_q75 = rep(0,quartersCount)
        depreciation_amortization_q90 = rep(0,quartersCount)
        
        
        for (d in 1:quartersCount ) {
            depreciation_amortization_av[d]=median(depreciation_amortization_paths()[,d])
            depreciation_amortization_avg[d]=mean(depreciation_amortization_paths()[,d])
            depreciation_amortization_minv[d]=min(depreciation_amortization_paths()[,d])
            depreciation_amortization_maxv[d]=max(depreciation_amortization_paths()[,d])
            depreciation_amortization_q10[d]=quantile(depreciation_amortization_paths()[,d], probs=0.1, na.rm = TRUE)
            depreciation_amortization_q25[d]=quantile(depreciation_amortization_paths()[,d], probs=0.25, na.rm = TRUE)
            depreciation_amortization_q75[d]=quantile(depreciation_amortization_paths()[,d], probs=0.75, na.rm = TRUE)
            depreciation_amortization_q90[d]=quantile(depreciation_amortization_paths()[,d], probs=0.90, na.rm = TRUE)
        }
        
        x_data = factor(c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #X axis data ticks as factor
        x_data = factor(x_data, levels =c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #Leveling X axis data ticks to depreciation_amortization_avoid sorting
        x_axis = list(title="Quarter", fond=list(size=12)) #Attributes for X axis label
        y_axis = list(title="Costs per quarter [€]", fond=list(size=12))
        
        table_depreciation_amortization = data.frame(depreciation_amortization_minv,depreciation_amortization_avg,depreciation_amortization_maxv,depreciation_amortization_q10, depreciation_amortization_q25, depreciation_amortization_q75, depreciation_amortization_q90, row.names=x_data)
        names(table_depreciation_amortization) = c("Best Case Scenario", "Base Case Scenario", "Worst Case Scenario", "10th Quantile", "25th Quantile", "75th Quantile", "90th Quantile")
        table_depreciation_amortization = plot_ly(type='table', header = list(values = c("<b>Quarter</b>", names(table_depreciation_amortization)), align = c('left', rep('center', ncol(table_depreciation_amortization))), line = list(width = 1, color = 'black'),
                                                                        fill = list(color = 'rgb(39, 43, 48)'), font = list(family = "Arial", size = 14, color = "white")),
                                            cells = list(values = rbind(rownames(table_depreciation_amortization), t(as.matrix(unname(round(table_depreciation_amortization, digits=0))))),
                                                         align = c('left', rep('center', ncol(table_depreciation_amortization))), line = list(color = "black", width = 1), fill = list(color = c('rgb(39, 43, 48)', 'rgba(39, 43, 48, 0.65)')), font = list(family = "Arial", size = 12, color = c("white","black"))))
        
        
        
    })
    
    #Simulate Interest Costs
    interests_cost_data = reactive({
        if (input$interests_cost_distribution == "PERT Beta") {
            interests_cost_data = rpert(10000, min=input$interests_cost_min, mode=input$interests_cost_base, max=input$interests_cost_max, shape=1)
        } else if (input$interests_cost_distribution == "Normal") {
            interests_cost_data = rnorm(1000, mean=mean(c(input$interests_cost_min, input$interests_cost_base, input$interests_cost_max)), sd=sd(c(input$interests_cost_min, input$interests_cost_base, input$interests_cost_max)))
        } else {
            interests_cost_data = rtriang(1000, min=input$interests_cost_min, mode=input$interests_cost_base, max=input$interests_cost_max)
        }
    })
    
    interests_cost_paths = reactive({
        interests_cost_vol = sd(interests_cost_data()) / mean(interests_cost_data()) #interests_cost_volatility in percent
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        interests_cost_t1 = sample((interests_cost_data()[interests_cost_data() >= quantile(interests_cost_data(), 0.35) & interests_cost_data() <= quantile(interests_cost_data(), 0.65)]), 1)
        interests_cost_paths = matrix(interests_cost_t1, numberIterations, quartersCount)
        for (d in 1:quartersCount ) {
            for (i in 1:numberIterations ) {
                if (d>1) 
                    interests_cost_paths[i,d] = interests_cost_paths[i,d-1] * (1 + drift()/quartersCount+ (runif(1, 0, 1) - 0.5) * interests_cost_vol * sqrt(1/quartersCount))
            }
        }
        interests_cost_paths[is.nan(interests_cost_paths)] = 0
        interests_cost_paths
    })
    
    
    output$line_plot_interests_cost <- renderPlotly({
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        interests_cost_av = rep(0,quartersCount)
        interests_cost_avg = rep(0,quartersCount)
        interests_cost_minv = rep(0,quartersCount)
        interests_cost_maxv = rep(0,quartersCount)
        
        for (d in 1:quartersCount ) {
            interests_cost_av[d]=median(interests_cost_paths()[,d])
            interests_cost_avg[d]=mean(interests_cost_paths()[,d])
            interests_cost_minv[d]=min(interests_cost_paths()[,d])
            interests_cost_maxv[d]=max(interests_cost_paths()[,d])
        }
        
        x_data = factor(c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #X axis data ticks as factor
        x_data = factor(x_data, levels =c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #Leveling X axis data ticks to interests_cost_avoid sorting
        x_axis = list(title="Quarter", fond=list(size=12)) #Attributes for X axis label
        y_axis = list(title="Costs per quarter [€]", fond=list(size=12)) #Attributes for Y axis label
        
        line_plot_interests_cost = plot_ly()
        line_plot_interests_cost = plot_ly(x = ~x_data, y = ~interests_cost_avg, type="scatter", mode="lines", name="Base Case Scenario", line=list(color='rgb(0,0,255)', width=2))
        line_plot_interests_cost = line_plot_interests_cost %>% add_trace(y= ~interests_cost_minv, mode="lines", name="Best Case Scenario", line=list(color='rgb(0,255,0)', width=2))
        line_plot_interests_cost = line_plot_interests_cost %>% add_trace(y= ~interests_cost_maxv, mode="lines", name="Worst Case Scenario", line=list(color='rgb(255,0,0)', width=2))
        line_plot_interests_cost = line_plot_interests_cost %>% layout(title="5 Year Cost Simulation \n of  Interest Costs", legend = list(orientation = 'h', font=list(size=6)), xaxis=x_axis, yaxis=y_axis)
        
        #Plots a specific amount of simulations based on the modulo operation 
        for (i in 1:numberIterations) {
            #if (i %% 5 == 0) {
            line_plot_interests_cost = line_plot_interests_cost %>% add_trace(y= interests_cost_paths()[i,], mode="lines", name= i, line=list(color='rgb(140,140,140)', width=0.5), showlegend=FALSE)
            #}
        } 
        line_plot_interests_cost
    })
    
    output$table_interests_cost = renderPlotly({
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        interests_cost_av = rep(0,quartersCount)
        interests_cost_avg = rep(0,quartersCount)
        interests_cost_minv = rep(0,quartersCount)
        interests_cost_maxv = rep(0,quartersCount)
        interests_cost_q10 = rep(0,quartersCount)
        interests_cost_q25 = rep(0,quartersCount)
        interests_cost_q75 = rep(0,quartersCount)
        interests_cost_q90 = rep(0,quartersCount)
        
        
        for (d in 1:quartersCount ) {
            interests_cost_av[d]=median(interests_cost_paths()[,d])
            interests_cost_avg[d]=mean(interests_cost_paths()[,d])
            interests_cost_minv[d]=min(interests_cost_paths()[,d])
            interests_cost_maxv[d]=max(interests_cost_paths()[,d])
            interests_cost_q10[d]=quantile(interests_cost_paths()[,d], probs=0.1, na.rm = TRUE)
            interests_cost_q25[d]=quantile(interests_cost_paths()[,d], probs=0.25, na.rm = TRUE)
            interests_cost_q75[d]=quantile(interests_cost_paths()[,d], probs=0.75, na.rm = TRUE)
            interests_cost_q90[d]=quantile(interests_cost_paths()[,d], probs=0.90, na.rm = TRUE)
        }
        
        x_data = factor(c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #X axis data ticks as factor
        x_data = factor(x_data, levels =c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #Leveling X axis data ticks to interests_cost_avoid sorting
        x_axis = list(title="Quarter", fond=list(size=12)) #Attributes for X axis label
        y_axis = list(title="Costs per quarter [€]", fond=list(size=12))
        
        table_interests_cost = data.frame(interests_cost_minv,interests_cost_avg,interests_cost_maxv,interests_cost_q10, interests_cost_q25, interests_cost_q75, interests_cost_q90, row.names=x_data)
        names(table_interests_cost) = c("Best Case Scenario", "Base Case Scenario", "Worst Case Scenario", "10th Quantile", "25th Quantile", "75th Quantile", "90th Quantile")
        table_interests_cost = plot_ly(type='table', header = list(values = c("<b>Quarter</b>", names(table_interests_cost)), align = c('left', rep('center', ncol(table_interests_cost))), line = list(width = 1, color = 'black'),
                                                                              fill = list(color = 'rgb(39, 43, 48)'), font = list(family = "Arial", size = 14, color = "white")),
                                                  cells = list(values = rbind(rownames(table_interests_cost), t(as.matrix(unname(round(table_interests_cost, digits=0))))),
                                                               align = c('left', rep('center', ncol(table_interests_cost))), line = list(color = "black", width = 1), fill = list(color = c('rgb(39, 43, 48)', 'rgba(39, 43, 48, 0.65)')), font = list(family = "Arial", size = 12, color = c("white","black"))))
        
        
        
    })
    
    #Aggregate Total Costs Paths
    total_cost_paths = reactive({material_cost_paths() + personnel_cost_paths() +  otheroperating_cost_paths() + depreciation_amortization_paths() + interests_cost_paths()})
    
    output$line_plot_total_cost <- renderPlotly({
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        total_cost_av = rep(0,quartersCount)
        total_cost_avg = rep(0,quartersCount)
        total_cost_minv = rep(0,quartersCount)
        total_cost_maxv = rep(0,quartersCount)
        
        for (d in 1:quartersCount ) {
            total_cost_av[d]=median(total_cost_paths()[,d])
            total_cost_avg[d]=mean(total_cost_paths()[,d])
            total_cost_minv[d]=min(total_cost_paths()[,d])
            total_cost_maxv[d]=max(total_cost_paths()[,d])
        }
        
        x_data = factor(c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #X axis data ticks as factor
        x_data = factor(x_data, levels =c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #Leveling X axis data ticks to total_cost_avoid sorting
        x_axis = list(title="Quarter", fond=list(size=12)) #Attributes for X axis label
        y_axis = list(title="Costs per quarter [€]", fond=list(size=12)) #Attributes for Y axis label
        
        line_plot_total_cost = plot_ly()
        line_plot_total_cost = plot_ly(x = ~x_data, y = ~total_cost_avg, type="scatter", mode="lines", name="Base Case Scenario", line=list(color='rgb(0,0,255)', width=2))
        line_plot_total_cost = line_plot_total_cost %>% add_trace(y= ~total_cost_minv, mode="lines", name="Best Case Scenario", line=list(color='rgb(0,255,0)', width=2))
        line_plot_total_cost = line_plot_total_cost %>% add_trace(y= ~total_cost_maxv, mode="lines", name="Worst Case Scenario", line=list(color='rgb(255,0,0)', width=2))
        line_plot_total_cost = line_plot_total_cost %>% layout(title="5 Year Cost Simulation \n of  Total Costs", legend = list(orientation = 'h', font=list(size=6)), xaxis=x_axis, yaxis=y_axis)
        
        #Plots a specific amount of simulations based on the modulo operation 
        for (i in 1:numberIterations) {
            #if (i %% 5 == 0) {
            line_plot_total_cost = line_plot_total_cost %>% add_trace(y= total_cost_paths()[i,], mode="lines", name= i, line=list(color='rgb(140,140,140)', width=0.5), showlegend=FALSE)
            #}
        } 
        line_plot_total_cost
    })
    
    output$table_total_cost = renderPlotly({
        quartersCount = 20 #Number of quarters which will be
        numberIterations = 400 #Number of iterations for each quarter
        total_cost_av = rep(0,quartersCount)
        total_cost_avg = rep(0,quartersCount)
        total_cost_minv = rep(0,quartersCount)
        total_cost_maxv = rep(0,quartersCount)
        total_cost_q10 = rep(0,quartersCount)
        total_cost_q25 = rep(0,quartersCount)
        total_cost_q75 = rep(0,quartersCount)
        total_cost_q90 = rep(0,quartersCount)
        
        
        for (d in 1:quartersCount ) {
            total_cost_av[d]=median(total_cost_paths()[,d])
            total_cost_avg[d]=mean(total_cost_paths()[,d])
            total_cost_minv[d]=min(total_cost_paths()[,d])
            total_cost_maxv[d]=max(total_cost_paths()[,d])
            total_cost_q10[d]=quantile(total_cost_paths()[,d], probs=0.1, na.rm = TRUE)
            total_cost_q25[d]=quantile(total_cost_paths()[,d], probs=0.25, na.rm = TRUE)
            total_cost_q75[d]=quantile(total_cost_paths()[,d], probs=0.75, na.rm = TRUE)
            total_cost_q90[d]=quantile(total_cost_paths()[,d], probs=0.90, na.rm = TRUE)
        }
        
        x_data = factor(c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #X axis data ticks as factor
        x_data = factor(x_data, levels =c("Q1.20XX", "Q2.20XX", "Q3.20XX", "Q4.20XX", "Q1.20XX+1", "Q2.20XX+1", "Q3.20XX+1", "Q4.20XX+1", "Q1.20XX+2", "Q2.20XX+2", "Q3.20XX+2", "Q4.20XX+2", "Q1.20XX+3", "Q2.20XX+3", "Q3.20XX+3", "Q4.20XX+3",  "Q1.20XX+4", "Q2.20XX+4", "Q3.20XX+4", "Q4.20XX+4")) #Leveling X axis data ticks to total_cost_avoid sorting
        x_axis = list(title="Quarter", fond=list(size=12)) #Attributes for X axis label
        y_axis = list(title="Costs per quarter [€]", fond=list(size=12))
        
        table_total_cost = data.frame(total_cost_minv,total_cost_avg,total_cost_maxv,total_cost_q10, total_cost_q25, total_cost_q75, total_cost_q90, row.names=x_data)
        names(table_total_cost) = c("Best Case Scenario", "Base Case Scenario", "Worst Case Scenario", "10th Quantile", "25th Quantile", "75th Quantile", "90th Quantile")
        table_total_cost = plot_ly(type='table', header = list(values = c("<b>Quarter</b>", names(table_total_cost)), align = c('left', rep('center', ncol(table_total_cost))), line = list(width = 1, color = 'black'),
                                                                   fill = list(color = 'rgb(39, 43, 48)'), font = list(family = "Arial", size = 14, color = "white")),
                                       cells = list(values = rbind(rownames(table_total_cost), t(as.matrix(unname(round(table_total_cost, digits=0))))),
                                                    align = c('left', rep('center', ncol(table_total_cost))), line = list(color = "black", width = 1), fill = list(color = c('rgb(39, 43, 48)', 'rgba(39, 43, 48, 0.65)')), font = list(family = "Arial", size = 12, color = c("white","black"))))
        
        
        
    })

})
