##############################################################################################################################################
library('shiny')
source('compare_user_vs_ml_fcst.R')


ui <- fluidPage(
  
  # Application title
  titlePanel("Descriptive Statistics of Automobiles"),
  sidebarLayout(
    sidebarPanel(
      # 단위 선택
      selectInput("Unit", "Unit",
                  c(Total = "total", Model = "model")),
      
      #전체 단위
      conditionalPanel(
        condition = "input.Unit == 'total'",
        selectInput("T_account", "Select account", 
                    choice = c("DIXONS", 
                               "JOHNLEWIS"),
                    selectize = F),
        
        selectInput("T_product", "Select product", 
                    choice = c("LTV", 
                               "WM",
                               "REF"),
                    selectize = F)),
      
      #모델 단위 
      conditionalPanel(
        condition = 'input.Unit == "model"',
        selectInput("M_account", "Select account", 
                    choice = c("DIXONS", 
                               "JOHNLEWIS"),
                    selectize = F),
        
        selectInput("M_product", "Select product", 
                    choice = c("LTV", 
                               "WM",
                               "REF"),
                    selectize = F),
        selectInput("M_model", "Select model",
                    choices = c(),
                    selectize = F)
      ),
      width = 3
    ),
    
    
    mainPanel(
      conditionalPanel(
        condition = "input.Unit == 'total'",
        h4("Summary Statistics of a variable"),
        plotlyOutput('fcst_plot'),
        DT::dataTableOutput('T_gap_table')),
      
      conditionalPanel(
        condition = "input.Unit == 'model'",
        h4("Summary Statistics of a variable"),
        plotlyOutput('fcst_plot'),
        DT::dataTableOutput('M_gap_table'))
    )
  )
)


# Define server logic required to calculate summary statistics
server <- function(input, output, session) {
  
  observe({
    x <- kam_fcst_melt[account_name == input$M_account & product == input$M_product & Measure == 'USER_FCST', sum(value, na.rm = T)
                       , by=.(model_suffix_code)][order(-V1)][,model_suffix_code]
    
    # Can also set the label and select items
    updateSelectInput(session, "M_model",
                      label = "Select model",
                      choices = x,
                      selected = head(x, 1)
    )
  })
  
  
  output$T_fcst_plot <- renderPlotly({
    gg <- ggplot() + geom_line(data = fcst_sum[account_name == input$T_account & product == input$T_product], aes(x=target_week_date, y=V1, group = Measure, col = Measure)) +
      geom_line(data = gap_by_account[account_name == input$T_account & product == input$T_product], aes(x=target_week_date, y=Gap_sum, colour = 'Gap')) +
      theme_bw() + labs(x = 'target_week', y = 'fcst_sum',title = str_interp('${input$T_account} ${input$T_product}')) +
      scale_x_date(date_labels = '%V',date_breaks = '1 week')
    
    print(ggplotly(gg))
  })
  
  
  output$T_gap_table <- DT::renderDataTable({
    gap_by_account <- kam_fcst_dcast[account_name == input$T_account & product == input$T_product,
                                     .('ML_FCST_sum' = sum(ML_FCST, na.rm = T),'USER_FCST_sum' = sum(USER_FCST, na.rm = T),'Gap_sum' = sum(abs_gap, na.rm = T)), 
                                     by=.(account_name, product, target_week_date, weeknum)]
    gap_by_account
  })
  
  
  
  output$M_fcst_plot <- renderPlotly({
    gg <- ggplot() + geom_line(data = kam_fcst_melt[account_name == input$M_account & product == input$M_product & model_suffix_code == input$M_model], aes(x=target_week_date, y=value, group = Measure, col = Measure)) +
      geom_line(data = kam_fcst_dcast[account_name == input$M_account & product == input$M_product & model_suffix_code == input$M_model], aes(x=target_week_date, y=abs_gap, colour = 'Gap')) +
      theme_bw() + labs(x = 'target_week', y = 'fcst_sum',title = str_interp('${input$M_account} ${input$M_product} / MODEL : ${input$M_model}')) +
      scale_x_date(date_labels = '%V',date_breaks = '1 week')
    
    print(ggplotly(gg))
  })
  
  
  output$M_gap_table <- DT::renderDataTable({
    gap_by_account <- kam_fcst_dcast[account_name == input$M_account & product == input$M_product & model_suffix_code == input$M_model,
                                     .('ML_FCST_sum' = sum(ML_FCST, na.rm = T),'USER_FCST_sum' = sum(USER_FCST, na.rm = T),'Gap_sum' = sum(abs_gap, na.rm = T)), 
                                     by=.(account_name, product, target_week_date, weeknum)]
    gap_by_account
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
