#Those codes are from https://towardsdatascience.com/how-to-build-a-data-analysis-app-in-r-shiny-143bee9338f7
#I modified them and fit my work. I'm Greatly inspired by this author. 

library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)

not_sel <- "Not Selected"

about_page <- tabPanel(
  title = "About",
  titlePanel("About"),
  "The original codes are from https://towardsdatascience.com/how-to-build-a-data-analysis-app-in-r-shiny-143bee9338f7.",
  br(),
  "This is a class project from KUMC Data Visualization Class: Fall 2022. I'm Philip Yeung, PharmD, candidate of MS Health Data Science program."
)

main_page <- tabPanel(
  title = "Summary Statistics",
  titlePanel("Summary Statistics and Plots"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
      selectInput("num_var_1", "Numerical Variable 1", choices = c(not_sel)),
      selectInput("num_var_2", "Numerical Variable 2", choices = c(not_sel)),
      selectInput("num_var_3", "Numerical Variable 3", choices = c(not_sel)),
      selectInput("num_var_4", "Numerical Variable 4", choices = c(not_sel)),
      selectInput("fact_var", "Factor Variable", choices = c(not_sel)),
      br(),
      actionButton("run_button", "Run Summary Statistics", icon = icon("play"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Plot",
          plotOutput("plot_1")
        ),
        tabPanel(
          title = "Summary Statistics",
          fluidRow(
            column(width = 2, strong(textOutput("num_var_1_title"))),
            column(width = 2, strong(textOutput("num_var_2_title"))),
            column(width = 2, strong(textOutput("num_var_3_title"))),
            column(width = 2, strong(textOutput("num_var_4_title"))),
            column(width = 2, strong(textOutput("fact_var_title")))
          ),
          fluidRow(
            column(width = 2, tableOutput("num_var_1_summary_table")),
            column(width = 2, tableOutput("num_var_2_summary_table")),
            column(width = 2, tableOutput("num_var_3_summary_table")),
            column(width = 2, tableOutput("num_var_4_summary_table")),
            column(width = 2, tableOutput("fact_var_summary_table"))
          ),
          fluidRow(
            column(width = 4, strong("Combined Summary Statistics"))
          ),
          fluidRow(
            column(width = 4, tableOutput("Combined_Summary_Table"))
          )
          
        )
      )
    )
  )
)

draw_plot_1 <- function(data_input, num_var_1, num_var_2, fact_var){
  if(fact_var!=not_sel){
    data_input[,(fact_var):= as.factor(data_input[,get(fact_var)])]
  }
  if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2, color = fact_var, shape = fact_var )) +
      geom_point()+
      geom_smooth(method = "lm", fill = NA)
  }
  else if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2)) +
      geom_point()
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_1)) +
      geom_violin()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_2)) +
      geom_violin()
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1)) +
      geom_histogram()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_2)) +
      geom_histogram()
  }
  else if(num_var_1 == not_sel & num_var_2 == not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var)) +
      geom_bar()
  }
}

draw_plot_2 <- function(data_input, num_var_3, num_var_4, fact_var){
  if(fact_var!=not_sel){
    data_input[,(fact_var):= as.factor(data_input[,get(fact_var)])]
  }
  if(num_var_3 != not_sel & num_var_4 != not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_3, y = num_var_4, color = fact_var, shape = fact_var )) +
      geom_point()+
      geom_smooth(method = "lm", fill = NA)
  }
  else if(num_var_3 != not_sel & num_var_4 != not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_3, y = num_var_4)) +
      geom_point()
  }
  else if(num_var_3 != not_sel & num_var_4 == not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_1)) +
      geom_violin()
  }
  else if(num_var_3 == not_sel & num_var_4 != not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_4)) +
      geom_violin()
  }
  else if(num_var_3 != not_sel & num_var_4 == not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_3)) +
      geom_histogram()
  }
  else if(num_var_3 == not_sel & num_var_4 != not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_3)) +
      geom_histogram()
  }
  else if(num_var_3 == not_sel & num_var_4 == not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var)) +
      geom_bar()
  }
}




create_num_var_table <- function(data_input, num_var){
  if(num_var != not_sel){
    col <- data_input[,get(num_var)]
    if (length(col)>5000) col_norm <- sample(col,5000) else col_norm <- col
    norm_test <- shapiro.test(col_norm)
    statistic <- c("mean", "median", "5th percentile", "95th percentile",
                   "Shapiro statistic", "Shapiro p-value")
    value <- c(round(mean(col),2), round(median(col),2),
               round(quantile(col, 0.05),2), round(quantile(col, 0.95),2),
               norm_test$statistic, norm_test$p.value)
    data.table(statistic, value)
  }
}

create_fact_var_table <- function(data_input, fact_var){
  if(fact_var != not_sel){
    freq_tbl <- data_input[,.N, by = get(fact_var)]
    freq_tbl <- setnames(freq_tbl,c("factor_value", "count"))
    freq_tbl
  }
}

create_combined_table <- function(data_input, num_var_1, num_var_2, num_var_3, num_var_4, fact_var){
  if(fact_var != not_sel){
    if(num_var_1 != not_sel & num_var_2 & num_var_3 & num_var_4 != not_sel){
      res_tbl <- data_input[,.(correlation = cor(get(num_var_1), get(num_var_2), get(num_var_3), get(num_var_4))), by = fact_var]
    }
    else if(num_var_1 != not_sel & num_var_2 & num_var_3 & num_var_4  == not_sel){
      res_tbl <- data_input[,.(mean = mean(get(num_var_1))), by = fact_var]
    }
    else if(num_var_1 == not_sel & num_var_2 & num_var_3 & num_var_4 != not_sel){
      res_tbl <- data_input[,.(mean = mean(get(num_var_2))), by = fact_var]
    }
    else if(num_var_1 != not_sel & num_var_2 & num_var_3 & num_var_4 == not_sel){
      res_tbl <- data_input[,.(mean = mean(get(num_var_3))), by = fact_var]
    }
    else if(num_var_1 == not_sel & num_var_2 & num_var_3 & num_var_4 != not_sel){
      res_tbl <- data_input[,.(mean = mean(get(num_var_4))), by = fact_var]
    }  
  }
  else if(num_var_1 != not_sel & num_var_2 & num_var_3 & num_var_4 != not_sel){
    res_tbl <- data.table(
      statistic = c("correlation"),
      value = c(cor(
        data_input[,get(num_var_1)],
        data_input[,get(num_var_2)], 
        data_input[,get(num_var_3)],
        data_input[,get(num_var_4)]
        )))
  }
  return(res_tbl)
}

ui <- navbarPage(
  title = "Data Analyser",
  theme = shinytheme('cerulean'),
  main_page,
  about_page
)

server <- function(input, output){
  
  options(shiny.maxRequestSize=10*1024^2) 
  
  data_input <- reactive({
    req(input$csv_input)
    fread(input$csv_input$datapath)
  })
  
  observeEvent(data_input(),{
    choices <- c(not_sel,names(data_input()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "num_var_3", choices = choices)
    updateSelectInput(inputId = "num_var_4", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
  })
  
  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  num_var_2 <- eventReactive(input$run_button,input$num_var_2)
  num_var_3 <- eventReactive(input$run_button,input$num_var_3)
  num_var_4 <- eventReactive(input$run_button,input$num_var_4)
  fact_var <- eventReactive(input$run_button,input$fact_var)
  
  # plot
  
  plot_1 <- eventReactive(input$run_button,{
    draw_plot_1(data_input(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$plot_1 <- renderPlot(plot_1())
  
  plot_2 <- eventReactive(input$run_button,{
    draw_plot_2(data_input(), num_var_3(), num_var_4(), fact_var())
  })
  
  output$plot_2 <- renderPlot(plot_2())
  
  # 1-d summary tables
  
  output$num_var_1_title <- renderText(paste("Num Var 1:",num_var_1()))
  
  num_var_1_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data_input(), num_var_1())
  })
  
  output$num_var_1_summary_table <- renderTable(num_var_1_summary_table(),colnames = FALSE)
  
  output$num_var_2_title <- renderText(paste("Num Var 2:",num_var_2()))
  
  num_var_2_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data_input(), num_var_2())
  })
  
  output$num_var_2_summary_table <- renderTable(num_var_2_summary_table(),colnames = FALSE)
  
  output$num_var_3_title <- renderText(paste("Num Var 3:",num_var_3()))
  
  num_var_3_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data_input(), num_var_3())
  })
  
  output$num_var_3_summary_table <- renderTable(num_var_3_summary_table(),colnames = FALSE)
  
  output$num_var_4_title <- renderText(paste("Num Var 4:",num_var_4()))
  
  num_var_4_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data_input(), num_var_4())
  })
  
  output$num_var_4_summary_table <- renderTable(num_var_4_summary_table(),colnames = FALSE)
  
  
  output$fact_var_title <- renderText(paste("Factor Var:",fact_var()))
  
  fact_var_summary_table <- eventReactive(input$run_button,{
    create_fact_var_table(data_input(), fact_var())
  })
  
  output$fact_var_summary_table <- renderTable(fact_var_summary_table(),colnames = FALSE)
  
  # multi-d summary table
  
  combined_summary_table <- eventReactive(input$run_button,{
    create_combined_table(data_input(), num_var_1(), num_var_2(), num_var_3(), num_var_4(),  fact_var())
  })
  
  output$combined_summary_table <- renderTable(combined_summary_table())
  
}

shinyApp(ui = ui, server = server)
