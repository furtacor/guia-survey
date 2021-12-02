library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(highcharter)
library(dplyr)


df <- data.frame(vcd::Arthritis) %>% 
    mutate(Age = cut(Age,
                     breaks = c(20, 40, 60, 80),
                     right = FALSE,
                     labels = c("Entre 20 e 40", "Entre 40 e 60", "Entre 60 e 80")))

variaveis <- setdiff(names(df), "ID")


ui <- dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            
            box(pickerInput(inputId = "seletor_univariado",
                            label = "Selecione uma variável:",
                            choices = variaveis),
                
                highchartOutput(outputId = "grafico_univariado")),
            
            
            box(pickerInput(inputId = "seletor_var1",
                            label = "Selecione a primeira variável:",
                            choices = variaveis),
                
                pickerInput(inputId = "seletor_var2",
                            label = "Selecione a segunda variável:",
                            choices = variaveis),
                
                highchartOutput(outputId = "grafico_bivariado"))
            
            )
        )
    )

server <- function(input, output, session) {
    
    
# Gráfico univariado -----
    
    
    filtro_univariado <- reactive({
        df %>% 
            select(input$seletor_univariado)
    })
    
    data_grafico_univariado <- reactive({
        as.data.frame(table(filtro_univariado())) %>%
            setNames(c("variavel", "frequencia"))
    })
    
    output$grafico_univariado <- renderHighchart({
        highchart() %>% 
            hc_xAxis(list(categories = data_grafico_univariado()$variavel)) %>% 
            hc_add_series(data = data_grafico_univariado(),
                          name = "Frequência",
                          type = "column",
                          hcaes(x = variavel, y = frequencia))
    })
        
    
# Gráfico bivariado -----

    
    data_choices <- reactive({
        setdiff(variaveis, input$seletor_var1)
    })
    
    observeEvent(input$seletor_var1, {
        updatePickerInput(session = session,
                          inputId = "seletor_var2",
                          choices = data_choices())
    })

    filtro_bivariado <- reactive({
        df %>% 
            select(input$seletor_var1, input$seletor_var2)
    })
    
    data_grafico_bivariado <- reactive({
        as.data.frame(table(filtro_bivariado())) %>% 
            setNames(c("Var1", "Var2", "Freq")) %>% 
            group_by(Var1) %>% 
            mutate(Porcentagem = round(Freq/sum(Freq)*100,
                                       digits = 1))
    })
    
    output$grafico_bivariado <- renderHighchart({
        highchart() %>% 
            hc_plotOptions(column = list(stacking = "percent",
                                         dataLabels = list(enabled = TRUE))) %>% 
            hc_yAxis(labels = list(format = '{value}%')) %>% 
            hc_xAxis(list(categories = data_grafico_bivariado()$Var1)) %>% 
            hc_add_series(data_grafico_bivariado(),
                          "column",
                          hcaes(x = Var1, y = Porcentagem, group = Var2),
                          tooltip = list(valueSuffix = '%',
                                         pointFormat = "Frequência: {point.Freq}"))
    })

    
}

shinyApp(ui, server)
