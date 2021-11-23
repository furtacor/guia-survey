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

variaveis <- setdiff(names(df), c("ID", "Sex"))


# Início Interface do Usuário -----


ui <- dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            fluidPage(
                pickerInput("uni_seletor",
                            label = "Selecione uma variável:",
                            choices = variaveis)
            )
        ),
        fluidRow(
            box(title = "Feminino",
                height = "100%",
                highchartOutput("uni_grafico1")),
            box(title = "Masculino",
                height = "100%",
                highchartOutput("uni_grafico2"))
        ),
        
        br(),
        
        
# Cruzamento -----
        
        
        fluidRow(
            column(6,
                   pickerInput("bi_seletor1",
                               label = "Selecione a primeira variável:",
                               choices = variaveis)
            ),
            column(6,pickerInput("bi_seletor2",
                                 label = "Selecione a segunda variável:",
                                 choices = variaveis))
        ),
        fluidRow(
            box(title = "Feminino",
                height = "100%",
                highchartOutput("bi_grafico1")),
            box(title = "Masculino",
                height = "100%",
                highchartOutput("bi_grafico2"))
        )
    )
)

server <- function(input, output, session) {
    
    
# Gráfico 1 -----
    
    
    uni_filtro1 <- reactive({
        df %>% 
            filter(Sex == "Female") %>% 
            select(input$uni_seletor)
    })
    
    data_uni_grafico1 <- reactive({
        as.data.frame(table(uni_filtro1())) %>%
            setNames(c("variavel", "frequencia")) %>%
            mutate(frequencia_porcentagem = round(frequencia/sum(frequencia)*100, digits = 1)) %>% 
            mutate(frequencia_acumulada = round(cumsum(frequencia_porcentagem), digits = 0))
    })
    
    output$uni_grafico1 <- renderHighchart({
        highchart() %>% 
            hc_yAxis_multiples(list(title = list(text = "Frequência %"),
                                    opposite = FALSE,
                                    labels = list(format = '{value}%')),
                               list(title = list(text = "Frequência acumulada %"),
                                    opposite = TRUE,
                                    max = 100,
                                    labels = list(format = '{value}%'))) %>% 
            hc_xAxis(list(categories = data_uni_grafico1()$variavel)) %>% 
            hc_add_series(data_uni_grafico1(),
                          "column",
                          name = "Porcentagem",
                          yAxis = 0,
                          hcaes(x = variavel, y = frequencia_porcentagem),
                          tooltip = list(valueSuffix = '%')) %>% 
            hc_add_series(data_uni_grafico1(),
                          "spline",
                          name = "Porcentagem acumulada",
                          yAxis = 1,
                          hcaes(x = variavel, y = frequencia_acumulada),
                          tooltip = list(valueSuffix = '%')) %>% 
            hc_plotOptions(column = list(dataLabels = list(enabled = TRUE)))
    })
    
    
# Gráfico 2 -----
    
    
    uni_filtro2 <- reactive({
        df %>% 
            filter(Sex == "Male") %>% 
            select(input$uni_seletor)
    })
    
    data_uni_grafico2 <- reactive({
        as.data.frame(table(uni_filtro2())) %>%
            setNames(c("variavel", "frequencia")) %>%
            mutate(frequencia_porcentagem = round(frequencia/sum(frequencia)*100, digits = 1)) %>% 
            mutate(frequencia_acumulada = round(cumsum(frequencia_porcentagem), digits = 0))
    })
    
    output$uni_grafico2 <- renderHighchart({
        highchart() %>% 
            hc_yAxis_multiples(list(title = list(text = "Frequência %"),
                                    opposite = FALSE,
                                    labels = list(format = '{value}%')),
                               list(title = list(text = "Frequência acumulada %"),
                                    opposite = TRUE,
                                    max = 100,
                                    labels = list(format = '{value}%'))) %>% 
            hc_xAxis(list(categories = data_uni_grafico2()$variavel)) %>% 
            hc_add_series(data_uni_grafico2(),
                          "column",
                          name = "Porcentagem",
                          yAxis = 0,
                          hcaes(x = variavel, y = frequencia_porcentagem),
                          tooltip = list(valueSuffix = '%')) %>% 
            hc_add_series(data_uni_grafico2(),
                          "spline",
                          name = "Porcentagem acumulada",
                          yAxis = 1,
                          hcaes(x = variavel, y = frequencia_acumulada),
                          tooltip = list(valueSuffix = '%')) %>% 
            hc_plotOptions(column = list(dataLabels = list(enabled = TRUE)))
    })
    
    
# Gráfico Cruzamento 1 -----
    
    
    data_choices <- reactive({
        setdiff(variaveis, input$bi_seletor1)
    })
    
    observeEvent(input$bi_seletor1, {
        updatePickerInput(session = session,
                          inputId = "bi_seletor2",
                          choices = data_choices())
    })
    
    filtro_bi_grafico1 <- reactive({
        df %>% 
            filter(Sex == "Female") %>% 
            select(input$bi_seletor1, input$bi_seletor2)
    })
    
    data_bi_grafico1 <- reactive({
        as.data.frame(table(filtro_bi_grafico1())) %>% 
            setNames(c("Var1", "Var2", "Freq")) %>% 
            group_by(Var1) %>% 
            mutate(Porcentagem = round(Freq/sum(Freq)*100, digits = 1))
    })
    
    output$bi_grafico1 <- renderHighchart({
        highchart() %>% 
            hc_plotOptions(column = list(stacking = "percent",
                                         dataLabels = list(enabled = TRUE))) %>% 
            hc_yAxis(labels = list(format = '{value}%')) %>% 
            hc_xAxis(list(categories = data_bi_grafico1()$Var1)) %>% 
            hc_add_series(data_bi_grafico1(),
                          "column",
                          hcaes(x = Var1, y = Porcentagem, group = Var2),
                          tooltip = list(valueSuffix = '%',
                                         pointFormat = "Porcentagem: {point.Porcentagem}%<br> Frequência: {point.Freq}"))
    })
    
    
# Gráfico Cruzamento 2 -----
    
    
    filtro_bi_grafico2 <- reactive({
        df %>% 
            filter(Sex == "Male") %>% 
            select(input$bi_seletor1, input$bi_seletor2)
    })
    
    data_bi_grafico2 <- reactive({
        as.data.frame(table(filtro_bi_grafico2())) %>% 
            setNames(c("Var1", "Var2", "Freq")) %>% 
            group_by(Var1) %>% 
            mutate(Porcentagem = round(Freq/sum(Freq)*100, digits = 1))
    })
    
    output$bi_grafico2 <- renderHighchart({
        highchart() %>% 
            hc_plotOptions(column = list(stacking = "percent",
                                         dataLabels = list(enabled = TRUE))) %>% 
            hc_yAxis(labels = list(format = '{value}%')) %>% 
            hc_xAxis(list(categories = data_bi_grafico2()$Var1)) %>% 
            hc_add_series(data_bi_grafico2(),
                          "column",
                          hcaes(x = Var1, y = Porcentagem, group = Var2),
                          tooltip = list(valueSuffix = '%',
                                         pointFormat = "Porcentagem: {point.Porcentagem}%<br> Frequência: {point.Freq}")) 
    })
    
}

shinyApp(ui = ui, server = server)

