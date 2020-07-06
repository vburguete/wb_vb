## app.R ##
library(tidyverse)
library(plotly)
library(DT)

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinydashboardPlus)


## Inputs
# source('./src/utils.R')


# Choices 
paises <- (poblacion %>% select(country_name) %>% unique() %>% arrange(country_name))$country_name
anios <- (poblacion %>% select(anio) %>% unique() %>% arrange(anio))$anio



# User Interface - UI ####
header <- dashboardHeader(
  title = "Análisis poblacional", 
  titleWidth = 200,
  # Incorpora logo desde la web:
  tags$li(a(href = 'https://www.rafap.com.uy/',
            img(src = "https://www.rafap.com.uy/mvdcms/plantillas/images/logo.svg",
                height="30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown")
  )

sidebar <- dashboardSidebar(
  disable = FALSE,
  width = 200,
  sidebarMenu(
    menuItem("Totales poblacionales", tabName = "totales", icon = icon("line-chart", lib = "font-awesome")),
    menuItem("Proceso demográfico", tabName = "proceso", icon = icon("bar-chart", lib = "font-awesome")),
    menuItem("Pirámides poblacionales", tabName = "piramide", icon = icon("users", lib = "font-awesome")),
    menuItem("Comparativo", tabName = "comparativo", icon = icon("clone", lib = "font-awesome"))
  ))

body <- dashboardBody(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tabItems(
    tabItem(tabName = "totales",
            fluidRow(
              column(2,
                     br(),
                     fluidRow(
                       gradientBox(
                         title = "Seleccionar datos:",
                         boxToolSize = "xs", 
                         closable = FALSE,
                         collapsible = FALSE,
                         width = 12,
                         footer = list(
                           # selectInput('totales_1',
                           #             label = "Seleccione variable:",
                           #             choices = c("Totales",
                           #                         "Zona Residencia",
                           #                         "Género",
                           #                         "Grupo etario"), 
                           #             selected = "Totales",
                           #             multiple = FALSE,
                           #             options = list(`live-search` = TRUE,
                           #                            `none-selected-text` = "Seleccione variable")),

                           pickerInput('totales_2', label = "Seleccione año(s):",
                                       choices = anios, 
                                       selected = c(1960, 2011, 2050),
                                       multiple = TRUE,
                                       options = list(`live-search` = TRUE,
                                                      `none-selected-text` = "Seleccione al menos un año",
                                                      `max-options` = 3,
                                                      `max-options-text` = "Máximo 3 años",
                                                      size = 10))
                           
                         )))
              ),
              
              column(10,
                     br(),
                     fluidRow(
                       uiOutput("nota_totales"),
                       br()
                     ),
                     
                     fluidRow(
                       br(),
                       tabBox(title = "",
                              width = 12,
                              side = 'right',
                              selected = "1",
                              tabPanel(value = "1",
                                       title = tagList(shiny::icon("list", lib = "font-awesome"), ""),
                                       br(),
                                       column(12, 
                                              br(),
                                              DT::dataTableOutput("tabla_totales_1"))
                              ))
                     )
              )
            )
            ),
    tabItem(tabName = "proceso",
            fluidRow(
              column(2,
                     br(),
                     fluidRow(
                       gradientBox(
                         title = "Seleccionar datos:",
                         boxToolSize = "xs", 
                         closable = FALSE,
                         collapsible = FALSE,
                         width = 12,
                         footer = list(
                           pickerInput('proceso_1', label = "Seleccione país:",
                                       choices = paises, 
                                       selected = "Uruguay",
                                       multiple = TRUE,
                                       options = list(`live-search` = TRUE,
                                                      `none-selected-text` = "Seleccione al menos un país",
                                                      `max-options` = 5,
                                                      `max-options-text` = "Máximo 5 países",
                                                      size = 10)),
                           
                           sliderInput("proceso_2", "Seleccione período:",
                                       min = min(anios), max = max(anios), 
                                       value = c(min, 2011),
                                       sep = "")
                         )))
              ),
              
              column(10,
                     br(),
                     fluidRow(
                       uiOutput("nota_proceso"),
                       br()
                     ),
                     
                     fluidRow(
                       br(),
                       tabBox(title = "Proceso demográfico",
                              width = 12,
                              side = 'right',
                              selected = "1",
                              tabPanel(value = "1",
                                       title = tagList(shiny::icon("line-chart", lib = "font-awesome"), ""),
                                       column(12, 
                                              br(),
                                              plotOutput("plot_proceso_1"))
                              ),
                              tabPanel(value = "2",
                                       title = tagList(shiny::icon("bar-chart", lib = "font-awesome"), ""),
                                       column(12, 
                                              br(),
                                              plotOutput("plot_proceso_2"))
                              ),
                              tabPanel(value = "3",
                                       title = tagList(shiny::icon("list", lib = "font-awesome"), ""),
                                       br(),
                                       column(12, 
                                              br(),
                                              DT::dataTableOutput("tabla_proceso_1"))
                              ))
                     )
              )
            )),
    
    tabItem(tabName = "piramide",
            fluidRow(
              column(2,
                     br(),
                     fluidRow(
                       gradientBox(
                         title = "Seleccionar datos:",
                         boxToolSize = "xs", 
                         closable = FALSE,
                         collapsible = FALSE,
                         width = 12,
                         footer = list(
                           pickerInput('piramide_1', label = "Seleccione país:",
                                       choices = paises, 
                                       selected = "Uruguay",
                                       multiple = FALSE,
                                       options = list(`live-search` = TRUE,
                                                      `none-selected-text` = "Seleccione país",
                                                      size = 10)),
                           pickerInput('piramide_2', label = "Seleccione año:",
                                       choices = anios, 
                                       selected = c(1960, 2011, 2050),
                                       multiple = TRUE,
                                       options = list(`live-search` = TRUE,
                                                      `none-selected-text` = "Seleccione al menos un año",
                                                      `max-options` = 3,
                                                      `max-options-text` = "Máximo 3 años",
                                                      size = 10))
                         ))),
                     fluidRow(
                       br(),
                       column(12,
                       dropdown(style = "bordered", status = "primary",
                                tooltip = tooltipOptions(title = "Fuentes"),
                                icon = icon("link", lib = "font-awesome"),
                                tags$li(
                                  a('Banco Mundial',
                                    href = "https://www.worldbank.org/",
                                    target = "_blank",
                                    tagAppendAttributes(icon(""), class = "text-info")
                                  ))
                       ))
              )),
              column(10,
                     br(),
                     fluidRow(
                       uiOutput("nota_piramide"),
                       br()
                       ),

                     fluidRow(
                       br(),
                       tabBox(title = "Pirámide poblacional",
                              width = 12,
                              side = 'right',
                              selected = "1",
                              tabPanel(value = "1",
                                       title = tagList(shiny::icon("bar-chart", lib = "font-awesome"), ""),
                                       column(12, 
                                              br(),
                                              uiOutput("plot_piramide_1"))
                              ),
                              tabPanel(value = "2",
                                       title = tagList(shiny::icon("list", lib = "font-awesome"), ""),
                                       br(),
                                       column(12, 
                                              br(),
                                              DT::dataTableOutput("tabla_piramide_1"))
                              ))
                     ))
            )),
    
    tabItem(tabName = "comparativo",
            fluidRow(
              column(2,
                     br(),
                     fluidRow(
                       gradientBox(
                         title = "Seleccionar datos:",
                         boxToolSize = "xs", 
                         closable = FALSE,
                         collapsible = FALSE,
                         width = 12,
                         footer = list(
                           pickerInput('comparativo_1', label = "Seleccione país:",
                                       choices = setdiff(paises, "Uruguay"), 
                                       selected = "Croatia",
                                       multiple = FALSE,
                                       options = list(`live-search` = TRUE,
                                                      `none-selected-text` = "Seleccione país",
                                                      size = 10)),
                           pickerInput('comparativo_2', label = "Seleccione año:",
                                       choices = anios, 
                                       selected = c(2011),
                                       multiple = FALSE,
                                       options = list(`live-search` = TRUE,
                                                      `none-selected-text` = "Seleccione año",
                                                      size = 10))
                         )))
                     
                     ),
              column(4,
                     br(),
                     fluidRow(
                       uiOutput("nota_comparativo_uru"),
                       br()
                     ),
                     fluidRow(
                       br(),
                       uiOutput("boxPad_comparativo_uru_1"),  # población total
                       br(),
                       uiOutput("boxPad_comparativo_uru_2"),  # zona urbana
                       uiOutput("boxPad_comparativo_uru_3")), # zona rural
                     fluidRow(
                       br(),
                       plotOutput("plot_comparativo_uru_1", width = "100%", height = "500px"),
                       br(),
                       uiOutput("boxPad_comparativo_uru_4"),  # tasa natalidad
                       uiOutput("boxPad_comparativo_uru_5"),  # tasa mortalidad
                       uiOutput("boxPad_comparativo_uru_6")   # tasa fertilidad
                     )),
              column(1),
              column(4,
                     br(),
                     fluidRow(
                       uiOutput("nota_comparativo_otro"),
                       br()
                     ),
                     
                     fluidRow(
                       br(),
                       uiOutput("boxPad_comparativo_otro_1"),  # población total
                       br(),
                       uiOutput("boxPad_comparativo_otro_2"),  # zona urbana
                       uiOutput("boxPad_comparativo_otro_3")), # zona rural
                     fluidRow(
                       br(),
                       plotOutput("plot_comparativo_otro_1", width = "100%", height = "500px"),
                       br(),
                       uiOutput("boxPad_comparativo_otro_4"),  # tasa natalidad
                       uiOutput("boxPad_comparativo_otro_5"),  # tasa mortalidad
                       uiOutput("boxPad_comparativo_otro_6")   # tasa fertilidad
                     ))
              ))
  )
)

ui <- dashboardPage(header, sidebar, body)

# server ####
server <- function(input, output) {
  
  output$nota_totales <- renderUI({
    helpText(
      h3(
        if (length(input$totales_2) == 1) {
          str_c('Población estimada, año ', input$totales_2)
        } else if (length(input$totales_2) == 2) {
          str_c('Población estimada, años ', input$totales_2[1], ' y ', input$totales_2[2])
        } else {
          str_c('Población estimada, años ', input$totales_2[1], ', ', input$totales_2[2], ' y ', input$totales_2[3])
        }
      ) 
    )
  })
  
  df_totales_1 <- reactive({
    poblacion %>%
      filter(series_name == "Population, total",
             anio %in% input$totales_2) %>% 
      select(`País` = country_name, anio, valor) %>% 
      spread(anio, valor)
  })
  
  
  output$tabla_totales_1 <-  DT::renderDataTable({
    DT::datatable(
      df_totales_1(),
      filter = "top",
      options = list(
        dom = 't',
        ordering = TRUE,
        lengthChange = FALSE,
        paging = FALSE, 
        scrollX = TRUE)) %>%
      DT::formatCurrency(2:(1+length(input$totales_2)), digits = 0, currency = "", mark = ".", dec.mark =  ",")
  })
  
  output$nota_proceso <- renderUI({
    
    a <- input$proceso_1
    b <- NULL
    for (i in 1:length(a)) {
      b <- str_c(b, a[i], sep = ', ')
    }
    helpText(
      h3(str_c('Países: ',  b, '.')
        ),
      h3(
        if (input$proceso_2[1] == input$proceso_2[2]) {
          str_c('Año: ', input$proceso_2[1])
        } else {
          str_c('Período: ', input$proceso_2[1], ' - ', input$proceso_2[2])
        }
      )
    )
  })
  
  df_proceso_1 <- reactive({
    poblacion %>%
      filter(series_name == "Population, total",
             country_name %in% input$proceso_1,
             anio >= input$proceso_2[1], 
             anio <= input$proceso_2[2])
  })
  
  max_proceso_1 <- reactive({
    max(df_proceso_1()$valor)
  })
  
  output$plot_proceso_1 <- renderPlot({
    p_proceso_1 <- ggplot(df_proceso_1(),
                          aes(x = anio,
                              y = valor)) +
      geom_line(aes(group = country_name,
                    colour = country_name)) +
      # scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
      ylim(0, max_proceso_1()) +
      labs(x = "Año", 
           y = "Población estimada", 
           colour = "",
           size = "") +
      theme(axis.line = element_line())
    p_proceso_1
  })
  
  output$plot_proceso_2 <- renderPlot({
    p_proceso_2 <- ggplot(df_proceso_1(),
                          aes(x = anio,
                              y = valor,
                              fill = country_name)) +
      geom_bar(stat = "identity",
               alpha = 0.7) +
      
      labs(x = "Año", 
           y = "Población estimada", 
           colour = "",
           size = "") +
      theme(axis.line = element_line())
    p_proceso_2
  })
  
  df_proceso_2 <- reactive({
    df_proceso_1() %>% 
      data.frame() %>% 
      select(country_name, anio, valor) %>% 
      spread(country_name, valor)
  })
 
  output$tabla_proceso_1 <-  DT::renderDataTable({
    DT::datatable(
      df_proceso_2(),
      rownames = FALSE,
      options = list(
        dom = 't',
        searching = FALSE,
        ordering = FALSE,
        lengthChange = FALSE,
        paging = FALSE, 
        scrollX = TRUE)
    ) %>%
      DT::formatCurrency(2:(1+length(input$proceso_1)), digits = 0, currency = "", mark = ".", dec.mark =  ",")
  })
  
  
  
  
  output$nota_piramide <- renderUI({
    helpText(
      h3(str_c('País: ', input$piramide_1)),
      h3(
        if (length(input$piramide_2) == 1) {
          str_c('Año: ', input$piramide_2)
        } else if (length(input$piramide_2) == 2) {
          str_c('Años: ', input$piramide_2[1], ' y ', input$piramide_2[2])
        } else {
          str_c('Años: ', input$piramide_2[1], ', ', input$piramide_2[2], ' y ', input$piramide_2[3])
        }
      ) 
    )
  })
  
  df_piramide_1 <- reactive({
    poblacion_edad_sexo %>%
      filter(country_name == input$piramide_1,
             anio %in% input$piramide_2)
  })

  max_piramide_1 <- reactive({
    max(df_piramide_1()$valor)
  })
  
  output$plot_piramide_1 <- renderUI({
    cantidad <- length(input$piramide_2)
    if (cantidad == 1) {
      plotOutput("plot_piramide_2", width = "60%", height = "600px")
    } else if (cantidad == 2) {
      plotOutput("plot_piramide_2", width = "75%", height = "600px")
    } else {
      plotOutput("plot_piramide_2", width = "100%", height = "600px")
    }
  })

  output$plot_piramide_2 <- renderPlot({
    p_piramide_1 <- ggplot(df_piramide_1(),
                 aes(x = as.factor(rango_etario),
                               y = ifelse(test = sexo == "M",
                                          yes = -valor,
                                          no = valor),
                               fill = sexo)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ anio) +
      coord_flip() +
      scale_y_continuous(#labels = abs,
                         limits = max_piramide_1() * c(-1,1)) +
      xlab("Rango etario") +
      ylab("Población estimada") +
      theme(legend.position="none",
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10, face = "bold"))
    p_piramide_1
  })

  output$nota_comparativo_uru <- renderUI({
    helpText(
      h1("Uruguay")
    )
  })
  
  df_comparativo_1 <- reactive({
    poblacion %>% 
      filter(series_name == "Population, total",
             country_name %in% c("Uruguay", input$comparativo_1),
             anio == input$comparativo_2)
  })
  

  output$boxPad_comparativo_uru_1 <- renderUI({
    boxPad(
      color = "blue",
      descriptionBlock(
        header = (df_comparativo_1() %>%
                    filter(country_name == "Uruguay") %>% 
                    mutate(valor =
                             format(valor,
                                    big.mark = ".", decimal.mark = ","))
        )$valor,
        text = "Población estimada",
        right_border = FALSE,
        margin_bottom = FALSE
      ))
  })
  
  df_comparativo_2 <- reactive({
    poblacion_residencia %>% 
      filter(country_name %in% c("Uruguay", input$comparativo_1),
             anio == input$comparativo_2)
  })
  
  output$boxPad_comparativo_uru_2 <- renderUI({
    boxPad(
      color = "gray",
      descriptionBlock(
        header = (df_comparativo_2() %>%
                    filter(country_name == "Uruguay",
                           zona == "Urban") %>% 
                    mutate(valor =
                             format(valor,
                                    big.mark = ".", decimal.mark = ","))
        )$valor,
        text = "Zona urbana",
        right_border = FALSE,
        margin_bottom = FALSE
      ))
  })


  output$boxPad_comparativo_uru_3 <- renderUI({
    boxPad(
      color = "gray",
      descriptionBlock(
        header = (df_comparativo_2() %>%
                    filter(country_name == "Uruguay",
                           zona != "Urban") %>% 
                    mutate(valor =
                             format(valor,
                                    big.mark = ".", decimal.mark = ","))
        )$valor,
        text = "Zona rural",
        right_border = FALSE,
        margin_bottom = FALSE
      ))
    })

  df_comparativo_3 <- reactive({
    poblacion_edad_sexo %>%
      filter(country_name %in% c("Uruguay", input$comparativo_1),
             anio == input$comparativo_2)
  })
  
    max_comparativo_1 <- reactive({
      max((df_comparativo_3() %>%
            filter(country_name == "Uruguay"))$valor)
    })
    

  output$plot_comparativo_uru_1 <- renderPlot({
    p_comparativo_1 <- ggplot(df_comparativo_3() %>% 
                                filter(country_name == "Uruguay"),
                           aes(x = as.factor(rango_etario),
                               y = ifelse(test = sexo == "M",
                                          yes = -valor,
                                          no = valor),
                               fill = sexo)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_y_continuous(limits = max_comparativo_1() * c(-1,1)) +
      xlab("Rango etario") +
      ylab("Población estimada") +
      theme(legend.position="none",
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10, face = "bold"))
    p_comparativo_1
  })
  
  df_comparativo_4 <- reactive({
    poblacion_tasas %>%
      filter(country_name %in% c("Uruguay", input$comparativo_1),
             anio == input$comparativo_2) 
  })
  


  # output$boxPad_comparativo_uru_4 <- renderUI({
  #   boxPad(
  #     color = "gray",
  #     descriptionBlock(
  #       header = (df_comparativo_4() %>%
  #                   filter(country_name == "Uruguay",
  #                          series_name %in% str_match(series_name, ".*Birth.*"),) %>% 
  #                   mutate(valor =
  #                            format(valor,
  #                                   big.mark = ".", decimal.mark = ","))
  #       )$valor,
  #       text = "Zona rural",
  #       right_border = FALSE,
  #       margin_bottom = FALSE
  #     ))
  # })
  # 
  # 
  # output$boxPad_comparativo_uru_5 <- renderUI({
  #   boxPad(
  #     color = "gray",
  #     descriptionBlock(
  #       header = (datasetInput_9() %>% 
  #                   mutate(credito = 
  #                            format(credito, 
  #                                   big.mark = ".", decimal.mark = ","))
  #       )$credito, 
  #       text = "Créditos presupuestados",
  #       right_border = FALSE,
  #       margin_bottom = FALSE
  #     ))
  # })
  # 
  # 
  # output$boxPad_comparativo_uru_6 <- renderUI({
  #   boxPad(
  #     color = "gray",
  #     descriptionBlock(
  #       header = (datasetInput_9() %>% 
  #                   mutate(credito = 
  #                            format(credito, 
  #                                   big.mark = ".", decimal.mark = ","))
  #       )$credito, 
  #       text = "Créditos presupuestados",
  #       right_border = FALSE,
  #       margin_bottom = FALSE
  #     ))
  # })
  # 
  
  output$nota_comparativo_otro <- renderUI({
    helpText(
      h1(input$comparativo_1)
    )
  })
  
  # 
  output$boxPad_comparativo_otro_1 <- renderUI({
    boxPad(
      color = "blue",
      descriptionBlock(
        header = (df_comparativo_1() %>%
                    filter(country_name == input$comparativo_1) %>% 
                    mutate(valor =
                             format(valor,
                                    big.mark = ".", decimal.mark = ","))
        )$valor,
        text = "Población estimada",
        right_border = FALSE,
        margin_bottom = FALSE
      ))
    })


  output$boxPad_comparativo_otro_2 <- renderUI({
    boxPad(
      color = "gray",
      descriptionBlock(
        header = (df_comparativo_2() %>%
                    filter(country_name != "Uruguay",
                           zona == "Urban") %>% 
                    mutate(valor =
                             format(valor,
                                    big.mark = ".", decimal.mark = ","))
        )$valor,
        text = "Zona urbana",
        right_border = FALSE,
        margin_bottom = FALSE
      ))
  })


  output$boxPad_comparativo_otro_3 <- renderUI({
    boxPad(
      color = "gray",
      descriptionBlock(
        header = (df_comparativo_2() %>%
                    filter(country_name != "Uruguay",
                           zona != "Urban") %>% 
                    mutate(valor =
                             format(valor,
                                    big.mark = ".", decimal.mark = ","))
        )$valor,
        text = "Zona rural",
        right_border = FALSE,
        margin_bottom = FALSE
      ))
    
  })

    max_comparativo_2 <- reactive({
      max((df_comparativo_3() %>% 
            filter(country_name != "Uruguay"))$valor)
      })
    

  output$plot_comparativo_otro_1 <- renderPlot({
    p_comparativo_2 <- ggplot(df_comparativo_3() %>% 
                                filter(country_name != "Uruguay"),
                              aes(x = as.factor(rango_etario),
                                  y = ifelse(test = sexo == "M",
                                             yes = -valor,
                                             no = valor),
                                  fill = sexo)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_y_continuous(limits = max_comparativo_2() * c(-1,1)) +
      xlab("Rango etario") +
      ylab("Población estimada") +
      theme(legend.position="none",
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10, face = "bold"))
    p_comparativo_2
  })
  # 
  # 
  # output$boxPad_comparativo_otro_4 <- renderUI({
  #   boxPad(
  #     color = "gray",
  #     descriptionBlock(
  #       header = (datasetInput_9() %>% 
  #                   mutate(credito = 
  #                            format(credito, 
  #                                   big.mark = ".", decimal.mark = ","))
  #       )$credito, 
  #       text = "Créditos presupuestados",
  #       right_border = FALSE,
  #       margin_bottom = FALSE
  #     ))
  # })
  # 
  # 
  # output$boxPad_comparativo_otro_5 <- renderUI({
  #   boxPad(
  #     color = "gray",
  #     descriptionBlock(
  #       header = (datasetInput_9() %>% 
  #                   mutate(credito = 
  #                            format(credito, 
  #                                   big.mark = ".", decimal.mark = ","))
  #       )$credito, 
  #       text = "Créditos presupuestados",
  #       right_border = FALSE,
  #       margin_bottom = FALSE
  #     ))
  # })
  # 
  # 
  # output$boxPad_comparativo_otro_6 <- renderUI({
  #   boxPad(
  #     color = "gray",
  #     descriptionBlock(
  #       header = (datasetInput_9() %>% 
  #                   mutate(credito = 
  #                            format(credito, 
  #                                   big.mark = ".", decimal.mark = ","))
  #       )$credito, 
  #       text = "Créditos presupuestados",
  #       right_border = FALSE,
  #       margin_bottom = FALSE
  #     ))
  # })
  # 
  # 
  # output$obj_credito <- renderUI({
  #   boxPad(
  #     color = "gray",
  #     descriptionBlock(
  #       header = (datasetInput_9() %>% 
  #                   mutate(credito = 
  #                            format(credito, 
  #                                   big.mark = ".", decimal.mark = ","))
  #       )$credito, 
  #       text = "Créditos presupuestados",
  #       right_border = FALSE,
  #       margin_bottom = FALSE
  #     ))
  # })
  # 
  
}

# Run app
shinyApp(ui, server)
