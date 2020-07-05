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
  titleWidth = 220,
  # Incorpora logo desde la web:
  tags$li(a(href = 'https://www.rafap.com.uy/',
            img(src = "https://www.rafap.com.uy/mvdcms/plantillas/images/logo.svg",
                height="30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown")
  )

sidebar <- dashboardSidebar(
  disable = FALSE,
  width = 220,
  sidebarMenu(
    menuItem("Proceso demográfico", tabName = "proceso", icon = icon("bar-chart", lib = "font-awesome")),
    menuItem("Pirámides poblacionales", tabName = "piramide", icon = icon("users", lib = "font-awesome"))
  ))

body <- dashboardBody(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tabItems(
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
                                       multiple = FALSE,
                                       options = list(`live-search` = TRUE,
                                                      `none-selected-text` = "Seleccione país",
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
                                       title = tagList(shiny::icon("bar-chart", lib = "font-awesome"), ""),
                                       column(12, 
                                              br(),
                                              plotOutput("plot_proceso_1"))
                              ),
                              tabPanel(value = "2",
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
                       
                       
                     )
              ),
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
                     )
                     )
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)#, skin = "black")

# server ####
server <- function(input, output) {
  
  output$nota_proceso <- renderUI({
    helpText(
      h3(str_c('País: ', input$proceso_1)),
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
             country_name == input$proceso_1,
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
      
      geom_point(aes(text = str_c(country_name, ' (', anio, '): ',
                                 format(valor, big.mark = ".", decimal.mark = ","))),
                 alpha = 0.7,
                 colour = country_name) +
      scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
      ylim(0, max_proceso_1()) +
      labs(x = "Año", 
           y = "Población estimada", 
           colour = "",
           size = "") +
      theme(axis.line = element_line())
    p_proceso_1
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
                               fill = sexo)
                 ) +
      geom_bar(stat = "identity"
               # ,
               # aes(text = str_c(ifelse(sexo == "F", "Mujeres, ", "Hombres, "),
               #                  rango_etario, ' (', anio, '):\n',
               #                  format(valor, big.mark = ".", decimal.mark = ","))
               #                  )
               ) +
      facet_wrap(~ anio) +
      # , scales = "free") +
      coord_flip() +
      scale_y_continuous(#labels = abs,
                         limits = max_piramide_1() * c(-1,1)) +
      xlab("Rango etario") +
      ylab("Población estimada") +
      theme(legend.position="none",
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10, face = "bold"))
    # p_piramide_1 <- ggplotly(p_piramide_1, tooltip = "text") %>%
    #   config(displayModeBar = FALSE) %>%
    #   config(showLink = FALSE)
    p_piramide_1

  })

}

# Run app
shinyApp(ui, server)
