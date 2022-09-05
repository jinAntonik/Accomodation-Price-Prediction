library(shiny)
library(leaflet)
library(shinythemes)
library(shinycustomloader)


ui <- fluidPage(
  theme = shinytheme("slate"),
  
  titlePanel("Результаты рекомендации и предсказания цены"),
  
  sidebarLayout(
    sidebarPanel(
      column(6, 
             selectInput(inputId = "district", label = "Введите название района",
                         selected = as.factor(levels(sort(data_final$district)[1])),
                         choices = sort(data_final$district)),
             
             selectInput(inputId = "subway", label = "Выберите ближайшее метро", 
                         selected = as.factor(levels(sort(data_final$subway_TRUE)[1])),
                         choices = sort(data_final$subway_TRUE)),
             
             numericInput("min_subway", label = ("Укажите сколько минут до метро пешком"), 
                          min = 1, max = 120, step = 1, value = 20),
             
             numericInput(inputId = "year", label = "Введите год постройки",  
                          min = 1746, max = 2022, value = 1965, step = 1),
             
             selectInput(inputId = "house_type", label = "Выберите тип дома", 
                         selected = as.factor(levels(sort(data_final$housetype)[1])),
                         choices = sort(data_final$housetype)),
             
             sliderInput(inputId = "floor", label = "Выберите этаж", 
                         min = 1, max = 40, value = 5, step = 1),
             
             selectInput(inputId = "rooms", label = "Введите количество комнат",
                         selected = as.factor(levels(sort(data_final$rooms)[1])),
                         choices = sort(data_final$rooms))),
      column(5,
             numericInput("num_square", label = ("Укажите общую площадь"), min = 1, value = 50, step = 1),
             
             numericInput("num_kitsquare", label = ("Укажите площадь кухни"), min = 1, value = 8, step = 1),
             
             numericInput("num_livsquare", label = ("Укажите жилую площадь"), min = 1, value = 20, step = 1),
             
             selectInput(inputId = "remont", label = "Выберите тип ремонта",
                         selected = as.factor(levels(sort(data_final$remont)[1])), 
                         choices = sort(data_final$remont)),
             
             radioButtons("balcony", "Наличие балкона",
                          selected = 1,
                          c("Есть" = 1,
                            "Нет" = 0)),
             
             radioButtons("elevator", "Наличие лифта",
                          selected = 1,
                          c("Есть" = 1,
                            "Нет" = 0)),
             
             selectInput(inputId = "toilet_type", label = "Выберите тип санузла", 
                         selected = as.factor(levels(sort(data_final$type_toilet)[1])),
                         choices = c("раздельный"= 1, 
                                     "совмещенный"= 0)),
             
             numericInput(inputId = "toilet_quantity", label = "Введите количество санузлов", 
                          min = 1, value = 1, step = 1)),
      
      actionButton("button", "Предсказать цену"),
      width = 5 # input width
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Похожие квартиры", withLoader(leafletOutput("mymap", 
                                                                        width = "115%", height = 600), 
                                                          type="html", loader="loader1")),
                  
                  tabPanel("Рекомендованная цена", withLoader(htmlOutput("prediction"), 
                                                              type="html", loader="loader1"))
      ),
      width = 6 # output width
      
    )
  )
)