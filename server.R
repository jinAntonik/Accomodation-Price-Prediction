library(readxl)
library(readr)

library(dplyr)
library(tidymodels)
library(rpart.plot)

library(lmtest)
library(xgboost)
library(gbm)
library(glmnet)
library(randomForest)
library(mlbench)
library(caret)
library(caretEnsemble)
library(lattice)

library(rsconnect)


data_final = read_excel("~/data_final.xlsx")

server <- function(input, output) {
  data_user <- eventReactive(input$button, {  
    data_user = data.frame(id = 9999,
                           square = as.numeric(input$num_square),
                           rooms = as.character(input$rooms),
                           kitchen_square = as.numeric(input$num_kitsquare),
                           living_square = as.numeric(input$num_livsquare),
                           year = as.numeric(input$year),
                           toilet_quantity = as.numeric(input$toilet_quantity),
                           type_toilet = as.numeric(input$toilet_type),
                           if_lift = as.numeric(input$elevator),
                           if_balcony = as.numeric(input$balcony),
                           housetype = as.character(input$house_type),
                           remont = as.character(input$remont),
                           subway_dist_peshkom_TRUE = as.numeric(input$min_subway),
                           district = as.character(input$district),
                           floor = as.numeric(input$floor),
                           subway_TRUE = as.character(input$subway))
  })
  
  # recommendation system
  recommend = reactive({
    data_final = read_excel("~/data_final.xlsx")
    data1 = data_final %>% select(id,
                                  square,
                                  rooms,
                                  district,
                                  kitchen_square,
                                  living_square,
                                  floor,
                                  year,
                                  toilet_quantity,
                                  type_toilet,
                                  if_balcony,
                                  if_lift,
                                  housetype,
                                  remont,
                                  subway_TRUE,
                                  subway_dist_peshkom_TRUE)
    
    
    data_user = data_user() # user's parameters
    data_cont = rbind(data1, data_user)
    
    data_cont$square_cat = paste(as.character(round(data_cont$square/10,0)),"square")
    data_cont$kitchen_square_cat = paste(as.character(round(data_cont$kitchen_square/10,0)),"kitchen_square")
    data_cont$living_square_cat = paste(as.character(round(data_cont$living_square/10,0)),"living_square")
    data_cont$subway_dist_cat = paste(as.character(round(data_cont$subway_dist_peshkom_TRUE/5,0)),"subway")
    data_cont$floor = paste(as.character(data_cont$floor),"floor")
    data_cont$year = as.character(data_cont$year)
    data_cont$toilet_quantity = paste(as.character(data_cont$toilet_quantity),"toilet")
    data_cont$type_toilet = paste(as.character(data_cont$type_toilet),"type_toilet")
    data_cont$if_balcony = paste(as.character(data_cont$if_balcony),"balcony")
    data_cont$if_lift = paste(as.character(data_cont$if_lift),"lift")
    
    
    # to wide format
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=rooms,values_from=exist,values_fill = 0)
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=floor,values_from=exist,values_fill = 0)
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=year,values_from=exist,values_fill = 0)
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=toilet_quantity,values_from=exist,names_repair = "minimal",values_fill = 0)
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=type_toilet,values_from=exist,values_fill = 0)
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=if_balcony,values_from=exist,names_repair = "minimal",values_fill = 0)
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=if_lift,values_from=exist,names_repair = "minimal",values_fill = 0)
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=district,values_from=exist,values_fill = 0)
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=remont,values_from=exist,values_fill = 0)
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=housetype,values_from=exist,values_fill = 0)
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=subway_TRUE,values_from=exist,values_fill = 0)
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=square_cat,values_from=exist,values_fill = 0)
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=kitchen_square_cat,values_from=exist,values_fill = 0)
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=living_square_cat,values_from=exist,values_fill = 0)
    data_cont$exist = 1
    data_cont = data_cont %>% pivot_wider(names_from=subway_dist_cat,values_from=exist,values_fill = 0)
    
    
    id = data_cont$id
   
    data_cont = data_cont %>% select(-c(id, square, kitchen_square, living_square, subway_dist_peshkom_TRUE))
    rownames(data_cont) = id
    a = as.matrix(data_cont)
    t = t(a)
    sim = coop::cosine(t)
    diag(sim) = 0
    
    sim_1 = sim
    sim_1["9999",]
    simCut = sim_1[,"9999"]
    mostSimilar = head(sort(simCut, decreasing = T), n = 5)
    a = which(simCut %in% mostSimilar, arr.ind = TRUE, useNames = T)
    a = a[1:5]
    index = arrayInd(a, .dim = dim(sim))
    result = rownames(sim)[index[,1]]
    recommend = data_final %>% filter(row.names(data_final) %in% result)
    recommend$shirota = as.numeric(recommend$shirota)
    recommend$dolgota = as.numeric(recommend$dolgota)
    recommend
  })
  
  # alike flats
  output$mymap = renderLeaflet({
    vector1 = c(1, 0)
    vector2 = c("есть", "нет")
    
    recommend = recommend()
    M = leaflet(recommend) %>% setView(lng = 30.3141, lat = 59.9386, zoom = 10)
    M %>% addTiles() %>%
      addMarkers(~dolgota, ~shirota, popup = paste("<br>Адрес:", recommend$addr,
                                                   "<br>Район:", recommend$district,
                                                   "<br>Цена:", paste0(formatC(as.numeric(recommend$price), format="f", digits=0, big.mark="'"), " ₽"),
                                                   "<br>Кол-во комнат:", recommend$rooms,
                                                   "<br>Площадь (кв.м):", recommend$square,
                                                   "<br>Жилая площадь (кв.м):", recommend$living_square,
                                                   "<br>Площадь кухни (кв.м):", recommend$kitchen_square,
                                                   "<br>Этаж:", recommend$floor,
                                                   "<br>Год постройки:", recommend$year,
                                                   "<br>Тип дома:", recommend$housetype,
                                                   "<br>Балкон:", recommend$if_balcony,
                                                   "<br>Наличие лифта:", recommend$if_lift,
                                                   "<br>Кол-во санузлов:", recommend$toilet_quantity,
                                                   "<br>Тип туалета:", recommend$type_toilet,
                                                   "<br>Ремонт:", recommend$remont,
                                                   "<br>Станция метро:", recommend$subway_TRUE,
                                                   "<br>Пешком до метро (мин):", recommend$subway_dist_peshkom_TRUE),
                 label = paste0(formatC(as.numeric(recommend$price), format="f", digits=0, big.mark="'"), " ₽"))
  })
  
  # prediction model
  output$prediction = renderUI({
  
    load("~/model1.Rda")
    load("~/model2.Rda")
    load("~/model3.Rda")
    load("~/model4.Rda")
    load("~/model5.Rda")
  
    load("~/lamb1.Rda")
    load("~/lamb2.Rda")
    
    data_user = data_user()
    
    data = data_user
    
    data$rooms = case_when(data_user$rooms =="1"~1, data_user$rooms =="2"~2, data_user$rooms =="3"~3, data_user$rooms == "4"~4, data_user$rooms =="5"~5, data_user$rooms == "Апартаменты-студия"~6, data_user$rooms =="Квартира свободной планировки"~7, data_user$rooms == "Многокомнатная квартира"~8, data_user$rooms == "Многокомнатные апартаменты"~9, data_user$rooms == "Студия"~10)
    data$district = case_when(data_user$district == "Адмиралтейский"~1, data_user$district == "Василеостровский"~2, data_user$district == "Выборгский"~3, data_user$district == "Калининский"~4, data_user$district == "Кировский"~5, data_user$district == "Колпинский"~6, data_user$district == "Красногвардейский"~7, data_user$district == "Красносельский"~8, data_user$district == "Кронштадтский"~9, data_user$district == "Курортный"~10, data_user$district == "Московский"~11, data_user$district == "Невский"~12, data_user$district == "Петроградский"~13, data_user$district == "Петродворцовый"~14, data_user$district == "Приморский"~15, data_user$district == "Пушкинский"~16, data_user$district == "Фрунзенский"~17, data_user$district == "Центральный"~18)
    data$housetype = case_when(data_user$housetype == "Блочный"~1, data_user$housetype == "Кирпичный"~2, data_user$housetype == "Монолитный"~3, data_user$housetype == "Панельный"~4, data_user$housetype == "Сталинский"~5)
    data$remont = case_when(data_user$remont == "Без ремонта"~1, data_user$remont == "Дизайнерский"~2, data_user$remont == "Евроремонт"~3, data_user$remont == "Косметический"~4)
    data$subway_TRUE = case_when(data_user$subway_TRUE == "Автово"~1, data_user$subway_TRUE == "Адмиралтейская"~2, data_user$subway_TRUE == "Академическая"~3, data_user$subway_TRUE == "Балтийская"~4, data_user$subway_TRUE == "Беговая"~5, data_user$subway_TRUE == "Бухарестская"~6, data_user$subway_TRUE == "Василеостровская"~7, data_user$subway_TRUE == "Владимирская"~8, data_user$subway_TRUE == "Волковская"~9, data_user$subway_TRUE == "Выборгская"~10, data_user$subway_TRUE == "Горьковская"~11, data_user$subway_TRUE == "Гостиный двор"~12, data_user$subway_TRUE == "Гражданский проспект"~13, data_user$subway_TRUE == "Девяткино"~14, data_user$subway_TRUE == "Достоевская"~15, data_user$subway_TRUE == "Дунайская"~16, data_user$subway_TRUE == "Елизаровская"~17, data_user$subway_TRUE == "Звездная"~18, data_user$subway_TRUE == "Звенигородская"~19, data_user$subway_TRUE == "Зенит"~20, data_user$subway_TRUE == "Кировский завод"~21, data_user$subway_TRUE == "Комендантский проспект"~22, data_user$subway_TRUE == "Крестовский остров"~23, data_user$subway_TRUE == "Купчино"~24, data_user$subway_TRUE == "Ладожская"~25, data_user$subway_TRUE == "Ленинский проспект"~26, data_user$subway_TRUE == "Лесная"~27, data_user$subway_TRUE == "Лиговский проспект"~28, data_user$subway_TRUE == "Ломоносовская"~29, data_user$subway_TRUE == "Маяковская"~30, data_user$subway_TRUE == "Международная"~31, data_user$subway_TRUE == "Московская"~32, data_user$subway_TRUE == "Московские ворота"~33, data_user$subway_TRUE == "Нарвская"~34, data_user$subway_TRUE == "Невский проспект"~35, data_user$subway_TRUE == "Новочеркасская"~36, data_user$subway_TRUE == "Обводный канал"~37, data_user$subway_TRUE == "Обухово"~38, data_user$subway_TRUE == "Озерки"~39, data_user$subway_TRUE == "Парк Победы"~40, data_user$subway_TRUE == "Парнас"~41, data_user$subway_TRUE == "Петроградская"~42, data_user$subway_TRUE == "Пионерская"~43, data_user$subway_TRUE == "Площадь Александра Невского"~44, data_user$subway_TRUE == "Площадь Восстания"~45, data_user$subway_TRUE == "Площадь Ленина"~46, data_user$subway_TRUE == "Площадь Мужества"~47, data_user$subway_TRUE == "Политехническая"~48, data_user$subway_TRUE == "Приморская"~49, data_user$subway_TRUE == "Пролетарская"~50, data_user$subway_TRUE == "Проспект Большевиков"~51, data_user$subway_TRUE == "Проспект Ветеранов"~52, data_user$subway_TRUE == "Проспект Просвещения"~53, data_user$subway_TRUE == "Проспект Славы"~54, data_user$subway_TRUE == "Пушкинская"~55, data_user$subway_TRUE == "Рыбацкое"~56, data_user$subway_TRUE == "Садовая"~57, data_user$subway_TRUE == "Сенная площадь"~58, data_user$subway_TRUE == "Спасская"~59, data_user$subway_TRUE == "Спортивная"~60, data_user$subway_TRUE == "Старая Деревня"~61, data_user$subway_TRUE == "Технологический институт"~62, data_user$subway_TRUE == "Удельная"~63, data_user$subway_TRUE == "Улица Дыбенко"~64, data_user$subway_TRUE == "Фрунзенская"~65, data_user$subway_TRUE == "Черная речка"~66, data_user$subway_TRUE == "Чернышевская"~67, data_user$subway_TRUE == "Чкаловская"~68, data_user$subway_TRUE == "Шушары"~69, data_user$subway_TRUE == "Электросила"~70)
    
    
    data_user$rooms = data_user$rooms %>% as.factor()
    levels(data_user$rooms) = c("1", "2", "3", "4", "5", "Апартаменты-студия", "Квартира свободной планировки", "Многокомнатная квартира", "Многокомнатные апартаменты", "Студия")
    
    data_user$district = as.factor(data_user$district)
    levels(data_user$district) = c("Адмиралтейский", "Василеостровский", "Выборгский", "Калининский", "Кировский", "Колпинский", "Красногвардейский", "Красносельский", "Кронштадтский", "Курортный", "Московский", "Невский", "Петроградский", "Петродворцовый", "Приморский", "Пушкинский", "Фрунзенский", "Центральный")
    
    data_user$housetype = as.factor(data_user$housetype)
    levels(data_user$housetype) = c("Блочный", "Кирпичный","Монолитный","Панельный","Сталинский")
    
    data_user$remont = data_user$remont %>% as.factor()
    levels(data_user$remont) = c("Без ремонта", "Дизайнерский", "Евроремонт", "Косметический")
    
    data_user$subway_TRUE = data_user$subway_TRUE %>% as.factor()
    levels(data_user$subway_TRUE) = c("Автово", "Адмиралтейская", "Академическая", "Балтийская", "Беговая", "Бухарестская", "Василеостровская", "Владимирская", "Волковская", "Выборгская", "Горьковская", "Гостиный двор", "Гражданский проспект", "Девяткино", "Достоевская", "Дунайская", "Елизаровская", "Звездная", "Звенигородская", "Зенит", "Кировский завод", "Комендантский проспект", "Крестовский остров", "Купчино", "Ладожская", "Ленинский проспект", "Лесная", "Лиговский проспект", "Ломоносовская", "Маяковская", "Международная", "Московская", "Московские ворота", "Нарвская", "Невский проспект", "Новочеркасская", "Обводный канал", "Обухово", "Озерки", "Парк Победы", "Парнас", "Петроградская", "Пионерская", "Площадь Александра Невского", "Площадь Восстания", "Площадь Ленина", "Площадь Мужества", "Политехническая", "Приморская", "Пролетарская", "Проспект Большевиков", "Проспект Ветеранов", "Проспект Просвещения", "Проспект Славы", "Пушкинская", "Рыбацкое", "Садовая", "Сенная площадь", "Спасская", "Спортивная", "Старая Деревня", "Технологический институт", "Удельная", "Улица Дыбенко", "Фрунзенская", "Черная речка", "Чернышевская", "Чкаловская", "Шушары", "Электросила")
    
    
    matr_data_user = data.matrix(data[,2:16])
    
    pred1 = predict(model1_wf, data_user)
    pred2 = predict(model2_wf, data_user)
    pred3 = predict(ridge_model, s = best_lambda, newx = matr_data_user)
    pred4 = predict(lasso_model, s = best_lambda2, newx = matr_data_user)
    pred5 = predict(rf, data_user)
    
    prediction = (pred1 + pred2 + pred3 + pred4 + pred5) / 5
    HTML(paste("Рекомендованная цена продажи Вашей квартиры составляет примерно: ", "<br>", 
               "<font size = 30, color = 'white'>", formatC(round(0.98*as.numeric(prediction),-4), format="f", digits=0, big.mark="'"),"–",formatC(round(1.02*as.numeric(prediction),-4), format="f", digits=0, big.mark="'"),"₽", "</font>"))
  })
  
}


shinyApp(ui = ui, server = server)