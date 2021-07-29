#####trzecia karta####

hide(id='legenda')
hide(id='legenda1')
show(id='legenda2')


observeEvent(input$zastosuj2,{ 
  shinyjs::show(selector = '.progress-group:has(#postep2)')
  updateProgressBar(session = session, id = "postep2", value = 0, title = "Pasek postępu")
  

  # piotrek <- reactive({
  #   if (is.null(input$grupa_ostatnia2)){
  #     TRUE
  #   } else {
  #     x <- paste0(" last_sent_order_products_group %like% '\\\\<",input$grupa_ostatnia2,"\\\\>' ", collapse =  input$warunek_ostatnia2)
  #     paste0("(", x, ")")
  #   }
  # })
  # 
  # piotrek2 <- reactive({
  #   if (is.null(input$grupa_wszystkie2)){
  #     TRUE
  #   } else {
  #     y <- paste0(" client_sent_product_groups %like% '\\\\<",input$grupa_wszystkie2,"\\\\>' ", collapse =  input$warunek_wszystkie2)
  #     paste0("(", y, ")")
  #   }
  # })

  updateProgressBar(session = session, id = "postep2", value = 10, title = "Przetwarzanie danych")
  
  if (!is.null(input$ostatnie_wyslane2)) {
    aaa<- paste0(  "(",  paste0(" last_sent_order_products_group %like% '",input$grupa_ostatnia2,"' ", collapse =  input$warunek_ostatnia2), ")")
    bbb<- paste0("(",paste0(" client_sent_product_groups %like% '",input$grupa_wszystkie2,"' ", collapse =  input$warunek_wszystkie2), ")")
    
    
    texxt<- paste0("dane_trzecia1 <- dane_trzecia0[country %in% input$kraj2 &",
                                     if (is.null(input$grupa_ostatnia2)){T}else{ 
                                      aaa } ,"&
                                    client_sex %in% input$plec2 &",
                                     if (is.null(input$grupa_wszystkie2)){T}else{ 
                                     bbb },"&
                                    last_order_client_age_range %in% input$wiek2 &
                                    client_type %in% input$typ2 &
                                    has_address %in% input$adres2 &
                                    has_email %in% input$mail2 &
                                    last_order_action_channel %in% input$kanal2 &
                                    substr(first_order_added_date,1,7) >= substr(input$pierwsze_zamowienie2[1],1,7) &
                                    substr(first_order_added_date,1,7) <= substr(input$pierwsze_zamowienie2[length(input$pierwsze_zamowienie2)],1,7) &
                                    substr(last_order_added_date,1,7) >= substr(input$ostatnie_zamowienie2[1],1,7) &
                                    substr(last_order_added_date,1,7) <= substr(input$ostatnie_zamowienie2[length(input$ostatnie_zamowienie2)],1,7) &
                                    substr(last_sent_date,1,7) >= substr(input$ostatnie_wyslane2[1],1,7) &
                                    substr(last_sent_date,1,7) <= substr(input$ostatnie_wyslane2[length(input$ostatnie_wyslane2)],1,7)
                                  ]")
    
    eval(parse(text=texxt))

  } else {
    aaa<- paste0(  "(",  paste0(" last_sent_order_products_group %like% '",input$grupa_ostatnia2,"' ", collapse =  input$warunek_ostatnia2), ")")
    bbb<- paste0("(",paste0(" client_sent_product_groups %like% '",input$grupa_wszystkie2,"' ", collapse =  input$warunek_wszystkie2), ")")
   texxt<- paste0("dane_trzecia1 <- dane_trzecia0[country %in% input$kraj2 &",
                                     if (is.null(input$grupa_ostatnia2)){T}else{aaa
                                       }
                                 ,"&   client_sex %in% input$plec2 &",
                                     if (is.null(input$grupa_wszystkie2)){T}else{   bbb
                                       }
                           ,"& last_order_client_age_range %in% input$wiek2 &
                                    client_type %in% input$typ2 &
                                    has_address %in% input$adres2 &
                                    has_email %in% input$mail2 &
                                    last_order_action_channel %in% input$kanal2 &
                                    substr(first_order_added_date,1,7) >= substr(input$pierwsze_zamowienie2[1],1,7) &
                                    substr(first_order_added_date,1,7) <= substr(input$pierwsze_zamowienie2[length(input$pierwsze_zamowienie2)],1,7) &
                                    substr(last_order_added_date,1,7) >= substr(input$ostatnie_zamowienie2[1],1,7) &
                                    substr(last_order_added_date,1,7) <= substr(input$ostatnie_zamowienie2[length(input$ostatnie_zamowienie2)],1,7)
                                  ]")
   eval(parse(text=texxt))

  }
  
  updateProgressBar(session = session, id = "postep2", value = 50, title = "Przetwarzanie danych")
  
  if (nrow(dane_trzecia1)>1) {
    if (input$agregat21 == 'przedział_ostatnie') {
      
      dane_trzecia1[is.na(last_sent_order_value) & orders_sent == 1, last_sent_order_value := sent_orders_value]
      dane_trzecia1 <- dane_trzecia1[!is.na(last_sent_order_value),]
      dane_trzecia1[,Total:='Total']
      
      slow<-data.table(c('country','client_sex','last_order_client_age_range','last_order_action_channel','Total'),
                      c('Kraj','Płeć konsumenta','Kanał ostatniego zamówienia','Wiek konsumenta','Total'))
      
      

        dane_trzecia_tab <- dane_trzecia1[, lapply(.SD, mean, na.rm=TRUE), by = c(input$agregat22,input$agregat23), .SDcols=c("last_sent_order_value") ]
        dane_trzecia_tab[,drugie := input$agregat23]
        sort <- dane_trzecia1[,.N,by=c(input$agregat22)]
        sort1 <- dane_trzecia1[,.(N1 = .N), by=c(input$agregat23)]
        dane_trzecia_tab <- merge(dane_trzecia_tab, sort1, by = c(input$agregat23), all.x = T)
        dane_trzecia_tab <- merge(dane_trzecia_tab, sort, by = c(input$agregat22), all.x = T)
        dane_trzecia_tab <- dane_trzecia_tab[order(-N,-N1)]
        dane_trzecia_tab <- dane_trzecia_tab[,c(1,2,3)]
        kolumna <- slow[V1==input$agregat22,V2]
        kolumna1 <- slow[V1==input$agregat23,V2]
      

      
      dane_trzecia_tab$last_sent_order_value <- round(dane_trzecia_tab$last_sent_order_value)
      
      names(dane_trzecia_tab) <- c('kolumna', 'kolumna1', 'N')
      
      if ((input$agregat22 != input$agregat23) & input$agregat23 != 'Total') {
        
        for (i in nrow(dane_trzecia_tab):1) {
          if (dane_trzecia_tab$kolumna[i] %in% dane_trzecia_tab$kolumna[-i]) {
            dane_trzecia_tab$kolumna[i] <- paste0('<span style="font-size: 0%">', dane_trzecia_tab$kolumna[i], '</span>')
          }
        }
        
      } else {
        dane_trzecia_tab$kolumna1 <- ''
      }
      if(input$waluta==FALSE) {      
        output$tabela_trzecia <- DT::renderDataTable({
          datatable(
            dane_trzecia_tab,
            filter = 'bottom',
            rownames = FALSE,
            colnames = c(kolumna, kolumna1, 'Średnia wartość zamówienia'),
            escape = FALSE,
            options = list(
              lenghtMenu = c(10, 15, 20, 50, 100),
              paging = T,
              fixedHeader=TRUE,
              scroller = T,
              scrollx=T,
              scrolly=F,
              pageLength = 15,
              autoWidth = F,
              dom ='t<"bottom"lp><"clear">',
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#2276b3', 'color': '#fff'});",
                "}")
            )) %>% formatCurrency(3, ' zł', digits = 0, before = FALSE)
          
        })
      } else {
        
        dane_trzecia_tab[,N:=N/4.5]
        output$tabela_trzecia <- DT::renderDataTable({
          datatable(
            dane_trzecia_tab,
            filter = 'bottom',
            rownames = FALSE,
            colnames = c(kolumna, kolumna1, 'Średnia wartość zamówienia'),
            escape = FALSE,
            options = list(
              lenghtMenu = c(10, 15, 20, 50, 100),
              paging = T,
              fixedHeader=TRUE,
              scroller = T,
              scrollx=T,
              scrolly=F,
              pageLength = 15,
              autoWidth = F,
              dom ='t<"bottom"lp><"clear">',
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#2276b3', 'color': '#fff'});",
                "}")
            )) %>% formatCurrency(3, ' €', digits = 0, before = FALSE)
          
        })
        
      }
      
      if (input$wykres2 == 'Total') {
        
        if(input$waluta==FALSE) {
          dane_trzecia1[is.na(last_sent_order_value) & orders_sent == 1, last_sent_order_value := sent_orders_value]
          dane_trzecia1 <- dane_trzecia1[!is.na(last_sent_order_value),]
          dane_trzecia1[, przedział_ostatnie := ceiling(last_sent_order_value/50)*50]
          dane_trzecia1[przedział_ostatnie == 0 | przedział_ostatnie == 50, przedział_ostatnie := 100]
          dane_trzecia1[przedział_ostatnie >900, przedział_ostatnie := 900]
          dane_trzecia_bar <- dane_trzecia1[,.N,by = 'przedział_ostatnie']
          dane_trzecia_bar <- dane_trzecia_bar[order(przedział_ostatnie)]
          dane_trzecia_bar[,przedział_ostatnie := paste(przedział_ostatnie - 50, 'zł -', przedział_ostatnie, 'zł')]
          dane_trzecia_bar[przedział_ostatnie == '850 zł - 900 zł', przedział_ostatnie := '850+ zł']
          dane_trzecia_bar[przedział_ostatnie == '50 zł - 100 zł', przedział_ostatnie := '0 zł - 100 zł']
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(dane_trzecia_bar, x = ~dane_trzecia_bar$przedział_ostatnie, y = ~dane_trzecia_bar$N, type = 'bar',
                    text = paste0(round(dane_trzecia_bar$N/nrow(dane_trzecia1),3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość ostatniego zamówienia: %{x} <br>Liczba konsumentów: ', format(dane_trzecia_bar$N, big.mark = ' ', big.interval = 3L),'<extra></extra>'))%>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości ostatniego zamówienia",
                  yanchor = "top",
                  pad = list(
                    t = 10 )),
                xaxis = list(
                  title = list(
                    text = 'Wartość ostatniego zamówienia'
                  ),
                  autorange = TRUE,
                  categoryorder = "array",
                  categoryarray = dane_trzecia_bar$przedział_ostatnie
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  ) ) ) })
          
        }
        else if (input$waluta==TRUE) {
          dane_trzecia1[, przedział_ostatnie_e := ceiling((last_sent_order_value/4.5)/20)*20]
          dane_trzecia1[przedział_ostatnie_e >300, przedział_ostatnie_e := 300]
          dane_trzecia1[przedział_ostatnie_e == 0 , przedział_ostatnie_e := 20]
          dane_trzecia_bar_e <- dane_trzecia1[,.N,by = 'przedział_ostatnie_e']
          dane_trzecia_bar_e <- dane_trzecia_bar_e[order(przedział_ostatnie_e)]
          dane_trzecia_bar_e[,przedział_ostatnie_e := paste(przedział_ostatnie_e - 20, '€ -', przedział_ostatnie_e, '€')]
          dane_trzecia_bar_e[przedział_ostatnie_e == '280 € - 300 €', przedział_ostatnie_e := '280+ €']
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(dane_trzecia_bar_e, x = ~dane_trzecia_bar_e$przedział_ostatnie_e, y = ~dane_trzecia_bar_e$N, type = 'bar',
                    text = paste0(round(dane_trzecia_bar_e$N/nrow(dane_trzecia1),3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość ostatniego zamówienia: %{x} <br>Liczba konsumentów: ', format(dane_trzecia_bar_e$N, big.mark = ' ', big.interval = 3L),'<extra></extra>'))%>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości ostatniego zamówienia",
                  yanchor = "top",
                  pad = list(
                    t = 10 )),
                xaxis = list(
                  title = list(
                    text = 'Wartość ostatniego zamówienia'
                  ),
                  autorange = TRUE,
                  categoryorder = "array",
                  categoryarray = dane_trzecia_bar_e$przedział_ostatnie_e
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  ) ) ) })
          
        }
        
      } 
      else if (input$wykres2 == 'last_order_client_age_range') {
        if(input$waluta==FALSE) {
          dane_trzecia1[is.na(last_sent_order_value) & orders_sent == 1, last_sent_order_value := sent_orders_value]
          dane_trzecia1 <- dane_trzecia1[!is.na(last_sent_order_value),]
          dane_trzecia1[, przedział_ostatnie := ceiling(last_sent_order_value/100)*100]
          dane_trzecia1[przedział_ostatnie == 0, przedział_ostatnie := 100]
          dane_trzecia1[przedział_ostatnie >400, przedział_ostatnie := 400]
          
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_ostatnie,last_order_client_age_range)]
          kork <- dane_trzecia1[,.(ile = .N),by = last_order_client_age_range]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='last_order_client_age_range')
          dane_trzecia_bar$last_order_client_age_range <- factor(dane_trzecia_bar$last_order_client_age_range, levels = c('24-', '25-39', '40-54', '55-59', '60-69', '70+', 'unknown'))
          dane_trzecia_bar <- dane_trzecia_bar[order(dane_trzecia_bar$last_order_client_age_range, przedział_ostatnie)]
          dane_trzecia_bar[,przedział_ostatnie := paste(przedział_ostatnie - 100, 'zł -', przedział_ostatnie, 'zł')]
          dane_trzecia_bar[przedział_ostatnie == '300 zł - 400 zł', przedział_ostatnie := '300+ zł']
          remove(kork)
          remove(dane_trzecia1)
          list <- split(dane_trzecia_bar, by = 'przedział_ostatnie')
          
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~last_order_client_age_range, y = ~N, type = 'bar', name = '0 zł - 100 zł', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość ostatniego zamówienia: ', a$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Wiek konsumenta: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '100 zł - 200 zł', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', b$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '200 zł - 300 zł', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', c$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '300+ zł', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', d$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości ostatniego zamówienia i wieku",
                  yanchor = 'top',
                  pad = list(
                    t = 5
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Wiek konsumenta'
                  )
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
        } else {
          dane_trzecia1[is.na(last_sent_order_value) & orders_sent == 1, last_sent_order_value := sent_orders_value]
          dane_trzecia1 <- dane_trzecia1[!is.na(last_sent_order_value),]
          dane_trzecia1[, przedział_ostatnie := ceiling((last_sent_order_value/4.5)/25)*25]
          dane_trzecia1[przedział_ostatnie == 0, przedział_ostatnie := 25]
          dane_trzecia1[przedział_ostatnie >100, przedział_ostatnie := 100]
          
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_ostatnie,last_order_client_age_range)]
          kork <- dane_trzecia1[,.(ile = .N),by = last_order_client_age_range]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='last_order_client_age_range')
          dane_trzecia_bar$last_order_client_age_range <- factor(dane_trzecia_bar$last_order_client_age_range, levels = c('24-', '25-39', '40-54', '55-59', '60-69', '70+', 'unknown'))
          dane_trzecia_bar <- dane_trzecia_bar[order(dane_trzecia_bar$last_order_client_age_range, przedział_ostatnie)]
          dane_trzecia_bar[,przedział_ostatnie := paste(przedział_ostatnie - 25, '€ -', przedział_ostatnie, '€')]
          dane_trzecia_bar[przedział_ostatnie == '75 € - 100 €', przedział_ostatnie := '75+ €']
          remove(kork)
          remove(dane_trzecia1)
          list <- split(dane_trzecia_bar, by = 'przedział_ostatnie')
          
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~last_order_client_age_range, y = ~N, type = 'bar', name = '0 € - 25 €', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość ostatniego zamówienia: ', a$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Wiek konsumenta: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '25 € - 50 €', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', b$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '50 € - 75 €', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', c$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '75+ €', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', d$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości ostatniego zamówienia i wieku",
                  yanchor = 'top',
                  pad = list(
                    t = 5
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Wiek konsumenta'
                  )
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
        }

      } 
      else if (input$wykres2 == 'client_sex') {
        
        if(input$waluta==FALSE) {
          dane_trzecia1[is.na(last_sent_order_value) & orders_sent == 1, last_sent_order_value := sent_orders_value]
          dane_trzecia1 <- dane_trzecia1[!is.na(last_sent_order_value),]
          dane_trzecia1[, przedział_ostatnie := ceiling(last_sent_order_value/100)*100]
          dane_trzecia1[przedział_ostatnie == 0, przedział_ostatnie := 100]
          dane_trzecia1[przedział_ostatnie >400, przedział_ostatnie := 400]
          

          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_ostatnie,client_sex)]
          kork <- dane_trzecia1[,.(ile = .N),by = client_sex]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='client_sex')
          dane_trzecia_bar <- dane_trzecia_bar[order(dane_trzecia_bar$client_sex, przedział_ostatnie)]
          dane_trzecia_bar[,przedział_ostatnie := paste(przedział_ostatnie - 100, 'zł -', przedział_ostatnie, 'zł')]
          dane_trzecia_bar[przedział_ostatnie == '300 zł - 400 zł', przedział_ostatnie := '300+ zł']
          remove(kork)
          remove(dane_trzecia1)
          list <- split(dane_trzecia_bar, by = 'przedział_ostatnie')
          
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~client_sex, y = ~N, type = 'bar', name = '0 zł - 100 zł', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość ostatniego zamówienia: ', a$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Wiek konsumenta: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '100 zł - 200 zł', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', b$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '200 zł - 300 zł', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', c$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '300+ zł', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', d$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości ostatniego zamówienia i płci",
                  yanchor = "top",
                  pad = list(
                    t = 10
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Płeć konsumenta'
                  )
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
        } else {
          dane_trzecia1[is.na(last_sent_order_value) & orders_sent == 1, last_sent_order_value := sent_orders_value]
          dane_trzecia1 <- dane_trzecia1[!is.na(last_sent_order_value),]
          dane_trzecia1[, przedział_ostatnie := ceiling((last_sent_order_value/4.5)/25)*25]
          dane_trzecia1[przedział_ostatnie == 0, przedział_ostatnie := 25]
          dane_trzecia1[przedział_ostatnie >100, przedział_ostatnie := 100]
          
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_ostatnie,client_sex)]
          kork <- dane_trzecia1[,.(ile = .N),by = client_sex]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='client_sex')
          dane_trzecia_bar <- dane_trzecia_bar[order(dane_trzecia_bar$client_sex, przedział_ostatnie)]
          dane_trzecia_bar[,przedział_ostatnie := paste(przedział_ostatnie - 25, '€ -', przedział_ostatnie, '€')]
          dane_trzecia_bar[przedział_ostatnie == '75 € - 100 €', przedział_ostatnie := '75+ €']
          remove(kork)
          remove(dane_trzecia1)
          list <- split(dane_trzecia_bar, by = 'przedział_ostatnie')
          
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~client_sex, y = ~N, type = 'bar', name = '0 € - 25 €', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość ostatniego zamówienia: ', a$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Wiek konsumenta: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '25 € - 50 €', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', b$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '50 € - 75 €', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', c$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '75+ €', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', d$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości ostatniego zamówienia i płci",
                  yanchor = "top",
                  pad = list(
                    t = 10
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Płeć konsumenta'
                  )
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
        }
        
        
        
        

      } 
      else if (input$wykres2 == 'country') {
        
        if(input$waluta==FALSE) {
          dane_trzecia1[is.na(last_sent_order_value) & orders_sent == 1, last_sent_order_value := sent_orders_value]
          dane_trzecia1 <- dane_trzecia1[!is.na(last_sent_order_value),]
          dane_trzecia1[, przedział_ostatnie := ceiling(last_sent_order_value/100)*100]
          dane_trzecia1[przedział_ostatnie == 0, przedział_ostatnie := 100]
          dane_trzecia1[przedział_ostatnie >400, przedział_ostatnie := 400]
          
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_ostatnie,country)]
          orka <- data.table(country = c(rep(input$kraj2, 4)), przedział_ostatnie = c(rep(100, length(input$kraj2)), rep(200, length(input$kraj2)),
                                                                                      rep(300, length(input$kraj2)),rep(400, length(input$kraj2))))
          dane_trzecia_bar <- merge(orka, dane_trzecia_bar, by = c('country', 'przedział_ostatnie'), all.x = T)
          dane_trzecia_bar[is.na(N), N := 0]
          kork <- dane_trzecia1[,.(ile = .N),by = country]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='country')
          dane_trzecia_bar <- dane_trzecia_bar[order(-ile, country, przedział_ostatnie)]
          dane_trzecia_bar[,przedział_ostatnie := paste(przedział_ostatnie - 100, 'zł -', przedział_ostatnie, 'zł')]
          dane_trzecia_bar[przedział_ostatnie == '300 zł - 400 zł', przedział_ostatnie := '300+ zł']
          
          list <- split(dane_trzecia_bar, by = 'przedział_ostatnie')
          remove(kork)
          remove(dane_trzecia1)
          remove(orka)
          
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          

          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~country, y = ~N, type = 'bar', name = '0 zł - 100 zł', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość ostatniego zamówienia: ', a$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Kraj: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '100 zł - 200 zł', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', b$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kraj: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '200 zł - 300 zł', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', c$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kraj: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '300+ zł', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', d$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kraj: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości ostatniego zamówienia i kraju",
                  xref = 'paper',
                  x = 0.05,
                  yanchor = 'top',
                  pad = list(
                    t = 5
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Kraj konsumenta'
                  ),
                  autorange = TRUE,
                  categoryorder = "array",
                  categoryarray = dane_trzecia_bar$country
                ),
                yaxis = list(
                  title = list(       
                    
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
        }
        else {
          dane_trzecia1[is.na(last_sent_order_value) & orders_sent == 1, last_sent_order_value := sent_orders_value]
          dane_trzecia1 <- dane_trzecia1[!is.na(last_sent_order_value),]
          dane_trzecia1[, przedział_ostatnie := ceiling((last_sent_order_value/4.5)/25)*25]
          dane_trzecia1[przedział_ostatnie == 0, przedział_ostatnie := 25]
          dane_trzecia1[przedział_ostatnie >100, przedział_ostatnie := 100]
          
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_ostatnie,country)]
          orka <- data.table(country = c(rep(input$kraj2, 4)), przedział_ostatnie = c(rep(25, length(input$kraj2)), rep(50, length(input$kraj2)),
                                                                                      rep(75, length(input$kraj2)),rep(100, length(input$kraj2))))
          dane_trzecia_bar <- merge(orka, dane_trzecia_bar, by = c('country', 'przedział_ostatnie'), all.x = T)
          dane_trzecia_bar[is.na(N), N := 0]
          kork <- dane_trzecia1[,.(ile = .N),by = country]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='country')
          dane_trzecia_bar <- dane_trzecia_bar[order(-ile, country, przedział_ostatnie)]
          dane_trzecia_bar[,przedział_ostatnie := paste(przedział_ostatnie - 25, '€ -', przedział_ostatnie, '€')]
          dane_trzecia_bar[przedział_ostatnie == '75 € - 100 €', przedział_ostatnie := '75+ €']
          
          list <- split(dane_trzecia_bar, by = 'przedział_ostatnie')
          remove(kork)
          remove(dane_trzecia1)
          remove(orka)
          
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~country, y = ~N, type = 'bar', name = '0 € - 25 €', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość ostatniego zamówienia: ', a$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Kraj: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '25 € - 50 €', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', b$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kraj: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '50 € - 75 €', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', c$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kraj: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '75+ €', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', d$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kraj: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości ostatniego zamówienia i kraju",
                  xref = 'paper',
                  x = 0.05,
                  yanchor = 'top',
                  pad = list(
                    t = 5
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Kraj konsumenta'
                  ),
                  autorange = TRUE,
                  categoryorder = "array",
                  categoryarray = dane_trzecia_bar$country
                ),
                yaxis = list(
                  title = list(       
                    
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
        }
        
      } 
      else if (input$wykres2 == 'last_order_action_channel') {
        if(input$waluta==FALSE) {
          dane_trzecia1[is.na(last_sent_order_value) & orders_sent == 1, last_sent_order_value := sent_orders_value]
          dane_trzecia1 <- dane_trzecia1[!is.na(last_sent_order_value),]
          dane_trzecia1[, przedział_ostatnie := ceiling(last_sent_order_value/100)*100]
          dane_trzecia1[przedział_ostatnie == 0, przedział_ostatnie := 100]
          dane_trzecia1[przedział_ostatnie >400, przedział_ostatnie := 400]
          
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_ostatnie,last_order_action_channel)]
          orka <- data.table(last_order_action_channel = c(rep(input$kanal2, 4)), przedział_ostatnie = c(rep(100, length(input$kanal2)), rep(200, length(input$kanal2)),
                                                                                                         rep(300, length(input$kanal2)),rep(400, length(input$kanal2))))
          dane_trzecia_bar <- merge(orka, dane_trzecia_bar, by = c('last_order_action_channel', 'przedział_ostatnie'), all.x = T)
          dane_trzecia_bar[is.na(N), N := 0]
          kork <- dane_trzecia1[,.(ile = .N),by = last_order_action_channel]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='last_order_action_channel')
          dane_trzecia_bar <- dane_trzecia_bar[order(-ile,last_order_action_channel, przedział_ostatnie)]
          dane_trzecia_bar[,przedział_ostatnie := paste(przedział_ostatnie - 100, 'zł -', przedział_ostatnie,'zł')]
          dane_trzecia_bar[przedział_ostatnie == '300 zł - 400 zł', przedział_ostatnie := '300+ zł']
          #dane_trzecia_bar <- dane_trzecia_bar[ile >100]
          list <- split(dane_trzecia_bar, by = 'przedział_ostatnie')
          remove(kork)
          remove(dane_trzecia1)
          remove(orka)
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~last_order_action_channel, y = ~N, type = 'bar', name = '0 zł - 100 zł', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość ostatniego zamówienia: ', a$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Kanał reklamy: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '100 zł - 200 zł', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', b$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kanał reklamy: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '200 zł - 300 zł', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', c$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kanał reklamy: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '300+ zł', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', d$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kanał reklamy: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości ostatniego zamówienia i kanału reklamy",
                  xref = 'paper',
                  x = 0.05,
                  yanchor = 'top',
                  pad = list(
                    t = 5
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Kanał reklamy'
                  ),
                  autorange = TRUE,
                  categoryorder = "array",
                  categoryarray = dane_trzecia_bar$last_order_action_channel
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
          
        }  
        else {
          dane_trzecia1[is.na(last_sent_order_value) & orders_sent == 1, last_sent_order_value := sent_orders_value]
          dane_trzecia1 <- dane_trzecia1[!is.na(last_sent_order_value),]
          dane_trzecia1[, przedział_ostatnie := ceiling((last_sent_order_value/4.5)/25)*25]
          dane_trzecia1[przedział_ostatnie == 0, przedział_ostatnie := 25]
          dane_trzecia1[przedział_ostatnie >100, przedział_ostatnie := 100]
          
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_ostatnie,last_order_action_channel)]
          orka <- data.table(last_order_action_channel = c(rep(input$kanal2, 4)), przedział_ostatnie = c(rep(25, length(input$kanal2)), rep(50, length(input$kanal2)),
                                                                                                         rep(75, length(input$kanal2)),rep(100, length(input$kanal2))))
          dane_trzecia_bar <- merge(orka, dane_trzecia_bar, by = c('last_order_action_channel', 'przedział_ostatnie'), all.x = T)
          dane_trzecia_bar[is.na(N), N := 0]
          kork <- dane_trzecia1[,.(ile = .N),by = last_order_action_channel]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='last_order_action_channel')
          dane_trzecia_bar <- dane_trzecia_bar[order(-ile,last_order_action_channel, przedział_ostatnie)]
          dane_trzecia_bar[,przedział_ostatnie := paste(przedział_ostatnie - 25, '€ -', przedział_ostatnie,'€')]
          dane_trzecia_bar[przedział_ostatnie == '75 € - 100 €', przedział_ostatnie := '75+ €']
          #dane_trzecia_bar <- dane_trzecia_bar[ile >100]
          list <- split(dane_trzecia_bar, by = 'przedział_ostatnie')
          remove(kork)
          remove(dane_trzecia1)
          remove(orka)
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~last_order_action_channel, y = ~N, type = 'bar', name = '0 € - 25 €', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość ostatniego zamówienia: ', a$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Kanał reklamy: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '25 € - 50 €', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', b$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kanał reklamy: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '50 € - 75 €', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', c$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kanał reklamy: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '75+ €', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość ostatniego zamówienia: ', d$przedział_ostatnie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kanał reklamy: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości ostatniego zamówienia i kanału reklamy",
                  xref = 'paper',
                  x = 0.05,
                  yanchor = 'top',
                  pad = list(
                    t = 5
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Kanał reklamy'
                  ),
                  autorange = TRUE,
                  categoryorder = "array",
                  categoryarray = dane_trzecia_bar$last_order_action_channel
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
          
        }
      }
      
      
      
      #else#### 
    } 
    else {
      
      dane_trzecia1 <- dane_trzecia1[!is.na(sent_orders_value),]
      
      
      dane_trzecia1[,Total:='Total']
      
      slow<-data.table(c('country','client_sex','last_order_client_age_range','last_order_action_channel','Total'),
                       c('Kraj','Płeć konsumenta','Kanał ostatniego zamówienia','Wiek konsumenta','Total'))
      
      
      
      dane_trzecia_tab <- dane_trzecia1[, lapply(.SD, mean, na.rm=TRUE), by = c(input$agregat22,input$agregat23), .SDcols=c("sent_orders_value") ]
      dane_trzecia_tab[,drugie := input$agregat23]
      sort <- dane_trzecia1[,.N,by=c(input$agregat22)]
      sort1 <- dane_trzecia1[,.(N1 = .N), by=c(input$agregat23)]
      dane_trzecia_tab <- merge(dane_trzecia_tab, sort1, by = c(input$agregat23), all.x = T)
      dane_trzecia_tab <- merge(dane_trzecia_tab, sort, by = c(input$agregat22), all.x = T)
      dane_trzecia_tab <- dane_trzecia_tab[order(-N,-N1)]
      dane_trzecia_tab <- dane_trzecia_tab[,c(1,2,3)]
      kolumna <- slow[V1==input$agregat22,V2]
      kolumna1 <- slow[V1==input$agregat23,V2]  
      
      
      dane_trzecia_tab$sent_orders_value <- round(dane_trzecia_tab$sent_orders_value)
      
      names(dane_trzecia_tab) <- c('kolumna', 'kolumna1', 'N')
      
      if ((input$agregat22 != input$agregat23) & input$agregat23 != 'Total') {
        
        for (i in nrow(dane_trzecia_tab):1) {
          if (dane_trzecia_tab$kolumna[i] %in% dane_trzecia_tab$kolumna[-i]) {
            dane_trzecia_tab$kolumna[i] <- paste0('<span style="font-size: 0%">', dane_trzecia_tab$kolumna[i], '</span>')
          }
        }
        
      } else {
        dane_trzecia_tab$kolumna1 <- ''
      }
      
      if(input$waluta==FALSE) {      
        output$tabela_trzecia <- DT::renderDataTable({
        datatable(
          dane_trzecia_tab,
          filter = 'bottom',
          rownames = FALSE,
          colnames = c(kolumna, kolumna1, 'Średnia wartość wszystkich zamówień konsumenta'),
          escape = FALSE,
          options = list(
            lenghtMenu = c(10, 15, 20, 50, 100),
            paging = T,
            fixedHeader=TRUE,
            scroller = T,
            scrollx=T,
            scrolly=F,
            pageLength = 15,
            autoWidth = F,
            dom ='t<"bottom"lp><"clear">',
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#2276b3', 'color': '#fff'});",
              "}")
          )) %>% formatCurrency(3, ' zł', digits = 0, before = FALSE)
        
      })
      }
      else {
        dane_trzecia_tab[,N:=N/4.5]
        output$tabela_trzecia <- DT::renderDataTable({
          datatable(
            dane_trzecia_tab,
            filter = 'bottom',
            rownames = FALSE,
            colnames = c(kolumna, kolumna1, 'Średnia wartość wszystkich zamówień konsumenta'),
            escape = FALSE,
            options = list(
              lenghtMenu = c(10, 15, 20, 50, 100),
              paging = T,
              fixedHeader=TRUE,
              scroller = T,
              scrollx=T,
              scrolly=F,
              pageLength = 15,
              autoWidth = F,
              dom ='t<"bottom"lp><"clear">',
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#2276b3', 'color': '#fff'});",
                "}")
            )) %>% formatCurrency(3, ' €', digits = 0, before = FALSE)
          
        })
        
      }
      
      
      
      if (input$wykres2 == 'Total') {
        if(input$waluta==FALSE) { 
          browser()
        dane_trzecia1 <- dane_trzecia1[, przedział_wszystkie := ceiling(sent_orders_value/100)*100]
        dane_trzecia1 <- dane_trzecia1[przedział_wszystkie == 0, przedział_wszystkie := 100]
        dane_trzecia1 <- dane_trzecia1[przedział_wszystkie >1700, przedział_wszystkie := 1700]
        dane_trzecia_bar <- dane_trzecia1[,.N,by = 'przedział_wszystkie']
        dane_trzecia_bar <- dane_trzecia_bar[order(przedział_wszystkie)]
        dane_trzecia_bar <- dane_trzecia_bar[,przedział_wszystkie := paste(przedział_wszystkie - 100, 'zł -', przedział_wszystkie, 'zł')]
        dane_trzecia_bar <- dane_trzecia_bar[przedział_wszystkie == '1600 zł - 1700 zł', przedział_wszystkie := '1600+ zł']
        
        remove(kork)
        remove(orka)
        output$Plotbar2 <- renderPlotly({
          
          plot_ly(dane_trzecia_bar, x = ~dane_trzecia_bar$przedział_wszystkie, y = ~dane_trzecia_bar$N, type = 'bar',
                  text = paste0(round(dane_trzecia_bar$N/nrow(dane_trzecia1),3)*100, '%'), textposition = 'outside',
                  hovertemplate = paste0('Wartość wszystkich zamówień: %{x} <br>Liczba konsumentów: ', format(dane_trzecia_bar$N, big.mark = ' ', big.interval = 3L),'<extra></extra>'))%>%
            layout(
              title = list(
                text = "Liczba konsumentów według wartości wszystkich zamówień",
                yanchor = "top",
                pad = list(
                  t = 10
                )
              ),
              xaxis = list(
                title = list(
                  text = 'Wartość wszystkich zamówień'
                ),
                autorange = TRUE,
                categoryorder = "array",
                categoryarray = dane_trzecia_bar$przedział_wszystkie
              ),
              yaxis = list(
                title = list(
                  text = "Liczba konsumentów"
                )
              )
            )
          
        })
        }
        else {
          dane_trzecia1 <- dane_trzecia1[, przedział_wszystkie := ceiling((sent_orders_value/4.5)/25)*25]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie == 0, przedział_wszystkie := 25]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie >425, przedział_wszystkie := 425]
          dane_trzecia_bar <- dane_trzecia1[,.N,by = 'przedział_wszystkie']
          dane_trzecia_bar <- dane_trzecia_bar[order(przedział_wszystkie)]
          dane_trzecia_bar <- dane_trzecia_bar[,przedział_wszystkie := paste(przedział_wszystkie - 25, '€ -', przedział_wszystkie, '€')]
          dane_trzecia_bar <- dane_trzecia_bar[przedział_wszystkie == '400 € - 425 €', przedział_wszystkie := '400+ €']
          
          remove(kork)
          remove(orka)
          output$Plotbar2 <- renderPlotly({
            
            plot_ly(dane_trzecia_bar, x = ~dane_trzecia_bar$przedział_wszystkie, y = ~dane_trzecia_bar$N, type = 'bar',
                    text = paste0(round(dane_trzecia_bar$N/nrow(dane_trzecia1),3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość wszystkich zamówień: %{x} <br>Liczba konsumentów: ', format(dane_trzecia_bar$N, big.mark = ' ', big.interval = 3L),'<extra></extra>'))%>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości wszystkich zamówień",
                  yanchor = "top",
                  pad = list(
                    t = 10
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Wartość wszystkich zamówień'
                  ),
                  autorange = TRUE,
                  categoryorder = "array",
                  categoryarray = dane_trzecia_bar$przedział_wszystkie
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                )
              )
            
          })
          
        }
        
        
      } else if (input$wykres2 == 'last_order_client_age_range') {
        if(input$waluta==FALSE) { 
          dane_trzecia1 <- dane_trzecia1[!is.na(sent_orders_value),]
          dane_trzecia1 <- dane_trzecia1[, przedział_wszystkie := ceiling(sent_orders_value/200)*200]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie == 0, przedział_wszystkie := 200]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie >800, przedział_wszystkie := 800]
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_wszystkie,last_order_client_age_range)]
          kork <- dane_trzecia1[,.(ile = .N),by = last_order_client_age_range]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='last_order_client_age_range')
          dane_trzecia_bar$last_order_client_age_range <- factor(dane_trzecia_bar$last_order_client_age_range, levels = c('24-', '25-39', '40-54', '55-59', '60-69', '70+', 'unknown'))
          dane_trzecia_bar <- dane_trzecia_bar[order(dane_trzecia_bar$last_order_client_age_range, przedział_wszystkie)]
          dane_trzecia_bar <- dane_trzecia_bar[,przedział_wszystkie := paste(przedział_wszystkie - 200, 'zł -', przedział_wszystkie, 'zł')]
          dane_trzecia_bar <- dane_trzecia_bar[przedział_wszystkie == '600 zł - 800 zł', przedział_wszystkie := '600+ zł']
          remove(kork)
          remove(dane_trzecia1)
          remove(orka)
          list <- split(dane_trzecia_bar, by = 'przedział_wszystkie')
          
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~last_order_client_age_range, y = ~N, type = 'bar', name = '0 zł - 200 zł', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość wszystkich zamówień: ', a$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Wiek konsumenta: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '200 zł - 400 zł', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', b$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '400 zł - 600 zł', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', c$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '600+ zł', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', d$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości wszystkich zamówień i wieku",
                  yanchor = "top",
                  pad = list(
                    t = 10
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Wiek konsumenta'
                  )
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
        }
        else {
          dane_trzecia1 <- dane_trzecia1[!is.na(sent_orders_value),]
          dane_trzecia1 <- dane_trzecia1[, przedział_wszystkie := ceiling((sent_orders_value/4.5)/50)*50]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie == 0, przedział_wszystkie := 50]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie >200, przedział_wszystkie := 200]
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_wszystkie,last_order_client_age_range)]
          kork <- dane_trzecia1[,.(ile = .N),by = last_order_client_age_range]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='last_order_client_age_range')
          dane_trzecia_bar$last_order_client_age_range <- factor(dane_trzecia_bar$last_order_client_age_range, levels = c('24-', '25-39', '40-54', '55-59', '60-69', '70+', 'unknown'))
          dane_trzecia_bar <- dane_trzecia_bar[order(dane_trzecia_bar$last_order_client_age_range, przedział_wszystkie)]
          dane_trzecia_bar <- dane_trzecia_bar[,przedział_wszystkie := paste(przedział_wszystkie - 50, '€ -', przedział_wszystkie, '€')]
          dane_trzecia_bar <- dane_trzecia_bar[przedział_wszystkie == '150 € - 200 €', przedział_wszystkie := '150+ €']
          remove(kork)
          remove(dane_trzecia1)
          remove(orka)
          list <- split(dane_trzecia_bar, by = 'przedział_wszystkie')
          
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~last_order_client_age_range, y = ~N, type = 'bar', name = '0 € - 50 €', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość wszystkich zamówień: ', a$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Wiek konsumenta: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '50 € - 100 €', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', b$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '100 € - 150 €', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', c$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '150+ €', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', d$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Wiek konsumenta: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości wszystkich zamówień i wieku",
                  yanchor = "top",
                  pad = list(
                    t = 10
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Wiek konsumenta'
                  )
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
        }
        
      } 
      else if (input$wykres2 == 'client_sex') {
        if(input$waluta==FALSE) { 
          dane_trzecia1 <- dane_trzecia1[!is.na(sent_orders_value),]
          dane_trzecia1 <- dane_trzecia1[, przedział_wszystkie := ceiling(sent_orders_value/200)*200]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie == 0, przedział_wszystkie := 200]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie >800, przedział_wszystkie := 800]
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_wszystkie,client_sex)]
          kork <- dane_trzecia1[,.(ile = .N),by = client_sex]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='client_sex')
          dane_trzecia_bar <- dane_trzecia_bar[order(-ile, client_sex, przedział_wszystkie)]
          dane_trzecia_bar <- dane_trzecia_bar[,przedział_wszystkie := paste(przedział_wszystkie - 200, 'zł -', przedział_wszystkie, 'zł')]
          dane_trzecia_bar <- dane_trzecia_bar[przedział_wszystkie == '600 zł - 800 zł', przedział_wszystkie := '600+ zł']
          remove(kork)
          remove(dane_trzecia1)
          remove(orka)
          list <- split(dane_trzecia_bar, by = 'przedział_wszystkie')
          
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~client_sex, y = ~N, type = 'bar', name = '0 zł - 200 zł', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość wszystkich zamówień: ', a$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Płeć konsumenta: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '200 zł - 400 zł', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', b$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Płeć konsumenta: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '400 zł - 600 zł', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', c$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Płeć konsumenta: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '600+ zł', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', d$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Płeć konsumenta: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości wszystkich zamówień i płci",
                  yanchor = "top",
                  pad = list(
                    t = 10
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Płeć konsumenta'
                  )
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
          
          }
        else {
          dane_trzecia1 <- dane_trzecia1[!is.na(sent_orders_value),]
          dane_trzecia1 <- dane_trzecia1[, przedział_wszystkie := ceiling((sent_orders_value/4.5)/50)*50]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie == 0, przedział_wszystkie := 50]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie >200, przedział_wszystkie := 200]
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_wszystkie,client_sex)]
          kork <- dane_trzecia1[,.(ile = .N),by = client_sex]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='client_sex')
          dane_trzecia_bar <- dane_trzecia_bar[order(-ile, client_sex, przedział_wszystkie)]
          dane_trzecia_bar <- dane_trzecia_bar[,przedział_wszystkie := paste(przedział_wszystkie - 50, '€ -', przedział_wszystkie, '€')]
          dane_trzecia_bar <- dane_trzecia_bar[przedział_wszystkie == '150 € - 200 €', przedział_wszystkie := '150+ €']
          remove(kork)
          remove(dane_trzecia1)
          remove(orka)
          list <- split(dane_trzecia_bar, by = 'przedział_wszystkie')
          
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~client_sex, y = ~N, type = 'bar', name = '0 € - 50 €', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość wszystkich zamówień: ', a$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Płeć konsumenta: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '50 € - 100 €', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', b$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Płeć konsumenta: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '100 € - 150 €', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', c$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Płeć konsumenta: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '150+ €', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', d$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Płeć konsumenta: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości wszystkich zamówień i płci",
                  yanchor = "top",
                  pad = list(
                    t = 10
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Płeć konsumenta'
                  )
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
          
        }
      } 
      else if (input$wykres2 == 'country') {
        if(input$waluta==FALSE) {
          dane_trzecia1 <- dane_trzecia1[!is.na(sent_orders_value),]
          dane_trzecia1 <- dane_trzecia1[, przedział_wszystkie := ceiling(sent_orders_value/200)*200]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie == 0, przedział_wszystkie := 200]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie >800, przedział_wszystkie := 800]
          
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_wszystkie,country)]
          orka <- data.table(country = c(rep(input$kraj2, 4)), przedział_wszystkie = c(rep(200, length(input$kraj2)), rep(400, length(input$kraj2)),
                                                                                       rep(600, length(input$kraj2)),rep(800, length(input$kraj2))))
          dane_trzecia_bar <- merge(orka, dane_trzecia_bar, by = c('country', 'przedział_wszystkie'), all.x = T)
          dane_trzecia_bar[is.na(N), N := 0]
          kork <- dane_trzecia1[,.(ile = .N),by = country]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='country')
          dane_trzecia_bar <- dane_trzecia_bar[order(-ile, country, przedział_wszystkie)]
          dane_trzecia_bar <- dane_trzecia_bar[,przedział_wszystkie := paste(przedział_wszystkie - 200, 'zł -', przedział_wszystkie, 'zł')]
          dane_trzecia_bar <- dane_trzecia_bar[przedział_wszystkie == '600 zł - 800 zł', przedział_wszystkie := '600+ zł']
          #dane_trzecia_bar <- dane_trzecia_bar[ile >100]
          list <- split(dane_trzecia_bar, by = 'przedział_wszystkie')
          remove(kork)
          remove(dane_trzecia1)
          remove(orka)
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~country, y = ~N, type = 'bar', name = '0 zł - 200 zł', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość wszystkich zamówień: ', a$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Kraj: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '200 zł - 400 zł', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', b$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kraj: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '400 zł - 600 zł', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', c$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kraj: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '600+ zł', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', d$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kraj: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości wszystkich zamówień i kraju",
                  xref = 'paper',
                  x = 0.05,
                  yanchor = 'top',
                  pad = list(
                    t = 5
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Kraj konsumenta'
                  ),
                  autorange = TRUE,
                  categoryorder = "array",
                  categoryarray = dane_trzecia_bar$country
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
          
        }
        else {
          dane_trzecia1 <- dane_trzecia1[!is.na(sent_orders_value),]
          dane_trzecia1 <- dane_trzecia1[, przedział_wszystkie := ceiling((sent_orders_value/4.5)/50)*50]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie == 0, przedział_wszystkie := 50]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie >200, przedział_wszystkie := 200]
          
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_wszystkie,country)]
          orka <- data.table(country = c(rep(input$kraj2, 4)), przedział_wszystkie = c(rep(50, length(input$kraj2)), rep(100, length(input$kraj2)),
                                                                                       rep(150, length(input$kraj2)),rep(200, length(input$kraj2))))
          dane_trzecia_bar <- merge(orka, dane_trzecia_bar, by = c('country', 'przedział_wszystkie'), all.x = T)
          dane_trzecia_bar[is.na(N), N := 0]
          kork <- dane_trzecia1[,.(ile = .N),by = country]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='country')
          dane_trzecia_bar <- dane_trzecia_bar[order(-ile, country, przedział_wszystkie)]
          dane_trzecia_bar <- dane_trzecia_bar[,przedział_wszystkie := paste(przedział_wszystkie - 50, '€ -', przedział_wszystkie, '€')]
          dane_trzecia_bar <- dane_trzecia_bar[przedział_wszystkie == '150 € - 200 €', przedział_wszystkie := '150+ €']
          #dane_trzecia_bar <- dane_trzecia_bar[ile >100]
          list <- split(dane_trzecia_bar, by = 'przedział_wszystkie')
          remove(kork)
          remove(dane_trzecia1)
          remove(orka)
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~country, y = ~N, type = 'bar', name = '0 € - 50 €', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość wszystkich zamówień: ', a$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Kraj: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '50 € - 100 €', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', b$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kraj: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '100 € - 150 €', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', c$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kraj: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '150+ €', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', d$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kraj: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości wszystkich zamówień i kraju",
                  xref = 'paper',
                  x = 0.05,
                  yanchor = 'top',
                  pad = list(
                    t = 5
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Kraj konsumenta'
                  ),
                  autorange = TRUE,
                  categoryorder = "array",
                  categoryarray = dane_trzecia_bar$country
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
          
          
        }
      } 
      else if (input$wykres2 == 'last_order_action_channel') {
        if(input$waluta==FALSE) {
          dane_trzecia1 <- dane_trzecia1[!is.na(sent_orders_value),]
          dane_trzecia1 <- dane_trzecia1[, przedział_wszystkie := ceiling(sent_orders_value/200)*200]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie == 0, przedział_wszystkie := 200]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie >800, przedział_wszystkie := 800]
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_wszystkie,last_order_action_channel)]
          orka <- data.table(last_order_action_channel = c(rep(input$kanal2, 4)), przedział_wszystkie = c(rep(200, length(input$kanal2)), rep(400, length(input$kanal2)),
                                                                                                          rep(600, length(input$kanal2)),rep(800, length(input$kanal2))))
          dane_trzecia_bar <- merge(orka, dane_trzecia_bar, by = c('last_order_action_channel', 'przedział_wszystkie'), all.x = T)
          dane_trzecia_bar[is.na(N), N := 0]
          kork <- dane_trzecia1[,.(ile = .N),by = last_order_action_channel]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='last_order_action_channel')
          dane_trzecia_bar <- dane_trzecia_bar[order(-ile, last_order_action_channel, przedział_wszystkie)]
          dane_trzecia_bar <- dane_trzecia_bar[,przedział_wszystkie := paste(przedział_wszystkie - 200, 'zł -', przedział_wszystkie, 'zł')]
          dane_trzecia_bar <- dane_trzecia_bar[przedział_wszystkie == '600 zł - 800 zł', przedział_wszystkie := '600+ zł']
          #dane_trzecia_bar <- dane_trzecia_bar[ile >100]
          list <- split(dane_trzecia_bar, by = 'przedział_wszystkie')
          
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          remove(kork)
          remove(dane_trzecia1)
          remove(orka)
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~last_order_action_channel, y = ~N, type = 'bar', name = '0 zł - 200 zł', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość wszystkich zamówień: ', a$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Kanał reklamy: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '200 zł - 400 zł', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', b$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kanał reklamy: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '400 zł - 600 zł', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', c$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kanał reklamy: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '600+ zł', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', d$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kanał reklamy: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości wszystkich zamówień i kanału reklamy",
                  xref = 'paper',
                  x = 0.05,
                  yanchor = 'top',
                  pad = list(
                    t = 5
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Kanał reklamy'
                  ),
                  autorange = TRUE,
                  categoryorder = "array",
                  categoryarray = dane_trzecia_bar$last_order_action_channel
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
          
        }
        else {
          dane_trzecia1 <- dane_trzecia1[!is.na(sent_orders_value),]
          dane_trzecia1 <- dane_trzecia1[, przedział_wszystkie := ceiling((sent_orders_value/4.5)/50)*50]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie == 0, przedział_wszystkie := 50]
          dane_trzecia1 <- dane_trzecia1[przedział_wszystkie >200, przedział_wszystkie := 200]
          
          dane_trzecia_bar <- dane_trzecia1[,.N,by = list(przedział_wszystkie,last_order_action_channel)]
          orka <- data.table(last_order_action_channel = c(rep(input$kanal2, 4)), przedział_wszystkie = c(rep(50, length(input$kanal2)), rep(100, length(input$kanal2)),
                                                                                                          rep(150, length(input$kanal2)),rep(200, length(input$kanal2))))
          dane_trzecia_bar <- merge(orka, dane_trzecia_bar, by = c('last_order_action_channel', 'przedział_wszystkie'), all.x = T)
          dane_trzecia_bar[is.na(N), N := 0]
          kork <- dane_trzecia1[,.(ile = .N),by = last_order_action_channel]
          dane_trzecia_bar <- merge(dane_trzecia_bar, kork, by='last_order_action_channel')
          dane_trzecia_bar <- dane_trzecia_bar[order(-ile, last_order_action_channel, przedział_wszystkie)]
          dane_trzecia_bar <- dane_trzecia_bar[,przedział_wszystkie := paste(przedział_wszystkie - 50, '€ -', przedział_wszystkie, '€')]
          dane_trzecia_bar <- dane_trzecia_bar[przedział_wszystkie == '150 € - 200 €', przedział_wszystkie := '150+ €']
          #dane_trzecia_bar <- dane_trzecia_bar[ile >100]
          list <- split(dane_trzecia_bar, by = 'przedział_wszystkie')
          
          a <- list[[1]]
          b <- list[[2]]
          c <- list[[3]]
          d <- list[[4]]
          remove(kork)
          remove(dane_trzecia1)
          remove(orka)
          
          output$Plotbar2 <- renderPlotly({
            plot_ly(a, x = ~last_order_action_channel, y = ~N, type = 'bar', name = '0 € - 50 €', marker = list(color = '#7B3294'),
                    text = paste0(round(a$N/a$ile,3)*100, '%'), textposition = 'outside',
                    hovertemplate = paste0('Wartość wszystkich zamówień: ', a$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                           format(a$N, big.mark = ' ', big.interval = 3L),
                                           '<br>Kanał reklamy: %{x}<extra></extra>'),
                    width = (((nrow(dane_trzecia_bar)*520))^(0.8))+450) %>%
              add_trace(b, y = b$N, name = '50 € - 100 €', marker = list(color = '#C2A5CF'),
                        text = paste0(round(b$N/b$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', b$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(b$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kanał reklamy: %{x}<extra></extra>')) %>%
              add_trace(c, y = c$N, name = '100 € - 150 €', marker = list(color = "#A6DBA0"),
                        text = paste0(round(c$N/c$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', c$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(c$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kanał reklamy: %{x}<extra></extra>')) %>%
              add_trace(d, y = d$N, name = '150+ €', marker = list(color = "#008837"),
                        text = paste0(round(d$N/d$ile,3)*100, '%'), textposition = 'outside',
                        hovertemplate = paste0('Wartość wszystkich zamówień: ', d$przedział_wszystkie, '<br>Liczba konsumentów: ', 
                                               format(d$N, big.mark = ' ', big.interval = 3L),
                                               '<br>Kanał reklamy: %{x}<extra></extra>')) %>%
              layout(
                title = list(
                  text = "Liczba konsumentów według wartości wszystkich zamówień i kanału reklamy",
                  xref = 'paper',
                  x = 0.05,
                  yanchor = 'top',
                  pad = list(
                    t = 5
                  )
                ),
                xaxis = list(
                  title = list(
                    text = 'Kanał reklamy'
                  ),
                  autorange = TRUE,
                  categoryorder = "array",
                  categoryarray = dane_trzecia_bar$last_order_action_channel
                ),
                yaxis = list(
                  title = list(
                    text = "Liczba konsumentów"
                  )
                ),
                legend = list(orientation = 'h')
              )
          })
          
        }
      }
      
    }
    
  } else {
    output$tabela_trzecia <- NULL
    output$Plotbar2 <- renderPlotly({
      plot_ly(
        type = "sankey",
        domain = list(
          x =  c(0,1),
          y =  c(0,1)
        ),
        orientation = "h",
        valueformat = ".0f",
        arrangement = "freeform",
        
        textfont = list(
          size = "automatic"  
        )
      )%>%
        add_annotations(
          x = 0.5,
          y = 0.95,
          text = "Za mało konsumentów w bazie - wybierz inne ograniczenia.",
          font = list(
            size = 30,
            color = 'firebrick2'
          ),
          showarrow = FALSE
        )
    })
  }
  
  shinyjs::hide(selector = '.progress-group:has(#postep2)') 
  updateProgressBar(session = session, id = "postep2", value = 0, title = "Pasek postępu")
  
})
