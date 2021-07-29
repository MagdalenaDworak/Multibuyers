#####drugakarta####


hide(id='legenda')
hide(id='legenda2')
show(id='legenda1')

#shinyjs::click("zastosuj1")
observeEvent(input$zastosuj1,{
 
  shinyjs::show(selector = '.progress-group:has(#postep1)')
  updateProgressBar(session = session, id = "postep1", value = 0, title = "Pasek postępu")
  
  data_drugie <- data[czy_bezposrednia == 'tak' & products_group.x_1 %in% input$grupax1 & products_group.y_1 %in% input$grupay1 & 
                        campaign_product_group.x %in% input$kampaniax1 & campaign_product_group.y %in% input$kampaniay1 & action_channel.x %in% input$kanalx1
                      & action_channel.y %in% input$kanaly1 & cross %in% input$cross1 & program.x %in% input$programx1 & program.y %in% input$programy1
                      & client_sex %in% input$plec1 & last_order_client_age_range %in% input$wiek1 & language_code %in% input$kraj1,]
  
  if (!is.null(input$data1)) {
    data_drugie <- data_drugie[substr(added_datetime.x,1,7) >= substr(input$data1[1],1,7) &
                                 substr(added_datetime.x,1,7) <= substr(input$data1[length(input$data1)],1,7)]
  }
  
  updateProgressBar(session = session, id = "postep1", value = 30, title = "Przetwarzanie danych")
  
  if (nrow(data_drugie)>1) {
    data_line <- data_drugie[, dni := ceiling(roznica_pp/10)*10]
    data_line1 <- data_line[, .N, by = dni]
    data_line1 <- data_line1[order(dni)]
    data_line1 <- data_line1[,N := cumsum(N)]
    data_line1 <- data_line1[, proc := N/nrow(data_line)]
    data_line1[, text := paste0('Do ',dni,' dni ', 'powstało ', format(data_line1$N, big.mark = ' ', big.interval = 3L), ' zamówień.', '<br>Udział: ', round(N/nrow(data_line), 2)*100, '%')]
    
    
    
    data_bar <- data_drugie[, dni := ceiling(roznica_pp/20)*20]
    data_bar <- data_bar[dni >=620, dni := 620]
    data_bar <- data_bar[dni < 20, dni := 20]
    data_bar1 <- data_bar[, .N, by = dni]
    data_bar1 <- data_bar1[order(dni)]
    data_bar1[, dni := paste(dni-20, '-', dni)]
    data_bar1[dni == '600 - 620', dni := '600+']
    
    
    data_tabela <- data_drugie[czy_bezposrednia == 'tak']
    
    
    if (input$agregat11 == 'Kraj' & input$agregat12 == 'Total'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = language_code]
      data_tabela1 < data_tabela1[,drugie := 'Total']
      sort <- data_tabela[,.N,by=language_code]
      data_tabela1 <- merge(data_tabela1, sort, by = 'language_code', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Kraj'
      nazwa_kolumny1 <- 'Total'
    }
    
    if (input$agregat11 == 'Płeć konsumenta' & input$agregat12 == 'Total'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = client_sex]
      data_tabela1 < data_tabela1[,drugie := 'Total']
      sort <- data_tabela[,.N,by=client_sex]
      data_tabela1 <- merge(data_tabela1, sort, by = 'client_sex', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Płeć konsumenta'
      nazwa_kolumny1 <- 'Total'
    } 
    
    if (input$agregat11 == 'Wiek konsumenta' & input$agregat12 == 'Total'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = last_order_client_age_range]
      data_tabela1 < data_tabela1[,drugie := 'Total']
      sort <- data_tabela[,.N,by=last_order_client_age_range]
      data_tabela1 <- merge(data_tabela1, sort, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Wiek konsumenta'
      nazwa_kolumny1 <- 'Total'
    }
    
    if (input$agregat11 == "Grupa produktowa zamówienia X" & input$agregat12 == 'Total'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = products_group.x]
      data_tabela1 < data_tabela1[,drugie := 'Total']
      sort <- data_tabela[,.N,by=products_group.x]
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- "Grupa produktowa zamówienia X"
      nazwa_kolumny1 <- 'Total'
    } 
    
    if (input$agregat11 == "Grupa produktowa zamówienia Y" & input$agregat12 == 'Total'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = products_group.y]
      data_tabela1 < data_tabela1[,drugie := 'Total']
      sort <- data_tabela[,.N,by=products_group.y]
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- "Grupa produktowa zamówienia Y"
      nazwa_kolumny1 <- 'Total'
    } 
    
    if (input$agregat11 == 'Kanał reklamy X' & input$agregat12 == 'Total'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = action_channel.x]
      data_tabela1 < data_tabela1[,drugie := 'Total']
      sort <- data_tabela[,.N,by=action_channel.x]
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Kanał reklamy X'
      nazwa_kolumny1 <- 'Total'
    } 
    
    if (input$agregat11 == 'Kanał reklamy Y' & input$agregat12 == 'Total'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = action_channel.y]
      data_tabela1 < data_tabela1[,drugie := 'Total']
      sort <- data_tabela[,.N,by=action_channel.y]
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Kanał reklamy Y'
      nazwa_kolumny1 <- 'Total'
    } 
    
    if (input$agregat11 == 'Program zamówienia X' & input$agregat12 == 'Total'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = program.x]
      data_tabela1 < data_tabela1[,drugie := 'Total']
      sort <- data_tabela[,.N,by=program.x]
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Program zamówienia X'
      nazwa_kolumny1 <- 'Total'
    } 
    
    if (input$agregat11 == 'Program zamówienia Y' & input$agregat12 == 'Total'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = program.y]
      data_tabela1 < data_tabela1[,drugie := 'Total']
      sort <- data_tabela[,.N,by=program.y]
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Program zamówienia Y'
      nazwa_kolumny1 <- 'Total'
    } 
    
    
    
    
    
    
    if (input$agregat11 == 'Kraj' & input$agregat12 == 'Kraj'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = language_code]
      data_tabela1 < data_tabela1[,drugie := language_code]
      sort <- data_tabela[,.N,by=language_code]
      data_tabela1 <- merge(data_tabela1, sort, by = 'language_code', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Kraj'
      nazwa_kolumny1 <- 'Kraj'
    }
    
    
    if (input$agregat11 == 'Płeć konsumenta' & input$agregat12 == 'Kraj'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(client_sex,language_code), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=client_sex]
      sort1 <- data_tabela[,.(N1 = .N), by=language_code]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'language_code', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'client_sex', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Płeć konsumenta'
      nazwa_kolumny1 <- 'Kraj'
    } 
    
    if (input$agregat11 == 'Wiek konsumenta' & input$agregat12 == 'Kraj'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(last_order_client_age_range,language_code), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=last_order_client_age_range]
      sort1 <- data_tabela[,.(N1 = .N), by=language_code]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'language_code', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Wiek konsumenta'
      nazwa_kolumny1 <- 'Kraj'
    }
    
    if (input$agregat11 == "Grupa produktowa zamówienia X" & input$agregat12 == 'Kraj'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.x,language_code), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.x]
      sort1 <- data_tabela[,.(N1 = .N), by=language_code]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'language_code', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia X'
      nazwa_kolumny1 <- 'Kraj'
    } 
    
    if (input$agregat11 == "Grupa produktowa zamówienia Y" & input$agregat12 == 'Kraj'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.y,language_code), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.y]
      sort1 <- data_tabela[,.(N1 = .N), by=language_code]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'language_code', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia Y'
      nazwa_kolumny1 <- 'Kraj'
    } 
    
    if (input$agregat11 == 'Kanał reklamy X' & input$agregat12 == 'Kraj'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.x,language_code), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.x]
      sort1 <- data_tabela[,.(N1 = .N), by=language_code]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'language_code', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy X'
      nazwa_kolumny1 <- 'Kraj'
    } 
    
    if (input$agregat11 == 'Kanał reklamy Y' & input$agregat12 == 'Kraj'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.y,language_code), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.y]
      sort1 <- data_tabela[,.(N1 = .N), by=language_code]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'language_code', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy Y'
      nazwa_kolumny1 <- 'Kraj'
    } 
    
    if (input$agregat11 == 'Program zamówienia X' & input$agregat12 == 'Kraj'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.x,language_code), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.x]
      sort1 <- data_tabela[,.(N1 = .N), by=language_code]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'language_code', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia X'
      nazwa_kolumny1 <- 'Kraj'
    } 
    
    if (input$agregat11 == 'Program zamówienia Y' & input$agregat12 == 'Kraj'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.y,language_code), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.y]
      sort1 <- data_tabela[,.(N1 = .N), by=language_code]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'language_code', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia Y'
      nazwa_kolumny1 <- 'Kraj'
    } 
    
    
    
    
    
    if (input$agregat11 == 'Płeć konsumenta' & input$agregat12 == 'Płeć konsumenta'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = client_sex]
      data_tabela1 < data_tabela1[,drugie := client_sex]
      sort <- data_tabela[,.N,by=client_sex]
      data_tabela1 <- merge(data_tabela1, sort, by = 'client_sex', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Płeć konsumenta'
      nazwa_kolumny1 <- 'Płeć konsumenta'
    }
    
    
    if (input$agregat11 == 'Kraj' & input$agregat12 == 'Płeć konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(language_code,client_sex), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=language_code]
      sort1 <- data_tabela[,.(N1 = .N), by=client_sex]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'client_sex', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'language_code', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kraj'
      nazwa_kolumny1 <- 'Płeć konsumenta'
    } 
    
    if (input$agregat11 == 'Wiek konsumenta' & input$agregat12 == 'Płeć konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(last_order_client_age_range,client_sex), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=last_order_client_age_range]
      sort1 <- data_tabela[,.(N1 = .N), by=client_sex]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'client_sex', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Wiek konsumenta'
      nazwa_kolumny1 <- 'Płeć konsumenta'
    }
    
    if (input$agregat11 == "Grupa produktowa zamówienia X" & input$agregat12 == 'Płeć konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.x,client_sex), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.x]
      sort1 <- data_tabela[,.(N1 = .N), by=client_sex]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'client_sex', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia X'
      nazwa_kolumny1 <- 'Płeć konsumenta'
    } 
    
    if (input$agregat11 == "Grupa produktowa zamówienia Y" & input$agregat12 == 'Płeć konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.y,client_sex), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.y]
      sort1 <- data_tabela[,.(N1 = .N), by=client_sex]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'client_sex', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia Y'
      nazwa_kolumny1 <- 'Płeć konsumenta'
    } 
    
    if (input$agregat11 == 'Kanał reklamy X' & input$agregat12 == 'Płeć konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.x,client_sex), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.x]
      sort1 <- data_tabela[,.(N1 = .N), by=client_sex]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'client_sex', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy X'
      nazwa_kolumny1 <- 'Płeć konsumenta'
    } 
    
    if (input$agregat11 == 'Kanał reklamy Y' & input$agregat12 == 'Płeć konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.y,client_sex), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.y]
      sort1 <- data_tabela[,.(N1 = .N), by=client_sex]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'client_sex', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy Y'
      nazwa_kolumny1 <- 'Płeć konsumenta'
    } 
    
    if (input$agregat11 == 'Program zamówienia X' & input$agregat12 == 'Płeć konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.x,client_sex), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.x]
      sort1 <- data_tabela[,.(N1 = .N), by=client_sex]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'client_sex', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia X'
      nazwa_kolumny1 <- 'Płeć konsumenta'
    } 
    
    if (input$agregat11 == 'Program zamówienia Y' & input$agregat12 == 'Płeć konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.y,client_sex), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.y]
      sort1 <- data_tabela[,.(N1 = .N), by=client_sex]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'client_sex', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia Y'
      nazwa_kolumny1 <- 'Płeć konsumenta'
    } 
    
    
    
    
    if (input$agregat11 == 'Wiek konsumenta' & input$agregat12 == 'Wiek konsumenta'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = last_order_client_age_range]
      data_tabela1 < data_tabela1[,drugie := last_order_client_age_range]
      sort <- data_tabela[,.N,by=last_order_client_age_range]
      data_tabela1 <- merge(data_tabela1, sort, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Wiek konsumenta'
      nazwa_kolumny1 <- 'Wiek konsumenta'
    }
    
    
    if (input$agregat11 == 'Kraj' & input$agregat12 == 'Wiek konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(language_code,last_order_client_age_range), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=language_code]
      sort1 <- data_tabela[,.(N1 = .N), by=last_order_client_age_range]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'language_code', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kraj'
      nazwa_kolumny1 <- 'Wiek konsumenta'
    } 
    
    if (input$agregat11 == 'Płeć konsumenta' & input$agregat12 == 'Wiek konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(client_sex,last_order_client_age_range), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=client_sex]
      sort1 <- data_tabela[,.(N1 = .N), by=last_order_client_age_range]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'client_sex', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Płeć konsumenta'
      nazwa_kolumny1 <- 'Wiek konsumenta'
    }
    
    if (input$agregat11 == "Grupa produktowa zamówienia X" & input$agregat12 == 'Wiek konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.x,last_order_client_age_range), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.x]
      sort1 <- data_tabela[,.(N1 = .N), by=last_order_client_age_range]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia X'
      nazwa_kolumny1 <- 'Wiek konsumenta'
    } 
    
    if (input$agregat11 == "Grupa produktowa zamówienia Y" & input$agregat12 == 'Wiek konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.y,last_order_client_age_range), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.y]
      sort1 <- data_tabela[,.(N1 = .N), by=last_order_client_age_range]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia Y'
      nazwa_kolumny1 <- 'Wiek konsumenta'
    } 
    
    if (input$agregat11 == 'Kanał reklamy X' & input$agregat12 == 'Wiek konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.x,last_order_client_age_range), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.x]
      sort1 <- data_tabela[,.(N1 = .N), by=last_order_client_age_range]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy X'
      nazwa_kolumny1 <- 'Wiek konsumenta'
    } 
    
    if (input$agregat11 == 'Kanał reklamy Y' & input$agregat12 == 'Wiek konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.y,last_order_client_age_range), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.y]
      sort1 <- data_tabela[,.(N1 = .N), by=last_order_client_age_range]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy Y'
      nazwa_kolumny1 <- 'Wiek konsumenta'
    } 
    
    if (input$agregat11 == 'Program zamówienia X' & input$agregat12 == 'Wiek konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.x,last_order_client_age_range), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.x]
      sort1 <- data_tabela[,.(N1 = .N), by=last_order_client_age_range]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia X'
      nazwa_kolumny1 <- 'Wiek konsumenta'
    } 
    
    if (input$agregat11 == 'Program zamówienia Y' & input$agregat12 == 'Wiek konsumenta'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.y,last_order_client_age_range), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.y]
      sort1 <- data_tabela[,.(N1 = .N), by=last_order_client_age_range]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia Y'
      nazwa_kolumny1 <- 'Wiek konsumenta'
    } 
    
    
    
    
    if (input$agregat11 == 'Grupa produktowa zamówienia X' & input$agregat12 == 'Grupa produktowa zamówienia X'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = products_group.x]
      data_tabela1 < data_tabela1[,drugie := products_group.x]
      sort <- data_tabela[,.N,by=products_group.x]
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia X'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia X'
    }
    
    
    if (input$agregat11 == 'Kraj' & input$agregat12 == 'Grupa produktowa zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(language_code,products_group.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=language_code]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'language_code', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kraj'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia X'
    } 
    
    if (input$agregat11 == 'Płeć konsumenta' & input$agregat12 == 'Grupa produktowa zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(client_sex,products_group.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=client_sex]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'client_sex', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Płeć konsumenta'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia X'
    }
    
    if (input$agregat11 == "Wiek konsumenta" & input$agregat12 == 'Grupa produktowa zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(last_order_client_age_range,products_group.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=last_order_client_age_range]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Wiek konsumenta'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia X'
    } 
    
    if (input$agregat11 == "Grupa produktowa zamówienia Y" & input$agregat12 == 'Grupa produktowa zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.y,products_group.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.y]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia Y'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia X'
    } 
    
    if (input$agregat11 == 'Kanał reklamy X' & input$agregat12 == 'Grupa produktowa zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.x,products_group.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.x]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy X'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia X'
    } 
    
    if (input$agregat11 == 'Kanał reklamy Y' & input$agregat12 == 'Grupa produktowa zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.y,products_group.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.y]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy Y'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia X'
    } 
    
    if (input$agregat11 == 'Program zamówienia X' & input$agregat12 == 'Grupa produktowa zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.x,products_group.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.x]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia X'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia X'
    } 
    
    if (input$agregat11 == 'Program zamówienia Y' & input$agregat12 == 'Grupa produktowa zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.y,products_group.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.y]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia Y'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia X'
    } 
    
    
    
    
    if (input$agregat11 == 'Grupa produktowa zamówienia Y' & input$agregat12 == 'Grupa produktowa zamówienia Y'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = products_group.y]
      data_tabela1 < data_tabela1[,drugie := products_group.y]
      sort <- data_tabela[,.N,by=products_group.y]
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia Y'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia Y'
    }
    
    
    if (input$agregat11 == 'Kraj' & input$agregat12 == 'Grupa produktowa zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(language_code,products_group.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=language_code]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'language_code', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kraj'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia Y'
    } 
    
    if (input$agregat11 == 'Płeć konsumenta' & input$agregat12 == 'Grupa produktowa zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(client_sex,products_group.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=client_sex]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'client_sex', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Płeć konsumenta'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia Y'
    }
    
    if (input$agregat11 == "Wiek konsumenta" & input$agregat12 == 'Grupa produktowa zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(last_order_client_age_range,products_group.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=last_order_client_age_range]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Wiek konsumenta'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia Y'
    } 
    
    if (input$agregat11 == "Grupa produktowa zamówienia X" & input$agregat12 == 'Grupa produktowa zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.x,products_group.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.x]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia X'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia Y'
    } 
    
    if (input$agregat11 == 'Kanał reklamy X' & input$agregat12 == 'Grupa produktowa zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.x,products_group.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.x]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy X'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia Y'
    } 
    
    if (input$agregat11 == 'Kanał reklamy Y' & input$agregat12 == 'Grupa produktowa zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.y,products_group.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.y]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy Y'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia Y'
    } 
    
    if (input$agregat11 == 'Program zamówienia X' & input$agregat12 == 'Grupa produktowa zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.x,products_group.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.x]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia X'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia Y'
    } 
    
    if (input$agregat11 == 'Program zamówienia Y' & input$agregat12 == 'Grupa produktowa zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.y,products_group.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.y]
      sort1 <- data_tabela[,.(N1 = .N), by=products_group.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'products_group.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia Y'
      nazwa_kolumny1 <- 'Grupa produktowa zamówienia Y'
    } 
    
    
    
    
    if (input$agregat11 == 'Kanał reklamy X' & input$agregat12 == 'Kanał reklamy X'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = action_channel.x]
      data_tabela1 < data_tabela1[,drugie := action_channel.x]
      sort <- data_tabela[,.N,by=action_channel.x]
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Kanał reklamy X'
      nazwa_kolumny1 <- 'Kanał reklamy X'
    }
    
    
    if (input$agregat11 == 'Kraj' & input$agregat12 == 'Kanał reklamy X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(language_code,action_channel.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=language_code]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'language_code', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kraj'
      nazwa_kolumny1 <- 'Kanał reklamy X'
    } 
    
    if (input$agregat11 == 'Płeć konsumenta' & input$agregat12 == 'Kanał reklamy X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(client_sex,action_channel.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=client_sex]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'client_sex', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Płeć konsumenta'
      nazwa_kolumny1 <- 'Kanał reklamy X'
    }
    
    if (input$agregat11 == "Wiek konsumenta" & input$agregat12 == 'Kanał reklamy X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(last_order_client_age_range,action_channel.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=last_order_client_age_range]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Wiek konsumenta'
      nazwa_kolumny1 <- 'Kanał reklamy X'
    } 
    
    if (input$agregat11 == "Grupa produktowa zamówienia X" & input$agregat12 == 'Kanał reklamy X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.x,action_channel.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.x]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia X'
      nazwa_kolumny1 <- 'Kanał reklamy X'
    } 
    
    if (input$agregat11 == 'Grupa produktowa zamówienia Y' & input$agregat12 == 'Kanał reklamy X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.y,action_channel.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.y]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia Y'
      nazwa_kolumny1 <- 'Kanał reklamy X'
    } 
    
    if (input$agregat11 == 'Kanał reklamy Y' & input$agregat12 == 'Kanał reklamy X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.y,action_channel.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.y]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy Y'
      nazwa_kolumny1 <- 'Kanał reklamy X'
    } 
    
    if (input$agregat11 == 'Program zamówienia X' & input$agregat12 == 'Kanał reklamy X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.x,action_channel.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.x]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia X'
      nazwa_kolumny1 <- 'Kanał reklamy X'
    } 
    
    if (input$agregat11 == 'Program zamówienia Y' & input$agregat12 == 'Kanał reklamy X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.y,action_channel.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.y]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia Y'
      nazwa_kolumny1 <- 'Kanał reklamy X'
    } 
    
    
    
    
    if (input$agregat11 == 'Kanał reklamy Y' & input$agregat12 == 'Kanał reklamy Y'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = action_channel.y]
      data_tabela1 < data_tabela1[,drugie := action_channel.y]
      sort <- data_tabela[,.N,by=action_channel.y]
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Kanał reklamy Y'
      nazwa_kolumny1 <- 'Kanał reklamy Y'
    }
    
    
    if (input$agregat11 == 'Kraj' & input$agregat12 == 'Kanał reklamy Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(language_code,action_channel.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=language_code]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'language_code', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kraj'
      nazwa_kolumny1 <- 'Kanał reklamy Y'
    } 
    
    if (input$agregat11 == 'Płeć konsumenta' & input$agregat12 == 'Kanał reklamy Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(client_sex,action_channel.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=client_sex]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'client_sex', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Płeć konsumenta'
      nazwa_kolumny1 <- 'Kanał reklamy Y'
    }
    
    if (input$agregat11 == "Wiek konsumenta" & input$agregat12 == 'Kanał reklamy Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(last_order_client_age_range,action_channel.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=last_order_client_age_range]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Wiek konsumenta'
      nazwa_kolumny1 <- 'Kanał reklamy Y'
    } 
    
    if (input$agregat11 == "Grupa produktowa zamówienia X" & input$agregat12 == 'Kanał reklamy Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.x,action_channel.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.x]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia X'
      nazwa_kolumny1 <- 'Kanał reklamy Y'
    } 
    
    if (input$agregat11 == 'Grupa produktowa zamówienia Y' & input$agregat12 == 'Kanał reklamy Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.y,action_channel.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.y]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia Y'
      nazwa_kolumny1 <- 'Kanał reklamy Y'
    } 
    
    if (input$agregat11 == 'Kanał reklamy X' & input$agregat12 == 'Kanał reklamy Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.x,action_channel.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.x]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy X'
      nazwa_kolumny1 <- 'Kanał reklamy Y'
    } 
    
    if (input$agregat11 == 'Program zamówienia X' & input$agregat12 == 'Kanał reklamy Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.x,action_channel.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.x]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia X'
      nazwa_kolumny1 <- 'Kanał reklamy Y'
    } 
    
    if (input$agregat11 == 'Program zamówienia Y' & input$agregat12 == 'Kanał reklamy Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.y,action_channel.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.y]
      sort1 <- data_tabela[,.(N1 = .N), by=action_channel.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'action_channel.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia Y'
      nazwa_kolumny1 <- 'Kanał reklamy Y'
    } 
    
    
    
    
    
    if (input$agregat11 == 'Program zamówienia X' & input$agregat12 == 'Program zamówienia X'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = program.x]
      data_tabela1 < data_tabela1[,drugie := program.x]
      sort <- data_tabela[,.N,by=program.x]
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Program zamówienia X'
      nazwa_kolumny1 <- 'Program zamówienia X'
    }
    
    
    if (input$agregat11 == 'Kraj' & input$agregat12 == 'Program zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(language_code,program.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=language_code]
      sort1 <- data_tabela[,.(N1 = .N), by=program.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'language_code', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kraj'
      nazwa_kolumny1 <- 'Program zamówienia X'
    } 
    
    if (input$agregat11 == 'Płeć konsumenta' & input$agregat12 == 'Program zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(client_sex,program.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=client_sex]
      sort1 <- data_tabela[,.(N1 = .N), by=program.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'client_sex', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Płeć konsumenta'
      nazwa_kolumny1 <- 'Program zamówienia X'
    }
    
    if (input$agregat11 == "Wiek konsumenta" & input$agregat12 == 'Program zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(last_order_client_age_range,program.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=last_order_client_age_range]
      sort1 <- data_tabela[,.(N1 = .N), by=program.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Wiek konsumenta'
      nazwa_kolumny1 <- 'Program zamówienia X'
    } 
    
    if (input$agregat11 == "Grupa produktowa zamówienia X" & input$agregat12 == 'Program zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.x,program.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.x]
      sort1 <- data_tabela[,.(N1 = .N), by=program.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia X'
      nazwa_kolumny1 <- 'Program zamówienia X'
    } 
    
    if (input$agregat11 == 'Grupa produktowa zamówienia Y' & input$agregat12 == 'Program zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.y,program.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.y]
      sort1 <- data_tabela[,.(N1 = .N), by=program.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia Y'
      nazwa_kolumny1 <- 'Program zamówienia X'
    } 
    
    if (input$agregat11 == 'Kanał reklamy X' & input$agregat12 == 'Program zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.x,program.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.x]
      sort1 <- data_tabela[,.(N1 = .N), by=program.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy X'
      nazwa_kolumny1 <- 'Program zamówienia X'
    } 
    
    if (input$agregat11 == 'Kanał reklamy Y' & input$agregat12 == 'Program zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.y,program.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.y]
      sort1 <- data_tabela[,.(N1 = .N), by=program.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy Y'
      nazwa_kolumny1 <- 'Program zamówienia X'
    } 
    
    if (input$agregat11 == 'Program zamówienia Y' & input$agregat12 == 'Program zamówienia X'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.y,program.x), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.y]
      sort1 <- data_tabela[,.(N1 = .N), by=program.x]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.x', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia Y'
      nazwa_kolumny1 <- 'Program zamówienia X'
    } 
    
    
    
    
    if (input$agregat11 == 'Program zamówienia Y' & input$agregat12 == 'Program zamówienia Y'){
      data_tabela1 <- data_tabela[,.(roznica_pp = mean(roznica_pp)), by = program.y]
      data_tabela1 < data_tabela1[,drugie := program.y]
      sort <- data_tabela[,.N,by=program.y]
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N)]
      data_tabela1 <- data_tabela1[,c(1,3,2)]
      nazwa_kolumny <- 'Program zamówienia Y'
      nazwa_kolumny1 <- 'Program zamówienia Y'
    }
    
    
    if (input$agregat11 == 'Kraj' & input$agregat12 == 'Program zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(language_code,program.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=language_code]
      sort1 <- data_tabela[,.(N1 = .N), by=program.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'language_code', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kraj'
      nazwa_kolumny1 <- 'Program zamówienia Y'
    } 
    
    if (input$agregat11 == 'Płeć konsumenta' & input$agregat12 == 'Program zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(client_sex,program.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=client_sex]
      sort1 <- data_tabela[,.(N1 = .N), by=program.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'client_sex', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Płeć konsumenta'
      nazwa_kolumny1 <- 'Program zamówienia Y'
    }
    
    if (input$agregat11 == "Wiek konsumenta" & input$agregat12 == 'Program zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(last_order_client_age_range,program.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=last_order_client_age_range]
      sort1 <- data_tabela[,.(N1 = .N), by=program.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'last_order_client_age_range', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Wiek konsumenta'
      nazwa_kolumny1 <- 'Program zamówienia Y'
    } 
    
    if (input$agregat11 == "Grupa produktowa zamówienia X" & input$agregat12 == 'Program zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.x,program.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.x]
      sort1 <- data_tabela[,.(N1 = .N), by=program.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia X'
      nazwa_kolumny1 <- 'Program zamówienia Y'
    } 
    
    if (input$agregat11 == 'Grupa produktowa zamówienia Y' & input$agregat12 == 'Program zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(products_group.y,program.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=products_group.y]
      sort1 <- data_tabela[,.(N1 = .N), by=program.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'products_group.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Grupa produktowa zamówienia Y'
      nazwa_kolumny1 <- 'Program zamówienia Y'
    } 
    
    if (input$agregat11 == 'Kanał reklamy X' & input$agregat12 == 'Program zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.x,program.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.x]
      sort1 <- data_tabela[,.(N1 = .N), by=program.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy X'
      nazwa_kolumny1 <- 'Program zamówienia Y'
    } 
    
    if (input$agregat11 == 'Kanał reklamy Y' & input$agregat12 == 'Program zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(action_channel.y,program.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=action_channel.y]
      sort1 <- data_tabela[,.(N1 = .N), by=program.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'action_channel.y', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Kanał reklamy Y'
      nazwa_kolumny1 <- 'Program zamówienia Y'
    } 
    
    if (input$agregat11 == 'Program zamówienia X' & input$agregat12 == 'Program zamówienia Y'){
      data_tabela1 <- data_tabela[, lapply(.SD, mean, na.rm=TRUE), by=list(program.x,program.y), .SDcols=c("roznica_pp") ]
      sort <- data_tabela[,.N,by=program.x]
      sort1 <- data_tabela[,.(N1 = .N), by=program.y]
      data_tabela1 <- merge(data_tabela1, sort1, by = 'program.y', all.x = T)
      data_tabela1 <- merge(data_tabela1, sort, by = 'program.x', all.x = T)
      data_tabela1 <- data_tabela1[order(-N, -N1)]
      data_tabela1 <- data_tabela1[,c(1,2,3)]
      nazwa_kolumny <- 'Program zamówienia X'
      nazwa_kolumny1 <- 'Program zamówienia Y'
    } 
    
    
    
    
    data_tabela1$roznica_pp <- round(data_tabela1$roznica_pp)
    names(data_tabela1) <- c('kolumna', 'kolumna1', 'N')
    
    if ((input$agregat22 != input$agregat11) & input$agregat12 != 'Total') {
      
      for (i in nrow(data_tabela1):1) {
        if (data_tabela1$kolumna[i] %in% data_tabela1$kolumna[-i]) {
          data_tabela1$kolumna[i] <- paste0('<span style="font-size: 0%">', data_tabela1$kolumna[i], '</span>')
        }
      }
      
    } else {
      data_tabela1$kolumna1 <- ''
    }
    
    output$tabela_dni <- DT::renderDataTable({
      datatable(
        data_tabela1,
        filter = 'none',
        rownames = FALSE,
        colnames = c(nazwa_kolumny, nazwa_kolumny1, 'Średnia liczba dni pomiędzy zamówieniami'),
        escape = FALSE,
        options = list(
          lenghtMenu = c(10, 20, 50),
          paging = T,
          fixedHeader=TRUE,
          scroller = T,
          scrollx=T,
          scrolly=F,
          pageLength = 10,
          autoWidth = F,
          dom ='t<"bottom"lp><"clear">'
        )) 
      
    })
    
    output$Plotline1 <- renderPlotly({
      data_line1%>%
        plot_ly()%>%
        add_trace( x = ~data_line1$dni, y = ~data_line1$proc,
                   type = 'scatter', hoverinfo = "text", text = data_line1$text,
                   mode = 'lines', fill = 'tozeroy') %>%
        layout(title = list(
          text = "Rozkład zamówień w czasie",
          yanchor = "top",
          pad = list(
            t = 10
          )
        ),
        xaxis = list(title = 'Do ilu dni powstało drugie zamówienie'),
        yaxis = list(title = 'Udział',
                     tickformat = ',.0%',
                     range = c(0,1)))
    })
    
    
    
    output$Plotbar1 <- renderPlotly({
      plot_ly(data_bar1, x = ~data_bar1$dni, y = ~data_bar1$N, type = 'bar',
              text = paste0(round(data_bar1$N/nrow(data_bar),3)*100, '%'), textposition = 'outside',
              hovertemplate = paste0('Przedział czasowy: %{x} dni od pierwszego zamówienia<br>Liczba zamówień: ', format(data_bar1$N, big.mark = ' ', big.interval = 3L),'<extra></extra>'))%>%
        layout(
          title = list(
            text = "Liczba zamówień w poszczególnych przedziałach czasowych",
            yanchor = "top",
            pad = list(
              t = 10
            )
          ),
          xaxis = list(
            title = list(
              text = "Przedział czasowy (dni)"
            ),
            autorange = TRUE,
            categoryorder = "array",
            categoryarray = data_bar$dni
          ),
          yaxis = list(
            title = list(
              text = "Liczba zamówień"
            )
          )
        )
    })
    
  } else {
    
    output$tabela_dni <- NULL
    output$Plotline1 <- NULL
    output$Plotbar1 <- renderPlotly({
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
          text = "Za mało zamówień w bazie - wybierz inne ograniczenia.",
          font = list(
            size = 30,
            color = 'firebrick2'
          ),
          showarrow = FALSE
        )
    })
  }
  updateProgressBar(session = session, id = "postep1", value = 70, title = "Wyświetlanie")
  
  shinyjs::hide(selector = '.progress-group:has(#postep1)')   
  updateProgressBar(session = session, id = "postep1", value = 0, title = "Pasek postępu")
})
