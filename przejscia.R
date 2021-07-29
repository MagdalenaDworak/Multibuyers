hide(id='legenda1')
hide(id='legenda2')
show(id='legenda')

balax <- dostepy_ALLx[user_name == login]
bala1x <- balax[products_group1 != 'multi']
ijoijox <- data[products_group.x_1 %in% balax$products_group1, .N, by = products_group.x_1]
ijoijox <- ijoijox[order(-N)]
jajox <- data[campaign_product_group.x %in% balax$campaign_product_group, .N, by = campaign_product_group.x] 
jajox <- jajox[order(-N)]

balax1 <- dostepy_ALLx1[user_name == login]
bala1x1 <- balax1[products_group1 != 'multi']
ijoijox1 <- data[campaign_product_group.x %in% balax1$campaign_product_group, .N, by = campaign_product_group.x]
ijoijox1 <- ijoijox1[order(-N)]
jajox1 <- data[products_group.x_1 %in% balax1$products_group1, .N, by = products_group.x_1] 
jajox1 <- jajox1[order(-N)]

balay <- dostepy_ALLy[user_name == login]
bala1y <- balay[products_group1 != 'multi']
ijoijoy <- data[products_group.x_1 %in% balay$products_group1, .N, by = products_group.x_1]
ijoijoy <- ijoijoy[order(-N)]
jajoy <- data[campaign_product_group.y %in% balax$campaign_product_group, .N, by = campaign_product_group.y] 
jajoy <- jajoy[order(-N)]

observeEvent(input$agregat,{

  if (login %in% c("sternickip", 'niedzielakd', "golatm", "sradziszewska", "zalewskaal", "nowakkr", "boczkowskim", "szymaniake", "kolszewski", "kisielinskih", "jrodzen",
                         "ihorbaj", "kprus", "nowakowskaa","michnowskaa","burdzyd","kolanol","kaczorowskim","bartoszewskip", "hnatyukv", "maciejczykk","yakovlevao",
                         "skrzypczakm", "kowalczyko", "adudzinska","gaszczk", "rdzanowskim",  "zeganl", "eroller","rlipnicki", "stelmachk", "tyszkop", "nowocinm", "kwojtyla","asierocka","hyba","jastrzebskak")) {
    if (input$agregat == 'Kanał reklamy') {
      hide(id='dokładny')
      hide(id='kamp')
      
      updateSelectInput(
        session,
        inputId = 'glownedanex',
        label = 'Kanał reklamy X',
        choices = sort(unique(data[, action_channel.x])),
        selected = 'ATD'
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedanex',
        label = 'Grupa produktowa zamówienia X',
        choices = sort(unique(data[, products_group.x_1])),
        selected = sort(unique(data[, products_group.x_1]))
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedaney',
        label = 'Grupa produktowa zamówienia Y',
        choices = sort(unique(data[, products_group.y_1])),
        selected = sort(unique(data[, products_group.y_1]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedanex',
        label = 'Grupa produktowa kampanii X',
        choices = sort(unique(data[, campaign_product_group.x])),
        selected = sort(unique(data[, campaign_product_group.x]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedaney',
        label = 'Grupa produktowa kampanii Y',
        choices = sort(unique(data[, campaign_product_group.y])),
        selected = sort(unique(data[, campaign_product_group.y]))
      )
      
      updateSelectInput(
        session,
        inputId = 'warn',
        label = "Warunek na łączenie kanałów",
        choices = c('OR'='|'),
        selected = c('OR'='|')
      )
      
      
    } 
    if (input$agregat == 'Grupa produktowa zamówienia') {
      show(id='dokładny')
      hide(id='kamp')
      
      updateSelectInput(
        session,
        inputId = 'glownedanex',
        label = 'Grupa produktowa zamówienia X',
        choices = sort(unique(data[products_group.x_1 != 'multi', products_group.x_1])),
        selected = "hearing"
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedanex',
        label = 'Kanał reklamy X',
        choices = sort(unique(data[, action_channel.x])),
        selected = sort(unique(data[, action_channel.x]))
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedaney',
        label = 'Kanał reklamy Y',
        choices = sort(unique(data[, action_channel.y])),
        selected = sort(unique(data[, action_channel.y]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedanex',
        label = 'Grupa produktowa kampanii X',
        choices = sort(unique(data[, campaign_product_group.x])),
        selected = sort(unique(data[, campaign_product_group.x]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedaney',
        label = 'Grupa produktowa kampanii Y',
        choices = sort(unique(data[, campaign_product_group.y])),
        selected = sort(unique(data[, campaign_product_group.y]))
      )
      
      updateSelectInput(
        session,
        inputId = 'warn',
        label = "Warunek na łączenie grup",
        choices = c('OR'='|', 'AND'='&'),
        selected = c('OR'='|')
      )
      
      
    } 
    if (input$agregat == 'Grupa produktowa kampanii') {
      show(id='dokładny')
      show(id='kamp')
      
      updateSelectInput(
        session,
        inputId = 'glownedanex',
        label = 'Grupa produktowa kampanii X',
        choices = sort(unique(data[, campaign_product_group.x])),
        selected = "hearing"
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedanex',
        label = 'Kanał reklamy X',
        choices = sort(unique(data[, action_channel.x])),
        selected = sort(unique(data[, action_channel.x]))
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedaney',
        label = 'Kanał reklamy Y',
        choices = sort(unique(data[, action_channel.y])),
        selected = sort(unique(data[, action_channel.y]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedanex',
        label = 'Grupa produktowa zamówienia X',
        choices = sort(unique(data[, products_group.x_1])),
        selected = sort(unique(data[, products_group.x_1]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedaney',
        label = 'Grupa produktowa zamówienia Y',
        choices = sort(unique(data[, products_group.y_1])),
        selected = sort(unique(data[, products_group.y_1]))
      )
      
      updateSelectInput(
        session,
        inputId = 'warn',
        label = "Warunek na łączenie grup",
        choices = c('OR'='|', 'AND'='&'),
        selected = c('OR'='|')
      )
      
    }
    
  } else if (login %in% loginy_mdm$ldap_login) {
    
    if (input$agregat == 'Kanał reklamy') {
      hide(id='dokładny')
      hide(id='kamp')
      
      updateSelectInput(
        session,
        inputId = 'glownedanex',
        label = 'Kanał reklamy X',
        choices = sort(unique(data[, action_channel.x])),
        selected = 'ATD'
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedanex',
        label = 'Grupa produktowa zamówienia X',
        choices = sort(unique(balax1$products_group1)),
        selected = sort(unique(balax1$products_group1))
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedaney',
        label = 'Grupa produktowa zamówienia Y',
        choices = sort(unique(data[, products_group.y_1])),
        selected = sort(unique(data[, products_group.y_1]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedanex',
        label = 'Grupa produktowa kampanii X',
        choices = sort(unique(balax1$campaign_product_group)),
        selected = sort(unique(balax1$campaign_product_group))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedaney',
        label = 'Grupa produktowa kampanii Y',
        choices = sort(unique(data[, campaign_product_group.y])),
        selected = sort(unique(data[, campaign_product_group.y]))
      )
      
      updateSelectInput(
        session,
        inputId = 'warn',
        label = "Warunek na łączenie kanałów",
        choices = c('OR'='|'),
        selected = c('OR'='|')
      )
      
    } 
    if (input$agregat == 'Grupa produktowa zamówienia') {
      show(id='dokładny')
      hide(id='kamp')
      
      updateSelectInput(
        session,
        inputId = 'glownedanex',
        label = 'Grupa produktowa zamówienia X',
        choices = jajox1$products_group.x_1,
        selected = jajox1$products_group.x_1[1]
      ) 
      
      updateSelectInput(
        session,
        inputId = 'drugiedanex',
        label = 'Kanał reklamy X',
        choices = sort(unique(data[, action_channel.x])),
        selected = sort(unique(data[, action_channel.x]))
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedaney',
        label = 'Kanał reklamy Y',
        choices = sort(unique(data[, action_channel.y])),
        selected = sort(unique(data[, action_channel.y]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedanex',
        label = 'Grupa produktowa kampanii X',
        choices = sort(unique(balax1$campaign_product_group)),
        selected = sort(unique(balax1$campaign_product_group))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedaney',
        label = 'Grupa produktowa kampanii Y',
        choices = sort(unique(data[, campaign_product_group.y])),
        selected = sort(unique(data[, campaign_product_group.y]))
      )
      
      updateSelectInput(
        session,
        inputId = 'warn',
        label = "Warunek na łączenie grup",
        choices = c('OR'='|', 'AND'='&'),
        selected = c('OR'='|')
      )
      
      
    } 
    if (input$agregat == 'Grupa produktowa kampanii') {
      show(id='dokładny')
      show(id='kamp')
      
      updateSelectInput(
        session,
        inputId = 'glownedanex',
        label = 'Grupa produktowa kampanii X',
        choices = sort(unique(bala1x$campaign_product_group)),
        selected = jajox$campaign_product_group[1]
      ) 
      
      updateSelectInput(
        session,
        inputId = 'drugiedanex',
        label = 'Kanał reklamy X',
        choices = sort(unique(data[, action_channel.x])),
        selected = sort(unique(data[, action_channel.x]))
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedaney',
        label = 'Kanał reklamy Y',
        choices = sort(unique(data[, action_channel.y])),
        selected = sort(unique(data[, action_channel.y]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedanex',
        label = 'Grupa produktowa zamówienia X',
        choices = sort(unique(balax$products_group1)),
        selected = sort(unique(balax$products_group1))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedaney',
        label = 'Grupa produktowa zamówienia Y',
        choices = sort(unique(data[, products_group.y_1])),
        selected = sort(unique(data[, products_group.y_1]))
      )
      
      updateSelectInput(
        session,
        inputId = 'warn',
        label = "Warunek na łączenie grup",
        choices = c('OR'='|', 'AND'='&'),
        selected = c('OR'='|')
      )
      
    }
    
  }
  
})


####dwa####
observeEvent(input$agregatz,{
  
  if (login %in% c("sternickip", 'niedzielakd', "golatm", "sradziszewska", "zalewskaal", "nowakkr", "boczkowskim", "szymaniake", "kolszewski", "kisielinskih", "jrodzen",
                         "ihorbaj", "kprus", "nowakowskaa","michnowskaa","burdzyd","kolanol","kaczorowskim","bartoszewskip", "hnatyukv", "maciejczykk","yakovlevao",
                         "skrzypczakm", "kowalczyko", "adudzinska","gaszczk", "rdzanowskim", "zeganl", "eroller","rlipnicki", "stelmachk", "tyszkop", "nowocinm", "kwojtyla","asierocka","hyba","jastrzebskak")) {
    if (input$agregatz == 'Kanał reklamy') {
      
      
      updateSelectInput(
        session,
        inputId = 'drugiedanexz',
        label = 'Grupa produktowa zamówienia X',
        choices = sort(unique(data[, products_group.x_1])),
        selected = sort(unique(data[, products_group.x_1]))
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedaneyz',
        label = 'Grupa produktowa zamówienia Y',
        choices = sort(unique(data[, products_group.y_1])),
        selected = sort(unique(data[, products_group.y_1]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedanexz',
        label = 'Grupa produktowa kampanii X',
        choices = sort(unique(data[, campaign_product_group.x])),
        selected = sort(unique(data[, campaign_product_group.x]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedaneyz',
        label = 'Grupa produktowa kampanii Y',
        choices = sort(unique(data[, campaign_product_group.y])),
        selected = sort(unique(data[, campaign_product_group.y]))
      )
      
      
    } 
    if (input$agregatz == 'Grupa produktowa zamówienia') {
      
      
      updateSelectInput(
        session,
        inputId = 'drugiedanexz',
        label = 'Kanał reklamy X',
        choices = sort(unique(data[, action_channel.x])),
        selected = sort(unique(data[, action_channel.x]))
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedaneyz',
        label = 'Kanał reklamy Y',
        choices = sort(unique(data[, action_channel.y])),
        selected = sort(unique(data[, action_channel.y]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedanexz',
        label = 'Grupa produktowa kampanii X',
        choices = sort(unique(data[, campaign_product_group.x])),
        selected = sort(unique(data[, campaign_product_group.x]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedaneyz',
        label = 'Grupa produktowa kampanii Y',
        choices = sort(unique(data[, campaign_product_group.y])),
        selected = sort(unique(data[, campaign_product_group.y]))
      )
      
      
      
    } 
    if (input$agregatz == 'Grupa produktowa kampanii') {
      
      
      updateSelectInput(
        session,
        inputId = 'drugiedanexz',
        label = 'Kanał reklamy X',
        choices = sort(unique(data[, action_channel.x])),
        selected = sort(unique(data[, action_channel.x]))
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedaneyz',
        label = 'Kanał reklamy Y',
        choices = sort(unique(data[, action_channel.y])),
        selected = sort(unique(data[, action_channel.y]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedanexz',
        label = 'Grupa produktowa zamówienia X',
        choices = sort(unique(data[, products_group.x_1])),
        selected = sort(unique(data[, products_group.x_1]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedaneyz',
        label = 'Grupa produktowa zamówienia Y',
        choices = sort(unique(data[, products_group.y_1])),
        selected = sort(unique(data[, products_group.y_1]))
      )
      
      
    }
    
  } else if (login %in% loginy_mdm$ldap_login) {
    
    if (input$agregatz == 'Kanał reklamy') {
      
      updateSelectInput(
        session,
        inputId = 'drugiedanexz',
        label = 'Grupa produktowa zamówienia X',
        choices = sort(unique(balax1$products_group1)),
        selected = sort(unique(balax1$products_group1))
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedaneyz',
        label = 'Grupa produktowa zamówienia Y',
        choices = sort(unique(data[, products_group.y_1])),
        selected = sort(unique(data[, products_group.y_1]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedanexz',
        label = 'Grupa produktowa kampanii X',
        choices = sort(unique(balax1$campaign_product_group)),
        selected = sort(unique(balax1$campaign_product_group))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedaneyz',
        label = 'Grupa produktowa kampanii Y',
        choices = sort(unique(data[, campaign_product_group.y])),
        selected = sort(unique(data[, campaign_product_group.y]))
      )
      
    } 
    if (input$agregatz == 'Grupa produktowa zamówienia') {
      
      
      updateSelectInput(
        session,
        inputId = 'drugiedanexz',
        label = 'Kanał reklamy X',
        choices = sort(unique(data[, action_channel.x])),
        selected = sort(unique(data[, action_channel.x]))
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedaneyz',
        label = 'Kanał reklamy Y',
        choices = sort(unique(data[, action_channel.y])),
        selected = sort(unique(data[, action_channel.y]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedanexz',
        label = 'Grupa produktowa kampanii X',
        choices = sort(unique(balax1$campaign_product_group)),
        selected = sort(unique(balax1$campaign_product_group))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedaneyz',
        label = 'Grupa produktowa kampanii Y',
        choices = sort(unique(data[, campaign_product_group.y])),
        selected = sort(unique(data[, campaign_product_group.y]))
      )
      
    } 
    if (input$agregatz == 'Grupa produktowa kampanii') {
      
      updateSelectInput(
        session,
        inputId = 'drugiedanexz',
        label = 'Kanał reklamy X',
        choices = sort(unique(data[, action_channel.x])),
        selected = sort(unique(data[, action_channel.x]))
      )
      
      updateSelectInput(
        session,
        inputId = 'drugiedaneyz',
        label = 'Kanał reklamy Y',
        choices = sort(unique(data[, action_channel.y])),
        selected = sort(unique(data[, action_channel.y]))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedanexz',
        label = 'Grupa produktowa zamówienia X',
        choices = sort(unique(balax$products_group1)),
        selected = sort(unique(balax$products_group1))
      )
      
      updateSelectInput(
        session,
        inputId = 'trzeciedaneyz',
        label = 'Grupa produktowa zamówienia Y',
        choices = sort(unique(data[, products_group.y_1])),
        selected = sort(unique(data[, products_group.y_1]))
      )
      
    }
    
  }
  
})
#####tutera####
#shinyjs::click("zastosujz")
observeEvent(input$zastosujz,{

  if (input$agregatz == 'Grupa produktowa zamówienia') {
    
    shinyjs::show(selector = '.progress-group:has(#postepz)')
    updateProgressBar(session = session, id = "postepz", value = 0, title = "Pasek postępu")
    
    
    dane <- data[action_channel.x %in% input$drugiedanexz & action_channel.y %in% input$drugiedaneyz
                 & campaign_product_group.x %in% input$trzeciedanexz
                 & campaign_product_group.y %in% input$trzeciedaneyz
                 & czy_bezposrednia %in% input$bezpoz
                 & cross %in% input$crossz
                 & program.x %in% input$programxz
                 & program.y %in% input$programyz
                 & client_sex %in% input$plecz
                 & last_order_client_age_range %in% input$wiekz
                 & language_code %in% input$krajz,
                 ]

    
    if (!is.null(input$dataz)) {
      dane <- dane[substr(added_datetime.x,1,7) >= substr(input$dataz[1],1,7) &
                     substr(added_datetime.x,1,7) <= substr(input$dataz[length(input$dataz)],1,7)]
    }
    
    dane1 <- dane[, .('liczba zamówień'=.N, 'średnia wartość zamówienia X (zł)'=mean(a_current_price.x),'średnia wartość zamówienia X (€)'=mean(a_current_price.x)/4.5, 
                      'średnia wartość zamówienia Y (zł)'=mean(a_current_price.y), 'średnia wartość zamówienia Y (€)'=mean(a_current_price.y)/4.5, 'średnia ilość dni między zamówieniami'=mean(roznica_pp)), by = c('products_group.x_1','products_group.y_1')]
    dane1 <- dane1[order(-`liczba zamówień`)]
    dane1 <- dane1[1:input$als]
    dane1[,para := paste(products_group.x_1, '->', products_group.y_1)]
    dane1[,abc := rank(-`liczba zamówień`), by = products_group.x_1]
    
  } else
    if (input$agregatz == 'Grupa produktowa kampanii') {
      
      dane <- data[action_channel.x %in% input$drugiedanexz
                   & action_channel.y %in% input$drugiedaneyz
                   & products_group.x_1 %in% input$trzeciedanexz
                   & products_group.y_1 %in% input$trzeciedaneyz
                   & czy_bezposrednia %in% input$bezpoz
                   & cross %in% input$crossz
                   & program.x %in% input$programxz
                   & program.y %in% input$programyz
                   & client_sex %in% input$plecz
                   & last_order_client_age_range %in% input$wiekz
                   & language_code %in% input$krajz,]
      
      if (!is.null(input$dataz)) {
        dane <- dane[substr(added_datetime.x,1,7) >= substr(input$dataz[1],1,7) &
                       substr(added_datetime.x,1,7) <= substr(input$dataz[length(input$dataz)],1,7)]
      }
      
      dane1 <- dane[, .('liczba zamówień'=.N, 'średnia wartość zamówienia X (zł)'=mean(a_current_price.x),'średnia wartość zamówienia X (€)'=mean(a_current_price.x)/4.5, 
                        'średnia wartość zamówienia Y (zł)'=mean(a_current_price.y), 'średnia wartość zamówienia Y (€)'=mean(a_current_price.y)/4.5, 'średnia ilość dni między zamówieniami'=mean(roznica_pp)), by = c('campaign_product_group.x','campaign_product_group.y')]
      dane1 <- dane1[order(-`liczba zamówień`)]
      dane1 <- dane1[1:input$als]
      dane1[,para := paste(campaign_product_group.x, '->', campaign_product_group.y)]
      dane1[,abc := rank(-`liczba zamówień`), by = campaign_product_group.x]
      
    } else 
      if (input$agregatz == 'Kanał reklamy') {
        
        dane <- data[products_group.x_1 %in% input$drugiedanexz
                     & products_group.y_1 %in% input$drugiedaneyz
                     & products_group.x_1 %in% input$trzeciedanexz
                     & products_group.y_1 %in% input$trzeciedaneyz
                     & czy_bezposrednia %in% input$bezpoz
                     & cross %in% input$crossz
                     & program.x %in% input$programxz
                     & program.y %in% input$programyz
                     & client_sex %in% input$plecz
                     & last_order_client_age_range %in% input$wiekz
                     & language_code %in% input$krajz,]
        
        
        if (!is.null(input$dataz)) {
          dane <- dane[substr(added_datetime.x,1,7) >= substr(input$dataz[1],1,7) &
                         substr(added_datetime.x,1,7) <= substr(input$dataz[length(input$dataz)],1,7)]
        }
        
        dane1 <- dane[, .('liczba zamówień'=.N, 'średnia wartość zamówienia X (zł)'=mean(a_current_price.x),'średnia wartość zamówienia X (€)'=mean(a_current_price.x)/4.5, 
                          'średnia wartość zamówienia Y (zł)'=mean(a_current_price.y), 'średnia wartość zamówienia Y (€)'=mean(a_current_price.y)/4.5, 'średnia ilość dni między zamówieniami'=mean(roznica_pp)), by = c('action_channel.x','action_channel.y')]
        dane1 <- dane1[order(-`liczba zamówień`)]
        dane1 <- dane1[1:input$als]
        dane1[,para := paste(action_channel.x, '->', action_channel.y)]
        dane1[,abc := rank(-`liczba zamówień`), by = action_channel.x]
      }
  
  updateProgressBar(session = session, id = "postep", value = 30, title = "Pasek postępu")
  
  if (input$agregatz == 'Grupa produktowa zamówienia') {
    output$Plotsan <- renderPlot(ggplot2::ggplot(dane1, ggplot2::aes(area = `liczba zamówień`, label=paste(`products_group.y_1`,format(`liczba zamówień`, big.mark = "'", big.interval = 3L)),
                                                                     fill = `products_group.x_1`, subgroup = paste(`products_group.x_1`,'➪'))) +
                                   geom_treemap(fill = "grey") +
                                   geom_treemap(aes(alpha = abc)) +
                                   geom_treemap_subgroup_border(color='white')+
                                   geom_treemap_subgroup_text(place = 'center',alpha = 0.7)+
                                   geom_treemap_text(colour = 'white', place = 'topleft', reflow = T) +
                                   scale_alpha_continuous(range = c(1, 0.2))+
                                   theme(legend.position = 'none')
                                 
    )
    
  } else 
    if (input$agregatz == 'Grupa produktowa kampanii') {
      output$Plotsan <- renderPlot(ggplot2::ggplot(dane1, ggplot2::aes(area = `liczba zamówień`, label=paste(`campaign_product_group.y`,format(`liczba zamówień`, big.mark = "'", big.interval = 3L)),
                                                                       fill = `campaign_product_group.x`, subgroup = paste(`campaign_product_group.x`,'➪'))) +
                                     geom_treemap(fill = "grey") +
                                     geom_treemap(aes(alpha = abc)) +
                                     geom_treemap_subgroup_border(color='white')+
                                     geom_treemap_subgroup_text(place = 'center',alpha = 0.7)+
                                     geom_treemap_text(colour = 'white', place = 'topleft', reflow = T) +
                                     scale_alpha_continuous(range = c(1, 0.2))+
                                     theme(legend.position = 'none')
                                   
      )
      
    } else 
      if (input$agregatz == 'Kanał reklamy') {
        output$Plotsan <- renderPlot(ggplot2::ggplot(dane1, ggplot2::aes(area = `liczba zamówień`, label=paste(`action_channel.y`,format(`liczba zamówień`, big.mark = "'", big.interval = 3L)),
                                                                         fill = `action_channel.x`, subgroup = paste(`action_channel.x`,'➪'))) +
                                       geom_treemap(fill = "grey") +
                                       geom_treemap(aes(alpha = abc)) +
                                       geom_treemap_subgroup_border(color='white')+
                                       geom_treemap_subgroup_text(place = 'center',alpha = 0.7)+
                                       geom_treemap_text(colour = 'white', place = 'topleft', reflow = T) +
                                       scale_alpha_continuous(range = c(1, 0.2))+
                                       theme(legend.position = 'none')
                                     
        )
        
      }
  
  updateProgressBar(session = session, id = "postepz", value = 80, title = "Pasek postępu")
  
  
  shinyjs::hide(selector = '.progress-group:has(#postepz)')
  updateProgressBar(session = session, id = "postepz", value = 0, title = "Pasek postępu")
  
})


#shinyjs::click("zastosuj")
observeEvent(input$zastosuj,{

  if (input$agregat == 'Grupa produktowa zamówienia') {
    
    shinyjs::show(selector = '.progress-group:has(#postep)')
    updateProgressBar(session = session, id = "postep", value = 0, title = "Pasek postępu")
    
    nazwa_firstnoda <- ifelse(length(input$glownedanex)>1, paste0(input$glownedanex, collapse = ifelse(input$warn=='|',' OR ',' AND ')), paste(input$glownedanex))
    #####po grupie produktowej####
    if (input$dokładny == 'Uwzględnij multicategory') {
      if(length(input$glownedanex)>1) {
        piotrek3 <- reactive({
          x <- paste0(" products_group.x %like% '\\\\<",input$glownedanex,"\\\\>' ", collapse =  input$warn)
          paste0("(", x, ")") 
        })
        
        data1 <- data[piotrek3() %>% parse(text = .) %>% eval()]
        
      } else{
        data1 <- data[products_group.x_1 == input$glownedanex | (products_group.x_1 == 'multi' & products_group.x %like% paste0('\\<',input$glownedanex,'\\>'))]
      }
      
    } else {
      if(length(input$glownedanex)>1) {
        piotrek3 <- reactive({
          x <- paste0(" products_group.x_1 %like% '\\\\<",input$glownedanex,"\\\\>' ", collapse =  input$warn)
          paste0("(", x, ")") 
        })
        
        data1 <- data[piotrek3() %>% parse(text = .) %>% eval()]
        
      } else{
        data1 <- data[products_group.x_1 == input$glownedanex] 
      }
      
    }
    
    data1 <- data1[action_channel.x %in% input$drugiedanex & action_channel.y %in% input$drugiedaney & campaign_product_group.x %in% input$trzeciedanex & 
                     campaign_product_group.y %in% input$trzeciedaney & czy_bezposrednia %in% input$bezpo & cross %in% input$cross & program.x %in% input$programx
                   & program.y %in% input$programy & client_sex %in% input$plec & last_order_client_age_range %in% input$wiek & language_code %in% input$kraj,]
    
    if (!is.null(input$data)) {
      data1 <- data1[substr(added_datetime.x,1,7) >= substr(input$data[1],1,7) &
                       substr(added_datetime.x,1,7) <= substr(input$data[length(input$data)],1,7)]
    }
    
    updateProgressBar(session = session, id = "postep", value = 10, title = "Pasek postępu")
    if (nrow(data1)>=2) {
      dni <- data1[,.(srednie_dni = mean(roznica_pp)), by = products_group.y_1]
      
      wartoscx <- data1[,.(srednia_wartosc.x_zl = mean(a_current_price.x),
                           srednia_wartosc.x_e = mean(a_current_price.x)/4.5), by = products_group.y_1]
      wartoscy <- data1[,.(srednia_wartosc.y_zl = mean(a_current_price.y),
                           srednia_wartosc.y_e = mean(a_current_price.y)/4.5), by = products_group.y_1]
      komentarz <- merge(dni,wartoscx, by = "products_group.y_1")
      komentarz <- merge(komentarz, wartoscy, by = "products_group.y_1")
      komentarz$koment <- paste0("Średnia ilosc dni miedzy zamowieniami:", round(komentarz$srednie_dni, digits = 1),
                                 "<br>Średnia wartosc produktu zamoweinia X (zł):", round(komentarz$srednia_wartosc.x_zl, digits = 1), 
                                 "<br>Średnia wartosc produktu zamoweinia X (€):", round(komentarz$srednia_wartosc.x_e, digits = 1), 
                                 "<br>Średnia wartosc produktu zamowienia Y (zł):", round(komentarz$srednia_wartosc.y_zl, digits = 1),
                                 "<br>Średnia wartosc produktu zamowienia Y (€):", round(komentarz$srednia_wartosc.y_e, digits = 1))
      komentarz[, srednie_dni := NULL]
      komentarz[, srednia_wartosc.x_zl := NULL]
      komentarz[, srednia_wartosc.y_zl := NULL]
      komentarz[, srednia_wartosc.x_e := NULL]
      komentarz[, srednia_wartosc.y_e := NULL]
      
      data2 <- data1[,.N, by = products_group.y_1]
      
      Nzwykle <- 0.01*nrow(data1)
      # if (Nzwykle >= 5) {
      #   Nzwykle <- Nzwykle
      # } else {
      #   Nzwykle <- 5
      # }
      
      data3 <- data2[N<Nzwykle & products_group.y_1 != 'multi', products_group.y_1 := 'inne']
      
      data4 <- data3[,.(N = sum(N)), by = products_group.y_1]
      data4 <- merge(data4, komentarz, by = 'products_group.y_1', all.x = T)
      data4 <- data4[order(-N),]
      
      data5 <- data4[, target := seq_len(.N)]
      data5 <- data5[, Zamowienie_X := 0]
      data5[, kolor := kolor]
      data5$kolor[which(data5[,products_group.y_1] == 'inne')]<- 'green'
      
      if (input$dokładny == 'Uwzględnij multicategory') {
        if(length(input$glownedanex)>1) {
          piotrek4 <- reactive({
            x <- paste0(" products_group.x %like% '\\\\<",input$glownedanex,"\\\\>' ", collapse =  input$warn)
            paste0("(", x, ")") 
          })
          
          data_multi1 <- data_multi[piotrek3() %>% parse(text = .) %>% eval()]
          
        } else{
          data_multi1 <- data_multi[products_group.x_1 == input$glownedanex | (products_group.x_1 == 'multi' & products_group.x %like% paste0('\\<',input$glownedanex,'\\>'))]
        }
        
      } else {
        if(length(input$glownedanex)>1) {
          piotrek4 <- reactive({
            x <- paste0(" products_group.x_1 %like% '\\\\<",input$glownedanex,"\\\\>' ", collapse =  input$warn)
            paste0("(", x, ")") 
          })
          
          data_multi1 <- data_multi[piotrek3() %>% parse(text = .) %>% eval()]
          
        } else{
          data_multi1 <- data_multi[products_group.x_1 == input$glownedanex] 
        }
        
      }
      data_multi1 <- data_multi1[action_channel.x %in% input$drugiedanex & action_channel.y %in% input$drugiedaney & 
                                   campaign_product_group.x %in% input$trzeciedanex & campaign_product_group.y %in% input$trzeciedaney & 
                                   czy_bezposrednia %in% input$bezpo & cross %in% input$cross & program.x %in% input$programx & program.y %in% input$programy & 
                                   client_sex %in% input$plec & last_order_client_age_range %in% input$wiek & language_code %in% input$kraj,]
      if (!is.null(input$data)) {
        data_multi1 <- data_multi1[substr(added_datetime.x,1,7) >= substr(input$data[1],1,7) &
                                     substr(added_datetime.x,1,7) <= substr(input$data[length(input$data)],1,7)]
      }
      data_multi1 <- data_multi1[order_id.x %in% data1$order_id.x]
      
      updateProgressBar(session = session, id = "postep", value = 30, title = "Pasek postępu")
      if (nrow(data_multi1)>=1) {
        data_multi2 <- data_multi1[,.N, by = grupy_produktowe.y]
        
        Nmulti <- 0.01*nrow(data_multi1)
        # if (Nmulti >= 2) {
        #   Nmulti <- Nmulti
        # } else {
        #   Nmulti <- 2
        # }
        
        data_multi3 <- data_multi2[N<Nmulti, grupy_produktowe.y := 'inne']
        data_multi4 <- data_multi3[,.(N = sum(N)), by = grupy_produktowe.y]
        data_multi4 <- data_multi4[order(-N),]
        data_multi5 <- data_multi4[, target := (seq_len(.N)+nrow(data5))]
        data_multi5 <- data_multi5[, Zamowienie_X := data5[which(data5$products_group.y_1 == 'multi')]$target]
        data_multi5[, kolor := kolor]
        data_multi5$kolor[which(data_multi5[,grupy_produktowe.y] == 'inne')]<- 'yellow'
        
        names(data_multi5) <- c('products_group.y_1','N', 'target', 'Zamowienie_X', 'kolor')       
        
        data6 <- rbind(data5, data_multi5, fill=T)
        data6 <- data6[order(Zamowienie_X,-N)]
        
        
        nodes <- data6[,list(products_group.y_1, N, target, Zamowienie_X, kolor)]
        node0 <- data.table(products_group.y_1 = nazwa_firstnoda, N = nrow(data1), target = 0, Zamowienie_X = Inf, kolor = kolor)
        nodes <- rbind(node0, nodes)
        nodes[Zamowienie_X == 0, procenty := N/nodes$N[1]]
        nodes[products_group.y_1 == 'multi', N := nrow(data_multi1)]
        nodes[Zamowienie_X == nodes[products_group.y_1 == 'multi', target], procenty := N/nodes$N[which(nodes[,products_group.y_1] == 'multi')]]
        nodes[Zamowienie_X == 0, X := 0.5]
        nodes[Zamowienie_X == Inf, X := 0.005]
        nodes[Zamowienie_X == nodes[products_group.y_1 == 'multi', target], X := 0.995]
        nodes[Zamowienie_X == Inf, Y := 0.000001- (((nodes$N[2]/sum(nodes$N[2:nrow(data4)+1]))/2.4))]
        
        for (i in 2:(nrow(data4)+1)) {
          nodes$Y[i] = (nodes$Y[i-1] + ((nodes$N[i]/sum(nodes$N[2:nrow(data4)+1])))/2.2) + ((1/nrow(data4)/2))
        }
        
        nodes[Zamowienie_X == nodes[products_group.y_1 == 'multi', target], Y := 0.000001]
        
        for (i in (nrow(data4)+3):(nrow(nodes))) {
          nodes$Y[i] = nodes$Y[i-1]  + (nodes$N[i]/sum(nodes$N[(nrow(data4)+2):nrow(nodes)]))/5 + (1/(nrow(nodes)-nrow(data4)-1)/5)*4
        }
        
        nodes[Zamowienie_X == Inf, Y := 0.5]
        
        if (which(nodes[,products_group.y_1] == 'multi') != 2){
          nodes$Y[which(nodes[,products_group.y_1] == 'multi')] <- (nodes$Y[which(nodes[,products_group.y_1] == 'multi')-1]/40)*15 + (nodes$Y[which(nodes[,products_group.y_1] == 'multi')+1]/40)*25
        }
        
        if (which(nodes[,products_group.y_1] == 'multi') != 3 & which(nodes[,products_group.y_1] == 'multi') != 2) {
          for (i in (which(nodes[,products_group.y_1] == 'multi')-1):3) {
            nodes$Y[i] <- nodes$Y[i-1]/3 + nodes$Y[i+1]/1.5
          }
        }
        nodes$procenty <- paste0(round(nodes$procenty ,digits =4) * 100, '%')
        nodes$procenty[1] <-  " "
        nodes$products_group.y_1[which(nodes[,Zamowienie_X] == nodes[products_group.y_1 == 'multi', target])] <- paste(nodes$products_group.y_1[which(nodes[,Zamowienie_X] == nodes[products_group.y_1 == 'multi', target])], nodes$procenty[which(nodes[,Zamowienie_X] == nodes[products_group.y_1 == 'multi', target])])
        nodes$products_group.y_1[which(nodes[,Zamowienie_X] == 0)] <- paste(nodes$procenty[which(nodes[,Zamowienie_X] == 0)], nodes$products_group.y_1[which(nodes[,Zamowienie_X] == 0)])
        
        updateProgressBar(session = session, id = "postep", value = 50, title = "Pasek postępu")
        #####inne_multi####
        inne_multi_ <- data_multi1[,.N, by = grupy_produktowe.y]
        inne_multi <- inne_multi_[N<Nmulti]
        inne_multi[, udział := paste0(round(N/nrow(data_multi1), 4) * 100, "%")]
        inne_multi <- inne_multi[order(-N)]
        
        output$inne_multi <- DT::renderDataTable({
          datatable(
            inne_multi,
            filter = 'none',
            rownames = FALSE,
            colnames = c('Grupy produktowe z <span style="color:#e6ac00">inne</span> w kategorii multi', 'Liczba zamówień', 'Udział procentowy'),
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
              dom ='ltp'
            )) 
          
        })
      } else {
        data6 <- data5[order(Zamowienie_X,-N)]
        updateProgressBar(session = session, id = "postep", value = 50, title = "Przetwarzanie danych")
        nodes <- data6[,list(products_group.y_1, N, target, Zamowienie_X, kolor)]
        node0 <- data.table(products_group.y_1 = nazwa_firstnoda, N = nrow(data1), target = 0, Zamowienie_X = Inf, kolor = kolor)
        nodes <- rbind(node0, nodes)
        nodes[Zamowienie_X == 0, procenty := N/nodes$N[1]]
        nodes[Zamowienie_X == 0, X := 0.5]
        nodes[Zamowienie_X == Inf, X := 0.005]
        nodes[Zamowienie_X == Inf, Y := 0.5]
        nodes$procenty <- paste0(round(nodes$procenty ,digits =4) * 100, '%')
        nodes$procenty[1] <-  " "
        nodes$products_group.y_1[which(nodes[,Zamowienie_X] == 0)] <- paste(nodes$procenty[which(nodes[,Zamowienie_X] == 0)], nodes$products_group.y_1[which(nodes[,Zamowienie_X] == 0)])
        
        output$inne_multi <- NULL
      }
      updateProgressBar(session = session, id = "postep", value = 60, title = "Formowanie tabeli")
      
      data6$koment[is.na(data6$koment)] <- " "
      data6$koment <- paste0('Liczba zamówień: ', format(data6$N, big.mark = " ", big.interval = 3L), '<br>', data6$koment)
      
      inne_ <- data1[,.N, by = products_group.y_1]
      inne <- inne_[N<Nzwykle]
      inne[, udział := paste0(round(N/nrow(data1), 4) * 100, "%")]
      inne <- inne[order(-N)]
      
      output$inne <- DT::renderDataTable({
        datatable(
          inne,
          filter = 'none',
          rownames = FALSE,
          colnames = c('Grupy produktowe z <span style="color:#006600">inne</span></span>', 'Liczba zamówień', 'Udział procentowy'),
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
            dom ='ltp'
          )) 
        
      })
      updateProgressBar(session = session, id = "postep", value = 80, title = "Wyświetlanie")
      
      output$distPlot <- renderPlotly({
        plot_ly(
          type = "sankey",
          domain = list(
            x =  c(0,1),
            y =  c(0,1)
          ),
          orientation = "h",
          valueformat = ".^ #3,.0f",
          arrangement = "freeform",
          
          textfont = list(
            size = "automatic"  
          ),
          
          node = list(
            label = nodes$products_group.y_1,
            x = nodes$X,
            y = nodes$Y,
            color = nodes$kolor,
            pad = 30,
            thickness = 15,
            line = list(
              color = 'black',
              width = 1
            )
          ),
          
          link = list(
            source = data6$Zamowienie_X,
            target = data6$target,
            value =  data6$N,
            color = kolor1,
            label = data6$koment,
            hovertemplate = '%{label}<extra></extra>'
          )
          
        ) %>% 
          layout(
            title = "",
            paper_bgcolor = '#eeecff',
            font = list(size = "automatic")
          )
        
      })
      #####za mao####
    } else {
      output$distPlot <- renderPlotly({
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
          
        ) %>% 
          add_annotations(
            x = 0.5,
            y = 0.95,
            text = "Za mało zamówień w bazie - wybierz inne ograniczenia albo zmień grupę produktową.",
            font = list(
              size = 30,
              color = 'firebrick2'
            ),
            showarrow = FALSE
          )
        
      })
      
    }
    
    
  } 
  
  if (input$agregat == 'Kanał reklamy') {
    #####po kanale#### 
    nazwa_firstnoda <- ifelse(length(input$glownedanex)>1, paste0(input$glownedanex, collapse = ifelse(input$warn=='|',' OR ',' AND ')), paste(input$glownedanex))
    
    if(length(input$glownedanex)>1) {
      piotrek3 <- reactive({
        x <- paste0(" action_channel.x %like% '\\\\<",input$glownedanex,"\\\\>' ", collapse =  input$warn)
        paste0("(", x, ")") 
      })
      
      data1 <- data[piotrek3() %>% parse(text = .) %>% eval()]
      
    } else{
      data1 <- data[action_channel.x == input$glownedanex]
    }
    
    
    data1 <- data1[products_group.x_1 %in% input$drugiedanex & products_group.y_1 %in% input$drugiedaney & 
                     campaign_product_group.x %in% input$trzeciedanex & campaign_product_group.y %in% input$trzeciedaney & 
                     czy_bezposrednia %in% input$bezpo & cross %in% input$cross & program.x %in% input$programx]
    data1 <- data1[program.y %in% input$programy]
    data1 <- data1[client_sex %in% input$plec]
    data1 <- data1[last_order_client_age_range %in% input$wiek]
    data1 <- data1[language_code %in% input$kraj]
    
    if (!is.null(input$data)){
      data1 <- data1[added_datetime.x >= substr(input$data[1],1,7) &
                       added_datetime.x <= substr(input$data[length(input$data)],1,7)]
      
    }
    
    if (nrow(data1)>=2) {
      
      dni <- data1[,.(srednie_dni = mean(roznica_pp)), by = action_channel.y]
      
      wartoscx <- data1[,.(srednia_wartosc.x_zl = mean(a_current_price.x),
                           srednia_wartosc.x_e = mean(a_current_price.x)/4.5), by = action_channel.y]
      wartoscy <- data1[,.(srednia_wartosc.y_zl = mean(a_current_price.y),
                           srednia_wartosc.y_e = mean(a_current_price.y)/4.5), by = action_channel.y]
      komentarz <- merge(dni,wartoscx, by = "action_channel.y")
      komentarz <- merge(komentarz, wartoscy, by = "action_channel.y")
      komentarz$koment <- paste0("Średnia ilosc dni miedzy zamowieniami:", round(komentarz$srednie_dni, digits = 1),
                                 "<br>Średnia wartosc produktu zamoweinia X (zł):", round(komentarz$srednia_wartosc.x_zl, digits = 1), 
                                 "<br>Średnia wartosc produktu zamoweinia X (€):", round(komentarz$srednia_wartosc.x_e, digits = 1), 
                                 "<br>Średnia wartosc produktu zamowienia Y (zł):", round(komentarz$srednia_wartosc.y_zl, digits = 1),
                                 "<br>Średnia wartosc produktu zamowienia Y (€):", round(komentarz$srednia_wartosc.y_e, digits = 1))
      komentarz[, srednie_dni := NULL]
      komentarz[, srednia_wartosc.x_zl := NULL]
      komentarz[, srednia_wartosc.y_zl := NULL]
      komentarz[, srednia_wartosc.x_e := NULL]
      komentarz[, srednia_wartosc.y_e := NULL]
      

      data2 <- data1[,.N, by = action_channel.y]
      
      data3 <- data2[,.(N = sum(N)), by = action_channel.y]
      data4 <- merge(data3, komentarz, by = 'action_channel.y', all.x = T)
      data4 <- data4[order(-N),]
      
      data5 <- data4[, target := seq_len(.N)]
      data5 <- data5[, Zamowienie_X := 0]
      
      data6 <- data5[order(Zamowienie_X,-N)]
      
      
      nodes <- data6[,list(action_channel.y, N, target, Zamowienie_X)]
      node0 <- data.table(action_channel.y = nazwa_firstnoda, N = nrow(data1), target = 0, Zamowienie_X = Inf)
      nodes <- rbind(node0, nodes)
      nodes[Zamowienie_X == 0, procenty := N/nodes$N[1]]
      nodes[Zamowienie_X == 0, X := 0.5]
      nodes[Zamowienie_X == Inf, X := 0.005]
      
      nodes[Zamowienie_X == Inf, Y := 0.5]
      
      nodes$procenty <- paste0(round(nodes$procenty ,digits =4) * 100, '%')
      nodes$procenty[1] <-  " "
      nodes$action_channel.y[which(nodes[,Zamowienie_X] == 0)] <- paste(nodes$procenty[which(nodes[,Zamowienie_X] == 0)], nodes$action_channel.y[which(nodes[,Zamowienie_X] == 0)])
      
      output$inne_multi <- NULL
      output$inne <- NULL
      
      data6$koment[is.na(data6$koment)] <- " "
      data6$koment <- paste0('Liczba zamówień: ', format(data6$N, big.mark = " ", big.interval = 3L), '<br>', data6$koment)
      
      output$distPlot <- renderPlotly({
        plot_ly(
          type = "sankey",
          domain = list(
            x =  c(0,1),
            y =  c(0,1)
          ),
          orientation = "h",
          valueformat = ".^ #3,.0f",
          arrangement = "freeform",
          
          textfont = list(
            size = "automatic"  
          ),
          
          node = list(
            label = nodes$action_channel.y,
            x = nodes$X,
            y = nodes$Y,
            color = kolor,
            pad = 30,
            thickness = 15,
            line = list(
              color = "black",
              width = 1
            )
          ),
          
          link = list(
            source = data6$Zamowienie_X,
            target = data6$target,
            value =  data6$N,
            color = kolor1,
            label = data6$koment,
            hovertemplate = '%{label}<extra></extra>'
          )
          
        ) %>% 
          layout(
            title = "",
            paper_bgcolor = '#eeecff',
            font = list(size = "automatic")
          )
        
      })
      
    } else {
      output$distPlot <- renderPlotly({
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
          
          
        ) %>% 
          add_annotations(
            x = 0.5,
            y = 0.95,
            text = "Za mało zamówień w bazie - wybierz inne ograniczenia albo zmień Kanał reklamy.",
            font = list(
              size = 30,
              color = 'firebrick2'
            ),
            showarrow = FALSE
          )
        
      })
      
    }
  }
  
  if (input$agregat == 'Grupa produktowa kampanii') {
    
    #####po grupie kampanii####
    nazwa_firstnoda <- ifelse(length(input$glownedanex)>1, paste0(input$glownedanex, collapse = ifelse(input$warn=='|',' OR ',' AND ')), paste(input$glownedanex))
    
    if (input$dokładny == 'Uwzględnij multicategory') {
      if(length(input$glownedanex)>1) {
        piotrek3 <- reactive({
          x <- paste0(" campaign_product_group.x %like% '\\\\<",input$glownedanex,"\\\\>' ", collapse =  input$warn)
          paste0("(", x, ")") 
        })
        
        data1 <- data[piotrek3() %>% parse(text = .) %>% eval()]
        
      } else{
        data1 <- data[campaign_product_group.x == input$glownedanex | campaign_product_group.x %like% paste0('\\<',input$glownedanex,'\\>')]
      }
      
    } else {
      if(length(input$glownedanex)>1) {
        piotrek3 <- reactive({
          x <- paste0(" campaign_product_group.x %like% '\\\\<",input$glownedanex,"\\\\>' ", collapse =  input$warn)
          paste0("(", x, ")") 
        })
        
        data1 <- data[piotrek3() %>% parse(text = .) %>% eval()]
        
      } else{
        data1 <- data[campaign_product_group.x == input$glownedanex]
      }
      
    }
    
    
    data1 <- data1[action_channel.x %in% input$drugiedanex]
    data1 <- data1[action_channel.y %in% input$drugiedaney]
    data1 <- data1[products_group.x_1 %in% input$trzeciedanex]
    data1 <- data1[products_group.y_1 %in% input$trzeciedaney]
    data1 <- data1[czy_bezposrednia %in% input$bezpo]
    data1 <- data1[cross %in% input$cross]
    data1 <- data1[program.x %in% input$programx]
    data1 <- data1[program.y %in% input$programy]
    data1 <- data1[client_sex %in% input$plec]
    data1 <- data1[last_order_client_age_range %in% input$wiek]
    data1 <- data1[language_code %in% input$kraj]
    
    if (!is.null(input$data)) {
      data1 <- data1[substr(added_datetime.x,1,7) >= substr(input$data[1],1,7) &
                       substr(added_datetime.x,1,7) <= substr(input$data[length(input$data)],1,7)]
    }
    
    updateProgressBar(session = session, id = "postep", value = 10, title = "Pasek postępu")
    
    if (input$kamp == 'Grupa produktowa zamówienia') {
      if (nrow(data1)>=2) {
        dni <- data1[,.(srednie_dni = mean(roznica_pp)), by = products_group.y_1]

        wartoscx <- data1[,.(srednia_wartosc.x_zl = mean(a_current_price.x),
                             srednia_wartosc.x_e = mean(a_current_price.x)/4.5), by = products_group.y_1]
        wartoscy <- data1[,.(srednia_wartosc.y_zl = mean(a_current_price.y),
                             srednia_wartosc.y_e = mean(a_current_price.y)/4.5), by = products_group.y_1]
        komentarz <- merge(dni,wartoscx, by = "products_group.y_1")
        komentarz <- merge(komentarz, wartoscy, by = "products_group.y_1")
        komentarz$koment <- paste0("Średnia ilosc dni miedzy zamowieniami:", round(komentarz$srednie_dni, digits = 1),
                                   "<br>Średnia wartosc produktu zamoweinia X (zł):", round(komentarz$srednia_wartosc.x_zl, digits = 1), 
                                   "<br>Średnia wartosc produktu zamoweinia X (€):", round(komentarz$srednia_wartosc.x_e, digits = 1), 
                                   "<br>Średnia wartosc produktu zamowienia Y (zł):", round(komentarz$srednia_wartosc.y_zl, digits = 1),
                                   "<br>Średnia wartosc produktu zamowienia Y (€):", round(komentarz$srednia_wartosc.y_e, digits = 1))
        komentarz[, srednie_dni := NULL]
        komentarz[, srednia_wartosc.x_zl := NULL]
        komentarz[, srednia_wartosc.y_zl := NULL]
        komentarz[, srednia_wartosc.x_e := NULL]
        komentarz[, srednia_wartosc.y_e := NULL]
        
        
        data2 <- data1[,.N, by = products_group.y_1]
        
        Nzwykle <- 0.01*nrow(data1)
        # if (Nzwykle >= 5) {
        #   Nzwykle <- Nzwykle
        # } else {
        #   Nzwykle <- 5
        # }
        
        data3 <- data2[N<Nzwykle & products_group.y_1 != 'multi', products_group.y_1 := 'inne']
        
        data4 <- data3[,.(N = sum(N)), by = products_group.y_1]
        data4 <- merge(data4, komentarz, by = 'products_group.y_1', all.x = T)
        data4 <- data4[order(-N),]
        
        data5 <- data4[, target := seq_len(.N)]
        data5 <- data5[, Zamowienie_X := 0]
        data5[, kolor := kolor]
        data5$kolor[which(data5[,products_group.y_1] == 'inne')]<- 'green'
        
        if (input$dokładny == 'Uwzględnij multicategory') {
          if(length(input$glownedanex)>1) {
            piotrek4 <- reactive({
              x <- paste0(" campaign_product_group.x %like% '\\\\<",input$glownedanex,"\\\\>' ", collapse =  input$warn)
              paste0("(", x, ")") 
            })
            
            data_multi1 <- data_multi[piotrek3() %>% parse(text = .) %>% eval()]
            
          } else{
            data_multi1 <- data_multi[campaign_product_group.x == input$glownedanex | campaign_product_group.x %like% paste0('\\<',input$glownedanex,'\\>')]
          }
          
        } else {
          if(length(input$glownedanex)>1) {
            piotrek4 <- reactive({
              x <- paste0(" campaign_product_group.x %like% '\\\\<",input$glownedanex,"\\\\>' ", collapse =  input$warn)
              paste0("(", x, ")") 
            })
            
            data_multi1 <- data_multi[piotrek3() %>% parse(text = .) %>% eval()]
            
          } else{
            data_multi1 <- data_multi[campaign_product_group.x == input$glownedanex]
          }
          
        }
        
        data_multi1 <- data_multi1[action_channel.x %in% input$drugiedanex]
        data_multi1 <- data_multi1[action_channel.y %in% input$drugiedaney]
        data_multi1 <- data_multi1[products_group.x_1 %in% input$trzeciedanex]
        data_multi1 <- data_multi1[products_group.y_1 %in% input$trzeciedaney]
        data_multi1 <- data_multi1[czy_bezposrednia %in% input$bezpo]
        data_multi1 <- data_multi1[cross %in% input$cross]
        data_multi1 <- data_multi1[program.x %in% input$programx]
        data_multi1 <- data_multi1[program.y %in% input$programy]
        data_multi1 <- data_multi1[client_sex %in% input$plec]
        data_multi1 <- data_multi1[last_order_client_age_range %in% input$wiek]
        data_multi1 <- data_multi1[language_code %in% input$kraj]
        if (!is.null(input$data)) {
          data_multi1 <- data_multi1[substr(added_datetime.x,1,7) >= substr(input$data[1],1,7) &
                                       substr(added_datetime.x,1,7) <= substr(input$data[length(input$data)],1,7)]
        }
        updateProgressBar(session = session, id = "postep", value = 30, title = "Pasek postępu")
        if (nrow(data_multi1)>=1) {
          data_multi2 <- data_multi1[,.N, by = grupy_produktowe.y]
          
          Nmulti <- 0.01*nrow(data_multi1)
          # if (Nmulti >= 2) {
          #   Nmulti <- Nmulti
          # } else {
          #   Nmulti <- 2
          # }
          
          data_multi3 <- data_multi2[N<Nmulti, grupy_produktowe.y := 'inne']
          data_multi4 <- data_multi3[,.(N = sum(N)), by = grupy_produktowe.y]
          data_multi4 <- data_multi4[order(-N),]
          data_multi5 <- data_multi4[, target := (seq_len(.N)+nrow(data5))]
          data_multi5 <- data_multi5[, Zamowienie_X := data5[which(data5$products_group.y_1 == 'multi')]$target]
          data_multi5[, kolor := kolor]
          data_multi5$kolor[which(data_multi5[,grupy_produktowe.y] == 'inne')]<- 'yellow'
          
          names(data_multi5) <- c('products_group.y_1','N', 'target', 'Zamowienie_X', 'kolor')
          
          data6 <- rbind(data5, data_multi5, fill=T)
          data6 <- data6[order(Zamowienie_X,-N)]
          
          nodes <- data6[,list(products_group.y_1, N, target, Zamowienie_X, kolor)]
          node0 <- data.table(products_group.y_1 = nazwa_firstnoda, N = nrow(data1), target = 0, Zamowienie_X = Inf, kolor = kolor)
          nodes <- rbind(node0, nodes)
          nodes[Zamowienie_X == 0, procenty := N/nodes$N[1]]
          nodes[products_group.y_1 == 'multi', N := nrow(data_multi1)]
          nodes[Zamowienie_X == nodes[products_group.y_1 == 'multi', target], procenty := N/nodes$N[which(nodes[,products_group.y_1] == 'multi')]]
          nodes[Zamowienie_X == 0, X := 0.5]
          nodes[Zamowienie_X == Inf, X := 0.005]
          nodes[Zamowienie_X == nodes[products_group.y_1 == 'multi', target], X := 0.995]
          nodes[Zamowienie_X == Inf, Y := 0.000001- (((nodes$N[2]/sum(nodes$N[2:nrow(data4)+1]))/2.4))]
          
          for (i in 2:(nrow(data4)+1)) {
            nodes$Y[i] = (nodes$Y[i-1] + ((nodes$N[i]/sum(nodes$N[2:nrow(data4)+1])))/2.2) + ((1/nrow(data4)/2))
          }
          
          nodes[Zamowienie_X == nodes[products_group.y_1 == 'multi', target], Y := 0.000001]
          
          for (i in (nrow(data4)+3):(nrow(nodes))) {
            nodes$Y[i] = nodes$Y[i-1]  + (nodes$N[i]/sum(nodes$N[(nrow(data4)+2):nrow(nodes)]))/5 + (1/(nrow(nodes)-nrow(data4)-1)/5)*4
          }
          
          nodes[Zamowienie_X == Inf, Y := 0.5]
          
          if (which(nodes[,products_group.y_1] == 'multi') != 2){
            nodes$Y[which(nodes[,products_group.y_1] == 'multi')] <- (nodes$Y[which(nodes[,products_group.y_1] == 'multi')-1]/40)*15 + (nodes$Y[which(nodes[,products_group.y_1] == 'multi')+1]/40)*25
          }
          
          if (which(nodes[,products_group.y_1] == 'multi') != 3 & which(nodes[,products_group.y_1] == 'multi') != 2) {
            for (i in (which(nodes[,products_group.y_1] == 'multi')-1):3) {
              nodes$Y[i] <- nodes$Y[i-1]/3 + nodes$Y[i+1]/1.5
            }
          }
          nodes$procenty <- paste0(round(nodes$procenty ,digits =4) * 100, '%')
          nodes$procenty[1] <-  " "
          nodes$products_group.y_1[which(nodes[,Zamowienie_X] == nodes[products_group.y_1 == 'multi', target])] <- paste(nodes$products_group.y_1[which(nodes[,Zamowienie_X] == nodes[products_group.y_1 == 'multi', target])], nodes$procenty[which(nodes[,Zamowienie_X] == nodes[products_group.y_1 == 'multi', target])])
          nodes$products_group.y_1[which(nodes[,Zamowienie_X] == 0)] <- paste(nodes$procenty[which(nodes[,Zamowienie_X] == 0)], nodes$products_group.y_1[which(nodes[,Zamowienie_X] == 0)])
          
          updateProgressBar(session = session, id = "postep", value = 50, title = "Pasek postępu")
          #####inne_multi####
          inne_multi_ <- data_multi1[,.N, by = grupy_produktowe.y]
          inne_multi <- inne_multi_[N<Nmulti]
          inne_multi[, udział := paste0(round(N/nrow(data_multi1), 4) * 100, "%")]
          inne_multi <- inne_multi[order(-N)]
          
          output$inne_multi <- DT::renderDataTable({
            datatable(
              inne_multi,
              filter = 'none',
              rownames = FALSE,
              colnames = c('Grupy produktowe z <span style="color:#e6ac00">inne</span> w kategorii multi', 'Liczba zamówień', 'Udział procentowy'),
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
                dom ='ltp'
              ))
            
          })
        } 
        else {
          data6 <- data5[order(Zamowienie_X,-N)]
          updateProgressBar(session = session, id = "postep", value = 50, title = "Przetwarzanie danych")
          nodes <- data6[,list(products_group.y_1, N, target, Zamowienie_X, kolor)]
          node0 <- data.table(products_group.y_1 = nazwa_firstnoda, N = nrow(data1), target = 0, Zamowienie_X = Inf, kolor = kolor)
          nodes <- rbind(node0, nodes)
          nodes[Zamowienie_X == 0, procenty := N/nodes$N[1]]
          nodes[Zamowienie_X == 0, X := 0.5]
          nodes[Zamowienie_X == Inf, X := 0.005]
          nodes[Zamowienie_X == Inf, Y := 0.5]
          nodes$procenty <- paste0(round(nodes$procenty ,digits =4) * 100, '%')
          nodes$procenty[1] <-  " "
          nodes$products_group.y_1[which(nodes[,Zamowienie_X] == 0)] <- paste(nodes$procenty[which(nodes[,Zamowienie_X] == 0)], nodes$products_group.y_1[which(nodes[,Zamowienie_X] == 0)])
          
          output$inne_multi <- NULL
        }
        updateProgressBar(session = session, id = "postep", value = 60, title = "Formowanie tabeli")
        
        data6$koment[is.na(data6$koment)] <- " "
        data6$koment <- paste0('Liczba zamówień: ', format(data6$N, big.mark = " ", big.interval = 3L), '<br>', data6$koment)
        
        inne_ <- data1[,.N, by = products_group.y_1]
        inne <- inne_[N<Nzwykle]
        inne[, udział := paste0(round(N/nrow(data1), 4) * 100, "%")]
        inne <- inne[order(-N)]
        
        output$inne <- DT::renderDataTable({
          datatable(
            inne,
            filter = 'none',
            rownames = FALSE,
            colnames = c('Grupy produktowe z <span style="color:#006600">inne</span></span>', 'Liczba zamówień', 'Udział procentowy'),
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
              dom ='ltp'
            ))
          
        })
        updateProgressBar(session = session, id = "postep", value = 80, title = "Wyświetlanie")
        
        output$distPlot <- renderPlotly({
          plot_ly(
            type = "sankey",
            domain = list(
              x =  c(0,1),
              y =  c(0,1)
            ),
            orientation = "h",
            valueformat = ".^ #3,.0f",
            arrangement = "freeform",
            
            textfont = list(
              size = "automatic"
            ),
            
            node = list(
              label = nodes$products_group.y_1,
              x = nodes$X,
              y = nodes$Y,
              color = nodes$kolor,
              pad = 30,
              thickness = 15,
              line = list(
                color = 'black',
                width = 1
              )
            ),
            
            link = list(
              source = data6$Zamowienie_X,
              target = data6$target,
              value =  data6$N,
              color = kolor1,
              label = data6$koment,
              hovertemplate = '%{label}<extra></extra>'
            )
            
          ) %>%
            layout(
              title = "",
              paper_bgcolor = '#eeecff',
              font = list(size = "automatic")
            )
          
        })
        #####za mao####
      } 
      else {
        output$distPlot <- renderPlotly({
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
            
          ) %>%
            add_annotations(
              x = 0.5,
              y = 0.95,
              text = "Za mało zamówień w bazie - wybierz inne ograniczenia albo zmień grupę produktową.",
              font = list(
                size = 30,
                color = 'firebrick2'
              ),
              showarrow = FALSE
            )
          
        })
        
      }
      
    }
    
    if (input$kamp == 'Grupa produktowa kampanii') {
      if (nrow(data1)>=2) {
        dni <- data1[,.(srednie_dni = mean(roznica_pp)), by = campaign_product_group.y]
        wartoscx <- data1[,.(srednia_wartosc.x_zl = mean(a_current_price.x),
                             srednia_wartosc.x_e = mean(a_current_price.x)/4.5), by = campaign_product_group.y]
        wartoscy <- data1[,.(srednia_wartosc.y_zl = mean(a_current_price.y),
                             srednia_wartosc.y_e = mean(a_current_price.y)/4.5), by = campaign_product_group.y]
        komentarz <- merge(dni,wartoscx, by = "campaign_product_group.y")
        komentarz <- merge(komentarz, wartoscy, by = "campaign_product_group.y")
        komentarz$koment <- paste0("Średnia ilosc dni miedzy zamowieniami:", round(komentarz$srednie_dni, digits = 1),
                                   "<br>Średnia wartosc produktu zamoweinia X (zł):", round(komentarz$srednia_wartosc.x_zl, digits = 1), 
                                   "<br>Średnia wartosc produktu zamoweinia X (€):", round(komentarz$srednia_wartosc.x_e, digits = 1), 
                                   "<br>Średnia wartosc produktu zamowienia Y (zł):", round(komentarz$srednia_wartosc.y_zl, digits = 1),
                                   "<br>Średnia wartosc produktu zamowienia Y (€):", round(komentarz$srednia_wartosc.y_e, digits = 1))
        komentarz[, srednie_dni := NULL]
        komentarz[, srednia_wartosc.x_zl := NULL]
        komentarz[, srednia_wartosc.y_zl := NULL]
        komentarz[, srednia_wartosc.x_e := NULL]
        komentarz[, srednia_wartosc.y_e := NULL]
        
        data2 <- data1[,.N, by = campaign_product_group.y]
        
        Nzwykle <- 0.01*nrow(data1)
        
        data2 <- data2[N<Nzwykle, campaign_product_group.y := 'inne']
        
        data3 <- data2[,.(N = sum(N)), by = campaign_product_group.y]
        data4 <- merge(data3, komentarz, by = 'campaign_product_group.y', all.x = T)
        data4 <- data4[order(-N),]
        
        data5 <- data4[, target := seq_len(.N)]
        data5 <- data5[, Zamowienie_X := 0]
        
        data6 <- data5[order(Zamowienie_X,-N)]
        
        
        nodes <- data6[,list(campaign_product_group.y, N, target, Zamowienie_X)]
        node0 <- data.table(campaign_product_group.y = nazwa_firstnoda, N = nrow(data1), target = 0, Zamowienie_X = Inf)
        nodes <- rbind(node0, nodes)
        nodes[Zamowienie_X == 0, procenty := N/nodes$N[1]]
        nodes[Zamowienie_X == 0, X := 0.5]
        nodes[Zamowienie_X == Inf, X := 0.005]
        
        nodes[Zamowienie_X == Inf, Y := 0.5]
        
        nodes$procenty <- paste0(round(nodes$procenty ,digits =4) * 100, '%')
        nodes$procenty[1] <-  " "
        nodes[,kolor := kolor]
        nodes[campaign_product_group.y == 'inne', kolor := 'green']
        nodes$campaign_product_group.y[which(nodes[,Zamowienie_X] == 0)] <- paste(nodes$procenty[which(nodes[,Zamowienie_X] == 0)], nodes$campaign_product_group.y[which(nodes[,Zamowienie_X] == 0)])
        
        output$inne_multi <- NULL
        output$inne <- NULL
        
        data6$koment[is.na(data6$koment)] <- " "
        data6$koment <- paste0('Liczba zamówień: ', format(data6$N, big.mark = " ", big.interval = 3L), '<br>', data6$koment)
        
        output$distPlot <- renderPlotly({
          plot_ly(
            type = "sankey",
            domain = list(
              x =  c(0,1),
              y =  c(0,1)
            ),
            orientation = "h",
            valueformat = ".^ #3,.0f",
            arrangement = "freeform",
            
            textfont = list(
              size = "automatic"  
            ),
            
            node = list(
              label = nodes$campaign_product_group.y,
              x = nodes$X,
              y = nodes$Y,
              color = nodes$kolor,
              pad = 30,
              thickness = 15,
              line = list(
                color = "black",
                width = 1
              )
            ),
            
            link = list(
              source = data6$Zamowienie_X,
              target = data6$target,
              value =  data6$N,
              color = kolor1,
              label = data6$koment,
              hovertemplate = '%{label}<extra></extra>'
            )
            
          ) %>% 
            layout(
              title = "",
              paper_bgcolor = '#eeecff',
              font = list(size = "automatic")
            )
          
        })
        
        inne_ <- data1[,.N, by = campaign_product_group.y]
        inne <- inne_[N<Nzwykle]
        inne[, udział := paste0(round(N/nrow(data1), 4) * 100, "%")]
        inne <- inne[order(-N)]
        
        output$inne <- DT::renderDataTable({
          datatable(
            inne,
            filter = 'none',
            rownames = FALSE,
            colnames = c('Grupy produktowe z <span style="color:#006600">inne</span></span>', 'Liczba zamówień', 'Udział procentowy'),
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
              dom ='ltp'
            )) 
          
        })
        
      } else {
        output$distPlot <- renderPlotly({
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
            
            
          ) %>% 
            add_annotations(
              x = 0.5,
              y = 0.95,
              text = "Za mało zamówień w bazie - wybierz inne ograniczenia albo zmień grupę produktową.",
              font = list(
                size = 30,
                color = 'firebrick2'
              ),
              showarrow = FALSE
            )
          
        })
        output$inne <- NULL  
      }
    }
    
    
  }
  
  
  shinyjs::hide(selector = '.progress-group:has(#postep)')
  updateProgressBar(session = session, id = "postep", value = 0, title = "Pasek postępu")
})
