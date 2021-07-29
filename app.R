library(shiny)
library(dbConnectBPO)
library(data.table)
library(DT)
library(shinydashboard)
library(ggplot2)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(shinyBS)
library(treemapify)
library(dplyr)

source("zac/zac.R")
### # ###
APP_NAME <<- "Multibuyers_przejscia"  # bardzo ważne aby ta nazwa pokrywała się z nazwą jaką nadaliśmy aplikacji w tabeli shiny.apps na anr1.infr.co
PROTECTED <<- T  # info czy nadajemy dostępy do aplikacji czy nie T/F
### # ###
 
#####ui####
ui <- dashboardPage(
  dashboardHeader(title = 'Multibuyers'),
  dashboardSidebar(
    
    
    
    width = "250px", 
    sidebarMenu(id = 'menu',
                useShinyjs(),
                shinyjs::hidden(actionButton('legenda',HTML("<center>  Legenda </center>"), block= T ,
                                             style="color: #fff; background-color: 	#4f8bf2;border-color: #191970; width:190px; height: 40px;font-size:16px;")),
                shinyjs::hidden(actionButton('legenda1',HTML("<center>  Legenda </center>"), block= T ,
                                             style="color: #fff; background-color: 	#4f8bf2;border-color: #191970; width:190px; height: 40px;font-size:16px;")),
                shinyjs::hidden(actionButton('legenda2',HTML("<center>  Legenda </center>"), block= T ,
                                             style="color: #fff; background-color: 	#4f8bf2;border-color: #191970; width:190px; height: 40px;font-size:16px;")),
                #nazwy zakladek                
                menuItem("Przejścia",
                         tabName = "przejscia"),
                menuItem("Dni",
                         tabName = "dni"),
                menuItem("Portfele konsumentów",
                         tabName = "wartosc"),
                menuItem(tabName = "blank", '', selected = TRUE)
                
                
    )),
  #####legendy####
  dashboardBody(
    tags$head(
      includeScript("www/js/getLogin.js"),
      
      tags$script(' 
                  $(document).ready(function () {
                  var przejscia = document.querySelector(".sidebar-menu li a[data-value=przejscia]");
                  var dni = document.querySelector(".sidebar-menu li a[data-value=dni]");
                  var wartosc = document.querySelector(".sidebar-menu li a[data-value=wartosc]");
                  var blank = document.querySelector(".sidebar-menu li a[data-value=blank]");
                  przejscia.style.display = "none";
                  dni.style.display = "none";
                  wartosc.style.display = "none";
                  blank.style.display = "none";
                  })
                  var client = new XMLHttpRequest();
                  client.open("GET", window.location.href, true);
                  client.send();
                  client.onreadystatechange = function() {
                  var response = client.getResponseHeader("X-Remote-User");
                  var link = window.location.href;
                  setTimeout(function() {Shiny.onInputChange("login", response)}, 10);
                  }
                  
                  ;'),
      bsModal("modal", "Legenda", trigger = "legenda",  HTML('<html>
                                                             <head>
                                                             </head>
                                                             <body>
                                                             <font face="Arial">
                                                             <table>
                                                             <tr>
                                                             <td style="padding-left:20px;"><strong style="font-size:18px; color:#3d4044">Karta przedstawia informacje o zamówieniach konsumentów typu Multibuyers.</strong></td>		
                                                             </tr>
                                                             <tr>
                                                             <br> <br>
                                                             </tr>
                                                             <tr>				
                                                             <td style="font-size:15px; color:#46484; padding-left:20px">Zamówienia konsumenta złożone po 2017 roku na statusie sent z Brandnowy/Centrum Innowacji/edukacji łączone są w pary (x,y), gdzie zamówienie X powstało przed zamówieniem Y.<br>
                                                             <br><u>Uwaga,</u> liczby na wykresach nie będą zbliżone do liczby konsumentów. Na przykład, jeżeli jeden konsument złożył 5 zamówień: Z1, Z2, Z3, Z4, Z5 to powstanie z nich (bez ograniczenia żadnymi filtrami) aż 10 par (X,Y), które mogą pojawić się na wykresie - (Z1,Z2), (Z2,Z3), (Z3,Z4), (Z4,Z5), (Z1,Z3), (Z1,Z4), (Z1,Z5), (Z2,Z4), (Z2,Z5), (Z3,Z5) <br><br>
                                                             </td>
                                                             </tr>
                                                             <tr>
                                                             <td style="font-size:15px; color:#46484; padding-left:20px">
                                                             <b> Zakładka Przejścia </b> <br>
                                                             Wykres przedstawia rozkład pochodzenia zamówienia Y ze względu na grupę produktową zamówienia/kanał reklamy przy ograniczeniach zadanych w filtrach. W przypadku grupy produktowej kampanii możemy dodatkowo wybrać agregację zamówienia Y - po grupie zamówienia lub kampanii.<br>
                                                             <br>Ze względu na jego czytelność, grupy produktowe stanowiące mniej niż 1% wszystkich zostały połączone w grupę <i>inne</i>, a ich szczegółowy rozkład znajduje się w tabeli pod wykresem.<br>
                                                             Rozbicie <i>multi</i> na kolejne podgrupy obrazuje jakie pojedyncze produkty znajdowały się w zamówieniach o więcej niż jednej grupie produktowej.<br>
                                                             <br>Wybierając więcej niż jedną grupę produktową/kanał należy pamiętać o odpowiednim warunku na ich łączeie. OR pokaże wszystkie pary, których zamówienie X miało jedną z wybranych grup. AND pokaże te, których każde zamówienie X miało <u>wszystkie</u> wybrane grupy.
                                                             <br>Zaznaczenie opcji <i>"Uwzględnij multicategory"</i> oznacza, że w zamówieniu X wybrana grupa zamówienia/kampanii mogła pojawić się razem z inną grupą. Przy wyborze <i>"Dokładne dopasowanie"</i> zamówienie X będzie miało dokładnie jedną grupę zamówienia/kampanii.
                                                             <br><br><b>Zakładka Najczęstsze crossy</b>
                                                             <br>Grupa zamówienia X jest zaznaczona na wykresie czarną czcionką, a zamówienia Y - białą. Wielkość kafelków jest proporcjonalna do liczby par zamówieniowych na danym przecięciu, która jest wyświetlana obok grupy zamówienia Y.<br>
                                                             <br> Filtrem "Ile crossów pokazać?" można wybrać ilość wyświetlanych crossów. W związku z tym suma ilości par zamówieniowych na kafelkach rzadko będzie przedstawiała wszystkie zamówienia z bazy, a jedynie największe crossy przy danych ograniczeniach.
                                                             <br> <br> <b>Wspólne filtry:</b>
                                                             <br>"<i>Czy para bezpośrednia?</i>" - Gdy ustawiony na <i>tak</i> wyświetlane są tylko pary, które dla danego konsumenta nastąpiły bezpośrednio po sobie.<br>
                                                             "<i>Czy cross</i>?" - Odpowiada na pytanie czy zamówienie X i Y miały różne grupy produktowe.<br>
                                                             "<i>Data dodania zamówienia X</i>" nie działa (pokazuje wszystkie rekordy), gdy pole wyboru jest puste.<br><br>
                                                             <tr>				
                                                             <td style="font-size:12px; color:#46484; padding-left:20px"><i>Tablice źródłowe są przeliczane codziennie.</i><br><br>
                                                             </td>
                                                             </tr>	
                                                             </table>
                                                             </font>
                                                             </body>
                                                             </html>
                                                             '
      )),
      
      bsModal("modal1", "Legenda", trigger = "legenda1",  HTML('<html>
                                                               <head>
                                                               </head>
                                                               <body>
                                                               <font face="Arial">
                                                               <table>
                                                               <tr>
                                                               <td style="padding-left:20px;"><strong style="font-size:18px; color:#3d4044">Karta przedstawia informacje o zamówieniach konsumentów typu Multibuyers.</strong></td>		
                                                               </tr>
                                                               <tr>
                                                               </tr>
                                                               <tr>				
                                                               <td style="font-size:15px; color:#46484; padding-left:20px">Zamówienia konsumenta złożone po 2017 roku na statusie sent z Brandnowy/Centrum Innowacji/edukacji łączone są w pary (x,y), gdzie zamówienie X powstało przed zamówieniem Y. W tej karcie brane są pod uwagę tylko zamówienia <u>bezpośrednie</u><br><br>
                                                               </td>
                                                               </tr>
                                                               <tr>
                                                               <td style="font-size:15px; color:#46484; padding-left:20px">
                                                               Tabela przedstawia średnią ilość dni pomiędzy zamówieniami według wybranej w filtrach agregacji, sortując je według liczby zamówień w kategorii pierwszej agregacji.<br>
                                                               <br>"<i>Czy cross</i>?" - Odpowiada na pytanie czy zamówienie X i Y miały różne grupy produktowe.<br>
                                                               "<i>Data dodania zamówienia X</i>" nie działa (pokazuje wszystkie rekordy), gdy pole wyboru jest puste.<br><br>
                                                               <tr>				
                                                               <td style="font-size:12px; color:#46484; padding-left:20px"><i>Tablice źródłowe są przeliczane codziennie.</i><br>
                                                               </td>
                                                               </tr>	
                                                               </table>
                                                               </font>
                                                               </body>
                                                               </html>
                                                               '
      )),
      
      bsModal("modal2", "Legenda", trigger = "legenda2",  HTML('<html>
                                                               <head>
                                                               </head>
                                                               <body>
                                                               <font face="Arial">
                                                               <table>
                                                               <tr>
                                                               <td style="padding-left:20px;"><strong style="font-size:18px; color:#3d4044">Karta przedstawia informacje o wartości <u>wszystkich</u> zamówień konsumentów Brandnovy i Centrum innowacji typu Multibuyers i Singlebuyers.<br></strong></td>		
                                                               </tr>
                                                               <tr>
                                                               </tr>
                                                               <tr>				
                                                               <td style="font-size:15px; color:#46484; padding-left:20px">Po wybraniu odpowiedniej opcji w filtrze <i>"Informacje o:"</i> możemy wybrać czy interesuje nas ostatnie zamówienie konsumenta, czy wszystkie jakie do dzisiaj złożył.<br><br>
                                                               </td>
                                                               </tr>
                                                               <tr>
                                                               <td style="font-size:15px; color:#46484; padding-left:20px">
                                                               Filtry "<i>Data wysłania ostatniego zamówienia</i>" oraz te dotyczące grup produktowych nie działają (pokazują wszystkie rekordy), gdy pole wyboru jest puste. Jest to szczególnie ważna informacja w przypadku filtra "<i>Data wysłania ostatniego zamówienia</i>". Ze względu na brak informacji dla niektórych konsumentów nawet ustawienie pełnego zakresu dat spowoduje wyświetlanie niepełnych danych.<br>
                                                               <br>
                                                               <tr>				
                                                               <td style="font-size:12px; color:#46484; padding-left:20px"><i>Tablice źródłowe są przeliczane codziennie.</i><br><br>
                                                               </td>
                                                               </tr>	
                                                               </table>
                                                               </font>
                                                               </body>
                                                               </html>
                                                               '
      )),
      
      useShinyjs()),
    tabItems(
      #####pierwsza karta####
      tabItem(tabName = 'przejscia',useShinyjs(),
              tabsetPanel(id = "tabs",
                          tabPanel('Przejścia', useShinyjs(),
                                   
                                   fluidRow( shinyjs::hidden(progressBar(id = "postep", value = 0, striped = T, display_pct = T, title = "Pasek postępu"))
                                   ),
                                   fluidRow(
                                     br(),
                                     column(2,
                                            selectInput(
                                              inputId = 'agregat',
                                              label = 'Agregacja zamówień',
                                              choices = c('Grupa produktowa zamówienia', 'Grupa produktowa kampanii', 'Kanał reklamy'),
                                              selected = 'Grupa produktowa zamówienia',
                                              multiple = FALSE
                                            )),
                                     column(2,
                                            selectInput(
                                              inputId = 'glownedanex',
                                              label = "Grupa produktowa zamowienia X",
                                              choices = sort(unique(data[products_group.x_1 != 'multi', products_group.x_1])),
                                              selected = "hearing",
                                              multiple = TRUE
                                            )),
                                     column(2,
                                            selectInput(inputId = 'warn', label = "Warunek na łączenie grup", choices = c('OR'='|', 'AND'='&'), selected = ('OR'='|'), multiple = FALSE)
                                     ),
                                     column(2,
                                            shinyjs::hidden(radioButtons(inputId = 'dokładny', label = NULL, choices = c('Dokładne dopasowanie', 'Uwzględnij multicategory'), selected = 'Uwzględnij multicategory'))
                                     ),
                                     column(2,
                                            shinyjs::hidden(radioButtons(inputId = 'kamp', label = "Agregacja zamówień Y", choices = c('Grupa produktowa kampanii', 'Grupa produktowa zamówienia'), selected = 'Grupa produktowa zamówienia'))
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(2,
                                            pickerInput(
                                              inputId = 'drugiedanex',
                                              label = 'Kanał reklamy X',
                                              choices = sort(unique(data[, action_channel.x])),
                                              selected = sort(unique(data[, action_channel.x])),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 8,
                                                selectedTextFormat = "count > 3",
                                                deselectAllText = 'Odznacz wszystkie',
                                                selectAllText = 'Zaznacz wszytskie',
                                                liveSearch = TRUE
                                              ),
                                              multiple = TRUE
                                            ),
                                            pickerInput(
                                              inputId = 'drugiedaney',
                                              label = 'Kanał reklamy Y',
                                              choices = sort(unique(data[, action_channel.y])),
                                              selected = sort(unique(data[, action_channel.y])),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 8,  
                                                selectedTextFormat = "count > 3",
                                                deselectAllText = 'Odznacz wszystkie',
                                                selectAllText = 'Zaznacz wszytskie',
                                                liveSearch = TRUE
                                              ),
                                              multiple = TRUE
                                            )),
                                     column(2,
                                            pickerInput(
                                              inputId = 'trzeciedanex',
                                              label = 'Grupa produktowa kampanii X',
                                              choices = sort(unique(data[, campaign_product_group.x])),
                                              selected = sort(unique(data[, campaign_product_group.x])),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 8,
                                                selectedTextFormat = "count > 3",
                                                deselectAllText = 'Odznacz wszystkie',
                                                selectAllText = 'Zaznacz wszytskie',
                                                liveSearch = TRUE
                                              ),
                                              multiple = TRUE
                                            ),
                                            pickerInput(
                                              inputId = 'trzeciedaney',
                                              label = 'Grupa produktowa kampanii Y',
                                              choices = sort(unique(data[, campaign_product_group.y])),
                                              selected = sort(unique(data[, campaign_product_group.y])),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 8,
                                                selectedTextFormat = "count > 3",
                                                deselectAllText = 'Odznacz wszystkie',
                                                selectAllText = 'Zaznacz wszytskie',
                                                liveSearch = TRUE
                                              ),
                                              multiple = TRUE
                                            )),
                                     column(2,
                                            pickerInput(
                                              inputId = 'programx',
                                              label = 'Program X',
                                              choices = c("P0", "P1", "P2", "P3", "DOK"),
                                              selected = c("P0", "P1"),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 8,  
                                                selectedTextFormat = "count > 3",
                                                deselectAllText = 'Odznacz wszystkie',
                                                selectAllText = 'Zaznacz wszytskie',
                                                liveSearch = TRUE
                                              ),
                                              multiple = TRUE
                                            ), 
                                            pickerInput(
                                              inputId = 'programy',
                                              label = 'Program Y',
                                              choices = c("P0", "P1", "P2", "P3", "DOK"),
                                              selected = c("P0", "P1"),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 8,  
                                                selectedTextFormat = "count > 3",
                                                deselectAllText = 'Odznacz wszystkie',
                                                selectAllText = 'Zaznacz wszytskie',
                                                liveSearch = TRUE
                                              ),
                                              multiple = TRUE 
                                            )),
                                     column(6,
                                            column(3,
                                                   pickerInput(
                                                     inputId = 'bezpo',
                                                     label = 'Czy para bezpośrednia?',
                                                     choices = c("tak", "nie"),
                                                     selected = "tak",
                                                     multiple = TRUE
                                                   ),
                                                   pickerInput(
                                                     inputId = 'cross',
                                                     label = 'Czy cross?',
                                                     choices = c("tak", "nie"),
                                                     selected = c("tak", "nie"),
                                                     multiple = TRUE
                                                   )),
                                            column(3,
                                                   pickerInput(
                                                     inputId = 'plec',
                                                     label = 'Płeć konsumenta',
                                                     choices = sort(unique(data[, client_sex])),
                                                     selected = sort(unique(data[, client_sex])),
                                                     multiple = TRUE
                                                   ),
                                                   pickerInput(
                                                     inputId = 'wiek',
                                                     label = 'Wiek konsumenta',
                                                     choices = sort(unique(data[, last_order_client_age_range])),
                                                     selected = sort(unique(data[, last_order_client_age_range])),
                                                     options = pickerOptions(
                                                       actionsBox = TRUE,
                                                       size = 8,  
                                                       selectedTextFormat = "count > 3",
                                                       deselectAllText = 'Odznacz wszystkie',
                                                       selectAllText = 'Zaznacz wszytskie',
                                                       liveSearch = TRUE
                                                     ),
                                                     multiple = TRUE
                                                   )),
                                            column(3,
                                                   pickerInput(
                                                     inputId = 'kraj',
                                                     label = 'Kraj',
                                                     choices = sort(unique(data[, language_code])),
                                                     selected = sort(unique(data[, language_code])),
                                                     options = pickerOptions(
                                                       actionsBox = TRUE,
                                                       size = 8,  
                                                       selectedTextFormat = "count > 3",
                                                       deselectAllText = 'Odznacz wszystkie',
                                                       selectAllText = 'Zaznacz wszytskie',
                                                       liveSearch = TRUE
                                                     ),
                                                     multiple = TRUE
                                                   ),
                                                   airDatepickerInput(
                                                     inputId = "data",
                                                     label = "Data dodania zamowienia X",
                                                     view = "months", minView = "months",
                                                     dateFormat = "MM yyyy", monthsField = "months",
                                                     range = T, 
                                                     minDate = paste0(min(data[!is.na(added_datetime.x), unique(added_datetime.x)]),'-01'),
                                                     maxDate = paste0(max(data[!is.na(added_datetime.x), unique(added_datetime.x)]),'-01'),
                                                     value = NULL,
                                                     addon = 'none'
                                                   )
                                            ),
                                            column(3, align = 'left',
                                                   actionButton("zastosuj" , "Zastosuj", style="color: #fff; background-color: #4a9949; border-color: #4a9949; padding:20px")
                                            )
                                     )
                                   ),
                                   plotlyOutput("distPlot", height = '1000px'),
                                   fluidRow(
                                     column(5, offset = 1, 
                                            DT::dataTableOutput("inne")),
                                     column(5,
                                            DT::dataTableOutput("inne_multi"))
                                   )
                          ),
                          #####zakladak####
                          tabPanel('Najczęstsze crossy', useShinyjs(),
                                   
                                   fluidRow( shinyjs::hidden(progressBar(id = "postepz", value = 0, striped = T, display_pct = T, title = "Pasek postępu"))
                                   ),
                                   fluidRow(
                                     column(2,
                                            selectInput(
                                              inputId = 'agregatz',
                                              label = 'Agregacja zamówień',
                                              choices = c('Grupa produktowa zamówienia', 'Grupa produktowa kampanii', 'Kanał reklamy'),
                                              selected = 'Grupa produktowa zamówienia',
                                              multiple = FALSE
                                            ))),
                                   fluidRow(
                                     column(2,
                                            pickerInput(
                                              inputId = 'drugiedanexz',
                                              label = 'Kanał reklamy X',
                                              choices = sort(unique(data[, action_channel.x])),
                                              selected = sort(unique(data[, action_channel.x])),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 8,
                                                selectedTextFormat = "count > 3",
                                                deselectAllText = 'Odznacz wszystkie',
                                                selectAllText = 'Zaznacz wszytskie',
                                                liveSearch = TRUE
                                              ),
                                              multiple = TRUE
                                            ),
                                            pickerInput(
                                              inputId = 'drugiedaneyz',
                                              label = 'Kanał reklamy Y',
                                              choices = sort(unique(data[, action_channel.y])),
                                              selected = sort(unique(data[, action_channel.y])),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 8,  
                                                selectedTextFormat = "count > 3",
                                                deselectAllText = 'Odznacz wszystkie',
                                                selectAllText = 'Zaznacz wszytskie',
                                                liveSearch = TRUE
                                              ),
                                              multiple = TRUE
                                            )),
                                     column(2,
                                            pickerInput(
                                              inputId = 'trzeciedanexz',
                                              label = 'Grupa produktowa kampanii X',
                                              choices = sort(unique(data[, campaign_product_group.x])),
                                              selected = sort(unique(data[, campaign_product_group.x])),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 8,
                                                selectedTextFormat = "count > 3",
                                                deselectAllText = 'Odznacz wszystkie',
                                                selectAllText = 'Zaznacz wszytskie',
                                                liveSearch = TRUE
                                              ),
                                              multiple = TRUE
                                            ),
                                            pickerInput(
                                              inputId = 'trzeciedaneyz',
                                              label = 'Grupa produktowa kampanii Y',
                                              choices = sort(unique(data[, campaign_product_group.y])),
                                              selected = sort(unique(data[, campaign_product_group.y])),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 8,
                                                selectedTextFormat = "count > 3",
                                                deselectAllText = 'Odznacz wszystkie',
                                                selectAllText = 'Zaznacz wszytskie',
                                                liveSearch = TRUE
                                              ),
                                              multiple = TRUE
                                            )),
                                     column(2,
                                            pickerInput(
                                              inputId = 'programxz',
                                              label = 'Program X',
                                              choices = c("P0", "P1", "P2", "P3", "DOK"),
                                              selected = c("P0", "P1"),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 8,  
                                                selectedTextFormat = "count > 3",
                                                deselectAllText = 'Odznacz wszystkie',
                                                selectAllText = 'Zaznacz wszytskie',
                                                liveSearch = TRUE
                                              ),
                                              multiple = TRUE
                                            ), 
                                            pickerInput(
                                              inputId = 'programyz',
                                              label = 'Program Y',
                                              choices = c("P0", "P1", "P2", "P3", "DOK"),
                                              selected = c("P0", "P1"),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 8,  
                                                selectedTextFormat = "count > 3",
                                                deselectAllText = 'Odznacz wszystkie',
                                                selectAllText = 'Zaznacz wszytskie',
                                                liveSearch = TRUE
                                              ),
                                              multiple = TRUE 
                                            )),
                                     column(6,
                                            column(3,
                                                   pickerInput(
                                                     inputId = 'krajz',
                                                     label = 'Kraj',
                                                     choices = sort(unique(data[, language_code])),
                                                     selected = 'PL',
                                                     options = pickerOptions(
                                                       actionsBox = TRUE,
                                                       size = 8,  
                                                       selectedTextFormat = "count > 3",
                                                       deselectAllText = 'Odznacz wszystkie',
                                                       selectAllText = 'Zaznacz wszytskie',
                                                       liveSearch = TRUE
                                                     ),
                                                     multiple = TRUE
                                                   ),
                                                   airDatepickerInput(
                                                     inputId = "dataz",
                                                     label = "Data dodania zamowienia X",
                                                     view = "months", minView = "months",
                                                     dateFormat = "MM yyyy", monthsField = "months",
                                                     range = T, 
                                                     minDate = paste0(min(data[!is.na(added_datetime.x), unique(added_datetime.x)]),'-01'),
                                                     maxDate = paste0(max(data[!is.na(added_datetime.x), unique(added_datetime.x)]),'-01'),
                                                     value = NULL,
                                                     addon = 'none'
                                                   )
                                            ),
                                            column(3,
                                                   pickerInput(
                                                     inputId = 'bezpoz',
                                                     label = 'Czy para bezpośrednia?',
                                                     choices = c("tak", "nie"),
                                                     selected = "tak",
                                                     multiple = TRUE
                                                   ),
                                                   pickerInput(
                                                     inputId = 'crossz',
                                                     label = 'Czy cross?',
                                                     choices = c("tak", "nie"),
                                                     selected = c("tak"),
                                                     multiple = TRUE
                                                   )),
                                            column(3,
                                                   pickerInput(
                                                     inputId = 'plecz',
                                                     label = 'Płeć konsumenta',
                                                     choices = sort(unique(data[, client_sex])),
                                                     selected = sort(unique(data[, client_sex])),
                                                     multiple = TRUE
                                                   ),
                                                   pickerInput(
                                                     inputId = 'wiekz',
                                                     label = 'Wiek konsumenta',
                                                     choices = sort(unique(data[, last_order_client_age_range])),
                                                     selected = sort(unique(data[, last_order_client_age_range])),
                                                     options = pickerOptions(
                                                       actionsBox = TRUE,
                                                       size = 8,  
                                                       selectedTextFormat = "count > 3",
                                                       deselectAllText = 'Odznacz wszystkie',
                                                       selectAllText = 'Zaznacz wszytskie',
                                                       liveSearch = TRUE
                                                     ),
                                                     multiple = TRUE
                                                   )),
                                            column(3, align = 'left',
                                                   numericInput("als", "Ile crossów pokazać?", 100),
                                                   actionButton("zastosujz" , "Zastosuj", style="color: #fff; background-color: #4a9949; border-color: #4a9949; padding:20px")
                                                   
                                            )
                                     )
                                   )
                                   ,
                                   plotOutput('Plotsan', height = '1000px')
                          )
              )
      ),
      
      
      
      #####druga karta####
      tabItem(tabName = 'dni',useShinyjs(),
              fluidRow(  
                useShinyjs(),
                shinyjs::hidden(progressBar(id = "postep1", value = 0, striped = T, display_pct = T, title = "Pasek postępu"))
              ),
              fluidRow(
                br(),
                column(2,
                       pickerInput(
                         inputId = 'grupax1',
                         label = "Grupa produktowa zamówienia X",
                         choices = sort(unique(data[, products_group.x_1])),
                         selected  = sort(unique(data[, products_group.x_1])),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           size = 8,
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'Odznacz wszystkie',
                           selectAllText = 'Zaznacz wszytskie',
                           liveSearch = TRUE
                         ),
                         multiple = TRUE
                       ),
                       pickerInput(
                         inputId = 'kampaniax1',
                         label = 'Grupa produktowa kampanii X',
                         choices = sort(unique(data[, campaign_product_group.x])),
                         selected = sort(unique(data[, campaign_product_group.x])),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           size = 8,
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'Odznacz wszystkie',
                           selectAllText = 'Zaznacz wszytskie',
                           liveSearch = TRUE
                         ),
                         multiple = TRUE
                       ),
                       pickerInput(
                         inputId = 'kanalx1',
                         label = 'Kanał reklamy X',
                         choices = sort(unique(data[, action_channel.x])),
                         selected = sort(unique(data[, action_channel.x])),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           size = 8,
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'Odznacz wszystkie',
                           selectAllText = 'Zaznacz wszytskie',
                           liveSearch = TRUE
                         ),
                         multiple = TRUE
                       ),
                       pickerInput(
                         inputId = 'programx1',
                         label = 'Program X',
                         choices = c("P0", "P1", "P2", "P3", "DOK"),
                         selected = c("P0", "P1"),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           size = 8,  
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'Odznacz wszystkie',
                           selectAllText = 'Zaznacz wszytskie',
                           liveSearch = TRUE
                         ),
                         multiple = TRUE
                       )
                ),
                column(2,
                       pickerInput(
                         inputId = 'grupay1',
                         label = 'Grupa produktowa zamówienia Y',
                         choices = sort(unique(data[, products_group.y_1])),
                         selected = sort(unique(data[, products_group.y_1])),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           size = 8,
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'Odznacz wszystkie',
                           selectAllText = 'Zaznacz wszytskie',
                           liveSearch = TRUE
                         ),
                         multiple = TRUE
                       ),
                       pickerInput(
                         inputId = 'kampaniay1',
                         label = 'Grupa produktowa kampanii Y',
                         choices = sort(unique(data[, campaign_product_group.y])),
                         selected = sort(unique(data[, campaign_product_group.y])),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           size = 8,
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'Odznacz wszystkie',
                           selectAllText = 'Zaznacz wszytskie',
                           liveSearch = TRUE
                         ),
                         multiple = TRUE
                       ),
                       pickerInput(
                         inputId = 'kanaly1',
                         label = 'Kanał reklamy Y',
                         choices = sort(unique(data[, action_channel.y])),
                         selected = sort(unique(data[, action_channel.y])),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           size = 8,  
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'Odznacz wszystkie',
                           selectAllText = 'Zaznacz wszytskie',
                           liveSearch = TRUE
                         ),
                         multiple = TRUE
                       ),
                       pickerInput(
                         inputId = 'programy1',
                         label = 'Program Y',
                         choices = c("P0", "P1", "P2", "P3", "DOK"),
                         selected = c("P0", "P1"),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           size = 8,  
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'Odznacz wszystkie',
                           selectAllText = 'Zaznacz wszytskie',
                           liveSearch = TRUE
                         ),
                         multiple = TRUE 
                       )                       
                ),
                column(2,
                       pickerInput(
                         inputId = 'kraj1',
                         label = 'Kraj',
                         choices = sort(unique(data[, language_code])),
                         selected = sort(unique(data[, language_code])),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           size = 8,  
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'Odznacz wszystkie',
                           selectAllText = 'Zaznacz wszytskie',
                           liveSearch = TRUE
                         ),
                         multiple = TRUE
                       ),
                       pickerInput(
                         inputId = 'plec1',
                         label = 'Płeć konsumenta',
                         choices = sort(unique(data[, client_sex])),
                         selected = sort(unique(data[, client_sex])),
                         multiple = TRUE
                       ),
                       pickerInput(
                         inputId = 'wiek1',
                         label = 'Wiek konsumenta',
                         choices = sort(unique(data[, last_order_client_age_range])),
                         selected = sort(unique(data[, last_order_client_age_range])),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           size = 8,  
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'Odznacz wszystkie',
                           selectAllText = 'Zaznacz wszytskie',
                           liveSearch = TRUE
                         ),
                         multiple = TRUE
                       ),
                       airDatepickerInput(
                         inputId = "data1",
                         label = "Data dodania zamowienia X",
                         view = "months", minView = "months",
                         dateFormat = "MM yyyy", monthsField = "months",
                         range = T, 
                         minDate = paste0(min(data[!is.na(added_datetime.x), unique(added_datetime.x)]),'-01'),
                         maxDate = paste0(max(data[!is.na(added_datetime.x), unique(added_datetime.x)]),'-01'),
                         value = NULL,
                         addon = 'none'
                       )
                ),
                column(2,
                       pickerInput(
                         inputId = 'cross1',
                         label = 'Czy cross?',
                         choices = c("tak", "nie"),
                         selected = c("tak", "nie"),
                         multiple = TRUE
                       ),
                       selectInput(
                         inputId = 'agregat11',
                         label = '1 Agregacja tabeli',
                         choices = c('Kraj', 'Płeć konsumenta', 'Wiek konsumenta', "Grupa produktowa zamówienia X", "Grupa produktowa zamówienia Y", 'Kanał reklamy X', 'Kanał reklamy Y', 'Program zamówienia X', 'Program zamówienia Y'),
                         selected = 'Kraj',
                         multiple = FALSE
                       ),
                       selectInput(
                         inputId = 'agregat12',
                         label = '2 Agregacja tabeli',
                         choices = c('Kraj', 'Płeć konsumenta', 'Wiek konsumenta', "Grupa produktowa zamówienia X", "Grupa produktowa zamówienia Y", 'Kanał reklamy X', 'Kanał reklamy Y', 'Program zamówienia X', 'Program zamówienia Y', 'Total'),
                         selected = 'Total',
                         multiple = FALSE
                       ),
                       actionButton("zastosuj1" , "Zastosuj", style="color: #fff; background-color: #4a9949; border-color: #4a9949; padding:20px")
                ),
                column(4,
                       DT::dataTableOutput("tabela_dni")
                )),
              br(),
              fluidRow(plotlyOutput("Plotbar1")),
              br(),
              fluidRow(  plotlyOutput("Plotline1"))
      ),
      
      #####trzecia karta####
      tabItem(tabName = "wartosc",useShinyjs(),
              fluidRow(  
                useShinyjs(),
                shinyjs::hidden(progressBar(id = "postep2", value = 0, striped = T, display_pct = T, title = "Pasek postępu"))
              ),
              fluidRow(
                column(2, 
                       pickerInput(
                         inputId = 'agregat21',
                         label = 'Informacje o:',
                         choices = agregaty21,
                         selected = agregaty21[1],
                         multiple = FALSE
                       ),
                       pickerInput(
                         inputId = 'wykres2',
                         label = 'Agregacja wykresu',
                         choices = agregaty23,
                         selected = agregaty23[length(agregaty23)],
                         multiple = FALSE
                       ),
                       pickerInput(
                         inputId = 'agregat22',
                         label = '1 agregacja tabeli',
                         choices = agregaty22,
                         selected = agregaty22[1],
                         multiple = FALSE
                       ),
                       pickerInput(
                         inputId = 'agregat23',
                         label = '2 agregacja tabeli',
                         choices = agregaty23,
                         selected = agregaty23[length(agregaty23)],
                         multiple = FALSE
                       )
                ),
                column(2,
                       pickerInput(
                         inputId = 'kraj2',
                         label = 'Kraj',
                         choices = sort(unique(dane_trzecia0[,country])),
                         selected = sort(unique(dane_trzecia0[,country])),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           size = 8,  
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'Odznacz wszystkie',
                           selectAllText = 'Zaznacz wszytskie',
                           liveSearch = TRUE
                         ),
                         multiple = TRUE
                       ),
                       pickerInput(
                         inputId = 'wiek2',
                         label = 'Wiek konsumenta',
                         choices = sort(unique(dane_trzecia0[,last_order_client_age_range])),
                         selected = sort(unique(dane_trzecia0[,last_order_client_age_range])),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           size = 8,  
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'Odznacz wszystkie',
                           selectAllText = 'Zaznacz wszytskie',
                           liveSearch = TRUE
                         ),
                         multiple = TRUE
                       ),
                       pickerInput(
                         inputId = 'plec2',
                         label = 'Płeć konsumenta',
                         choices = sort(unique(dane_trzecia0[,client_sex])),
                         selected = sort(unique(dane_trzecia0[,client_sex])),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           size = 8,  
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'Odznacz wszystkie',
                           selectAllText = 'Zaznacz wszytskie',
                           liveSearch = TRUE
                         ),
                         multiple = TRUE
                       ),   
                       pickerInput(
                         inputId = 'typ2',
                         label = 'Typ konsumenta',
                         choices = sort(unique(dane_trzecia0[,client_type])),
                         selected = sort(unique(dane_trzecia0[,client_type])),
                         multiple = TRUE
                       )
                ),
                column(2,
                       pickerInput(
                         inputId = 'kanal2',
                         label = 'Kanał reklamy ostatniego zamówienia',
                         choices = sort(unique(dane_trzecia0[,last_order_action_channel])),
                         selected = sort(unique(dane_trzecia0[,last_order_action_channel])),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           size = 8,  
                           selectedTextFormat = "count > 3",
                           deselectAllText = 'Odznacz wszystkie',
                           selectAllText = 'Zaznacz wszytskie',
                           liveSearch = TRUE
                         ),
                         multiple = TRUE
                       ),
                       pickerInput(
                         inputId = 'mail2',
                         label = 'Czy znamy adres E-mail?',
                         choices = czy_znamy,
                         selected = czy_znamy,
                         multiple = TRUE
                       ),
                       pickerInput(
                         inputId = 'adres2',
                         label = 'Czy znamy adres?',
                         choices = czy_znamy,
                         selected = czy_znamy,
                         multiple = TRUE
                       )
                ),
                column(6,
                       column(5,
                              pickerInput(
                                inputId = 'grupa_wszystkie2',
                                label = 'Grupy wszystkich sentowych zamówień',
                                choices = grupy_wszystkie,
                                selected = NULL,
                                options = pickerOptions(
                                  title = '',
                                  actionsBox = TRUE,
                                  size = 8,  
                                  selectedTextFormat = "count > 3",
                                  deselectAllText = 'Odznacz wszystkie',
                                  selectAllText = 'Zaznacz wszytskie',
                                  liveSearch = TRUE
                                ),
                                multiple = TRUE
                              ),
                              pickerInput(
                                inputId = 'warunek_wszystkie2',
                                label = 'Warunek na grupy wszystkich zamówień',
                                choices = warunek_grupy,
                                selected = warunek_grupy[1],
                                multiple = FALSE
                              ),
                              
                              pickerInput(
                                inputId = 'grupa_ostatnia2',
                                label = 'Grupy ostatniego sentowego zamówienia',
                                choices = grupy_ostatnie,
                                selected = NULL,
                                options = pickerOptions(
                                  title = '',
                                  actionsBox = TRUE,
                                  size = 8,  
                                  selectedTextFormat = "count > 3",
                                  deselectAllText = 'Odznacz wszystkie',
                                  selectAllText = 'Zaznacz wszytskie',
                                  liveSearch = TRUE
                                ),
                                multiple = TRUE
                              ),
                              pickerInput(
                                inputId = 'warunek_ostatnia2',
                                label = 'Warunek na grupy ostatniego zamówienia',
                                choices = warunek_grupy,
                                selected = warunek_grupy[1],
                                multiple = FALSE
                              )
                       ),
                       column(4, 
                              airDatepickerInput(
                                inputId = "ostatnie_zamowienie2",
                                label = "Data dodania ostatniego zamowienia",
                                view = "months", minView = "months",
                                dateFormat = "MM yyyy", monthsField = "months",
                                range = T, 
                                minDate = paste0(min(dane_trzecia0[!is.na(last_order_added_date), unique(last_order_added_date)]),'-01'),
                                maxDate = paste0(max(dane_trzecia0[!is.na(last_order_added_date), unique(last_order_added_date)]),'-01'),
                                value = c(paste0(min(dane_trzecia0[!is.na(last_order_added_date), unique(last_order_added_date)]),'-01'),
                                          paste0(max(dane_trzecia0[!is.na(last_order_added_date), unique(last_order_added_date)]),'-01')),
                                addon = 'none'
                              ),
                              airDatepickerInput(
                                inputId = "pierwsze_zamowienie2",
                                label = "Data dodania pierwszego zamowienia",
                                view = "months", minView = "months",
                                dateFormat = "MM yyyy", monthsField = "months",
                                range = T, 
                                minDate = paste0(min(dane_trzecia0[!is.na(first_order_added_date), unique(first_order_added_date)]),'-01'),
                                maxDate = paste0(max(dane_trzecia0[!is.na(first_order_added_date), unique(first_order_added_date)]),'-01'),
                                value = c(paste0(min(dane_trzecia0[!is.na(first_order_added_date), unique(first_order_added_date)]),'-01'),
                                          paste0(max(dane_trzecia0[!is.na(first_order_added_date), unique(first_order_added_date)]),'-01')),
                                addon = 'none'
                              ),
                              airDatepickerInput(
                                inputId = "ostatnie_wyslane2",
                                label = "Data wysłania ostatniego zamowienia",
                                view = "months", minView = "months",
                                dateFormat = "MM yyyy", monthsField = "months",
                                range = T, 
                                minDate = paste0(min(dane_trzecia0[!is.na(last_sent_date), unique(last_sent_date)]),'-01'),
                                maxDate = paste0(max(dane_trzecia0[!is.na(last_sent_date), unique(last_sent_date)]),'-01'),
                                value = NULL,
                                addon = 'none'
                              ),
                              actionButton("zastosuj2" , "Zastosuj", style="color: #fff; background-color: #4a9949; border-color: #4a9949; padding:20px")
                       )
                )),
              fluidRow(
                column(4,
                       DT::dataTableOutput("tabela_trzecia")),
                column(7,
                       plotly::plotlyOutput('Plotbar2',height='750px'),style = "overflow-x: auto;"),
                column(1,fluidRow(),
                       fluidRow(),
                       switchInput(
                         inputId = "waluta",
                         label = 'Currency',
                         offLabel = 'ZŁ',
                         onLabel = "€",
                         onStatus = 'gray',
                         offStatus = 'gray'
                       )
                )
              )
      )
    )
      )
      )
#####serwer####
server <- function(input, output, session) {
  useShinyjs()
  
  observeEvent(input$getLogin, {
    # jeśli js przechwycił login (warunek length(login) > 0) i nie testuję kodu aplikacji z poziomu rstudio (warunek login != 'developer')
    login <- input$getLogin
    passy<-"~/dostepy_analizy_crm/pass_analizy_crm_host_anr1"
    #passy<-'/home/nowakkr/R/anr1_passy'
    # login<-'bocianj'
    if(length(login) > 0 && login != 'developer'){
      # jeśli aplikacja jest chronione - to definiujemy w global.R w zmiennej PROTECTED
      if(PROTECTED){
        # sprawdzam czy dana osoba ma wjazd na aplikacje
        access <- accessVerify2(appName = APP_NAME, login = login, host = "anr1.infr.co", db_name = "shiny",
                                table_name = "accesses", creds =passy )
        # jeśli brak dostępu to wyskakuje alert i przekierowanie na kreator zadań w JIRA o dostęp do tej aplikacji
        if(!access){
          jiraLink <- createJiraLink2(appName = APP_NAME, login = login, project = "wato")
          shinyjs::runjs(code = "alert('You dont have access to this app. Click OK to create Jira task.');")
          shinyjs::runjs(code = sprintf("$(location).attr('href', '%s');", jiraLink))
          return(NULL)
        }
      }
      # a jeśli aplikacja nie jest chroniona lub jest ale osoba przeszła weryfikację to zapisujemy wizyte tej osoby na bazie
      addVisits2(appName = APP_NAME, login = login, host = "anr1.infr.co", db_name = "shiny", table_name = "visits", creds = passy)
    }
    
req(access)

    if(names(access) =='user_super' ){
      shinyjs::show(selector = '.sidebar-menu li a[data-value=dni]')
  
      shinyjs::show(selector = '.sidebar-menu li a[data-value=wartosc]')
      shinyjs::show(selector = '.sidebar-menu li a[data-value=przejscia]')
      updateTabsetPanel(session, "menu", selected = "przejscia")
      
      
      
    } else if (names(access) =='pm' ){
      shinyjs::show(selector = '.sidebar-menu li a[data-value=dni]')
      shinyjs::show(selector = '.sidebar-menu li a[data-value=przejscia]')
      updateTabsetPanel(session, "menu", selected = "przejscia")
      
      balax <- dostepy_ALLx[user_name == login]
      bala1x <- balax[products_group1 != 'multi']
      ijoijox <- data[products_group.x_1 %in% balax$products_group1, .N, by = products_group.x_1]
      ijoijox <- ijoijox[order(-N)]
      
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
      
      grupal <- data[products_group.x_1 %in% dostepy[user_name == login, campaign_product_group], products_group.x_1]
      
      updateSelectInput(
        session,
        inputId = 'glownedanex',
        label = 'Grupa produktowa zamówienia X',
        choices = jajox1$products_group.x_1,
        selected = jajox1$products_group.x_1[1]
      ) 
      
      updateSelectInput(
        session,
        inputId = 'grupax1',
        label = 'Grupa produktowa zamówienia X',
        choices = sort(unique(balax1$products_group1)),
        selected = sort(unique(balax1$products_group1))
      ) 
      
      
      updateSelectInput(
        session,
        inputId = 'kampaniax1',
        label = 'Grupa produktowa kampanii X',
        choices = sort(unique(bala1x1$campaign_product_group)),
        selected = sort(unique(bala1x1$campaign_product_group))
      ) 
      
      
    }
      
    
    
  })
  
  useShinyjs()

  
  
 # observe({input$menu})
  observeEvent(input$getLogin,{
  shinyjs::click("zastosuj")
    shinyjs::click("zastosuj1")
    shinyjs::click("zastosuj2")
    },once=T)
  
  observeEvent(input$menu,{
login<-input$getLogin
# login<-'bocianj'
    if (input$menu == 'przejscia') {
      source(file="zac/przejscia.R", local=T)  }
if (input$menu == 'dni') { 
  source(file="zac/dni.R", local=T)   }  
 if (input$menu == 'wartosc') { 
   source(file="zac/portfele_konsum.R", local=T)}
      
  
    
  })
  
  
  
}
#####koniecpliku####
shinyApp(ui = ui, server = server)
