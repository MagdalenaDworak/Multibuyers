library(data.table)
library(dbConnectBPO)
library(stringr)
library(dplyr)
library(tictoc)
tic()
#credsy <-'/home/boczkowskim/pass/login'
credsy <- '/home/analizy_crm/dostepy_analizy_crm/pass_dwh_crm'

mydb <-open_channel('dwh-hd-team-crm.3bpo.com', credentials_path=credsy)


############### klucze trzeba dodać
datak <- as.data.table(dbGetQuery(mydb,paste0(
  "SELECT 
  *
  FROM bu_dzp_crm.dashboard_multibuyers
  ")))

data_multi <- as.data.table(dbGetQuery(mydb,paste0(
  "SELECT 
  *
  FROM bu_dzp_crm.dashboard_multibuyers_1
  ")))

co <- datak[,.(wsumie = .N), by = products_group.x_1]
co1 <- datak[,.(wsumiekanal = .N), by = action_channel.x]
data <- merge(datak,co,by = 'products_group.x_1', all.x = T)

remove(datak)

data <- merge(data, co1, by = 'action_channel.x', all.x = T)

data <- data[added_datetime.x>= '2017-01-01']

remove(co)
remove(co1)


add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

kolor <- "#384bc2"
kolor1 <- add.alpha(kolor, 0.3)
kampaniex <- sort(unique(unlist(strsplit(data[,campaign_product_group.x], ', '))))
kampaniey <- sort(unique(unlist(strsplit(data[,campaign_product_group.y], ', '))))


#credsy2 <- '/home/analizy_crm/dostepy_analizy_crm/pass_analizy_crm'

#mydb <-open_channel('bi.3bpo.com', credentials_path=credsy2)

credsy <- '/home/analizy_crm/dostepy_analizy_crm/pass_dwh_crm'

mydb <-open_channel('dwh-hd-team-crm.3bpo.com', credentials_path=credsy)


loginy_mdm <- as.data.table(dbGetQuery(mydb,paste0(
  "SELECT 
  ldap_login 
  FROM bu_dzp_crm.frontowy_cof
  where group_id = 452
  ")))


dane_trzecia0 <- as.data.table(dbGetQuery(mydb,paste0(
  "SELECT 
  client_sex, client_sent_product_groups, last_order_client_age_range, upper(country) as country, client_type, phone_type, has_email, has_address, last_sent_date, last_order_added_date, first_order_added_date,
  last_sent_order_value, sent_orders_value, last_order_status, last_order_action_channel, last_sent_order_products_group, last_order_campaign_product_group, 
  orders_sent
  FROM bu_dzp_crm.hit_rate where first_order_added_date >=  ADDDATE(CURDATE(), INTERVAL -1460 day) and client_type in ( 'Singlebuyer', 'Multibuyer')
  ")))

#dane_trzecia0 <- dane_trzecia0[client_type == 'Singlebuyer' | client_type == 'Multibuyer']

warunek_grupy <- c(or = ' | ', and = ' & ')
czy_znamy <- c(tak = 1, nie = 0)
grupy_ostatnie <- sort(unique(unlist(strsplit(dane_trzecia0[,last_sent_order_products_group], ','))))
grupy_wszystkie <- sort(unique(unlist(strsplit(dane_trzecia0[,client_sent_product_groups], ','))))
agregaty21 <- c('przedział_ostatnie', 'przedział_wszystkie')
names(agregaty21) <- c("Ostatnie sentowe zamówienie","Wszystkie sentowe zamówienia")
agregaty22 <- c('country', 'client_sex', 'last_order_client_age_range', 'last_order_action_channel')
names(agregaty22) <- c("Kraj", "Płeć konsumenta", "Wiek konsumenta", "Kanał reklamy ostatniego zamówienia")
agregaty23 <- c('country', 'client_sex', 'last_order_client_age_range', 'last_order_action_channel', 'Total')
names(agregaty23) <- c("Kraj", "Płeć konsumenta", "Wiek konsumenta", "Kanał reklamy ostatniego zamówienia", 'Total')

kampanie <-as.data.table(dbGetQuery(mydb,paste0(
  "SELECT 
  products_group,
  campaign_product_group
  FROM olap.fact_orders_all
  WHERE 
  (campaign_product_group='education' or business_unit in ('brandnova', 'Centum Innowacji'))
  and status = 'Sent'
  and added_date >= '2017-01-01'
  ")))

dostepy <- as.data.table(dbGetQuery(mydb,paste0(
  "select 
  product_group_name as campaign_product_group,
  left(user_name,length(user_name)-2) as user_name
  from dicts_mdm.users_pg_configs e
  where product_group_name <> 'none'
  " )))

dostepy<-dostepy[! user_name %like% 'pchodaczek',]
kampanie <- unique(kampanie)
dostepy <- unique(dostepy)
kampanie <- kampanie[products_group %like% '/', products_group1 := 'multi']
kampanie <- kampanie[is.na(products_group1), products_group1 := products_group]
kampanie$products_group <- NULL
dostepy_ALL <- merge(dostepy, kampanie, by = 'campaign_product_group', allow.cartesian = T)
dostepy_ALL <- unique(dostepy_ALL)
dostepy_ALLx <- dostepy_ALL[products_group1 %in% data$products_group.x_1 & campaign_product_group %in% data$campaign_product_group.x]
dostepy_ALLy <- dostepy_ALL[products_group1 %in% data$products_group.y_1 & campaign_product_group %in% data$campaign_product_group.y]

dostepy_ALLx <- dostepy_ALLx[,products_group := NULL]
dostepy_ALLx <- unique(dostepy_ALLx)

dostepy_ALLy <- dostepy_ALLy[,products_group := NULL]
dostepy_ALLy <- unique(dostepy_ALLy)

dostepy1 <- dostepy[, products_group1 := campaign_product_group]
dostepy1$campaign_product_group <- NULL
dostepy_ALL1 <- merge(dostepy1, kampanie, by = 'products_group1', allow.cartesian = T)
dostepy_ALL1 <- unique(dostepy_ALL1)
dostepy_ALLx1 <- dostepy_ALL1[products_group1 %in% data$products_group.x_1 & campaign_product_group %in% data$campaign_product_group.x]

dostepy_ALLx1 <- unique(dostepy_ALLx1)

disconnect_all_MySQL()
toc()
