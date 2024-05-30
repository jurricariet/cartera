library(yfR)
library(tidyverse)
library(lubridate)
# BONOS: descarga desde Alphacast
ticker_bono <- 'GD35'
bonos <- read_csv('https://api.alphacast.io/datasets/5357/data?apiKey=ak_b4clHOA8NEK7Gv5q5S5R&$format=csv') %>% 
  filter(Ticker == ticker_bono)

df_bonos <- bonos %>% 
  select(fecha=Date,precio=Precio,ticker=Ticker)

# CEDEARS desde el paquete yahoo finance
# Buscar tickers en https://finance.yahoo.com

ticker_acciones <- c('CVX.BA','INTC.BA')
first_date <- as.Date('2024-01-01')
last_date <- Sys.Date()

# Descargo los datos de todos los tickers seleccionados
df_acciones <- yf_get(tickers = ticker_acciones, 
                           first_date = first_date,
                           last_date = last_date)

df_acciones <- df_acciones %>% 
  select(ticker,fecha=ref_date,precio = price_adjusted)
# Dolar MEP de ambito.com
mep_ambito <- glue::glue('https://mercados.ambito.com//dolarrava/mep/grafico/{first_date}/{last_date}')
response <- httr::GET(mep_ambito[[1]])
content <- httr::content(response, "text")
j <- jsonlite::fromJSON(content)

df_mep <- j %>%as.data.frame() %>% 
  slice(-1) %>% 
  mutate(fecha = dmy(V1),
         precio = as.numeric(V2)) %>% 
  select(fecha,precio) %>% 
  mutate(ticker='MEP')
##### UNIFICO EN UN DF
fecha_compra = as.Date('2024-05-28')
df_todos <- bind_rows(df_bonos,df_acciones) %>% 
  bind_rows(df_mep) %>%  
  group_by(ticker) %>% 
  mutate(indice_100 = 100*precio/precio[fecha==fecha_compra])

df_todos %>% 
  filter(fecha >= fecha_compra) %>% 
  ggplot(aes(fecha,y=indice_100,group=ticker,color=ticker))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept=100,color='black')
