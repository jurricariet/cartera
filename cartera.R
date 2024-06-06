library(yfR)
library(tidyverse)
library(lubridate)
library(directlabels)
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
  mutate(indice_100 = 100*precio/precio[fecha==fecha_compra]) %>% 
  group_by(fecha) %>% 
  mutate(vs_mep = precio/precio[ticker == 'MEP'])

df_todos %>% 
  filter(fecha >= fecha_compra) %>% 
  ggplot(aes(fecha,y=indice_100,group=ticker,color=ticker))+
  geom_dl(aes(label = ticker), method = list(dl.combine( "last.points")), cex = 0.8)+ 
  geom_line()+
  geom_point()+
  geom_hline(yintercept=100,color='black')+
  ggthemes::scale_color_canva()+
  theme_minimal()+
  theme(legend.position='none')+
  labs(x='',y='',title='Evolución activos',
       subtitle='Índice 28-may = 100')

ggsave('acciones_usa.png',scale=3)
df_todos %>% 
  filter(fecha >= fecha_compra) %>% 
  ggplot(aes(fecha,y=vs_mep,group=ticker,color=ticker))+
  geom_dl(aes(label = ticker), method = list(dl.combine( "last.points")), cex = 0.8)+ 
  geom_line()+
  geom_point()+
  geom_hline(yintercept=1,color='black')+
  ggthemes::scale_color_canva()+
  theme_minimal()+
  theme(legend.position='none')+
  labs(x='',y='',title='Evolución activos',
       subtitle='Índice 28-may = 100')

####
datapasta::df_paste()
cvx.ba <- data.frame(
  stringsAsFactors = FALSE,
  Fecha.Cotización = c("6/5/2024","6/4/2024",
                       "6/3/2024","05/31/2024","05/30/2024","05/29/2024",
                       "05/28/2024"),
          Apertura = c(12900, 12785, 12659.5, 12100, 11900, 12319, 12513),
            Máximo = c(13140, 13050, 13100, 12709, 12066, 12319, 12775.5),
            Mínimo = c(12623.5, 12100, 12382.5, 12042, 11700, 11701.5, 12160),
            Cierre = c(12658, 12819, 12731, 12697.5, 12060, 11922, 12313),
   Cierre.ajustado = c(12658, 12819, 12731, 12697.5, 12060, 11922, 12313),
     Volumen.Monto = c(226223344,151861057.5,
                       111599772,138718810,181903413,139774967.5,121816192.5),
   Volumen.Nominal = c(17767, 11887, 8815, 11115, 15276, 11814, 9959)
) %>% 
  mutate(fecha = mdy(Fecha.Cotización),
         precio=Cierre.ajustado,
         ticker = 'CVX.BA') %>% 
  select(fecha,precio,ticker)

datapasta::df_paste()
intc.ba <- data.frame(
  stringsAsFactors = FALSE,
  Fecha.Cotización = c("6/5/2024","6/4/2024",
                       "6/3/2024","05/31/2024","05/30/2024","05/29/2024",
                       "05/28/2024"),
          Apertura = c(7960, 7860, 7650, 7497, 7345, 7635, 7874.5),
            Máximo = c(8200, 8149.5, 7897.5, 7742, 7396.5, 7635, 7874.5),
            Mínimo = c(7900, 7788, 7591, 7339, 7000, 7300, 7560),
            Cierre = c(8050, 7868, 7826, 7700, 7352, 7340, 7720.5),
   Cierre.ajustado = c(8050, 7868, 7826, 7700, 7352, 7340, 7720.5),
     Volumen.Monto = c(152505633,275841296.5,
                       168342362.5,146628157,216726555,124353955.5,212246265.5),
   Volumen.Nominal = c(19051, 34895, 21686, 19536, 29571, 16911, 27705)
) %>% 
  mutate(fecha = mdy(Fecha.Cotización),
         precio=Cierre.ajustado,
         ticker = 'INTC.BA') %>% 
  select(fecha,precio,ticker)


cartera <- df_todos %>% 
  filter(ticker %in% c(ticker_acciones,ticker_bono)) %>%
  mutate(precio = ifelse(ticker %in% ticker_bono,precio/10,precio)) %>% 
  group_by(fecha) %>% 
  summarise(precio = sum(precio)) %>% 
  filter(fecha >= '2024-05-28' ) %>% 
  mutate(ticker='Cartera')
  
df_todos <- bind_rows(df_bonos) %>% 
  bind_rows(df_mep) %>%  
  bind_rows(intc.ba) %>% 
  bind_rows(cvx.ba) %>% 
  bind_rows(cartera) %>% 
  group_by(ticker) %>% 
  mutate(indice_100 = 100*precio/precio[fecha==fecha_compra])

df_todos %>% 
  filter(fecha >= fecha_compra) %>% 
  ggplot(aes(fecha,y=indice_100,group=ticker,color=ticker))+
  ggrepel::geom_label_repel(data=df_todos %>% filter(fecha == max(fecha)),aes(label = ticker))+ 
  geom_line()+
  geom_point()+
  coord_cartesian(clip='off')+
  scale_x_date(breaks = seq.Date(as.Date('2024-05-28'),as.Date(max(df_todos$fecha))+days(3),by="1 day"),
               date_breaks = "1 day",
               date_labels = "%d %b",
               expand=c(0,1))+
  geom_hline(yintercept=100,color='black')+
  ggthemes::scale_color_colorblind()+
  theme_minimal()+
  theme(legend.position='none')+
  labs(x='',y='',title='Evolución activos',
       subtitle='Índice 28-may = 100')
ggsave('evolucion.png',scale=5)
