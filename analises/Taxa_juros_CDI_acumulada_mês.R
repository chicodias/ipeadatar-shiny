library(fpp3)
library(ipeadatar)
library(ggplot2)

#Taxa de juros - CDI / Over - acumulada no mês
ts <- ipeadata("BM12_TJCDI12") %>% 
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

# Time plot
ts %>% 
  autoplot()

# Seasonal plot
#Analisando o gráfico de sazonalidade notamnos uma crescente durante os meses, com picos em dezembro,
#apesar de discrepâncias acontecerem em determinados anos, principalmente em anos antes de 1995 e valores recentes muito mais baixos.
ts %>% 
  gg_season(value, labels = "both")

# Sub series plot
ts %>% 
  gg_subseries(value)

# STL decomposition (p/ extrair Tendencia)
#Dessa forma, podemos analisar somente a tendência da série ou podemos remover a 
#sazonalidade da série para obter a série “sazonalmente ajustada”, dentre outras análises possíveis
ts %>% 
  model(STL(value)) %>%
  components() %>%
  autoplot()

# ACF plot , lag_max = 100
#Temos um pico de autocorrelação no inicio que segue decrescendo
ts %>% 
  ACF(value, lag_max = 50) %>%
  autoplot()

# Lag plot
ts %>% 
  gg_lag(value, geom = "point", period = "year")
