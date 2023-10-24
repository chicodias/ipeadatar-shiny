library(fpp3)
library(ipeadatar)
library(ggplot2)
library(dplyr)

### Séries

# Índice de preços ao consumidor - Índice de preços no consumidor é usado para
# observar tendências de inflação. É calculado com base no preço médio necessário
# para comprar um conjunto de bens de consumo e serviços num país, comparando com
# períodos anteriores. 

## 1. 	IPC (FIPE) - taxa de inflação
IPC_tx <- ipeadata("FIPE_FIPE0001") %>% 
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

# Time plot
IPC_tx %>% 
  autoplot() +
  labs(title = "IPC(tx de inflação)",
       x = "Ano/Mês",
       y = "Valor")

# Seasonal plot
IPC_tx %>% 
  gg_season(value, labels = "both") +
  labs(title = "IPC(tx de inflação- seasonal plot",
       x = "Ano/Mês",
       y = "")

# Subseries plot
IPC_tx %>% 
  gg_subseries(value) +
  labs(title = "IPC(tx de inflação)- subseries plot",
       x = "Anos",
       y = "IPC")

# STL decomposition (p/ extrair Tendencia)
IPC_tx %>% 
  model(STL(value)) %>%
  components() %>%
  autoplot()

# ACF plot
IPC_tx %>% 
  ACF(value, lag_max = 100) %>%
  autoplot() + 
  labs(title = "IPC",
       x = "Lag",
       y = "ACF")


## 2.	IPC (FIPE) - taxa de variação
IPC_var <- ipeadata("FIPE12_FIPE0001") %>% 
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

# Time plot
IPC_var %>% 
  autoplot() +
  labs(title = "IPC (FIPE) - taxa de variação",
       x = "Ano/Mês",
       y = "Valor")

# Seasonal plot
IPC_var %>% 
  gg_season(value, labels = "both") +
  labs(title = "IPC (FIPE) - taxa de variação- seasonal plot",
       x = "Ano/Mês",
       y = "")

# Subseries plot
IPC_var %>% 
  gg_subseries(value) +
  labs(title = "IPC (FIPE) - taxa de variação- subseries",
       x = "Anos",
       y = "IGP-M")

# STL decomposition (p/ extrair Tendencia)
IPC_var %>% 
  model(STL(value)) %>%
  components() %>%
  autoplot()

# ACF plot
IPC_var %>% 
  ACF(value, lag_max = 100) %>%
  autoplot() + 
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) mensal",
       x = "Lag",
       y = "ACF")


## 3. IPC - grupo: habitação - RMSP - taxa de variação
IPC_hab <- ipeadata("FIPE12_FIPE0002") %>% 
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

# Time plot
IPC_hab %>% 
  autoplot() +
  labs(title = "IPC - grupo: habitação",
       x = "Ano/Mês",
       y = "Valor")

# Seasonal plot
IPC_hab %>% 
  gg_season(value, labels = "both") +
  labs(title = "IPC - grupo: habitação",
       x = "Ano/Mês",
       y = "")

# Subseries plot
IPC_hab %>% 
  gg_subseries(value) +
  labs(title = "IPC - grupo: habitação",
       x = "Anos",
       y = "IPC")

# STL decomposition (p/ extrair Tendencia)
IPC_hab %>% 
  model(STL(value)) %>%
  components() %>%
  autoplot()

# ACF plot
IPC_hab %>% 
  ACF(value, lag_max = 100) %>%
  autoplot() + 
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) 1º Decendio",
       x = "Lag",
       y = "ACF")