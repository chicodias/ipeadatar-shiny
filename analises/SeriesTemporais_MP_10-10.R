setwd("C:/Users/JP/Desktop")

library(fpp3)
library(ipeadatar)
library(ggplot2)


### Séries

# Índice Geral de Preços - Mercado (IGP-M) é a média aritmética ponderada 
# de três índices de preços: IPA, IPC e INCC, e revela as fontes de 
# pressão inflacionária e a evolução dos preços de produtos e serviços 
# mais relevantes para produtor, consumidor e construção civil.

## 1. IGP-M geral
IGPM_geral <- ipeadata("IGP12_IGPM12") %>% 
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

# Time plot
IGPM_geral %>% 
  autoplot() +
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) geral",
       x = "Ano/Mês",
       y = "Valor")

# Seasonal plot
#È diffícil por esse gráfico notar alguma sazonalidade, porém notamos que 
#conforme os anos passam a tendencia é um aumento dos valores, sendo mais explícito
#em anos recentes
IGPM_geral %>% 
  gg_season(value, labels = "both") +
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) geral",
       x = "Tempo (ano/mês)",
       y = "")

# Sub series plot
#os gráficos apresentam aumento conforme os anos sem grande diferença entre os meses
# como era de se esperar pelo gráfico passado
IGPM_geral %>% 
  gg_subseries(value) +
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) geral",
       x = "Tempo (em anos)",
       y = "IGP-M")

# STL decomposition (p/ extrair Tendencia)
IGPM_geral %>% 
  model(STL(value)) %>%
  components() %>%
  autoplot()

# ACF plot
# o gráffico apresenta uma queda da autocorrelação conforme o LAG
IGPM_geral %>% 
  ACF(value, lag_max = 100) %>%
  autoplot() + 
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) geral",
       x = "Lag",
       y = "ACF")

# Lag plot
IGPM_geral %>% 
  gg_lag(value, geom = "point", period = "year") + 
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) geral",
       x = "Lag",
       y = "IGP-M",
       colour = "Mês")


## 2. IGP-M mensal
IGPM_mensal <- ipeadata("IGP12_IGPMG12") %>% 
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

# Time plot
# valores muito altos em anos iniciais que abaixam em anos mais recentes
IGPM_mensal %>% 
  autoplot() +
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) mensal",
       x = "Tempo (ano/mês)",
       y = "Valor")

# Seasonal plot
IGPM_mensal %>% 
  gg_season(value, labels = "both") +
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) mensal",
       x = "Tempo (em mêses)",
       y = "")

# Sub series plot
#Os meses de janeiro fevereiro e março se destacam por uma alta principalmente nos anos iniciais
IGPM_mensal %>% 
  gg_subseries(value) +
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) mensal",
       x = "Tempo (em anos)",
       y = "IGP-M")

# STL decomposition (p/ extrair Tendencia)
#nota-se que a tendencia tinha valores mais altos até 1995
IGPM_mensal %>% 
  model(STL(value)) %>%
  components() %>%
  autoplot()

# ACF plot
IGPM_mensal %>% 
  ACF(value, lag_max = 100) %>%
  autoplot() + 
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) mensal",
       x = "Lag",
       y = "ACF")

# Lag plot
IGPM_mensal %>% 
  gg_lag(value, geom = "point", period = "year") + 
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) mensal",
       x = "Lag",
       y = "IGP-M",
       colour = "Mês")


## 3. IGP-M 1o decendio
IGPM_1D <- ipeadata("IGP12_IGPMG1D12") %>% 
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

# Time plot
IGPM_1D %>% 
  autoplot() +
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) 1º Decendio",
       x = "Tempo (ano/mês)",
       y = "Valor")

# Seasonal plot
IGPM_1D %>% 
  gg_season(value, labels = "both") +
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) 1º Decendio",
       x = "Tempo (em mêses)",
       y = "")

# Sub series plot
IGPM_1D %>% 
  gg_subseries(value) +
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) 1º Decendio",
       x = "Tempo (em anos)",
       y = "IGP-M")

# STL decomposition (p/ extrair Tendencia)
IGPM_1D %>% 
  model(STL(value)) %>%
  components() %>%
  autoplot()

# ACF plot
IGPM_1D %>% 
  ACF(value, lag_max = 100) %>%
  autoplot() + 
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) 1º Decendio",
       x = "Lag",
       y = "ACF")

# Lag plot
IGPM_1D %>% 
  gg_lag(value, geom = "point", period = "year") + 
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) 1º Decendio",
       x = "Lag",
       y = "IGP-M",
       colour = "Mês")


## 4. IGP-M 2o decendio
IGPM_2D <- ipeadata("IGP12_IGPMG2D12") %>% 
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

# Time plot
IGPM_2D %>% 
  autoplot() +
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) 2º Decendio",
       x = "Tempo (ano/mês)",
       y = "Valor")

# Seasonal plot
IGPM_2D %>% 
  gg_season(value, labels = "both") +
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) 2º Decendio",
       x = "Tempo (em mêses)",
       y = "")

# Sub series plot
IGPM_2D %>% 
  gg_subseries(value) +
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) 2º Decendio",
       x = "Tempo (em anos)",
       y = "IGP-M")

# STL decomposition (p/ extrair Tendencia)
IGPM_2D %>% 
  model(STL(value)) %>%
  components() %>%
  autoplot()

# ACF plot
IGPM_2D %>% 
  ACF(value, lag_max = 100) %>%
  autoplot() + 
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) 2º Decendio",
       x = "Lag",
       y = "ACF")

# Lag plot
IGPM_2D %>% 
  gg_lag(value, geom = "point", period = "year") + 
  labs(title = "Índice Geral de Preços – Mercado (IGP-M) 2º Decendio",
       x = "Lag",
       y = "IGP-M",
       colour = "Mês")
