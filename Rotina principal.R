# Manipulação de dados
library(tidyverse)
# Manipulação de Séries Temporais
library(tsibble)
# Funções de Previsão
library(fable)
# Gráficos e Estatísticas de Séries Temporais
library(feasts)
# Séries Temporais Tidy
library(tsibbledata)
# Todos os itens acima e mais
library(fpp3)

#----

# Um tsibble permite o armazenamento e manipulação de múltiplas séries temporais em R
# Ele contêm: um index (informação de tempo), variáveis medidas e variáveis chave (identificadores únicos opcionais para cada série)

global_economy
tourism

# lendo um arquivo csv e convertendo para tsibble

covid = readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv")
covid

covid = readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv") %>% 
  select(date, state, newDeaths, newCases)%>% 
  as_tsibble(
    index = date,
    key = state
  ) %>% 
  filter(date < today()) %>% 
  group_by(state) %>% 
  mutate(MM_mortes = zoo::rollmean(newDeaths, k = 7, fill = NA, align = "right"),
         MM_casos = zoo::rollmean(newCases, k = 7, fill = NA, align = "right"))
covid

# plotando com o ggplot2

covid %>% 
  filter(state == "TOTAL") %>% 
  autoplot(newDeaths, color = "grey") +
  geom_line(aes(y = MM_mortes)) +
  labs(x="Dia",y="Mortes", title="Média Móvel (7 dias) do número de mortes por COVID-19 no Brasil")

covid %>% 
  filter(state != "TOTAL") %>% 
  autoplot(MM_mortes) +
  labs(x="Dia",y="Mortes (MM 7 dias)")

covid %>% 
  filter(state != "TOTAL") %>% 
  autoplot(MM_mortes) +
  facet_wrap(~state, scales = "free") +
  labs(x="Dia",y="Mortes (MM 7 dias)") +
  theme(legend.position = "none")

covid %>% 
  filter(state == "TOTAL") %>% 
  gg_season(MM_mortes) +
  labs(x="Dia",y="Mortes (MM 7 dias)")

covid %>% 
  filter(state == "TOTAL") %>% 
  gg_season(MM_mortes, period = "month")  +
  labs(x="Dia",y="Mortes (MM 7 dias)")

# PREVISÃO ESTATÍSTICA
# y{T+h} --> coisa a ser prevista
# y{1},...,y{T} --> o que conhecemos/histórico
# yhat{T+h|T} = E[y{T+h}|y{1},...,y{T}] --> previsão pontual

# BENCHMARKS
# MEAN(y): previsões são iguais a média histórica
# NAIVE(y): previsões são iguais ao último valor observado
# SNAIVE(y~lag(m)): previsões iguais ao último valor do mesmo período
# RW(y~drift()): previsões iguais ao último valor mais variação média

# a função model() treina o modelo nos dados

covid_fit = covid %>% 
  filter(state == "TOTAL") %>% 
  model(
    Seasonal_naive = SNAIVE(newCases),
    Naive = NAIVE(newCases),
    Drift = RW(newCases ~ drift()),
    Mean = MEAN(newCases)
  )
covid_fit

# para produzir as previsões use a função forecast()

covid_fc = covid_fit %>% 
  forecast(h = 12)
covid_fc

# fable é uma tabela de previsão com previsões pontuais e distribuições

covid_fc %>% 
  autoplot(covid, level = NULL)

covid_fc %>% 
  autoplot(covid %>% filter(date >= "2021-07-01"), level = NULL)

# Residuos: et = y{t}-haty{t|t-1}
# Premissas:
# {et} não são correlacionados, caso sejam: ficaram informações nos resíduos que deveriam estar no modelo
# {et} possui média zero, caso não seja então as previsões são viesadas

augment(covid_fit)

augment(covid_fit) %>%
  filter(.model == "Seasonal_naive") %>% 
  autoplot(.resid)

augment(covid_fit) %>%
  filter(.model == "Seasonal_naive") %>% 
  ACF(.resid) %>%
  autoplot()

# Teste de Ljung-box
# H0: os resíduos são iid
# não quero rejeitar H0 (pvalor grande)

augment(covid_fit) %>%
  features(.resid, ljung_box)

# Medidas de acurácia

covid_fit = covid %>% 
  filter(state == "TOTAL",
         date <= "2021-08-16") %>% 
  model(Seasonal_naive = SNAIVE(newCases))

covid_fit %>% 
  forecast(h = "1 month") %>% 
  autoplot(covid %>% 
             filter(state == "TOTAL", date >= "2021-07-01"),
           level = NULL)

#MAE = mean(|eT+h|)
#MSE = mean(e^2T+h)
#MAPE = 100mean(|eT+h|/|yT+h|)
#RMSE = sqrt(mean(e^2{T+h}))

#MAE,MSE e RMSE são dependentes da escala dos dados
#MAPE é independente da escala, porém só é sensível

covid_fc = covid_fit %>% 
  forecast(h = "1 month")

accuracy(covid_fc, covid %>% 
           filter(state == "TOTAL"))

# EXPONENTIAL SMOOTHING

# E (Error) T (Trend) S (Season)
# Erro: aditivo (A) ou multiplicativo (M)
# Tendência: nenhuma (N), aditiva (A), multiplicativa (M) ou amortecida (Ad ou Md)
# Sazonalidade: nenhuma (N), aditiva (A) ou multiplicativa (M)

covid_RJ = covid %>% 
  filter(state == "RJ") %>% 
  select(newDeaths)

covid_RJ %>% 
  autoplot()

fit = covid_RJ %>%
  model(ets = ETS(newDeaths))

fit
report(fit)
components(fit)

components(fit) %>% 
  autoplot()

fit %>% 
  forecast(h = 14) %>% 
  autoplot(covid_RJ %>% filter(date >= "2021-08-01"))

covid_RJ %>%
  model(ets = ETS(newDeaths ~ trend("Ad"))) %>% 
  forecast(h = 14) %>% 
  autoplot(covid_RJ %>% filter(date >= "2021-08-01"))

# Bootstrap

sim = fit %>% 
  generate(h = 14, times = 5, bootstrap = TRUE)
sim

covid_RJ %>% 
  filter(date >= "2021-08-01") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = newDeaths)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)), data = sim) +
  guides(col = "none")

# ARIMA

# AR: autoregressivo (observações defasadas como input)
#  I: integrado (diferenciação para tornar a série estacionária)
# MA: média móvel (erros defasados como input)

fit_ARIMA = covid_RJ %>%
  model(arima = ARIMA(newDeaths))

fit_ARIMA
report(fit_ARIMA)

fit_ARIMA %>% 
  forecast(h = 14) %>% 
  autoplot(covid_RJ %>% filter(date >= "2021-08-01"))

covid_RJ %>%
  model(arima = ARIMA(newDeaths ~ pdq(1,1,1)+PDQ(1,1,1))) %>% 
  forecast(h = 14) %>% 
  autoplot(covid_RJ %>% filter(date >= "2021-08-01"))

covid_RJ %>% 
  filter(date >= "2021-08-01") %>% 
  model(ets = ETS(newDeaths),
        arima = ARIMA(newDeaths)) %>% 
  forecast(h = 14) %>% 
  autoplot(covid_RJ %>% 
             filter(date >= "2021-08-01"))
