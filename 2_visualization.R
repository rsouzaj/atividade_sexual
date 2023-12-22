library(tidyverse)


df_fim <- read.csv('df_unigranrio.csv')

str(df_fim)

glimpse(df_fim)


# Banco climatério --------------------------------------------------------

# Pacientes com menopausa estabelecida ou com ao menos um sintomas de climatério

df_climaterio <- df_fim |>
  #filter(menopausa=='Sim' | sintomas_climaterio___1 == 1 | sintomas_climaterio___2 == 1 | sintomas_climaterio___3 == 1 | sintomas_climaterio___4 == 1)|>
  mutate(
    tempo_abstinencia_sexual  = fct_relevel(tempo_abstinencia_sexual,
                                            c("Há menos de um mês", "Entre um e seis meses",
      "Entre seis meses e um ano", "Há mais de um ano"))
  )


# Data visualization ------------------------------------------------------

df_climaterio |>
  ggplot(aes(idade)) +
  geom_boxplot()

df_climaterio |>
  ggplot(aes(imc))+
  geom_boxplot()

df_climaterio |>
  ggplot(aes(civil))+
  geom_bar()

df_climaterio |>
  ggplot(aes(escolaridade))+
  geom_bar()

df_climaterio |>
  ggplot(aes(etnia))+
  geom_bar()

df_climaterio |>
  ggplot(aes(menopausa))+
  geom_bar()

df_climaterio |>
  ggplot(aes(tempo_abstinencia_sexual))+
  geom_bar()


# Análise para simpósio Unigranrio ----------------------------------------


df_climaterio |>
  select(idade, imc, civil,atividade_fisica,
         escolaridade,etnia, reposicao_th, tempo_abstinencia_sexual,
         fsfi_score, atrapalha_relacao) |>
  gtsummary::tbl_summary(
    by = tempo_abstinencia_sexual,
    missing ='no'
  ) |>
  gtsummary::add_overall() |>
  gtsummary::as_flex_table()







