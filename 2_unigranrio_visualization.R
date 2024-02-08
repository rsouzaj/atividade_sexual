library(tidyverse)


df |>
  group_by(redcap_data_access_group) |>
  summarise(
    N = n(),
    mean_age = mean(idade, na.rm=T),
    min_age = min(idade, na.rm=T),
    max_age = max(idade, na.rm=T)
  )


names(df)

df |>
  ggplot(aes(idade, fill=redcap_data_access_group))+
  geom_boxplot()

df |>
  mutate(
  imc = round(peso/((altura/100)^2), digits = 1)) |>
  ggplot(aes(imc, fill=redcap_data_access_group))+
  geom_boxplot()

df |>
  ggplot(aes(civil, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(atividade_fisica, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(escolaridade, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(etnia, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(anticoncepcional, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(antidepressivo, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(parto_normal, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(parto_cesaria, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(menopausa, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(reposicao_th, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(tempo_abstinencia_sexual, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(atrapalha_relacao, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(incontinencia_relacao___1, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(incontinencia_relacao___2, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(escala_incomodo, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(iciq_1, fill=redcap_data_access_group))+
  geom_bar(position = 'dodge')


df |>
  filter(redcap_data_access_group == 'unigranrio') |>
  ggplot(aes(iciq_1))+
  geom_bar()

df |>
  filter(redcap_data_access_group == 'uerj') |>
  ggplot(aes(iciq_1))+
  geom_bar()








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







