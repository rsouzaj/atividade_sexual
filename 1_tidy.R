library(tidyverse)


# Read data ---------------------------------------------------------------

df <-  readr::read_delim('OImpactoDaIncontinnc_DATA_2023-12-26_1813.csv',
                        delim = '|')


View(df)

str(df)

glimpse(df)


# Selecting and treating data ---------------------------------------------


df |>
  mutate(
    imc = round(peso/((altura/100)^2), digits = 1),
    civil = case_when(
      civil == 1 ~ 'Solteira',
      civil == 2 ~ 'Casada',
      civil == 3 ~ 'Casada', # considerei união estável como casada
      civil == 4 ~ 'Divorciada',
      civil == 5 ~ 'Viúva'
    ),
    tabagismo = case_when(
      tabagismo == 0 ~ 'Não',
      tabagismo == 1 ~ 'Sim',
      tabagismo == 2 ~ 'Interrompeu'
    ),
    atividade_fisica = case_when(
      atividade_fisica == 0 ~ 'Não',
      atividade_fisica == 1 ~ 'Sim',
      atividade_fisica == 2 ~ 'Nunca'
    ),
    escolaridade = case_when(
      escolaridade == 1 ~ 'Analfabeta',
      escolaridade == 2 ~'Fundamental incompleto',
      escolaridade == 3 ~ 'Fundamental completo',
      escolaridade == 4 ~ 'Médio',
      escolaridade == 5 ~ 'Superior ou maior'
    ),
    etnia = case_when(
      etnia == 1 ~'Branca',
      etnia == 2 ~ 'Parda',
      etnia == 3 ~'Preta',
      etnia == 4 ~'Amarela',
      etnia == 5 ~'Indígena'
    ),
    anticoncepcional = case_when(
      anticoncepcional == 0 ~ 'Não',
      anticoncepcional == 1 ~'Hormônio oral',
      anticoncepcional == 2 ~'Hormônio transdérmico',
      anticoncepcional == 3 ~ 'Hormônio vaginal',
      anticoncepcional == 5 ~'DIU',
      anticoncepcional == 6 ~'Outros'
    ),
    antidepressivo = if_else(
      antidepressivo == 0 , 'Não', 'Sim'
    ),
    menopausa = if_else(
      menopausa == 0, 'Não', 'Sim'
    ),
    reposicao_th = case_when(
      reposicao_th == 0 ~'Não',
      reposicao_th == 1 ~ 'Sim',
      reposicao_th == 2 ~ 'Interrompeu'
    ),
    tempo_abstinencia_sexual = case_when(
      tempo_abstinencia_sexual == 1 ~ 'Há menos de um mês',
      tempo_abstinencia_sexual == 2 ~ 'Entre um e seis meses',
      tempo_abstinencia_sexual == 3 ~ 'Entre seis meses e um ano',
      tempo_abstinencia_sexual == 4 ~ 'Há mais de um ano'
    ),
    atrapalha_relacao = case_when(
      atrapalha_relacao == 0 ~ 'Eu tenho relação normal',
      atrapalha_relacao == 1 ~ 'Perda de urina',
      atrapalha_relacao == 2 ~'Falta de parceiro',
      atrapalha_relacao == 3 ~'Parceiro não pode',
      atrapalha_relacao == 4 ~'Não quero mais',
      atrapalha_relacao == 5 ~ 'Outros'
    ),
    iciq_3 = as.numeric(iciq_3)
  ) |>
  rowwise() |>
  mutate(
    iciq_score = sum(c_across(iciq_1:iciq_3)),
    fsfi_score = sum(c_across(starts_with('fsfi')))
    ) |>
  filter(
    comorbidades___1 != 1, # excuindo pacientes com câncer

  ) |>
  select(record_id, redcap_data_access_group, idade, imc, everything(),
         -ends_with('complete'), -ends_with('timestamp'), -data_consulta) -> df_tidy


# UERJ project ------------------------------------------------------------

df_uerj <- df_tidy |>
  filter(redcap_data_access_group == 'uerj')


readr::write_csv(df_uerj, 'df_uerj.csv')


# Unigranrio project ------------------------------------------------------

df_unigranrio <- df_tidy |>
  filter(redcap_data_access_group == 'unigranrio')

readr::write_csv(df_unigranrio, 'df_unigranrio.csv')








