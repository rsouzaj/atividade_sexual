library(tidyverse)



df <-
  readr::read_delim('OImpactoDaIncontinnc_DATA_2024-04-03_1724.csv',
                    delim = '|')

names(df)
str(df)


# Definir variável desfecho para cada trabalho ----------------------------



df_uerj <- df |>
  filter(
    redcap_data_access_group  == 'uerj' #filtrar somente UERJ
      ) |>
  mutate(imc = round(peso / ((altura / 100) ^ 2), digits = 1),
         iciq_3 = as.numeric(iciq_3)) |>
  select(
    record_id,
    idade,
    imc,
    civil,
    tabagismo,
    atividade_fisica,
    atividade_semanal,
    escolaridade,
    etnia,
    starts_with('comorbi'),
    parto_normal,
    parto_cesaria,
    antidepressivo,
    reposicao_th,
    tempo_abstinencia_sexual,
    atrapalha_relacao,
    incontinencia_relacao___1,
    incontinencia_relacao___2,
    escala_incomodo,
    starts_with('fsfi'),
    starts_with('iciq'),-ends_with('complete'),
    -ends_with('timestamp'),
    -data_consulta
  ) |>
  rowwise() |>
  mutate(
    iciq_score = sum(c_across(iciq_1:iciq_3)),
    fsfi_score = sum(c_across(fsfi_interesse:fsfi_dor)),
    IUU = if_else(
      iciq_4___2 == 1 |
        iciq_4___3 == 1 |
        iciq_4___4 == 1 |
        iciq_4___6 == 1 | iciq_4___7 == 1 | iciq_4___8 == 1,
      '1',
      '0'
    ),
    IUE = if_else(iciq_4___2 == 1 | iciq_4___5 == 1,
                  '1', '0'),
    tipo_incontinencia = case_when(
      iciq_4___1 == 1 | is.na(iciq_score) ~ 'Continente',
      IUU == 0 & IUE == 0 ~ 'Continente',
      IUU == 1 & IUE == 1 ~ 'IUM',
      IUU == 1 & IUE == 0 ~ 'IUU',
      IUU == 0 & IUE == 1 ~ 'IUE'
    )
  )


# Análise Univariada ------------------------------------------------------

df_uerj |>
  ggplot(aes(idade)) +
  geom_boxplot()


mean(df_uerj$idade, na.rm = T)
median(df_uerj$idade, na.rm = T)


df_uerj |>
  # filter(idade < 75) |>
  ggplot(aes(idade)) +
  geom_histogram()


df_uerj |>
  ggplot(aes(imc))+
  geom_boxplot()

df_uerj |>
  ggplot(aes(imc))+
  geom_histogram()


## Parece haver dois grupos (clusters) diferentes
df_uerj |>
  ggplot(aes(fsfi_score))+
  geom_histogram()


df_uerj |>
  ggplot(aes(fsfi_score, fill = tipo_incontinencia))+
  geom_boxplot()

df_uerj |>
  ggplot(aes(idade, fsfi_score))+
  geom_point()+
  geom_smooth(method = 'lm')

funcao_sex <- function(x){
  df_uerj |>
    ggplot(aes({{x}}))+
    geom_bar()
}


funcao_sex(fsfi_interesse)
funcao_sex(fsfi_excitacao) # Algumas mulheres responderam diferente
# devem ter confundido excitação fora da atividade sexual

funcao_sex(fsfi_lubrificacao) # lubrificação tb
funcao_sex(fsfi_orgasmo)
funcao_sex(fsfi_satisfacao)
funcao_sex(fsfi_dor)

