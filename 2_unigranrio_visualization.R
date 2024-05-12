library(tidyverse)


df <-
  readr::read_delim('OImpactoDaIncontinnc_DATA_2024-04-17_1750.csv',
                    delim = '|')

names(df)
str(df)



# Variável desfecho? ------------------------------------------------------

## Disfunção sexual - prevalência
### Qual a definição, como é medida?
#### Escore FSFI - quanto maior, pior (tem ponto de corte?)
###
### Em que poupulação?


# Tidy --------------------------------------------------------------------


df_breno <- df |>
  filter(
    redcap_data_access_group  == 'unigranrio', #filtrar somente Unigranrio
    sintomas_climaterio___1 == 1 | menopausa == 1 | # População: mulheres no climatério
      sintomas_climaterio___2 == 1 |
      sintomas_climaterio___3 == 1 |
      sintomas_climaterio___4 == 1
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


str(df_breno)
names(df_breno)


# Análise Univariada ------------------------------------------------------

df_breno |>
  ggplot(aes(idade)) +
  geom_boxplot()


mean(df_breno$idade)
median(df_breno$idade)


df_breno |>
  # filter(idade < 75) |>
  ggplot(aes(idade)) +
  geom_histogram( boundary = 45, bins = 6)

hist(df_fim$idade, nclass = 'FD')

df_breno |>
  ggplot(aes(imc))+
  geom_boxplot()

df_breno |>
  ggplot(aes(imc))+
  geom_histogram()


## Parece haver dois grupos (clusters) diferentes
df_fim|>
  ggplot(aes(fsfi_score))+
  geom_histogram(bins = nclass.FD(df_fim$fsfi_score),
                 boundary= 0)

hist(df_fim$fsfi_score, breaks = 'FD')

df_breno |>
  ggplot(aes(fsfi_score, fill = as.factor(tipo_incontinencia)))+
  geom_boxplot()

df_breno |>
  ggplot(aes(idade, fsfi_score))+
  geom_point()+
  geom_smooth(method = 'lm')

funcao_sex <- function(x){
  df_breno |>
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




df_breno |>
  select(fsfi_excitacao, fsfi_lubrificacao) |> View()

df_breno |>
  select(fsfi_interesse:fsfi_dor) |>
  gtsummary::tbl_summary()


df_breno |>
  select(fsfi_lubrificacao, fsfi_orgasmo) |> View()

## Temos que trabalhar os dados para torná-los homogêneos
df_breno |>
  select(fsfi_dor, fsfi_orgasmo) |> View()

## Temos que manipular os dados para que fiquem coerentes

df_breno_controle <- df_breno |>
  filter(!is.na(fsfi_interesse)) |> # retira as observações NA
  mutate(
    fsfi_excitacao = if_else(fsfi_orgasmo == 0, 0, fsfi_excitacao),
    fsfi_lubrificacao = if_else(fsfi_orgasmo == 0, 0, fsfi_lubrificacao),
    ) |>
  rowwise() |> # tem que refazer o escore
  mutate(fsfi_score = sum(c_across(fsfi_interesse:fsfi_dor)))


df_breno_controle |>
  ggplot(aes(fsfi_score))+
  geom_histogram()


df_breno |>
  ggplot(aes(fsfi_score))+
  geom_histogram()





df_breno |>
  group_by(tipo_incontinencia) |>
summarise(
  N = n(),
       media_idade = mean(idade),
       mediana_idade = median(idade),
       media_imc = mean(imc, na.rm = T),
       mediana_imc = median(imc, na.rm = T)
       )






df |>
  ggplot(aes(idade, fill = redcap_data_access_group)) +
  geom_boxplot()

df |>
  mutate(imc = round(peso / ((altura / 100) ^ 2), digits = 1)) |>
  ggplot(aes(imc, fill = redcap_data_access_group)) +
  geom_boxplot()

df |>
  ggplot(aes(civil, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(atividade_fisica, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(escolaridade, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(etnia, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(anticoncepcional, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(antidepressivo, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(parto_normal, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(parto_cesaria, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(menopausa, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(reposicao_th, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(tempo_abstinencia_sexual, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(atrapalha_relacao, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(incontinencia_relacao___1, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(incontinencia_relacao___2, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(escala_incomodo, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(iciq_1, fill = redcap_data_access_group)) +
  geom_bar(position = 'dodge')


df |>
  filter(redcap_data_access_group == 'unigranrio') |>
  ggplot(aes(iciq_1)) +
  geom_bar()

df |>
  filter(redcap_data_access_group == 'uerj') |>
  ggplot(aes(iciq_1)) +
  geom_bar()








df_fim <- read.csv('df_unigranrio.csv')

str(df_fim)

glimpse(df_fim)


# Banco climatério --------------------------------------------------------

# Pacientes com menopausa estabelecida ou com ao menos um sintomas de climatério

df_climaterio <- df_fim |>
  #filter(menopausa=='Sim' | sintomas_climaterio___1 == 1 | sintomas_climaterio___2 == 1 | sintomas_climaterio___3 == 1 | sintomas_climaterio___4 == 1)|>
  mutate(tempo_abstinencia_sexual  = fct_relevel(
    tempo_abstinencia_sexual,
    c(
      "Há menos de um mês",
      "Entre um e seis meses",
      "Entre seis meses e um ano",
      "Há mais de um ano"
    )
  ))


# Data visualization ------------------------------------------------------

df_climaterio |>
  ggplot(aes(idade)) +
  geom_boxplot()

df_climaterio |>
  ggplot(aes(imc)) +
  geom_boxplot()

df_climaterio |>
  ggplot(aes(civil)) +
  geom_bar()

df_climaterio |>
  ggplot(aes(escolaridade)) +
  geom_bar()

df_climaterio |>
  ggplot(aes(etnia)) +
  geom_bar()

df_climaterio |>
  ggplot(aes(menopausa)) +
  geom_bar()

df_climaterio |>
  ggplot(aes(tempo_abstinencia_sexual)) +
  geom_bar()


# Análise para simpósio Unigranrio ----------------------------------------


df_climaterio |>
  select(
    idade,
    imc,
    civil,
    atividade_fisica,
    escolaridade,
    etnia,
    reposicao_th,
    tempo_abstinencia_sexual,
    fsfi_score,
    atrapalha_relacao
  ) |>
  gtsummary::tbl_summary(by = tempo_abstinencia_sexual,
                         missing = 'no') |>
  gtsummary::add_overall() |>
  gtsummary::as_flex_table()
