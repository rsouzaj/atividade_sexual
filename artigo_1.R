

# Pacotes -----------------------------------------------------------------


library(tidyverse)



# Banco de dados ----------------------------------------------------------

df <-
  readr::read_delim('OImpactoDaIncontinnc_DATA_2024-06-13_1844.csv',
                    delim = '|')

## Idade entre 45 e 65 anos
## Sintomas de climatério ou amenorreia (menopausa)

df_artigo_1 <- df |>
  filter(
    # redcap_data_access_group  == 'unigranrio', #filtrar somente Unigranrio
    idade <= 65 & idade >= 45,
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
    starts_with('sintomas_climaterio'),
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
      iciq_4___1 == 1 | is.na(iciq_score) ~ 'Continent',
      IUU == 0 & IUE == 0 ~ 'Continent',
      IUU == 1 & IUE == 1 ~ 'MUI',
      IUU == 1 & IUE == 0 ~ 'UUI',
      IUU == 0 & IUE == 1 ~ 'SUI'
    )
  )




# Label  ------------------------------------------------------------------


df_artigo_1_fim <- df_artigo_1 |>
mutate(
  civil = case_when(
    civil == 1 ~ 'Single',
    civil == 2 ~ 'Married',
    civil == 3 ~ 'Married', # considerei união estável como casada
    civil == 4 ~ 'Divorced',
    civil == 5 ~ 'Widow'
  ),
  tabagismo = case_when(
    tabagismo == 0 ~ 'No',
    tabagismo == 1 ~ 'Yes',
    tabagismo == 2 ~ 'Interrupted'
  ),
  atividade_fisica = case_when(
    atividade_fisica == 0 ~ 'No',
    atividade_fisica == 1 ~ 'Yes',
    atividade_fisica == 2 ~ 'Never'
  ),
  escolaridade = case_when(
    escolaridade == 1 ~ 'Illiterate',
    escolaridade == 2 ~'Elementary School incomplete',
    escolaridade == 3 ~ 'Elementary School complete',
    escolaridade == 4 ~ 'High School',
    escolaridade == 5 ~ 'Higher Education'
  ),
  escolaridade = fct(escolaridade),
  escolaridade = fct_relevel(escolaridade,
                             c(
                               'Illiterate',
                               'Elementary School incomplete',
                               'Elementary School complete',
                               'High School',
                               'Higher Education'
                             )
  ),
  etnia = case_when(
    etnia == 1 ~'White',
    etnia == 2 ~ 'Black',
    etnia == 3 ~'Black',
    etnia == 4 ~'Asian',
    etnia == 5 ~'Brazilian Indian'
  ),
  antidepressivo = if_else(
    antidepressivo == 0 , 'No', 'Yes'
  ),
  reposicao_th = fct( case_when(
    reposicao_th == 0 ~'No',
    reposicao_th == 1 ~ 'Yes',
    reposicao_th == 2 ~ 'Interrupted'
  )),

  disfuncao_sexual = if_else(tempo_abstinencia_sexual<3, "No", "Yes"),
  disfuncao_sexual_binary = if_else(tempo_abstinencia_sexual<3, 0, 1),

  tempo_abstinencia_sexual = case_when(
    tempo_abstinencia_sexual == 1 ~ 'Less than a month',
    tempo_abstinencia_sexual == 2 ~ 'Between one and six months',
    tempo_abstinencia_sexual == 3 ~ 'Between six and twelve months',
    tempo_abstinencia_sexual == 4 ~ 'More than twelve months'
  ),
  tempo_abstinencia_sexual = fct(tempo_abstinencia_sexual),
  tempo_abstinencia_sexual = fct_relevel(
    tempo_abstinencia_sexual,
    c(
      'Less than a month',
      'Between one and six months',
      "Between six and twelve months",
      'More than twelve months'
    )
  ),

  atrapalha_relacao = case_when(
    atrapalha_relacao == 0 ~ 'I have normal sex',
    atrapalha_relacao == 1 ~ 'Urinay incontinence',
    # atrapalha_relacao == 1 ~ 'Other reasons',  # alternative
    atrapalha_relacao == 2 ~'Lack of partner',
    atrapalha_relacao == 3 ~'Partner problems',
    atrapalha_relacao == 4 ~'I do not want any more',
    atrapalha_relacao == 5 ~ 'Other reasons'
  ),
  atrapalha_relacao = fct(atrapalha_relacao),
  atrapalha_relacao = fct_relevel(atrapalha_relacao,
                                  c(
                                    'I have normal sex',
                                    'I do not want any more',
                                    'Lack of partner',
                                    'Partner problems',
                                    'Urinay incontinence',
                                    'Other reasons'
                                  )
  ),
  iciq_3 = as.numeric(iciq_3),
  `Hot flashes` = if_else(sintomas_climaterio___1 == 0, 'No', 'Yes'),
  `Hot flashes` = if_else(sintomas_climaterio___2 == 0, 'No', 'Yes'),
  Oligomenorrhea = if_else(sintomas_climaterio___3 == 0, 'No', 'Yes'),
  `Vaginal dryness` = if_else(sintomas_climaterio___4 == 0, 'No', 'Yes')

)



# Outcome -----------------------------------------------------------------

## Female sexual dysfunction -> more than 6 months without sexual relationship

##  Variable -> disfuncao_sexual (Yes or No)
##  Variable -> FSFI (score)
##  Variable -> FSFI (dimensions)

df_artigo_1_fim |>
  count(disfuncao_sexual)
#
# disfuncao_sexual        n
# <chr>               <int>
# 1 No                  33   57.8%
# 2 Yes                 24   42.2%





# Visualização ------------------------------------------------------------


glimpse(
df_artigo_1_fim
)

summary(df_artigo_1_fim)



df_artigo_1_fim |>
  ggplot(aes(idade, fill = disfuncao_sexual))+
 geom_boxplot()

df_artigo_1_fim |>
  ggplot(aes(imc, fill = disfuncao_sexual))+
  geom_boxplot()

df_artigo_1_fim |>
  ggplot(aes(etnia, fill = disfuncao_sexual))+
  geom_bar(position = 'dodge')

df_artigo_1_fim |>
  ggplot(aes(civil, fill = disfuncao_sexual))+
  geom_bar(position = 'dodge')

df_artigo_1_fim |>
  ggplot(aes(escolaridade, fill = disfuncao_sexual))+
  geom_bar(position = 'dodge')

df_artigo_1_fim |>
  ggplot(aes(atividade_fisica, fill = disfuncao_sexual))+
  geom_bar(position = 'dodge')

df_artigo_1_fim |>
  ggplot(aes(sintomas_climaterio___1, fill = disfuncao_sexual))+
  geom_bar(position = 'dodge') # hot flashes

df_artigo_1_fim |>
  ggplot(aes(sintomas_climaterio___2, fill = disfuncao_sexual))+
  geom_bar(position = 'dodge') # hot flashes

df_artigo_1_fim |>
  ggplot(aes(sintomas_climaterio___3, fill = disfuncao_sexual))+
  geom_bar(position = 'dodge') # Oligomenorrhea

df_artigo_1_fim |>
  ggplot(aes(sintomas_climaterio___4, fill = disfuncao_sexual))+
  geom_bar(position = 'dodge') # vaginal dryness

df_artigo_1_fim |>
  ggplot(aes(antidepressivo, fill = disfuncao_sexual))+
  geom_bar(position = 'dodge')

df_artigo_1_fim |>
  ggplot(aes(reposicao_th, fill = disfuncao_sexual))+
  geom_bar(position = 'dodge')


df_artigo_1_fim |>
  ggplot(aes(atrapalha_relacao, fill = disfuncao_sexual))+
  geom_bar(position = 'dodge') #Para você, o que atrapalha ou impede a
                              #  relação sexual no momento?




# Tests -------------------------------------------------------------------

names(df_artigo_1_fim)


t.test(df_artigo_1_fim$idade ~ df_artigo_1_fim$disfuncao_sexual)
# p-value = 0.008089

#p-value = 1.695e-05 (including UERJ)

t.test(df_artigo_1_fim$imc ~ df_artigo_1_fim$disfuncao_sexual)
# p-value = 0.3068
# p-value = 0.6793

chisq.test(df_artigo_1_fim$escolaridade, df_artigo_1_fim$disfuncao_sexual)
# p-value = 0.82

fisher.test(df_artigo_1_fim$escolaridade, df_artigo_1_fim$disfuncao_sexual)
# p-value = 0.8619

chisq.test(df_artigo_1_fim$antidepressivo, df_artigo_1_fim$disfuncao_sexual)

fisher.test(df_artigo_1_fim$antidepressivo, df_artigo_1_fim$disfuncao_sexual)

chisq.test(df_artigo_1_fim$atrapalha_relacao, df_artigo_1_fim$disfuncao_sexual)
# p-value = 4.667e-08

summary(
glm(data = df_artigo_1_fim, disfuncao_sexual_binary ~ atrapalha_relacao, family = binomial() )
)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
#   (Intercept)                               -2.1972     0.6086  -3.610 0.000306 ***
#   atrapalha_relacaoI do not want any more    3.8067     0.9851   3.864 0.000111 ***
#   atrapalha_relacaoLack of partner           4.1431     0.9705   4.269 1.96e-05 ***
#   atrapalha_relacaoPartner problems         19.7633  1978.0903   0.010 0.992028
#   atrapalha_relacaoUrinay incontinence       1.1676     0.8011   1.457 0.144990
#   atrapalha_relacaoOther reasons             1.5041     0.7876   1.910 0.056183 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 134.96  on 98  degrees of freedom
# Residual deviance:  87.19  on 93  degrees of freedom
# AIC: 99.19
#
# Number of Fisher Scoring iterations: 16




chisq.test(df_artigo_1_fim$sintomas_climaterio___1, df_artigo_1_fim$disfuncao_sexual)
# p-value = 0.2192

chisq.test(df_artigo_1_fim$sintomas_climaterio___2, df_artigo_1_fim$disfuncao_sexual)
# p-value = 0.1079

chisq.test(df_artigo_1_fim$sintomas_climaterio___3, df_artigo_1_fim$disfuncao_sexual)
# p-value = 0.6192


chisq.test(df_artigo_1_fim$sintomas_climaterio___4, df_artigo_1_fim$disfuncao_sexual)
# p-value = 0.3154

fisher.test(df_artigo_1_fim$comorbidades___6, df_artigo_1_fim$disfuncao_sexual)
# p-value = 0.0269
# p-value = 0.163
# Fibromialgia

chisq.test(df_artigo_1_fim$comorbidades___6, df_artigo_1_fim$disfuncao_sexual)
#  p-value = 0.05652

df_artigo_1_fim |>
  count(comorbidades___6)
# comorbidades___6        n
#                 <dbl> <int>
# 1                0    53
# 2                1     4

# UERJ
#   comorbidades___6            n
# #                   <dbl> <int>
#     1                0    90
#     2                1     9
#
#


chisq.test(df_artigo_1_fim$comorbidades___10, df_artigo_1_fim$disfuncao_sexual)






    hist(df_artigo_1$idade, nclass = 15, xlim = range(45,65))


  hist(df_artigo_1$idade, nclass = 'FD', xlim = range(45,65))

df_artigo_1 |>
  ggplot(aes(imc))+
  geom_boxplot()

df_artigo_1 |>
  ggplot(aes(idade))+
  geom_histogram( bins = 8)



