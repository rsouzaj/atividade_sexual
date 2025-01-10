

# Pacotes -----------------------------------------------------------------


library(tidyverse)
library(officer)



# Banco de dados ----------------------------------------------------------

df <-
  readr::read_delim('OImpactoDaIncontinnc_DATA_2024-11-28_1035.csv',
                    delim = '|')

## Idade entre 45 e 65 anos
## Sintomas de climatério ou amenorreia (menopausa)

df_artigo_1 <- df |>
  mutate(
    menopausa = ifelse(idade>=55, 1, menopausa) # Considerado todas as mulheres acima de 55 na menopausa
  ) |>
  filter(
    # redcap_data_access_group  == 'unigranrio', #filtrar somente Unigranrio
    idade <= 65 & idade >= 45,
    sintomas_climaterio___1 == 1 | menopausa == 1 | # População: mulheres no climatério
                                                    # ou com algum sintoma
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
  ) |>
  filter(comorbidades___1 != 1)




# Label  ------------------------------------------------------------------


df_artigo_1_fim <- df_artigo_1 |>
mutate(
  civil = case_when(
    civil == 1 ~ 'Single',
    civil == 2 ~ 'Married',
    civil == 3 ~ 'Civil Union', # considerei união estável como casada
    civil == 4 ~ 'Divorced',
    civil == 5 ~ 'Widowed'
  ),
  civil = fct(civil),
  civil = fct_relevel(
    civil,
    c('Single', 'Civil Union', 'Married', 'Divorced', 'Widowed')
  ),
  tabagismo = case_when(
    tabagismo == 0 ~ 'No',
    tabagismo == 1 ~ 'Yes',
    tabagismo == 2 ~ 'Former smoker'
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

  disfuncao_sexual = if_else(tempo_abstinencia_sexual>2, "Dysfunctional", "Functional"),
  disfuncao_sexual_binary = if_else(tempo_abstinencia_sexual>2, 1, 0),

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

) |>
  filter(!is.na(disfuncao_sexual))



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

table(df_artigo_1_fim$antidepressivo, df_artigo_1_fim$disfuncao_sexual)

#     No Yes
# No  49  34
# Yes  8   9

fisher.test(df_artigo_1_fim$antidepressivo, df_artigo_1_fim$disfuncao_sexual)
#p-value = 0.4254
#
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


summary(
  glm(data = df_artigo_1_fim, disfuncao_sexual_binary ~ idade+ atrapalha_relacao + civil, family = binomial() )
)

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
  count(comorbidades___1) # Câncer

df_artigo_1_fim |>
  count(comorbidades___2) # Diabetes
#   comorbidades___2     n
#<dbl> <int>
  #1                0    95
  #2                1    29

df_artigo_1_fim |>
  count(comorbidades___3) # doença autoimune


df_artigo_1_fim |>
  count(comorbidades___4) # Endometriose

df_artigo_1_fim |>
  count(comorbidades___5)


df_artigo_1_fim |>
  count(comorbidades___6) #Fibromialgia
# comorbidades___6     n
# <dbl> <int>
# 1                0   113
# 2                1    11


df_artigo_1_fim |>
  count(comorbidades___7)

df_artigo_1_fim |>
  count(comorbidades___8) # HAS
# comorbidades___8     n
# <dbl> <int>
# 1                0    66
# 2                1    58


df_artigo_1_fim |>
  count(comorbidades___9) # SBDF

df_artigo_1_fim |>
  count(comorbidades___10) #Sindrome do intestino irritável


# UERJ
#   comorbidades___6            n
# #                   <dbl> <int>
#     1                0    90
#     2                1     9
#
#


chisq.test(df_artigo_1_fim$comorbidades___10, df_artigo_1_fim$disfuncao_sexual)




# Table 1 -----------------------------------------------------------------

min(df_artigo_1_fim$idade)
max(df_artigo_1_fim$idade)

## Verificar se hot flashes e vaginal dryness afetam o escore

vaginal_dry <- glm(data = df_artigo_1_fim,
    fsfi_score~`Vaginal dryness`)

hot_flashes <- glm(data = df_artigo_1_fim,
                   fsfi_score~`Hot flashes`)

oligome <- glm(data = df_artigo_1_fim,
                   fsfi_score~Oligomenorrhea)

summary(vaginal_dry)
summary(hot_flashes)
summary(oligome)

gtsummary::tbl_regression(vaginal_dry)

gtsummary::tbl_regression(hot_flashes)

gtsummary::tbl_regression(oligome)

gtsummary::tbl_regression(c(vaginal_dry, hot_flashes))

chisq.test(df_artigo_1_fim$`Hot flashes`,
         df_artigo_1_fim$`Vaginal dryness`)

table(df_artigo_1_fim$`Hot flashes`,
       df_artigo_1_fim$`Vaginal dryness`)

# Aplicação do tema do JAMA
gtsummary::theme_gtsummary_journal("jama")

# Tema BMJ

gtsummary::theme_gtsummary_journal("bmj")


table_1 <- df_artigo_1_fim |>
  select(
    idade:civil,
    reposicao_th,
    # tempo_abstinencia_sexual,
    # atrapalha_relacao,
    fsfi_score,
    `Vaginal dryness`,
    `Hot flashes`,
    disfuncao_sexual
  ) |>
  gtsummary::tbl_summary(
    by = disfuncao_sexual,
    statistic = list(
      c(idade, imc, fsfi_score
        # parto_normal, parto_cesaria
      )~"{mean} ({sd})"
    ),
    missing = 'no',
    label = list(
      idade = "Age; years",
      imc = "BMI",
      civil = "Marital status",
      reposicao_th = "Hormonal therapy",
      # atrapalha_relacao = "Affect sexual intimacy",
      fsfi_score = "FSFI score"
    )
  ) |>
  gtsummary::add_p()|>
  gtsummary::bold_labels() |>
  gtsummary::modify_header(label = '**Characteristics**') |>
  gtsummary::as_flex_table() |>
  flextable::width(j = 1:2, width = 2.5)



# Exportação para Word
doc <- read_docx() |>
  officer::body_add_par("Table 1: Characteristics of study participants stratified by sexual dysfunction status",
                        style = "Table Caption") |>
  body_add_par("") |>
  flextable::body_add_flextable(value = table_1)

# Salvar o documento
print(doc, target = "tabela_1.docx")

# Idade vs parceiro -------------------------------------------------------

df_artigo_1_fim |>
  filter(!is.na(atrapalha_relacao)) |>
  ggplot(aes(idade, fill = atrapalha_relacao))+
  geom_boxplot()




# Table 2 ----------------------------------------------------------------


df_artigo_1_fim |>
  select(idade, civil, atividade_fisica,
         tempo_abstinencia_sexual, starts_with('sintomas')) |>
  gtsummary::tbl_summary(
    by = tempo_abstinencia_sexual,
    missing = 'no',
    statistic = list(
      c(idade, fsfi_score)~"{mean} ({sd})"),
    label = c(
      idade ~ "Idade (anos)",
      civil ~ "Estado civil",
      atividade_fisica ~"Prática de atividade física",
      fsfi_score ~ 'Escore do FSFI'
    )
  )|>
  gtsummary::bold_labels() |>
  gtsummary::add_p() |>
  gtsummary::modify_header(label = '**Características**') |>
  gtsummary::as_flex_table() |>
  flextable::save_as_docx(path = "table_2.docx")





# Patients without dysfunction --------------------------------------------

df_dimensions |>
  filter(disfuncao_sexual =="No") |>
  ggplot(aes(dimensions_score, fill = sexual_dimensions))+
  geom_boxplot() +
    facet_wrap(~civil)


df_dimensions |>
  filter(disfuncao_sexual =="No") |>
  ggplot(aes( idade, dimensions_score))+
  geom_point()+
  facet_wrap(~sexual_dimensions)


df_dimensions |>
  filter(disfuncao_sexual =="No") |> # Mulheres que tiveram relação nos últimos 6 meses
  ggplot(aes( sexual_dimensions, dimensions_score, fill = `Hot flashes`))+
  geom_boxplot()



df_dimensions |>
  filter(disfuncao_sexual =="No") |> # Mulheres que tiveram relação nos últimos 6 meses
  ggplot(aes( sexual_dimensions, dimensions_score, fill = `Vaginal dryness`))+
  geom_boxplot()

# Table 3 -----------------------------------------------------------------



df_artigo_1_fim |>
 select(fsfi_interesse:fsfi_dor,
        `Hot flashes`) |>
  gtsummary::tbl_summary(
    by = `Hot flashes`,
    missing = "no",
    type = list(fsfi_interesse:fsfi_dor ~ "continuous")
    )  |>
  gtsummary::add_p()


df_artigo_1_fim |>
  filter(!is.na(fsfi_score)) |>
  select(fsfi_interesse:fsfi_dor,
         `Vaginal dryness`) |>
  gtsummary::tbl_summary(
    by = `Vaginal dryness`,
    missing = "no",
    type = list(fsfi_interesse:fsfi_dor ~ "continuous")
  )  |>
  gtsummary::add_p()


# Medir o poder estatístico de uma amostra

# Dimentions --------------------------------------------------------------


## Algo errado que não está certo!
##
df_dimensions <- df_artigo_1_fim |>
  pivot_longer(
    cols = c(fsfi_interesse:fsfi_dor),
    names_to = "sexual_dimensions",
    values_to = "dimensions_score"
  )

  df_dimensions |>
    ggplot(aes(dimensions_score, idade, fill = 'sexual_dimentions'))+
    geom_point()


  df_artigo_1_fim |>
    ggplot(aes(f))

df_artigo_1_fim |>
  mutate(
    tempo_abstinencia_sexual= if_else(
      tempo_abstinencia_sexual == "Less than a month" | tempo_abstinencia_sexual == "Between one and six months",
      1, 0
    )
    )|>
  filter(tempo_abstinencia_sexual == 1) |>
  select(idade,imc,
         # escolaridade,
         reposicao_th,
         starts_with("sintomas_cl"),
         tempo_abstinencia_sexual,
         fsfi_interesse:fsfi_dor,
         ) |>

  gtsummary::tbl_summary(
    by = tempo_abstinencia_sexual,
    missing = 'no',
    statistic = list(
      c(idade, imc)~"{mean} ({sd})"),
    label = c(
      idade ~ "Idade (anos)"
      # civil ~ "Estado civil",
      # atividade_fisica ~"Prática de atividade física",
      # fsfi_score ~ 'Escore do FSFI'
    )
  )|>
  gtsummary::bold_labels() |>
  gtsummary::add_p() |>
  gtsummary::modify_header(label = '**Características**') |>
  gtsummary::as_flex_table()

















    hist(df_artigo_1$idade, nclass = 15, xlim = range(45,65))


  hist(df_artigo_1$idade, nclass = 'FD', xlim = range(45,65))

df_artigo_1 |>
  ggplot(aes(imc))+
  geom_boxplot()

df_artigo_1 |>
  ggplot(aes(idade))+
  geom_histogram( bins = 8)

table(df$fsfi_dor)
table(df$fsfi_excitacao)

count(as.character(df$fsfi_dor))

df |>
  filter(fsfi_lubrificacao == 0) |>
  select(starts_with('fsfi')) |>
  filter(fsfi_excitacao > 0)

test_rows <- df |>
  select(fsfi_excitacao, fsfi_lubrificacao, fsfi_orgasmo, fsfi_dor)

inconstistent_row <- test_rows[rowSums(test_rows == 0)> 0 & rowSums(test_rows==0)< 4,]
print(inconstistent_row)


vars_of_interest <- c("fsfi_excitacao", 'fsfi_lubrificacao', 'fsfi_orgasmo', 'fsfi_dor')

inconsistent_rows <- df[rowSums(df[, vars_of_interest] == 0) > 0 &
                          rowSums(df[, vars_of_interest] == 0) < 4, ]

print("Inconsistent rows:")
print(inconsistent_rows[, vars_of_interest])

df[rowSums(df[, vars_of_interest] == 0) > 0, vars_of_interest] <- 0

df_update <- df_artigo_1_fim |>
  mutate(across(all_of(vars_of_interest),
                ~ if_else(rowSums(across(all_of(vars_of_interest)) == 0) > 0, 0, .)))


df_update %>% select(all_of(vars_of_interest))

changed_rows <- df_update %>%
  filter(rowSums(across(all_of(vars_of_interest)) == 0) > 0) %>%
  select(all_of(vars_of_interest))

print(changed_rows, n = 50)

  0.037 < 0.05

  50/(50+73)

  73/(50+73)

  df_artigo_1_fim |>
    filter(tempo_abstinencia_sexual != 'Less than a month'
           & atrapalha_relacao != 'Urinay incontinence') |>
    ggplot(aes(fsfi_interesse, fill = atrapalha_relacao)) +
    geom_bar(position = 'dodge') +
    labs(
      x = "Sexual desire or interest",
      y = "Frequency",
      fill = "Interferes with relationship"  # Changed 'legend' to 'fill' for correct legend title
    ) +
    scale_x_continuous(breaks = 1:5,
                       labels = c(
                         "1" = "Almost never\nor never",
                         "2" = "A few times\n(less than half\nthe time)",
                         "3" = "Sometimes\n(about half\nthe time)",
                         "4" = "Most times\n(more than half\nthe time)",
                         "5" = "Almost always\nor always"
                       )
    ) +
    theme_minimal(base_size = 14) +  # Increased base font size
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      strip.background = element_rect(fill = "lightgray"),
      strip.text = element_text(face = "bold")
    ) +
    scale_fill_brewer(palette = "Set2")


  ## Grey scale


  df_artigo_1_fim |>
    filter(tempo_abstinencia_sexual != 'Less than a month'
           & atrapalha_relacao != 'Urinay incontinence') |>
    ggplot(aes(fsfi_interesse, fill = atrapalha_relacao)) +
    geom_bar(position = 'dodge') +
    labs(
      x = "Sexual desire or interest",
      y = "Frequency",
      fill = "Interferes with relationship"
    ) +
    scale_x_continuous(breaks = 1:5,
                       labels = c(
                         "1" = "Almost never\nor never",
                         "2" = "A few times\n(less than half\nthe time)",
                         "3" = "Sometimes\n(about half\nthe time)",
                         "4" = "Most times\n(more than half\nthe time)",
                         "5" = "Almost always\nor always"
                       )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
      axis.text.y = element_blank(),  # Exclude y-axis text
      axis.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      strip.background = element_rect(fill = "lightgray"),
      strip.text = element_text(face = "bold"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    ) +
    scale_fill_grey(start = 0.2, end = 0.9) +
    guides(fill = guide_legend(reverse = TRUE))
