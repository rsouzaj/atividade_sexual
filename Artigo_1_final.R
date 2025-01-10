
library(tidyverse)
library(officer)
library(gtsummary)




# Data tidy ---------------------------------------------------------------


df <-
  readr::read_delim('OImpactoDaIncontinnc_DATA_2024-11-28_1035.csv',
                    delim = '|')

## Idade entre 45 e 65 anos
## Sintomas de climatério ou amenorreia (menopausa)



# equivalência das respostas ----------------------------------------------

# Quando a mulher responde que não teve relações nas últimas 4 semanas, todas as
# variáveis abaixo deveriam ser iguais, mas algumas mulheres colocam  assinalam
# outras alternativas.
# Tive que igualar todas a zero.


vars_of_interest <- c("fsfi_excitacao", 'fsfi_lubrificacao', 'fsfi_orgasmo', 'fsfi_dor')

df <- df |>
  mutate(across(all_of(vars_of_interest),
                ~ if_else(rowSums(across(all_of(vars_of_interest)) == 0) > 0, 0, .)))



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
         iciq_3 = as.numeric(iciq_3),

         ) |>
  filter(!is.na(fsfi_interesse)) |>
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
    `Vaginal dryness` = if_else(sintomas_climaterio___4 == 0, 'No', 'Yes'),

    fsfi_interesse_label = case_when(
      fsfi_interesse == 5 ~ "Almost always or always",
      fsfi_interesse == 4 ~ "Most times (more than half the time)",
      fsfi_interesse == 3 ~ "Sometimes (about half the time)",
      fsfi_interesse == 2 ~ "A few times (less than half the time)",
      fsfi_interesse == 1 ~ "Almost never or never"
    ),
    fsfi_satisfacao_label = case_when(
      fsfi_satisfacao == 5 ~ "Very satisfied",
      fsfi_satisfacao == 4 ~ "Moderately satisfied",
      fsfi_satisfacao == 3 ~ "About equally satisfied and dissatisfied",
      fsfi_satisfacao == 2 ~ "Moderately dissatisfied",
      fsfi_satisfacao == 1 ~ "Very dissatisfied"
    )

  ) |>
  filter(!is.na(disfuncao_sexual))


# Outcome -----------------------------------------------------------------

## Female sexual dysfunction -> more than 6 months without sexual relationship

##  Variable -> disfuncao_sexual (Yes or No)
##  Variable -> FSFI (score)
##  Variable -> FSFI (dimensions)


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
  gtsummary::add_p(test= list(
    c(idade, imc, fsfi_score) ~ "t.test"
    ))|>
  gtsummary::bold_labels() |>
  gtsummary::modify_header(label = '**Characteristics**') |>
  gtsummary::as_flex_table() |>
  flextable::width(j = 1:2, width = 2.5)



# Exportação para Word
doc_1 <- read_docx() |>
  officer::body_add_par("Table 1: Characteristics of study participants stratified by sexual dysfunction status",
                        style = "Table Caption") |>
  body_add_par("") |>
  flextable::body_add_flextable(value = table_1)

# Salvar o documento
print(doc_1, target = "tabela_1.docx")


# Como está o interesse entre os dois grupos de mulheres? -----------------


table_2 <- df_artigo_1_fim |>
  select(fsfi_interesse, fsfi_satisfacao, disfuncao_sexual) |>
  gtsummary::tbl_summary(
    missing = 'no',
    by = disfuncao_sexual,
    type = list(starts_with("fsfi") ~ "continuous"),
    # statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_continuous() ~ 0),
    label = list(
      fsfi_interesse = "Interest",
      fsfi_satisfacao = "Satisfaction"
    )
  ) |>
  gtsummary::add_p()|>
  gtsummary::bold_labels() |>
  gtsummary::modify_header(label = '**Sexual dimensions**') |>
  gtsummary::as_flex_table() |>
  flextable::width(j = 1:2, width = 2.5)

# Export
doc_2 <- read_docx() |>
  officer::body_add_par(
    "Table 2: sexual interest and satisfaction according to sexual function",
    style = "Table Caption"
  ) |>
  body_add_par("") |>
  flextable::body_add_flextable(value = table_2)

# save

print(doc_2, target = "tabela_2.docx")



# Perguntas... ------------------------------------------------------------

## Por que alguma mulheres com disfunção sexual estão satisfeitas?
## por que as mulheres com função sexual adequada são as que se queixam de ressecamento?
##




# Figura 1 ----------------------------------------------------------------

figure_1 <- df_artigo_1_fim |>
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


ggsave("figure_1.png", plot, width = 8, height = 6, dpi = 300)

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



