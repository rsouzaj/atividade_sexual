library(tidyverse)

# Todas as mulheres estão na menopausa ou experimentando algum sintoma de climatério

df_fim <- df_breno|>

  mutate(
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
      escolaridade == 4 ~ 'Ensino médio',
      escolaridade == 5 ~ 'Superior ou maior'
    ),
    escolaridade = fct(escolaridade),
    escolaridade = fct_relevel(escolaridade,
      c(
        'Analfabeta',
        'Fundamental incompleto',
        'Fundamental completo',
        'Ensino médio',
        'Superior ou maior'
      )
    ),
    etnia = case_when(
      etnia == 1 ~'Branca',
      etnia == 2 ~ 'Parda',
      etnia == 3 ~'Preta',
      etnia == 4 ~'Amarela',
      etnia == 5 ~'Indígena'
    ),
    antidepressivo = if_else(
      antidepressivo == 0 , 'Não', 'Sim'
    ),
    reposicao_th = fct( case_when(
      reposicao_th == 0 ~'Não',
      reposicao_th == 1 ~ 'Sim',
      reposicao_th == 2 ~ 'Interrompeu'
    )),
    tempo_abstinencia_sexual = case_when(
      tempo_abstinencia_sexual == 1 ~ 'Há menos de um mês',
      tempo_abstinencia_sexual == 2 ~ 'Entre um e seis meses',
      tempo_abstinencia_sexual == 3 ~ 'Entre seis meses e um ano',
      tempo_abstinencia_sexual == 4 ~ 'Há mais de um ano'
    ),
    tempo_abstinencia_sexual = fct(tempo_abstinencia_sexual),
    tempo_abstinencia_sexual = fct_relevel(
      tempo_abstinencia_sexual,
      c(
        'Há menos de um mês',
        'Entre um e seis meses',
        "Entre seis meses e um ano",
        'Há mais de um ano'
      )
    ),

    atrapalha_relacao = case_when(
      atrapalha_relacao == 0 ~ 'Eu tenho relação normal',
      atrapalha_relacao == 1 ~ 'Outros motivos',
      atrapalha_relacao == 2 ~'Falta de parceiro',
      atrapalha_relacao == 3 ~'Parceiro não pode',
      atrapalha_relacao == 4 ~'Não quero mais',
      atrapalha_relacao == 5 ~ 'Outros motivos'
    ),
    atrapalha_relacao = fct(atrapalha_relacao),
    atrapalha_relacao = fct_relevel(atrapalha_relacao,
                                    c(
                                      'Eu tenho relação normal',
                                      'Não quero mais',
                                      'Falta de parceiro',
                                      'Parceiro não pode',
                                      'Outros motivos'
                                    )
                                    ),
    iciq_3 = as.numeric(iciq_3),
    `Fogachos` = if_else(sintomas_climaterio___1 == 0, 'Não', 'Sim'),
    `Fogachos` = if_else(sintomas_climaterio___2 == 0, 'Não', 'Sim'),
    Oligomenorreia = if_else(sintomas_climaterio___3 == 0, 'Não', 'Sim'),
    `Ressecamento vaginal` = if_else(sintomas_climaterio___4 == 0, 'Não', 'Sim')

  ) |>
  rowwise() |>
  mutate(
    iciq_score = sum(c_across(iciq_1:iciq_3)),
    fsfi_score = sum(c_across(starts_with('fsfi')))
  ) |>
  filter(
    comorbidades___1 != 1, # excuindo pacientes com câncer
    # !is.na(fsfi_score)

  ) |>

  select(record_id:etnia,Fogachos,`Ressecamento vaginal`, parto_normal:tipo_incontinencia, fsfi_score) |>
  mutate(
    atividade_semanal = if_else(is.na(atividade_semanal), 0 , atividade_semanal)
  ) |>
  filter(!is.na(fsfi_score))


# Tabela 1 ----------------------------------------------------------------

df_fim |>
  select(idade: atrapalha_relacao,
         -atividade_semanal, - parto_normal, -parto_cesaria) |>

gtsummary::tbl_summary(
  missing = 'no',
  # type = list(
  #   c(parto_cesaria, parto_normal)~'continuous'
  # ),
  statistic = list(
    c(idade, imc,
      # parto_normal, parto_cesaria
      )~"{mean} ({sd})"
  ),
  label = c(
    idade ~ "Idade (anos)",
    imc ~"IMC",
    civil ~ "Estado civil",
    tabagismo ~"Tagabismo",
    atividade_fisica ~"Prática de atividade física",
    escolaridade ~"Escolaridade",
    etnia ~"Raça",
    # parto_normal ~"Partos normais",
    # parto_cesaria~"Partos cesariana",
    antidepressivo~"Uso de antidepressivo",
    reposicao_th~ "Reposição hormonal",
    tempo_abstinencia_sexual~"Última relação sexual",
    atrapalha_relacao~"Principais queixas que interferiram na relação sexual"

  )
) |>
  gtsummary::bold_labels() |>
  gtsummary::modify_header(label = '**Características**') |>
  gtsummary::as_flex_table() |>
  flextable::save_as_docx(path = "table_1.docx")



# Objetivo ----------------------------------------------------------------

# Disfunção sexual
  # Ocorre com freqência
  # Crônica (meses)
  # sofrimento clinicamente significativo

# como definir DISFUNÇÃO SEXUAL população?


## Idade é um fator importante

df_fim |>
  ggplot(aes(idade, fsfi_score))+
  geom_point()+
  geom_smooth(method = 'glm')

summary(
glm(df_fim$fsfi_score~df_fim$idade)
)

## IMC - Não significativo

df_fim |>
  ggplot(aes(imc, fsfi_score))+
  geom_point()+
  geom_smooth(method = 'glm')

summary(
  glm(df_fim$fsfi_score~df_fim$imc)
)

## Estado civil - escores mais baixos entre as viúvas (esperado),
                # mas entre as outras categorias não.

df_fim |>
  ggplot(aes(civil, fsfi_score))+
  geom_boxplot()

summary(
  glm(df_fim$fsfi_score~df_fim$civil)
)

## Atividade física - mulheres que praticam atividade física têm escores menores
                      # com significência estatística

df_fim |>
  ggplot(aes(atividade_fisica, fsfi_score))+
  geom_boxplot()

summary(
  glm(df_fim$fsfi_score~df_fim$atividade_fisica)
)


## Escolaridade - sem interferência

df_fim |>
  ggplot(aes(escolaridade, fsfi_score))+
  geom_boxplot()

summary(
  glm(data = df_fim,
      fsfi_score~ escolaridade,
      )
)

## Antidepressivos e TH (poucas mulheres usando para uma análise adequada)


## Tempo de abstinência sexual foi estremamente significativo no escore
        # com uma magnitude de efeito muito grande
        # mulheres que tiveram relação sexual no último mês têm escores mais elevados.

df_fim |>
  ggplot(aes(tempo_abstinencia_sexual, fsfi_score))+
  geom_boxplot()

summary(
  glm(data = df_fim,
      fsfi_score~ tempo_abstinencia_sexual,
  )
)

# Análise descritiva das dimenões medidas pelo FSFI
fun_sexual <- function(x){
  df_fim |>
    ggplot(aes({{x}}))+
    geom_bar()
}

## Desejo

fun_sexual(fsfi_interesse)

## Excitação

fun_sexual(fsfi_excitacao)

## Lubrificação

fun_sexual(fsfi_lubrificacao)

## Orgasmo

fun_sexual(fsfi_orgasmo)

## Satisfação

fun_sexual(fsfi_satisfacao)

## Dor

fun_sexual(fsfi_dor)

## Identificar fatores presentes de risco no período do climatério associados  negativamente
# com a atividade sexual feminina, como a incontinência urinária,
# fogachos, ressecamento vaginal, entre outros.

## Sintomas do climatério

### Fogachos p_value = 0.0861

df_fim |>
  # filter(idade< 65) |>
  ggplot(aes(Fogachos, fsfi_score))+
  geom_boxplot()

df_fim_menos_65 <- df_fim |> # p =  0.247
  filter(idade<65)

summary(
  glm(data = df_fim_menos_65,
      fsfi_score ~ Fogachos)
)


### Ressecamento vaginal -  p_value =  0.755

df_fim_menos_65 |>
  ggplot(aes(`Ressecamento vaginal`, fsfi_score))+
  geom_boxplot()

summary(
  glm(data = df_fim_menos_65,
      fsfi_score ~ `Ressecamento vaginal`)
)


# Tabela 2 ----------------------------------------------------------------


df_fim |>
  select(idade, civil, atividade_fisica,
         tempo_abstinencia_sexual, Fogachos, `Ressecamento vaginal`,
         fsfi_score) |>
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



# Figura ------------------------------------------------------------------

# Standardized variable

df_fim$norm_fsfi_score <- as.numeric(scale(df_fim$fsfi_score))

df_fim$norm_idade <- as.numeric(scale(df_fim$idade))

cor_idade_fsfi <- glm(data=df_fim, fsfi_score~idade)

cor_idade_fsfi_norm <- glm(data=df_fim, norm_fsfi_score~norm_idade)


summary(cor_idade_fsfi_norm)


p_value <- summary(cor_idade_fsfi)$coefficients["idade", "Pr(>|t|)"]


p_value <-  ifelse(
  p_value < 0.001,
  '< 0.001',
  p_value
)


#
# df_fim |>
#   ggplot(aes(idade, fsfi_score))+
#   geom_point()+
#   geom_smooth(method = 'glm', se = T)+
#   geom_text(
#     aes(
#       label = paste0(
#         'p-value = ', p_value
#       )
#     ),
#     x = 45,
#     y = 4,
#     size = 3,
#     family = "Source Sans Pro"
#   )+
#   labs(
#     x = 'Idade (anos)',
#     y = 'Escore do FSFI'
#   )+
#   theme_classic(base_size = 8)+
#   guides(
#     size =  guide_none()
#   )

df_fim |>
  ggplot(aes(idade, fsfi_score))+
  geom_point()+
  geom_smooth(method = 'glm', se = T)+
  annotate("text",
           label = paste0(
             'p-value = ', p_value
           ),
           x = 45,
           y = 2.5,
           size = 3
  )+
  labs(
    x = 'Idade (anos)',
    y = 'Escore do FSFI'
  )+
  theme_classic(base_size = 8)+
  guides(
    size =  guide_none()
  )

ggsave('fig_1.png', dpi = 300)



str(cor_idade_fsfi)

names(
cor_idade_fsfi$effects
)



df_fim |>
  ggplot(aes(idade, fsfi_score))+
  geom_point()+
  geom_smooth(method = 'glm', se = F)




df_standard |>
  ggplot(aes(idade, norm_fsfi_score))+
  geom_point()+
  geom_smooth(method = 'glm', se = F)





