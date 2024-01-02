
# Correlação --------------------------------------------------------------

df <- df |>
  mutate(imc = round(peso/((altura/100)^2), digits = 1))

## Select numeric variables

df_corr <- df |>
  select(record_id:etnia, parto_normal, parto_cesaria,
         escala_incomodo, tempo_abstinencia_sexual,
         starts_with('fsfi'),
         starts_with('iciq'))


  corrgram::corrgram(
    df_corr,
    upper.panel = corrgram::panel.cor,
    lower.panel = corrgram::panel.shade,
    # diag.panel = corrgram::panel.density,
    # panel = corrgram::panel.ellipse ,
    gap = .3,
    cex.labels = 1
    )


# Plots -------------------------------------------------------------------

df |>
    ggplot(aes(idade, fill= redcap_data_access_group)) +
    geom_boxplot()


df |>
  filter(idade>=45) |>
    ggplot(aes(idade, fill= redcap_data_access_group)) +
    geom_boxplot()

df |>
  ggplot(aes(imc, fill= redcap_data_access_group))+
  geom_boxplot()


df |>
  ggplot(aes(civil, fill= redcap_data_access_group))+
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(atividade_fisica, fill= redcap_data_access_group))+
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(escolaridade, fill= redcap_data_access_group))+
  geom_bar(position = 'dodge')


df |>
  ggplot(aes(etnia, fill= redcap_data_access_group))+
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(antidepressivo, fill= redcap_data_access_group))+
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(menopausa, fill= redcap_data_access_group))+
  geom_bar(position = 'dodge')

df |>
  ggplot(aes(atrapalha_relacao, fill= redcap_data_access_group))+
  geom_bar(position = 'dodge')



# Resumo ------------------------------------------------------------------

df |>
  group_by(redcap_data_access_group) |>
  summarise(
    N = n(),
    mean_age = mean(idade, na.rm=T),
    min_age = min(idade, na.rm=T),
    max_age = max(idade, na.rm=T),
    incomodo_IU = mean(escala_incomodo, na.rm = T),
    incomodo_sd = sd(escala_incomodo, na.rm = T)
  )



