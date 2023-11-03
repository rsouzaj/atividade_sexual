library(tidyverse)


df_uerj <- read.csv('df_uerj.csv') |>
              mutate(
                imc = round(peso/((altura/100)^2), digits = 1)
              )



names(df_uerj)


df_uerj |>
  ggplot(aes(idade)) +
  geom_boxplot()

df_uerj |>
  ggplot(aes(imc))+
  geom_boxplot()

df_uerj |>
  ggplot(aes(civil))+
  geom_bar()

df_uerj |>
  ggplot(aes(etnia))+
  geom_bar()

df_uerj |>
  ggplot(aes(escolaridade))+
  geom_bar()

df_uerj |>
  ggplot(aes(menopausa))+
  geom_bar()

df_uerj |>
  ggplot(aes(sintomas_climaterio___1))+
  geom_bar()

df_uerj |>
  ggplot(aes(sintomas_climaterio___2))+
  geom_bar()

df_uerj |>
  ggplot(aes(sintomas_climaterio___3))+
  geom_bar()

df_uerj |>
  ggplot(aes(sintomas_climaterio___4))+
  geom_bar()

df_uerj |>
  mutate(sintomas_climaterio___4= as.factor(sintomas_climaterio___4)) |>
  ggplot(aes(menopausa, fill = sintomas_climaterio___4))+
  geom_bar()

df_uerj |>
  ggplot(aes(atrapalha_relacao))+
  geom_bar()

df_uerj |>
  ggplot(aes(escala_incomodo))+
  geom_bar()

