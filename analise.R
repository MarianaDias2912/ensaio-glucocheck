library(tidyverse)
library(here)
library(janitor)
library(gtsummary)

# Carregar dados
dados <- read_csv(
  here("data", "raw", "glucocheck.csv")
) |>
  clean_names() |>
  mutate(
    grupo = factor(grupo, levels = c("controlo", "intervencao"),
                   labels = c("Controlo", "Intervenção")),
    sexo  = factor(sexo, levels = c("F", "M"),
                   labels = c("Feminino", "Masculino"))
  )

# Tabela 1
tabela1 <- dados |>
  select(grupo, idade, sexo, hba1c_baseline,
         glicemia_jejum_baseline, peso_baseline, des_sf) |>
  tbl_summary(
    by = grupo,
    label = list(
      idade                   ~ "Idade (anos)",
      sexo                    ~ "Sexo",
      hba1c_baseline          ~ "HbA1c basal (%)",
      glicemia_jejum_baseline ~ "Glicemia em jejum basal (mg/dL)",
      peso_baseline           ~ "Peso basal (kg)",
      des_sf                  ~ "DES-SF basal"
    ),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) |>
  add_overall()

# Gráfico outcome primário
grafico_hba1c <- ggplot(
  dados |> filter(!is.na(hba1c_6meses)),
  aes(x = grupo, y = hba1c_6meses, colour = grupo)
) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  geom_jitter(width = 0.12, alpha = 0.7, size = 2.5) +
  scale_colour_manual(
    values = c(
      "Controlo"    = "#7A9B9E",
      "Intervenção" = "#1B4965"
    )
  ) +
  labs(
    x = "Grupo",
    y = "HbA1c aos 6 meses (%)",
    title = "GlucoCheck: outcome primário"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")