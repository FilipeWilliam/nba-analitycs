install.packages("tidyverse")
install.packages("skimr")

library(tidyverse)
library(skimr)

Player_Career_Info <- read_csv("Player Career Info.csv")
Player_Directory <- read_csv("Player Directory.csv")
Player_Totals <- read_csv("Player Totals.csv")

glimpse(Player_Totals)
skim(Player_Totals)

glimpse(Player_Directory)
skim(Player_Directory)

glimpse(Player_Career_Info)
skim(Player_Career_Info)

# Observações comuns e pré-tratamentos necessários:
# - Colunas com nomes estranhos/caracteres especiais: Renomear.
# - Colunas numéricas lidas como texto: Converter para numérico.
# - Valores NA: Decidir o tratamento (remover linhas, preencher com 0, média, etc.).
# - Colunas redundantes ou desnecessárias: Remover.

# 3. Limpeza e Padronização de Nomes de Colunas e Dados

# Vamos padronizar nomes de colunas que serão usadas para juntar tabelas.
# Ex: 'player' para 'player_name', 'player_id' para 'player_id' (já estão ok)

# Player_Totals_Cleaned:
# Parece que algumas colunas numéricas podem ter sido lidas como texto se tiverem 'NA' ou outros caracteres.
# Vamos forçar a conversão de tipos para algumas colunas chave onde NA é comum (ex: porcentagens)
Player_Totals_Cleaned <- Player_Totals %>%
  filter(lg == "NBA") %>%
  mutate(
    # Certificar que estatísticas são numéricas, NA para não-numéricos
    across(c(`fg_percent`:`ft_percent`, `x3p_percent`, `e_fg_percent`, `x2p_percent`), as.numeric),
    # Preencher NA em porcentagens com 0 se faz sentido para cálculos, ou manter NA
    # Aqui, para fins de cálculo, podemos assumir 0% para quem não tentou, ou manter NA e filtrar depois
    # Por agora, vamos manter NA para não introduzir viés em médias se não houver attempts
    # NA nos totais (como assistências, pontos) devem ser investigados se são 0 ou dados ausentes
    across(c(`g`:`pts`), ~replace_na(as.numeric(.), 0)) # Assumindo que NA em totais significa 0
  ) %>%
  # Renomear '# player' para 'player_id' para consistência (se necessário, mas parece ser 'player_id')
  rename(player_id = `player_id`) # Já parece ser o nome correto '# player_id'
# Se 'player_id' for 'NA', podemos considerar essas linhas inválidas ou tentar preencher.
# Por enquanto, vamos assumir que player_id é sempre preenchido.

# Player_Directory_Cleaned:
# 'ht_in_in' parece altura em polegadas. 'wt' em libras.
Player_Directory_Cleaned <- Player_Directory %>%
  # Converter altura de polegadas para cm e peso de libras para kg (opcional, mas bom para brasileiros)
  mutate(
    height_cm = `ht_in_in` * 2.54, # 1 polegada = 2.54 cm
    weight_kg = `wt` * 0.453592 # 1 libra = 0.453592 kg
  ) %>%
  # Tratar 'NA' em 'birth_date'. Para análises de idade, podemos ignorar NA ou estimar.
  # Para este exercício, vamos manter NA e filtrar se a análise exigir a data.
  # Converter 'birth_date' para formato de data
  mutate(birth_date = ymd(birth_date))

# Player_Career_Info_Cleaned:
# 'hof' (Hall of Fame) é TRUE/FALSE.
Player_Career_Info_Cleaned <- Player_Career_Info %>%
  rename(player_id = `player_id`) %>% # Se o nome da coluna for diferente
  # As colunas `num_seas`, `first_seas`, `last_seas` devem ser numéricas.
  mutate(
    num_seas = as.numeric(num_seasons),
    first_seas = as.numeric(first_seas),
    last_seas = as.numeric(last_seas)
  )

# Verificar após a limpeza
glimpse(Player_Totals_Cleaned)
glimpse(Player_Directory_Cleaned)
glimpse(Player_Career_Info_Cleaned)

# Análise 1
# Juntar Player Totals com Player Career Info
Merged_Career <- Player_Totals_Cleaned %>%
  inner_join(Player_Career_Info_Cleaned, by = c("player_id", "player")) %>%
  # Adicionar informações de diretório para altura/peso/data de nascimento
  left_join(Player_Directory_Cleaned %>% select(player_name = player, birth_date,height_cm, weight_kg),
            by = c("player" = "player_name")) %>%
  distinct(player_id, player, season, .keep_all = TRUE) # Remover duplicatas se houver

# 1.1 Média das estatísticas das primeiras 3 temporadas (assumindo que "num_seas" é a duração total)
First_Seasons_stats <- Merged_Career %>%
  group_by(player_id, player) %>%
  filter(season <= min(season, na.rm = TRUE) + 2) %>% # Primeiras 3 temporadas
  summarise(
    avg_pts_first_3 = mean(`pts`, na.rm = TRUE),
    avg_ast_first_3 = mean(`ast`, na.rm = TRUE),
    avg_trb_first_3 = mean(`trb`, na.rm = TRUE),
    num_seasons_career = first(num_seas),
    is_hof = first(hof),
    .groups = 'drop'
  )

# Análise: Correlação com Duração da Carreira
Cor_pts_career <- cor(First_Seasons_stats$avg_pts_first_3, First_Seasons_stats$num_seasons_career, use = "pairwise.complete.obs")
Cor_ast_career <- cor(First_Seasons_stats$avg_ast_first_3, First_Seasons_stats$num_seasons_career, use = "pairwise.complete.obs")
Cor_trb_career <- cor(First_Seasons_stats$avg_trb_first_3, First_Seasons_stats$num_seasons_career, use = "pairwise.complete.obs")

cat("\n--- Desempenho e Longevidade da Carreira ---\n")
cat(sprintf("Correlação entre Média de Pontos nas 3 primeiras temporadas e Duração da Carreira: %.2f\n", Cor_pts_career))
cat(sprintf("Correlação entre Média de Assistências nas 3 primeiras temporadas e Duração da Carreira: %.2f\n", Cor_ast_career))
cat(sprintf("Correlação entre Média de Rebotes nas 3 primeiras temporadas e Duração da Carreira: %.2f\n", Cor_trb_career))

# Visualização: Pontos nas primeiras 3 temporadas vs. Duração da Carreira
ggplot(First_Seasons_stats, aes(x = avg_pts_first_3, y = num_seasons_career)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Pontos nas Primeiras 3 Temporadas vs. Duração da Carreira",
    x = "Média de Pontos nas Primeiras 3 Temporadas",
    y = "Duração da Carreira (Número de Temporadas)"
  ) +
  theme_minimal()

# Análise: Entrada no Hall da Fama (HOF)
Hof_stats <- First_Seasons_stats %>%
  filter(!is.na(is_hof)) %>%
  group_by(is_hof) %>%
  summarise(
    median_pts_first_3 = median(avg_pts_first_3, na.rm = TRUE),
    median_ast_first_3 = median(avg_ast_first_3, na.rm = TRUE),
    median_trb_first_3 = median(avg_trb_first_3, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\nEstatísticas Medianas nas 3 primeiras temporadas para Jogadores do Hall da Fama vs. Não-Hall da Fama:\n")
print(Hof_stats)

# Visualização: Boxplot de Pontos nas primeiras 3 temporadas por status HOF
ggplot(First_Seasons_stats %>% filter(!is.na(is_hof)), aes(x = factor(is_hof, levels = c(FALSE, TRUE), labels = c("Não HOF", "HOF")), y = avg_pts_first_3)) +
  geom_boxplot() +
  labs(
    title = "Média de Pontos nas Primeiras 3 Temporadas por Status do Hall da Fama",
    x = "Status do Hall da Fama",
    y = "Média de Pontos nas Primeiras 3 Temporadas"
  ) +
  theme_minimal()



# Análise 2
Season_Trends <- Player_Totals_Cleaned %>%
  group_by(season) %>%
  summarise(
    avg_pts_per_game = mean(`pts` / `g`, na.rm = TRUE), # Pontos por jogo
    avg_ast_per_game = mean(`ast` / `g`, na.rm = TRUE), # Assistências por jogo
    avg_trb_per_game = mean(`trb` / `g`, na.rm = TRUE), # Rebotes por jogo
    avg_3p_att_per_game = mean(`x3pa` / `g`, na.rm = TRUE), # Tentativas de 3 pontos por jogo
    avg_blk_per_game = mean(`blk` / `g`, na.rm = TRUE), # Média de Bloqueios por jogo (por jogador)
    avg_stl_per_game = mean(`stl` / `g`, na.rm = TRUE), # Média de Roubos por jogo (por jogador)
    avg_fg_perc_league = mean(`fg_percent`, na.rm = TRUE), # Média da % de Arremessos de Quadra da Liga (por jogador)
    # ----------------------------------------------------

    avg_fg_perc = mean(`fg_percent`, na.rm = TRUE),
    avg_3p_perc = mean(`x3p_percent`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(season) %>%
  filter(season >= 1979) # Filtrar para temporadas mais recentes para melhor visualização da NBA moderna

cat("\n--- Evolução das Estatísticas ao Longo do Tempo ---\n")
cat("\nAlgumas estatísticas médias por temporada:\n")
print(head(Season_Trends))
print(tail(Season_Trends))

# Visualização: Pontos por Jogo ao Longo das Temporadas
ggplot(Season_Trends, aes(x = season, y = avg_pts_per_game)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Média de Pontos por Jogo na NBA ao Longo das Temporadas",
    x = "Temporada",
    y = "Média de Pontos por Jogo"
  ) +
  theme_minimal()

# Visualização: Tentativas de 3 Pontos por Jogo ao Longo das Temporadas
ggplot(Season_Trends, aes(x = season, y = avg_3p_att_per_game)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "purple", size = 2) +
  labs(
    title = "Média de Tentativas de 3 Pontos por Jogo na NBA ao Longo das Temporadas",
    x = "Temporada",
    y = "Média de Tentativas de 3 Pontos por Jogo"
  ) +
  theme_minimal()

# Visualização: Média de Bloqueios por Jogo na NBA ao Longo das Temporadas
ggplot(Season_Trends, aes(x = season, y = avg_blk_per_game)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Média de Bloqueios por Jogo (por jogador) na NBA ao Longo das Temporadas",
    x = "Temporada",
    y = "Média de Bloqueios por Jogo"
  ) +
  theme_minimal()

#Visualização: Média de Roubos por Jogo na NBA ao Longo das Temporadas
ggplot(Season_Trends, aes(x = season, y = avg_stl_per_game)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Média de Roubos por Jogo (por jogador) na NBA ao Longo das Temporadas",
    x = "Temporada",
    y = "Média de Roubos por Jogo"
  ) +
  theme_minimal()

# Visualização: Média da Porcentagem de Arremessos de Quadra na NBA ao Longo das Temporadas
ggplot(Season_Trends, aes(x = season, y = avg_fg_perc_league)) +
  geom_line(color = "orange", size = 1) +
  geom_point(color = "orange", size = 2) +
  labs(
    title = "Média da Porcentagem de Arremessos de Quadra (por jogador) na NBA ao Longo das Temporadas",
    x = "Temporada",
    y = "Média da Porcentagem de Arremessos de Quadra"
  ) +
  theme_minimal()


# Análise 3
Physical_Trends <- Player_Totals_Cleaned %>%
  select(player, season) %>% # Seleciona apenas player_id e season de totals
  distinct() %>% # Garante uma linha por jogador por temporada
  left_join(Player_Directory_Cleaned %>%
              select(player, pos, height_cm, weight_kg),
            by = "player")

# 2. Remover NAs de altura/peso/posição
Physical_Trends <- Physical_Trends %>%
  drop_na(height_cm, weight_kg)

# 3. Unificar as posições "F-C" e "C-F" em uma única categoria "F-C"
# E unificar "G-F" e "F-G" em uma única categoria "G-F"
Physical_Trends <- Physical_Trends %>%
  mutate(
    pos_unified = case_when(
      pos == "C-F" ~ "F-C", # Altera C-F para F-C
      pos == "F-G" ~ "G-F", # Altera F-G para G-F
      TRUE ~ pos            # Mantém todas as outras posições como estão
    )
  )

# Verificar as novas categorias de posição unificadas
glimpse(Physical_Trends)
table(Physical_Trends$pos)

# 4. Calcular as Médias de Altura e Peso por Posição Unificada e Temporada
Pos_Physical_Averages <- Physical_Trends %>%
  group_by(season, pos_unified) %>% # AGORA agrupamos por 'pos_unified'
  summarise(
    avg_height_cm = mean(height_cm, na.rm = TRUE),
    avg_weight_kg = mean(weight_kg, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(season, pos_unified)


# Verificar o resultado
glimpse(Pos_Physical_Averages)

# Visualização: Média de Altura por Posição ao Longo das Temporadas
ggplot(Pos_Physical_Averages, aes(x = season, y = avg_height_cm, color = pos_unified)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolução da Média de Altura (cm) por Categoria de Posição ao Longo das Temporadas",
    x = "Temporada",
    y = "Média de Altura (cm)",
    color = "Categoria de Posição"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") # Usar uma paleta de cores para melhor distinção

# Visualização: Média de Peso por Posição Unificada ao Longo das Temporadas
ggplot(Pos_Physical_Averages, aes(x = season, y = avg_weight_kg, color = pos_unified)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolução da Média de Peso (kg) por Categoria de Posição ao Longo das Temporadas",
    x = "Temporada",
    y = "Média de Peso (kg)",
    color = "Categoria de Posição"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")
