library(tidyverse)
library(readxl)
library(geobr)
library(sf)
library(ggspatial)
library(ggbump)
library(ggrepel)

setwd("C:/Users/Lapei_Cigets/Desktop/RFB2024/01_dfs_tratados")

# distribuicao_frequencia

dist_frequencia <- read_excel("C:/Users/Lapei_Cigets/Desktop/RFB2024/01_dfs_tratados/proporcoes_tempo_estado24.xlsx")


a <- 
  dist_frequencia |> 
  ggplot(aes(x = ano, 
             y = freq, 
             col = genero)) + 
  geom_line(size = 1) + 
  geom_label(aes(label = round(freq, 2)),
             size = 1.5, 
             label.size = 0.10, 
             alpha = 0.7,
             vjust = -0.5) +
  labs(
    title = "Evolução de empreendedores com negócios ativos em Goiás",
    subtitle = "Fonte: Receita Federal do Brasil",
    x = "Ano",
    y = "Frequência",
    col = "Gênero"
  ) +
  scale_y_continuous(limits = c(0, 0.85)) +
  scale_color_manual(values = c("Male" = "#1f78b4", 
                                "Female" = "#e31a1c"),
                     labels = c("Male" = "Masculino", 
                                "Female" = "Feminino")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.major = element_line(color = "gray80")
  )

a

ggsave(plot = a, 
       filename = "evolucao.jpeg", 
       width = 10,
       height = 4,
       dpi = 1000)



# criando mapa ------------------------------------------------------------

taxas <- read_excel("01_dfs_tratados/cap2_taxas24.xlsx")

goias_shape <- read_municipality(code_muni = "GO", year = 2020)

goias_mapa <- goias_shape %>%
  left_join(taxas, by = c("code_muni" = "cod_IBGE"))


b <- ggplot(data = goias_mapa) +
  geom_sf(aes(fill = taxa), color = "white", size = 0.2) +  # Cor varia pela coluna "taxa"
  scale_fill_gradient(low = "#FFB6C1", high = "#8B0000", name = "Taxa") +
  labs(title = "Empreendedorismo por mulheres nos municípios",
       subtitle = "Taxa por 100 habitantes do sexo feminino",
       caption = "RFB (2024)") +
  theme_minimal() +
  annotation_scale(location = "bl", width_hint = 0.3) +  # Escala no canto inferior esquerdo
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +  # Rosa dos Ventos no canto superior direito
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

ggsave(plot = b, 
       filename = "mapa.jpeg", 
       width = 8,
       height = 8,
       dpi = 1000)


# Evolucao top 10 municipios

taxas_evolucao <- read_excel("C:/Users/Lapei_Cigets/Desktop/RFB2024/01_dfs_tratados/taxas_evolucao.xlsx", 
                             sheet = "Planilha1")


taxa_evolucao <- taxas_evolucao |> 
  select(municipio_pad, taxa24, taxa23, taxa22) |> 
  gather(key = "ano", value = "taxa", 2:4) |> 
  mutate(ano = case_when(ano == "taxa24" ~ 2024,
                         ano == "taxa23" ~ 2023,
                         ano == "taxa22" ~ 2022)) 


writexl::write_xlsx(taxa_evolucao, "evolucao_top10.xlsx")


library(ggplot2)
library(ggrepel)

c <- taxa_evolucao |>
  mutate(taxa = round(taxa, 2)) |> 
  ggplot(aes(x = ano, y = taxa, col = municipio_pad)) + 
  geom_bump(size = 1) +
  geom_label(aes(label = taxa), 
             size = 5) +
  geom_text(data = subset(taxa_evolucao, ano == 2024),
            aes(label = municipio_pad),
            hjust = 0, nudge_x = 0.2,  # Aumenta o deslocamento horizontal
            size = 4, 
            show.legend = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  
  # Expandir o eixo X para dar espaço aos nomes
  scale_x_continuous(breaks = c(2022, 2023, 2024), 
                     expand = expansion(mult = c(0.1, 0.5))) + # Mais espaço à direita
  
  scale_y_continuous(breaks = seq(floor(min(taxa_evolucao$taxa)), 
                                  ceiling(max(taxa_evolucao$taxa)), 
                                  by = 1)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10) # Pequena margem adicional
  ) +
  ylab("Taxa") +
  ggtitle("Taxa de empreendedorismo (empreendedoras/100 mulheres)")

c

ggsave(plot = c, 
       filename = "grafico_municipios.jpeg", 
       width = 10,
       height = 8,
       dpi = 1000)



taxa_evolucao |>
  ggplot(aes(x = ano, y = taxa, col = municipio_pad)) + 
  geom_bump(size = 1) +
  geom_point(size = 10) +
  
  # Adicionar o valor da taxa dentro dos pontos
  geom_text(aes(label = round(taxa, 1)), 
            color = "white", size = 4, vjust = 0.5) +  # Texto branco e centralizado
  
  # Adicionar nomes dos municípios apenas no ano de 2024
  # Ajustes de escala e estilo
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  scale_y_continuous(breaks = seq(floor(min(taxa_evolucao$taxa)), 
                                  ceiling(max(taxa_evolucao$taxa)), 
                                  by = 1)) +
  
  # Temas e títulos
  theme_minimal() +
  ylab("Taxa") +
  ggtitle("Taxa de empreendedorismo (empreendedoras/100 mulheres)")




