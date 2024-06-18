#bibliotecas
library(tidyverse)
library(readxl)
library(ggplot2)
##

#leitura dos dados
dados = read_excel("pp.xlsx") 
dados$bpm = as.factor(dados$bpm)
dados$genero = as.factor(dados$genero)
##

#boxplot dos dados separados
ggplot(dados, aes(x = genero, y = tempo, fill = genero)) +
  geom_boxplot() +
  labs(title = "Genero vs Tempo",
       x = "Gênero",
       y = "Tempo (minutos)") +
  scale_fill_brewer(palette = "Set2") + 
  theme_bw()+
  theme(legend.position = "none")

ggplot(dados, aes(x = bpm, y = tempo, fill = bpm)) +
  geom_boxplot() +
  labs(title = "BPM vs Tempo",
       x = "BPM",
       y = "Tempo (minutos)") +
  scale_fill_brewer(palette = "Set2") + 
  theme_bw()+
  theme(legend.position = "none")
##

#boxplot com todos os dados
ggplot(dados, aes(x = genero, y = tempo, fill = bpm)) +
  geom_boxplot() +
  labs(title = "Boxplot: Gênero vs Tempo",
       x = "Gênero",
       y = "Tempo (minutos)") +
  scale_fill_brewer(palette = "Set2") + 
  theme_bw()

ggplot(dados, aes(x = bpm, y = tempo, fill = genero)) +
  geom_boxplot() +
  labs(title = "Boxplot: BPM vs Tempo",
       x = "BPM",
       y = "Tempo (minutos)") +
  scale_fill_brewer(palette = "Set2") + 
  theme_bw()
##

#grids
ggplot(dados, aes(x = genero, y = tempo, fill = genero)) +
  geom_boxplot() +
  labs(title = "Tempo vs Genero por BPM",
       x = "Gênero",
       y = "Tempo (minutos)") +
  scale_fill_brewer(palette = "Set3") + 
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~ bpm, ncol = 2)

ggplot(dados, aes(x = bpm, y = tempo, fill = bpm)) +
  geom_boxplot() +
  labs(title = "Tempo vs BPM por Genero",
       x = "BPM",
       y = "Tempo (minutos)") +
  scale_fill_brewer(palette = "Set3") +  
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~ genero, ncol = 3)
##

#tempo médio
media_dados <- dados %>%
  group_by(bpm, genero) %>%
  summarise(tempo_medio = mean(tempo)) %>%
  ungroup()
##

#dotplot dos dados
ggplot(dados, aes(x = genero, y = tempo)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  geom_line(data = media_dados, aes(x = genero, y = tempo_medio, group = bpm, color = as.factor(bpm)), size = 1) + 
  geom_point(data = media_dados, aes(x = genero, y = tempo_medio, color = as.factor(bpm)), size = 3) +
  labs(title = "Dotplot do Tempo por Genero e BPM",
       x = "Gênero",
       y = "Tempo (minutos)",
       color = "BPM") +
  theme_bw() +
  facet_wrap(~ bpm, ncol = 2)

ggplot(dados, aes(x = bpm, y = tempo)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) + 
  geom_line(data = media_dados, aes(x = bpm, y = tempo_medio, group = 1, color = as.factor(bpm)), size = 1) + 
  geom_point(data = media_dados, aes(x = bpm, y = tempo_medio, color = as.factor(bpm)), size = 3) +
  labs(title = "Dotplot do Tempo por BPM e Genero",
       x = "BPM",
       y = "Tempo (minutos)",
       color = "BPM") +
  theme_bw() +
  facet_wrap(~ genero, ncol = 3)
##

#gráfico de interação
ggplot(media_dados, aes(x = bpm, y = tempo_medio, group = genero, color = genero)) +
  geom_line(size = 1) +  # Adicionando linhas para cada bpm
  geom_point(size = 3) +  # Adicionando pontos médios para cada gênero
  labs(title = "Gráfico de Interação: Genero vs Tempo",
       x = "BPM",
       y = "Tempo Médio (minutos)",
       color = "Genero") +
  theme_bw() +
  theme(legend.position = "right")

ggplot(media_dados, aes(x = genero, y = tempo_medio, group = bpm, color = bpm)) +
  geom_line(size = 1) +  
  geom_point(size = 3) +  
  labs(title = "Gráfico de Interação: BPM vs Tempo",
       x = "Genero",
       y = "Tempo Médio (minutos)",
       color = "BPM") +
  theme_bw() +
  theme(legend.position = "right")
##

#anova
fit = lm(tempo ~ bpm*genero, data=dados)
anova(fit)
##

#análise dos resíduos
#QQplot
residuos <- resid(fit)
qqplot(qnorm(ppoints(length(residuos))), residuos, col = "purple", main = "Q-Q Plot dos Resíduos", 
       xlab = "Quantis Teóricos", ylab = "Resíduos")
qqline(residuos)

#residuos vs valores ajustados
plot(fit$fitted.values, residuos, 
     xlab = "Valores ajustados", ylab = "Resíduos",
     col = "purple", pch = 8)
abline(h = 0, col = "black", lty = 2)  # linha pontilhada preta no zero

#residuos vs cada fator
#fator A
ggplot(data = dados, aes(x = bpm, y = residuos)) +
  geom_point(color = "purple", shape = 7) +
  labs(x = "BPM", y = "Resíduos") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw()

#fator B
ggplot(data = dados, aes(x = genero, y = residuos)) +
  geom_point(color = "purple", shape = 7) +
  labs(x = "Genero", y = "Resíduos") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw()

#mudança dos nomes
dados$genero <- factor(dados$genero, levels = c("pop internacional", "mpb", "classica"),
                       labels = c(1, 2, 3))
dados$bpm <- factor(dados$bpm, levels = c("alto", "baixo"),
                    labels = c(4, 5))

#tukey
tukey = aov(fit)
summary(tukey)
plot(TukeyHSD(tukey), las=1, col=c(2, 2, 1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 2, 2, 2))


