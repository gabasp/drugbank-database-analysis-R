# Fontes dos dados:
# https://go.drugbank.com/
# https://github.com/gabasp/drugbank-database-analysis-R


# Instalando pacotes
install.package("dplyr"); library(dplyr) # manipulação de dataframe
install.package("ggplot2"); library(ggplot2) # pacote gráfico
install.package("tidyvese"); library(tidyverse) # pacote gráfico


# Gerando dataframe do csv
df_drugbank <- read.csv("drugbank_data_final.csv"); View(df_drugbank)

# Sumário do dataframe
summary(df_drugbank)
# Com base no sumário, sabemos que a base de dados possui 11290 linhas, ou seja, 11290 compostos


# Legenda de cada coluna:


# SMILES (Simplified Molecular Input Line Entry System) é uma notação química voltada a computação

# ExactMolWt é a massa molar exata dos compostos
  # a media de massa molar dos compostos é de 387.624 g/mol (gramas por mol)
    # a massa molar exata do composto mais leve é de 2.016 g/mol
    select(df_drugbank, Generic.Name, ExactMolWt) %>% arrange(ExactMolWt) %>% head(10)
      # comandos select e head para mostrar os 10 compostos mais leves
    # a massa molar exata do composto mais pedaso é de 8268.351 g/mol
    select(df_drugbank, Generic.Name, ExactMolWt) %>% arrange(desc(ExactMolWt)) %>% head(10)
      # comandos select e head para mostrar os 10 compostos mais pesados

# MolLogP é o coeficiente de partição
  # a média do coeficiente de partição é 1.8278
    # o menor coeficiente de partição é de -83.7536
    select(df_drugbank, Generic.Name, MolLogP) %>% arrange(MolLogP) %>% head(5)
      # comandos select e head para mostrar os 5 menores coeficientes de partição
    # o maior coeficiente de partição é de 57.75100
    select(df_drugbank, Generic.Name, MolLogP) %>% arrange(desc(MolLogP)) %>% head(5)
      # comandos select e head para mostrar os 5 maiores coeficientes de partição

# NumHAcceptors é a quantidade de átomos de hidrogênio que o composto pode receber
  # a média dessas quantias é de 5.709
    # a menor quantidade é 0
    select(df_drugbank, Generic.Name, NumHAcceptors) %>% arrange(NumHAcceptors) %>% head(1)
      # comandos select e head para mostrar a menor quantidade (muitos não aceitam nenhum)
    # a maior dessa quantidade é 224
    select(df_drugbank, Generic.Name, NumHAcceptors) %>% arrange(desc(NumHAcceptors)) %>% head(5)
      # comandos select e head para mostrar as 5 maiores quantidades
    
# NumHDonors é a quantidade de átomos de hidrogênio que o composto pode doar
  # a média dessas quantias é de 2.722
    # a menor quantidade é 0
    select(df_drugbank, Generic.Name, NumHDonors) %>% arrange(NumHDonors) %>% head(1)
      # comandos select e head para mostrar a menor quantidade (muitos não aceitam nenhum)
    # a maior quanidade é 116
    select(df_drugbank, Generic.Name, NumHDonors) %>% arrange(desc(NumHDonors)) %>% head(5)
      # comandos select e head para mostrar as 5 maiores quantidades

# NumRotatableBonds é a quantidade de anéis rotativos do composto
  # a média dessas quantias é 6.236
    # a menor quantidade é 0
    select(df_drugbank, Generic.Name, NumRotatableBonds) %>% arrange(NumRotatableBonds) %>% head(1)
      # comandos select e head para mostrar a menor quantidade (muitos não têm nenhum)
    # maior quantidade é 176
    select(df_drugbank, Generic.Name, NumRotatableBonds) %>% arrange(desc(NumRotatableBonds)) %>% head(5)
      # comandos select e head para mostrar as 5 maiores quantidades

# RingCount é a quantidade de anéis do composto
  # a média dessas quantias é 2.687
    # a menor quantidade é 0
    select(df_drugbank, Generic.Name, RingCount) %>% arrange(RingCount) %>% head(1)
      # comandos select e head para mostrar a menor quantidade (muitos não têm nenhum)
    # a maior quantidade é 69
    select(df_drugbank, Generic.Name, RingCount) %>% arrange(desc(RingCount)) %>% head(5)
      # comandos select e head para mostrar as 5 maiores quantidades
    
# TPSA (Total Polar Surface Area) é a soma da superfície dos átomos polares do composto
  # a média das áreas é 107.26 Å² (angstroms ao quadrado)
    # a área polar do menor composto é 0 Å²
    select(df_drugbank, Generic.Name, TPSA) %>% arrange(TPSA) %>% head(1)
      # comandos select e head para mostrar a menor área polar (muitos não têm átomos polares)
    # a área polar do maior composto é 3710.38 Å²
    select(df_drugbank, Generic.Name, TPSA) %>% arrange(desc(TPSA)) %>% head(5)
      # comandos select e head para mostrar as 5 maiores áreas polares
    
# Database.ID é o ID do composto na base DrugBank

# Molecular_formula é a fórmula molecular do composto
    
# Drug.Groups é o status atual do composto em relação ao seu uso legal/medicinal:
  # approved
    # oficialmente aprovados para serem comercializados (pode variar com a legislação do país)
  # illicit
    # drogas ilícitas (pode variar com a legislação do país)
  # experimental
    # droga experimental em fase de descobrimento ou pré descobrimento
  # investigational
    # droga em fase de desenvolvimento e triagem científica
  # withdrawn
    # costumava ser comercializada mas foi descontinuada ou proibida (pode variar com a legislação do país)
  # neutraceutical
    # composto regulado a nível farmaceutico e possui efeito nutricional
  # vet_approved
    # composto aprovado para uso veterinário
    
# Generic.Name é o nome cientifico ou genérico do composto
    
# Products é a lista de produtos fármacos que utilizam o composto
    

# Gráficos
# Usaremos o composto diazepam (index 712 no dataframe, DB00829) como referência para os gráficos
    
    
# ExactMolWt
# Gráfico de dispersão em ordem crescente
df_drugbank %>% # dataframe base
  arrange(ExactMolWt) %>% # função arrange ordena o dataframe pela coluna ExactMolWt
  mutate(Database.ID = factor(Database.ID, levels = Database.ID)) %>% # usando outra coluna como eixo X
  ggplot(aes(x = Database.ID, y = ExactMolWt)) + # define os eixos X e Y
  geom_hline(yintercept = mean(df_drugbank$ExactMolWt),
             color = "#0000ff", linewidth = 1.5) + # definindo a linha da média, em azul
  geom_point() + # gráfico de disperção
  geom_point(aes(x = Database.ID, y = ExactMolWt,
                 color = "#red", size = 3), df_drugbank[712,]) + # destacando o diazepam
  labs(title = "Massa molar",
       subtitle = "Média em azul\nPonto em destaque se refere ao diazepam",
       x = "", y = "Massa molar (em g/mol)") # nomeando os eixos, título e subtítulo do gráfico
    
# Gráfico de dispersão com ExactMolWt na escala logaritmica para melhor visualização de valores pequenos
df_drugbank %>% 
  arrange(ExactMolWt) %>% 
  mutate(Database.ID = factor(Database.ID, levels = Database.ID)) %>% 
  ggplot(aes(x = Database.ID, y = log(ExactMolWt))) + # escala logaritmica 
  geom_point() +
  geom_point(aes(x = Database.ID, y = log(ExactMolWt),
                 color = "#red", size = 3), df_drugbank[712,]) +
  labs(title = "Massa molar extrapolada",
       subtitle = "Ponto em destaque se refere ao diazepam",
       x = "", y = "Massa molar nna escala log10")
    
# Gráfico de densidade 
df_drugbank %>%
  ggplot(aes(x = ExactMolWt)) + # definindo o eixo X
  geom_density(fill = "#a6c6ff", color = "#eaeaea") + # gráfico de densidade
  geom_vline(xintercept = df_drugbank[712, "ExactMolWt"], size = .5, color = "red") + 
  labs(title ="Densidade da massa molar",
       subtitle = "Reta em destaque se refere ao diazepam",
       x = "Massa molar (em g/mol)", y = "Densidade")

# Gráfico de densidade com foco na maior concentração
df_drugbank %>% 
  filter(ExactMolWt <= 2000) %>% # função filter filtra os dados com base na coluna ExactMolWt
  ggplot(aes(x = ExactMolWt)) + 
  geom_density(fill = "#a6c6ff", color = "#eaeaea") + 
  geom_vline(xintercept = df_drugbank[712, "ExactMolWt"], size = 1.5, color = "red") + 
  labs(title ="Foco da densidade da massa molar",
       subtitle = "Reta em destaque se refere ao diazepam", 
       x = "Massa molar (em g/mol)", y = "Densidade")

# MolLogP
# Gráfico de dispersão em ordem crescente
df_drugbank %>%
  arrange(MolLogP) %>%
  mutate(Database.ID = factor(Database.ID , levels = Database.ID)) %>%
  ggplot(aes(x = Database.ID, y = MolLogP)) +
  geom_hline(yintercept = mean(df_drugbank$MolLogP),
             color = "#0000ff", linewidth = 1.5) + # definindo a linha da média, em azul
  geom_hline(yintercept = 0,
             color = "#ff0000", linewidth = 1.5) + # definindo o eixo das abcissas, em vermelho
  geom_point() + 
  geom_point(aes(x = Database.ID, y = MolLogP,
                 color = "#red", size = 3), df_drugbank[712,]) +
  labs(title = "Coeficiennte de partição",
       subtitle = "Média em azul\nEixo das abcissas em vermelho\nPonto em destaque se refere ao diazepam", 
       x = "", y = "Coeficiente de partição")

# Gráfico de densidade 
df_drugbank %>%
  ggplot(aes(x = MolLogP)) + 
  geom_density(fill = "#a6c6ff", color = "#eaeaea") + 
  geom_vline(xintercept = df_drugbank[712, "MolLogP"], size = .5, color = "red") +
  labs(title = "Densidade do coeficiente de partição",
       subtitle = "Reta em destaque se refere ao diazepam",
       x = "Coeficiente de partição", y = "Densidade")

# Gráfico de densidade com foco na maior concentração
df_drugbank %>%
  filter(MolLogP >= -15) %>%
  filter(MolLogP <= 15) %>% # filtrando um intervalo específico (com "maior que" e "menor que")
  ggplot(aes(x = MolLogP)) + 
  geom_density(fill = "#a6c6ff", color = "#eaeaea") + 
  geom_vline(xintercept = df_drugbank[712, "MolLogP"], size = 1.5, color = "red") +
  labs(title = "Foco da densidade do coeficiente de partição",
       subtitle = "Reta em destaque se refere ao diazepam",
       x = "Coeficiente de partição",
       y = "Densidade")

# NumHAcceptors
# Gráfico de dispersão em ordem crescente
df_drugbank %>%
  arrange(NumHAcceptors) %>%
  mutate(Database.ID = factor(Database.ID , levels = Database.ID)) %>%
  ggplot(aes(x = Database.ID, y = NumHAcceptors)) +
  geom_hline(yintercept = mean(df_drugbank$NumHAcceptors),
             color = "#0000ff", linewidth = 1.5) +
  geom_point() + 
  geom_point(aes(x = Database.ID, y = NumHAcceptors,
                 color = "#red", size = 3), df_drugbank[712,]) +
  labs(title = "Receptores de hidrogênio",
       subtitle = "Média em azul\nPonto em destaque se refere ao diazepam", 
       x = "", y = "Número de hidrogênios aceitáveis")

# Histograma
df_drugbank %>%
  ggplot(aes(x = NumHAcceptors)) + 
  geom_histogram(binwidth = 1, fill = "#a6c6ff", color = "#eaeaea") + # histograma
  geom_vline(xintercept = df_drugbank[712, "NumHAcceptors"], size = .5, color = "red") +
  labs(title = "Frequência de receptores de hidrogênio por quantidade",
       subtitle = "Reta em destaque se refere ao diazepam",
       x = "Número de hidrogênios aceitáveis", y = "Frequência")

# Histograma com foco na maior concentração
df_drugbank %>%
  filter(NumHAcceptors <= 30) %>%
  ggplot(aes(x = NumHAcceptors)) + 
  geom_histogram(binwidth = 1, fill = "#a6c6ff", color = "#eaeaea") + 
  geom_vline(xintercept = df_drugbank[712, "NumHAcceptors"], size = 1.5, color = "red") +
  labs(title = "Foco da frequência de receptores de hidrogênio por quantidade",
       subtitle = "Reta em destaque se refere ao diazepam",
       x = "Número de hidrogênios aceitáveis", y = "Frequência")

# NumHDonors
# Gráfico de dispersão em ordem crescente
df_drugbank %>%
  arrange(NumHDonors) %>%
  mutate(Database.ID = factor(Database.ID , levels = Database.ID)) %>%
  ggplot(aes(x = Database.ID, y = NumHDonors)) +
  geom_hline(yintercept = mean(df_drugbank$NumHDonors),
             color = "#0000ff", linewidth = 1.5) +
  geom_point() + 
  geom_point(aes(x = Database.ID, y = NumHDonors,
                 color = "red", size = 3), df_drugbank[712,]) +
  labs(title = "Doadores de hidrogênio",
       subtitle = "Média em azul\nPonto em destaque se refere ao diazepam", 
       x = "", y = "Número de hidrogênios doáveis")

# Histograma
df_drugbank %>%
  ggplot(aes(x = NumHDonors)) + 
  geom_histogram(binwidth = 1, fill = "#a6c6ff", color = "#eaeaea") + 
  geom_vline(xintercept = df_drugbank[712, "NumHDonors"], size = .5, color = "red") +
  labs(title = "Doadores de hidrogênio",
       subtitle = "Reta em destaque se refere ao diazepam",
       x = "Número de hidrogênios doáveis", y = "Frequência")

# Histograma com foco na maior concentração
df_drugbank %>%
  filter(NumHDonors <= 20) %>%
  ggplot(aes(x = NumHDonors)) + 
  geom_histogram(binwidth = 1, fill = "#a6c6ff", color = "#eaeaea") + 
  geom_vline(xintercept = df_drugbank[712, "NumHDonors"], size = 1.5, color = "red") +
  labs(title = "Foco da frequência de doadores de hidrogênio",
       subtitle = "Reta em destaque se refere ao diazepam",
       x = "Número de hidrogênios doáveis", y = "Frequência")

# NumRotatableBonds
# Gráfico de dispersão em ordem crescente
df_drugbank %>%
  arrange(NumRotatableBonds) %>%
  mutate(Database.ID = factor(Database.ID , levels = Database.ID)) %>%
  ggplot(aes(x = Database.ID, y = NumRotatableBonds)) +
  geom_hline(yintercept = mean(df_drugbank$NumRotatableBonds),
             color = "#0000ff", linewidth = 1.5) +
  geom_point() + 
  geom_point(aes(x = Database.ID, y = NumRotatableBonds,
                 color = "#red", size = 3), df_drugbank[712,]) +
  labs(title = "Quantidade de anéis rotativos",
       subtitle = "Média em azul\nPonto em destaque se refere ao diazepam", 
       x = "", y = "Quantidade de anéis rotativos do composto")

# Histograma
df_drugbank %>%
  ggplot(aes(x = NumRotatableBonds)) + 
  geom_histogram(binwidth = 1, fill = "#a6c6ff", color = "#eaeaea") +
  geom_vline(xintercept = df_drugbank[712, "NumRotatableBonds"], size = .5, color = "red") +
  labs(title = "Frequência da quantidade de anéis rotativos",
       subtitle = "Reta em destaque se refere ao diazepam",
       x = "Número de anéis rotativos", y = "Frequência")

# Histograma com foco na maior concentração
df_drugbank %>%
  filter(NumRotatableBonds <= 50) %>%
  ggplot(aes(x = NumRotatableBonds)) + 
  geom_histogram(binwidth = 1, fill = "#a6c6ff", color = "#eaeaea") +
  geom_vline(xintercept = df_drugbank[712, "NumRotatableBonds"], size = 1.5, color = "red") +
  labs(title = "Foco da frequência da quantidade de anéis rotativos", 
       subtitle = "Reta em destaque se refere ao diazepam",
       x = "Número de anéis rotativos", y = "Frequência")
  
# RingCount
# Gráfico de dispersão em ordem crescente
df_drugbank %>%
  arrange(RingCount) %>%
  mutate(Database.ID = factor(Database.ID , levels = Database.ID)) %>%
  ggplot(aes(x = Database.ID, y = RingCount)) +
  geom_hline(yintercept = mean(df_drugbank$RingCount),
             color = "#0000ff", linewidth = 1.5) +
  geom_point() + 
  geom_point(aes(x = Database.ID, y = RingCount,
                 color = "#red", size = 3), df_drugbank[712,]) +
  labs(title = "Quantidade de anéis",
       subtitle = "Média em azul\nPonto em destaque se refere ao diazepam", 
       x = "", y = "Quantidade de anéis do composto")

# Histograma
df_drugbank %>%
  ggplot(aes(x = RingCount)) + 
  geom_histogram(binwidth = 1, fill = "#a6c6ff", color = "#eaeaea") + 
  geom_vline(xintercept = df_drugbank[712, "RingCount"], size = .5, color = "red") +
  labs(title = "Frequência da quantidade de anéis",
       subtitle = "Reta em destaque se refere ao diazepam",
       x = "Quantidade de anéis do composto", y = "Frequência")

# Histograma com foco na maior concentração
df_drugbank %>%
  filter(RingCount <= 15) %>%
  ggplot(aes(x = RingCount)) + 
  geom_histogram(binwidth = 1, fill = "#a6c6ff", color = "#eaeaea") + 
  geom_vline(xintercept = df_drugbank[712, "RingCount"], size = 1.5, color = "red") +
  labs(title = "Foco da frequência da quantidade de anéis",
       subtitle = "Reta em destaque se refere ao diazepam",
       x = "Quantidade de anéis do composto", y = "Frequência")

# TPSA
# Gráfico de dispersão em ordem crescente
df_drugbank %>%
  arrange(TPSA) %>%
  mutate(Database.ID = factor(Database.ID , levels = Database.ID)) %>%
  ggplot(aes(x = Database.ID, y = TPSA)) +
  geom_hline(yintercept = mean(df_drugbank$TPSA),
             color = "#0000ff", linewidth = 1.5) +
  geom_point() + 
  geom_point(aes(x = Database.ID, y = TPSA,
                 color = "red", size = 3), df_drugbank[712,]) +
  labs(title = "Área da superfície polar em Å²",
       subtitle = "Média em azul\nPonto em destaque se refere ao diazepam",
       x = "", y = "Área total da superfície dos átomos polares em Å²")

# Gráfico de densidade
df_drugbank %>%
  ggplot(aes(x = TPSA)) + 
  geom_density(fill = "#a6c6ff", color = "#eaeaea") + 
  geom_vline(xintercept = df_drugbank[712, "TPSA"], size = .5, color = "red") +
  labs(title = "Densidade da área da superfície polar",
       subtitle = "Reta em destaque se refere ao diazepam",
       x = "Área total da superfície dos átomos polares em Å²", y = "Frequência")

# Gráfico de densidade com foco na maior concentração
df_drugbank %>%
  filter(TPSA <= 750) %>%
  ggplot(aes(x = TPSA)) + 
  geom_density(fill = "#a6c6ff", color = "#eaeaea") + 
  geom_vline(xintercept = df_drugbank[712, "TPSA"], size = 1.5, color = "red") +
  labs(title = "Foco da densidade da área da superfície polar",
       subtitle = "Reta em destaque se refere ao diazepam",
       x = "Área total da superfície dos átomos polares em Å²", y = "Frequência")


# Grupos de compostos


# Criando dataframe de cada grupo
# approved
contains_approved <- df_drugbank %>%
  filter(Drug.Groups %in% c("approved",
                            "approved; experimental",
                            "approved; experimental; investigational",
                            "approved; investigational; withdrawn",
                            "approved; experimental; vet_approved",
                            "approved; illicit",
                            "approved; illicit; investigational",
                            "approved; illicit; investigational; vet_approved",
                            "approved; illicit; investigational; withdrawn",
                            "approved; illicit; vet_approved", "approved; illicit; withdrawn",
                            "approved; investigational",
                            "approved; investigational; nutraceutical",
                            "approved; investigational; nutraceutical; vet_approved",
                            "approved; investigational; vet_approved",
                            "approved; investigational; vet_approved; withdrawn",
                            "approved; investigational; withdrawn",
                            "approved; nutraceutical",
                            "approved; nutraceutical; vet_approved",
                            "approved; nutraceutical; withdrawn",
                            "approved; vet_approved",
                            "approved; vet_approved; withdrawn",
                            "approved; withdrawn"))

# illicit
contains_illicit <- df_drugbank %>%
  filter(Drug.Groups %in% c("experimental; illicit",
                            "illicit",
                            "experimental; illicit; investigational",
                            "illicit; vet_approved",
                            "experimental; illicit; withdrawn",
                            "approved; illicit; vet_approved",
                            "approved; illicit; investigational; vet_approved",
                            "illicit; investigational; withdrawn",
                            "illicit; investigational",
                            "illicit; withdrawn",
                            "approved; illicit; withdrawn",
                            "approved; illicit; investigational",
                            "approved; illicit",
                            "approved; illicit; investigational; withdrawn",
                            "illicit; investigational; vet_approved"))

# illict que não contém approved
not_approved_illicit <- df_drugbank %>%
  filter(Drug.Groups %in% c("experimental; illicit",
                            "illicit",
                            "experimental; illicit; investigational",
                            "illicit; vet_approved",
                            "experimental; illicit; withdrawn",
                            "illicit; investigational; withdrawn",
                            "illicit; investigational",
                            "illicit; withdrawn",
                            "illicit; investigational; vet_approved"))

# approved e illicit
approved_illicit <- df_drugbank %>%
  filter(Drug.Gropus %in% c("approved; illicit; vet_approved",
                            "approved; illicit; investigational; vet_approved",
                            "approved; illicit; withdrawn",
                            "approved; illicit; investigational",
                            "approved; illicit",
                            "approved; illicit; investigational; withdrawn"))


# Criando gráfico de barras para comparação dos grupos

# criando dataframe para utilizar o ggplot
df <- data.frame(group = c("countains_approved",
                           "countains_illicit",
                           "not_approved_illicit"),
                 total = c(length(contains_approved$Database.ID),
                           length(contains_illicit$Database.ID),
                           length(not_approved_illicit)))

ggplot(data = df, aes(x = group, y = total)) + 
  geom_bar(stat = "identity") # gráfico de barras