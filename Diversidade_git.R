library(readxl)
library(ggplot2)
library(dplyr)
library(scales)

df <- read_xlsx("C:\\Users\\brmmi111\\Documents\\Diversidade\\Apoio\\DIVERSIDADE_TABELA.xlsx")


df <- df %>% 
  mutate(
    RETENCAO = ifelse(TRETENCAO <= 3, 'at� 3 meses',
                      ifelse(TRETENCAO >3  & TRETENCAO <= 6, '3 a 6 meses',
                             ifelse(TRETENCAO >6  & TRETENCAO <= 12, '6 meses a 1 ano',
                                    ifelse(TRETENCAO  >12, 'mais de 1 ano',
                                          'n�o')))))

df %>%
  filter(BASE == "MULHER") %>%
  filter(ANO_MOV >= 2019)%>%
  ggplot(aes(x=ANO_MOV)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  scale_y_continuous(breaks = seq(from = 0,to = 2000,by = 500), limits = c(0,2000))+
  ylab("Frequ�ncia") +
  xlab("Ano") +
  ggtitle("Movimenta��es Mulheres - Ativas ou N�o")+
  facet_wrap(~ATIVO)

df %>%
  filter(BASE == "MULHER") %>%
  filter(ALT_GRADE == "sim")%>%
  ggplot(aes(x=ANO_MOV)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  scale_y_continuous(breaks = seq(from = 0,to = 1000,by = 250), limits = c(0,1000))+
  ylab("Frequ�ncia") +
  xlab("Ano") +
  ggtitle("Promo��es Mulheres - Ativas ou N�o")+
  facet_wrap(~ATIVO)

df %>%
  filter(BASE == "MULHER") %>%
  filter(ANO_MOV >= 2019)%>%
  filter(ATIVO == "n�o")%>%
  mutate(
    MOTIVO = factor(MOTIVO, levels = c('Volunt�rio', 'Involunt�rio',
                                       'Transferencia')))%>%
  ggplot(aes(x=MOTIVO)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  scale_y_continuous(breaks = seq(from = 0,to = 500,by = 100), limits = c(0,500))+
  ylab("Frequ�ncia") +
  xlab("Motivos") +
  ggtitle("Mulheres - Motivos de Desligamento Ap�s Movimenta��o")

df %>%
  filter(BASE == "MULHER") %>%
  filter(ALT_GRADE == "sim")%>%
  filter(ATIVO == "n�o")%>%
  mutate(
    MOTIVO = factor(MOTIVO, levels = c('Volunt�rio', 'Involunt�rio',
                                           'Transferencia')))%>%
  ggplot(aes(x=MOTIVO)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  scale_y_continuous(breaks = seq(from = 0,to = 100,by = 25), limits = c(0,100))+
  ylab("Frequ�ncia") +
  xlab("Motivos") +
  ggtitle("Mulheres - Motivos de Desligamento Ap�s Promo��o")

df %>%
  filter(BASE == "MULHER") %>%
  filter(MOV =="sim")%>%
  filter(ATIVO =="sim")%>%
  mutate(
    RETENCAO = factor(RETENCAO, levels = c('at� 3 meses', '3 a 6 meses',
                                           '6 meses a 1 ano', 'mais de 1 ano')))%>%
  ggplot(aes(x=RETENCAO)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), color = "white", fill = "light blue") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.2,) +
  scale_y_continuous(labels = percent)+
  ylab("Frequ�ncia") +
  xlab("Tempo Reten��o") +
  ggtitle("Mulheres - % Reten��o Ap�s Movimenta��o")

df %>%
  filter(BASE == "MULHER") %>%
  filter(ALT_GRADE =="sim")%>%
  filter(ATIVO =="sim")%>%
  mutate(
    RETENCAO = factor(RETENCAO, levels = c('at� 3 meses', '3 a 6 meses',
                                           '6 meses a 1 ano', 'mais de 1 ano')))%>%
  ggplot(aes(x=RETENCAO)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), color = "white", fill = "light blue") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.2,) +
  scale_y_continuous(labels = percent)+
  ylab("Frequ�ncia") +
  xlab("Tempo Reten��o") +
  ggtitle("Mulheres - % Reten��o Ap�s Promo��o")


df %>%
  filter(BASE == "ETNIA") %>%
  filter(ANO_MOV >= 2019)%>%
  ggplot(aes(x=ANO_MOV)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  scale_y_continuous(breaks = seq(from = 0,to = 2500,by = 500), limits = c(0,2500))+
  ylab("Frequ�ncia") +
  xlab("Ano") +
  ggtitle("Movimenta��es Negros/Pardos - Ativos ou N�o")+
  facet_wrap(~ATIVO)

df %>%
  filter(BASE == "ETNIA") %>%
  filter(ANO_MOV >= 2019)%>%
  filter(ATIVO == "n�o")%>%
  mutate(
    MOTIVO = factor(MOTIVO, levels = c('Volunt�rio', 'Involunt�rio',
                                       'Transferencia')))%>%
  ggplot(aes(x=MOTIVO)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  ylab("Frequ�ncia") +
  xlab("Motivos") +
  ggtitle("Negros/Pardos - Motivos de Desligamento Ap�s Movimenta��o")

df %>%
  filter(BASE == "ETNIA") %>%
  filter(ALT_GRADE== "sim")%>%
  filter(ATIVO == "n�o")%>%
  mutate(
    MOTIVO = factor(MOTIVO, levels = c('Volunt�rio', 'Involunt�rio',
                                       'Transferencia')))%>%
  ggplot(aes(x=MOTIVO)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  scale_y_continuous(breaks = seq(from = 0,to = 150,by = 50), limits = c(0,150))+
  ylab("Frequ�ncia") +
  xlab("Motivos") +
  ggtitle("Negros/Pardos - Motivos de Desligamento Ap�s Promo��o")

df %>%
  filter(BASE == "ETNIA") %>%
  filter(ALT_GRADE == "sim")%>%
  ggplot(aes(x=ANO_MOV)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  scale_y_continuous(breaks = seq(from = 0,to = 1200,by = 300), limits = c(0,1200))+
  ylab("Frequ�ncia") +
  xlab("Ano") +
  ggtitle("Promo��es Negros/Pardos - Ativos ou N�o")+
  facet_wrap(~ATIVO)


df %>%
  filter(BASE == "ETNIA") %>%
  filter(MOV == "sim")%>%
  filter(ATIVO == "sim")%>%
  mutate(
    RETENCAO = factor(RETENCAO, levels = c('at� 3 meses', '3 a 6 meses',
                                           '6 meses a 1 ano', 'mais de 1 ano')))%>%
  ggplot(aes(x=RETENCAO)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), color = "white", fill = "light blue") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.2,) +
  scale_y_continuous(labels = percent)+
  ylab("Frequ�ncia") +
  xlab("Tempo Reten��o") +
  ggtitle("Negros/Pardos - % Reten��o Ap�s Movimenta��o")


df %>%
  filter(BASE == "ETNIA") %>%
  filter(ALT_GRADE == "sim")%>%
  filter(ATIVO == "sim")%>%
  mutate(
    RETENCAO = factor(RETENCAO, levels = c('at� 3 meses', '3 a 6 meses',
                                           '6 meses a 1 ano', 'mais de 1 ano')))%>%
  ggplot(aes(x=RETENCAO)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), color = "white", fill = "light blue") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.2,) +
  scale_y_continuous(labels = percent)+
  ylab("Frequ�ncia") +
  xlab("Ano") +
  ggtitle("Negros/Pardos - % Reten��o Ap�s Promo��o")


