library(readxl)
library(ggplot2)
library(dplyr)
library(scales)

df <- read_xlsx("C:\\Users\\brmmi111\\Documents\\Diversidade\\Apoio\\DIVERSIDADE_TABELA.xlsx")


df <- df %>% 
  mutate(
    RETENCAO = ifelse(TRETENCAO <= 3, 'até 3 meses',
                      ifelse(TRETENCAO >3  & TRETENCAO <= 6, '3 a 6 meses',
                             ifelse(TRETENCAO >6  & TRETENCAO <= 12, '6 meses a 1 ano',
                                    ifelse(TRETENCAO  >12, 'mais de 1 ano',
                                          'não')))))

df %>%
  filter(BASE == "MULHER") %>%
  filter(ANO_MOV >= 2019)%>%
  ggplot(aes(x=ANO_MOV)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  scale_y_continuous(breaks = seq(from = 0,to = 2000,by = 500), limits = c(0,2000))+
  ylab("Frequência") +
  xlab("Ano") +
  ggtitle("Movimentações Mulheres - Ativas ou Não")+
  facet_wrap(~ATIVO)

df %>%
  filter(BASE == "MULHER") %>%
  filter(ALT_GRADE == "sim")%>%
  ggplot(aes(x=ANO_MOV)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  scale_y_continuous(breaks = seq(from = 0,to = 1000,by = 250), limits = c(0,1000))+
  ylab("Frequência") +
  xlab("Ano") +
  ggtitle("Promoções Mulheres - Ativas ou Não")+
  facet_wrap(~ATIVO)

df %>%
  filter(BASE == "MULHER") %>%
  filter(ANO_MOV >= 2019)%>%
  filter(ATIVO == "não")%>%
  mutate(
    MOTIVO = factor(MOTIVO, levels = c('Voluntário', 'Involuntário',
                                       'Transferencia')))%>%
  ggplot(aes(x=MOTIVO)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  scale_y_continuous(breaks = seq(from = 0,to = 500,by = 100), limits = c(0,500))+
  ylab("Frequência") +
  xlab("Motivos") +
  ggtitle("Mulheres - Motivos de Desligamento Após Movimentação")

df %>%
  filter(BASE == "MULHER") %>%
  filter(ALT_GRADE == "sim")%>%
  filter(ATIVO == "não")%>%
  mutate(
    MOTIVO = factor(MOTIVO, levels = c('Voluntário', 'Involuntário',
                                           'Transferencia')))%>%
  ggplot(aes(x=MOTIVO)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  scale_y_continuous(breaks = seq(from = 0,to = 100,by = 25), limits = c(0,100))+
  ylab("Frequência") +
  xlab("Motivos") +
  ggtitle("Mulheres - Motivos de Desligamento Após Promoção")

df %>%
  filter(BASE == "MULHER") %>%
  filter(MOV =="sim")%>%
  filter(ATIVO =="sim")%>%
  mutate(
    RETENCAO = factor(RETENCAO, levels = c('até 3 meses', '3 a 6 meses',
                                           '6 meses a 1 ano', 'mais de 1 ano')))%>%
  ggplot(aes(x=RETENCAO)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), color = "white", fill = "light blue") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.2,) +
  scale_y_continuous(labels = percent)+
  ylab("Frequência") +
  xlab("Tempo Retenção") +
  ggtitle("Mulheres - % Retenção Após Movimentação")

df %>%
  filter(BASE == "MULHER") %>%
  filter(ALT_GRADE =="sim")%>%
  filter(ATIVO =="sim")%>%
  mutate(
    RETENCAO = factor(RETENCAO, levels = c('até 3 meses', '3 a 6 meses',
                                           '6 meses a 1 ano', 'mais de 1 ano')))%>%
  ggplot(aes(x=RETENCAO)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), color = "white", fill = "light blue") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.2,) +
  scale_y_continuous(labels = percent)+
  ylab("Frequência") +
  xlab("Tempo Retenção") +
  ggtitle("Mulheres - % Retenção Após Promoção")


df %>%
  filter(BASE == "ETNIA") %>%
  filter(ANO_MOV >= 2019)%>%
  ggplot(aes(x=ANO_MOV)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  scale_y_continuous(breaks = seq(from = 0,to = 2500,by = 500), limits = c(0,2500))+
  ylab("Frequência") +
  xlab("Ano") +
  ggtitle("Movimentações Negros/Pardos - Ativos ou Não")+
  facet_wrap(~ATIVO)

df %>%
  filter(BASE == "ETNIA") %>%
  filter(ANO_MOV >= 2019)%>%
  filter(ATIVO == "não")%>%
  mutate(
    MOTIVO = factor(MOTIVO, levels = c('Voluntário', 'Involuntário',
                                       'Transferencia')))%>%
  ggplot(aes(x=MOTIVO)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  ylab("Frequência") +
  xlab("Motivos") +
  ggtitle("Negros/Pardos - Motivos de Desligamento Após Movimentação")

df %>%
  filter(BASE == "ETNIA") %>%
  filter(ALT_GRADE== "sim")%>%
  filter(ATIVO == "não")%>%
  mutate(
    MOTIVO = factor(MOTIVO, levels = c('Voluntário', 'Involuntário',
                                       'Transferencia')))%>%
  ggplot(aes(x=MOTIVO)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  scale_y_continuous(breaks = seq(from = 0,to = 150,by = 50), limits = c(0,150))+
  ylab("Frequência") +
  xlab("Motivos") +
  ggtitle("Negros/Pardos - Motivos de Desligamento Após Promoção")

df %>%
  filter(BASE == "ETNIA") %>%
  filter(ALT_GRADE == "sim")%>%
  ggplot(aes(x=ANO_MOV)) +
  geom_bar(stat="count", color = "white", fill = "light blue")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2, color="black",size=4.0)+
  scale_y_continuous(breaks = seq(from = 0,to = 1200,by = 300), limits = c(0,1200))+
  ylab("Frequência") +
  xlab("Ano") +
  ggtitle("Promoções Negros/Pardos - Ativos ou Não")+
  facet_wrap(~ATIVO)


df %>%
  filter(BASE == "ETNIA") %>%
  filter(MOV == "sim")%>%
  filter(ATIVO == "sim")%>%
  mutate(
    RETENCAO = factor(RETENCAO, levels = c('até 3 meses', '3 a 6 meses',
                                           '6 meses a 1 ano', 'mais de 1 ano')))%>%
  ggplot(aes(x=RETENCAO)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), color = "white", fill = "light blue") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.2,) +
  scale_y_continuous(labels = percent)+
  ylab("Frequência") +
  xlab("Tempo Retenção") +
  ggtitle("Negros/Pardos - % Retenção Após Movimentação")


df %>%
  filter(BASE == "ETNIA") %>%
  filter(ALT_GRADE == "sim")%>%
  filter(ATIVO == "sim")%>%
  mutate(
    RETENCAO = factor(RETENCAO, levels = c('até 3 meses', '3 a 6 meses',
                                           '6 meses a 1 ano', 'mais de 1 ano')))%>%
  ggplot(aes(x=RETENCAO)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), color = "white", fill = "light blue") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.2,) +
  scale_y_continuous(labels = percent)+
  ylab("Frequência") +
  xlab("Ano") +
  ggtitle("Negros/Pardos - % Retenção Após Promoção")


