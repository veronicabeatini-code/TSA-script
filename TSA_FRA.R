#4/03/2024 Script per organizzare i dati da excel by Veronica Beatini

#creo un vettore in un oggetto "variables" con i valori colonna originali

variables <- c('SUPP1', 'SUPP2', 'SUPP3','SUPP4','SUPP5','NUMR','SUPP6','TEMPS',
               'SUPP7','NUMA')

#vettore unico con serie e domande

sessions <- paste0('S', 1:9)
questions <- paste0('Q', 1:6)
table <- expand.grid(questions, sessions)


table$Var3 <- paste0(table$Var2, table$Var1,"_")
table1 <- expand.grid(variables , table$Var3)
table1$Var3 <- paste0(table1$Var2, table1$Var1)

#installo il pacchetto tidyverse per dopo

library(tidyverse)

#importo file excel

library(readxl)
TSA_fra <- read_excel("TSA_FRA.xlsx")#[, 1:547]
View(TSA_fra)
#creo un vettore in un oggetto con l'insieme delle variabili di prima e le nuove

nomvariables <- c('AGE','SEX','CODE', table1$Var3, 'latency(s)',
                  'timetot/latency','rightansw/tot','score tot')

names(TSA_fra) <- nomvariables

#funzione grep per selezionare tutti i membri con supp e 
#con invert quelli che non hanno supp

varconcerve <- grep(pattern = 'SUPP', x = names(TSA_fra), value = T, invert = T)
tsadatafinal <- TSA_fra[, varconcerve]

#correction de donnes pour NUMA

varnuma <- grep(pattern = 'NUMA', x = varconcerve , value = T) 

#ciclo for per correggere le colonne risposte attese 

for(i in varnuma) {
  tsadatafinal[2:nrow(tsadatafinal), i] <- tsadatafinal[1, i]
}
#creo un file excel con i dati ordinati 

#write_excel_csv2(x = tsadatafinal, file = 'dataprop.csv')

library(tidyr)
dati_wide <- data.frame(tsadatafinal)
print(dati_wide)

#creo i vettori per ciascuna colonna 

NUMR <- names(dati_wide) %>% 
  grep(pattern = '^S[1-9]', value = T) %>% 
  grep(pattern = 'NUMR$', value = T)

NUMA <- names(dati_wide) %>% 
  grep(pattern = '^S[1-9]', value = T) %>% 
  grep(pattern = 'NUMA$', value = T)

TEMPS <- names(dati_wide) %>% 
  grep(pattern = '^S[1-9]', value = T) %>% 
  grep(pattern = 'TEMPS$', value = T)


library(tidyr)
library(dplyr)

# Assuming df is your dataframe and spec is your pivot specification
dati_wide <- dati_wide %>%
  mutate(across(c(S1Q1_NUMA, S4Q5_NUMA), as.numeric))

#NUMR: pivot loger per ciscuna colonna 
dati_long_NUMR <- dati_wide %>%
  pivot_longer(cols = all_of(NUMR),  
               names_to = c('Serie','Question'),
               values_to = "NUMR",
               names_pattern = "S(\\d+)Q(\\d+)") %>%
  select(CODE, AGE, SEX, Serie, Question, NUMR)

#NUMA
dati_long_NUMA <- dati_wide %>%
  pivot_longer(cols = all_of(NUMA),  
               names_to = c('Serie','Question'),
               values_to = "NUMA",
               names_pattern = "S(\\d+)Q(\\d+)") %>%
  select(CODE, AGE, SEX, Serie, Question, NUMA)

#TEMPS
dati_long_TEMPS <- dati_wide %>%
  pivot_longer(cols = all_of(TEMPS),  
               names_to = c('Serie','Question'),
               values_to = "TEMPS",
               names_pattern = "S(\\d+)Q(\\d+)") %>%
  select(CODE, AGE, SEX, Serie, Question, TEMPS)

print('Dati in formato long:')
#print(dati_long)

merge1 <- merge(dati_long_NUMR, 
                dati_long_NUMA %>% select(-SEX, -AGE), 
                by = c('CODE', 'Serie','Question'))
merge2 <- merge(merge1, dati_long_TEMPS %>% select(-SEX, -AGE), 
                by = c('CODE', 'Serie','Question'))




#merge2 %>% arrange(AGE) %>% View()

#togliere dal tempo

library(lubridate)

merge2$TEMPS %>% 
  hour() %>% 
  head()

merge2$TEMPS %>% 
  minute() %>% 
  head()

x <- c(6, 3, 2)
y <- c(3, 3, 7)

(x * 100 + y) / 100

merge2$TEMPS2 <- ((merge2$TEMPS %>% 
                     hour())* 100 
                  + (merge2$TEMPS %>% 
                       minute())) /100

merge2$TEMPS <- NULL

merge2$Veronica <- NULL


# Funzione per creare il vettore desiderato (vedere pattern discriminazione)


merge2$inverso <- ifelse(merge2$NUMR+merge2$NUMA ==6 & merge2$NUMR!= merge2$NUMA,
                         1, 0)

merge2$bonnereponse <- ifelse(merge2$NUMR == merge2$NUMA,
                              1, 0)

merge2$inversoerrate <- ifelse(merge2$inverso & !merge2$bonnereponse,
                               1, 0)
#analisi di frequenze assolute 

tab2 <- table(merge2$bonnereponse)

##0   1 
#210 168 

# creo la tabella tab1
tab1 <- table(merge2$bonnereponse)

# frequenze relative
tab1 / sum(tab1)

#0         1 
#0.5555556 0.4444444 

#prop.table(tab1)*100
#round(prop.table(tab1)*100, digits = 2)
#pie(tab2, main = "Distribuzione di risposte corrette")      # diagramma a torta
#barplot(tab2, main = "Distribuzione di risposta corrette")  # diagramma a barre


#for (i in vett) {
#print(mean(merge2[[i]], na.rm = TRUE))
#}


#tsadatafinal[2:nrow(tsadatafinal),1]

#creo un ciclo for per calcolare i valori di ciascun partecipante
#frequenzeassolute

# Inizializzare una lista per memorizzare i risultati
risultati <- list()

# Definire una funzione per calcolare la frequenza assoluta di vettore1 in base a vettore2
calcola_frequenza <- function(subset_dati) {
  table(subset_dati$bonnereponse)
}

# Ciclo for per iterare attraverso gli intervalli da 1 a 6
for (i in 1:7) {
  # Filtrare il data frame per i valori di vettore2 compresi tra i e i+1
  subset_dati <- merge2[merge2$Question == i, ]
  
  # Calcolare la frequenza assoluta e salvare il risultato nella lista
  risultati[[paste("Sujet", i, sep = "_")]] <- calcola_frequenza(subset_dati)
}

# Visualizzare i risultati
print(risultati)

#freqpercentualirisposte

# Inizializzare una lista per memorizzare i risultati
risultati1 <- list()

# Definire una funzione per calcolare le frequenze relative in percentuale di vettore1 in base a vettore2
calcola_frequenza_percentuale <- function(subset_dati) {
  frequenze_assolute <- table(subset_dati$bonnereponse)
  frequenze_relative <- round(prop.table(frequenze_assolute) * 100, digits = 2)
  return(frequenze_relative)
}

# Ciclo for per iterare attraverso gli intervalli da 1 a 6
for (i in 1:6) {
  # Filtrare il data frame per i valori di vettore2 compresi tra i e i+1
  subset_dati <- merge2[merge2$Question == i, ]
  
  # Calcolare le frequenze relative in percentuale e salvare il risultato nella lista
  risultati1[[paste("Sujet", i, sep = "_")]] <- 
    calcola_frequenza_percentuale(subset_dati)
}

# Visualizzare i risultati
print(risultati1)


# Inizializzare una lista per memorizzare i risultati delle medie e deviazioni standard
medie_deviazioni <- list()

# Ciclo for per iterare attraverso gli intervalli da 1 a 6
for (i in 1:6) {
  # Filtrare il data frame per i valori di vettore2 compresi tra i e i+1
  subset_dati <- merge2[merge2$Question == i, ]
  
  # Calcolare la media e la deviazione standard e salvare il risultato nella lista
  media <- mean(subset_dati$TEMPS2)
  deviazione_standard <- sd(subset_dati$TEMPS2)
  
  medie_deviazioni[[paste("soggetto", i, sep = "_")]] <- c(Media = media, Deviazione_Standard = deviazione_standard)
}

for(i in TEMPS2) {
  merge2[1:6(merge2), i] <- tsadatafinal[1, i]
}

# Visualizzare i risultati delle medie e deviazioni standard
print(medie_deviazioni)


# Inizializzare una lista per memorizzare i risultati
risultati2 <- list()

# Definire una funzione per calcolare la frequenza assoluta di vettore1 in base a vettore2
calcola_frequenza_inversione <- function(subset_data) {
  table(subset_data$inverso)
}

# Ciclo for per iterare attraverso gli intervalli da 1 a 6
for (i in 1:6) {
  # Filtrare il data frame per i valori di vettore2 compresi tra i e i+1
  subset_data <- merge2[merge2$Question == i, ]
  
  # Calcolare la frequenza assoluta e salvare il risultato nella lista
  risultati2[[paste("Sujet", i, sep = "_")]] <- calcola_frequenza_inversione(subset_data)
}

# Visualizzare i risultati
print(risultati2)
plot(risultati2)


frequenze_relative <- list()

library(tidyverse)

merge2 %>% group_by(CODE) %>% summarise(inverso)
merge2 %>% group_by(CODE)

merge2 %>% group_by(CODE) %>% summarise(mean(TEMPS2))

merge2 %>% group_by(CODE) %>% summarise(temps_moyen = mean(TEMPS2), temps_sd = sd(TEMPS2))

merge2 %>% group_by(CODE) %>% summarise(temps_moyen = mean(TEMPS2), temps_sd = sd(TEMPS2)) %>% ggplot(aes(x = CODE, y = temps_moyen)) + geom_point()

merge2 %>% group_by(CODE) %>% summarise(bonne_reponse = bonnereponse) %>% ggplot(aes(x = CODE, y = bonne_reponse)) + geom_point()


merge2 %>% group_by(CODE, bonnereponse) %>% summarise(n = n()) %>% summarise(rel = n/sum(n))%>% ggplot(aes(x = CODE, y = rel)) + geom_point()

merge2 %>% group_by(CODE, bonnereponse) %>% summarise(n = n())%>% ggplot(aes(x = CODE, y = rel)) + geom_point()


merge2 %>% group_by(CODE, bonnereponse) %>% summarise(n = n())

#analisibootstrap


library(boot)

bootR <- boot(data = merge2, statistic = statistic, R = 1000)


statistic <- function(data, index) {
  
  frequenza_relativa_1 <- sum(data$inverso[index] == 1) / length(data$inverso[index])
  
  return(frequenza_relativa_1)
  
}

statistic(data = merge2, index = 1:540)



library(ggplot2)

hist(bootR$t)

library(boot)

boot.ci(boot.out = bootR, conf = 0.95, type = c("norme", "basic","bca"))


stat_originale <- statistic(merge2)
stat_bootstrap <- bootR$t
delta_stat <- stat_bootstrap - stat_originale 
p_value <- mean( delta_stat >= 0) 
print(p_value)

#analisibootstrap


#metodopoisson

#Adattare un modello di regressione di Poisson
modello_poisson <- glm(numero_inversioni ~ offset(log(totale_osservazioni)), family = poisson)

summary(modello_poisson)

library(lme4)

mod <- glmer(inverso~1 + (1|CODE), family = binomial(), data = merge2)


prob_inverso <- exp(mod@beta) / (1 + exp(mod@beta))



mod_simple <- glm(inverso~1, family = binomial(), data = merge2)


library(tidyverse)


Ugo <- function(nombre) { 
  hugo <- as.character(nombre) 
  hugues <- str_replace(string = hugo, pattern = '\\.0', replacement = '.')
  hughi <- as.numeric(hugues) 
  return(hughi)
  
}


Ugo(nombre = merge2$TEMPS2)

merge2$TEMPS2 <- Ugo(nombre = merge2$TEMPS2)

# Plot
merge2 %>%
  group_by(CODE) %>%
  ggplot(aes(x = CODE, y = TEMPS2)) +
  geom_boxplot() +
  labs(x = "Subjects", 
       y = "Response time (s)",
       title = 'Response time in function of subjects (s)')



merge2 %>%
  mutate(bonnereponse = as.factor(bonnereponse)) %>%
  group_by(CODE, bonnereponse) %>%
  tally() %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = CODE, y = perc, fill = bonnereponse)) +
  geom_bar(position="stack", stat="identity") +
  labs(x = "Sujet", 
       y = "Fréquence relative",
       title = 'Relative frequency in function of subjects',
       fill = 'Correct answer')


(merge2 %>%
    mutate(bonnereponse = as.factor(bonnereponse)) %>%
    group_by(CODE, Serie, bonnereponse, .drop = FALSE) %>%
    tally() %>%
    mutate(perc = n/ sum(n) * 100) %>%
    filter(bonnereponse == 1) %>%
    ggplot(aes(x = Serie, y = perc, group = CODE, color = CODE)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = 'lm') +
    facet_wrap(~CODE) +
    labs(x = "Série",
         y = "Fréquence relative",
         title = 'Fréq bonnes réponses en fonct de la série',
         color = 'Sujet')) %>%
  ggsave(filename = 'perc_f_serietsafra.svg', 
         plot = .,
         device = 'svg',
         width = 15,
         height = 10,
         units = 'cm')


# Plot
merge2 %>%
  ggplot(aes(x = Serie, y = TEMPS2  , group = CODE, color = CODE)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~CODE) +
  labs(x = "Série",
       y = "Temps de reponse (s)",
       title = 'Temps de reponse en fonct de la série',
       color = 'Sujet') 
%>%
ggsave(filename = 'temps_serietsafra.svg', 
       plot = .,
       device = 'svg',
       width = 15,
       height = 10,
       units = 'cm') 


merge2 %>%
  mutate(inverso = as.factor(inverso)) %>%
  group_by(CODE, inverso) %>%
  tally() %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = CODE, y = perc, fill = inverso)) +
  geom_bar(position="stack", stat="identity") +
  labs(x = "Sujet", 
       y = "Fréquence relative",
       title = 'Fréq rel des inversions en fonct des sujets',
       fill = 'Réponse correcte')



# Plot
merge2$Question_total <- as.numeric(merge2$Question) + (6 * (as.numeric(merge2$Serie) - 1))

merge2 %>%
  ggplot(aes(x = Question_total, y = TEMPS2  , group = CODE, color = CODE)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = 'lm') +
  facet_wrap(~CODE) +
  labs(x = "Question",
       y = "Temps de reponse (s)",
       title = 'Temps de reponse en fonct de la question',
       color = 'Sujet') 
# ggsave(filename = 'temps_question.svg', 
#        plot = .,
#        device = 'svg',
#        width = 15,
#        height = 10,
#        units = 'cm')

