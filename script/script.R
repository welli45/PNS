# pacotes
if(!require(survey)){install.packages("survey")};library(survey)
if(!require(srvyr)){install.packages("srvyr")};library(srvyr)
if(!require(PNSIBGE)){install.packages("PNSIBGE");require(PNSIBGE)}
if(!require(tidyverse)){install.packages("tidyverse");require(tidyverse)}

# obtendo ajuda sobre o pacote
help("get_pns")

help("as_survey")

# padrão dos resultados
options(scipen=999)

################################################################################
############################### - PNS 2013 - ###################################
################################################################################

# Baixando sem o design PNS2013
pns.svy3Sd <- get_pns(year=2013, selected=F, anthropometry=T,
                      labels=T, deflator=T, design=F)

# criando indicadores

# IMC

pns.svy3Sd <- pns.svy3Sd %>% mutate(altura_metro = W00203 / 100)
pns.svy3Sd <- pns.svy3Sd %>% mutate(IMC = W00103 / (altura_metro * altura_metro))

# Excesso de peso - excpeso 
pns.svy3Sd <- pns.svy3Sd %>% mutate(excpeso = ifelse(IMC >=24.999999999999,1,2))
pns.svy3Sd$excpeso<-factor(pns.svy3Sd$excpeso, levels=c(1,2), labels=c("Sim","Não"))
summary(pns.svy3Sd$excpeso)

# Obesidade - obesid 

pns.svy3Sd <- pns.svy3Sd %>% mutate(obesid = ifelse(IMC >=29.999999999999,1,2))
pns.svy3Sd$obesid<-factor(pns.svy3Sd$obesid, levels=c(1,2), labels=c("Sim","Não"))
summary(pns.svy3Sd$obesid)

# Classificação IMC
pns.svy3Sd <- pns.svy3Sd %>% 
  mutate(IMC_classif= cut(IMC,
                          breaks=c(18.5, 24.9, 29.9, 34.9, 39.9, 40, Inf),
                          labels=c("Abaixo do peso", "Eutrofia", "Sobrepeso", 
                                   "Obesidade grau I", "Obesidade grau II", 
                                   "Obesidade grau III")))
table(pns.svy3Sd$IMC_classif)

#Situação Urbano ou Rural
pns.svy3Sd <- pns.svy3Sd %>% 
  rename(Sit_Urbano_Rural=V0026)

summary(pns.svy3Sd$Sit_Urbano_Rural)

#Sexo
pns.svy3Sd <- pns.svy3Sd %>% 
  rename(Sexo=C006)

summary(pns.svy3Sd$Sexo)

#Estados - UFs
pns.svy3Sd <- pns.svy3Sd %>% 
  rename(Unidades_da_Federacao=V0001)

summary(pns.svy3Sd$Unidades_da_Federacao)

#Capital
pns.svy3Sd<- pns.svy3Sd %>% mutate(Capital= fct_collapse(Unidades_da_Federacao,
                                                       `Porto Velho`= "Rondônia", 
                                                       `Boa Vista`= "Roraima",              
                                                       `Rio Branco`= "Acre", 
                                                       `Manaus` = "Amazonas",
                                                       `Belém` = "Pará" ,
                                                       `Macapá`= "Amapá",
                                                       `Palmas` = "Tocantins",
                                                       `São Luís` = "Maranhão",
                                                       `Teresina`= "Piauí" ,
                                                       `Fortaleza`= "Ceará",
                                                       `Natal`= "Rio Grande do Norte",
                                                       `João Pessoa`= "Paraíba",
                                                       `Recife`= "Pernambuco",
                                                       `Maceió`= "Alagoas",
                                                       `Aracaju`= "Sergipe",
                                                       `Salvador`= "Bahia",
                                                       `Belo Horizonte`= "Minas Gerais",
                                                       `Vitória`= "Espírito Santo",
                                                       `Rio de Janeiro`= "Rio de Janeiro",
                                                       `São Paulo`= "São Paulo",
                                                       `Curitiba`= "Paraná",
                                                       `Florianópolis`= "Santa Catarina",
                                                       `Porto Alegre`= "Rio Grande do Sul",
                                                       `Campo Grande`=  "Mato Grosso do Sul",
                                                       `Cuiabá`= "Mato Grosso",
                                                       `Goiânia` = "Goiás",
                                                       `Brasília`= "Distrito Federal"))
summary(pns.svy3Sd$Capital)


#Faixas Etárias
pns.svy3Sd <-  pns.svy3Sd %>% 
  mutate(faixa_idade=cut(C008,
                         breaks = c(18,25,30,35,45,55,65,75,Inf),
                         labels = c("18 a 24 anos","25 a 29 anos","30 a 34 anos",
                                    "35 a 44 anos","45 a 54 anos", "55 a 64 anos",
                                    "65 a 74 anos", "75 anos ou mais"), 
                                    ordered_result = TRUE, right = FALSE))
summary(pns.svy3Sd$faixa_idade)



#Rendimento domiciliar per capita
pns.svy3Sd <-  pns.svy3Sd  %>% mutate(rend_per_capita=cut(VDF003,
                                                        breaks = c(-Inf,339, 678, 1356, 2034,Inf),
                                                        labels=c("Até 1/2 SM","1/2 até 1 SM","1 até 2 SM","2 até 3 SM","Mais de 3 SM"), 
                                                        ordered_result = TRUE, right = TRUE, na.exclude= TRUE))

summary(pns.svy3Sd$rend_per_capita)
      

# Escolaridade
pns.svy3Sd <- pns.svy3Sd %>% 
  rename(gescol=VDD004A)

summary(pns.svy3Sd$gescol)

# aplicando o design
dadosPNS13 <- pns_design(data_pns=pns.svy3Sd)


# Prevalencia da obesidade em 2013

svymean(~obesid,dadosPNS13, na.rm = T)

# Prevalência de pessoas com obesidade no total de pessoas de 18 anos ou mais de idade, 
# segundo os grupos de idade - Brasil - 2013

svyby(~obesid, ~faixa_idade, dadosPNS13, na.rm = T, svymean)

# Excesso de peso

svyby(~excpeso, ~faixa_idade, dadosPNS13, na.rm = T, svymean)

# tabela para BI - UF
tab_classifica_IMCPNSUF3 <- svytable(~IMC_classif+obesid+excpeso+Unidades_da_Federacao+
                                       +faixa_idade+rend_per_capita+Sit_Urbano_Rural+
                                       +Sexo+gescol, dadosPNS13) %>% as.data.frame() %>% 
  filter(Freq != 0)

# Salvando tabela
write.csv(tab_classifica_IMCPNSUF3, "tab_classifica_IMCPNSUF3.csv",
          fileEncoding = "UTF-8", row.names = F)

# capitais 
pns.svy3SdC <- subset(dadosPNS13, V0031=="Capital")

# tabela para BI - Capitais

tab_classifica_IMCPNSCS3 <- svytable(~IMC_classif+obesid+excpeso+Capital+
                                       +faixa_idade+rend_per_capita+Sit_Urbano_Rural+
                                       +Sexo+gescol, pns.svy3SdC) %>% as.data.frame() %>% 
  filter(Freq != 0)

# Salvando tabela
write.csv(tab_classifica_IMCPNSCS3, "tab_classifica_IMCPNSCS3.csv",
          fileEncoding = "UTF-8", row.names = F)

# removendo data frame de 2013
rm(pns.svy3Sd)

################################################################################
############################### - PNS 2019 - ###################################
################################################################################


# Baixando sem o design PNS2019
pns.svy9Sd <- get_pns(year=2019, selected=F, anthropometry=T,
                      labels=T, deflator=T, design=F)

# criando indicadores

# IMC

pns.svy9Sd <- pns.svy9Sd %>% 
  mutate(altura_metro = W00203 / 100)

pns.svy9Sd <- pns.svy9Sd %>% 
  mutate(IMC = W00103 / (altura_metro * altura_metro))

# Excesso de peso - excpeso 
pns.svy9Sd <- pns.svy9Sd %>% 
  mutate(excpeso = ifelse(IMC >=24.999999999999,1,2))

pns.svy9Sd$excpeso <- factor(pns.svy9Sd$excpeso, 
                             levels=c(1,2), labels=c("Sim","Não"))
summary(pns.svy9Sd$excpeso)

# Obesidade - obesid 

pns.svy9Sd <- pns.svy9Sd %>% 
  mutate(obesid = ifelse(IMC >=29.999999999999,1,2))

pns.svy9Sd$obesid<-factor(pns.svy9Sd$obesid, 
                          levels=c(1,2), labels=c("Sim","Não"))
summary(pns.svy9Sd$obesid)

# Classificação IMC
pns.svy9Sd <- pns.svy9Sd %>% 
  mutate(IMC_classif= cut(IMC,
                          breaks=c(18.5, 24.9, 29.9, 34.9, 39.9, 40, Inf),
                          labels=c("Abaixo do peso", "Eutrofia", "Sobrepeso", 
                                   "Obesidade grau I", "Obesidade grau II", 
                                   "Obesidade grau III")))
table(pns.svy9Sd$IMC_classif)

#Situação Urbano ou Rural
pns.svy9Sd <- pns.svy9Sd %>% 
  rename(Sit_Urbano_Rural=V0026)

summary(pns.svy9Sd$Sit_Urbano_Rural)

#Sexo
pns.svy9Sd <- pns.svy9Sd %>% 
  rename(Sexo=C006)

summary(pns.svy9Sd$Sexo)

#Estados - UFs
pns.svy9Sd <- pns.svy9Sd %>% 
  rename(Unidades_da_Federacao=V0001)

summary(pns.svy9Sd$Unidades_da_Federacao)

#Capital
pns.svy9Sd<- pns.svy9Sd %>% mutate(Capital= fct_collapse(Unidades_da_Federacao,
                                                         `Porto Velho`= "Rondônia", 
                                                         `Boa Vista`= "Roraima",              
                                                         `Rio Branco`= "Acre", 
                                                         `Manaus` = "Amazonas",
                                                         `Belém` = "Pará" ,
                                                         `Macapá`= "Amapá",
                                                         `Palmas` = "Tocantins",
                                                         `São Luís` = "Maranhão",
                                                         `Teresina`= "Piauí" ,
                                                         `Fortaleza`= "Ceará",
                                                         `Natal`= "Rio Grande do Norte",
                                                         `João Pessoa`= "Paraíba",
                                                         `Recife`= "Pernambuco",
                                                         `Maceió`= "Alagoas",
                                                         `Aracaju`= "Sergipe",
                                                         `Salvador`= "Bahia",
                                                         `Belo Horizonte`= "Minas Gerais",
                                                         `Vitória`= "Espírito Santo",
                                                         `Rio de Janeiro`= "Rio de Janeiro",
                                                         `São Paulo`= "São Paulo",
                                                         `Curitiba`= "Paraná",
                                                         `Florianópolis`= "Santa Catarina",
                                                         `Porto Alegre`= "Rio Grande do Sul",
                                                         `Campo Grande`=  "Mato Grosso do Sul",
                                                         `Cuiabá`= "Mato Grosso",
                                                         `Goiânia` = "Goiás",
                                                         `Brasília`= "Distrito Federal"))
summary(pns.svy9Sd$Capital)

#Faixas Etárias
pns.svy9Sd <-  pns.svy9Sd %>% 
  mutate(faixa_idade=cut(C008,
                         breaks = c(18,25,40,60,Inf),
                         labels = c("18 a 24 anos","25 a 39 anos","40 a 59 anos",
                                    "60 anos ou mais"), 
                         ordered_result = TRUE, right = FALSE))
summary(pns.svy9Sd$faixa_idade)



#Rendimento domiciliar per capita
pns.svy9Sd <-  pns.svy9Sd  %>% mutate(rend_per_capita=cut(VDF003,
                                                          breaks = c(-Inf,339, 678, 1356, 2034,Inf),
                                                          labels=c("Até 1/2 SM","1/2 até 1 SM","1 até 2 SM","2 até 3 SM","Mais de 3 SM"), 
                                                          ordered_result = TRUE, right = TRUE, na.exclude= TRUE))

summary(pns.svy9Sd$rend_per_capita)

# Escolaridade
pns.svy9Sd <- pns.svy9Sd %>% 
  rename(gescol=VDD004A)

summary(pns.svy9Sd$gescol)

# aplicando o design
dadosPNS19 <- pns_design(data_pns=pns.svy9Sd)


# Prevalencia da obesidade em 2013

svymean(~obesid,dadosPNS19, na.rm = T)

# Prevalência de pessoas com obesidade no total de pessoas de 18 anos ou mais de idade, 
# segundo os grupos de idade - Brasil - 2013

svyby(~obesid, ~faixa_idade, dadosPNS19, svytotal, na.rm = T)


# Excesso de peso

svyby(~excpeso, ~faixa_idade, dadosPNS19, na.rm = T, svymean)

# tabela para BI - UF
tab_classifica_IMCPNSUF9 <- svytable(~IMC_classif+obesid+excpeso+Unidades_da_Federacao+
                                       +faixa_idade+rend_per_capita+Sit_Urbano_Rural+
                                       +Sexo+gescol, dadosPNS19) %>% as.data.frame() %>% 
  filter(Freq != 0)

# Salvando tabela
write.csv(tab_classifica_IMCPNSUF9, "tab_classifica_IMCPNSUF9.csv",
          fileEncoding = "UTF-8", row.names = F)

# capitais 
pns.svy9SdC <- subset(dadosPNS19, V0031=="Capital")

# tabela para BI - Capitais

tab_classifica_IMCPNSCS9 <- svytable(~IMC_classif+obesid+excpeso+Capital+
                                       +faixa_idade+rend_per_capita+Sit_Urbano_Rural+
                                       +Sexo+gescol, pns.svy9SdC) %>% as.data.frame() %>% 
  filter(Freq != 0)

# Salvando tabela
write.csv(tab_classifica_IMCPNSCS9, "tab_classifica_IMCPNSCS9.csv",
          fileEncoding = "UTF-8", row.names = F)

