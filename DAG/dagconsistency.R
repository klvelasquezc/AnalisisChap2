##########################################
##
##  DAG Consistency - Chapter 2
##  Karen Velasquez-C
##  03 Nov 2025
##
##########################################

library(dagitty)
library(tidyverse)

# DAG Small mammal abundance and Ecological integrity

DAG_abu <- downloadGraph("dagitty.net/mhXq8GEfk")
plot(DAG_abu)

#https://dagitty.net/m4umWLy4A DAG sin influencia humana
#https://dagitty.net/mhXq8GEfk DAG con influencia humana
#https://dagitty.net/mNZRWNkEw DAG sin path directo entre ProxiIE0.5 y RodAbu


DAG_abu

Dat <- read.csv("data/dataSEM.csv") %>% 
  select(RodAbu:IE250) %>% 
  select(-NDVI, -Aspect) %>% 
  rename(EI250= IE250)

str(Dat)

res_abu <- localTests(x=DAG_abu, data=Dat, type= "cis", R=1000)
res_abu$p.value <- p.adjust(res_abu$p.value) 

res2plot_abu <- res_abu %>% 
  rownames_to_column(var= "Pairs")

ggplot(res2plot_abu, aes(x=estimate, y= Pairs))+
  geom_pointrange(aes(xmin= `2.5%`, xmax= `97.5%`))+
  geom_vline(xintercept = 0, linetype= "dashed", color= "purple")+
  theme_minimal()

# Correction p-value

test$p.value <- p.adjust(test$p.value) 

# DAG Small mammal diversity and Ecological integrity

# Model EI - Small mammal diversity https://dagitty.net/maX7kEkAu


DAG_div <- downloadGraph("dagitty.net/maX7kEkAu")
plot(DAG_div)

DAG_div

Dat <- read.csv("data/dataSEM.csv") %>% 
  select(Rodq1:IE250) %>% 
  select(-NDVI, -Aspect, -Rodq2) %>% 
  rename(EI250= IE250)

res_Div <- localTests( x=DAG_div, data=Dat, type="cis")

res2plot_Div <- res_Div %>% 
  rownames_to_column(var= "Pairs")

ggplot(res2plot_Div, aes(x=estimate, y= Pairs))+
  geom_pointrange(aes(xmin= `2.5%`, xmax= `97.5%`))+
  geom_vline(xintercept = 0, linetype= "dashed", color= "purple")+
  theme_minimal()

