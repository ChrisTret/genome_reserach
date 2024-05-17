library(readr)
library(tidyverse)
library(matrixStats)
data <- read_delim("L1_ALu Data/repeats.tsv", 
  delim = "\t", escape_double = FALSE, trim_ws = TRUE)

Alu_L1 <- data %>% 
  filter(data$repFamily == "Alu" | data$repFamily == 'L1') %>% 
  mutate(genoCenter = round(rowMedians(cbind(genoStart,genoEnd)))) %>% 
  mutate(genoLength = genoEnd - genoStart)

Alu <- Alu_L1 %>% 
  filter(Alu_L1$repFamily == "Alu") %>% 
  select(genoCenter) %>% 
  as.matrix()

just_Alu <- Alu_L1 %>% 
  filter(Alu_L1$repFamily == "Alu")

gene_start_vec <- just_Alu$genoStart[-1]
gene_end_vec <- just_Alu$genoEnd[-length(just_Alu)]
inter_gene_length <-  gene_start_vec - gene_end_vec

hist(inter_gene_length)
ggplot(as.data.frame(inter_gene_length), aes(x = inter_gene_length)) + 
  geom_histogram()
  
L1 <- Alu_L1 %>% 
  filter(Alu_L1$repFamily == 'L1') %>% 
  select(genoCenter) %>% 
  as.matrix()

range <- max(Alu_L1$genoEnd) - min(Alu_L1$genoStart)

mat <- matrix(NA,ncol=1, nrow=29510)
mat[1:length(L1)] <- L1

test <- cbind(Alu, mat)
k1d.s(test, start = 1, end = 100)

dummy <- cbind(c(1:500), c(101:600))
k1d.s(dummy, start = 1, end = 500)

# Analyze gene lengths
# 90%tile around 500
hist(Alu_L1$genoLength)
summary(Alu_L1$genoLength)
sum(Alu_L1$genoLength > 2000)
