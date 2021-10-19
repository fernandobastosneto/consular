library(tidyverse)
library(readxl)

ano_2020 <- readxl::read_excel("data-raw/2020-2018producao_consular.xlsx", sheet = 1, skip = 1) %>% 
  janitor::clean_names() %>% 
  mutate(across(3:last_col(), ~ .x %>% replace_na(0))) %>% 
  select(-c(1, 2)) %>% 
  mutate(posto = str_remove(posto, "^C ")) %>% 
  mutate(posto = str_remove(posto, "^CG ")) %>%
  mutate(posto = str_remove(posto, "^E ")) %>% 
  mutate(posto = str_remove(posto, "^VC ")) %>% 
  arrange(desc(posto)) %>% 
  rename(total = qtd_servi_a_os, total_ouro = tota_l_r_ouro) %>% 
  mutate(ano = "2020")

ano_2019 <- readxl::read_excel("data-raw/2020-2018producao_consular.xlsx", sheet = 2, skip = 1) %>% 
  janitor::clean_names() %>% 
  mutate(across(3:last_col(), ~ .x %>% replace_na(0))) %>% 
  select(-c(1, 2)) %>% 
  mutate(posto = str_remove(posto, "^C ")) %>% 
  mutate(posto = str_remove(posto, "^CG ")) %>%
  mutate(posto = str_remove(posto, "^E ")) %>% 
  mutate(posto = str_remove(posto, "^VC ")) %>% 
  mutate(posto = str_trim(posto)) %>% 
  arrange(desc(posto)) %>% 
  select(-e_visa) %>% 
  rename(total = qtd_servicos, total_ouro = tota_l_r_ouro)%>% 
  mutate(ano = "2019")


ano_2018 <- readxl::read_excel("data-raw/2020-2018producao_consular.xlsx", sheet = 3) %>% 
  janitor::clean_names() %>% 
  mutate(across(3:last_col(), ~ .x %>% replace_na(0))) %>% 
  select(-c(1)) %>% 
  mutate(postos = str_remove(postos, "^Consulado ")) %>% 
  mutate(postos = str_remove(postos, "^do Brasil em")) %>% 
  mutate(postos = str_remove(postos, "^Geral em")) %>% 
  mutate(postos = str_remove(postos, "^Consulado-Geral em")) %>% 
  mutate(postos = str_remove(postos, "^Consulado-Geral")) %>% 
  mutate(postos = str_remove(postos, "^Embaixada")) %>% 
  mutate(postos = str_remove(postos, "^Vice-Consulado ")) %>% 
  mutate(postos = str_remove(postos, "^em ")) %>% 
  mutate(postos = str_remove(postos, "^ em ")) %>% 
  mutate(postos = str_remove(postos, "^ na ")) %>% 
  mutate(postos = str_remove(postos, "^ no ")) %>% 
  mutate(postos = str_remove(postos, "^V\\.C\\. em ")) %>% 
  mutate(postos = str_trim(postos)) %>% 
  arrange(desc(postos)) %>% 
  select(-visto_eletronico) %>% 
  rename(total_ouro = emolumentos_totais_real_ouro, posto = postos) %>% 
  mutate(ano = "2018")


df <- ano_2020 %>% 
  dplyr::bind_rows(ano_2019) %>% 
  filter(posto != "0") %>% 
  filter(str_detect(posto, "CGAC", negate = T)) %>% 
  dplyr::bind_rows(ano_2018) %>%
  filter(str_detect(posto, "Total", negate = T)) %>% 
  filter(str_detect(posto, "TOTAL", negate = T))

