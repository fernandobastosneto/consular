library(patchwork)

df %>% 
  filter(str_detect(posto, "Cairo") | str_detect(posto, "Barcelona") | str_detect(posto, "Miami")) %>%
  # filter() %>% 
  ggplot(aes(total, posto, fill = posto)) +
  geom_col(show.legend = F) +
  facet_wrap(~ ano, scales = "free_y") +
  ggplot2::scale_x_continuous(labels = scales::label_number_si(accuracy = 0.1)) +
  ggthemes::scale_fill_tableau() +
  theme_minimal() +
  labs(y = "Vistos",
       title = "Comparação de Consulados",
       subtitle = "Vistos Produzidos")


df %>% 
  arrange(desc(total)) %>% 
  filter(ano == "2019") %>% 
  View()
  View()

documentos_postos <- function(nome) {

  df %>% 
    filter(str_detect(posto, nome)) %>% 
             pivot_longer(2:10, names_to = "documento", values_to = "quantidade") %>%
             ggplot(aes(documento, quantidade, fill = posto)) +
             geom_col(show.legend = F) +
             facet_wrap(~ ano, scales = "free_y") +
             ggplot2::scale_y_continuous(labels = scales::label_number_si(accuracy = 0.1)) +
             ggthemes::scale_fill_tableau() +
             theme_minimal() +
             labs(y = "Quantidade",
                  # title = "Documentos Produzidos",
                  subtitle = nome,
                  x = "Documentos") +
             coord_flip()
         
}


documentos_postos("Xangai")/documentos_postos("Pequim")/documentos_postos("Cantão")


df %>% 
  filter(ano == "2019") %>% 
  # filter(str_detect(posto, "Xangai") | 
  #          str_detect(posto, "Pequim") | 
  #          str_detect(posto, "Cantão") |
  #          str_detect(posto, "Lisboa") |
  #          str_detect(posto, "Boston") |
  #          str_detect(posto, "Barcelona") |
  #          str_detect(posto, "Nova York")) %>%
  mutate(passaporte = arb +pases + padip + pasof + pacom) %>% 
  arrange(desc(total)) %>% 
  slice_max(total, n = 90) %>% 
  select(-c(arb, pases, padip, pasof, pacom)) %>% 
  relocate(passaporte, .before = vistos) %>% 
  pivot_longer(2:6, names_to = "documento", values_to = "quantidade") %>%
  ggplot() +
  geom_col(aes(posto, quantidade, fill = documento), position = "fill") +
  facet_wrap(~ ano, scales = "free") +
  coord_flip() +
  ggthemes::scale_fill_tableau(palette = "Tableau 20") +
  theme_minimal() +
  labs(y = "Quantidade",
       # title = "Documentos Produzidos",
       # subtitle = nome,
       x = "Documentos")
  # coord_flip()


df %>% 
  filter(ano != "2020") %>% 
  filter(posto == "Nova York" | posto == "Miami" | posto == "Boston" | posto == "Chicago" |
           posto == "Atlanta" | posto == "Chicago" | posto == "São Francisco") %>% 
  arrange(desc(total_ouro)) %>% 
  select(posto, ano, total_ouro) %>% 
  pivot_wider(names_from = ano, values_from = total_ouro) %>% 
  mutate(perda = `2018` - `2019`) %>% 
  # select(posto, perda)
  ggplot() +
  geom_col(aes(posto, perda, fill = posto), position = "dodge")



paises_om <- c("Amã","Cairo","Beirute"
                 ,"Argel" ,"Rabat","Túnis"
                 ,"Abu Dhabi","Bagdá","Damasco"
                 ,"Doha","Kuaite","Mascate"
                 ,"Riade")

df %>% 
  # filter(posto == "Amã" | posto == "Cairo" | posto == "Beirute" |
  #          posto == "Argel" |  posto == "Rabat" | posto == "Túnis" |
  #          posto == "Abu Dhabi" | posto == "Bagdá" | posto == "Damasco" |
  #          posto == "Doha" | posto == "Kuaite" | posto == "Mascate" |
  #          posto == "Riade") %>% 
  filter(posto %in% paises_om) %>% 
  ggplot() +
  geom_col(aes(tidytext::reorder_within(posto, total, ano), total, fill = posto), position = "dodge",
           show.legend = F) +
  facet_wrap(~ ano, scales = "free_y") +
  coord_flip() +
  tidytext::scale_x_reordered()

df %>% 
  # filter(posto == "Amã" | posto == "Cairo" | posto == "Beirute" |
  #          posto == "Argel" |  posto == "Rabat" | posto == "Túnis" |
  #          posto == "Abu Dhabi" | posto == "Bagdá" | posto == "Damasco" |
  #          posto == "Doha" | posto == "Kuaite" | posto == "Mascate" |
  #          posto == "Riade") %>% 
  filter(posto %in% paises_om) %>% 
  mutate(passaporte = arb +pases + padip + pasof + pacom) %>% 
  arrange(desc(total)) %>% 
  # slice_max(total, n = 90) %>% 
  select(-c(arb, pases, padip, pasof, pacom)) %>% 
  relocate(passaporte, .before = vistos) %>% 
  pivot_longer(2:6, names_to = "documento", values_to = "quantidade") %>% 
  ggplot() +
  geom_col(aes(quantidade, posto, fill = documento), position = "fill") +
  # geom_col(aes(tidytext::reorder_within(posto, quantidade, ano), total, fill = quantidade), position = "fill",
           # show.legend = F) +
  facet_wrap(~ ano, scales = "free")
  # coord_flip() +
  # tidytext::scale_x_reordered()


df %>% 
  filter(ano == "2019") %>% 
  mutate(dia = total/251) %>% 
  View()
  
