library(stringr)
library(rvest)

webfile <- "https://futurama.fandom.com/wiki/Futurama_Wiki"
webpage <- read_html( webfile)


SeasonsOversheet4 <-  webpage %>%
  
  html_nodes(xpath = "/html/body/div[4]/div[4]/div[2]/header/nav/ul/li[4]/div[2]/ul/li/div/ul/li/a") %>%
  html_attr("href") %>%
  str_trim()


season <- read_html(SeasonsOversheet4[1])

SeaonTables <-  season %>%
  as.data.frame() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table(header = T)


check<-as.data.frame(SeaonTables)
