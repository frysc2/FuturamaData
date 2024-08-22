library(stringr)
library(rvest)

webfile <- "https://futurama.fandom.com/wiki/Futurama_Wiki"
webpage <- read_html( webfile)


SeasonsOversheet4 <-  webpage %>%
  
  html_nodes(xpath = "/html/body/div[4]/div[4]/div[2]/header/nav/ul/li[4]/div[2]/ul/li/div/ul/li/a") %>%
  html_attr("href") %>%
  str_trim()


SeasonDataTable_list <- data.frame()
for (i in 1:5) {
  season <- read_html(SeasonsOversheet4[i])
  
  SeaonTables <-  season %>%
    html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
    html_table(header = T)
  
  
  SeasonDataTable_raw<-as.data.frame(SeaonTables)
  SeasonDataTable <- SeasonDataTable_raw[seq(from=1, to=length(SeasonDataTable_raw$Ep), by=2),]
  descriptions <- SeasonDataTable_raw$Ep[seq(from=2, to=length(SeasonDataTable_raw$Ep), by=2)]
  SeasonDataTable$Description <- descriptions
  SeasonDataTable$season <- paste0("Season ", i)
  colnames(SeasonDataTable) <- c("Eps", "Title","Writers", "Director", "Original_AirDate", "Description", "Season")
  SeasonDataTable_list <- rbind(SeasonDataTable_list, SeasonDataTable)
  print(i)
}


  season <- read_html(SeasonsOversheet4[6])
  
  SeaonTables <-  season %>%
    html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
    html_table(header = T)
  
  
  SeasonDataTable_raw<-as.data.frame(SeaonTables)
  SeasonDataTable <- SeasonDataTable_raw[seq(from=1, to=length(SeasonDataTable_raw$Ep), by=2),]
  descriptions <- SeasonDataTable_raw$Ep[seq(from=2, to=length(SeasonDataTable_raw$Ep), by=2)]
  SeasonDataTable$Description <- descriptions
  SeasonDataTable$season <- paste0("Season ", 6)
  SeasonDataTable <- SeasonDataTable[,-5]
  colnames(SeasonDataTable) <- c("Eps", "Title","Writers", "Director", "Original_AirDate", "Description", "Season")
  SeasonDataTable_list <- rbind(SeasonDataTable_list, SeasonDataTable)
  print(i)

  
  for (i in 7:11) {
    season <- read_html(SeasonsOversheet4[i])
    
    SeaonTables <-  season %>%
      html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
      html_table(header = T)
    
    
    SeasonDataTable_raw<-as.data.frame(SeaonTables)
    SeasonDataTable <- SeasonDataTable_raw[seq(from=1, to=length(SeasonDataTable_raw$Ep), by=2),]
    descriptions <- SeasonDataTable_raw$Ep[seq(from=2, to=length(SeasonDataTable_raw$Ep), by=2)]
    SeasonDataTable$Description <- descriptions
    SeasonDataTable$season <- paste0("Season ", i)
    colnames(SeasonDataTable) <- c("Eps", "Title","Writers", "Director", "Original_AirDate", "Description", "Season")
    SeasonDataTable_list <- rbind(SeasonDataTable_list, SeasonDataTable)
    print(i)
  }
