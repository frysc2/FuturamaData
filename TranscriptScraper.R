library(stringr)
library(rvest)

webfile <- "https://futurama.fandom.com/wiki/Futurama_Wiki"
webpage <- read_html( webfile)

###########
#### Getting Futurama Episode list
###########
SeasonsOversheet4 <-  webpage %>%
  
  html_nodes(xpath = "/html/body/div[4]/div[4]/div[2]/header/nav/ul/li[4]/div[2]/ul/li/div/ul/li/a") %>%
  html_attr("href") %>%
  str_trim()

data.frame(SeaonTables_links)

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
  
  
  SeasonDataTable_list$Title <- gsub("[\"/]", "", SeasonDataTable_list$Title)
  
  SeasonDataTable_list$URL <- paste0("https://futurama.fandom.com/wiki/", str_replace_all(SeasonDataTable_list$Title," ", "_"))
  SeasonDataTable_list$URL[55] <- "https://futurama.fandom.com/wiki/The_30%25_Iron_Chef"
###########
#### Transcript Extraction
###########

transcipt_data_full  <- data.frame()
    
for(i in 55:length(SeasonDataTable_list$URL)){
  webfile <- paste0(SeasonDataTable_list$URL[i], "/Transcript")
  webpage <- read_html(webfile)  
  
  Transcript <-  webpage %>%
    html_nodes(xpath = '//*[@id="mw-content-text"]/div') %>%
    html_text()

  transcipt_data<-data.frame(str_split(Transcript, "\n") ) 
  colnames(transcipt_data) <- "raw"
  
  sceneCheck <- data.frame(str_split_fixed(transcipt_data$raw, "[()]", 6))
  colnames(sceneCheck) <- c("Dialog_1", "Scene_1",  "Dialog_2", "Scene_2", "Dialog_3", "Scene_3")
  transcipt_data<- cbind(transcipt_data, sceneCheck)
  
  actionCheck <- data.frame(str_split_fixed(transcipt_data$raw, "[\\[\\]]", 6))
  colnames(actionCheck) <- c("Dialog_1", "action_1",  "Dialog_2", "action_2", "Dialog_3", "action_3")
  transcipt_data<- cbind(transcipt_data, actionCheck)
  
  
  NoScenes <- data.frame(str_split_fixed(transcipt_data$raw, "[\\[\\]()]", 6))
  colnames(NoScenes) <- c("Dialog_1", "action_1",  "Dialog_2", "action_2", "Dialog_3", "action_3")
  transcipt_data$NoScenes <- paste(NoScenes$Dialog_1, NoScenes$Dialog_2, NoScenes$Dialog_3, sep = " ")
  transcipt_data$character<-NA
  transcipt_data$character[grep(":",transcipt_data$NoScenes)] <- str_split_fixed(transcipt_data$NoScenes[grep(":",transcipt_data$NoScenes)], ":", 2)[,1]
  
  table2<-data.frame(table(transcipt_data$character))
  transcipt_data <- transcipt_data[-(1:20),]
  transcipt_data$title <- SeasonDataTable_list$Title[i]
  transcipt_data$eps <- SeasonDataTable_list$Eps[i]
  transcipt_data$season <- SeasonDataTable_list$Season[i]
  
  transcipt_data_full <- rbind(transcipt_data_full, transcipt_data)
  print(SeasonDataTable_list$Title[i])
}
write.csv(SeasonDataTable_list, "~/GitHub/FuturamaData/SeasonDataTable_list.csv", row.names = F)
write.csv(transcipt_data_full, "~/GitHub/FuturamaData/transcipt_data_full.csv", row.names = F)
  
  