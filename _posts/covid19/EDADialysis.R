
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(leaflet)

df <- read_csv("data/dialysisdata_edit.csv")

df$`Five Star` <- as.character(df$`Five Star`)

NE_star <- df %>% select(`Five Star`, `Profit or Non-Profit`, State, Zip, County, City) %>% 
  filter(State == "NE") %>% data.frame()
colnames(NE_star) <- gsub('\\.', '_', colnames(NE_star))

dfwide <- dcast(NE_star,Five_Star~Profit_or_Non_Profit, value.var = "Five_Star") 
dflong <-  melt(dfwide, id.vars = "Five_Star") %>% group_by(Five_Star) %>% 
  mutate(percent=round(value/sum(value)*100,2)) %>% data.frame()

ggplot(dflong, aes(fill=variable, y=percent, x=Five_Star)) +
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=value),size=4, position = position_dodge(width = 1)) +
  xlab("Five Star") + ggtitle("Five Star Distribution-Non profit Vs Profit Organization")


colval <<- colorFactor(palette = 'Dark2',domain = df$`Five Star`)

df %>%
  leaflet::leaflet() %>%
  leaflet::addProviderTiles(providers$OpenStreetMap) %>%
  leaflet::addAwesomeMarkers(
    popup = ~paste0(
      "<b>Five Star:</b>", df$`Five Star`,'<br>',
      "<b>State: </b>", df$`Profit or Non-Profit`, "<br>",
      "<b>Zip Code: </b>", "",df$Zip, '<br>',
      "<b>County: </b>", df$County, "<br>",
      "<b>State: </b>", df$State, "<br>",
      ""
    ),  # end popup()
    icon = awesomeIcons(
      library = "ion",
      icon = ifelse(
        test = df$State == "Nebraska",
        yes = "ion-android-star-outline",
        no = "ion-android-radio-button-off"
      ),
      iconColor = "white",
      markerColor = ~colval(`Five Star`)
      # markerColor = ifelse(test = data_ctryC()$p_2018 <= 0.107, yes = "lightgreen", 
      #                      ifelse(test = data_ctryC()$p_2018 <= 0.131, yes = "green",
      #                             ifelse(test = data_ctryC()$p_2018 <= 0.149, yes = "orange",
      #                                    ifelse(test = data_ctryC()$p_2018 <= 0.170, yes = "blue",
      #                                           ifelse(test = data_ctryC()$p_2018 <= 0.195, yes = "purple",no = "red")))))
    ),
    clusterOptions = markerClusterOptions()
  ) %>%   
  leaflet::addMeasure() 