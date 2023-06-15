library(ggplot2)
library(readxl)
library(png)
library(grid)

chapters <- read_excel("OnePieceData.xlsx")
characters <- read.csv("Characters.csv")
image_fond <- readPNG("One_piece_logo.png")

char_app <- data.frame(char_name = characters$name, appearances = numeric(length(characters$name)))

for (i in 1:nrow(chapters)){
  nams = strsplit(chapters$Characters[i], ",")[[1]]
  for (j in 1:length(nams)){
    name = nams[j]
    char_app$appearances[char_app$char_name == name] <- 1 + char_app$appearances[char_app$char_name == name]
  }
}

most_frequent <- char_app[order(char_app$appearances, decreasing = TRUE),]
top_twenty <- head(most_frequent, 20)
top_twenty$char_name <- reorder(top_twenty$char_name, top_twenty$appearances)

ggplot(data = top_twenty, aes(x = appearances, y = char_name)) +
  annotation_custom(rasterGrob(image_fond,width = unit(1,"npc"),height = unit(1,"npc")),xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf) +
  geom_bar(
    stat = "identity", fill = "#FFE877", color = "black") + 
    labs(x = "Number of chapters in which the character appears", y = "Name of character", title = "Twenty most frequent characters in One Piece")+
  geom_text(aes(label=appearances),hjust=1.5,vjust=0.5,color = "#820000", size = 2.5)
