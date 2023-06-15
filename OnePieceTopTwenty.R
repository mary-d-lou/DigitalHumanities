#loading required libraries
library(ggplot2) #for plotting
library(readxl) #to read Excel files
library(png) #to read PNG images
library(grid) #to customize a plot (rasterGrob)

#reading external files
chapters <- read_excel("OnePieceData.xlsx") #database that tracks which characters appear in each chapter
characters <- read_csv("Characters.csv") #database that contains a list of all character names
background <- readPNG("One_piece_logo.png") #background image

#creating a new table that contains the names of the characters and in how many chapters they appear  
char_app <- data.frame(char_name = characters$name, appearances = numeric(length(characters$name))) #I initialized the number of appearances with zeros for each characacter

#creation of a loop counting in how many chapters each character appears through the manga
for (i in 1:nrow(chapters)){ #for each chapter
  nams = strsplit(chapters$Characters[i], ",")[[1]] #convert to a list (called "nams") of the names of every character in the chapter
  for (j in 1:length(nams)){ #for each character in said list
    name = nams[j] #the variable called "name" takes the name of a character
    char_app$appearances[char_app$char_name == name] <- 1 + char_app$appearances[char_app$char_name == name] #searching for the name of said character in char_app table and adding 1 to his number of appearances 
  }
}

#creating a new table that ranks characters from the one that appears in most chapters to the one who appears in the least
most_frequent <- char_app[order(char_app$appearances, decreasing = TRUE),]
#creating a subtable that only contains the 20 characters that appear the most in the manga
top_twenty <- head(most_frequent, 20)
#rearranging the table so that the graph displays them in descending order
top_twenty$char_name <- reorder(top_twenty$char_name, top_twenty$appearances)

#plotting a graph that shows the 20 most frequent characters and the number of chapters in which each of them appears
ggplot(data = top_twenty, aes(x = appearances, y = char_name)) +
  annotation_custom(rasterGrob(background,width = unit(1,"npc"),height = unit(1,"npc")),xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf) + #setting the background image
  geom_bar(
    stat = "identity", fill = "#FFE877", color = "black") + #customizing the bars
    labs(x = "Number of chapters in which the character appears", y = "Name of character", title = "Twenty most frequent characters in One Piece")+ #displaying the title and axis labels
  geom_text(aes(label=appearances),hjust=1.5,vjust=0.5,color = "#820000", size = 2.5) #displays the number of appearances on each bar of the graph (with customized text size and color that fits the bars)
