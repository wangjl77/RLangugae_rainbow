#Author: JL WANG <wangjl7799@gmail.com>
#Purpose: The plot represents the top30 men's inernational soccer teams. 
#Each team is rated with three criterion 
#including SPI(Soccer Power Index), OFF(Offensive Ability Index), 
#and DEF(Defensive Ability Index).

#Set the Working Directory
#setwd("D:/Program/RLanguage/")

#install required packages and import libraries
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("grid")
library("ggplot2")
library("dplyr")
library("grid")


#Read the data
soccer_rank <- read.csv("/spi_global_rankings_intl.csv")

#Filter the data to keep the top30 teams and the useful columns
soccer_rank <- filter(soccer_rank, rank <= 30)
soccer_rank <- select(soccer_rank,c("rank","name","off","def","spi")) 

#Copy the spi column for showing the values later
soccer_rank$spi_value <- soccer_rank$spi

#Set different "hjust" and "angle" for each row, the first one is at 90 degree 
#and the order is anticlockwise. 
number_of_team <- nrow(soccer_rank)
angle <- 90 + 360 * (soccer_rank$rank) /number_of_team
soccer_rank$hjust <- ifelse(angle < 270, 1, 0)
soccer_rank$angle <- ifelse(angle >= 270, angle, angle+180)

#This part is for the plot, the code adds 50 to the spi for a bigger plot
p0 <- ggplot(data = soccer_rank, aes(x = rank, y = spi+50)) +
  geom_bar(stat = "identity", fill = rainbow(30, s= 1, alpha = 0.9, start = 0, end = 0.7), 
           color = "#FFFFFF", width = 1.2) + 
  ylim(-20, 170) +
  #Choose the theme for grid, remove all the elements and the background is black
  theme_void() + theme( axis.text = element_blank(),
                        axis.title = element_blank(),
                        panel.grid = element_blank(),
                        plot.background = element_rect(fill = "#121212"),
                        plot.margin = unit(c(0, 0, 0, 0), "cm"),) +
  coord_polar(start = 0, direction = -1, clip = "on")

#Show the "name" column 
p1 <- geom_text(data = soccer_rank, position = "identity", 
            aes(x = rank, y = spi+60, label = name, 
                hjust = hjust, angle = angle),
            size = 3.8, fontface = "bold", color = "white")

#Show the "spi_value" column
p2 <- geom_text(data = soccer_rank, position = "identity", 
            aes(x = rank, y = spi+20, label = spi_value, 
                hjust = hjust, angle = angle),
            size = 3.2, fontface = "bold", color = "white")
  
#Show the "off" column
p3 <- geom_text(data = soccer_rank, position = "identity", 
            aes(x = rank, y = spi-10, 
                label = off, hjust = hjust, angle = angle),
            size = 3.0, fontface="bold")

#Show the "def" column
p4 <- geom_text(data = soccer_rank, position = "identity", 
            aes(x = rank, y = spi-40, 
                label = def, hjust = hjust, angle = angle),
            size = 2.6, fontface="bold", color = "white") 

#Show the plot
p0 + p1 + p2 + p3 + p4

#This part adds the plot title and legend
plot_title = "Men's National Soccer Team Rankings TOP30"
plot_content = "UPDATED AUG. 14, 2020, AT 1:01 AM"
legend = "TEAM RATING LEGEND\n(from outer to inner) :
* SPI: Soccer Power Index 
* OFF: Offensive Ability
* DEF: Defensive Ability"

#Show "plot_title"
grid.text(label = plot_title,x = 0.50,y = 0.97,
          gp = gpar(col = "white", fontsize = 17, 
                    draw = TRUE, font = 2, just = "right"))
#Show "plot_content"
grid.text(label = plot_content, x = 0.66, y = 0.93,
          gp = gpar(col = "white", fontsize = 7, draw = TRUE, just = "right"))
#Show "rectangle"
grid.rect(x = unit(0.69, "npc"), y = unit(0.16, "npc"),
          width = unit(0.14, "npc"), height = unit(0.12, "npc"),
          just = c("left", "top"), hjust = NULL, vjust = NULL,
          default.units = "npc", name = NULL,
          gp = gpar(col="#F0F0F0", alpha = 0.3), draw = TRUE, vp = NULL)
#Show "legend"
grid.text(label = legend,x = 0.70,y = 0.15, just = c("left", "top"),
          gp = gpar(col = "white", fontsize = 6, draw = TRUE,font = 2))
