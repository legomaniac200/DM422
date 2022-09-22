library(tidyverse)
library(janitor)

input1 <- read.csv("C:/Users/pdkro/OneDrive/Desktop/Fall 2022/Data Mining/Traffic_Violations.csv")
input1$Color[input1$Color == "BLUE, DARK" | input1$Color == "BLUE, LIGHT"] <- "BLUE"
input1$Color[input1$Color == "GREEN, DK" | input1$Color == "GREEN, LGT"] <- "GREEN"

realdata <- subset(input1, Color == "BLACK" | Color == "WHITE" | Color == "GRAY" | Color == "SILVER" | Color == "RED" | Color == "BLUE" | Color == "BROWN" | Color == "GREEN" | Color == "BEIGE" | Color == "ORANGE" | Color == "GOLD" | Color == "YELLOW" | Color == "PURPLE")
#ggplot(data = realdata, mapping = aes(x = Color)) + geom_bar()


#Color
ctab <- tabyl(realdata, Color)
ctab["actual"] <- c(0.004, 0.232, 0.09, 0.014, 0.003,0.153, 0.007, 0.004, 0.001, 0.103, 0.145, 0.239, 0.002)
ctab$diff <- (ctab$percent - ctab$actual)

#count plot
ggplot(data = ctab, mapping = aes(x = reorder(Color, percent), y = percent))+ geom_col() + theme_bw() + labs(x = "Color of vehicle", y = "% that color makes up of total violations")

#Standardized plot
ggplot(data = ctab, mapping = aes(x = reorder(Color, diff), y = diff)) + geom_col() + labs(x= "Car Color", y = "Difference between violation total and actual total (pos means more violations)") + theme_bw()

#Model
mtab <- tabyl(realdata, Model)
mtab <- mtab %>% arrange(desc(n))
mtab <- mtab %>% slice(1:10)
mtab
ggplot(data = mtab, mapping = aes(x = reorder(Model, n), y = n)) + geom_col() + theme_bw() + labs(x = "Car Model", y = "Number of model involved in violations")
