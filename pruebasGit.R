library(tidyverse)
x <- as.data.frame(mtcars)

ggplot(x,aes(x=mpg,y=cyl,fill=hp))+
  geom_tile(color = "black", size = 0.5)
  