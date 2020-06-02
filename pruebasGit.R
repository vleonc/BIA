library(tidyverse)
x <- as.data.frame(mtcars)

ggplot(x,aes(x=mpg,y=cyl,fill=hp))+
  geom_tile(color = "red", size = 0.5)
  