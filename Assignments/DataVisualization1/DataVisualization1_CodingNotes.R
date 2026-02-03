# Coding notes for Data Visualization 1

#### Loading built in R dataset ####

data("mtcars")
str(mtcars)   # viewing the structure of the dataframe

## Plotting examples using Base R

plot(mtcars$wt, mtcars$mpg)

plot(mtcars$wt, mtcars$mpg,
     xlab = "Car Weight",
     ylab = "Miles per gallon",
     font.lab = 6,
     pch = 23)


## Plotting examples using GGPLOT2

#install.packages("ggplot2")
library(ggplot2)

ggplot()  # blank


ggplot(mtcars, aes(x = wt, y = mpg))  # with axes, no points


ggplot(mtcars, aes(x = wt, y = mpg)) +   # add points
  geom_point()


ggplot(mtcars, aes(x = wt, y = mpg)) +   # add smoothing line
  geom_point() + 
  geom_smooth()


ggplot(mtcars, aes(x = wt, y = mpg)) +  # adding 'lm' method
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) 


ggplot(mtcars, aes(x = wt, y = mpg)) +  # changing the order of the layers
  geom_smooth(method = lm, se = FALSE) +
  geom_point() 


ggplot(mtcars, aes(x = wt, y = mpg)) +   # adding axes labels
  geom_smooth(method = lm, se = FALSE) +
  geom_point() +
  xlab("Weight") + 
  ylab("Miles per gallon")


ggplot(mtcars, aes(x = wt, y = mpg)) +   # scaling point size by weight
  geom_smooth(method = lm, se = FALSE) +
  geom_point(aes(size = cyl, color = hp)) +
  xlab("Weight") + 
  ylab("Miles per gallon")


ggplot(mtcars, aes(x = wt, y = mpg)) +    # adding color gradient
  geom_smooth(method = lm, se = FALSE) +
  geom_point(aes(color = wt)) +
  xlab("Weight") + 
  ylab("Miles per gallon") +
  scale_colour_gradient(low = "blue", high = "black")


ggplot(mtcars, aes(x = wt, y = mpg/100)) +    # adding color gradient
  geom_smooth(method = lm, se = FALSE, color = "grey") +
  geom_point(aes(size = cyl, color = hp)) +
  xlab("log10 Weight") + 
  ylab("Miles per gallon") +
  scale_colour_gradient(low = "green", high = "black")+
  scale_x_log10() +
  scale_y_continuous(label = scales::percent)



#### ggplot with categorical X and Numeric Y variables####

bull.richness <- read.csv(file.choose())

str(bull.richness)

bull.richness.soy.no.till <- bull.richness[bull.richness$Crop == "Soy" & 
                                             bull.richness$Treatment == "No-till",] # subset to soy data



#### boxplots ####

ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide)) + 
  geom_boxplot()


ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide)) + 
  geom_boxplot() + 
  xlab("") + 
  ylab("Fungal richness")


ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide)) + 
  geom_boxplot() + 
  xlab("") + 
  ylab("Fungal richness") +
  geom_point(position = position_jitterdodge(dodge.width=0.9))



#### Bar plots ####

ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide)) + 
  stat_summary(fun=mean,geom="bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar") + 
  xlab("") + 
  ylab("Fungal richness") +
  geom_point(position=position_jitterdodge(dodge.width=0.9)) 


ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide)) + 
  stat_summary(fun=mean,geom="bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") + 
  xlab("") + 
  ylab("Fungal richness")


ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide, fill = Fungicide)) + 
  geom_point(position=position_jitterdodge(dodge.width=0.9)) +
  stat_summary(fun=mean,geom="bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") + 
  xlab("") + 
  ylab("Fungal richness") 


#### Line charts ####

ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide, fill = Fungicide)) + 
  geom_point(position=position_jitterdodge(dodge.width=0.9)) +
  stat_summary(fun=mean,geom="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar") + 
  xlab("") + 
  ylab("Fungal richness") 

# connecting lines
ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, group = Fungicide, color = Fungicide)) + 
  stat_summary(fun=mean,geom="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  ylab("Fungal \n richness") + 
  xlab("") 

# faceting
ggplot(bull.richness, aes(x = GrowthStage, y = richness, group = Fungicide, color = Fungicide)) + 
  stat_summary(fun=mean,geom="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  ylab("Fungal \n richness") + 
  xlab("") +
  facet_wrap(~Treatment)

# multiple faceting
ggplot(bull.richness, aes(x = GrowthStage, y = richness, group = Fungicide, color = Fungicide)) + 
  stat_summary(fun=mean,geom="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  ylab("Fungal \n richness") + 
  xlab("") +
  facet_wrap(~Treatment*Crop)

# scaling multiple faceting
ggplot(bull.richness, aes(x = GrowthStage, y = richness, group = Fungicide, color = Fungicide)) + 
  stat_summary(fun=mean,geom="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  ylab("Fungal \n richness") + 
  xlab("") +
  facet_wrap(~Treatment*Crop, scales = "free")

# changing faceting order
ggplot(bull.richness, aes(x = GrowthStage, y = richness, group = Fungicide, color = Fungicide)) + 
  stat_summary(fun=mean,geom="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  ylab("Fungal \n richness") + 
  xlab("") +
  facet_wrap(~Crop*Treatment, scales = "free")