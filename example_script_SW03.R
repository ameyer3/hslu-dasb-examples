library(ggplot2)
?mpg
View(mpg)

table(mpg$year)

plot <- ggplot(data = mpg, mapping = aes(x = mpg$hwy)) + 
  geom_histogram() + 
  facet_wrap(~year)

(plot + facet_grid(cyl~drv))
  
  
plot <- ggplot(data = mpg, mapping = aes(x = mpg$hwy))  
plot <- ggplot(mpg, aes(x = mpg$hwy))
plot <- ggplot(mapping = aes(x = mpg$hwy),data = mpg)
plot + geom_violin(mapping = aes(y = mpg$displ),alpha=0.5) +
  geom_rug(mapping = aes(y = mpg$displ)) +
  scale_x_discrete()+scale_y_discrete()

plot <- plot + geom_violin(mapping = aes(y = mpg$displ),alpha=0.5)
plot

ggsave()



View(diamonds)

ggplot(diamonds, aes(x=carat, y=price)) + 
  geom_rug() +
  geom_point(aes(color=cut)) +
  scale_color_manual(values=c("red", "red", "red", "blue", "blue")) + 
  # scale_color_manual(values=c("red", "yellow", "green", "blue", "violet")) +  
  scale_y_continuous(breaks=c(0,2500,5000,7500,10000,12500,15000,17500),
                     labels=c(0,2.5,5,7.5,10,12.5,15,17.5),
                     name="price(thousands of dollars)")

# From diamonds {ggplot2} documentation:
#color --> diamond colour, from D (best) to J (worst)
# clarity --> a measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_hex() +
  scale_color_manual(values=c("red", "yellow", "green", "blue", "violet")) + 
  scale_y_continuous(breaks=c(0,2500,5000,7500,10000,12500,15000,17500),
                     labels=c(0,2.5,5,7.5,10,12.5,15,17.5),
                     name="price(thousands of dollars)")

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_bin2d() +
  scale_color_manual(values=c("red", "yellow", "green", "blue", "violet")) + 
  scale_y_continuous(breaks=c(0,2500,5000,7500,10000,12500,15000,17500),
                     labels=c(0,2.5,5,7.5,10,12.5,15,17.5),
                     name="price(thousands of dollars)")

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_bin2d() +
  facet_wrap(~color) +
  scale_color_manual(values=c("red", "yellow", "green", "blue", "violet")) + 
  scale_y_continuous(breaks=c(0,2500,5000,7500,10000,12500,15000,17500),
                     labels=c(0,2.5,5,7.5,10,12.5,15,17.5),
                     name="price(thousands of dollars)")

ggplot(diamonds, aes(x=carat, y=price, fill=cut)) + 
  geom_bin2d() +
  facet_wrap(~color) +
  scale_color_manual(values=c("red", "red", "yellow", "green", "green")) + 
  scale_y_continuous(breaks=c(0,2500,5000,7500,10000,12500,15000,17500),
                     labels=c(0,2.5,5,7.5,10,12.5,15,17.5),
                     name="price(thousands of dollars)")

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_density2d() +
  facet_grid(clarity~color) +
  scale_color_manual(values=c("red", "yellow", "green", "blue", "violet")) + 
  scale_y_continuous(breaks=c(0,2500,5000,7500,10000,12500,15000,17500),
                     labels=c(0,2.5,5,7.5,10,12.5,15,17.5),
                     name="price(thousands of dollars)")

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_point() +
  facet_grid(clarity~color) +
  scale_color_manual(values=c("red", "yellow", "green", "blue", "violet")) + 
  scale_y_continuous(breaks=c(0,5000,10000,15000,17500),
                     labels=c(0,5,10,15,17.5),
                     name="price(thousands of dollars)")
