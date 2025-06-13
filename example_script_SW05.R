#LECTURE 4 -->  experiments with the "rabbit" dataset - EDA
#we need the MASS library because the dataset is there...
library(MASS)
library(ggplot2)

#you need it only if you use the piping operation, in the next step...
library(tidyverse)

help(Rabbit)

#create a subcollection with the summarized values, 
# Starting from the Rabbit dataset, the %>% is the piping, meaning the output of the command is passed to the next command
# grouping the entries using the treatment (MDL vs. Control) and the dose given, and
# creating a summary using the mean function (so you will have one record/row for each combination of treatment and dose).
Summarized_Rabbit <- Rabbit %>%
  group_by(Treatment, Dose) %>%
  summarise(mean_BPchange = mean(BPchange))
#alternatively, you can do it in distinct operations
t1 <- group_by(Rabbit, Treatment, Dose) 
Summarized_Rabbit <- summarize(t1, mean_BPchange = mean(BPchange))

# plot the summarized values
ggplot(Summarized_Rabbit, aes(x = Dose, y = mean_BPchange, color = Treatment)) +
  geom_line() +
  labs(x="Dose(mcg)", y="Change in blood pressure", color="Treatment") +
  #printing the individual record for each rabbit. N.B: it requires to use the original datasiource, as you lose this data in the aggregation
  geom_point(data = Rabbit, mapping = aes(x = Dose, y = BPchange, shape = Animal, color = Treatment))

ggplot(Summarized_Rabbit, aes(x = Dose, y = mean_BPchange, color = Treatment)) +
  geom_line() +
  labs(x="Dose(mcg)", y="Change in blood pressure", color="Treatment") +
  #printing the individual record for each rabbit. N.B: it requires to use the original datasiource, as you lose this data in the aggregation
  geom_point(data = Rabbit, mapping = aes(x = Dose, y = BPchange, shape = Animal, color = Treatment)) +
  geom_boxplot(data = Rabbit, mapping = aes(x = Dose, y=BPchange, color = Treatment), alpha=0.5)

