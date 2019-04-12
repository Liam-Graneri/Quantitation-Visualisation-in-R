library("tidyverse")
library("ggplot2")

Quant_Master <- read_csv("2019 IgG Quantitation.csv")
view(Quant_Master)

Quant_NoBlnk <- Quant_Master %>% 
  filter(!is.na(Mouse)) # this with the line above removes the blank slides b/w sections
view(Quant_NoBlnk)
str(Quant_NoBlnk) # Shows information on the dataset in the console

Data_Hippo <- Quant_NoBlnk %>% #the symbol is ctrl-Shift-M
  filter(`Hippo/Cortex` == "H") %>% #this makes sure that only hippocampus is shown in the Hip/Ctx column
  select(Mouse,Measurement,`Study Group`,`Hippo/Cortex`) #this displays the data with only mouse, measurement, and hip/ctx
view(Data_Hippo)

Data_Ctx <- Quant_NoBlnk %>%
  filter(`Hippo/Cortex` == "C") %>%
  select(Mouse,Measurement,`Study Group`,`Hippo/Cortex`)
view(Data_Ctx)

Avg_Hippo <- Data_Hippo %>% #Averages the hippo of each mouse, and displays in parallel columns
  group_by(Mouse,`Study Group`) %>% #Groups by the order of listed variables left to right (then by alphabetical/numerical order)
  summarise(Mean_Hippo = mean(Measurement))
head(Avg_Hippo)

Avg_Ctx <- Data_Ctx %>% 
  group_by(Mouse,`Study Group`) %>% 
  summarise(Mean_Ctx = mean(Measurement))
head(Avg_Ctx)

write_csv(Avg_Hippo,path = "./Avg_Hippo.csv")
write_csv(Avg_Ctx,path = "./Avg_Ctx.csv")

library(hexbin)

ggplot(data = Quant_NoBlnk,mapping = aes(x = Mouse, y = Image)) + geom_point(alpha = 0.3,colour="blue")
count(Quant_NoBlnk, is.na(Image))