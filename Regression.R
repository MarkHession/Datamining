# install.packages("ggplot2")
library('ggplot2')

machine <- read.csv("data/machine.csv")

head(machine)
data(machine)

plot(machine$PRP, machine$ERP)
pairs(machine)

model = lm(CACH ~ CHMIN + CHMAX + PRP., data=machine)
