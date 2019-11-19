# install.packages("ggplot2")
library('ggplot2')

machine <- read.csv("data/machine.csv")

machine$Model.Name=NULL
machine$ï..Vendor.Name= NULL


head(machine)
data(machine)
pairs(machine)



machine
machine.lm= lm(ERP ~ CACH + CHMIN + CHMAX + PRP, data=machine)
summary(machine.lm)
machine.lm$coefficients

newdata1 = data.frame( CACH= 128, CHMIN=8, CHMAX=64, PRP=367)
newdata1
predict(machine.lm,newdata1)
