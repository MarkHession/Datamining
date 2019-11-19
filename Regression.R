# install.packages("ggplot2")
library('ggplot2')

machine <- read.csv("data/machine.csv")

machine$Model.Name=NULL
machine$ï..Vendor.Name= NULL
machine$MYCT= NULL
machine$MMIN= NULL
machine$MMAX= NULL


pairs(machine)
head(machine)
data(machine)



#model generation
machine.lm= lm(ERP ~ CACH + CHMIN + CHMAX + PRP, data=machine)
summary(machine.lm)
machine.lm$coefficients

#prediction
newdata1 = data.frame( CACH= 128, CHMIN=8, CHMAX=64, PRP=367)
newdata1
predict(machine.lm,newdata1)

# using values from dataset to compare accuracy
dataCheck = data.frame( CACH= 256, CHMIN=16, CHMAX=128, PRP=198)
dataCheck
predict(machine.lm,dataCheck)
#prediction is 196, dataset is 199, within 90% accuracy
