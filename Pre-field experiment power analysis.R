library(pwr)


#Power analaysis 
#I should have 70 nests
pwr.t.test(power=0.8, n= 70, sig.level = 0.05)
#d = 0.4768651 so can see medium effect sizes
#For the 


#For the nestlings (counting everyone )


pwr.t.test(power=0.8, d=.4, sig.level = 0.05)
#We only need 100 nestlings to get medium sized effect sizes visible
pwr.t.test(power=0.8, n= 840, sig.level = 0.05)
#d = 0.1367796 can see small effect sizes