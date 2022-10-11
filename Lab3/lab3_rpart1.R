require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
#Tested different options for plot and text
plot(Swiss_rpart, font.main=3, main="Title", sub="Subtitle", cex.main=5, cex.sub=3.0) # try some different plot options
text(Swiss_rpart, pos=3, col = "#1b98e0", font=3) # try some different text options

