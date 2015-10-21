require("Amelia")
data(freetrade)
summary(freetrade)


#Running Amelia on this Free Trade dataset
amelia_out <- amelia(x = freetrade, m = 5, ts="year", cs="country")
