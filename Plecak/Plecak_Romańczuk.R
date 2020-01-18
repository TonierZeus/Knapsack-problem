install.packages("GA")
library(GA)
plecakDb = data.frame(
  przedmiot = c("choinka", "bombki", "bombki premium", "karp", "lampki", 
                "stroj mikolaja", "sztuczny snieg", 
                "grzane wino", "prezenty", "oplatek", "barszcz" ),
  wartosc = c(100, 25, 40, 65, 30, 25, 15, 10, 70, 10, 10 ),
  waga = c(40, 15, 20, 20, 30, 15, 25, 30, 20, 10, 15 )
)
plecakLimit = 100
plecakDb

fitnessFunc = function(chr) {
  calkowitaWartoscChr = chr %*% plecakDb$wartosc
  calkowitaWagaChr = chr %*% plecakDb$waga
  if (calkowitaWagaChr > plecakLimit) return(-calkowitaWartoscChr) 
  else return(calkowitaWartoscChr)
}

wyniki=ga(type="binary",nBits=11,fitness=fitnessFunc,popSize=100,
          pcrossover=0.85,pmutation=0.05,elitism=5,maxiter=30,seed=10)
summary(wyniki)
plot(wyniki)

decode=function(chr){
  print("Rozwiązanie: ")
  print( plecakDb[chr == 1, ] )
  print( paste("Waga plecaka =",chr %*% plecakDb$waga) )
  print( paste("Wartość przedmiotów =",chr %*% plecakDb$wartosc) )
}

decode(wyniki@solution[1,])
