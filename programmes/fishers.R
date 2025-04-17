# with cities in cities, data in reduced, info in qest, a set.

abled <- sapply(cities, function (city) {
  m <- qest[city, 1]
  s <- qest[city, 2]
  x <- reduced[reduced$D_UNION == 0 & 
                 reduced$PLACE == city,
               "NU_NOTA_MT"]
  esigma(x, a, m, s)
})

disabled <- sapply(cities, function (city) {
  m <- qest[city, 3]
  s <- qest[city, 4]
  x <- reduced[reduced$D_UNION == 1 & 
                 reduced$PLACE == city,
               "NU_NOTA_MT"]
  esigma(x, a, m, s)
})
