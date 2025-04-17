library(stringi)

fs15$v15 <- fs15$x + fs15$y
fs16$v16 <- fs16$x + fs16$y
fs17$v17 <- fs17$x + fs17$y
fs18$v18 <- fs18$x + fs18$y
fs19$v19 <- fs19$x + fs19$y
selection <- Reduce(
  intersect,
  lapply(list(fs15, fs16, fs17, fs18, fs19), function (x) {x$Row.names})
)

vars <- as.data.frame(cbind(
  selection,
  fs15[fs15$Row.names %in% selection, "v15"],
  fs16[fs16$Row.names %in% selection, "v16"],
  fs17[fs17$Row.names %in% selection, "v17"],
  fs18[fs18$Row.names %in% selection, "v18"],
  fs19[fs19$Row.names %in% selection, "v19"]
))

names(vars) <- c('PLACE', 'v15', 'v16', 'v17', 'v18', 'v19')

dissimilarity <- raw.dissimilarity[,c(1, 3, 12:8)]
dissimilarity$PLACE <- stri_join(dissimilarity$NO_MUNICIPIO,
                                 dissimilarity$SG_UF,
                                 sep='@')

`15`$m15 <- `15`[1] - `15`[3]
`16`$m16 <- `16`[1] - `16`[3]
`17`$m17 <- `17`[1] - `17`[3]
`18`$m18 <- `18`[1] - `18`[3]
`19`$m19 <- `19`[1] - `19`[3]

xs <- as.data.frame(cbind(
  selection,
  `15`[selection, "m15"],
  `16`[selection, "m16"],
  `17`[selection, "m17"],
  `18`[selection, "m18"],
  `19`[selection, "m19"]
))
names(xs) <- c('PLACE', 'm15', 'm16', 'm17', 'm18', 'm19')

coefsolve <- function (place) {
  X <- simplify2array(c(
    dissimilarity[
      dissimilarity$PLACE == place,
      c("X2015", "X2016", "X2017", "X2018", "X2019")
    ],
    rep(1.0, 5)
  ))
  dim(X) <- c(5, 2)
  sG <- diag(
    as.numeric(simplify2array(vars[vars$PLACE == place,
                        c("v15", "v16", "v17", "v18", "v19")]))^(-0.5)
  )
  U <- sG %*% X
  Y <- simplify2array(xs[xs$PLACE == place,
                         c('m15', 'm16', 'm17', 'm18', 'm19')])
  Z <- sG %*% Y
  beta <- solve(t(U) %*% U, t(U) %*% Z)
  as.vector(beta)
}

coeffs <- as.data.frame(t(sapply(selection, coefsolve)))
names(coeffs) <- c('C', 'D')
