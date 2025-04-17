# data in reduced
M <- mean(reduced$NU_NOTA_MT)
S <- sd(reduced$NU_NOTA_MT)
ui <- c(0, 1)
dim(ui) <- c(1, 2)
ci <- c(0)
llike = function (dat, k) {
  function (ms) {
    m <- ms[1]
    s <- ms[2]
    -sum(log(dnorm(dat, m, s)))+length(dat)*log(1-pnorm(k, m, s))
  }
}
abled <- by(reduced, reduced$PLACE, function (dat) {
  constrOptim(c(M, S), llike(dat[dat$D_UNION == 0, 'NU_NOTA_MT'], a),
              NULL, ui, ci)
})
abled <- sapply(abled, `[[`, 'par')
disabled <- by(reduced, reduced$PLACE, function (dat) {
  constrOptim(c(M, S), llike(dat[dat$D_UNION == 1, 'NU_NOTA_MT'], a),
              NULL, ui, ci)
})
disabled <- sapply(disabled, `[[`, 'par')
tot <- as.data.frame(cbind(t(abled), t(disabled)))
