require(mvtnorm)

# evaluate sample size
evalss <- function(cv, dropout, n, minpp, tarpp, th.stop, th.go, alpha, n.sim, power.shade = 0.8) {
  neval <- floor(n * (1 - dropout))
  mdd <- qnorm(1-alpha)*cv*sqrt(2/neval)
  es80 <- power.t.test(n = neval, sd = cv, sig.level = alpha, alternative = "one.sided", power = power.shade)$delta
  pow <- power.t.test(n = neval, sd = cv, sig.level = alpha, alternative = "one.sided", delta = tarpp)$power
  pow.mpp <- power.t.test(n = neval, sd = cv, sig.level = alpha, alternative = "one.sided", delta = minpp)$power  
  fpr.stop <- fprate(cv = cv, crit = th.stop, npbo = n, nact = n, dropout = dropout, nsim = n.sim)
  fpr.go <- fprate(cv = cv, crit = th.go, npbo = n, nact = n, dropout = dropout, nsim = n.sim)
  fnr.stop <- fnrate(tred = tarpp, cv = cv, crit = th.stop, npbo = n, nact = n, dropout = dropout, nsim = n.sim)
  fnr.go <- fnrate(tred = tarpp, cv = cv, crit = th.go, npbo = n, nact = n, dropout = dropout, nsim = n.sim)
  c(n = n, mdd = mdd, es80 = es80, pow.mpp = pow.mpp, pow = pow,
    fpr.stop = fpr.stop, fnr.stop = fnr.stop, 
    fpr.go = fpr.go, fnr.go = fnr.go)
}

# Simulate false positive rates
fprate <- function(cv, crit, npbo, nact, dropout, nsim = 10000) {
  npbo2 <- floor((1-dropout)*npbo)
  nact2 <- floor((1-dropout)*nact)
  val.pbo <- rnorm(n = nsim, mean = 1, sd = cv/sqrt(npbo2))
  val.act <- rnorm(n = nsim, mean = 1, sd = cv/sqrt(nact2))
  red <- (val.pbo-val.act)/val.pbo
  mean(red >= crit)
}

# Simulate false negative rates
fnrate <- function(tred, cv, crit, npbo, nact, dropout, nsim = 10000) {
  npbo2 <- floor((1-dropout)*npbo)
  nact2 <- floor((1-dropout)*nact)
  val.pbo <- rnorm(n = nsim, mean = 1, sd = cv/sqrt(npbo2))
  val.act <- rnorm(n = nsim, mean = 1 - tred, sd = cv/sqrt(nact2))
  red <- (val.pbo-val.act)/val.pbo
  mean(red < crit)
}

# create table with cell shading
colortable <- function(htmltab, css, style="table-condensed table-bordered"){
  tmp <- str_split(htmltab, "\n")[[1]] 
  CSSid <- gsub("\\{.+", "", css)
  CSSid <- gsub("^[\\s+]|\\s+$", "", CSSid)
  CSSidPaste <- gsub("#", "", CSSid)
  CSSid2 <- paste(" ", CSSid, sep = "")
  ids <- paste0("<td id='", CSSidPaste, "'")
  for (i in 1:length(CSSid)) {
    locations <- grep(CSSid[i], tmp)
    tmp[locations] <- gsub("<td", ids[i], tmp[locations])
    tmp[locations] <- gsub(CSSid2[i], "", tmp[locations], 
                           fixed = TRUE)
  }
  htmltab <- paste(tmp, collapse="\n")
  Encoding(htmltab) <- "UTF-8"
  list(
    tags$style(type="text/css", paste(css, collapse="\n")),
    tags$script(sprintf( 
      '$( "table" ).addClass( "table %s" );', style
    )),
    HTML(htmltab)
  )
}

# compute n from input
compute_n <- function(n.input){

  n.tmp <- unlist(strsplit(isolate(n.input), ','))
  range.ind <- grep(':', n.tmp)
  
  if (length(range.ind) > 0){
    n <- c(as.numeric(n.tmp[-range.ind]))
    
    for( ii in 1:length(range.ind)){
      range.pars <- as.numeric(unlist(strsplit(n.tmp[range.ind[ii]], ':')))
      n <- c(n, seq(range.pars[1], range.pars[2], by = range.pars[3]))
      
    }
  }else{
    n <- as.numeric(unlist(n.tmp))
  }
  
  sort(n)

}

# generate column names
colnames_gen_full <- function(input){
  c('N/Group', 'MDD', 
    paste0('ES', round(isolate(input$power.ctrl)*100) ),
    'Power @ MPP',
    'Power @ TPP',
    paste0('FPR @ Stop (', isolate(input$th.stop), ')'),
    paste0('FNR @ Stop  (', isolate(input$th.stop), ')'),
    paste0('FPR @ Go  (', isolate(input$th.go), ')'),
    paste0('FNR @ Go  (', isolate(input$th.go), ')'))
}

colnames_gen_int <- function(input){
  c('N/Group', 'MDD', 
    paste0('ES', round(isolate(input$power.ctrl)*100) ),
    'Power @ MPP',
    'Power @ TPP',
    paste0('FPR @ Stop (', isolate(input$th.stop.int), ')'),
    paste0('FNR @ Stop  (', isolate(input$th.stop.int), ')'), 
    paste0('FPR @ Go  (', isolate(input$th.go.int), ')'),
    paste0('FNR @ Go  (', isolate(input$th.go.int), ')'))
}

# filter unwanted columns

filter_cols <- function(input, tab){
  include.cols <- c(TRUE, 
                    input$show.mdd == "yes", input$show.es80 == "yes",
                    input$show.mpp.pow == "yes", input$show.tpp.pow == "yes",
                    input$show.fpr.stop == "yes", input$show.fn.stop == "yes",
                    input$show.fpr.go == "yes", input$show.fn.go == "yes")
  
  include.cols <- which(include.cols)
  
  tab[, include.cols]
  
}


# compute overall false positive rate if interim is included
joint_fp_fn <- function(int.corr, cv, cv.int, nsim, effect.size, n.full, n.int, 
                        dropout, dropout.int, th.stop.int, th.go.int, th.stop, th.go, alpha = NULL, comp = "fp"){

  # generate data for subjects observed at interim
  n.int.obs <- floor( n.int * (1 - dropout.int) )
  n.int.eos <- floor( n.int * (1 - dropout) )
  covar <- int.corr * cv * cv.int / (sqrt(n.int.obs * n.int.eos))
  cov.mat <- matrix(c(cv.int^2 / n.int.obs , covar, covar, cv^2 / n.int.eos), nr = 2, nc = 2) 
  
  val.act.int <- rmvnorm(n = nsim, mean = c(1, 1) * (1 - effect.size), sigma = cov.mat)
  val.pbo.int <- rmvnorm(n = nsim, mean = c(1, 1), sigma = cov.mat)
  
  red.int <- (val.pbo.int[, 1] - val.act.int[, 1]) / val.pbo.int[, 1]
  
  # generate data for subjects only observed at eos
  if(n.full > n.int) {
    n.rem <- floor( (n.full - n.int) * (1 - dropout) )
    val.act.rem <- rnorm(n = nsim, mean = (1 - effect.size), sd = cv/sqrt(n.rem))
    val.pbo.rem <- rnorm(n = nsim, mean = 1, sd = cv/sqrt(n.rem))
    
    val.act.full <- (n.rem*val.act.rem + n.int.obs*val.act.int[, 2]) / (n.int.eos + n.rem)
    val.pbo.full <- (n.rem*val.pbo.rem + n.int.obs*val.pbo.int[, 2]) / (n.int.eos + n.rem)
    
    red.full <- (val.pbo.full - val.act.full) / val.pbo.full
  }
  
  if(n.full == n.int) {
    val.pbo.full <- val.pbo.int[, 2]
    val.act.full <- val.act.int[, 2]
    
    red.full <- (val.pbo.full - val.act.full) / val.pbo.full
  }
  
  if(comp == "fp") {
    return( c(mean( (red.int > th.stop.int & red.int < th.go.int & red.full >= th.stop) | red.int >= th.go.int), 
              mean( (red.int > th.stop.int & red.int < th.go.int & red.full >= th.go) | red.int >= th.go.int))
    )
  }
  
  if(comp == "fn") {
    return( c(mean( (red.int > th.stop.int & red.int < th.go.int & red.full <= th.stop) | red.int <= th.stop.int), 
              mean( (red.int > th.stop.int & red.int < th.go.int & red.full <= th.go) | red.int <= th.stop.int)) )
  }
  
  if(comp == "pwr") {
    return( mean ( 
      red.int >= th.go.int | (red.int < th.go.int & red.int >= th.stop.int & (val.pbo.full - val.act.full) >= qnorm(1-alpha/2) * cv * sqrt(2/floor(n.full * (1-dropout))) )
      ))
  }
  
}


