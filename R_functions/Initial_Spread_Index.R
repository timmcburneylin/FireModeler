#equation from:https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19973.pdf

initial_spread_index <- function(
    ffmc,
    ws,
    fbpMod = FALSE) {
  # Eq. 10 - Moisture content
  fm <- 147.2 * (101 - ffmc) / (59.5 + ffmc)
  # Eq. 24 - Wind Effect
  # the ifelse, also takes care of the ISI modification for the fbp functions
  # This modification is Equation 53a in FCFDG (1992)
  fW <- ifelse(
    ws >= 40 & fbpMod == TRUE,
    12 * (1 - exp(-0.0818 * (ws - 28))),
    exp(0.05039 * ws)
  )
  # Eq. 25 - Fine Fuel Moisture
  fF <- 91.9 * exp(-0.1386 * fm) * (1 + (fm^5.31) / 49300000)
  # Eq. 26 - Spread Index Equation
  isi <- 0.208 * fW * fF
  return(isi)
}

.ISIcalc <- function(...) {
  .Deprecated("initial_spread_index")
  return(initial_spread_index(...))
}