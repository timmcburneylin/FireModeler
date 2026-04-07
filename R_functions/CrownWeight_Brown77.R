#Function to calculate crown weight in kg from Brown 1977

crown_weight <- function(species, dbh_cm, crown_ratio, h_m = NA) {
  
  # if scalar NA passed for h_m, expand to match length of other inputs
  if (length(h_m) == 1 && is.na(h_m)) {
    h_m <- rep(NA_real_, length(species))
  }
  
  # check lengths
  lengths <- c(length(species), length(dbh_cm), length(crown_ratio), length(h_m))
  if (length(unique(lengths)) != 1) {
    stop("All inputs must be the same length")
  }
  
  # --------------------------------------------------------------------------
  # Species name translation — accepts common names, codes, or variations
  # --------------------------------------------------------------------------
  translate_species <- function(sp) {
    sp_clean <- tolower(trimws(sp))
    
    lookup <- c(
      # codes (pass-through)
      "gf"                    = "GF",
      "l"                     = "L",
      "s"                     = "S",
      "af"                    = "AF",
      "lp"                    = "LP",
      "wp"                    = "WP",
      "wbp"                   = "WBP",
      "c"                     = "C",
      "pp"                    = "PP",
      "df"                    = "DF",
      "wh"                    = "WH",
      
      # grand fir
      "grand fir"             = "GF",
      "grand"                 = "GF",
      "abies grandis"         = "GF",
      
      # western larch
      "larch"                 = "L",
      "western larch"         = "L",
      "larix occidentalis"    = "L",
      
      # engelmann spruce
      "spruce"                = "S",
      "engelmann spruce"      = "S",
      "picea engelmannii"     = "S",
      
      # subalpine fir
      "subalpine fir"         = "AF",
      "fir"                   = "AF",   # generic fir -> subalpine fir
      "alpine fir"            = "AF",
      "abies lasiocarpa"      = "AF",
      
      # lodgepole pine
      "lodgepole"             = "LP",
      "lodgepole pine"        = "LP",
      "pinus contorta"        = "LP",
      
      # western white pine
      "western white pine"    = "WP",
      "white pine"            = "WP",
      "pinus monticola"       = "WP",
      
      # whitebark pine
      "whitebark pine"        = "WBP",
      "whitebark"             = "WBP",
      "pinus albicaulis"      = "WBP",
      
      # western redcedar
      "western redcedar"      = "C",
      "redcedar"              = "C",
      "red cedar"             = "C",
      "western red cedar"     = "C",
      "thuja plicata"         = "C",
      
      # ponderosa pine
      "ponderosa"             = "PP",
      "ponderosa pine"        = "PP",
      "pinus ponderosa"       = "PP",
      
      # douglas-fir
      "douglas-fir"           = "DF",
      "douglas fir"           = "DF",
      "douglasfir"            = "DF",
      "pseudotsuga menziesii" = "DF",
      
      # western hemlock
      "hemlock"               = "WH",
      "western hemlock"       = "WH",
      "tsuga heterophylla"    = "WH"
    )
    
    code <- lookup[sp_clean]
    
    if (is.na(code)) {
      stop(paste0(
        "Species not recognised: '", sp, "'\n",
        "Accepted common names: Lodgepole, Lodgepole Pine, Ponderosa, Ponderosa Pine,\n",
        "  Spruce, Engelmann Spruce, Douglas-fir, Douglas Fir,\n",
        "  Fir, Subalpine Fir, Alpine Fir, Grand Fir,\n",
        "  Hemlock, Western Hemlock, Larch, Western Larch,\n",
        "  Western Redcedar, Red Cedar, Western White Pine, White Pine,\n",
        "  Whitebark Pine, Whitebark\n",
        "Accepted codes: GF, L, S, AF, LP, WP, WBP, C, PP, DF, WH"
      ))
    }
    
    return(unname(code))
  }
  
  # ---- single-tree core function (scalar only) ----
  .crown_weight_single <- function(species, dbh_cm, crown_ratio, h_m) {
    
    # translate to Brown (1977) code
    species <- translate_species(species)
    
    # unit conversions
    d     <- dbh_cm / 2.54
    h     <- if (!is.na(h_m)) h_m * 3.28084 else NA_real_
    R     <- crown_ratio
    c_len <- if (!is.na(h) && !is.na(R)) h * R else NA_real_
    
    # ---- TABLE 1: live crown weight (lbs) ----
    live <- switch(species,
                   "GF"  = exp(1.3094 + 1.6076 * log(d)),
                   "L"   = exp(0.4373 + 1.6786 * log(d)),
                   "S"   = exp(1.0404 + 1.7096 * log(d)),
                   "AF"  = if (!is.na(R)) {
                     1.066 + 0.1862 * (d^2 * R)
                   } else {
                     7.345 + 1.255 * (d^2)
                   },
                   "LP"  = if (!is.na(R)) {
                     0.02238 * (d^3) + 0.1233 * (d^2 * R) - 2.00
                   } else {
                     exp(0.1224 + 1.8820 * log(d))
                   },
                   "WP"  = if (!is.na(R)) {
                     0.09470 * (d^2 * R)
                   } else if (!is.na(h)) {
                     3.65 - 0.04534 * (d^3) + 0.01233 * (d^2 * h)
                   } else {
                     exp(0.7276 + 1.5497 * log(d))
                   },
                   "WBP" = if (!is.na(R)) {
                     0.65 + 0.06056 * (d^3) + 0.05477 * (d^2 * R)
                   } else {
                     0.8371 * (d^2) - 1.00
                   },
                   "C"   = if (!is.na(R)) {
                     exp(1.7273 * log(d) - 2.8086)
                   } else {
                     exp(0.8815 + 1.6389 * log(d))
                   },
                   "PP"  = if (!is.na(R)) {
                     exp(2.2812 * log(d) + 1.5098 * log(R) - 3.0957)
                   } else {
                     exp(2.6280 + 2.0740 * log(d))
                   },
                   "DF"  = if (!is.na(h) && !is.na(R) && d > 15) {
                     27.94 - 0.008695 * (d^2 * h) + 0.02839 * (d^2 * c_len)
                   } else if (d < 17) {
                     exp(1.1368 + 1.5819 * log(d))
                   } else {
                     1.02037 * (d^2) - 20.74
                   },
                   "WH"  = if (!is.na(h) && !is.na(R)) {
                     0.3729 * (d^2) + 0.2840 * (d * c_len) -
                       0.005525 * (d^2 * c_len) - 4.50
                   } else if (!is.na(h)) {
                     3.60 - 1.5450 * (d^2) + 0.01734 * (d^3) + 0.3880 * (d * h)
                   } else {
                     exp(0.7218 + 1.7502 * log(d))
                   }
    )
    
    # ---- TABLE 2: dead crown weight (lbs) ----
    dead <- switch(species,
                   "GF"  = if (d <= 18) {
                     exp(3.5638 * log(d) - 5.3154)
                   } else {
                     0.38 * live
                   },
                   "S"   = exp(3.6172 * log(d) - 6.6860),
                   "AF"  = if (d <= 16) {
                     exp(2.0757 * log(d) - 10.4711)
                   } else {
                     0.31 * live
                   },
                   "LP"  = if (!is.na(R)) {
                     if (d <= 10) {
                       (0.026 * d - 0.025) * live
                     } else {
                       0.235 * live
                     }
                   } else {
                     NA_real_
                   },
                   "WP"  = exp(2.6076 * log(d) - 4.3970),
                   "WBP" = if (!is.na(c_len)) {
                     0.001713 * (d^2 * c_len) + 0.33
                   } else if (!is.na(h)) {
                     0.001397 * (d^2 * h) + 0.28
                   } else {
                     0.06117 * (d^2)
                   },
                   "C"   = 0.01063 * (d^3),
                   "PP"  = exp(2.8376 * log(d) - 3.7598),
                   "DF"  = if (!is.na(c_len)) {
                     7.29 + 0.02768 * (d^3) - 0.006978 * (d^2 * c_len)
                   } else {
                     0.01094 * (d^3)
                   },
                   "WH"  = if (!is.na(h)) {
                     exp(6.0111 * log(d * h) - 2.0496 * log(d) - 19.340)
                   } else {
                     exp(3.3664 * log(d) - 6.6768)
                   },
                   "L"   = NA_real_
    )
    
    # guard negatives
    live <- max(live, 0, na.rm = TRUE)
    dead <- if (!is.na(dead)) max(dead, 0) else NA_real_
    
    # lbs -> kg
    lbs_to_kg <- 0.453592
    live_kg   <- live  * lbs_to_kg
    dead_kg   <- if (!is.na(dead)) dead * lbs_to_kg else NA_real_
    total_kg  <- if (!is.na(dead_kg)) live_kg + dead_kg else live_kg
    
    c(live_kg  = round(live_kg,  3),
      dead_kg  = round(dead_kg,  3),
      total_kg = round(total_kg, 3))
  }
  
  # ---- apply over vectors ----
  out <- mapply(
    .crown_weight_single,
    species, dbh_cm, crown_ratio, h_m,
    SIMPLIFY = FALSE
  )
  
  # bind into tidy data frame
  result <- tibble::tibble(
    species     = species,
    species_code = sapply(species, translate_species),
    dbh_cm      = dbh_cm,
    crown_ratio = crown_ratio,
    height_m    = h_m,
    live_kg     = sapply(out, `[[`, "live_kg"),
    dead_kg     = sapply(out, `[[`, "dead_kg"),
    total_kg    = sapply(out, `[[`, "total_kg")
  )
  
  return(result)
}