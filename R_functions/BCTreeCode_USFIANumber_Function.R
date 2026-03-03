
# Crosswalk BC species codes to US NVEL species numbers
bc_to_nvel_species <- function(spp) {
  # Accept factor/character, scalar or vector
  spp <- trimws(as.character(spp))
  
  # BC code → US plant code → NVEL species number
  bc_to_us_code <- c(
    "Ac"  = "BEPA",   # Balsam Poplar → Paper Birch
    "Acb" = "BEPA",   # Balsam Poplar → Paper Birch
    "Act" = "BEPA",   # Black Cottonwood → Paper Birch
    "At"  = "POTR5",  # Trembling Aspen → Aspen
    "Ax"  = "POPSPP", # Poplar Hybrid → Poplar sp.
    "Ba"  = "ABAM",   # Amabilis Fir → Pacific Silver Fir
    "Bg"  = "ABGR",   # Grand Fir → Grand Fir
    "Bl"  = "ABLA",   # Subalpine Fir → Subalpine Fir
    "Bp"  = "ABPR",   # Noble Fir → Noble Fir
    "Cw"  = "THPL",   # Western Redcedar → Western Redcedar
    "Dg"  = "ALRU2",  # Green/Sitka Alder → Red Alder
    "Dr"  = "ALRU2",  # Red Alder → Red Alder
    "Xc1" = "ALRHB",  # Mountain Alder → White Alder
    "Ea"  = "BEPA",   # Alaska Paper Birch → Paper Birch
    "Ep"  = "BEPA",   # Paper Birch → Paper Birch
    "Eb"  = "BEPA",   # Paper Birch → Paper Birch
    "Fd"  = "PSME",   # Douglas-fir → Douglas-fir
    "Fdi" = "PSME",   # Douglas-fir (Interior) → Douglas-fir
    "Fdc" = "PSME",   # Douglas-fir (Coast) → Douglas-fir
    "Hm"  = "TSME",   # Mountain Hemlock → Mountain Hemlock
    "Hw"  = "TSHE",   # Western Hemlock → Western Hemlock
    "Lt"  = "LAOC",   # Tamarack → Western Larch
    "Lw"  = "LAOC",   # Western Larch → Western Larch
    "Mb"  = "ACMA3",  # Bigleaf Maple → Bigleaf Maple
    "Pa"  = "PIAL",   # Whitebark Pine → Whitebark Pine
    "Pf"  = "PIFL2",  # Limber Pine → Limber Pine
    "Pj"  = "PICO",   # Jack Pine → Lodgepole Pine
    "Pl"  = "PICO",   # Lodgepole Pine → Lodgepole Pine
    "Pli" = "PICO",   # Lodgepole Pine (Interior) → Lodgepole Pine
    "Pw"  = "PIMO3",  # Western White Pine → Western White Pine
    "Py"  = "PIPO",   # Yellow (Ponderosa) Pine → Ponderosa Pine
    "Sb"  = "PIMA",   # Black Spruce → Black Spruce
    "Sn"  = "PIRU",   # Norway Spruce → Red Spruce
    "Ss"  = "PISI",   # Sitka Spruce → Sitka Spruce
    "Sx"  = "PIEN",   # Spruce Hybrid → Engelmann Spruce
    "Sw"  = "PIGL",   # White Spruce → White Spruce
    "Se"  = "PIEN",   # Engelmann Spruce → Engelmann Spruce
    "Sxs" = "PISI",   # Sitka Spruce hybrid → Sitka Spruce
    "Yc"  = "CHNO",   # Alaska Yellow-cedar → Alaska Cedar
    "QG"  = "QUKE",   # Garry Oak → California Black Oak
    "Vb"  = "PREM",   # Bitter Cherry → Bitter Cherry
    "T"   = "TABR2"   # Yew → Yew
  )
  
  # US plant code → NVEL species number
  us_to_nvel <- c(
    "BEPA"   = 375,  # Paper birch
    "POTR5"  = 746,  # Quaking aspen
    "POPSPP" = 740,  # Cottonwood (generic)
    "ABAM"   = 011,  # Pacific silver fir (using as proxy for Amabilis)
    "ABGR"   = 017,  # Grand fir
    "ABLA"   = 019,  # Subalpine fir
    "ABPR"   = 022,  # Noble fir
    "THPL"   = 242,  # Western redcedar
    "ALRU2"  = 351,  # Red alder
    "ALRHB"  = 352,  # White alder
    "PSME"   = 202,  # Douglas-fir
    "TSME"   = 264,  # Mountain hemlock
    "TSHE"   = 263,  # Western hemlock
    "LAOC"   = 073,  # Western larch
    "ACMA3"  = 312,  # Bigleaf maple
    "PIAL"   = 101,  # Whitebark pine
    "PIFL2"  = 113,  # Limber pine
    "PICO"   = 108,  # Lodgepole pine
    "PIMO3"  = 119,  # Western white pine
    "PIPO"   = 122,  # Ponderosa pine
    "PIMA"   = 095,  # Black spruce
    "PIRU"   = 097,  # Red spruce
    "PISI"   = 098,  # Sitka spruce
    "PIEN"   = 093,  # Engelmann spruce
    "PIGL"   = 094,  # White spruce
    "CHNO"   = 042,  # Alaska yellow-cedar
    "QUKE"   = 818,  # California black oak
    "PREM"   = 764,  # Bitter cherry
    "TABR2"  = 231   # Pacific yew
  )
  
  # Convert BC codes to US codes
  us_codes <- unname(bc_to_us_code[spp])
  
  # Convert US codes to NVEL species numbers
  nvel_numbers <- unname(us_to_nvel[us_codes])
  
  # Create output data frame
  result <- data.frame(
    bc_code = spp,
    us_code = us_codes,
    nvel_species = nvel_numbers,
    stringsAsFactors = FALSE
  )
  
  # Mark unknown species
  result$nvel_species[is.na(result$nvel_species)] <- 108  # Unknown default lodgepole
  result$us_code[is.na(result$us_code)] <- "UNKNOWN"
  
  return(result)
}
