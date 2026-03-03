#Crosswalking BC Tree Species Codes to US codes for use in FFI,FUELCALC ETC
# Canadian (BC) → US species crosswalk
species_crosswalk <- function(spp) {
  # accept factor/character, scalar or vector
  spp <- trimws(as.character(spp))
  
  bc_to_us <- c(
    "Ac"  = "BEPA",  # Balsam Poplar →Paper Birch
    "Acb" = "BEPA",  # Balsam Poplar → Paper Birch
    "Act" = "BEPA",  # Black Cottonwood → Paper Birch
    "At"  = "POTR5",  # Trembling Aspen → Aspen
    "Ax"  = "POPSPP", # Poplar Hybrid → Poplar sp.
    "Ba"  = "ABAM",   # Amabilis Fir → Pacific Silver Fir
    "Bg"  = "ABGR",   # Grand Fir → Grand Fir
    "Bl"  = "ABLA",   # Subalpine Fir → Subalpine Fir
    "Bp"  = "ABPR",   # Noble Fir → Noble Fir
    "Cw"  = "THPL",   # Western Redcedar → Western Redcedar
    "Dg"  = "ALRU2",  # Green/Sitka Alder → White Alder
    "Dr"  = "ALRU2",   # Red Alder → Red Alder
    "Xc1" = "ALRHB",  # Mountain Alder → White Alder
    "Ea"  = "BEPA",  # Alaska Paper Birch → Paper Birch
    "Ep"  = "BEPA",  # Paper Birch → Paper Birch
    "Eb"  = "BEPA",  # Paper Birch → Paper Birch
    "Fd"  = "PSME",   # Douglas-fir → Douglas-fir
    "Fdi"  = "PSME",   # Douglas-fir (Interior) → Douglas-fir
    "Fdc"  = "PSME",   # Douglas-fir (Coast) → Douglas-fir
    "Hm"  = "TSME",   # Mountain Hemlock → Mountain Hemlock
    "Hw"  = "TSHE",   # Western Hemlock → Western Hemlock
    "Lt"  = "LAOC",   # Tamarack → Western Larch
    "Lw"  = "LAOC",   # Western Larch → Western Larch
    "Mb"  = "ACMA3",  # Bigleaf Maple → Bigleaf Maple
    "Pa"  = "PIAL",   # Whitebark Pine → Whitebark Pine
    "Pf"  = "PIFL2",  # Limber Pine → Limber Pine
    "Pj"  = "PICO",  # Jack Pine → Lodgepole Pine
    "Pl"  = "PICO",   # Lodgepole Pine → Lodgepole Pine
    "Pli"  = "PICO",   # Lodgepole Pine (Interior) → Lodgepole Pine
    "Pw"  = "PIMO3",   # Western White Pine → Western White Pine
    "Py"  = "PIPO",   # Yellow (Ponderosa) Pine → Ponderosa Pine
    "Sb"  = "PIMA",   # Black Spruce → Black Spruce
    "Sn"  = "PIRU",   # Norway Spruce → Red Spruce
    "Ss"  = "PISI",   # Sitka Spruce → Sitka Spruce
    "Sx"  = "PIEN", # Spruce Hybrid →  Engelmann Spruce
    "Sw"  = "PIGL",   # White Spruce → White Spruce
    "Se"  = "PIEN",   # Engelmann Spruce → Engelmann Spruce
    "Sxs" = "PISI", # Sitka Spruce hybrid → Spruce spp.
    "Yc"  = "CHNO",   # Alaska Yellow-cedar → Alaska Cedar
    "QG"  = "QUKE",  # Garry Oak → California Black Oak
    "Vb"  = "PREM",   # Bitter Cherry → Bitter Cherry
    "T"  = "TABR2", #Yew to Yew
    "Any" = "Any"
  )
  
  out <- unname(bc_to_us[spp])  # NA for unknowns
  # keep original codes for those not in the map
  out[is.na(out)] <- spp[is.na(out)]
  out
}


