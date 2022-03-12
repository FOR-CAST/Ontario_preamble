makeSppEquivON <- function() {
  data("sppEquivalencies_CA", package = "LandR", envir = environment())
  sppEquivalencies_CA[grep("Pin", LandR), `:=`(EN_generic_short = "Pine",
                                               EN_generic_full = "Pine",
                                               Leading = "Pine leading")]

  ## 'ONFRI' used for Ontario forest resource inventory layers
  sppEquivalencies_CA[, ONFRI := c(Abie_bal = "Abie_bal",
                                   Betu_pap = "Betu_pap",
                                   Lari_lar = "Lari_lar",
                                   Pice_gla = "Pice_gla",
                                   Pice_mar = "Pice_mar",
                                   Pinu_ban = "Pinu_ban", #Pinu_res = "Pinu_res", ## TODO: double check Red Pine
                                   Popu_bal = "Popu_bal", Popu_tre = "Popu_tre",
                                   Thuj_occ = "Thuj_spp")[LandR]]

  ## 'ON' used for simulations (i.e., sppEquivCol)
  sppEquivalencies_CA[, ON := c(Abie_bal = "Abie_bal",
                                Betu_pap = "Betu_pap",
                                Lari_lar = "Lari_lar",
                                Pice_gla = "Pice_gla",
                                Pice_mar = "Pice_mar",
                                Pinu_ban = "Pinu_ban", #Pinu_res = "Pinu_res", ## TODO: double check Red Pine
                                Popu_bal = "Popu_sp", Popu_tre = "Popu_sp",
                                Thuj_occ = "Thuj_sp")[LandR]]

  sppEquivalencies_CA[ON == "Abie_sp", EN_generic_full := "Fir"]
  sppEquivalencies_CA[ON == "Abie_sp", EN_generic_short := "Fir"]

  sppEquivalencies_CA[ON == "Betu_sp", EN_generic_full := "Birch"]
  sppEquivalencies_CA[ON == "Betu_sp", EN_generic_short := "Birch"]

  sppEquivalencies_CA[ON == "Lari_lar", EN_generic_full := "Tamarack"]
  sppEquivalencies_CA[ON == "Lari_lar", EN_generic_short := "Tamarack"]

  sppEquivalencies_CA[ON == "Popu_sp", EN_generic_full := "Poplar"]
  sppEquivalencies_CA[ON == "Popu_sp", EN_generic_short := "Poplar"]

  sppEquivalencies_CA[ON == "Thuj_sp", `:=`(EN_generic_full = "Cedar",
                                            EN_generic_short = "Cedar",
                                            LANDIS_traits = "THUJ.SPP.ALL",
                                            Leading = "Cedar leading")]

  sppEquivalencies_CA[!is.na(ON), ]
}
