remotes::install_github("nhebda/conpyro")
library(conpyro)

data<-data.frame(
  ws10= c(1:20),
  ffmc= rep(90,20),
    fsg=c(rep(1,5),rep(2,5),rep(3,5),rep(5,5)),
    sfc=rep(1,20),
    cbd=rep(0.2,20),
    dmc=rep(60,20),
    season=rep("summer",20),
    density=rep("moderate",20),
    stand=rep("mixed",20),
    smooth_CFO=rep(TRUE,20)
)

conpyro(
  data = data
)