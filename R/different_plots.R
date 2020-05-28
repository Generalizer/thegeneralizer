# install.packages("GGally")
# install.packages("HSAUR")
# install.packages("viridis")
library(GGally)
library(HSAUR)
library(viridis)


?parcoord
gen_out2 <- stratifier(ipeds)
stratifier(ipeds)

generalizer_output$iddata %>%
  select(pct_grant_aid, admit_rate, pct_female, pct_white, clusterID) %>%
  mutate(clusterID = factor(clusterID)) %>%
  ggparcoord(columns = 1:4, groupColumn = 5,
             alphaLines = 0.1) +
  theme_bw()

gen_out2$iddata %>%
  select(pct_female, pct_white, clusterID) %>%
  mutate(clusterID = factor(clusterID)) %>%
  ggparcoord(columns = 1:2, groupColumn = 3,
             alphaLines = 0.1) +
  theme_bw()

plot(silhouette(gen_out2[[1]]$clusters, dist = dist(gen_out2$data)),
     col = 1:4, border = NA)
