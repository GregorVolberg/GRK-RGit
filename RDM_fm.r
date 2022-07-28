# RDM felicitas & mariella
library(tidyverse)
library(R.matlab)
library(NbClust)
library(gplots)  # for heatmap.2
library(ggplot2) # for MDS
library(hexbin)

#source('./RDM_functions.R')
source('./RDM_functions2.r')

m    <- readMat('./mRDM.mat')$mRDM;

d    <- as.dist(m) # for MDS and correlation
d2   <- as.matrix(as.dist(m, diag=F, upper=T)) # must be full matrix for heatmap.2
diag(d2) <- NaN

dVec <- vectorizeRDM(d) # for correlation
#mMat <- makeSymRDM(m)
#dMat <- dist(mMat, upper=TRUE, diag=TRUE)

plotMDS2(d, 
        Colors = c(rep('blue', 114), rep('grey10', 114)),
        xyScale = 100)

Colors <- c(rep('blue', 114), rep('grey', 114))
heatmap.2(d2,
          Rowv = TRUE,
          symm = TRUE,
          RowSideColors = Colors,
          ColSideColors = Colors,
          breaks = seq(50, 100, length.out=101), # scale
          col = heat.colors(100), 
          trace = "none",
          density.info = "none",
          keysize = 1.2,
          key.title = NA,
          key.xlab = "Accuracy (%)",
          labRow = '',
          labCol = '',
          dendrogram = 'row',
          srtRow = 0,
          srtCol = 0,
          main = 'Decoding accuracy (106 ms)')

# list by David, sorted according to stimulus id in EEG (1:228)
stmList <- read_delim('./stimList.txt', 
                      col_names = c('nr', 'type', 'fname')) %>%
           mutate(image_id = str_sub(fname, 19)) %>%
           select(nr, type, image_id)

dfc <-    read_csv('./ge_ch_vf.csv')
dfg <-    read_csv('./ge_ge_vf.csv')
df  <-    bind_rows(dfc, dfg) %>%
          select(-1) %>%
          left_join(stmList,., by = 'image_id')


brightness     <- makeDist(df$brightness)
sharpness      <- makeDist(df$sharpness)
contrast       <- makeDist(df$contrast)
colourfulness  <- makeDist(df$colourfulness)
entropy        <- makeDist(df$entropy)
saturation     <- makeDist(df$saturation)
naturalness    <- makeDist(df$naturalness)
ColorHistogram <- df %>% 
                  select(ch1:ch512) %>%
                  makeDist(.)

fm <- as_tibble(data.frame(eeg106 = dVec, brightness, sharpness, contrast,
                colorfuless = colourfulness, entropy, saturation,
                naturalness, ColorHistogram))

saveRDS(fm, 'fm.rds')

# scatter density plot
ggplot(fm, aes(eeg106, brightness)) +
    geom_point()

# packages tidyverse, hexbin
ggplot(fm, aes(eeg106, sharpness)) +
  geom_hex(bins = 30) +
  xlim(40, 100) + 
  ylim(0, 1) + 
    xlab('Decoding accuracy (%)') + 
  ylab('Sharpness') +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

