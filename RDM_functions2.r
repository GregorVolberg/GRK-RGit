getLowerTri = function(dataIn){
  dataIn[lower.tri(dataIn)]
}

makeDist <- function(dataIn){
  getLowerTri(as.matrix(dist(dataIn)))
}  

# tet=df$brightness
# p= as.matrix(dist(tet))
# p2 = getLowerTri(p)
# k = dataIn[[1]]
# m = k[lower.tri(k)]
# r = row(lower.tri(k)==T);  c = col(lower.tri(k))

# vectorizeRDM = function(dataIn){
#   onlyLower = dataIn[lower.tri(dataIn)] # remove diagonal and off-diagonal
#   vectorizedAndRankTransformed = lapply(onlyLower, rank, na='keep')
# }

getNoiseCeilingKendall = function(dataIn){
  # Nili et al. 2014, page 7
  # Popal, 2019
  dataIn = dataIn[lower.tri(dataIn)] # remove diagonal and off-diagonal
  vectorizedAndRankTransformed = lapply(dataIn, rank, na='keep')
  rankMatrix = simplify2array(vectorizedAndRankTransformed)
  meanRanks  = apply(rankMatrix, 1, mean, na.rm = TRUE)
  kendallCorrelation = cor(meanRanks, 
      rankMatrix,
      use = 'pairwise.complete.obs',
      method = 'kendall')
  upperCeil = max(kendallCorrelation)
  
  # from R documentation:  When there are ties, Kendall's tau_b is computed,      as proposed by Kendall (1945).
}

getAverageRDM = function(dataIn, method = 'mean'){
  switch(method,
  mean  = apply(simplify2array(dataIn), c(1:2), mean, na.rm=T),
  meanz = ,
  meanrank = )
}

vectorizeRDM = function(dataIn){
  simplify2array(lapply(dataIn, as.vector))
}

zRDM = function(dataIn){
  lowerPart = dataIn[lower.tri(dataIn)]
  zMatrix   = (dataIn - mean(lowerPart, na.rm = T)) / sd(lowerPart, na.rm = T)
  diag(zMatrix) = 0
  return(zMatrix)
}

plotRDM = function(dataIn, letterColors = NULL){
  clz = hclust(as.dist(dataIn), method = 'ward.D')
  heatmap.2(dataIn,
            Rowv = as.dendrogram(clz),
            symm = TRUE,
            #          RowSideColors = hex(LUV(as.matrix(SynCol[[1]][11:13]))),
            #          ColSideColors = hex(LUV(as.matrix(SynCol[[1]][11:13]))),
            col = heat.colors(100), 
            colCol = letterColors,
            colRow = letterColors,
            trace = "none",
            density.info = "none",
            keysize = 1.2,
            key.title = NA,
            #key.xlab = "z-score",
            dendrogram = 'none',
            srtRow = 0,
            srtCol = 0,
            main = 'Color Difference') # OK, aber
}

plotMDS = function(dataIn, letterColors = NULL, xyScale = 2.5){
  if (is_empty(letterColors)){
    letterColors = 'black'
  }
  x1 = xyScale*-1
  x2 = xyScale
  y1 = xyScale*-1
  y2 = xyScale
  
  mds = cmdscale(as.dist(dataIn), k = 2, list = T, add=T)
  as_tibble(mds$points) %>%
    set_names(c('x','y')) %>%
    ggplot(aes(x = x, y = y)) + 
    geom_text(aes(label = row.names(mds$points)),
              size = 5,
              col = letterColors) + 
    xlim(x1, x2) + 
    ylim(y1, y2) + 
    xlab('Dimension 1') + 
    ylab('Dimension 2') +
    ggtitle('MDS', 
            subtitle = paste(round(mds$GOF[2]*100, 2),
                             ' % variance explained')) + 
    theme_classic() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
  
}