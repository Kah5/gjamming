# intial script to test GJAM on a subset of PLS composition data at the grid cell-level and play around with model devlopment based on our hypothese

# read in composition counts and climate:

pls <- read.csv("/Users/kah/Documents/PLS_products/PLS_species_counts.csv")
pls.climate <- read.csv("/Users/kah/Documents/Bimodality/data/PLS_FIA_density_climate_full.csv")
pls <- merge(pls, pls.climate[,c("x", "y", "cell", "mean_GS_soil","sandpct", "GS_ppet", "MAP1910", "CEC" )], by = c("x", "y", "cell"), all.x=TRUE)
head(pls)
pls <- pls[!is.na(pls$mean_GS_soil),]

xdata <- pls[,4:40]
plsYdata <- pls[,4:39]
xdata <- pls[,c("MAP1910", "GS_ppet", "sandpct")]
rl   <- list(r = 8, N = 20)
# create a modelList object to feed into gjam()
# ng is number of gibbs samples, burnin = burn in period
# typenames is teh typ of data
# note this model has effort = null
#effort = NULL, list containing 'columns', a vector of length <= S giving the names of columns in in y, and 'values', a length-n vector of effort or a n by S matrix (see Examples). effort can be plot area, search time, etc. for discrete count data 'DA

# creat model list
ml   <- list(ng = 2500, burnin = 500, typeNames = 'DA', reductList = rl)

# specify formula (here this is just a test model):
form <- as.formula( ~ MAP1910 + GS_ppet + sandpct )

# model using gjam:  # note that effort = NULL here for initial pls data
out  <- gjam(form, xdata = xdata, ydata = plsYdata, modelList = ml)

specNames <- colnames(plsYdata)
specColor <- c(RColorBrewer::brewer.pal(n = 11, name = "Spectral"),RColorBrewer::brewer.pal(n = 8, name = "Accent"), RColorBrewer::brewer.pal(n = 9,name = "Pastel1"), RColorBrewer::brewer.pal(n = 8,name = "Dark2"))
#specColor <- rep('black',ncol(plsYdata))
#specColor[ c(grep('quer',specNames),grep('cary',specNames)) ] <- 'brown'
#specColor[ c(grep('acer',specNames),grep('frax',specNames)) ] <- 'darkgreen'
#specColor[ c(grep('abie',specNames),grep('pice',specNames)) ] <- 'blue'

pl   <- list(SMALLPLOTS = F, GRIDPLOTS=T, specColor = specColor)
gjamPlot(output = out, plotPars = pl)

saveRDS(out,"initial_GJAM_PLS_8km_test.rds")
