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
# typenames is the type of data
# note this model has effort = null
#effort = NULL, list containing 'columns', a vector of length <= S giving the names of columns in in y, and 'values', a length-n vector of effort or a n by S matrix (see Examples). effort can be plot area, search time, etc. for discrete count data 'DA

# create model list
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



# some initial model assessment of the 8km simple model:



# make a map of predicted Oak density based on environment:
out <- readRDS("initial_GJAM_PLS_8km_test.rds")
pred.out <- data.frame(out$prediction$ypredMu)
pred.out$x <- pls$x
pred.out$y <- pls$y



ggplot(pred.out, aes(x,y, color = Oak))+geom_point(size = 0.0005)
test.preds <- pred.out
test.preds$Oak_discrete <- ifelse(test.preds$Oak < 1, "0", 
                                  ifelse(test.preds$Oak >= 1 & test.preds$Oak < 25, "1-25",
                                         ifelse(test.preds$Oak >= 25 & test.preds$Oak < 50, "25-50", 
                                                ifelse( test.preds$Oak >= 50 & test.preds$Oak < 100, "50-100", 
                                                        ifelse(test.preds$Oak >= 100 & test.preds$Oak < 200, "100-200", 
                                                               ifelse(test.preds$Oak >= 200 & test.preds$Oak < 400, "200-400",
                                                                      ifelse(test.preds$Oak >= 400, ">400", NA)))))))

ggplot(test.preds, aes(x,y, color = Oak_discrete))+geom_point(size = 0.0005)

obs.out <- data.frame(out$prediction$yPresentMu)
obs.out$x <- trees.spec.clim.eff$x
obs.out$y <- trees.spec.clim.eff$y

test.preds$pred_obs <- test.preds$Oak - obs.out$Oak

p.os <- pred.out-obs.out
p.os$x <- pred.out$x
p.os$y <- pred.out$y

ggplot(p.os, aes(x,y, color = Oak))+geom_point(size = 0.05)+scale_color_gradient2(midpoint = -50)



