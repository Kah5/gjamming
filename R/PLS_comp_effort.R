# intial script to test GJAM on a subset of PLS composition data (plus effort), and explore  model devlopment based on our hypothese
library(gjam)
library(dplyr)
library(tidyr)

# read in pls point-level data with environmental data:
mw.clim <- read.csv("/Users/kah/Documents/gjamming/data/point_climate.csv")

unique(mw.clim$state)
mw.clim <- mw.clim[mw.clim$state %in% "IN",]
# get species level counts for each point:
tree1 <- mw.clim[, c("x", "y", "L3_tree1", "dist1", "dist2", "state")]
tree2 <- mw.clim[, c("x", "y", "L3_tree2", "dist1", "dist2", "state")]
colnames(tree2)<- c("x", "y", "L3_tree1", "dist1", "dist2", "state")

trees <- rbind(tree1, tree2)
trees.obj <- trees %>% group_by(x,y) %>% summarize(effort = max(rbind(dist1, dist2)))
trees.spec <- trees %>% group_by(x,y, L3_tree1) %>% summarise(number = n()) %>% spread(key = L3_tree1, value = number)

trees.spec[is.na(trees.spec)]<- 0
plsYdata <- trees.spec[,3:35]

trees.spec.clim <- merge(mw.clim[,c("x","y","total_pr", "ksat", "Mean_tm")], trees.spec, by = c("x", "y"))
trees.spec.clim.eff <- merge(trees.spec.clim, trees.obj, by = c("x", "y"))

trees.spec.clim.eff <- trees.spec.clim.eff[!is.na(trees.spec.clim.eff$effort),]
trees.spec.clim.eff <- trees.spec.clim.eff[!is.na(trees.spec.clim.eff$total_pr),]
trees.spec.clim.eff <- trees.spec.clim.eff[!is.na(trees.spec.clim.eff$Mean_tm),]
trees.spec.clim.eff <- trees.spec.clim.eff[!is.na(trees.spec.clim.eff$ksat),]

xdata <-trees.spec.clim.eff[,c("total_pr", "ksat", "Mean_tm")]
plsYdata <- trees.spec.clim.eff[,6:38]
plsYdata <- plsYdata[!names(plsYdata) %in% "No tree"]
effort <- trees.spec.clim.eff$effort
rl   <- list(r = 4, N = 10)
# create a modelList object to feed into gjam()
# ng is number of gibbs samples, burnin = burn in period
# typenames is teh typ of data
# note this model has effort = null
#effort = NULL, list containing 'columns', a vector of length <= S giving the names of columns in in y, and 'values', a length-n vector of effort or a n by S matrix (see Examples). effort can be plot area, search time, etc. for discrete count data 'DA

# creat model list with no effort
ml   <- list(ng = 2500, burnin = 500, typeNames = 'DA', reductList = rl)

# specify formula (here this is just a test model):
form <- as.formula( ~ total_pr + ksat + Mean_tm )

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

saveRDS(out,"initial_GJAM_PLS_in_point_test.rds")


# running the model with different effort level at the point data:
effort.vals <- trees.spec.clim.eff$effort
effort <- list(columns = length(xdata), values = effort.vals)
ml   <- list(ng = 2500, burnin = 500, typeNames = 'DA', reductList = rl, effort = effort)

# specify formula (here this is just a test model):
form <- as.formula( ~ total_pr + ksat + Mean_tm )

# model using gjam:  # note that effort = NULL here for initial pls data
out.eff  <- gjam(form, xdata = xdata, ydata = plsYdata, modelList = ml)

specNames <- colnames(plsYdata)
specColor <- c(RColorBrewer::brewer.pal(n = 11, name = "Spectral"),RColorBrewer::brewer.pal(n = 8, name = "Accent"), RColorBrewer::brewer.pal(n = 9,name = "Pastel1"), RColorBrewer::brewer.pal(n = 8,name = "Dark2"))
#specColor <- rep('black',ncol(plsYdata))
#specColor[ c(grep('quer',specNames),grep('cary',specNames)) ] <- 'brown'
#specColor[ c(grep('acer',specNames),grep('frax',specNames)) ] <- 'darkgreen'
#specColor[ c(grep('abie',specNames),grep('pice',specNames)) ] <- 'blue'

pl   <- list(SMALLPLOTS = F, GRIDPLOTS=T, specColor = specColor)
gjamPlot(output = out.eff)

saveRDS(out.eff,"initial_GJAM_PLS_in_point_test_effort.rds")



