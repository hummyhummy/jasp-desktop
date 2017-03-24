#
# Copyright (C) 2017 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

MLRegressionBoosting <- function (dataset = NULL, options, perform = "run", callback = function(...) 0, ...) {


	# print("in debug mode, march 2017")
	# print(str(options))


	## Read Dataset ## ----

	predictors <- unlist(options[["predictors"]])
	# after unlist, length(predictors) become 1
	predictorsList <- options[["predictors"]]
	target <- options[["target"]]
	indicator <- options[["indicator"]]


	# if (!is.null(target))
	# 	target <- NULL

	# if (!is.null(indicator))
	# 	indicator <- NULL

	variablesToRead <- c(target,indicator,predictors)

	if(is.null(data) && !is.null(target) && !is.null(predictors)){

		if (perform=="run"){

			data <- .readDataSetToEnd(columns = variablesToRead, exclude.na.listwise = NULL)

		} else {

			data <- .readDataSetHeader(columns = variablesToRead)

			}
	}

	# predictors <- predictors[match(.unv(colnames(data)), predictors, nomatch = 0L)]





	## retrieve state ## ----


	# state <- .retrieveState()

	# diff <- NULL

	# if (!is.null(state)) {

	# 	diff <- .diff(options, state$options)

	# }





	## initialize result ## ----

	results <- list()

	meta <- list(
		list(name="title", type="title"),
		list(name="tableRF", type="table"),
		list(name="plotRF", type="image"),
		list(name="plotMarginalPlot", type="image"),
		list(name="plotError", type="image"))

	results[[".meta"]] = meta

	results[["title"]] = "Boosting Regression"




	## Do analysis ## ----
	errorList <- NULL

	if(!is.null(predictors) && !is.null(target)){

		runAnalysis <- TRUE

		# res <- .BoostingAnalysis(data, options, predictors, target, indicator, perform = perform)
		# load gbm object from file
		res <- NULL

	}else{

		runAnalysis <- FALSE

		res <- NULL

	}




	## Create output ## ----





	# if (options[["plotMarginalPlot"]])
	# 	results[["marginalPlot"]] <- .plotMarginalPlot()

	if (options[["tableRF"]])
		results[["tableRF"]] <- .tableRF(res,predictors,perform)

	if (options[["plotRF"]])
		results[["plotRF"]] <- .plotRF(res,predictorsList,perform)

	# if(options[["plotMarginalPlot"]]){

	# 	# one way only
	# 	if(options[["plotMarginalPlotOneWay"]] && !options[["plotMarginalPlotTwoWay"]]){
	# 		results[["plotMarginalPlot"]] <- .plotMarginalOneWay(res,predictorsList,perform)
	# 	}else{
	# 		results[["plotMarginalPlot"]] <- .plotMarginalPlot(res, predictorsList, options, perform)
	# 	}

	# }

	if(options[["plotMarginalPlot"]])
		results[["plotMarginalPlot"]] <- .plotMarginal(res,options,predictorsList,perform)

	if(options[["modelPerforamceShowError"]])
		results[["plotError"]] <- .plotError(res$model, options, perform)


	if (perform == "init"){

		results <- list(results=results, status="inited")
		return(results)
	}else{

		results <- list(results=results, status="complete")
		return(results)

	}

}



.BoostingAnalysis <- function(data, options, predictors, target, indicator, perform = perform){

	# setup random seed

	if (options[["seedBox"]] == "auto_seed")
		options[["value_seed"]] <- 1

	set.seed(as.numeric(options[["value_seed"]]))

	# #shuffle data set, since subsample process is not randomized
	# shuffleIdx <- sample(1:nrow(data),nrow(data))
	# data <- data[shuffleIdx,]

	# Split data according to applied Indicator
	# if(is.null(indicator)){
	# 	fitData <- data
	# }else{
	# 	fitData <- data[as.logical(data[,2]),]
	# 	appData <- data[!as.logical(data[,2]),]
	# }
	 fitData <- data

	# claim target, predictors and formula
	predictors = list(predictors)
	formula = as.formula(paste(target,"~",paste(predictors,collapse=" + ")))

	# check whether options have contradicted with one another, assigned value to parameters
	if (!options[["methodCV"]])
		options[["value_numberOfFold"]] <- 0



	# setup subsample ratio
	if (options[["OOBBox"]]=="auto_subsample")
		options[["value_subsampleRatio"]] <- 0.5

	# setup tree structure
	if (options[["TreeStructureBox"]] == "auto_TS"){
		options[["value_depthOfTree"]] <- 1
		options[["value_MinTermNodeSide"]] <- 5
	}else if (options[["TreeStructureBox"]] == "optimized_TS"){
		optStruc <- .findBestTreeStrucure(data,options)
		options[["value_depthOfTree"]] <- optStruc[1]
		options[["value_MinTermNodeSide"]] <- optStruc[2]
	}

	# setup No. cores
	if (options[["coreBox"]] == "auto_numberOfCore")
		options[["value_numberOfCore"]] <- 1

	# # data pre-process : target type check(no string for bernoulli in gbm)
	# if(options[["distribution"]] == "bernoulli" & is.factor(data[,1])){
	# 	className <- levels(data[,1])
	# 	# targetDF <- grep(options$target,colnames(data))
	# 	data[,1] <- as.character(data[,1])
	# 	data[!is.na(data[,1])&data[,1]==className[1],1]="1"
	# 	data[!is.na(data[,1])&data[,1]==className[2],1]="0"
	# 	data[,1] <- as.numeric(data[,1])
	# }

	# setup training set ratio
	if (options[["testBox"]]=="auto_percentageTrain"){

		options[["value_percentageTrain"]] <- 0.5

		model = gbm::gbm(
		formula = formula,
		data = fitData,
		distribution = options[["distribution"]],
		n.trees=options[["numberOfIterations"]],
		shrinkage=options[["Shrinkage"]],
		interaction.depth=options[["value_depthOfTree"]],
		bag.fraction = options[["value_subsampleRatio"]],
		train.fraction = options[["value_percentageTrain"]],
		n.minobsinnode = options[["value_MinTermNodeSide"]],
		cv.folds = options[["value_numberOfFold"]],
		keep.data=TRUE,
		verbose=FALSE,
		n.cores=options[["value_numberOfCore"]])

	}else if(options[["testBox"]]=="manual_numberTrain"){
		model = gbm::gbm(
		formula = formula,
		data = fitData,
		distribution = options[["distribution"]],
		n.trees=options[["numberOfIterations"]],
		shrinkage=options[["Shrinkage"]],
		interaction.depth=options[["value_depthOfTree"]],
		bag.fraction = options[["value_subsampleRatio"]],
		nTrain = options[["value_numberTrain"]],
		n.minobsinnode = options[["value_MinTermNodeSide"]],
		cv.folds = options[["value_numberOfFold"]],
		keep.data=TRUE,
		verbose=FALSE,
		n.cores=options[["value_numberOfCore"]])

	}else{
		model = gbm::gbm(
		formula = formula,
		data = fitData,
		distribution = options[["distribution"]],
		n.trees=options[["numberOfIterations"]],
		shrinkage=options[["Shrinkage"]],
		interaction.depth=options[["value_depthOfTree"]],
		bag.fraction = options[["value_subsampleRatio"]],
		train.fraction = options[["value_percentageTrain"]],
		n.minobsinnode = options[["value_MinTermNodeSide"]],
		cv.folds = options[["value_numberOfFold"]],
		keep.data=TRUE,
		verbose=FALSE,
		n.cores=options[["value_numberOfCore"]])
	}


	#make prediction
	if (options[["methodCV"]]){
		best.iter = gbm::gbm.perf(model,plot.it = FALSE,method="cv")
		best.iter_cv = best.iter
	}else{
		best.iter = gbm::gbm.perf(model,plot.it = FALSE,method="test")
		best.iter_cv = NULL
	}

	best.iter_test = gbm::gbm.perf(model,plot.it = FALSE,method="test")
	best.iter_oob = gbm::gbm.perf(model,plot.it = FALSE,method="OOB")

	if(!is.null(appData)){
		applicationPred = gbm::predict(
			model,
			newdata=appData,
			n.trees=best.iter)
	}else{
		applicationPred = NULL
	}


	return(list(model=model, best.iter = best.iter, best.iter_cv = best.iter_cv, best.iter_test = best.iter_test, best.iter_oob = best.iter_oob, applicationPred = applicationPred))

}

.varRF <- function(res){
	demo = TRUE
	if(!is.null(res)){

		table <- gbm::summary(res$model,
			n.trees=res$best.iter,
			plotit=FALSE,
			order=TRUE,
			method=relative.influence,
			normalize=TRUE
		)
	}else if (demo){
		table <- cbind(c("lstat","rm","dis","crim","age","ptratio","nox","black","tax","indus","rad","chas","zn"),
						c(38.50827704,33.16983264,8.20578448,4.02457263,3.75283362,3.66434619,3.51862823,2.34334977,1.60811012,0.48025373,0.46373763,0.18753937,0.07273454)
				)
		rownames(table) <- table[,1]
		}else{
			return
		}

	test <- chisq.test(as.numeric(table[,2]))
	table_output <- cbind(table[,2],test$residuals)
	chisqTestInfo <- c(test$statistic,test$parameter,test$p.value)
	rowNames <- rownames(table)

	return(list(table=table_output, rowNames = rowNames, chisqTestInfo = chisqTestInfo))

}

.tableRF <- function(res,predictors,perform){


	table <- list(title = "Relative Importance of Variables")
	colNames = c("relative influence","chisq residuals")

	# if(any(perform == "init", is.null(res), is.null(predictors))){
	if(any(perform == "init", is.null(predictors))){

		toTable <- matrix(".", nrow = 1, ncol = 2,
				  dimnames = list(".", colNames))

	}else{

		varRF <- .varRF(res)
		toTable <- varRF$table
		colnames(toTable) <- colNames
		rownames(toTable) <- varRF$rowNames
	}

	table[["schema"]] <- list(
	fields = list(list(name="case", title="", type="string"),
				  list(name = colNames[1], title = colNames[1], type="number", format="dp:3"),#sf:4;dp:3
				  list(name = colNames[2], title = colNames[2], type="number", format="dp:3"))
	)

	table[["data"]] <- .MLRFTables(toTable)

	# # add footnote
	# footnotes <- .newFootnotes()
	# if()


	return(table)
}


.MLRFTables <- function(x) {

	n = nrow(x)
	m = ncol(x)

	fieldNames = c("case", colnames(x))
	rmns = rownames(x)

	emptyRow <- vector("list", length = length(fieldNames))
	names(emptyRow) <- fieldNames

	data <- rep(list(emptyRow), n)

	if (is.numeric(x)) { # implies .clean must be used
		for (i in seq_len(n)) {

			data[[i]] <- c(case = rmns[i], lapply(x[i, ], .clean))

		}
	} else {
		for (i in seq_len(n)) {

			data[[i]] <- c(case = rmns[i], x[i, ])

		}
	}

	return(data)

}


.plotRF <- function(res,predictors,perform){
	demo =TRUE

	len = length(predictors)

	if (len < 6){
		width <- 300
		height <- 300
	}else{
		width <- len*50
		height <- len*50
	}

	plotRF <- list(
		title = "Relative Importance of Variables",
		width = width,
		height = height
		# custom = list(width = "plotWidth", height = "plotHeight"),
	)

	if(perform == "run"){
		if (!is.null(res)){
			varRF <- gbm::summary(res$model,
				n.trees=res$best.iter,
				plotit=FALSE,
				order=TRUE,
				method=relative.influence,
				normalize=TRUE
			)
		}else if (demo){
			varRF <- cbind(c("lstat","rm","dis","crim","age","ptratio","nox","black","tax","indus","rad","chas","zn"),
				c(38.50827704,33.16983264,8.20578448,4.02457263,3.75283362,3.66434619,3.51862823,2.34334977,1.60811012,0.48025373,0.46373763,0.18753937,0.07273454)
			)
		}

		toPlot <- data.frame(
			Feature = varRF[,1],
			Importance = as.numeric(varRF[,2])
			)

		# toPlot <- toPlot[order(toPlot[["Importance"]], decreasing = TRUE), ]
		axisLimits <- range(pretty(toPlot[["Importance"]]))
		axisLimits[1] <- min(c(0, axisLimits[1]))
		axisLimits[2] <- max(c(0, axisLimits[2]))

		image <- .beginSaveImage(width = plotRF[["width"]], height = plotRF[["height"]])
		barplot(toPlot[[2]], names.arg = toPlot[[1]], horiz = TRUE, xlim = c(axisLimits[1],axisLimits[2]), xlab = "Relative importance")
		content <- .endSaveImage(image)

		plotRF[["data"]] <- content

	}

	return(plotRF)
}

.plotError <- function(model, options, perform){
	demo = TRUE
	yName = list(bernoulli="Bernoulli deviance", multinomial="Multinomial deviance", gaussian="Squared error loss", laplace="Absolute loss", tdist="t-distribution loss", huberized = "Huberized hinge loss", adaboost="AdaBoost
						exponential loss", possion="Absolute loss")
	# ylab = yName[[options$distribution]]
	ylab = "SSE"

	plot <- list(
		title = "Error v.s. iteration",
		width = 600,
		height = 400
		# custom = list(width = "plotWidth", height = "plotHeight"),
	)

	legendName <- c()
	legendColor <- c()
	yUpper <- 0

	if(perform == "run"){
		if(!is.null(model)){	
			train.error <- model$train.error
			valid.error <- model$valid.error
			cv.error <- model$cv.error
			n.trees <- model$n.trees
		}else if(demo){

			dir = getwd()
			setwd("/Users/qiaodan/Documents/internship in JASP/GBT/GBT")
			train.error <-  dget("train.error")
			valid.error <-  dget("test.error")
			cv.error <-  dget("cv.error")
			n.trees <- 5000
			setwd(dir)

		}

		if(options[["methodCV"]]){
			yUpper <- max(train.error,valid.error,cv.error)
		}else{
			yUpper <- max(train.error,valid.error)
		}
		yUpper = round(yUpper)+1

		image <- .beginSaveImage(width = plot[["width"]], height = plot[["height"]])
		plot(1,type="n",ylab=ylab,xlab='Iteration',ylim=c(-1,yUpper),xlim=c(0,n.trees),main="error v.s. iteration")
		# train error
		lines(seq(1,n.trees,1),train.error,col=1)
		# best.Iter <- gbm::gbm.perf(model,method="OOB",plot.it=FALSE)
		# abline(v=best.Iter,col=1, lty=2)
		# legendName <- c(legendName,sprintf("OOB: %d",best.Iter))
		legendName <- c(legendName,sprintf("OOB"))
		legendColor <- c(legendColor,1)

		# test error
		lines(seq(1,n.trees,1),valid.error,col=2)
		# best.Iter <- gbm::gbm.perf(model,method="test",plot.it=FALSE)
		# abline(v=best.Iter,col=2, lty=2)
		# legendName <- c(legendName,sprintf("test: %d",best.Iter))
		legendName <- c(legendName,sprintf("test"))
		legendColor <- c(legendColor,2)

		# cv error
		if (options[["methodCV"]]){
			lines(seq(1,n.trees,1),cv.error,col=3)
			# best.Iter <- gbm::gbm.perf(model,method="cv",plot.it=FALSE)
			# abline(v=best.Iter,col=3, lty=2)
			# legendName <- c(legendName,sprintf("cv: %d",best.Iter))
			legendName <- c(legendName,sprintf("cv"))
			legendColor <- c(legendColor,3)
		}

		legend("topright",legendName, lty=c(1,1,1),col = legendColor)

		plot[["data"]] <- .endSaveImage(image)
	}

	return(plot)


}

.marginalPlotSize <- function(predictors, options){

	len <- length(predictors)
	if(options[["plotMarginalPlotOneWay"]] && !options[["plotMarginalPlotTwoWay"]]){
		width <- 300
		height <- 300*len
	}else{
		if (len < 3){
			width <- 600
			height <- 600
		}else{
			width <- 100*len
			height <- 100*len
		}

	}

	return(list(width = width, height = height))


}

.plotMarginalPlot <- function(res, predictors, options, perform){
	demo = TRUE

	dir = getwd()
	setwd("/Users/qiaodan/Documents/internship in JASP/GBT")
	oneWayGridAll <- dget("oneWayGridAll")
	setwd(dir)

	len <- length(predictors)

	size <- .marginalPlotSize(predictors, options)

	plot <- list(
		title = "Marginal Plot",
		width = size$wight,
		height = size$height
	)

	if (perform == "run" && length(options$predictors) > 0 ){


		# check whether predictors are plotable


		cexText <- 1.6

		image <- .beginSaveImage(plot[["width"]], plot[["height"]])

		cexText <- 1.3

		par(mfrow= c(l, l), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(0.2, 2.2, 2, 0))

		for (row in seq_len(len)){

			for (col in seq_len(len)){

				if (row == col){

					if (options[["plotMarginalPlotOneWay"]]){

						if(!is.null(res)){
							returnGrid <- plot(res$model,i.var=row,n.trees=res$best.iter,return.grid=TRUE)
						}else if (demo){
							returnGrid <- oneWayGridAll[row]
						}

						yLower = round(min(returnGrid[,2]))
						yUpper = round(max(returnGrid[,2]))+1
						xMin = round(min(returnGrid[,1]))
						xMax = round(max(returnGrid[,1]))+1

						plot(returnGrid[,1],returnGrid[,2], type= "l", ylim=c(yLower,yUpper),xlim=c(xMin,xMax),cex.lab=2)

					} else{

						plot(1, type= "n", axes= FALSE, ylab="", xlab="")

					}

					if (row < col){

						if (options[["plotMarginalPlotTwoWay"]]){

							if(!is.null(res)){
								partial <- plot(res$model,i.var=c(row,col),return.grid = T)
							}else if (demo){
								partial <- partialAll[row,col]
							}

							.plotMarginalTwoWay(partial,row,col)

						}else{

							plot(1, type= "n", axes= FALSE, ylab="", xlab="")
						}
					}

					if (row > col){

						if (options[["plotHStatistics"]]){

							plot(1, type= "n", axes= FALSE, ylab="", xlab="")
						}else{
							plot(1, type= "n", axes= FALSE, ylab="", xlab="")
						}
					}
				}
			}
		}



		content <- .endSaveImage(image)

		plot[["data"]]  <- content

	}

	return(plot)

}



.plotMarginalOneWay2 <- function(res,predictors,perform){

	demo = TRUE

	dir = getwd()
	setwd("/Users/qiaodan/Documents/internship in JASP/GBT")
	oneWayGridAll <- dget("oneWayGridAll")
	setwd(dir)

	# size <- .marginalPlotSize(predictors, options)
	len <- length(predictors)

	plot <- list(
		title = "Marginal Plot",
		width = 400,
		height =  400*len
	)

	if(perform == "run"){

		# image <- .beginSaveImage(width = plot[["width"]], height = plot[["height"]])
		image <- .beginSaveImage(width = 400, height = 4000)

		cexText <- 1.3

		par(mfrow= c(len, 1), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(0.2, 2.2, 2, 0))

		if (!is.null(res)){
			for (i in seq_len(len)){
				returnGrid <- plot(res$model,i.var=i,n.trees=res$best.iter,return.grid=TRUE)

				yLower = round(min(returnGrid[,2]))
				yUpper = round(max(returnGrid[,2]))+1
				xMin = round(min(returnGrid[,1]))
				xMax = round(max(returnGrid[,1]))+1

				plot(returnGrid[,1],returnGrid[,2], type= "l", xlab=predictors[i],main=predictors[i],ylim=c(yLower,yUpper),xlim=c(xMin,xMax),cex.lab=2)
			}
		}else if (demo){
			returnGridAll <- oneWayGridAll
			for (i in seq_len(len)){
				returnGrid <- returnGridAll[i]

				yLower = round(min(returnGrid[,2]))
				yUpper = round(max(returnGrid[,2]))+1
				xMin = round(min(returnGrid[,1]))
				xMax = round(max(returnGrid[,1]))+1

				plot(returnGrid[,1],returnGrid[,2], type= "l", xlab=predictors[i],main=predictors[i],ylim=c(yLower,yUpper),xlim=c(xMin,xMax),cex.lab=2)
			}
		}
	}


	content <- .endSaveImage(image)

	plot[["data"]] <- content


	return(plot)


	# if (varIdx == 1){
	# 	# multiple class level
	# 	if (length(returnGrid[,2])>length(returnGrid[,1])){
	# 		className <- colnames(returnGrid[,2])
	# 		plot(1, type= "n", axes= TRUE, ylab=varName, xlab="",main=varName,ylim=c(yLower,yUpper),xlim=c(xMin,xMax),cex.lab=2)
	# 		for (i in 1:length(className)){
	# 			lines(returnGrid[,1],returnGrid[,2][,i],col=i)
	# 		}
	# 	}else{
	# 	# regression
	# 		plot(returnGrid[,1],returnGrid[,2], type= "l", xlab=varName,main=varName,ylim=c(yLower,yUpper),xlim=c(xMin,xMax),cex.lab=2)
	# 	}
	# }else{
	# 	if (length(returnGrid[,2])>length(returnGrid[,1])){
	# 		className <- colnames(returnGrid[,2])
	# 		plot(1, type= "n", axes= TRUE, ylab="", xlab=varName,ylim=c(yLower,yUpper),xlim=c(xMin,xMax),cex.lab=2)
	# 		for (i in 1:length(className)){
	# 			lines(returnGrid[,1],returnGrid[,2][,i],col=i)
	# 		}
	# 	}else{
	# 	# regression
	# 		plot(returnGrid[,1],returnGrid[,2], type= "l", xlab="",ylab="",ylim=c(yLower,yUpper),xlim=c(xMin,xMax),cex.lab=2)
	# 	}

	# }
}


.plotMarginalTwoWay2 <- function(partial,varIdx1,varIdx2){
	# partial <- plot(model,i.var=c(varIdx1,varIdx2),return.grid = T)
	mat <- reshape2::acast(data = partial, 
		formula = as.formula(paste(colnames(partial)[1],"~",colnames(partial)[2])), 
		value.var = colnames(partial)[3])
	if (varIdx1 == 1){
		persp(x = as.numeric(colnames(mat)), 
			y = as.numeric(rownames(mat)), 
			z=mat,
			zlab = "",#'Partial dependence', 
			xlab = colnames(partial)[1],#, 
			ylab = colnames(partial)[2],#, 
			main = colnames(partial)[2],
			theta = 30,
			ticktype = "detailed")
	}else{
		persp(x = as.numeric(colnames(mat)), 
			y = as.numeric(rownames(mat)), 
			z=mat,
			zlab = "",#'Partial dependence', 
			xlab = colnames(partial)[1],#, 
			ylab = colnames(partial)[2],#, 
			theta = 30,
			ticktype = "detailed")
	}
}





.plotHstatistics2 <- function(HValueAll,perfMethod,colVarIdx,rowVarIdx,rowVarName){
	#arrange position
	if (length(perfMethod)==1){
		y_pos <- 1
	}
	if (length(perfMethod)==2){
		y_pos <- c(0.9,1.1)
	}
	if (length(perfMethod)==3){
		y_pos <- c(0.8,1,1.2)
	}

	# regression: H statistics is a single value
	if (colVarIdx==1){
		plot(1, type= "n", axes= FALSE, ylab=rowVarName, xlab="",cex.lab=2)
	}else{
		plot(1, type= "n", axes= FALSE, ylab="", xlab="")
	}
	if (length(HValueAll)==length(perfMethod)){
		for (i in 1:length(perfMethod)){
			text(1,y_pos[i],labels=sprintf("%s: %0.2f",perfMethod[i],HValueAll[i]),cex=2)
		}
	}else{
		# multi-class classification: H statistics is a vector
		classNum = length(HValueAll)/length(perfMethod)
		x_pos <- seq(0,classNum*0.2,0.2)+1-median(seq(0,classNum*0.2,0.2))
		for (i in 1:classNum){
			for (j in 1:length(perfMethod)){
				idx = j+length(perfMethod)*(i-1)
				if (i == 1){
					text(x_pos[i],y_pos[j],labels=sprintf("%s: %0.2f",perfMethod[j],HValueAll[idx]),cex=2)
				}else{
					text(x_pos[i],y_pos[j],labels=sprintf("%0.2f",HValueAll[idx]),cex=2)
					}
			}
			text(x_pos[i],y_pos[j]+0.2,labels=colnames(HValueAll)[i],cex=2,col=i)
		}
	}
}


# Partial dependent plots, maybe later redraw this in subplot
.plotMarginal <- function(res,options,predictors,perform){
	demo = TRUE
	varNum = length(predictors)
	# determine which best.iter to be used: "cv" better than "test" better than "OOB"
	# if ("cv" %in% options$perfMethod){
	# 	best.iter = gbm::gbm.perf(model,method="cv",plot.it=FALSE)
	# }else{
	# 	if ("test" %in% options$perfMethod){
	# 		best.iter = gbm::gbm.perf(model,method="test",plot.it=FALSE)
	# 	}else{
	# 		best.iter = gbm::gbm.perf(model,method="OOB",plot.it=FALSE)
	# 	}
	# }

	if(!is.null(res)){
		best.iter <- res$best.iter
	}else if (demo){
		best.iter <- 80
	}

	len <- length(predictors)
	if(options[["plotMarginalPlotOneWay"]] && !options[["plotMarginalPlotTwoWay"]]){
		width <- 400
		height <- 400*len
	}else{
		if (len < 3){
			width <- 600
			height <- 600
		}else{
			width <- 300*len
			height <- 300*len
		}

	}

	plot <- list(
		title = "Marginal plot",
		width = width,
		height = height
		# custom = list(width = "plotWidth", height = "plotHeight"),
	)

	if(perform == "run"){
		image <- .beginSaveImage(width = plot[["width"]], height = plot[["height"]])
		# 1 way partial dependent plot only
		if(options[["plotMarginalPlotOneWay"]] && !options[["plotMarginalPlotTwoWay"]]){
			# pdf(file="plotMarginal.pdf",width=5,height=5*varNum)
			# image <- .beginSaveImage(width = plot[["width"]], height = plot[["height"]])

			par(mfrow=c(varNum,1))
			for (i in 1:varNum){
				if(!is.null(res)){
					grid <- plot(res$model,i.var=i,n.trees=best.iter,return.grid=TRUE)
				}else if (demo){
					dir = getwd()
					setwd("/Users/qiaodan/Documents/internship in JASP/GBT/GBT")
					grid <- dget(predictors[i])
					setwd(dir)
				}
				.plotMarginalOneWay(grid,i,predictors[i],withHeader = TRUE)
			}
		}else if(options[["plotMarginalPlotTwoWay"]]){
			# image <- .beginSaveImage(width = plot[["width"]], height = plot[["height"]])
			# pdf(file="plotMarginal.pdf",width=5*varNum,height=5*varNum)
			# png(file="plotMarginal.png",width=500*varNum,height=500*varNum)
			# dev.new(width=5*varNum, height=5*varNum)
			# options(repr.plot.width=10*varNum, repr.plot.height=10*varNum,repr.plot.pointsize=120,repr.plot.quality=100)
			par(mfrow= c(varNum, varNum), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(0.2, 2.2, 2, 0))
			for (row in 1:varNum){
				for (col in 1:varNum){
					if (row==col){
						if(options[["plotMarginalPlotOneWay"]]){
							if (!is.null(res)){
									grid <- plot(res$model,i.var=row,n.trees=best.iter,return.grid=TRUE)
								}else if (demo){
									dir = getwd()
									setwd("/Users/qiaodan/Documents/internship in JASP/GBT/GBT")
									grid <- dget(predictors[row])
									setwd(dir)
								}
							.plotMarginalOneWay(grid,row,predictors[row],withHeader = FALSE)
						}else{
							plot(1, type= "n", axes= FALSE, ylab="", xlab="")
						}
					}
					if(col>row){
						if(!is.null(res)){
							grid <- plot(res$model,i.var=c(row,col),return.grid = T)
						}else if(demo){
							dir = getwd()
							setwd("/Users/qiaodan/Documents/internship in JASP/GBT/GBT")
							fileName = paste(predictors[row],predictors[col])
							grid <- dget(fileName)
							setwd(dir)
						}
						.plotMarginalTwoWay(grid,row,col)
					}

					if(col<row){
						if(options[["plotHStatistics"]]){
							if(!is.null(res)){
								HValue <- gbm::interact.gbm(res$model,
									res$data,
									i.var = c(row,col),
									n.trees = res$best.iter
								)
							}else if (demo){
								dir = getwd()
								setwd("/Users/qiaodan/Documents/internship in JASP/GBT/GBT")
								# fileName = paste(predictors[row],"H",predictors[col])
								fileName2 = paste(predictors[row],"H",predictors[col])
								HValue <- dget(fileName2)
								setwd(dir)
								# sHValue = 0.0836838010375261
							}

							plot(1, type= "n", axes= FALSE, ylab="", xlab="")
							text(1,1,labels=sprintf("%0.2f",HValue),cex=2)

						}else{

							plot(1, type= "n", axes= FALSE, ylab="", xlab="")
						}
					}

				}
			}
		}

		if( options[["plotMarginalPlotTwoWay"]] || options[["plotHStatistics"]]){
			if (len > 2 || ((len == 2 && options[["plotMarginalPlotOneWay"]]) || (len == 2 && options[["plotHStatistics"]]))) {

				textpos <- seq(1/(len*2), (len*2-1)/(len*2), 2/(len*2))

				if (! options[["plotMarginalPlotOneWay"]] && !options[["plotHStatistics"]]) {

						for (t in seq_along(textpos)) {

							mtext(text = predictors[t], side = 3, outer = TRUE, at= textpos[t], cex=1.5, line= -0.8)

							if (t < length(textpos)) {

								mtext(text = predictors[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.5, line= -0.1, las= 0)

							}
						}

				} else {

					for (t in seq_along(textpos)) {

							mtext(text = predictors[t], side = 3, outer = TRUE, at= textpos[t], cex=1.5, line= -0.8)
							mtext(text = predictors[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.5, line= -0.1, las= 0)
					}
				}
			}
		}
		plot[["data"]] <- .endSaveImage(image)
	}



	return (plot)

}

.plotMarginalOneWay <- function(returnGrid,varIdx,varName,withHeader){
	yLower = round(min(returnGrid[,2]))
	yUpper = round(max(returnGrid[,2]))+1
	xMin = round(min(returnGrid[,1]))
	xMax = round(max(returnGrid[,1]))+1

	main = ""
	if (withHeader)
		main = varName

	if (varIdx == 1){
		# multiple class level
		if (length(returnGrid[,2])>length(returnGrid[,1])){
			className <- colnames(returnGrid[,2])
			plot(1, type= "n", axes= TRUE, ylab=varName, xlab="",main=main,ylim=c(yLower,yUpper),xlim=c(xMin,xMax),cex.lab=2)
			for (i in 1:length(className)){
				lines(returnGrid[,1],returnGrid[,2][,i],col=i)
			}
		}else{
		# regression
			plot(returnGrid[,1],returnGrid[,2], type= "l", ylab= "", xlab="",main=main,ylim=c(yLower,yUpper),xlim=c(xMin,xMax),cex.lab=3)
		}
	}else{
		if (length(returnGrid[,2])>length(returnGrid[,1])){
			className <- colnames(returnGrid[,2])
			plot(1, type= "n", axes= TRUE, ylab="", xlab="",main=main,ylim=c(yLower,yUpper),xlim=c(xMin,xMax),cex.lab=3)
			for (i in 1:length(className)){
				lines(returnGrid[,1],returnGrid[,2][,i],col=i)
			}
		}else{
		# regression
			plot(returnGrid[,1],returnGrid[,2], type= "l", xlab="",ylab="",main=main,ylim=c(yLower,yUpper),xlim=c(xMin,xMax),cex.lab=3)
		}

	}
}

.plotMarginalTwoWay <- function(partial,varIdx1,varIdx2){
	# partial <- plot(model,i.var=c(varIdx1,varIdx2),return.grid = T)
	mat <- reshape2::acast(data = partial, 
		formula = as.formula(paste(colnames(partial)[1],"~",colnames(partial)[2])), 
		value.var = colnames(partial)[3])
	if (varIdx1 == 1){
		persp(x = as.numeric(colnames(mat)), 
			y = as.numeric(rownames(mat)), 
			z=mat,
			zlab = "",#'Partial dependence', 
			xlab = "",#colnames(partial)[1]
			ylab = "",# colnames(partial)[2]
			main = "",
			theta = 30,
			ticktype = "detailed")
	}else{
		persp(x = as.numeric(colnames(mat)), 
			y = as.numeric(rownames(mat)), 
			z=mat,
			zlab = "",#'Partial dependence', 
			xlab = "",#colnames(partial)[1]
			ylab = "",#colnames(partial)[2]
			theta = 30,
			ticktype = "detailed")
	}
}

.plotHstatistics <- function(HValueAll,colVarIdx,rowVarIdx){
	# #arrange position
	# 	if (length(perfMethod)==1){
	# 		y_pos <- 1
	# 	}
	# 	if (length(perfMethod)==2){
	# 		y_pos <- c(0.9,1.1)
	# 	}
	# 	if (length(perfMethod)==3){
	# 		y_pos <- c(0.8,1,1.2)
	# 	}

	# regression: H statistics is a single value
	plot(1, type= "n", axes= FALSE, ylab="", xlab="")
	text(1,1,labels=sprintf("%0.2f",HValueAll),cex=2)
	# if (colVarIdx==1){
	# 	plot(1, type= "n", axes= FALSE, ylab=rowVarName, xlab="",cex.lab=2)
	# }else{
	# 	plot(1, type= "n", axes= FALSE, ylab="", xlab="")
	# }
	# if (length(HValueAll)==length(perfMethod)){
	# 	for (i in 1:length(perfMethod)){
	# 		text(1,y_pos[i],labels=sprintf("%s: %0.2f",perfMethod[i],HValueAll[i]),cex=2)
	# 	}
	# }else{
	# 	# multi-class classification: H statistics is a vector
	# 	classNum = length(HValueAll)/length(perfMethod)
	# 	x_pos <- seq(0,classNum*0.2,0.2)+1-median(seq(0,classNum*0.2,0.2))
	# 	for (i in 1:classNum){
	# 		for (j in 1:length(perfMethod)){
	# 			idx = j+length(perfMethod)*(i-1)
	# 			if (i == 1){
	# 				text(x_pos[i],y_pos[j],labels=sprintf("%s: %0.2f",perfMethod[j],HValueAll[idx]),cex=2)
	# 			}else{
	# 				text(x_pos[i],y_pos[j],labels=sprintf("%0.2f",HValueAll[idx]),cex=2)
	# 				}
	# 		}
	# 		text(x_pos[i],y_pos[j]+0.2,labels=colnames(HValueAll)[i],cex=2,col=i)
	# 	}
	# }
}




