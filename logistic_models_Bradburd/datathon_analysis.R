################################################################
################################################################
#	script for running datathon analyses
################################################################
################################################################

library(rstan)

################################
#	compile rstan models
################################

stan.model <- scan("models/modelBlocks/datathon_model.txt",what=character(),sep="\n")
dtMod <- stan_model(model_code=stan.model)

################################
#	function library
################################

# plot trace of parameter marginal distribution
plotTrace <- function(fit,chainCols=c("blue","red","forestgreen","goldenrod1")){
	par(mfrow=c(2,1),mar=c(4,4,2,0.5))
	post <- Reduce("cbind",get_logposterior(fit))
	beta <- extract(fit,"beta[1]",inc_warmup=TRUE,permute=FALSE)
	matplot(post,type='l',col=chainCols,lty=1,xlab="",ylab="posterior probability")
	matplot(beta[,,1],type='l',col=chainCols,lty=1,xlab="mcmc iterations",ylab="param value")
}

# plot parameter marginal posterior distribution
plotMarginal <- function(param,predictor,col){
	parDens <- density(param)
		plot(0,
			 main = sprintf("beta (effect of %s)",predictor),
			 xlim=range(c(0, parDens$x)),ylim=range(parDens$y),
			 xlab="",cex.main=2,cex.axis=1.5,cex.lab=1.5,type='n',
			 ylab="Density")
			polygon(x=c(0,parDens$x,0),
					y=c(0,parDens$y,0),
					col=adjustcolor(col,0.5))
			abline(v=0,lwd=4,lty=2,col="red")
			demarcateCIs(param,parDens)
	legend(x="topright",col=c("black","red"),lty=c(1,2),lwd=3,legend=c("95% CI","Zero"),cex=1.5,bg="white")
}

# indicate where the 95% CIs are on a marginal posterior plot
demarcateCIs <- function(z,z.dens){
	CI <- quantile(z,c(0.025,0.975))
	segments(x0=CI[1],x1=CI[1],
			 y0=0,y1=max(z.dens$y)/2,
			 col="black",lwd=4)
	segments(x0=CI[2],x1=CI[2],
			 y0=0,y1=max(z.dens$y)/2,
			 col="black",lwd=4)
}

# plot raw data along with model fit
#	options for: 
#		the posterior distribution of model fits
#		the maximum a posterior (MAP) model fit	
plotModelFit <- function(dataBlock,fit,showMAP=TRUE,showPost=TRUE,...){
	beta1 <- c(extract(fit,"beta[1]",inc_warmup=TRUE,permute=FALSE))
	mu <- c(extract(fit,"mu",inc_warmup=TRUE,permute=FALSE))
	post <- unlist(get_logposterior(fit))
	X <- dataBlock$X
	plot(X,dataBlock$Y,pch=19,cex=0.5,col=adjustcolor(1,0.1),cex.main=2,cex.axis=1.5,cex.lab=1.5,...)
		if(showPost){
			sampledIter <- sample(1:length(post),50)
			invisible(
				lapply(sampledIter,function(i){
					sampledBeta <- beta1[i]
					sampledMu <- mu[i]
					sampledLine <- 1/(1+exp(-(sampledMu + sampledBeta*X)))
					lines(X[order(X)],sampledLine[order(X)],col=adjustcolor("blue",0.1),lwd=1)
				}))
		}
		if(showMAP){
			best <- which.max(post)
			bestbeta <- beta1[best]
			bestmu <- mu[best]
			bestLine <- 1/(1+exp(-(bestmu+bestbeta*X)))
			lines(X[order(X)],bestLine[order(X)],col="blue",lwd=3)
		}
		
}

################################
#	create a label key for 
#		figure readability
################################


label_key <- data.frame(metadata.df1 = c("hasMIDdatathonpublication", "hasMIDdatathonderivedGeneticDataX", 
                            "hasMIDdatathonpreservative", "hasMIDdatathonpermitInformation",
                            "hasMIDdatathonyearCollected", "hasMIDdatathonenvironmentalMedium", 
                            "hasMIDdatathonhabitat", "hasMIDdatathoncountry", 
                            "hasMIDdatathoncoordinates", "hasMIDdatathonlocality", 
                            "hasMIDdatathonmaterialSampleID", "hasMIDspatiotemp"),
           metadata.df3and4 = c("datathonpublication", "datathonderivedGeneticDataX", 
                                "datathonpreservative", "datathonpermitInformation", 
                                "datathonyearCollected", "datathonenvironmentalMedium", 
                                "datathonhabitat", "datathoncountry", 
                                "datathoncoordinates", "datathonlocality", 
                                "datathonmaterialSampleID", "datathonspatiotemp"),
           metadata_labels = c("Publication DOI", "Derived genetic data",
                               "Preservative used", "Permit ID",
                               "Collection year", "Enviro. medium",
                               "Habitat", "Country", 
                               "Lat./long.", "Place name",
                               "Sample ID", "Spatiotemporal"))

################################
#	ANALYSIS 1:
#	analyze effect of bioproject age on probability of 
#		metadata for each category of metadata
################################

load("logistic_models_Bradburd/dataObjs/df1.Robj")
#str(df.1)

responseVars <- names(df.1)[grep("has",names(df.1))]
varNames <- label_key[match(responseVars,label_key[,1]),3]
betaTable <- matrix(NA,nrow=length(responseVars),ncol=4)
row.names(betaTable) <- varNames
colnames(betaTable) <- c("mean","median","95% CI","p-value")

for(i in 1:length(responseVars)){
	dataBlock <- list("N" = nrow(df.1),
					  "X" = matrix(as.numeric(df.1$registration_date_bioprj_julianCont),nrow=1,ncol=nrow(df.1)),
					  "nX" = 1,
					  "Y" = ifelse(df.1[[grep("has",names(df.1))[i]]],1,0))

	fit <- sampling(dtMod,
					data=dataBlock,
					iter=2000,
					thin=2000/500,
					save_warmup=FALSE,
					chains=4)

	b <- c(extract(fit,"beta[1]",inc_warmup=FALSE,permute=FALSE))
	betaTable[i,1] <- round(mean(b),3)
	betaTable[i,2] <- round(median(b),3)
	ci <- round(quantile(b,c(0.025,0.975)),3)
	p <- ecdf(b)
	betaTable[i,3] <- paste0(c("(",ci[1]," - ",ci[2],")"),collapse="")
	betaTable[i,4] <- round(1-2*abs(0.5-p(0)),3)

	pdf(file=sprintf("models/figures/analysis1/trace_year_on_%s.pdf",responseVars[i]),width=7,height=8)
		plotTrace(fit)
	dev.off()
	pdf(file=sprintf("models/figures/analysis1/year_on_%s.pdf",responseVars[i]),width=10,height=6)
		layout(matrix(c(rep(1,6),rep(2,4)),nrow=2,ncol=5))
			plotModelFit(dataBlock,fit,showMAP=TRUE,showPost=TRUE,main=sprintf("Effect of BioProject Age on p(metadata)\n[metadata = %s]",varNames[i]),xlab="age of dataset (years)",ylab="p(metadata)")
			plotMarginal(extract(fit,"beta[1]",inc_warmup=FALSE,permute=FALSE),predictor="dataset age",col="blue")
	dev.off()
}

write.table(betaTable,file="models/analysis1_betaTable.csv")

################################
#	ANALYSIS 2:
#	analyze effect of year on probability of 
#		author response when we contacted them
################################

load("logistic_models_Bradburd/dataObjs/df2.Robj")
#str(df.2)

betaTable <- matrix(NA,nrow=1,ncol=4)
colnames(betaTable) <- c("mean","median","95% CI","p-value")

dataBlock <- list("N" = nrow(df.2),
				  "X" = matrix(as.numeric(df.2$registration_date_bioprj_julianCont),nrow=1,ncol=nrow(df.2)),
				  "nX" = 1,
				  "Y" = as.numeric(as.vector(df.2$author_response)))

fit <- sampling(dtMod,
				data=dataBlock,
				iter=2000,
				thin=2000/500,
				save_warmup=FALSE,
				chains=4)

b <- c(extract(fit,"beta[1]",inc_warmup=FALSE,permute=FALSE))
betaTable[1,1] <- round(mean(b),3)
betaTable[1,2] <- round(median(b),3)
ci <- round(quantile(b,c(0.025,0.975)),3)
p <- ecdf(b)
betaTable[1,3] <- paste0(c("(",ci[1]," - ",ci[2],")"),collapse="")
betaTable[1,4] <- round(1-2*abs(0.5-p(0)),3)

pdf(file="models/figures/analysis2/trace_year_on_authorResponse.pdf",width=7,height=8)
	plotTrace(fit)
dev.off()
pdf(file="models/figures/analysis2/year_on_authorResponse.pdf",width=10,height=6)
	layout(matrix(c(rep(1,6),rep(2,4)),nrow=2,ncol=5))
		plotModelFit(dataBlock,fit,showMAP=TRUE,showPost=TRUE,main="Effect of BioProject Age on Author Response",xlab="age of BioProject (years)",ylab=" p(response | contact attempted)")
		plotMarginal(extract(fit,"beta[1]",inc_warmup=FALSE,permute=FALSE),predictor="dataset age",col="blue")
dev.off()

write.table(betaTable,file="models/analysis2_betaTable.csv")

################################
#	ANALYSIS 3:
#	analyze effect of year on probability of 
#		recovering ANY metadata (for each category of metadata)
#		for datasets for which authors responded to our request
################################

load("logistic_models_Bradburd/dataObjs/df3.Robj")
#str(df.3)

responseVars <- names(df.3)[grep("datathon",names(df.3))]
varNames <- label_key[match(responseVars,label_key[,2]),3]
betaTable <- matrix(NA,nrow=length(responseVars),ncol=4)
row.names(betaTable) <- responseVars
colnames(betaTable) <- c("mean","median","95% CI","p-value")

for(i in 1:length(responseVars)){
	y <- as.numeric(as.vector(df.3[[responseVars[i]]]))
	dataBlock <- list("N" = length(which(!is.na(y))),
					  "X" = matrix(
					  			as.numeric(df.3$registration_date_bioprj_julianCont[which(!is.na(y))]),
					  			nrow=1,
					  			ncol=length(which(!is.na(y)))),
					  "nX" = 1,
					  "Y" = y[which(!is.na(y))])

	fit <- sampling(dtMod,
					data=dataBlock,
					iter=2000,
					thin=2000/500,
					save_warmup=FALSE,
					chains=4)

	b <- c(extract(fit,"beta[1]",inc_warmup=FALSE,permute=FALSE))
	betaTable[i,1] <- round(mean(b),3)
	betaTable[i,2] <- round(median(b),3)
	ci <- round(quantile(b,c(0.025,0.975)),3)
	p <- ecdf(b)
	betaTable[i,3] <- paste0(c("(",ci[1]," - ",ci[2],")"),collapse="")
	betaTable[i,4] <- round(1-2*abs(0.5-p(0)),3)

	pdf(file=sprintf("models/figures/analysis3/trace_year_on_%s.pdf",responseVars[i]),width=7,height=8)
		plotTrace(fit)
	dev.off()
	pdf(file=sprintf("models/figures/analysis3/year_on_%s.pdf",responseVars[i]),width=10,height=6)
		layout(matrix(c(rep(1,6),rep(2,4)),nrow=2,ncol=5))
			plotModelFit(dataBlock,fit,showMAP=TRUE,showPost=TRUE,main=sprintf("Effect of BioProject Age on p(metadata)\n[metadata = %s]", varNames[i]),xlab="age of dataset (years)",ylab="p(partial metadata provided | author response)")
			plotMarginal(extract(fit,"beta[1]",inc_warmup=FALSE,permute=FALSE),predictor="dataset age",col="blue")
	dev.off()
}

write.table(betaTable,file="models/analysis3_betaTable.csv")

################################
#	ANALYSIS 4:
#	analyze effect of year on probability of 
#		recovering ALL metadata (for each category of metadata)
#		for datasets for which authors responded to our request
################################

load("logistic_models_Bradburd/dataObjs/df4.Robj")
#str(df.4)

responseVars <- names(df.4)[grep("datathon",names(df.4))]
varNames <- label_key[match(responseVars,label_key[,2]),3]
betaTable <- matrix(NA,nrow=length(responseVars),ncol=4)
row.names(betaTable) <- responseVars
colnames(betaTable) <- c("mean","median","95% CI","p-value")

for(i in 1:length(responseVars)){
	y <- as.numeric(as.vector(df.4[[responseVars[i]]]))
	dataBlock <- list("N" = length(which(!is.na(y))),
					  "X" = matrix(
					  			as.numeric(df.4$registration_date_bioprj_julianCont[which(!is.na(y))]),
					  			nrow=1,
					  			ncol=length(which(!is.na(y)))),
					  "nX" = 1,
					  "Y" = y[which(!is.na(y))])

	fit <- sampling(dtMod,
					data=dataBlock,
					iter=2000,
					thin=2000/500,
					save_warmup=FALSE,
					chains=4)

	b <- c(extract(fit,"beta[1]",inc_warmup=FALSE,permute=FALSE))
	betaTable[i,1] <- round(mean(b),3)
	betaTable[i,2] <- round(median(b),3)
	ci <- round(quantile(b,c(0.025,0.975)),3)
	p <- ecdf(b)
	betaTable[i,3] <- paste0(c("(",ci[1]," - ",ci[2],")"),collapse="")
	betaTable[i,4] <- round(1-2*abs(0.5-p(0)),3)

	pdf(file=sprintf("models/figures/analysis4/trace_year_on_%s.pdf",responseVars[i]),width=7,height=8)
		plotTrace(fit)
	dev.off()
	pdf(file=sprintf("models/figures/analysis4/year_on_%s.pdf",responseVars[i]),width=10,height=6)
		layout(matrix(c(rep(1,6),rep(2,4)),nrow=2,ncol=5))
			plotModelFit(dataBlock,fit,showMAP=TRUE,showPost=TRUE,main=sprintf("Effect of BioProject Age on p(metadata)\n[metadata = %s]", varNames[i]),xlab="age of dataset (years)",ylab="p(complete metadata provided | author response)")
			plotMarginal(extract(fit,"beta[1]",inc_warmup=FALSE,permute=FALSE),predictor="dataset age",col="blue")
	dev.off()
}

write.table(betaTable,file="models/analysis4_betaTable.csv")