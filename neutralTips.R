###load this function directly into your R session:
### require (RCurl)
###> nt<-"https://raw.githubusercontent.com/enicurus/MoltEvolution/master/neutralTips.R"
### script <- getURL(nt, ssl.verifypeer = FALSE)
### eval(parse(text = script),envir=.GlobalEnv)



neutralTips<-function(tree,states){
	require(phytools)
	varMultiplier<-function(states){
		state<-matrix(nrow=length(states),ncol=length(states))
		for (i in 1:length(states))
		state[,i]<-states
		return(state)
		}
	matrix<-varMultiplier(states)
	matrixMaker<-function(vector){
		out<-to.matrix(vector,seq=0:1)
		return(out)
		}
	listMatrixMaker<-function(matrix,states){
		matrixList<-list()
		for(i in 1:length(matrix[1,])){
			matrixName<-paste("matrix",i,sep="_")
			matrixList[[matrixName]]<-(as.vector(matrix[,i]))
			names(matrixList[[matrixName]])<-names(states)
			matrixList[[matrixName]]<-matrixMaker(matrixList[[matrixName]])
			}
		return(matrixList)
		}
	matrix<-listMatrixMaker(matrix,states)
	JonSnow<-function(matrix){
		for (i in 1:length(matrix))
			matrix[[i]][i,]<-rep(0.5,2)
		return(matrix)
		}
	matrix<-JonSnow(matrix)
	multiSimmap<-function(tree,matrix){
		simmapList<-list()
		for(i in 1:length(matrix)){
			cat("Calculating 100 stochastic maps for tree",i,"of",length(matrix),"\n")
			simmapName<-paste("simmap",i,sep="_")
			x=matrix[[i]]
			simmapList[[simmapName]]<-make.simmap(tree,x,nsim=100)
							}
		return(simmapList)
		}	
	maps<-multiSimmap(tree,matrix)
	multiDescribeMap<-function(maps){
		describeList<-list()
		for(i in 1:length(maps)){
			cat("Integrating over 100 maps for tree",i,"out of",length(maps),"\n")
			listName<-paste("describe",i,sep="_")
			describeList[[listName]]<-describe.simmap(maps[[i]],plot=FALSE)
							}
			return(describeList)
		}
	describeMaps<-multiDescribeMap(maps)
	predictedTipMaker<-function(describeMaps,states){
		out<-list()
		for(i in 1:length(describeMaps)){
			outnames<-
			out[i]<-describeMaps[[i]]$tips[i,2]
			}
		out<-matrix(out)
		rownames(out)<-rownames(describeMaps[[1]]$tips)	
		out<-cbind(out,as.numeric(states))
		proportion<-as.numeric(1-(abs(as.numeric(out[,2])-(as.numeric(out[,1])))))
		out<-cbind(out,as.numeric(proportion))
		colnames(out)<-c("expected","observed","probBrown") 
		return(out)
		}
	outMatrix<-predictedTipMaker(describeMaps,states)
	print("Time is an illusion. Lunchtime, doubly so. --Douglas Adams")
	return(outMatrix)
}
