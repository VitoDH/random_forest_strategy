
FeatName <- c("CMO","LowerBand","TRIX","KST","MFI","FastD","TDI","FastK","UltimateOscillator","ChaikinVol","ADL","OBV")


#find parent
GetParent <- function(currentIndex,Tree){                  #get the parent information of current index
  ParentIndex <- which(apply(Tree[c(1:currentIndex),c("left daughter","right daughter")]==currentIndex,1,any) == TRUE)
  DaughterState <- colnames(Tree[which(Tree[ParentIndex,]==currentIndex)])[1]
  ParentInformation <- Tree[ParentIndex,c("split var","split point")]
  ParentInformation <- cbind(ParentIndex,DaughterState,ParentInformation)
  return(ParentInformation)
}
#GetParent(PredIndex1)



GetSingleCode <- function(IterIndex,Tree){        # get the code of only one label in the tree(there are many 1 and 0 label in the tree)
  singleLabelCode <- character()
  repeat{
    ParentInfo <- GetParent(IterIndex,Tree)
    #FeatIndex <- which(as.character(ParentInfo[,"split var"])==FeatName)                  round(ParentInfo[,"split point"],4)
    tempCondition <-  paste(as.character(ParentInfo[,"split var"]),"<=",as.character(ParentInfo[,"split point"]))
    tempNum <- 0

    if(!(tempCondition %in% ConditionSet)){
      ConditionSet <<- c(ConditionSet,tempCondition)
      ConditionNum <<- ConditionNum + 1
      tempNum <- ConditionNum
      if(tempNum < 10){
        tempNum <- paste("00",as.character(tempNum),sep = "")
      }else{
        if(tempNum < 100){
          tempNum <- paste("0",as.character(tempNum),sep = "")
        }else{
          tempNum <- as.character(tempNum)
        }
      }
      ConditionSetCode <<- c(ConditionSetCode,paste("condition",tempNum,":",tempCondition,sep = ""))
    }else{
      tempNum <- which(tempCondition == ConditionSet)
      if(tempNum < 10){
        tempNum <- paste("00",as.character(tempNum),sep = "")
      }else{
        if(tempNum < 100){
          tempNum <- paste("0",as.character(tempNum),sep = "")
        }else{
          tempNum <- as.character(tempNum)
        }
      }
    }
   
    if(as.character(ParentInfo[,"DaughterState"]) == "left daughter"){
      parentCode <- paste("condition",tempNum,sep = "")
      parentCode <- paste("(",parentCode,")",sep = "")
    }else{
      parentCode <- paste("!condition",tempNum,sep = "")
      parentCode <- paste("(",parentCode,")",sep = "")
    }
    
    if(length(singleLabelCode) == 0){
      singleLabelCode <- paste(parentCode,singleLabelCode,sep = "")
    }else{
      singleLabelCode <- paste(parentCode,"&&",singleLabelCode,sep = "")
    }
    
    if(as.numeric(ParentInfo[,"ParentIndex"]) == 1){      #reach the root
      break
    }else{
      IterIndex <- ParentInfo[,"ParentIndex"]
    }
  }
  #singleLabelCode <- paste("(",singleLabelCode,")",sep = "")   #don't add the outside bracket
  return(singleLabelCode)
}



GetLabelCode <- function(LabelIndex,Tree){
  LabelCode <- character()
  i <- 1
  for(tempIndex in LabelIndex){
    singleLabelCode <- GetSingleCode(tempIndex,Tree)
    if(i < 10){
      tempNum <- paste("00",as.character(i),sep = "")
    }else{
      if(i < 100){
        tempNum <- paste("0",as.character(i),sep = "")
      }else{
        tempNum <- as.character(i)
      }
    }
    singleLabelCode <- paste("rule",tempNum,":",singleLabelCode,sep = "")
    
    if(length(LabelCode) == 0){
      LabelCode <- paste(LabelCode,singleLabelCode,sep = "")
    }else{
      LabelCode <- paste(LabelCode,"\n",singleLabelCode,sep = "")
    }
    i <- i + 1;
  }
  return(LabelCode)
}


OutputRule <- function(path, BestTreeNum, forest){
  for(TreeIndex in 1:BestTreeNum){
    ConditionSet <<- NULL
    ConditionNum <<- 0
    ConditionSetCode <<- NULL
    tempTree <- getTree(forest,TreeIndex,labelVar = TRUE)
    OneIndex <- which(tempTree$prediction == "1")      #get the number of the row of index 1 and 0
    
    OneCode <- GetLabelCode(OneIndex,tempTree)
    ConditionSetTable <- paste(ConditionSetCode,collapse = "\n")
    
    #Not Append
    #write.table(ConditionSetTable,file = paste("E:\\LDH\\SRData\\sr-ml\\testStrat\\TreeRule-long-condition-seperated\\","Condition-",as.character(TreeIndex),".txt",sep = ""),row.names = FALSE,col.names = FALSE,quote = FALSE)
    #write.table(OneCode,file = paste("E:\\LDH\\SRData\\sr-ml\\testStrat\\TreeRule-long-condition-seperated\\","Rule-",as.character(TreeIndex),".txt",sep = ""),row.names = FALSE,col.names = FALSE,quote = FALSE)
    
    #Append
    if(TreeIndex < 10){
      TreeIndex <- paste("00",as.character(TreeIndex),sep = "")
    }else{
      if(TreeIndex < 100){
        TreeIndex <- paste("0",as.character(TreeIndex),sep = "")
      }else{
        TreeIndex <- as.character(TreeIndex)
      }
    }
    
    write.table(ConditionSetTable,file = paste(path,"OneCode-",as.character(TreeIndex),".txt",sep = ""),row.names = FALSE,col.names = FALSE,quote = FALSE)
    write.table("Rule",file = paste(path,"OneCode-",as.character(TreeIndex),".txt",sep = ""),append = TRUE,row.names = FALSE,col.names = FALSE,quote = FALSE)
    write.table(OneCode,file = paste(path,"OneCode-",as.character(TreeIndex),".txt",sep = ""),append = TRUE,row.names = FALSE,col.names = FALSE,quote = FALSE)
    
  }
}





