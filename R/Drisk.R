#' @title Disclosure Risk
#'
#' @description
#' \code{drscore} Disclosure Risk
#'
#' @importFrom stats mahalanobis median qchisq quantile var
#' @importFrom utils head
#'
#'
#' @usage
#' drscore(Sample,Protected,
#' delta = 0.05,
#' neighbourhood = 1,
#' kdistinct = 5,
#' ldeniable = kdistinct,
#' neigh_type = "constant",
#' numeric.vars = NULL,
#' outlier.par = list(centre = median,
#'                    scale = var,
#'                    thresh = 0.01))
#'
#' @details  Statistical disclosure control methods for micro-level continuous data are both varied and complex. This method can be applied to any pair of original and protected data-sets despite a difference in dimensionality and without assuming any particular joint probability structure between the original and protected data.
#'
#' @param Sample A sample of the original dataset. A data frame or data frame extension (e.g. a tibble), matrix or an array,
#' @param Protected A sample of the protected dataset. A data frame or data frame extension (e.g. a tibble), matrix or an array.
#' @param delta amount of difference
#' @param neighbourhood Possible 'neighbourhood' types
#' 1 = Mahalanobis (Based on Mahalanobis Distance)
#' 2 = DSTAR   (Based on Density Based Distance)
#' 3 = StdEuclid (Based on Standardised (by std dev) Euclidean Distance)
#' 4 = RelEuclid (Relative Euclidean Distance sum_k ((Xk-Yk)/Xk)^2)
#' @param kdistinct number of data point in the neighbourhood to be classified as protected. k distinct threshold if integer then probability threshold is k/SS (SS = sample size)
#' @param ldeniable methods of define disclosure. l undeniable threshold if integer then probability threshold is l/SS (SS = sample size)
#' @param neigh_type methods of define length/area of neighbourhood. Possible 'neigh_type' types
#' constant = fixed threshold on distance
#' prob = Nearest Neighbour Probability Neighbourhood used (Worst Case Scenario 1)
#' estprob = Nearest Neighbour Probability Neighbourhood used based on protected density (Worst Case Scenario 2)
#' @param numeric.vars #Continuous Variables in the dataset.
#' @param outlier.par A list(centre, scale, thresh). Define the method of determining outlier. Parameters to adjust how MV outliers are determined. Default is that lie 99 percent (based on Chi-Square n-1 dist) away from median after scale by variance.
#'
#' @seealso  \code{\link[dress]{update}}
#'
#' @returns
#' An object of the type as `DRisk`. Summary of the disclosure risk in terms of distinctness, similarities and undeniable parameters.
#' @examples
#' # ##################
#' # ##all continuous###################
#'
#' library(svMisc)
#' library(dress)
#' library(sdcMicro)
#'
#' CASC_sample <- CASCrefmicrodata[,c(2,3,4,6)]
#' CASC_protected <- addNoise(CASC_sample,noise = 100)$xm #Additive Noise protected
#'
#' DRisk_NN <- drscore(
#'   Sample = CASC_sample, #Original Sample
#'   Protected = CASC_protected,
#'   delta = 0.05,
#'   kdistinct = 0.05, #k distinct threshold if integer then
#'   # probability threshold is k/SS (SS = sample size)
#'   ldeniable = 5, # l undeniable threshold if integer then
#'   # probability threshold is l/SS (SS = sample size)
#'   neighbourhood = 1,
#'   #Possible 'neighbourhood' types
#'   # 1 = Mahalanobis (Based on Mahalanobis Distance)
#'   # 2 = DSTAR   (Based on Density Based Distance)
#'   # 3 = StdEuclid (Based on Standardised (by std dev) Euclidean Distance)
#'   # 4 = RelEuclid (Relative Euclidean Distance sum_k ((Xk-Yk)/Xk)^2)
#'   neigh_type = 'prob',
#'   #Possible 'neigh_type' types
#'   #constant = fixed threshold on distance
#'   #prob = Nearest Neighbour Probability Neighbourhood used
#'   #estprob = = Nearest Neighbour Probability Neighbourhood used based on protected density
#'   numeric.vars = 1:4, #Which Variables are continuous?
#'   outlier.par = list(centre = median,
#'                      scale = var,
#'                      thresh = 0.01)
#'   #Parameters to adjust how MV outliers are determined.
#'   #Default is that lie 99% (based on Chi-Square n-1 dist) away from median after scale by variance.
#' )
#'
#'
#' @export



drscore <-
  function(
    Sample,
    Protected,
    delta = 0.05,
    neighbourhood = 1,
    kdistinct = 5,
    ldeniable = kdistinct,
    neigh_type = 'constant',
    numeric.vars = NULL,
    outlier.par = list(centre = median,
                       scale = var,
                       thresh = 0.01)
  ){
    #testing parameter classes
    assertthat::assert_that(is.data.frame(Sample))
    assertthat::assert_that(is.data.frame(Protected)|is.matrix(Protected)|is.array(Protected))
    assertthat::assert_that(assertthat::noNA(Sample), assertthat::noNA(Protected))
    assertthat::assert_that(neighbourhood %in% c(1:4),
                            msg = "Only 4 neighbourhood present. Only 1,2,3 or 4 options allowed for neighbourhood")
    assertthat::assert_that(neigh_type %in% c("constant","prob","estprob"),
                            msg = "Only 3 neighbourhood types present,constant, prob, estprob.")

    if(is.character(numeric.vars)){
      numeric.vars <-  which(colnames(Sample) %in% numeric.vars)
    }

    if(kdistinct > 1) kdistinct <- kdistinct/NROW(Sample)
    if(ldeniable > 1) ldeniable <- ldeniable/NROW(Sample)
    Sample <- as.data.frame(Sample)
    Protected <- as.data.frame(Protected)
    if(sum(colnames(Sample) %in% colnames(Protected)) == NCOL(Sample)){
      Protected <- Protected[,colnames(Sample)]
    }else if(sum(colnames(Protected) %in% colnames(Sample)) == NCOL(Protected)){
      Sample <- Sample[,colnames(Protected)]
    }else if(NCOL(Protected)==NCOL(Sample)){
      colnames(Protected) <- colnames(Sample)
    }else{
      Nv <- min(NCOL(Sample),NCOL(Protected))
      Sample <- Sample[,1:Nv]
      Protected <- Protected[,1:Nv]
      colnames(Protected) <- colnames(Sample)
    }
    varname <- colnames(Sample)
    Nv <- NCOL(Sample)
    if(is.null(numeric.vars)){
      numvar <- which(sapply(1:Nv, function(k) typeof(Sample[,k])=='double'))
    }else{
      numvar <- numeric.vars
    }
    catID <- rep("",NROW(Sample))
    PcatID <- rep("",NROW(Protected))

    catvar <- (1:Nv)[-numvar]

    if(length(catvar)>0){
      for(k in catvar){
        catID <-  paste0(catID,Sample[,k])
        PcatID <- paste0(PcatID,Protected[,k])
      }
    }
    catLevels <- levels(factor(c(catID,PcatID)))

    if(length(catLevels)==1){
      if(catLevels == ""){
        catLevels <- "All"
        catID <- rep("All",length(catID))
        PcatID <- rep("All",length(PcatID))
      }}

    outlier <- function(x){
      x <- as.data.frame(x)
      centre <- apply(x,2,outlier.par$centre)
      mah <- mahalanobis(x,centre,outlier.par$scale(x))
      outthresh <-qchisq(1-outlier.par$thresh,NCOL(x))
      return(which(mah > outthresh))
    }

    # 1 = Mahalanobis
    # 2 = DSTAR
    # 3 = StdEuclid
    # 4 = RelEuclid

    if(neighbourhood == 1){
      dist <- function(x0,DX,DXstar){
        return(mahalanobis(DXstar,unlist(x0),var(DX)))
      }
      neighname <- 'Mahalanobis Radius'
    }else if(neighbourhood ==2){
      # dist <- function(x0,DX){
      #   FXhat <- function(x,y){
      #     x <- unlist(x)
      #     y <- unlist(y)
      #     vals <- rep(1,NROW(Sample))
      #     for(k in 1:Nv){
      #       lbnds <- min(x[k],y[k])
      #       ubnds <- max(x[k],y[k])
      #       vals <- vals*(Sample[,k] >= lbnds)*(Sample[,k] <= ubnds)
      #     }
      #     return(sum(vals)/NROW(Sample))
      #   }
      #   return(unlist(lapply(1:NROW(DX),function(i) FXhat(x0,DX[i,]))))
      # }
      neighname <- 'Density Star'
      dist <- function(x0,DX,DXstar){
        xcomp <- array(unlist(x0),dim =c(dim(x0),NROW(DXstar)))
        bnder <- function(x){
          bndtest <- rep(1,NROW(DX)^2)
          for(k in 1:Nv){
            bndtest <- bndtest*rep(DX[,k],each = NROW(DXstar)) >= rep(apply(cbind(x[k,],DXstar[,k]),1,min),NROW(DX)) &
              rep(DX[,k],each = NROW(DXstar)) <= rep(apply(cbind(x[k,],DXstar[,k]),1,max),NROW(DX))
          }
          return(apply(array(bndtest,dim = c(NROW(DXstar),NROW(DX))),1,mean))
        }
        probs <- apply(xcomp,1,bnder)
        return(t(probs))
      }

    }else if (neighbourhood ==3){
      neighname <- 'Normailised Euclidean Radius'
      dist <- function(x0,DX,DXstar){
        return(sqrt(apply(t((t(DXstar)-unlist(x0))^2/diag(var(DX))),1,sum)))
      }
    }else if(neighbourhood ==4){
      neighname <- 'Relative Euclidean Radius'
      dist <- function(x0,DX,DXstar){
        return(sqrt(apply(t((t(DXstar)-unlist(x0))^2/apply(DX^2,2,max)),1,sum)))
      }
    }

    Sample_Levels <- list()
    Protected_Levels <- list()
    x0_Levels <- list()
    Outlier_Levels <- list()
    Distance_Levels <- list()
    NNThresh <- list()
    pLinks <- list()
    oneLink <- list()
    Cond1 <- c()
    Cond2 <- c()
    Cond3 <- c()
    outs <- rep(F,NROW(Sample))
    exact <- c()
    int_match <- rep(F,NROW(Sample))
    LinkScore_Levels <- array(dim = c(length(catLevels),6))
    count <- 0
    probcount <- 0
    #require(svMisc)

    for(i in 1:length(catLevels)){
      catIDindex <- which(catID == as.character(catLevels[i]))
      Sample_Levels[[i]] <- as.matrix(Sample[catIDindex,numvar])
      Protected_Levels[[i]] <- as.matrix(Protected[which(PcatID==as.character(catLevels[i])),numvar])
      x0_Levels[[i]] <- Sample_Levels[[i]]
      outs[(catIDindex[outlier(Sample_Levels[[i]])])] <- T
      if(sum(outs)>0) Outlier_Levels <- Sample_Levels[[i]][which(outs[catIDindex]),]

      DistanceFunction <- function(j){
        DSS <- dist(x0_Levels[[i]][j,],Sample[,numvar],Sample_Levels[[i]])
        DSP <- dist(x0_Levels[[i]][j,],Sample[,numvar],Protected_Levels[[i]])
        #DPS <- dist(x0_Levels[[i]][j,],Protected,Sample)
        return(list(DSS,DSP))
      }

      DSS_temp <- array(dim = c(NROW(Sample_Levels[[i]]),NROW(Sample_Levels[[i]])))
      DSP_temp <- array(dim = c(NROW(Sample_Levels[[i]]),NROW(Protected_Levels[[i]])))

      for(j in 1:NROW(Sample_Levels[[i]])){
        count <- count + 1
        DF_val <- DistanceFunction(j)
        DSS_temp[j,] <- DF_val[[1]]
        DSP_temp[j,] <- DF_val[[2]]
        exact[j] <- max(DF_val[[2]] == 0)
        # progress(100*count/NROW(Sample))
      }
      Distance_Levels[[i]] <- list(DSS = DSS_temp,DSP = DSP_temp)
      if(neigh_type=='prob'){
        neigh_class <- 'Nearest Neighbour'
        NNThresh[[i]] <- apply(Distance_Levels[[i]]$DSS,1,quantile,delta,type=4)
      }else if(neigh_type == 'estprob'){
        neigh_class <- 'Approx Nearest Neighbour'
        NNThresh[[i]] <- apply(Distance_Levels[[i]]$DSP,1,quantile,delta)
      }else{
        neigh_class <- 'Threshold'
        NNThresh[[i]] <- delta
      }
      pLinks[[i]] <- (Distance_Levels[[i]]$DSP < array(NNThresh[[i]],dim(Distance_Levels[[i]]$DSP)))

      Cond1[catIDindex] <- apply(Distance_Levels[[i]]$DSS < array(NNThresh[[i]],dim(Distance_Levels[[i]]$DSS)),1,mean) <= kdistinct
      Cond2[catIDindex] <- apply(pLinks[[i]],1,mean)  > 0
      Cond3[catIDindex] <- sapply(1:NROW(pLinks[[i]]),function(j)min(apply(data.frame(pLinks[[i]][,which(pLinks[[i]][j,])]),2,mean))) <= ldeniable
      oneLink[[i]] <- which((pLinks[[i]])[which(apply(pLinks[[i]],1,sum)==1),which(apply(pLinks[[i]],2,sum)==1)]==T,arr.ind = T)
      if(NCOL(oneLink[[i]])==1) oneLink[[i]] <- cbind(oneLink[[i]],oneLink[[i]])
      int_match[catIDindex][(oneLink[[i]])[,1]] <- T

      discflag_Level <- Cond1[catIDindex]*
        Cond3[catIDindex]

      LinkScore_Levels[i,] <- c(NROW(Sample_Levels[[i]]),
                                mean(discflag_Level),
                                mean(discflag_Level*outs[catIDindex]),
                                mean(Cond1[catIDindex]),
                                mean(Cond2[catIDindex]),
                                mean(Cond3[catIDindex]))
    }
    discflag <- Cond1*Cond2*Cond3

    linkcounts <- data.frame(Values= c(NROW(Sample),NROW(Protected),length(numvar),length(catLevels),
                                       sum(outs),sum(Cond1),sum(outs*Cond1), sum(exact),
                                       sum(int_match),sum(int_match*outs),sum(int_match*outs*Cond1)))
    rownames(linkcounts) = c("Number of Observations in the Sample","Number of Observations in the Protected Sample",
                             "Number of Continuous Variables", "Number of Key Categories","Number of Outliers in Sample",
                             "Number of Distinct Points in Sample","Number of Distinct Outliers in Sample",
                             "Number of Exact Matches in Sample","Number of Interval Matches in Sample",
                             "Number of Outlier Interval Matches in Sample","Number of Distint Outlier Interval Matches in Sample")

    linkspace <- sapply(1:NROW(linkcounts),function(k)
      paste0(rep(' ',60-nchar(rownames(linkcounts))[k]),collapse = ''))

    linkscore <- data.frame(Values = c(mean(discflag),sum(discflag*outs)/sum(outs),
                                       mean(Cond1),mean(Cond2),mean(Cond3)))
    #cond1 dist, cond2 estimted, cond3 undeniable

    rownames(linkscore) = c("Delta Disclosure Risk of Sample",
                            "Delta Disclosure Risk of Sample Outliers",
                            'Proportion Distinct',
                            'Proportion Estimated',
                            'Proportion Undeniable')
    scorespace <- sapply(1:NROW(linkscore),function(k)
      paste0(rep(' ',60-nchar(rownames(linkscore))[k]),collapse = ''))

    LinkScore_Levels <- data.frame(LinkScore_Levels)
    colnames(LinkScore_Levels) <- c('N.Obs','DRisk','Out_DRisk','Distinct',
                                    'Estimated','Undeniable')
    rownames(LinkScore_Levels) <- catLevels
    print_output <- function(){
      cat('\n')
      cat(paste0(rep('#',70),collapse = ''),'\n')
      #cat('#',paste0(rep(' ',66),collapse = ''),'#','\n')
      cat('#',paste0(rep(' ',19),collapse = ''),'Disclosure Risk Assessment',
          paste0(rep(' ',19),collapse = ''),'#','\n')
      #cat('#',paste0(rep(' ',66),collapse = ''),'#','\n')
      cat(paste0(rep('#',70),collapse = ''),'\n')
      cat(paste0(neigh_class,' Neighbourhood with parameters:
        delta = ',delta,', kdistinct = ',kdistinct,
                 ', ldeniable = ',ldeniable,'. \n'))
      cat('\n')
      cat(paste0(rownames(linkcounts),linkspace,linkcounts$Values,collapse = '\n'),'\n \n')
      cat(paste0(rownames(linkscore),scorespace,round(linkscore$Values,4),collapse = '\n'),'\n \n')
      cat('Category Level Disclosure Risk: \n \n')
      print(head(LinkScore_Levels[order(LinkScore_Levels[,1]),],10))
      cat('\n')
    }
    print_output()

    parameters <- list(delta=delta, neighbourhood=neighbourhood,
                       kdistinct=kdistinct,ldeniable=ldeniable,
                       neigh_type = neigh_type,outlier.par=outlier.par,
                       numvar = numvar)

    output <- list(Score = mean(Cond1*Cond2*Cond3),
                   Disclosed = Cond1*Cond2*Cond3,
                   Cond = cbind(Cond1,Cond2,Cond3),
                   Exact = exact,
                   Outliers = outs,
                   Interval_match = int_match,
                   parameters=parameters,
                   Distance_Levels = Distance_Levels,
                   Sample = Sample,
                   Protected = Protected,
                   pLinks = pLinks,
                   oneLink = oneLink,
                   Linkcounts = linkcounts,
                   Linkscore = linkscore,
                   LinkScore_Levels=LinkScore_Levels)
    class(output) <- 'DRisk' #solved this class error
    return(output)

  }


