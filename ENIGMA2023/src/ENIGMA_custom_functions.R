
## Copyright © 2023 Lene J. Kjær, Michael P. Ward, Anette E. Boklund, Lars E. Larsen, Charlotte K. Hjulsager, and Carsten T. Kirkeby ”

##############################################################################################
# This file is part of the Shiny app for the ENIGMA HPAI model version 1.0.   

# The ENIGMA HPAI model is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                   

# The ENIGMA HPAI model is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                                                

# You should have received a copy of the GNU General Public License along with the ENIGMA HPAI model. If not, see <https://www.gnu.org/licenses/>.
##############################################################################################



#model_predictions=one-week-ahead predictions, model=model used for predictions
selectBestModel <- function(model_predictions, metrics = c("rps", "logs"), verbose = TRUE, plot = FALSE){
  
  options(digits=5)
  # returns names of the best models per metric
  pvals <- c()
  model_predictions_mean_scores<-c()
  perm_test <- c()
  best_models <- c()
  pvals_for_table  <- c()
  # model_predictions <- AI_seasonPreds
  # i<-1
 
  for (i in seq(1,length(metrics))){
    pvals[[i]] <- unlist(sapply(model_predictions, calibrationTest, method=2,which = metrics[i])["p.value",])
    model_predictions_scores <- lapply(model_predictions, scores, which = metrics, individual = TRUE, reverse = FALSE)
    model_predictions_mean_scores1 <- t(sapply(model_predictions_scores, colMeans, dims = 2))
    
    pvals_for_table <- cbind(pvals_for_table ,pvals[[i]])
    colnames(pvals_for_table )[i] <- paste0("p-values_",metrics[i])

    #check calibrated models
    calibrated_models <-names(which(pvals[[i]]  > 0.050))
    model_predictions_mean_scores <- model_predictions_mean_scores1[row.names(model_predictions_mean_scores1) %in% calibrated_models,,drop=FALSE]
    
    #order by scoring rule values (only calibrated models)
    ordered_index <- order(model_predictions_mean_scores[,i])
    
    #compares the two models with the lowest logS and rps scores
    models_to_compare <-row.names(model_predictions_mean_scores[ordered_index[1:2],])
    
    perm_test[[i]] <- permutationTest(colMeans(model_predictions_scores[[models_to_compare[1]]])[,i],
                                      colMeans(model_predictions_scores[[models_to_compare[2]]])[,i], plot = plot)
    best_models[[metrics[i]]] = models_to_compare[1]
  }
  #write results to file
write.csv(cbind(model_predictions_mean_scores1, pvals_for_table), paste0(deparse(substitute(model_predictions)),"_",data_name, "_results.csv"))
  
  if (verbose == TRUE){
    options(digits = 5)
    cat("------------------------------------","\n")
    cat("Mean scores:","\n")
    print(model_predictions_mean_scores1)
    for (i in seq(1,length(metrics))){
      cat("////////////////////////////////////","\n")
      cat("------------------------------------","\n")
      cat(paste0(metrics[i], ": p-values"),"\n")
      print(data.frame(t(pvals[[i]])))
      cat("------------------------------------","\n")
      cat(paste0(metrics[i], ": permutation test"),"\n")
      print(data.frame(t(unlist(perm_test[[i]]))))
      cat("------------------------------------","\n")
      cat(paste0(metrics[i], ": best performing model (name)"),"\n")
      cat(best_models[[i]],"\n")
    }
  }
  
  return(best_models)
}

#update fixed models to random models, correlated denotes whether the random effects are correlated or not
createRandomModels <- function( model,correlated, SEED = 1248){
  set.seed(SEED)
  
  if(correlated==TRUE){
    ri_option <- reformulate(c(".", "ri(type= 'iid', corr='all')"),
                             intercept = FALSE)
  }else{
    ri_option <- reformulate(c(".", "ri(type='iid')"),
                             intercept = FALSE)
  }
  
  ri_model <- update(model,
                     end = list(f = update(formula(model)$end, ri_option)),
                     ar = list(f = update(formula(model)$ar, ri_option)),
                     ne = list(f = update(formula(model)$ne,ri_option)),
                     optimizer = list(stop = list(tol=1e-5, niter=500),
                                      regression = list(method="nlminb"),
                                      variance = list(method="Nelder-Mead")),
                     subset = TRAIN,
                     keep.terms = TRUE
  )
  return(ri_model)
}

#Make footnote on downloaded pictures
# basic information at the beginning of each script
scriptName <- "filename.R"
author <- "ljk"
footnote <- paste(scriptName, format(Sys.time(), "%d %b %Y"),
                  author, sep=" / ")
# default footnote is today's date, cex=.7 (size) and color
# is a kind of grey
makeFootnote <- function(footnoteText=
                           format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(1, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

###############################################################