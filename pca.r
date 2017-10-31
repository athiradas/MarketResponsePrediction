selected_pca <- function(dataforpca){
  #PCA
  #check for categorical values
  #str(data_pca)
  #remove the original non pca columns from data
  #doing PCA
  prin_comp <- prcomp(dataforpca, scale. = TRUE)
  #names(prin_comp)
  par(mar = rep(2, 4))
  plot(prin_comp)
  #compute standard deviation of each principal component
  std_dev <- prin_comp$sdev
  #compute variance
  pr_var <- std_dev^2
  #check variance of first 10 components. 
  #We can see the variance is captures in first 6 components
  pr_var[1:10]
  #proportion of variance explained
  prop_varex <- (pr_var/sum(pr_var))*100
  #scree plot- we decide how many components should we select for modeling stage 
  prop_varex[1:20]
  plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")
  #Let's do a confirmation check, by plotting a cumulative variance plot. 
  #This will give us a clear picture of number of components.
  plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
  #Will take first 6 PCA 
  #Add PCA components to data
  return (predict(prin_comp, newdata = dataforpca))
}