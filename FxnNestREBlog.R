# Source Function 
# for the NestedRandomEffects_Blog post.
# This function only works for random
# intercepts


multi_int<- function(RE_Vector, FE_Vector, RELevels=1, RE_Vector2=NULL){

  # Creating stat_functions to plot lines
  #-------------------------------------
  re<- as.matrix(RE_Vector)
  if (is.null(RE_Vector2)==F){
    re2<- as.matrix(RE_Vector2)  
  }
  fe<- as.matrix(cbind(FE_Vector[1], FE_Vector[2]))
  if (RELevels == 1){
    coefs<- as.data.frame(
      cbind(
        re, do.call(rbind, replicate(nrow(re), fe, simplify = F))))
  }
  
  #if (RELevels == 2){
  #  coefs2<- as.data.frame(
  #    cbind(
  #      re2, do.call(rbind, replicate(nrow(re2), fe, simplify = F))
  #    )
  # )}
  
  coefs$pop <- c_pop_final
  colnames(coefs)<- c("alpha", "int", "beta", "pop")
  
  # Throwing the functions into a list
  #----------------------------------

  coeflines <-
    alply(coefs, 1, function(df) {
      stat_function(
        fun=function(x){ 
          df$alpha+ df$int + df$beta*x}, color=df$pop, size=2)
    })
  return(coeflines)
}

# Plotting the residuals by themselves
fxn_plot_resid<-function(ModelResidual){ggplot()+
    geom_point(data=d, aes(x=seq(1:nrow(d)),y=ModelResidual, color=pop), size=4 )+
    scale_color_manual(values = c(c_pop_final))+geom_hline(yintercept=0)+
    ylim(-20,20)+xlab("Index")
} 

# plotting the residuals against the predictor variable
fxn_plot_resid_x<- function(ModelResidual) {ggplot()+
    geom_point(data=d, aes(x=x,y=ModelResidual, color=pop), size=4 )+
    scale_color_manual(values = c(c_pop_final))+geom_hline(yintercept=0)+
   ylim(-20,20)
}