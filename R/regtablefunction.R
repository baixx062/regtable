
#' @param x an lm object. For example, if you have a<-lm(mpg~cyl+disp+drat*wt,data=mtcars), you can enter regtable(a,"replace_with_the_name_of_the_table_here.csv")
#' @return a csv file with the table output.
#' This is a function that marks significance such that ***<.001, **<.01, *<.05, +<.10.
significance<-function(x){
  result<-ifelse(summary(x)$coeff[,4]<.001,"***",
                 ifelse(summary(x)$coeff[,4]<.01,"**",
                        ifelse(summary(x)$coeff[,4]<.05,"*",
                               ifelse(summary(x)$coeff[,4]<.1,"+","ns"))))

}

#' This thing extracts all the relevant output from lm, such as the coefficients, standard error, confidence interval, and partial eta square
exportintotableraw<-function(x){
  cbind(round(summary(x)$coeff[,1],2),significance(x),round(summary(x)$coeff[,2],2),round(summary(x)$coeff[,4],3),"[",round(confint(x),2),"]",rbind("",round(lsr::etaSquared(x)*100,2)),round(lm.beta::lm.beta(x)$standardized.coefficients,2))
}

#' This thing extracts F statistics
fsta<-function(x){
  print(paste0("F(",summary(x)$fstatistic[2],",",summary(x)$fstatistic[3],")=",round(summary(x)$fstatistic[1],2),", R-squared=",round(summary(x)$r.squared,2),", adjusted R-squared=",round(summary(x)$adj.r.squared,2)))
}

#' This thing provides headers and footnotes.
exportintotableready<-function(x){
  a<-exportintotableraw(x)[,-9]
  colnames(a)<-c("Estimate","","S.E.","p","","95% CI","","","Partial eta square (in %)","Standardized estimate")
  b<-rbind(a,cbind(fsta(x),"","","","","","","","",""),
           cbind("Note. S.E. refers to standard error of the estimates. ***p<0.001, **p<.01, +p<.10.","","","","","","","","",""))
  print(b)
}

#' This is the final function that produces the csv output. x is the lm object, and y is the name of the csv table.

#' @export
regtable<-function(x,y){
  write.csv(exportintotableready(x),y)
}
