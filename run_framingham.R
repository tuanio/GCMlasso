library(GCMlasso)

GCMlasso_obj<-GCMlasso(data=Framingham,var_ord=1:15,var_group=16,
                       nsamp=200,odens=1,nwarm=50,seed=1,s=1e-2,t=1e-4,verb=TRUE)

# gr1 = 2005
# gr2 = 2020

gr1 = c(1, 2)
gr2 = c(3, 4)


compare_group(GCMlasso_obj,grp1=gr1,grp2=gr2,var=1:15,credible_level=0.95)

plot_graph(GCMlasso_obj,var=1:15,edge_perc=0.65)

reg_coef(GCMlasso_obj,var_pred=1:14,var_response=15)

predict_val<-predict(GCMlasso_obj,var_response=15,var_group=16)
predict_val

sink("predict_val.txt")
print(predict_val)
sink()

m <- mean(predict_val)

rinv <- rinvgauss(n=1000, mean=m)

hist(rinv)
