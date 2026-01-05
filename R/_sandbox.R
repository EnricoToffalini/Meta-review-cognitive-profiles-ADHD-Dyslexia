
# Plot and analysis

## PLOT all dots
ggplot(dx,aes(x=Domains,y=Effect_size,color=Year))+
  coord_flip()+
  ggtitle(disorder)+
  scale_colour_gradient(low="black",high="red")+
  geom_point(aes(size=dotDim,alpha=dotDim))+
  geom_hline(yintercept=0,linetype=2,linewidth=1)+
  theme(text=element_text(size=ts))

## PLOT aggregate by study
dxagg = aggregate(dx[,c("Effect_size","Std_Err","dotDim","Year")],by=list(ID_article=dx$ID_article,Domains=dx$Domains),FUN=mean)
nrow(dxagg)
ggplot(dxagg,aes(x=Domains,y=Effect_size,color=Year))+
  coord_flip()+
  ggtitle(disorder)+
  scale_colour_gradient(low="black",high="red")+
  geom_point(aes(size=dotDim,alpha=dotDim))+
  geom_hline(yintercept=0,linetype=2,linewidth=1)+
  theme(text=element_text(size=ts))

## PLOT further aggregate to one estimate
dxx = dx[!is.na(dx$Std_Err),]
dxagg = aggregate(dxx[,c("Effect_size","Std_Err","dotDim","Year")],by=list(ID_article=dxx$ID_article,Domains=dxx$Domains),FUN=mean)
dxagg$vi = dxagg$Std_Err^2
nrow(dxagg)
meta_by_domain = by(dxagg,dxagg$Domains,function(dat) {rma(yi = Effect_size, vi = vi, data = dat, method = "REML" ) })
meta_summary = do.call(
  rbind, lapply(names(meta_by_domain), function(dom) {
    m = meta_by_domain[[dom]]
    data.frame(Domains = dom,yi = as.numeric(m$b),se = m$se,ci.lb = m$ci.lb,ci.ub = m$ci.ub,tau2 = m$tau2,k = m$k
    )  } ) )
print(meta_summary)
ggplot(meta_summary,aes(x=Domains,y=yi))+
  coord_flip()+
  ggtitle(disorder)+
  geom_point(size=5)+
  geom_errorbar(aes(ymin=ci.lb,ymax=ci.ub),linewidth=1)+
  geom_hline(yintercept=0,linetype=2,linewidth=1)+
  theme(text=element_text(size=ts),
        axis.title.y=element_blank())+
  scale_y_continuous(breaks=seq(-4,4,.2))+
  ylab("Effect size")
