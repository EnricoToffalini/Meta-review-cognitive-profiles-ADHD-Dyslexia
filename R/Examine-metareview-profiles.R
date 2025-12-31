
############################################################

rm(list=ls())
library(readxl)
library(ggplot2)
library(metafor)

df = data.frame(read_excel("Data/coded-Dataset.xlsx"))
df = df[df$include==1,]
df$Effect_size = ifelse(df$direction=="positive",df$Effect_size,df$Effect_size*-1)
length(unique(df$ID_article))

############################################################

# Selecetion of population and domains

#### select ADHD - all primary domains
dx = df[df$Target_disorder=="ADHD",]
dx$Domains = dx$Domain_L1_general

#### select ADHD - executive functions subdomains + intelligence
dx = df[df$Target_disorder=="ADHD" & (df$Domain_L1_general%in%c("Executive functions","Intelligence / general cognition")),]
dx$Domains = dx$Domain_L2_specific
dx = dx[!is.na(dx$Domains), ]

############################################################

# Plot and analysis

#### arrange graphical parameters
dx$dotDim = 1/dx$Std_Err^0.8; dx$dotDim[is.na(dx$dotDim)] = median(dx$dotDim,na.rm=T)

## PLOT all dots
ggplot(dx,aes(x=Domains,y=Effect_size,color=Year))+
  coord_flip()+
  scale_colour_gradient(low="black",high="red")+
  geom_point(aes(size=dotDim,alpha=dotDim))+
  geom_hline(yintercept=0,linetype=2,linewidth=1)

## PLOT aggregate by study
dxagg = aggregate(dx[,c("Effect_size","Std_Err","dotDim","Year")],by=list(ID_article=dx$ID_article,Domains=dx$Domains),FUN=mean)
ggplot(dxagg,aes(x=Domains,y=Effect_size,color=Year))+
  coord_flip()+
  scale_colour_gradient(low="black",high="red")+
  geom_point(aes(size=dotDim,alpha=dotDim))+
  geom_hline(yintercept=0,linetype=2,linewidth=1)

## PLOT further aggregate to one estimate
dxagg$vi = dxagg$Std_Err^2
meta_by_domain = by(dxagg,dxagg$Domains,function(dat) {rma(yi = Effect_size, vi = vi, data = dat, method = "REML" ) })
meta_summary = do.call(
  rbind, lapply(names(meta_by_domain), function(dom) {
    m = meta_by_domain[[dom]]
    data.frame(Domains = dom,yi = as.numeric(m$b),se = m$se,ci.lb = m$ci.lb,ci.ub = m$ci.ub,tau2 = m$tau2,k = m$k
    )  } ) )
ggplot(meta_summary,aes(x=Domains,y=yi))+
  coord_flip()+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=ci.lb,ymax=ci.ub))+
  geom_hline(yintercept=0,linetype=2,linewidth=1)


############################################################


