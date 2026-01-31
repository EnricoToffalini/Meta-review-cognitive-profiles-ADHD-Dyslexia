
############################################################

rm(list=ls())
library(readxl)
library(ggplot2)
library(metafor)
library(dplyr)
ts = 30

# import dataset and check features
df = data.frame(read_excel("Data/coded-Dataset.xlsx"))
nrow(df)
length(unique(df$ID_article))

# check reasons for exclusion
table(df$why_not_include[df$include==0],df$Target_disorder[df$include==0])
# exclude effects for those reasons
df = df[df$include==1,]
# for now, keep only effects that are SMDs
df = df[grepl(paste(c("hedges", "cohen", "SMD"), collapse = "|"), tolower(df$Type_effect_size)) , ]
# for now, exclude "Other / unclear" domains
df = df[!df$Domain_L1_general%in%c("Other / unclear"),]
# for now, remove where there's no clear primary L1 domain
df = df[!is.na(df$Domain_L1_general), ]
# for now, assign "mixed" to unclear df$Age_group
df$Age_group[is.na(df$Age_group) | !df$Age_group%in%c("child","mixed","adult")] = "mixed"

# turn all effect sizes in the appropriate direction
df$Effect_size = ifelse(df$direction=="negative",df$Effect_size,df$Effect_size*-1)

# age group to factor
df$Age_group = factor(df$Age_group, levels=c("child","mixed","adult"))

# check remaining features
nrow(df)
length(unique(df$ID_article))
#table(df$Target_disorder,df$Domain_L1_general)
#table(df$Target_disorder,df$Domain_L2_specific)

############################################################

# Selection of population and domains

#### select Dyslexia - ad hoc primary domains
disorder = "Dyslexia"; domains = "primary"
dx = df[df$Target_disorder==disorder,]
for(i in 1:nrow(dx)) if(dx$Domain_L1_general[i]=="Working memory") dx$Domain_L1_general[i] = dx$Domain_L2_specific[i]
dx = dx[dx$Domain_L1_general != "Academic achievement", ]
dx$Domains = dx$Domain_L1_general
length(unique(dx$ID_article))
table(dx$Domains)

#### select ADHD - ad hoc primary domains
disorder = "ADHD"; domains = "primary"
dx = df[df$Target_disorder==disorder,]
for(i in 1:nrow(dx)) if(dx$Domain_L1_general[i] %in% c("Working memory","Perception")) dx$Domain_L1_general[i] = dx$Domain_L2_specific[i]
dx$Domains = dx$Domain_L1_general
length(unique(dx$ID_article))
table(dx$Domains)

#### select ADHD - executive functions subdomains
disorder = "ADHD"; domains = "executive_functions"
dx = df[df$Target_disorder==disorder & (df$Domain_L1_general%in%c("Executive functions")),]
dx$Domains = dx$Domain_L2_specific
dx = dx[!is.na(dx$Domains), ]
length(unique(dx$ID_article))
table(dx$Domains)

############################################################

# PLOT WITH OVERLAY META-ANALYTIC DOTS

# Prepare data

dxx = data.frame(dx %>%
                   filter(!is.na(Effect_size), !is.na(Domains), !is.na(ID_article)) %>%
                   mutate(
                     Domains = as.factor(Domains),
                     ID_article = as.factor(ID_article)
                   ) %>%
                   group_by(ID_article, Domains) %>%
                   ungroup()
)
for(dom in unique(dxx$Domains)) dxx$Std_Err[is.na(dxx$Std_Err)] = max(dxx$Std_Err[dxx$Domains==dom],na.rm=T)
#dxx = dxx %>% filter(!is.na(dxx$Std_Err), !is.infinite(dxx$Std_Err))
dxx$Std_Err[is.infinite(dxx$Std_Err)] = NA
dxx$vi = dxx$Std_Err^2
dxx$dotDim = 1/dxx$Std_Err^0.8
if (!("Year" %in% names(dxx)))   dxx$Year = NA
if (!("Age_group" %in% names(dxx)))   dxx$Age_group = NA

# Fit 3-level models by domain
spl = split(dxx, dxx$Domains)
meta_by_domain = vector("list", length(spl))
names(meta_by_domain) = names(spl)
for (dom in names(spl)) {
  dat = spl[[dom]]
  if (length(unique(dat$ID_article)) >= 2) {
    V = vcalc(dat$vi, cluster = dat$ID_article, rho = 0.7)
    meta_by_domain[[dom]] = rma.mv(
      yi = Effect_size,
      V  = V,
      random = ~ 1 | ID_article/row_id,
      method = "REML",
      data = dat
    )
  } else {
    meta_by_domain[[dom]] = rma(
      yi = Effect_size,
      vi = vi,
      data = dat
    )
  }
}

meta_summary = do.call(
  rbind,
  lapply(names(meta_by_domain), function(dom) {
    m = meta_by_domain[[dom]]
    dat_dom = dxx %>% filter(Domains == dom)
    
    data.frame(
      Domains = dom,
      yi   = as.numeric(coef(m)),
      se   = as.numeric(m$se),
      ci.lb = as.numeric(m$ci.lb),
      ci.ub = as.numeric(m$ci.ub),
      tau2_total = sum(m$sigma2),
      tau_total = sqrt(sum(m$sigma2)),
      k_effects  = nrow(dat_dom),
      k_articles = length(unique(dat_dom$ID_article))
    )
  } )
)
meta_summary$Domains = factor(meta_summary$Domains, levels = levels(dxx$Domains))
print(meta_summary)

# Create combined plot (all dots + meta-meta-analysis)
ggplot() +
  theme_bw() +
  coord_flip(ylim=c(-1.5,max(c(0,dxx$Effect_size)))) +
  # (A) all extracted effects
  geom_point(
    data = dxx,
    aes(
      x = Domains, y = Effect_size,
      size = dotDim,
      colour = Age_group,
      shape  = Age_group
    ),
    alpha = 0.4,
    position = position_nudge(x = -0.12, y = 0)
  ) +
  # (B) meta-meta summary per domain (point + CI)
  geom_errorbar(
    data = meta_summary,
    aes(x = Domains, ymin = ci.lb, ymax = ci.ub),
    inherit.aes = FALSE,
    linewidth = 0.8,
    width = 0,
    position = position_nudge(x = 0.12, y = 0)
  ) +
  geom_point(
    data = meta_summary,
    aes(x = Domains, y = yi),
    inherit.aes = FALSE,
    size = 4,
    position = position_nudge(x = 0.12, y = 0)
  ) +
  ggtitle(disorder) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 1) +
  scale_color_manual(values = c("red", "#993333", "black")) +
  scale_shape_manual(values = c(17,18,15)) +
  labs(color = NULL, shape = NULL) +        # remove "Age_group" title
  guides(
    size  = "none",                          # remove dotDim legend
    color = guide_legend(
      override.aes = list(size = 5, alpha = 1)
    )
  ) +
  theme(
    text = element_text(size = ts),
    axis.title.y = element_blank()
  ) +
  ylab("Effect size") +
  scale_x_discrete(limits = rev(levels(dxx$Domains)))

############################################################
