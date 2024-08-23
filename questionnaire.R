form=read.csv('D:/form.csv',header = T)
omitdata=na.omit(form)
z=omitdata[,-(2:5)]
varna=princomp(z)
l=(varna$sdev)^2 
par(mfrow=c(1,2))
plot(l,type="b",xlab="component",ylab="lambda",main="Scree plot")
plot(cumsum(l)/sum(l)*100,ylim=c(0,100),type="b",xlab="component",ylab="cumulative propotion (%)",main="Cum. Scree plot")
abline(a=80,b=0,col="red")
dataa=as.matrix(z)
heatmap(dataa)
dataa=as.matrix(omitdata)
heatmap(dataa)
library(psych)
fa(omitdata,8)
fa(z,8)
factanal(omitdata,4)
cor(omitdata)
varna=princomp(omitdata)
varna=princomp(z)
l=(varna$sdev)^2 
plot(l,type="b",xlab="component",ylab="lambda",main="Scree plot")
plot(cumsum(l)/sum(l)*100,ylim=c(0,100),type="b",xlab="component",ylab="cumulative propotion (%)",main="Cum. Scree plot")
abline(a=90,b=0,col="red")
S<-var(omitdata)
sum(diag(S))   #matrix sigma(cov)
(eigens<-eigen(S))
cumsum(eigens$values)/sum(eigens$values)
spr <-princomp(omitdata)   #mohasebe pca
U<-spr$loadings    #barhaye ameli #cor beyn moalefeha
L<-(spr$sdev)^2
(spr$sdev^2)/sum(spr$sdev^2)
fa(omitdata,6)
heatmap(cor(omitdata))
attach(omitdata)
cor(Q2.2,Q2.1,method = 'spearman')
cor(Q20,Q12,method = 'spearman')
library(aplpack)
faces(omitdata)

barplot(dataa[,(1:8)],col=c("blue", "grey",'red','pink','green'))
legend("top",legend = c("????????", "????",'???? ??????','????????','?????????? ????????')
       ,fill = c("blue", "grey",'red','pink','green'))
par(mfrow=c(3,1))
q3=t(prop.table(table(z$'Q3')))
barplot(q3,ylab='percentage',col='grey',xlab = '??????????')
q5=t(prop.table(table(z$'Q5')))
barplot(q5,ylab='percentage',col='grey',xlab = '?????? ?? ????????????')
q4b=t(prop.table(table(z$'Q7')))
barplot(q4b,ylab='percentage',col='grey',xlab = '?????? ???? ?????????? ???????? ?? ??????')



count=table(omitdata)
count=table(z[,(1:8)])
count=table(z[,(1:2)])
count=table(omitdata[,c(2,5)])
mosaicplot(count,xlab = "????????",ylab = "?????????? ????????")
library(vcd)
mosaic(omitdata)

library(packcircles)
library(ggplot2)
data <- data.frame(group=paste(z[,(1:8)]), value=c(1:8)) 
packing <- circleProgressiveLayout(data$value, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  geom_text(data = data, aes(x, y, size=value, label = group)) +
  scale_size_continuous(range = c(1,4)) +
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()

library(ggplot2)
library(dplyr)
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)
ggplot(z[,(1:8)], aes(x, y, size)) +
  geom_point(alpha=0.7)

par(mfrow=c(1,2))
pie(table(ï..Q1),labels = c('????????','??????'),main='??????????')
pie(table(Q2),labels = c('??????','??????'),main='?????? ???????? ??????????????')
pie(table(Q3))
pie(table(Q4))

lbs=c('????????','????','???? ??????','????????','?????????? ????????')
survey=omitdata %>%
  dplyr::mutate_if(is.character,factor) %>%
  dplyr::mutate_if(is.numeric,factor,levels=1:5,lbs) %>%
  as.data.frame()
plot(likert(survey[,6:38]),ordered=F,wrap=60)
plot(likert(survey[,6:38]),grouping=survey[,1])

source('itempersonmap.R')
items=select(omitdata,starts_with(c('Q3'))) %>%
  apply(.,2,function(x) x-1)
library(mirt)
mod= mirt(items,1,itemtype = 'Rasch',verbose = FALSE)
library(eRm)
plotPImap(mod)
res=PCM(omitdata[,6:38])
plotPImap(res,sorted = TRUE)

library(survey)
data(omitdata)
glimpse(apisrs)
apisrs_design <- svydesign(data = omitdata, weights = ~Q3, fpc = ~Q9, id = ~1)

omitdata %>%
  ggplot(aes(x = response)) + 
  geom_step(aes(y = ..y..), stat = "ecdf") +
  labs(y = "Cumulative Density") + 
  scale_x_discrete(limits = c("1","2","3","4","5"), 
                   breaks = c(1,2,3,4,5),
                   labels=c("1", "2", 
                            "3", "4", "5")) + theme_bw() 

library(ggplot2)
library(dplyr)
ggplot(omitdata, aes(x=Q3, y=Q5, size = Q2)) +
  geom_point(alpha=0.7)

mosaicplot(omitdata ,main = "Title")
table1 <- table(omitdata$Q2, omitdata$Q2.3)
mosaicplot(omitdata,xlab = "????????",ylab = "?????????? ????????")
