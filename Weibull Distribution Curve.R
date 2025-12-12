library(tidyr)
library(ggplot2)
library(patchwork)

t<-c(seq(0.01,10,0.01))

f1<-dweibull(t,0.5,1)
f2<-dweibull(t,1,1)
f3<-dweibull(t,1.5,1)
f4<-dweibull(t,3,1)
S1<-log(1-(pweibull(t,0.5,1)))
S2<-log(1-(pweibull(t,1,1)))
S3<-log(1-(pweibull(t,1.5,1)))
S4<-log(1-(pweibull(t,3,1)))
h1<-f1/(1-(pweibull(t,0.5,1)))
h2<-f2/(1-(pweibull(t,1,1)))
h3<-f3/(1-(pweibull(t,1.5,1)))
h4<-f4/(1-(pweibull(t,3,1)))

df1<-data.frame(t,f1,f2,f3,f4)
ldf1<-pivot_longer(df1,cols=c(f1,f2,f3,f4))
ldf1$name<-factor(ldf1$name,levels=c("f1","f2","f3","f4"),labels=c("0.5","1","1.5","3"))
y1<-ggplot(ldf1,aes(x=t,y=value,color=name))+
  geom_line(linewidth=1)+
  labs(title="Density Curve of Weibull Distribution",x="T",y="f(t)")+
  xlim(0,5)+
  ylim(0,1.25)+
  theme_minimal()

df2<-data.frame(t,S1,S2,S3,S4)
ldf2<-pivot_longer(df2,cols=c(S1,S2,S3,S4))
ldf2$name<-factor(ldf2$name,levels=c("S1","S2","S3","S4"),labels=c("0.5","1","1.5","3"))
y2<-ggplot(ldf2,aes(x=t,y=value,color=name))+
  geom_line(linewidth=1)+
  labs(title="Survival Curve of Weibull Distribution",x="T",y="S(t)")+
  xlim(0,4)+
  ylim(-2,0)+
  theme_minimal()

df3<-data.frame(t,h1,h2,h3,h4)
ldf3<-pivot_longer(df3,cols=c(h1,h2,h3,h4))
ldf3$name<-factor(ldf3$name,levels=c("h1","h2","h3","h4"),labels=c("0.5","1","1.5","3"))
y3<-ggplot(ldf3,aes(x=t,y=value,color=name))+
  geom_line(linewidth=1)+
  labs(title="Hazard Curve of Weibull Distribution",x="T",y="h(t)")+
  xlim(0,1)+
  ylim(0,3)+
  theme_minimal()
y1+y2+y3