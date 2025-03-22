library(rio)
data=import("/Users/loannguyen/Documents/SQL/UpdatedHMDA.csv")
#head(data,5)
#correlation analysis
round(cor(data$deny,data,method="pearson"),digits=2)

library(reshape2)
melted=melt(round(cor(data),digits=2))
head(melted)
#write.csv(melted,"/Users/loannguyen/Documents/SQL/MeltedHMDA.csv",row.names=FALSE)

library(ggplot2)
ggplot(data=melted,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  scale_fill_gradient2(low="red",high="blue",mid="white",
                       midpoint=0,limit=c(-1,1),space="Lab",
                       name="Correlation")+theme_minimal()+
  theme(axis.text.x=element_text(angle=45,vjust=1,size=12,hjust=1))+
  coord_fixed()+annotate("rect",xmin=.5,xmax=14.5,ymin=.5,ymax=1.5,
                         color="red",fill=NA,size=1)+
  annotate("rect",xmin=.5,xmax=1.5,ymin=.5,ymax=14.5,color="red",fill=NA,
           size=1)+geom_text(aes(label=value),color="black",size=3)+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())

#regression analysis
model=glm(deny~pirat+hirat+lvrat+chist+mhist+phist+unemp+selfemp+insurance+
            condomin+afam+single+hschool,family=binomial(link="logit"),data=data)
summary(model)
