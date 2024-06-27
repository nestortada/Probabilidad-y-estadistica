#x=c(180,180,182,174,180,179,172,172,178,178,177,185,186,170,182)#
y=c(158,160,168,163,166,167,165,152,153,163)
dats=summary(y)
m=length(y)
qm=quantile(y,type = 6)
#qm#
ric=qm[4]-qm[2]
ric
limif=qm[2]-1.5*ric
limsup=qm[4]+1.5*ric
lim=c(limif,limsup)
lim
bp=boxplot(y, horizontal = T)
bp
n=length(x)
media=sum(x)/n
mediam=sum(y)/m
desv=sd(x)
desvm=sd(y)
cv=desv*100/media
cvm=desvm*100/mediam
r=c(media,desv,cv)
r
j=c(mediam,desvm,cvm)
j