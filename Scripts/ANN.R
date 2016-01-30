# decent rate
a<-0.2  

# parameter
w<-rep(0,3)

#Petal.Length Petal.Width
iris1<-t(as.matrix(iris[,3:4]))  

# Real class
d<-c(rep(0,50),rep(1,100)) 

# define new class
e<-rep(0,150)  

# define probability
p<-rbind(rep(1,150),iris1)  

# max times
max<-100000  

#
eps<-rep(0,100000)  

# times
i<-0  
repeat{  
    v<-w%*%p;  
    y<-ifelse(sign(v)>=0,1,0);  #y=sign(w*x+b)
    e<-d-y;  
    eps[i+1]<-sum(abs(e))/length(e)  #判断，若满足收敛条件，算法结束
    if(eps[i+1]<0.01){  
        print("finish:");  
        print(w);  
        break;  
    }  
    w<-w+a*(d-y)%*%t(p);  #w(n+1)=w(n)+a[d-y(n)]*x(n)
    i<-i+1;  
    if(i>max){  
        print("max time loop");  
        print(eps[i])  
        print(y);  
        break;  
    }  
} 

#绘图程序  
plot(Petal.Length~Petal.Width,xlim=c(0,3),ylim=c(0,8),  
data=iris[iris$Species=="virginica",])  
data1<-iris[iris$Species=="versicolor",]  
points(data1$Petal.Width,data1$Petal.Length,col=2)  
data2<-iris[iris$Species=="setosa",]  
points(data2$Petal.Width,data2$Petal.Length,col=3)  
x<-seq(0,3,0.01)  
y<-x*(-w[3]/w[2])-w[1]/w[2]  
lines(x,y,col=4)  
#绘制每次迭代的平均绝对误差  
plot(1:i,eps[1:i],type="o")  
