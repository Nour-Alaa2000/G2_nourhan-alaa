
Nourhan<-read.csv("G2_anthropometry.csv")
View(Nourhan) 


#) Re-code gender

Nourhan$Sex[Nourhan$gender=="F" ]="Female"
Nourhan$Sex[Nourhan$gender=="cm"]="Male"

#Dataset Sense
nrow(nourhan)
ncol(nourhan)
nourhan
str(nourhan)

#) show a rows that have NA

Nourhan[!complete.cases(Nourhan) , ]



#)Replace each NA in foot_length according to the mean of

male_mean<-mean(Nourhan[Nourhan$Sex=="Male" , 'foot_length' ], na.rm=T)
Nourhan[is.na(Nourhan$foot_length) & Nourhan$Sex=="Male"  , 'foot_length']=male_mea

female_mean<-mean(Nourhan[Nourhan$Sex=="Female" , 'foot_length' ], na.rm=T)
Nourhan[is.na(Nourhan$foot_length) & Nourhan$Sex=="Female"  , 'foot_length']=female_mean

#)Re_code age variable 
Nourhan$ageRange[Nourhan$age <= 10]="0 _ 5"
Nourhan$ageRange[Nourhan$age > 5 & Nourhan$age <= 10]="6 _ 10"
Nourhan$ageRange[Nourhan$age >5 ]="11 _ .."


# Remove cm from the height column
nourhan$height <- gsub("cm", "", nourhan$height)
nourhan

#)Re_code the height feature

mean_x<-mean(Nourhan$height)

Nourhan$heightCat<-as.factor(ifelse(Nourhan$height< mean_x & Nourhan$age >10,"Abnormal kid" 
                                   ,"Normal kid"))
#)Re_code of code

Nourhan$heightCat2[Nourhan$heightCat=="Normal kid"]='1'
Nourhan$heightCat2[Nourhan$heightCat=="Abnormal kid"]='2'
Nourhan$heightCat2<-as.factor(Nourhan$heightCat2)

#)ratio of normal and abnormal child 

x<-mean(Nourhan$heightCat2==0)
y<-mean(Nourhan$heightCat2==1)
x
y


#)Subset abnormal childs

subabnormal<-Nourhan[Nourhan$heightCat2== 1 , ]
#or
subabnormal<-Nourhan[Nourhan$heightCat=="Abnormal kid" ,]

#using mice function
nourhan[ ! complete.cases(), ]

dd.imp <- mice(nourhan , m = 5 , meth = c("","","pmm","","") , maxit = 4)

dd.imp$imp
nourhannew<-complete(dd.imp,5)
nourhan


#is na funcion
nourhan[is.na(nourhan$foot_length),]


#)using median
subfoot<-Nourhan[Nourhan$foot_length > median(Nourhan$foot_length) &
               Nourhan$height >= 100 ,c(5,10) ]
  
#)sort the data set ascending according to 2 variables

sort<-Nourhan[order(Nourhan$age ,Nourhan$height) , ]

#)Get only the first 3 rows
h<-head(Nourhan ,3)

library(ggplot2)

#) using scatter plot,name the figure

pic1<-ggplot(Nourhan , aes(x=foot_length  , y= height))
pic1+ geom_point() + ggtitle("The co_relation between the Height and Foot_length")


#)Show the distribution of Height histogram 
pic2<-ggplot(Nourhan , aes(height))
pic2 + geom_histogram(binwidth = 8)
pic2 + geom_histogram(fill = "green")+ ggtitle("Child's Height distribution") 

#)summarize the heightcat2 using Bar chart

pic3<-ggplot(Nourhan , aes(y=heightCat2  ,fill= Sex))
pic3 +geom_bar()+labs(m=" Heightcat count" ,title="Height category rate")
pic3 +geom_bar() +theme_light()+facet_wrap(~ageRange)
















