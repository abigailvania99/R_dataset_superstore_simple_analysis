names = c("Adi","Budi","Cindi", "Dedi")
factor(names)
factor(c("male","female","male","male","female"))
#factor kyk ada level tmpilin yg unik

m=matrix(1:12,nrow=3)
#matrix 1-12 tpi 3 baris
#m[2,3] baris 2 kolom 3

a=array(1:24,c(3,4,2))
#utk lebih dri 2 dimensi

users <- data.frame(
  names=names,
  gender=c("male","male","female","male"),
  age=c(10,20,30,40)
)

#cara akses kolom, pengen tau kolom 1 apa aja, users[1]

#bisa juga users["name"]
#bisa juga users[c("names","age")]
#users[2,] untuk melihat baris ke2

users[c("age","names")]

l=list(a,names,users)

library(dplyr)
dataset<- read.csv('skill academy/dataset_superstore_simple.csv')
summary(dataset)
head(dataset,10)
#melihat 10 dataset teratas

nrow(dataset)
#melihat berapa baris dataset

ncol(dataset)
#melihat berapa kolom dataset
write.csv(dataset,'dataset_new.csv')

library(dplyr)
dataset<- read.csv('skill academy/dataset_superstore_simple.csv')

glimpse(dataset)

select(dataset,order_id)

dataset_result<-select(dataset,order_id)
dataset_result<-select(dataset,c(order_id,order_date,sales))
dataset_result<-select(dataset,-c(profit))
#menghilangkn kolom profit

select(dataset_result,c(order_id,sales,customer_id))

dataset_result<-filter(dataset,segment == 'Consumer')
#menampilkan smua elemen tetapi segment hanya consumer

dataset_result<-filter(dataset,segment == 'Consumer'& profit>0)
                       
dataset_result<-filter(dataset,segment == 'Consumer'| profit>0)
dataset_result<-filter(dataset,segment != 'Consumer'| profit>0)


#buat kolom baru
mutate(dataset,avg_price = sales/quantity)
dataset_result<-mutate(dataset,avg_price = sales/quantity)

dataset_result<-transmute(dataset,avg_price=sales/quantity)
#utk menampilkan avgnya doang

dataset3<-dataset

dataset3$avg_price<-transmute(dataset,avg_price=sales/quantity)

dataset2 <- filter(dataset,segment=='Consumer')

dataset2<-mutate(dataset2,avg_price=sales/quantity)

dataset2<-select(dataset2,c(order_id,order_date,sales,avg_price))

#typing spya dataset2 gk terus2an dipanggil pake %>%
dataset3<-dataset %>% filter(segment="Consumer")%>%mutate(avg_price=sales/quantity)%>%select(c(order_id,order_date,sales,avg_price))

dataset %>% group_by(segment) %>% summarise(total_Sales = sum(sales))
#mnmpilkan msing2 segment dan total sales dri masing2 segment

dataset %>% group_by(segment) %>% summarise(total_Sales = sum(sales),avg_sales=mean(sales),min_quantity=min(quantity),max_quantity=max(quantity),n_order=n())
#n_order utk brp bnyk baris dalam segment tersebut

dataset %>% group_by(segment,category) %>% summarise(total_Sales = sum(sales),avg_sales=mean(sales),min_quantity=min(quantity),max_quantity=max(quantity),n_order=n())

#menggabungkan 2 jenis data frame berdasarkan baris

data_a <- dataset%>%filter(segment="Corporate")%>%
  select(c.(order_id,order_date,segment,category,sub_category,sales))%>%
  head(10)



data_b <- dataset%>%filter(category="Technology")%>%
  select(c.(order_id,order_date,segment,category,sub_category,sales))%>%
  head(9)

intersect(data_a,data_b) #memilih baris yg sama antara a dan b

union(data_a,data_b) #menggabungkan data_a dan data_b dan cukup menampilkan data 1x klo ada yg sama

bind_rows(data_a,data_b)#smua isi dataset ditumpuk klo ada yg sama

setdiff(data_a,data_b) #kebalikan dari intersectmenampilkan data_a diluar intersect

setdiff(data_b,data_a) #kebalikan dari intersectmenampilkan data_b diluar intersect


#menggabungkan data frame berdasarkan kolom

data_c <- select(data_a,c(order_id,sub_category,segment,sales))%>%head(9)

data_d<- select(data_a,c(order_id,sub_category,category, order_date))

bind_cols(data_c,data_d)#Error krn yg satu lengthnya 10, yg satu 9

inner_join(data_c,data_d)
full_join(data_c,data_d)
left_join(data_c,data_d)
right_join(data_c,data_d)




##################################GAMBAR###########################################

ggplot(dataset,aes(x=sales,y=profit))+geom_point(colour='blue')
#titik

#histogram
ggplot(dataset,aes(x=sales))+geom_histogram(bins=10)
ggplot(dataset,aes(x=sales))+geom_histogram(binwidth=500,colour="yellow")
ggplot(dataset,aes(x=quantity))+geom_histogram(binwidth=1,colour="yellow",fill="red")


#barchart
ggplot(dataset,aes(x=segment,y=sales))+geom_bar(stat="identity",aes(fill=category),width = 0.5)

#piechart
sales_per_segment<-dataset%>%group_by(segment)%>%summarise(total_sales=sum(sales))

ggplot(sales_per_segment,aes(x="",y=total_sales,fill=segment))+geom_bar(stat="identity",width = 1)+coord_polar("y",start=0)

#linechart
dataset$order_date<-as.Date(dataset$order_date)

dataset$order_month <- as.Date(cut(dataset$order_date,breaks="month"))

ggplot(dataset,aes(x=order_month,y=sales))+stat_summary(fun.y=sum,geom='line')

monthly_sales<-dataset%>%group_by(order_month)%>%summarise(total_sales=sum(sales))

ggplot(monthly_sales,aes(x=order_month,y=total_sales))+geom_line()+geom_point(colour="blue")


############Advance plot#################

plot1<-ggplot(dataset,aes(x=sales,y=profit))+geom_point(aes(colour=category),shape=2,size=4)+geom_smooth(method='lm',colour='red',linetype='dashed')+
  labs(title='Scatterplot Sales vs Profit',
       subtitle="Based on Dataset Superstor",
       caption='R Language Tutorial')

plot2<-plot1+
  xlab("Order Sales")+ylab("Order Profit")+
  xlim(c(0,7500))+ylim(c(-2500,2500))+theme(
    plot.title=element_text(color='blue',size=15,face='bold'),
    plot.subtitle=element_text(size=13,face='italic')
    
  )+theme(
    legend.position = "bottom",
    #bisa juga "bottom" diganti c(0.8,0.2)
    legend.title = element_text(color = 'blue',size = 12,face = 'bold'),
    legend.text = element_text(color='red')
  )
plot2

#utk save
#ggsave('test.png',plot1)

