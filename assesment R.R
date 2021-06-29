# 1. Carilah customer_id yang memiliki sales paling besar!
#   2. Sub-category apa saja yang ada di dalam category 'Office Supplies', dan
# masing-masing berapa banyak total profitnya?
#   3. Berapa banyak order yang menghasilkan profit negatif (rugi)?
#   4. Antara 3 customer_id ini, mana yang total sales-nya paling banyak: JE-16165, KH-16510, AD-10180?
#   5. Buatlah data frame bernama 'yearly_sales' yang berisi total sales, jumlah
# customers, dan total profit tiap tahun. Tahun berapa profit tertinggi diperoleh?
#   6. Buatlah scatterplot antara sales dan profit untuk data di tahun 2014-2015 saja, bedakan warnanya antara tahun 2014 dan tahun 2015. Beri judul 'Sales vs Profit
# 2014-2015'!
#   7. Buatlah barchart yang berisi total profit dari 10 customer dengan total sales tertinggi di tahun 2015!

#shortcut ctrl+shift+c


library(dplyr)
dataset<- read.csv('skill academy/dataset_superstore_simple.csv')

# 1. Carilah customer_id yang memiliki sales paling besar!
dataset %>% filter(sales == max(sales)) %>% select(customer_id)

# 2. Sub-category apa saja yang ada di dalam category 'Office Supplies', dan masing-masing berapa banyak total profitnya?

dataset %>%filter(category=='Office Supplies') %>%
  group_by(sub_category)%>%
  summarise(total_profit = sum(profit))%>%
  select(sub_category,total_profit)

#   3. Berapa banyak order yang menghasilkan profit negatif (rugi)?
dataset%>%filter(profit<0)%>%summarise(n())

#   4. Antara 3 customer_id ini, mana yang total sales-nya paling banyak: JE-16165, KH-16510, AD-10180?
total_sales1=dataset %>% filter(customer_id == 'JE-16165')%>%summarise(total_sales = sum(sales))
total_sales2=dataset %>% filter(customer_id == 'KH-16510')%>%summarise(total_sales = sum(sales))
total_sales3=dataset %>% filter(customer_id == 'AD-10180')%>%summarise(total_sales = sum(sales))

hasil_sales = max(total_sales1,total_sales2,total_sales3)
hasil_sales

if(hasil_sales == total_sales1){
  dataset %>% group_by(customer_id)%>%summarise(total_sales = sum(sales))%>%filter(customer_id == 'JE-16165')
}else if(hasil_sales == total_sales2){
  dataset %>% group_by(customer_id)%>%summarise(total_sales = sum(sales))%>%filter(customer_id == 'KH-16510')
}else{
  dataset %>% group_by(customer_id)%>%summarise(total_sales = sum(sales))%>%filter(customer_id == 'AD-10180')
}

#nomor 5
dataset$order_date<-as.Date(dataset$order_date)

dataset$order_year <- as.Date(cut(dataset$order_date,breaks="year"))

yearly_Sales<-dataset%>%group_by(order_year)%>%summarise(sales=sum(sales),profit=sum(profit),jumlah_customer=n())


#nomor 6
yearly_2014_2015 <- dataset %>% select(order_year,sales,profit) %>% filter(order_year=="2014-01-01" | order_year=="2015-01-01")

ggplot(yearly_2014_2015,aes(x=sales,y=profit)) + geom_point (aes(color = order_year)) + labs(title = "Sales vs Profit 2014-2015")

#nomor 7
yearly_2015 <- dataset %>% select(order_year,customer_id,sales,profit) %>% 
  filter(order_year=="2015-01-01" & sales > 0) %>% 
  group_by(customer_id) %>% summarise(sales=sum(sales),profit=sum(profit)) %>% 
  arrange(desc(sales))

tahun2015_sales10tertinggi <- yearly_2015 %>% head(10)

tahun2015_sales10tertinggi
ggplot(tahun2015_sales10tertinggi,aes(x=profit,y=sales)) + 
  geom_bar(stat='identity',aes(fill=customer_id)) + 
  labs(title = "10 customer dengan total sales tertinggi tahun 2015")
