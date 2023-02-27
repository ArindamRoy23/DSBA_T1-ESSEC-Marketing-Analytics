  # Load the package
  library(RODBC)
  library(tidyr)
  library(dplyr)
  library(nnet)
  
  # Connect to MySQL (my credentials are mysql_server_64/root/.)
  db = odbcConnect("mysql_server_64", uid="root", pwd="root")
  sqlQuery(db, "USE ma_charity")
  query = "
  select *,datediff(20221231,act_date)/365 as recency,
  month(act_date) as month
  from ma_charity.acts
  "
  
  data = sqlQuery(db, query)
  # Close the connection
  odbcClose(db)
  
  # OHE important variables
  data = data %>% mutate(value = 1)  %>% spread(month, value,  fill = 0 ) 
  # data = data %>% mutate(value = 1)  %>% spread(act_type_id, value,  fill = 0) 
  # data = data%>% mutate(value = 1)  %>% spread(payment_method_id, value,  fill = 0)
  # data = data%>% mutate(value = 1)  %>% spread(channel_id, value,  fill = 0)
  
  
  data$act_date <- NULL
  data$act_type_id <- NULL
  data$payment_method_id <- NULL
  data$id <- NULL
  data$channel_id <- NULL
  data$message_id <- NULL
  data$campaign_id <- NULL
  months_col = list('mo_1','mo_2','mo_3','mo_4','mo_5','mo_6','mo_7','mo_8','mo_9','mo_10','mo_11','mo_12')
  data = data %>% rename(mo_1=4,mo_2=5,mo_3=6,mo_4=7,mo_5=8,mo_6=9,mo_7=10,mo_8=11,mo_9=12,mo_10=13,mo_11=14,mo_12=15)
  
  data$am_by_rec = (data$amount)/ (data$recency + 1)
  
  
  for (a in months_col){
    data[,a] = data[,a] * data[,'am_by_rec']
  }
  
  data = data %>% group_by(contact_id) %>% summarise(mo_sum_1=sum(mo_1),mo_sum_2=sum(mo_2),mo_sum_3=sum(mo_3),mo_sum_4=sum(mo_4),mo_sum_5=sum(mo_5),mo_sum_6=sum(mo_6),mo_sum_7=sum(mo_7),mo_sum_8=sum(mo_8),mo_sum_9=sum(mo_9),mo_sum_10=sum(mo_10),mo_sum_11=sum(mo_11),mo_sum_12=sum(mo_12))
  # 
  
  
  placeholder_cid = data$contact_id
  
  data$contact_id<- NULL
  
  final_data = data %>% ungroup() %>% mutate(across(where(is.numeric))/rowSums(across(where(is.numeric))))
  
  # final_data$month_to_target = colnames(data)[apply(data,1,which.max)]
  final_data$contact_id = placeholder_cid
  final_data_probab = final_data %>% pivot_longer(!contact_id, names_to = "month", values_to = "probab")
  final_data_probab[final_data_probab == 0] <- NA
  final_data_probab = final_data_probab[complete.cases(final_data_probab),]
  write.csv(final_data_probab, "C://Users//arind//OneDrive//Desktop//Classes//T1 ESSEC Marketing Analytics//Ass-3//output_files//probab.csv", row.names=FALSE)