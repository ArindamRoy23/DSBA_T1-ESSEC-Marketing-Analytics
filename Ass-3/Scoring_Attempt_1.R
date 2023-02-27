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
data = data %>% mutate(value = 1)  %>% spread(act_type_id, value,  fill = 0) 
data = data%>% mutate(value = 1)  %>% spread(payment_method_id, value,  fill = 0)
data = data%>% mutate(value = 1)  %>% spread(channel_id, value,  fill = 0)


data$act_date <- NULL
data$act_type_id <- NULL
data$payment_method_id <- NULL
data$id <- NULL
data$channel_id <- NULL
data$message_id <- NULL
data$campaign_id <- NULL

# Adding count data to the data
count_df = data %>% count(contact_id)
data = merge(x=data,y=count_df,by='contact_id',all.x=TRUE)
data = data %>% rename( frequency = n)

# Adding target amount
train_df = data %>% filter(recency > 1)
test_df = data 
train_df$recency = train_df$recency - 1
train_df$targetamount = train_df$amount / '^'(train_df$recency + 0.5,2)
train_df = train_df %>% mutate(loyal = if_else(recency<1, 1,0))
train_df = train_df %>% group_by(contact_id) %>% summarise(recency = min(recency), frequency = mean(frequency) , 
                                                           avgamount = mean(amount), maxamount = max(amount),
                                                           loyal = max(loyal), targetamount = sum(targetamount))

z = which(!is.na(train_df$targetamount))
print(head(train_df[z, ]))
amount.model = lm(formula = log(targetamount) ~ log(avgamount) + log(maxamount),
                  data = train_df[z, ])
prob.model = multinom(formula = loyal ~ (recency * frequency) + log(recency) + log(frequency),
                      data = train_df)

test_df$targetamount = test_df$amount / '^'(test_df$recency + 0.5,2)
test_df = test_df %>% mutate(loyal = if_else(recency<1, 1,0))
test_df = test_df %>% group_by(contact_id) %>% summarise(recency = min(recency), frequency = mean(frequency) , 
                                                         avgamount = mean(amount), maxamount = max(amount),
                                                         loyal = max(loyal), targetamount = sum(targetamount))
