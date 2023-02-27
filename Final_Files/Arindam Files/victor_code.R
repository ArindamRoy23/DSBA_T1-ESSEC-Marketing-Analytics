# Amount of money collected every year
query1 = "SELECT YEAR(act_date), act_type_id, SUM(amount) 
          FROM ma_charity.acts 
          GROUP BY 1,2 
          ORDER BY 1"
df1 = sqlQuery(db, query1)

colnames(df1)[colnames(df1) == 'YEAR(act_date)'] <- 'year'
colnames(df1)[colnames(df1) == 'SUM(amount)'] <- 'amount'

ggplot(data=df1, aes(x=year, y=amount, fill=act_type_id)) +
  ggtitle("Amount of money collected every year") +
  labs(y = "Amount of money", x = "Year")+
  scale_fill_manual(name="Type of donation",
                    breaks=c("DO", "PA"),
                    labels=c("Classic donation", "Automatic deduction")
                    ,values = c("DO" = "#E63946",
                                "PA" = "#A8DADC"))+
  geom_bar(position='dodge', stat="identity", color="black") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",hjust=0.5), axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))


# Amount of money collected by type of donation
query2 = "SELECT act_type_id, COUNT(*)
      FROM ma_charity.acts
      GROUP BY ma_charity.acts.act_type_id"
df2 = sqlQuery(db, query2)

colnames(df2)[colnames(df2) == 'act_type_id'] <- 'type_of_donation'
colnames(df2)[colnames(df2) == 'COUNT(*)'] <- 'amount'

ggplot(data = df2, aes(x=type_of_donation, y=amount, fill=type_of_donation)) +
  ggtitle("Amount of money collected by type of donation") +
  labs(y = "Amount of money", x = "Type of donation")+
  scale_fill_manual(name="Type of donation",
                    breaks=c("DO", "PA"),
                    labels=c("Classic donation", "Automatic deduction")
                    ,values = c("DO" = "#E63946",
                                "PA" = "#A8DADC"))+
  geom_bar(stat="identity", color="black") +
  geom_text(aes(label=amount), vjust=1.6, color="black", size=4.5)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",hjust=0.5), axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))


# How many contacts are there in the database? How many donors? gender/age? Regression?
query3 = "SELECT (SELECT COUNT(DISTINCT id) FROM ma_charity.contacts) as nb_contacts,
       (SELECT COUNT(DISTINCT contact_id) FROM ma_charity.acts) as nb_donors
FROM DUAL"
df3 = sqlQuery(db,query3)

df3_final = data.frame(type=c("Contact","Donor"),number=as.numeric(as.vector(df3[1,])))

ggplot(data = df3_final, aes(x=type, y=number,fill=type)) +
  ggtitle("Number of contacts and donors in the database") +
  labs(y = "Number", x = "Total contact and proportion of donor in the database")+
  scale_fill_manual(name="Type of people",
                    breaks=c("Contact", "Donor"),
                    values = c("Contact" = "#E63946",
                               "Donor" = "#A8DADC"))+
  geom_bar(stat="identity", color="black") +
  geom_text(aes(label=number), vjust=1.6, color="black", size=4.5)+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",hjust=0.5), axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))


# Number of distinct donors each year
query4 = "SELECT YEAR(act_date) as year, COUNT(DISTINCT contact_id) as nb_distinct_donors
          FROM ma_charity.acts 
          GROUP BY 1 
          ORDER BY 1"
df4 = sqlQuery(db,query4)

ggplot(data = df4, aes(x = year, y = nb_distinct_donors, fill=year)) +
  ggtitle("Number of distinct donors each year") +
  labs(y = "Number of distinct donors", x = "Year")+
  geom_bar(stat="identity", color="black",fill="#457B9D") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",hjust=0.5), axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))


# Is the average donation amount increasing or decreasing? How much is it?
query5 = "SELECT YEAR(act_date) as year, act_type_id, AVG(amount) as avg_amount 
          FROM ma_charity.acts 
          GROUP BY 1,2 
          ORDER BY 1"
df5 = sqlQuery(db,query5)

ggplot(data=df5, aes(x=year, y=avg_amount, fill=act_type_id)) +
  ggtitle("Average donation by year") +
  labs(y = "Average donation", x = "Year")+
  scale_fill_manual(name="Type of donation",
                    breaks=c("DO", "PA"),
                    labels=c("Classic donation", "Automatic deduction")
                    ,values = c("DO" = "#E63946",
                                "PA" = "#A8DADC"))+
  geom_bar(position='dodge', stat="identity", color="black") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",hjust=0.5), axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))



# Top 10 campaign by average classic donation and automatic deductions per day
query6 = "select 
if(act_type_id = 'PA', 'automatic deduction', 'one-off donation') act_type,
campaign_id, 
sum(amount), 
max(act_date) - min(act_date) as days,
sum(amount)/(max(act_date) - min(act_date) ) as average_per_day

from ma_charity.acts
group by act_type_id, campaign_id
order by sum(amount) DESC
"

df6 = sqlQuery(db,query6)

df6_classic_donation = df6[df6$act_type=="one-off donation",]
df6_classic_donation_sorted = df6_classic_donation[order(df6_classic_donation$average_per_day, decreasing = TRUE),]
top10_df6_classic_donation_sorted = head(df6_classic_donation_sorted,10)

ggplot(data = top10_df6_classic_donation_sorted, aes(x = reorder(campaign_id,average_per_day), y = average_per_day, fill=campaign_id)) +
  ggtitle("Top 10 campaign by average classic donation per day") +
  labs(y = "Average classic donation per day", x = "Campaign")+
  geom_bar(stat="identity", color="black") +
  coord_flip() +
  scale_fill_discrete(name = "Campaign") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",hjust=0.5), axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))


df6_automatic_deduction = df6[df6$act_type=="automatic deduction",]
df6_automatic_deduction_sorted = df6_automatic_deduction[order(df6_automatic_deduction$average_per_day, decreasing = TRUE),]
top10_df6_automatic_deduction_sorted = head(df6_automatic_deduction_sorted,10)

ggplot(data = top10_df6_automatic_deduction_sorted, aes(x = reorder(campaign_id,average_per_day), y = average_per_day, fill=campaign_id)) +
  ggtitle("Top 10 campaign by average automatic deduction per day") +
  labs(y = "Average automatic deduction per day", x = "Campaign")+
  geom_bar(stat="identity", color="black") +
  coord_flip() +
  scale_fill_discrete(name = "Campaign") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",hjust=0.5), axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))


# Yearly amount of classic donations and automatic deductions by channel
query7 = "select if(act_type_id = 'PA', 'automatic deduction', 'one-off donation') act_type,
(CASE 
	WHEN channel_id IS NULL THEN 'NULL'
    WHEN channel_id = 'MA' THEN 'paper-based direct marketing campaigns'
    WHEN channel_id = 'WW' THEN 'online'
    WHEN channel_id = 'SB' THEN 'Spontaneous'
    ELSE channel_id 
END ) channel_id
, 
year(act_date), sum(amount) from ma_charity.acts
group by year(act_date), channel_id,act_type_id
order by year(act_date), channel_id"

df7 = sqlQuery(db,query7)
colnames(df7)[colnames(df7) == 'year(act_date)'] <- 'year'
colnames(df7)[colnames(df7) == 'sum(amount)'] <- 'sum_amount'


df7_classic_donation = df7[df7$act_type == "one-off donation",]

ggplot(data = df7_classic_donation, aes(x = year, y = sum_amount, fill=channel_id)) +
  ggtitle("Yearly amount of classic donations by channel") +
  labs(y = "Amount of classic donations", x = "Year")+
  geom_bar(position='dodge', stat="identity", color="black") +
  scale_fill_manual(name = "Channel",breaks=c("paper-based direct marketing campaigns", "Spontaneous","online"),
                    labels=c("Paper-based", "Spontaneous","Online"),values = c("paper-based direct marketing campaigns" = "#E63946",
                                                                               "Spontaneous" = "#A8DADC",
                                                                               "online" = "#457B9D")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",hjust=0.5), axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))


df7_automatic_deduction = df7[df7$act_type == "automatic deduction",]

ggplot(data = df7_automatic_deduction, aes(x = year, y = sum_amount, fill=channel_id)) +
  ggtitle("Yearly amount of automatic deductions by channel") +
  labs(y = "Amount of classic donations", x = "Year")+
  geom_bar(position='dodge', stat="identity", color="black") +
  scale_fill_manual(name = "Channel",breaks=c("paper-based direct marketing campaigns", "Spontaneous","online"),
                    labels=c("Paper-based", "Spontaneous","Online"),values = c("paper-based direct marketing campaigns" = "#E63946",
                                                                               "Spontaneous" = "#A8DADC",
                                                                               "online" = "#457B9D")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",hjust=0.5), axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))