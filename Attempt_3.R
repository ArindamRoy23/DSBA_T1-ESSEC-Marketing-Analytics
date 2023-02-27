# Load the package
library(RODBC)
library(tidyr)

# Connect to MySQL (my credentials are mysql_server_64/root/.)
db = odbcConnect("mysql_server_64", uid="root", pwd="root")
sqlQuery(db, "USE ma_charity")

# Extract calibration data from database
query = "SELECT a.contact_id,
                DATEDIFF(20220530, MAX(a.act_date)) / 365 AS 'recency',
                COUNT(a.amount) AS 'frequency',
                AVG(a.amount) AS 'avgamount',
                MAX(a.amount) AS 'maxamount',
                assignment2.donation AS 'loyal',
                assignment2.amount AS 'targetamount'
         FROM ma_charity.acts a
         LEFT JOIN ma_charity.assignment2
		 on  assignment2.contact_id = a.contact_id
         WHERE calibration = 1
         GROUP BY 1"
data = sqlQuery(db, query)

# data = data %>% replace_na(list(targetamount = 0))
# Show data
print(head(data))

# In-sample, probability model
library(nnet)
prob.model = multinom(formula = loyal ~ (recency * frequency) + log(recency) + log(frequency),
                      data = data)


# In-sample, donation amount model
# Note that the amount model only applies to a subset of donors...
z = which(!is.na(data$targetamount))
print(head(data[z, ]))
amount.model = lm(formula = log(targetamount) ~ log(avgamount) + log(maxamount),
                  data = data[z, ])

# Extract prediction data from database
query = "SELECT a.contact_id,
                DATEDIFF(20220530, MAX(a.act_date)) / 365 AS 'recency',
                COUNT(a.amount) AS 'frequency',
                AVG(a.amount) AS 'avgamount',
                MAX(a.amount) AS 'maxamount',
                assignment2.donation AS 'loyal',
                assignment2.amount AS 'targetamount'
         FROM ma_charity.acts a
         LEFT JOIN ma_charity.assignment2
		 on  assignment2.contact_id = a.contact_id
         WHERE calibration = 0
         GROUP BY 1"
newdata = sqlQuery(db, query)
print(head(newdata))




query = "SELECT contact_id from  
         ma_charity.assignment2
         WHERE calibration = 0
         GROUP BY 1
         order by contact_id"
finaldata = sqlQuery(db, query)
print(head(finaldata))
# Close the connection
odbcClose(db)

# Out-of-sample predictions
# Do NOT forget to re-transform "log(amount)" into "amount"
out = data.frame(contact_id = newdata$contact_id)
out$probs  = predict(object = prob.model, newdata = newdata, type = "probs")
out$amount = exp(predict(object = amount.model, newdata = newdata))
out$score  = out$probs * out$amount

# Show results
print(head(out))

# Who is likely to be worth more than 2 EUR?
z = which(out$score > 2)
print(length(z))
nrow(out)
out[out$score > 2,]
out$Pred <- ifelse(out$score > 2, 1, 0)
print(tail(out))
final_out = out[c('contact_id','Pred')]
result_df = merge(x = finaldata, y = final_out, by = "contact_id", all.x = TRUE)

write.csv(result_df,"C://Users//arind//OneDrive//Desktop//Classes//T1 ESSEC Marketing Analytics//attempt_2.csv",
          row.names = FALSE, col.names = FALSE, sep = "\t")