# Load the packages
library(RODBC)
library(CLVTools)

# Connect to MySQL (my credentials are mysql_server_64/root/root)
db = odbcConnect("mysql_server_64", uid="root", pwd="")
sqlQuery(db, "USE ma_charity_full")

# Extract data from database
query = "SELECT contact_id, act_date, amount
         FROM acts
         WHERE (act_type_id LIKE 'DO') AND (act_date > 19900101)
         ORDER BY 1, 2;"
data = sqlQuery(db, query)
print(head(data))

# It's usually better to work with cohort-specific samples
# To restrict sample to people acquired N years ago, use the following query instead (below, 4 years)
# query = "SELECT contact_id, act_date, amount
#          FROM acts
#          WHERE (act_type_id LIKE 'DO') AND (contact_id IN (SELECT contact_id
#                                                            FROM acts
# 							                                        WHERE (act_type_id LIKE 'DO')
#                                                            GROUP BY 1
#                                                            HAVING TIMESTAMPDIFF(year, MIN(act_date), 20180626) = 4))
#          ORDER BY 1, 2;"
# data = sqlQuery(db, query)
# print(head(data))

# Prepare data in the right format
new.data = clvdata(data.transactions = data, date.format = "ymd", time.unit = "years", name.id = "contact_id", name.date = "act_date", name.price = "amount")

# Estimate model
est.pnbd = pnbd(clv.data = new.data)

# Report model parameters, confidence intervals
print(summary(est.pnbd))
print(coef(est.pnbd))
print(confint(est.pnbd))
print(plot(est.pnbd))

# Predict CLV over the next 10 years
results = predict(object = est.pnbd, prediction.end = 10)
print(results)
