# Load the package
library(RODBC)

# Connect to MySQL (my credentials are mysql_server_64/root/.)
db = odbcConnect("mysql_server_64", uid="root", pwd="root")
sqlQuery(db, "USE ma_charity")

# Extract data from database
query = "Select * from ma_charity.assignment2 as A left join 
(SELECT a.contact_id,
                DATEDIFF(20210530, MAX(a.act_date)) / 365 AS 'recency',
                COUNT(a.amount) AS 'frequency',
                AVG(a.amount) AS 'avgamount',
                DATEDIFF(20210530, MIN(a.act_date)) / 365 AS 'firstdonation',
                IF(c.counter IS NULL, 0, 1) AS 'loyal'
         FROM ma_charity.acts a
         LEFT JOIN (SELECT contact_id, COUNT(amount) AS counter
                    FROM ma_charity.Acts
                    WHERE (act_date >= 20210530) AND
                          (act_date <  20220530) AND
                          (act_type_id = 'DO')
                    GROUP BY contact_id) AS c
         ON c.contact_id = a.contact_id
         WHERE (act_type_id = 'DO') AND (act_date < 20210530)
         GROUP BY 1) b on a.contact_id = b.contact_id"
data = sqlQuery(db, query)

# Close the connection
odbcClose(db)

# Assign contact id as row names, remove id from data
rownames(data) = data$contact_id
data = data[, -1]

# One of the libraries available for (multinomial) logit model
library(nnet)

# Compute the logit model on the entire data set
# These are the predictions you need to use later on
formula = "loyal ~ (recency * frequency) + log(recency) + log(frequency)"
model = multinom(formula, data)

# Run a nfold cross-validation
nfold = 5
nobs  = nrow(data)
index = rep(1:nfold, length.out = nobs)
probs = rep(0, nobs)
for (i in 1:nfold) {

   # Assign in-sample and out-of-sample observations
   insample  = which(index != i)
   outsample = which(index == i)

   # Run model on in-sample data only
   submodel = multinom(formula, data[insample, ])

   # Obtain predicted probabilities on out-of-sample data
   probs[outsample] = predict(object = submodel, newdata = data[outsample, ], type = "probs")

}

# Print cross-validated probabilities
print(head(probs))

# How many loyal donors among the top 2000
# in terms of (out-of-sample) probabilities?
pred = data.frame(model = probs, truth = data$loyal)
pred = pred[order(pred$model, decreasing = TRUE), ]
print(sum(pred$truth[1:2000]) / 2000)

# vs. full model used to make actual predictions
probs = predict(object = model, newdata = data, type = "probs")
pred = data.frame(model = probs, truth = data$loyal)
pred = pred[order(pred$model, decreasing = TRUE), ]
print(sum(pred$truth[1:2000]) / 2000)


pred$contact_id = row.names(pred)

pred = pred[order(pred['contact_id']),]

db = odbcConnect("mysql_server_64", uid="root", pwd="root")
sqlQuery(db, "USE ma_charity")
query2 = "select contact_id from ma_charity.assignment2 where calibration = 0 "
data2 = sqlQuery(db, query2)

data2$contact_id = as.character(data2$contact_id) 
library(dplyr)
df2 <- data2 %>% left_join( pred, 
                             by=
                              c('contact_id'='contact_id'))
df2['model']<-NULL

write.csv(df2,"C://Users//arind//OneDrive//Desktop//Classes//T1 ESSEC Marketing Analytics//attempt_1.csv",
          row.names = FALSE, col.names = FALSE, sep = "\t")

