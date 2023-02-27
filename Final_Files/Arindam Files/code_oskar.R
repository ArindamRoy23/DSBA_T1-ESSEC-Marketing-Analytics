# Load the package
library(RODBC)
library(ggplot2)
library(rgdal);library(maps)
library(raster)
library(factoextra)
# Connect to you new data source
db = odbcConnect("mysql_server_64", uid="root", pwd="")
sqlQuery(db, "USE ma_charity")


###-----------Donations per departement - DATA CLEANING -----
q = "SELECT contact_id, SUM(CASE WHEN act_type_id = \"PA\" THEN amount ELSE 0 END) as sum_pa, SUM(CASE WHEN act_type_id = \"DO\" THEN amount ELSE 0 END) as sum_do, c.zip_code
FROM acts AS a
LEFT JOIN contacts AS c ON a.contact_id = c.id
GROUP BY contact_id"
# Getting the table using the query above
donations_dep = sqlQuery(db, q)

# Preparing the zip_code column for the analysis
donations_dep$zip_code = donations_dep$zip_code %/% 1000
for (i in 1:length(donations_dep$zip_code)){
  donations_dep$zip_code[i] = toString(donations_dep$zip_code[i])
  if (nchar(donations_dep$zip_code[i]) == 1){
    donations_dep$zip_code[i] = paste("0",donations_dep$zip_code[i],sep="")
  }
}

# Getting the Map of France
adm_fr <- getData('GADM', country='FRA', level=2)

# Creating df_map with the total amount of donations for every departement
departements = adm_fr$CC_2
total_amount_do = rep(0, length(adm_fr$CC_2))
total_amount_pa = rep(0, length(adm_fr$CC_2))
df_map = data.frame(departements,total_amount_do, total_amount_pa)
for (i in 1:length(donations_dep$zip_code)){
  df_map_idx = which(df_map$departements==donations_dep$zip_code[i])
  if (!(identical(df_map_idx, integer(0)))){
    df_map$total_amount_do[df_map_idx] = df_map$total_amount_do[df_map_idx] + donations_dep$sum_do[i]
    df_map$total_amount_pa[df_map_idx] = df_map$total_amount_pa[df_map_idx] + donations_dep$sum_pa[i]
  }
}



# Function that draws a map of France and fills it with colors depending on the buckets
# specified in limites
carto = function(parametre,limites,nom_carte,text=c()) {
  
  # COULEURS

  colors_buckets = c("#D2E1E6", "#C1E3ED", "#AAC9DD", "#78AED3", "#4F77AA")
  dep_bucket = rep(0, length(parametre))
  for (i in 1:(length(parametre))){
    value = parametre[i]
    j = 2
    dep_bucket[i] = 1
    while (value > limites[j]){
      dep_bucket[i] = dep_bucket[i]+1
      j = j+1
    }
  }
  
  my_colors = rep(0, length(parametre))
  for (i in 1:(length(parametre))){
    my_colors[i] = colors_buckets[dep_bucket[i]]
  }

  mes_couleurs = my_colors
  
  # PREPARER LA LEGENDE
  totaux_par_categories=c()
  
  j = 0
  
  for (i in c(1:(length(limites)-1))) {
    j = j+1
    indice <-which(parametre>=limites[i]&parametre<limites[i+1])
    totaux_par_categories =c(totaux_par_categories,length(mes_couleurs[indice]))
  }
  texte=c()
  
  
  for (i in c(1:(length(limites)-1))) {
    
    if (i < (length(limites)-1)) {
      low = ifelse(limites[i]<1e6, paste0(limites[i]%/%1000, "k"), paste0(limites[i]%/%1000000, "M"))
      high = ifelse(limites[i+1]<1e6, paste0(limites[i+1]%/%1000, "k"), paste0(limites[i+1]%/%1000000, "M"))
      temp <- paste(low,"-",high,sep="")
    }
    
    if (i == (length(limites)-1)){
      low = ifelse(limites[i]<1e6, paste0(limites[i]%/%1000, "k"), paste0(limites[i]%/%1000000, "M"))
      temp <- paste(">=",low,sep="")
    }
    
    texte <- c(texte,temp)
    
  }
  

  texte_complet = c()
  j=0
  for (i in texte) {
    j = j+1
    temp <- paste(i,"   (n= ",totaux_par_categories[j],")",sep="")
    texte_complet = c(texte_complet,temp)
    
  }
  
  # GRAPHIQUE
  plot(adm_fr,main=nom_carte,cex=3)
  
  j = 0
  for (i in 1:length(parametre)) {
    j = j+1
    plot (adm_fr[i,],col=my_colors[j],add=T)
  }
  
  if (length(text)>0) {text (x_communes,y_communes,text)}
  legend(x="topleft", legend=texte_complet, fill=colors_buckets,cex=1)

}

###-----------Donations per departement - PLOTS -----

carto(df_map$total_amount_do,limites=c(0,250000,500000,1000000,5000000,10000000),"Total donated - DO") 
carto(df_map$total_amount_pa,limites=c(0,25000,50000,100000,500000,1000000),"Total donated - PA") 


###----------- Segmentation - DATA PREPARATION -----

q = "select contact_id, avg(amount) as generosity, count(amount) as frequency,
datediff(20220530, max(act_date))/365 as recency,
datediff(20220530, min(act_date))/365 as relationship,
if(count(amount)>=2, 1,0) as returned_donor
from acts
where act_type_id = 'DO'
group by contact_id;"
raw <- sqlQuery(db, q)
rownames(raw$contact_id)
data <- raw[,-1]
data<- data[,-5]

nclusters = 4
k = kmeans(scale(data), centers = nclusters, nstart = 50)

# Overview of clusters
for (i in 1:nclusters){
  print(colMeans(data[k$cluster == i,]))
  print(length(data[k$cluster == i,1]))
}

# Creation of new data matrix to plot the segments
segments = matrix(0, nclusters,4)
for (i in 1:nclusters){
  segments[i,] = colMeans(data[k$cluster == i,])
}
colnames(segments) = colnames(data)
segments[,1] = log(segments[,1])
segment = 1:nclusters
segments = cbind(segments, segment)
segments = data.frame(segments)

new_segments = matrix(0, 4*nclusters, 3)
j = 1
for (i in 1:dim(segments)[1]){
  row1 = c(segments$segment[i], "generosity", segments$generosity[i])
  row2 = c(segments$segment[i], "frequency", segments$frequency[i])
  row3 = c(segments$segment[i], "recency", segments$recency[i])
  row4 = c(segments$segment[i], "relationship", segments$relationship[i])
  temp = matrix(c(row1,row2,row3,row4),nrow = 4,ncol = 3,byrow = TRUE)
  new_segments[j:(j+3),] = temp
  j = j+4
}

colnames(new_segments) = c("segment", "criteria", "value")
new_segments = data.frame(new_segments)
new_segments$value = as.numeric(new_segments$value)


###----------- Segmentation - PLOT -----

g = ggplot(data = new_segments, aes(x = segment, y = value, fill = criteria)) + 
  geom_bar(position = "dodge", stat="identity", color = "black") +
  labs(title = "Segments overview", caption = "Segments computed using k-means. Generosity was log-transformed.")+
  xlab("Segments") +
  ylab("Average Value") +
  scale_fill_manual(values = c("frequency" = "#E63946",
                               "generosity" = "#A8DADC",
                               "recency" = "#457B9D",
                               "relationship" = "#1D3557")) +
  theme_minimal()
print(g)


###----------- New donors per year - PLOT -----

# New Donors
q = "select year(a.first) as year, count(a.contact_id) as new_donor
from
(select distinct contact_id, min(act_date) as first
from acts
group by contact_id) as a 
group by 1
order by 1;"

data = sqlQuery(db, q)

g = ggplot(data, aes(x = year, y = new_donor))+
  geom_bar(stat="identity", fill = "#457B9D", color = "black") + 
  ggtitle("Amount of new donors per year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") + ylab("New donors") +
  theme_minimal()+
  theme(plot.title = element_text(face = "bold",hjust=0.5), axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))

print(g)


###----------- Channels - PLOT -----

q = "select year,
(coun_ma)/(coun_ma+coun_ww+coun_sb) as ra_ma,
(coun_ww)/(coun_ma+coun_ww+coun_sb) as ra_ww,
(coun_sb)/(coun_ma+coun_ww+coun_sb) as ra_sb
from
(select year(act_date) as year,
sum(if(channel_id = 'MA',1,0)) as coun_ma,
sum(if(channel_id = 'WW',1,0)) as coun_ww,
sum(if(channel_id = 'SB',1,0)) as coun_sb
from acts
group by year)as a
group by year
order by 1;
"

data2 = sqlQuery(db, q)
ra_ma = data2$ra_ma
ra_ww = data2$ra_ww
ra_sb = data2$ra_sb
year = data2$year
data2 = data.frame(year = rep(year,3), value = c(ra_ma, ra_ww,ra_sb), Channel = c(rep("Paper-based campaigns", length(year)),rep("Online donations", length(year)), rep("Spontaneous donations", length(year))))


g = ggplot(data2, aes(x = year, y = value, fill = Channel)) + 
  geom_bar(stat="identity")+
  xlab("Year") + ylab("Relative amount of donations") +
  ggtitle("Yearly relative amount of donations by channel")+
  coord_flip()+
  scale_fill_manual(values = c("Paper-based campaigns" = "#1D3557",
                               "Online donations" = "#A8DADC",
                               "Spontaneous donations" = "#457B9D"))+
  theme_minimal()+
  theme(plot.title = element_text(face = "bold",hjust=0.5), axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))
  
print(g)


