#importing data
maindata<- read.csv("samplemerge1.csv")
View(maindata)

#Creating bins for age to increase prediction accuracy
maindata$agemod[maindata$age>=10&maindata$age<=14]<- "10-14"
maindata$agemod[maindata$age>=15&maindata$age<=19]<- "15-19"
maindata$agemod[maindata$age>=20&maindata$age<=24]<- "20-24"
maindata$agemod[maindata$age>=25&maindata$age<=29]<- "25-29"
maindata$agemod[maindata$age>=30&maindata$age<=34]<- "30-34"
maindata$agemod[maindata$age>=35&maindata$age<=39]<- "35-39"
maindata$agemod[maindata$age>=40&maindata$age<=44]<- "40-44"
maindata$agemod[maindata$age>=45&maindata$age<=49]<- "45-49"
maindata$agemod[maindata$age>=50&maindata$age<=54]<- "50-54"
maindata$agemod[maindata$age>=55&maindata$age<=59]<- "55-59"
maindata$agemod[maindata$age>=60&maindata$age<=64]<- "60-64"
maindata$agemod[maindata$age>=65&maindata$age<=69]<- "65-69"
maindata$agemod[maindata$age>=70&maindata$age<=74]<- "70-74"
maindata$agemod[maindata$age>=75&maindata$age<=79]<- "75-79"
maindata$agemod[maindata$age>=80&maindata$age<=84]<- "80-84"
maindata$agemod[maindata$age>=85&maindata$age<=89]<- "85-89"
maindata$agemod[maindata$age>=90&maindata$age<=94]<- "90-94"
maindata$agemod[maindata$age>=95]<- "95+"
View(maindata)

#Removing Unwanted Columns
maindatadtree<- maindata[c(-33,-19,-20,-24)]
maindatadtree<- maindatadtree[,-23]
maindatadtree<- maindatadtree[,-26]
maindatadtree<- maindatadtree[c(-2,-5,-7,-24,-25,-26)]
maindatadtree<- maindatadtree[c(-11,-14,-20,-19)]

View(maindatadtree)

#Converting the month, day and year and signflow and male,female, other variables into factors
for(i in c(1,3:17))
  maindatadtree[,i]<- factor(maindatadtree[,i])
str(maindatadtree)

#Modifying variables to reduce levels

#affiliate_provider
levels(maindatadtree$affiliate_provider)
levels(maindatadtree$affiliate_provider)<- c("Others", "bing", "Others", "direct","Others", "facebook", "Others", "google", "Others", "Others", "Others", "Others", "Others", "Others", "Yahoo", "Others")

#first_affiliate_tracked
levels(maindatadtree$first_affiliate_tracked)
levels(maindatadtree$first_affiliate_tracked)<- c("", "linked", "Others", "Others", "omg", "Others", "Others", "untracked")

#first_browser
levels(maindatadtree$first_browser)
levels(maindatadtree$first_browser)<- c("Unknown", "Others", "Others", "Others", "Others", "Others", "Chrome", "Chrome", "Others", "Others", "Firefox", "Others", "IE", "IE", "Others", "Others", "Firefox", "Safari", "Opera", "Opera", "Others", "Safari", "Others", "Others", "Others", "Others", "Others")

#language
levels(maindatadtree$language)
levels(maindatadtree$language)<- c("Others", "Others", "Others", "de", "Others", "en", "es", "Others", "fr", "Others", "Others", "it", "ja", "ko", "Others", "Others", "Others", "ru", "sv", "Others", "Others", "zh")

str(maindatadtree)

#Creating dummy variables instead of levels
install.packages("dummies")
library(dummies)
dummy(maindatadtree$signup_method, sep = "_", drop = TRUE)

#signup_method
maindatadtree$signup_method_basic<- ifelse(maindatadtree$signup_method == "basic", 1,0)
maindatadtree$signup_method_facebook<- ifelse(maindatadtree$signup_method == "facebook", 1,0)
maindatadtree$signup_method_google<- ifelse(maindatadtree$signup_method == "google", 1,0)

#Language
maindatadtree$language_Others<- ifelse(maindatadtree$language == "Others",1, 0)
maindatadtree$language_de<- ifelse(maindatadtree$language == "de",1, 0)
maindatadtree$language_en<- ifelse(maindatadtree$language == "en",1, 0)
maindatadtree$language_es<- ifelse(maindatadtree$language == "es",1, 0)
maindatadtree$language_fr<- ifelse(maindatadtree$language == "fr",1, 0)
maindatadtree$language_it<- ifelse(maindatadtree$language == "it",1, 0)
maindatadtree$language_ja<- ifelse(maindatadtree$language == "ja",1, 0)
maindatadtree$language_ko<- ifelse(maindatadtree$language == "ko",1, 0)
maindatadtree$language_ru<- ifelse(maindatadtree$language == "ru",1, 0)
maindatadtree$language_sv<- ifelse(maindatadtree$language == "sv",1, 0)
maindatadtree$language_zh<- ifelse(maindatadtree$language == "zh",1, 0)

#affiliate_provider
maindatadtree$affiliate_provider_Others<- ifelse(maindatadtree$affiliate_provider == "Others",1,0)
maindatadtree$affiliate_provider_bing<- ifelse(maindatadtree$affiliate_provider == "bing",1,0)
maindatadtree$affiliate_provider_direct<- ifelse(maindatadtree$affiliate_provider == "direct",1,0)
maindatadtree$affiliate_provider_facebook<- ifelse(maindatadtree$affiliate_provider == "facebook",1,0)
maindatadtree$affiliate_provider_google<- ifelse(maindatadtree$affiliate_provider == "google",1,0)
maindatadtree$affiliate_provider_Yahoo<- ifelse(maindatadtree$affiliate_provider == "Yahoo",1,0)

#first_affiliate_tracked
maindatadtree$first_affiliate_tracked_linked<- ifelse(maindatadtree$first_affiliate_tracked == "linked",1,0)
maindatadtree$first_affiliate_tracked_Others<- ifelse(maindatadtree$first_affiliate_tracked == "Others",1,0)
maindatadtree$first_affiliate_tracked_omg<- ifelse(maindatadtree$first_affiliate_tracked == "omg",1,0)
maindatadtree$first_affiliate_tracked_untracked<- ifelse(maindatadtree$first_affiliate_tracked == "untracked",1,0)

#signup_app
maindatadtree$signup_app_And<-ifelse(maindatadtree$signup_app == "And",1,0)
maindatadtree$signup_app_iOS<-ifelse(maindatadtree$signup_app == "iOS",1,0)
maindatadtree$signup_app_Mow<-ifelse(maindatadtree$signup_app == "Mow",1,0)
maindatadtree$signup_app_Web<-ifelse(maindatadtree$signup_app == "Web",1,0)

#first_device_type 
maindatadtree$first_device_type_Andriod_Ph<- ifelse(maindatadtree$first_device_type == "Android Phone",1,0)
maindatadtree$first_device_type_Andriod_Tab<- ifelse(maindatadtree$first_device_type == "Android Tablet",1,0)
maindatadtree$first_device_type_Desk_Other<- ifelse(maindatadtree$first_device_type == "Desktop (Other)",1,0)
maindatadtree$first_device_type_iPad<- ifelse(maindatadtree$first_device_type == "iPad",1,0)
maindatadtree$first_device_type_iPhone<- ifelse(maindatadtree$first_device_type == "iPhone",1,0)
maindatadtree$first_device_type_Mac_Desk<- ifelse(maindatadtree$first_device_type == "Mac Desktop",1,0)
maindatadtree$first_device_type_Other<- ifelse(maindatadtree$first_device_type == "Other/Unknown",1,0)
maindatadtree$first_device_type_Smart_Other<- ifelse(maindatadtree$first_device_type == "SmartPhone (Oth",1,0)
maindatadtree$first_device_type_Win_Desk<- ifelse(maindatadtree$first_device_type == "Windows Desktop",1,0)

#first_browser
maindatadtree$first_browser_Others<- ifelse(maindatadtree$first_browser == "Others",1,0)
maindatadtree$first_browser_Chrome<- ifelse(maindatadtree$first_browser == "Chrome",1,0)
maindatadtree$first_browser_Firefox<- ifelse(maindatadtree$first_browser == "Firefox",1,0)
maindatadtree$first_browser_IE<- ifelse(maindatadtree$first_browser == "IE",1,0)
maindatadtree$first_browser_Safari<- ifelse(maindatadtree$first_browser == "Safari",1,0)
maindatadtree$first_browser_Opera<- ifelse(maindatadtree$first_browser == "Opera",1,0)

#Month_account_created
maindatadtree$Month_account_created_Jan<-ifelse(maindatadtree$Month_account_created == "1",1,0)
maindatadtree$Month_account_created_Feb<-ifelse(maindatadtree$Month_account_created == "2",1,0)
maindatadtree$Month_account_created_Mar<-ifelse(maindatadtree$Month_account_created == "3",1,0)
maindatadtree$Month_account_created_Apr<-ifelse(maindatadtree$Month_account_created == "4",1,0)
maindatadtree$Month_account_created_May<-ifelse(maindatadtree$Month_account_created == "5",1,0)
maindatadtree$Month_account_created_Jun<-ifelse(maindatadtree$Month_account_created == "6",1,0)
maindatadtree$Month_account_created_Jul<-ifelse(maindatadtree$Month_account_created == "7",1,0)
maindatadtree$Month_account_created_Aug<-ifelse(maindatadtree$Month_account_created == "8",1,0)
maindatadtree$Month_account_created_Sep<-ifelse(maindatadtree$Month_account_created == "9",1,0)
maindatadtree$Month_account_created_Oct<-ifelse(maindatadtree$Month_account_created == "10",1,0)
maindatadtree$Month_account_created_Nov<-ifelse(maindatadtree$Month_account_created == "11",1,0)
maindatadtree$Month_account_created_Dec<-ifelse(maindatadtree$Month_account_created == "12",1,0)

#Month_first_Booking 
maindatadtree$Month_first_Booking_Jan<-ifelse(maindatadtree$Month_first_Booking == "1",1,0)
maindatadtree$Month_first_Booking_Feb<-ifelse(maindatadtree$Month_first_Booking == "2",1,0)
maindatadtree$Month_first_Booking_Mar<-ifelse(maindatadtree$Month_first_Booking == "3",1,0)
maindatadtree$Month_first_Booking_Apr<-ifelse(maindatadtree$Month_first_Booking == "4",1,0)
maindatadtree$Month_first_Booking_May<-ifelse(maindatadtree$Month_first_Booking == "5",1,0)
maindatadtree$Month_first_Booking_Jun<-ifelse(maindatadtree$Month_first_Booking == "6",1,0)
maindatadtree$Month_first_Booking_Jul<-ifelse(maindatadtree$Month_first_Booking == "7",1,0)
maindatadtree$Month_first_Booking_Aug<-ifelse(maindatadtree$Month_first_Booking == "8",1,0)
maindatadtree$Month_first_Booking_Sep<-ifelse(maindatadtree$Month_first_Booking == "9",1,0)
maindatadtree$Month_first_Booking_Oct<-ifelse(maindatadtree$Month_first_Booking == "10",1,0)
maindatadtree$Month_first_Booking_Nov<-ifelse(maindatadtree$Month_first_Booking == "11",1,0)
maindatadtree$Month_first_Booking_Dec<-ifelse(maindatadtree$Month_first_Booking == "12",1,0)

#Year_account_created
maindatadtree$Year_account_2010<- ifelse(maindatadtree$Year_account_created == "2010",1,0)
maindatadtree$Year_account_2011<- ifelse(maindatadtree$Year_account_created == "2011",1,0)
maindatadtree$Year_account_2012<- ifelse(maindatadtree$Year_account_created == "2012",1,0)
maindatadtree$Year_account_2013<- ifelse(maindatadtree$Year_account_created == "2013",1,0)
maindatadtree$Year_account_2014<- ifelse(maindatadtree$Year_account_created == "2014",1,0)

#Year_first_Booking
maindatadtree$Year_first_Booking_2010<- ifelse(maindatadtree$Year_first_Booking == "2010",1,0)
maindatadtree$Year_first_Booking_2011<- ifelse(maindatadtree$Year_first_Booking == "2011",1,0)
maindatadtree$Year_first_Booking_2012<- ifelse(maindatadtree$Year_first_Booking == "2012",1,0)
maindatadtree$Year_first_Booking_2013<- ifelse(maindatadtree$Year_first_Booking == "2013",1,0)
maindatadtree$Year_first_Booking_2014<- ifelse(maindatadtree$Year_first_Booking == "2014",1,0)
maindatadtree$Year_first_Booking_2015<- ifelse(maindatadtree$Year_first_Booking == "2015",1,0)

#BookingType
maindatadtree$BookingType_Immediate<- ifelse(maindatadtree$BookingType == "Immediate",1,0)
maindatadtree$BookingType_Not_booked<- ifelse(maindatadtree$BookingType == "Not booke",1,0)
maindatadtree$BookingType_Quick<- ifelse(maindatadtree$BookingType == "Quick",1,0)

#Dropping Primary varibles and assigning dummy variables as factors
maindatadtree<- maindatadtree[c(-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-17)]

for(i in 6:86)
  maindatadtree[,i]<- factor(maindatadtree[,i])
  
#Partiitoning the data into training(70%) and testing(30%)
sampleinstance<- sample(1:nrow(maindatadtree), size = 0.75*nrow(maindatadtree))
train_data<- maindatadtree[sampleinstance,]
test_data<- maindatadtree[-sampleinstance,]


#Decision Tree
library(rpart)
library(rattle)
set.seed(1)
tree_model<- rpart(country_destination~.,train_data, method = "class",control = rpart.control(minsplit = 10,cp = 0.01, maxdepth = 30))
fancyRpartPlot(tree_model,sub = "Decision_Tree")
attributes(tree_model)
print(tree_model)
plot(tree_model)

#Pruning
opt<- which.min(tree_model$cptable[,"xerror"])
cp<- tree_model$cptable[opt, "CP"]
tree_model_prune<- prune(tree_model, cp=cp)
plot(tree_model_prune)

#Variable Importance
tree_model$variable.importance
#Calculated using MeanDecreaseGini - based on Gini Impurity Index

barchart(tree_model$variable.importance, horiz = TRUE, main = "Variable Importance - Least Significant to Most Significant", ylab = "Mean Decrease Gini Score", border = "red", density = tree_model$variable.importance, col = "darkgreen")
var_imp<- data.frame(tree_model$variable.importance)
var_imp<- data.frame(tree_model$variable.importance)
write.csv(var_imp, "Variable_Imp.csv")

#Dataset for clustering...

hcluster<- maindatadtree
#Converting dummy to numerical to aid in clustering
for(i in 2:85)
  hcluster[,i]<- as.numeric(hcluster[,i])

#Standardizing Age
hcluster[,1]<- scale(hcluster[,1])

#Sampling - 1
sampleinstance_cluster<- sample(1:nrow(hcluster), size = 0.50*nrow(hcluster))
train_data1<- hcluster[sampleinstance_cluster,]

#Partitioning CLustering with PAM and library(fpc) , k - need not be specified
library(fpc)
pamk_result<- pamk(train_data1[,-1], usepam = FALSE, seed = 1)
pamk_result$nc #[2]

#Understanding CLuster distribution
cluster_out<- data.frame(pamk_result$pamobject$clustering)
cluster_out<- cbind(cluster_out, train_data1)
colnames(cluster_out)[1]<- "Cluster_Group"

#Output File for Analysis
write.csv(cluster_out,file = "PAMClusterOutfifty.csv")

#Sampling 2
sampleinstance_cluster<- sample(1:nrow(hcluster), size = 0.70*nrow(hcluster))
train_data1<- hcluster[sampleinstance_cluster,]

pamk_result<- pamk(train_data1[,-1], usepam = FALSE, seed = 1)
pamk_result$nc #3

cluster_out2<- data.frame(pamk_result$pamobject$clustering)
cluster_out2<- cbind(cluster_out2, train_data1)
colnames(cluster_out2)[1]<- "Cluster_Group"

write.csv(cluster_out2,file = "PAMClusterOutSeventy.csv")


write.csv(maindatadtree, "DV_SampleMerge.csv")


