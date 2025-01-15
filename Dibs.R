#TASK 1(CLEANING THE DATA)#########################################################################
#Installing and impoering libraries
install.packages("caret")
install.packages("lmtest")
install.packages("car")
install.packages("Metrics")

library(tidyverse)   # Data manipulation and visualization
library(dplyr)       # Data manipulation
library(lubridate)   # Date and time manipulation
library(ggplot2)     # Data visualization
library(scales)      # Scale functions for visualization
library(caret)       # Machine learning tools
library(lmtest)      # Diagnostic tests for linear regression models
library(car)         # Companion to Applied Regression
library(Metrics)     # Evaluation metrics for machine learning
library(randomForest)# Random forest algorithm

#January sales
Jan_Sales = read_csv(file="01_Sales_Jan.csv")

#show first six rows
head(Jan_Sales) 
#We need to change the columns name.
Jan_Sales <- Jan_Sales %>% rename(Order_ID = `Order ID`)
Jan_Sales <- Jan_Sales %>% rename(Quantity_Ordered = `Quantity Ordered`)
Jan_Sales <- Jan_Sales %>% rename(Price_Each = `Price Each`)
Jan_Sales <- Jan_Sales %>% rename(Order_Date = `Order Date`)
Jan_Sales <- Jan_Sales %>% rename(Purchase_Address = `Purchase Address`)

#show summary of the tibble
str(Jan_Sales)
#Its visible that all the columns are in characters, 
#even the "Quantity Ordered" and "Price Each" which should be numeric, 
#and date should be in Date format.


# Lets work on column "Quantity_Ordered"
#lets find out if we have any non-numeric values in this column.
#These values will convert into NAs when we will convert the type of the column from character to integer
Not_num_Quantity <- Jan_Sales[grepl("[^0-9]", Jan_Sales$`Quantity_Ordered`), ]
nrow(Not_num_Quantity) 


# Convert the data type to integer.
Jan_Sales$`Quantity_Ordered` <- as.integer(Jan_Sales$`Quantity_Ordered`)
typeof(Jan_Sales$`Quantity_Ordered`)

#"Price_Each" Column
#check if there is any non numeric values present 
#this code will return if there is any values which are not decimal or are NAs
Not_num_Price <- grepl("^[0-9]*\\.?[0-9]+$", Jan_Sales$Price_Each)

# Identify non-conforming entries
invalid_values_Price = Jan_Sales[!Not_num_Price, ]

# Display invalid entries
print(invalid_values_Price)
#There are 43 row which includes all the NAs and the non numeric values.
#One of them is $11.95, the price is given with a dollar sign. 

#lets remove the dollar sign before converting the type of the data
Jan_Sales$Price_Each <- gsub("\\$", "", Jan_Sales$Price_Each)

# Now lets convert from character to numeric
Jan_Sales$Price_Each <- as.numeric(Jan_Sales$Price_Each)
typeof(Jan_Sales$Price_Each)

#Lets work on the "Order_Date" column
head(Jan_Sales)

Jan_Sales$Order_Date = mdy_hm(Jan_Sales$Order_Date)

typeof(Jan_Sales$Order_Date)


#lets check the if there are any misspelling in columns "Product" and "Purchase_Address"
unique(Jan_Sales$Product)

#lets replace all the misspellings in the "product" column 
Jan_Sales <- Jan_Sales %>%
  mutate(Product = str_replace_all(Product, "AAA Batteries \\(4pack\\)", "AAA Batteries (4-pack)")) %>% 
  mutate(Product = str_replace_all(Product, "Goo0gle Phone","Google Phone")) %>%
  mutate(Product = str_replace_all(Product, "Wired Headphoness", "Wired Headphones"))

Jan_Sales <- Jan_Sales %>%
  mutate(Product = replace(Product,
                           Product == "Product",NA))

unique(Jan_Sales$Product) 


#we could also see when we used the unique function there is some products are named as Product itself
#it can be those 16 rows where we found non-numeric in the previous column we worked on
#lets check if there are more rows which has Product as a Product Name
Product_names <- Jan_Sales %>% 
  filter(Product == "Product")

# Display the filtered dataset 
print(Product_names)
# These are the same rows as earlier
#In these records "Order_ID" and "Product_ID" is also named as there column name

#Lets work on "Purchase_Address" column
#While working on other columns we noticed that 16 records have text "Purchase Address" in the column
#lets check if there are more and then replace them all with NAs
address_only_rows <- Jan_Sales %>%
  filter(Purchase_Address == "Purchase Address")
print(address_only_rows)
Jan_Sales <- Jan_Sales %>%
  mutate(Purchase_Address = replace(Purchase_Address,
                                    Purchase_Address == "Purchase Address",NA))

#lets split the column into three columns by separating the existing column by using "," as the delimiter.
Jan_Sales <- Jan_Sales %>%
  separate(Purchase_Address, into = c("Street_Address", "City", "State_Zip"), sep = ", ", remove = FALSE)

#now lets seperate the "state_zip" column by using " " as the delimiter.
Jan_Sales <- Jan_Sales %>%
  separate(State_Zip, into = c("State", "Zip"), sep = " ")

#lets check if there are any typo
unique(Jan_Sales$City)
#there are no typo in this column

#We also observed that some records in the "Order_ID" column contained the text "Order ID" instead of numbers while modifying other columns
#and the count of them was 16, now lets check if there are more.
Not_Numeric <- Jan_Sales[grepl("[^0-9]", Jan_Sales$`Order_ID`), ]

#Get the total number of non-numeric values
nrow(Not_Numeric) 
 
#lets replaces these entries with NAs
Jan_Sales <- Jan_Sales %>%
  mutate(Order_ID = replace(Order_ID,
                            Order_ID == "Order ID",NA))
head(Jan_Sales)


#lets remove all the NAs
sum(is.na(Jan_Sales))
str(Jan_Sales)

# There are 372 null values present out of 9,723 records in the dataset. Now, lets check the location of the null values
colSums(is.na(Jan_Sales))

Jan_Sales_Clean <- na.omit(Jan_Sales)
colSums(is.na(Jan_Sales_Clean))


write_csv(Jan_Sales_Clean,"New_Jan_Sales.csv")



#February Sales
Feb_Sales = read_csv(file="02_Sales_Feb.csv")

#show first six rows
head(Feb_Sales) 
#We need to change the columns name.
Feb_Sales <- Feb_Sales %>% rename(Order_ID = `Order ID`)
Feb_Sales <- Feb_Sales %>% rename(Quantity_Ordered = `Quantity Ordered`)
Feb_Sales <- Feb_Sales %>% rename(Price_Each = `Price Each`)
Feb_Sales <- Feb_Sales %>% rename(Order_Date = `Order Date`)
Feb_Sales <- Feb_Sales %>% rename(Purchase_Address = `Purchase Address`)

#show summary of the tibble
str(Feb_Sales)

#same as the previous file, all the columns are in character 
#we will apply same steps as we did earlier
# Lets work on column "Quantity_Ordered"
Not_num_Quantity_Feb <- Feb_Sales[grepl("[^0-9]", Feb_Sales$`Quantity_Ordered`), ]
nrow(Not_num_Quantity_Feb) 
#There are 18 non-numeric values present 

# Convert the data type to integer.
Feb_Sales$`Quantity_Ordered` <- as.integer(Feb_Sales$`Quantity_Ordered`)
typeof(Feb_Sales$`Quantity_Ordered`)

#"Price_Each" Column
Not_num_Price_Feb <- grepl("^[0-9]*\\.?[0-9]+$", Feb_Sales$Price_Each)
invalid_values_Price_Feb = Feb_Sales[!Not_num_Price_Feb, ]
#There are 50 rows which includes all the NAs and the non numeric values.

# Now lets convert from character to numeric
Feb_Sales$Price_Each <- as.numeric(Feb_Sales$Price_Each)
typeof(Feb_Sales$Price_Each)

#Lets work on the "Order_Date" column
Feb_Sales$Order_Date = mdy_hm(Feb_Sales$Order_Date)

#lets check the if there are any misspelling in columns "Product".
unique(Feb_Sales$Product)
#we can see that there is some products which are named as Product itself
#And there is one typing mistake as well

#lets replace them all
Feb_Sales <- Feb_Sales %>%
  mutate(Product = replace(Product,
                           Product == "Product",NA))
Feb_Sales <- Feb_Sales %>%
  mutate(Product = replace(Product,
                           Product == "### syste error###",NA))


#"Purchase_Address" column
Feb_address_only_rows <- Feb_Sales %>%
  filter(Purchase_Address == "Purchase Address")
print(Feb_address_only_rows)
Feb_Sales <- Feb_Sales %>%
  mutate(Purchase_Address = replace(Purchase_Address,
                                    Purchase_Address == "Purchase Address",NA))

#lets split the column into three columns
Feb_Sales <- Feb_Sales %>%
  separate(Purchase_Address, into = c("Street_Address", "City", "State_Zip"), sep = ", ", remove = FALSE)

#now lets seperate the "state_zip" column by using " " as the delimiter.
Feb_Sales <- Feb_Sales %>%
  separate(State_Zip, into = c("State", "Zip"), sep = " ")

unique(Feb_Sales$City)
#there are no typo in this column

#While modifying the format of other columns, 
#same as before we observed that some records in the "Order_ID" column contained the text "Order ID" instead of numbers.
#and the count of them was 18, now lets check if there are more.
Feb_Not_Numeric_Ord <- Feb_Sales[grepl("[^0-9]", Feb_Sales$`Order_ID`), ]

#Get the total number of non-numeric values
nrow(Feb_Not_Numeric_Ord) 

#lets replaces these entries with NAs
Feb_Sales <- Feb_Sales %>%
  mutate(Order_ID = replace(Order_ID,
                            Order_ID == "Order ID",NA))

#lets remove all the NAs
sum(is.na(Feb_Sales))
str(Feb_Sales)

# There are 501 null values present out of 12,037 records in the dataset. 
#Now, lets check the location of the null values
colSums(is.na(Feb_Sales))
Feb_Sales_Clean <- na.omit(Feb_Sales)

write_csv(Feb_Sales_Clean,"New_Feb_Sales.csv")



# March Sales
Mar_Sales = read_csv(file="03_Sales_Mar.csv")

#show first six rows
head(Mar_Sales) 
#We need to change the columns name.
Mar_Sales <- Mar_Sales %>% rename(Order_ID = `Order ID`)
Mar_Sales <- Mar_Sales %>% rename(Quantity_Ordered = `Quantity Ordered`)
Mar_Sales <- Mar_Sales %>% rename(Price_Each = `Price Each`)
Mar_Sales <- Mar_Sales %>% rename(Order_Date = `Order Date`)
Mar_Sales <- Mar_Sales %>% rename(Purchase_Address = `Purchase Address`)

#show summary of the tibble
str(Mar_Sales)

# Lets work on column "Quantity_Ordered"
Not_num_Quantity_Mar <- Mar_Sales[grepl("[^0-9]", Mar_Sales$`Quantity_Ordered`), ]
nrow(Not_num_Quantity_Mar) 
#There are 35 non-numeric values present 

# Convert the data type to integer.
Mar_Sales$`Quantity_Ordered` <- as.integer(Mar_Sales$`Quantity_Ordered`)
typeof(Mar_Sales$`Quantity_Ordered`)

#"Price_Each" Column
Not_num_Price_Mar <- grepl("^[0-9]*\\.?[0-9]+$", Mar_Sales$Price_Each)
invalid_values_Price_Mar = Mar_Sales[!Not_num_Price_Mar, ]
#There are 35 rows which includes all the NAs and the non numeric values.

# Now lets convert from character to numeric
Mar_Sales$Price_Each <- as.numeric(Mar_Sales$Price_Each)
typeof(Mar_Sales$Price_Each)

#Lets work on the "Order_Date" column
Mar_Sales$Order_Date = mdy_hm(Mar_Sales$Order_Date)

#lets check the if there are any misspelling in columns "Product".
unique(Mar_Sales$Product)

#lets replace incorrect entries
Mar_Sales <- Mar_Sales %>%
  mutate(Product = replace(Product,
                           Product == "Product",NA))

#"Purchase_Address" column
Mar_address_only_rows <- Mar_Sales %>%
  filter(Purchase_Address == "Purchase Address")
Mar_Sales <- Mar_Sales %>%
  mutate(Purchase_Address = replace(Purchase_Address,
                                    Purchase_Address == "Purchase Address",NA))

#lets split the column into three columns
Mar_Sales <- Mar_Sales %>%
  separate(Purchase_Address, into = c("Street_Address", "City", "State_Zip"), sep = ", ", remove = FALSE)

#now lets seperate the "state_zip" column by using " " as the delimiter.
Mar_Sales <- Mar_Sales %>%
  separate(State_Zip, into = c("State", "Zip"), sep = " ")

unique(Mar_Sales$City)
#there are no typo in this column

#While modifying the format of other columns, 
#same as before we observed that some records in the "Order_ID" column contained the text "Order ID" instead of numbers.
#and the count of them was 35, now lets check if there are more.
Mar_Not_Numeric_Ord <- Mar_Sales[grepl("[^0-9]", Mar_Sales$`Order_ID`), ]
nrow(Mar_Not_Numeric_Ord) 

#lets replaces these entries with NAs
Mar_Sales <- Mar_Sales %>%
  mutate(Order_ID = replace(Order_ID,
                            Order_ID == "Order ID",NA))

#lets remove all the NAs
sum(is.na(Mar_Sales))
str(Mar_Sales)

# There are 723 null values present out of 15,227 records in the dataset. 
#Now, lets check the location of the null values
colSums(is.na(Mar_Sales))
Mar_Sales_Clean <- na.omit(Mar_Sales)

write_csv(Mar_Sales_Clean,"New_Mar_Sales.csv")



#April Sales
Apr_Sales = read_csv(file="04_Sales_Apr.csv")

#show first six rows
head(Apr_Sales) 
#We need to change the columns name.
Apr_Sales <- Apr_Sales %>% rename(Order_ID = `Order ID`)
Apr_Sales <- Apr_Sales %>% rename(Quantity_Ordered = `Quantity Ordered`)
Apr_Sales <- Apr_Sales %>% rename(Price_Each = `Price Each`)
Apr_Sales <- Apr_Sales %>% rename(Order_Date = `Order Date`)
Apr_Sales <- Apr_Sales %>% rename(Purchase_Address = `Purchase Address`)

#show summary of the tibble
str(Apr_Sales)

# Lets work on column "Quantity_Ordered"
Not_num_Quantity_Apr <- Apr_Sales[grepl("[^0-9]", Apr_Sales$`Quantity_Ordered`), ]
nrow(Not_num_Quantity_Apr) 
#There are 35 non-numeric values present 

# Convert the data type to integer.
Apr_Sales$`Quantity_Ordered` <- as.integer(Apr_Sales$`Quantity_Ordered`)
typeof(Apr_Sales$`Quantity_Ordered`)

#"Price_Each" Column
Not_num_Price_Apr <- grepl("^[0-9]*\\.?[0-9]+$", Apr_Sales$Price_Each)
invalid_values_Price_Apr = Apr_Sales[!Not_num_Price_Apr, ]
#There are 50 rows which includes all the NAs and the non numeric values.
#one them has dollar sign $149.99 with the price
Apr_Sales$Price_Each <- gsub("\\$", "", Apr_Sales$Price_Each)

# Now lets convert from character to numeric
Apr_Sales$Price_Each <- as.numeric(Apr_Sales$Price_Each)
typeof(Apr_Sales$Price_Each)

#Lets work on the "Order_Date" column
Apr_Sales$Order_Date = mdy_hm(Apr_Sales$Order_Date)

#lets check the if there are any misspelling in columns "Product" and some incorrect entries
unique(Apr_Sales$Product)

#lets replace incorrect entries
Apr_Sales <- Apr_Sales %>%
  mutate(Product = replace(Product,
                           Product == "Product",NA)) %>%
  mutate(Product = replace(Product,
                           Product == "Fault error",NA)) 
Apr_Sales <- Apr_Sales %>%
  mutate(Product = str_replace_all(Product, "IPhone","iPhone")) 

#"Purchase_Address" column
Apr_address_only_rows <- Apr_Sales %>%
  filter(Purchase_Address == "Purchase Address")
Apr_Sales <- Apr_Sales %>%
  mutate(Purchase_Address = replace(Purchase_Address,
                                    Purchase_Address == "Purchase Address",NA))

#lets split the column into three columns
Apr_Sales <- Apr_Sales %>%
  separate(Purchase_Address, into = c("Street_Address", "City", "State_Zip"), sep = ", ", remove = FALSE)

#now lets seperate the "state_zip" column by using " " as the delimiter.
Apr_Sales <- Apr_Sales %>%
  separate(State_Zip, into = c("State", "Zip"), sep = " ")

unique(Apr_Sales$City)
#there are two typing errors, lets replace them 
Apr_Sales <- Apr_Sales %>%
  mutate(State = str_replace_all(State, "Las Angeles","Los Angeles"))  %>%
  mutate(State = str_replace_all(State, "SanFrancisco","San Francisco")) 

#Order_ID column
Apr_Not_Numeric_Ord <- Apr_Sales[grepl("[^0-9]", Apr_Sales$`Order_ID`), ]
nrow(Apr_Not_Numeric_Ord) 

#lets replaces incorrect entries with NAs
Apr_Sales <- Apr_Sales %>%
  mutate(Order_ID = replace(Order_ID,
                            Order_ID == "Order ID",NA))

#lets remove all the NAs
sum(is.na(Apr_Sales))
str(Apr_Sales)

# There are 941 null values present out of 18,384 records in the dataset. 
#Now, lets check the location of the null values
colSums(is.na(Apr_Sales))
Apr_Sales_Clean <- na.omit(Apr_Sales)

write_csv(Apr_Sales_Clean,"New_Apr_Sales.csv")


#May Sales
May_Sales = read_csv(file="05_Sales_May.csv")

#show first six rows
head(May_Sales) 
#We need to change the columns name.
May_Sales <- May_Sales %>% rename(Order_ID = `Order ID`)
May_Sales <- May_Sales %>% rename(Quantity_Ordered = `Quantity Ordered`)
May_Sales <- May_Sales %>% rename(Price_Each = `Price Each`)
May_Sales <- May_Sales %>% rename(Order_Date = `Order Date`)
May_Sales <- May_Sales %>% rename(Purchase_Address = `Purchase Address`)

#show summary of the tibble
str(May_Sales)

# Lets work on column "Quantity_Ordered"
Not_num_Quantity_May <- May_Sales[grepl("[^0-9]", May_Sales$`Quantity_Ordered`), ]
nrow(Not_num_Quantity_May) 
#There are 35 non-numeric values present 

# Convert the data type to integer.
May_Sales$`Quantity_Ordered` <- as.integer(May_Sales$`Quantity_Ordered`)
typeof(May_Sales$`Quantity_Ordered`)

#"Price_Each" Column
Not_num_Price_May <- grepl("^[0-9]*\\.?[0-9]+$", May_Sales$Price_Each)
invalid_values_Price_May = May_Sales[!Not_num_Price_May, ]
#There are 50 rows which includes all the NAs and the non numeric values.

# Now lets convert from character to numeric
May_Sales$Price_Each <- as.numeric(May_Sales$Price_Each)
typeof(May_Sales$Price_Each)

#Lets work on the "Order_Date" column
May_Sales$Order_Date = mdy_hm(May_Sales$Order_Date)

#lets check the if there are any misspelling in columns "Product" and some incorrect entries
unique(May_Sales$Product)

#lets replace incorrect entries
May_Sales <- May_Sales %>%
  mutate(Product = replace(Product,
                           Product == "Product",NA)) 

#"Purchase_Address" column
May_address_only_rows <- May_Sales %>%
  filter(Purchase_Address == "Purchase Address")
May_Sales <- May_Sales %>%
  mutate(Purchase_Address = replace(Purchase_Address,
                                    Purchase_Address == "Purchase Address",NA))

#lets split the column into three columns
May_Sales <- May_Sales %>%
  separate(Purchase_Address, into = c("Street_Address", "City", "State_Zip"), sep = ", ", remove = FALSE)

#now lets seperate the "state_zip" column by using " " as the delimiter.
May_Sales <- May_Sales %>%
  separate(State_Zip, into = c("State", "Zip"), sep = " ")

unique(May_Sales$City)
#there are no typing errors

#Order_ID column
May_Not_Numeric_Ord <- May_Sales[grepl("[^0-9]", May_Sales$`Order_ID`), ]
nrow(May_Not_Numeric_Ord) 

#lets replaces incorrect entries with NAs
May_Sales <- May_Sales %>%
  mutate(Order_ID = replace(Order_ID,
                            Order_ID == "Order ID",NA))

#lets remove all the NAs
sum(is.na(May_Sales))
str(May_Sales)

# There are 810 null values present out of 16,635 records in the dataset. 
#Now, lets check the location of the null values
colSums(is.na(May_Sales))
May_Sales_Clean <- na.omit(May_Sales)

write_csv(May_Sales_Clean,"New_May_Sales.csv")

#June Sales
Jun_Sales = read_csv(file="06_Sales_Jun.csv")

#show first six rows
head(Jun_Sales) 
#We need to change the columns name.
Jun_Sales <- Jun_Sales %>% rename(Order_ID = `Order ID`)
Jun_Sales <- Jun_Sales %>% rename(Quantity_Ordered = `Quantity Ordered`)
Jun_Sales <- Jun_Sales %>% rename(Price_Each = `Price Each`)
Jun_Sales <- Jun_Sales %>% rename(Order_Date = `Order Date`)
Jun_Sales <- Jun_Sales %>% rename(Purchase_Address = `Purchase Address`)

#show summary of the tibble
str(Jun_Sales)

# Lets work on column "Quantity_Ordered"
Not_num_Quantity_Jun <- Jun_Sales[grepl("[^0-9]", Jun_Sales$`Quantity_Ordered`), ]
nrow(Not_num_Quantity_Jun) 
#There are 23 non-numeric values present 

# Convert the data type to integer.
Jun_Sales$`Quantity_Ordered` <- as.integer(Jun_Sales$`Quantity_Ordered`)
typeof(Jun_Sales$`Quantity_Ordered`)

#"Price_Each" Column
Not_num_Price_Jun <- grepl("^[0-9]*\\.?[0-9]+$", Jun_Sales$Price_Each)
invalid_values_Price_Jun = Jun_Sales[!Not_num_Price_Jun, ]
#There are 50 rows which includes all the NAs and the non numeric values.

# Now lets convert from character to numeric
Jun_Sales$Price_Each <- as.numeric(Jun_Sales$Price_Each)
typeof(Jun_Sales$Price_Each)

#Lets work on the "Order_Date" column
Jun_Sales$Order_Date = mdy_hm(Jun_Sales$Order_Date)

#lets check the if there are any misspelling in columns "Product" and some incorrect entries
unique(Jun_Sales$Product)

#lets replace incorrect entries
Jun_Sales <- Jun_Sales %>%
  mutate(Product = replace(Product,
                           Product == "Product",NA)) 

#"Purchase_Address" column
Jun_address_only_rows <- Jun_Sales %>%
  filter(Purchase_Address == "Purchase Address")
Jun_Sales <- Jun_Sales %>%
  mutate(Purchase_Address = replace(Purchase_Address,
                                    Purchase_Address == "Purchase Address",NA))

#lets split the column into three columns
Jun_Sales <- Jun_Sales %>%
  separate(Purchase_Address, into = c("Street_Address", "City", "State_Zip"), sep = ", ", remove = FALSE)

#now lets seperate the "state_zip" column by using " " as the delimiter.
Jun_Sales <- Jun_Sales %>%
  separate(State_Zip, into = c("State", "Zip"), sep = " ")

unique(Jun_Sales$City)
#there are no typing errors

#Order_ID column
Jun_Not_Numeric_Ord <- Jun_Sales[grepl("[^0-9]", Jun_Sales$`Order_ID`), ]
nrow(Jun_Not_Numeric_Ord) 

#lets replaces incorrect entries with NAs
Jun_Sales <- Jun_Sales %>%
  mutate(Order_ID = replace(Order_ID,
                            Order_ID == "Order ID",NA))

#lets remove all the NAs
sum(is.na(Jun_Sales))
str(Jun_Sales)

# There are 660 null values present out of 13,622 records in the dataset. 
#Now, lets check the location of the null values
colSums(is.na(Jun_Sales))
Jun_Sales_Clean <- na.omit(Jun_Sales)

write_csv(Jun_Sales_Clean,"New_Jun_Sales.csv")


#July Sales
July_Sales = read_csv(file="07_Sales_Jul.csv")

#show first six rows
head(July_Sales) 
#We need to change the columns name.
July_Sales <- July_Sales %>% rename(Order_ID = `Order ID`)
July_Sales <- July_Sales %>% rename(Quantity_Ordered = `Quantity Ordered`)
July_Sales <- July_Sales %>% rename(Price_Each = `Price Each`)
July_Sales <- July_Sales %>% rename(Order_Date = `Order Date`)
July_Sales <- July_Sales %>% rename(Purchase_Address = `Purchase Address`)

#show summary of the tibble
str(July_Sales)

# Lets work on column "Quantity_Ordered"
Not_num_Quantity_Jun <- July_Sales[grepl("[^0-9]", July_Sales$`Quantity_Ordered`), ]
nrow(Not_num_Quantity_Jun) 
#There are 35 non-numeric values present 

# Convert the data type to integer.
July_Sales$`Quantity_Ordered` <- as.integer(July_Sales$`Quantity_Ordered`)
typeof(July_Sales$`Quantity_Ordered`)

#"Price_Each" Column
Not_num_Price_July <- grepl("^[0-9]*\\.?[0-9]+$", July_Sales$Price_Each)
invalid_values_Price_July = July_Sales[!Not_num_Price_July, ]
#There are 82 rows which includes all the NAs and the non numeric values.

# Now lets convert from character to numeric
July_Sales$Price_Each <- as.numeric(July_Sales$Price_Each)
typeof(July_Sales$Price_Each)

#Lets work on the "Order_Date" column
July_Sales$Order_Date = mdy_hm(July_Sales$Order_Date)

#lets check the if there are any misspelling in columns "Product" and some incorrect entries
unique(July_Sales$Product)

#lets replace incorrect entries
July_Sales <- July_Sales %>%
  mutate(Product = replace(Product,
                           Product == "Product",NA))  %>%
  mutate(Product = replace(Product,
                           Product == "##system error##",NA))
July_Sales <- July_Sales %>%
  mutate(Product = str_replace_all(Product, "LightCharging Cable","Lightning Charging Cable")) %>%
  mutate(Product = str_replace_all(Product, "USBC Charging Cable", "USB-C Charging Cable"))
  

#"Purchase_Address" column
Jun_address_only_rows <- July_Sales %>%
  filter(Purchase_Address == "Purchase Address")
July_Sales <- July_Sales %>%
  mutate(Purchase_Address = replace(Purchase_Address,
                                    Purchase_Address == "Purchase Address",NA))

#lets split the column into three columns
July_Sales <- July_Sales %>%
  separate(Purchase_Address, into = c("Street_Address", "City", "State_Zip"), sep = ", ", remove = FALSE)

#now lets seperate the "state_zip" column by using " " as the delimiter.
July_Sales <- July_Sales %>%
  separate(State_Zip, into = c("State", "Zip"), sep = " ")

unique(July_Sales$City)
#there are no typing errors

#Order_ID column
July_Not_Numeric_Ord <- July_Sales[grepl("[^0-9]", July_Sales$`Order_ID`), ]
nrow(July_Not_Numeric_Ord) 

#lets replaces incorrect entries with NAs
July_Sales <- July_Sales %>%
  mutate(Order_ID = replace(Order_ID,
                            Order_ID == "Order ID",NA))

#lets remove all the NAs
sum(is.na(July_Sales))
str(July_Sales)

# There are 820 null values present out of 14,395 records in the dataset. 
#Now, lets check the location of the null values
colSums(is.na(July_Sales))
July_Sales_Clean <- na.omit(July_Sales)

write_csv(July_Sales_Clean,"New_July_Sales.csv")


#August Sales
Aug_Sales = read_csv(file="08_Sales_Aug.csv")

#show first six rows
head(Aug_Sales) 
#We need to change the columns name.
Aug_Sales <- Aug_Sales %>% rename(Order_ID = `Order ID`)
Aug_Sales <- Aug_Sales %>% rename(Quantity_Ordered = `Quantity Ordered`)
Aug_Sales <- Aug_Sales %>% rename(Price_Each = `Price Each`)
Aug_Sales <- Aug_Sales %>% rename(Order_Date = `Order Date`)
Aug_Sales <- Aug_Sales %>% rename(Purchase_Address = `Purchase Address`)

#show summary of the tibble
str(Aug_Sales)

# Lets work on column "Quantity_Ordered"
Not_num_Quantity_Aug <- Aug_Sales[grepl("[^0-9]", Aug_Sales$`Quantity_Ordered`), ]
nrow(Not_num_Quantity_Aug) 
#There are 17 non-numeric values present 

# Convert the data type to integer.
Aug_Sales$`Quantity_Ordered` <- as.integer(Aug_Sales$`Quantity_Ordered`)
typeof(Aug_Sales$`Quantity_Ordered`)

#"Price_Each" Column
Not_num_Price_Aug <- grepl("^[0-9]*\\.?[0-9]+$", Aug_Sales$Price_Each)
invalid_values_Price_Aug = Aug_Sales[!Not_num_Price_Aug, ]
#There are 54 rows which includes all the NAs and the non numeric values.

# Now lets convert from character to numeric
Aug_Sales$Price_Each <- as.numeric(Aug_Sales$Price_Each)
typeof(Aug_Sales$Price_Each)

#Lets work on the "Order_Date" column
Aug_Sales$Order_Date = mdy_hm(Aug_Sales$Order_Date)

#lets check the if there are any misspelling in columns "Product" and some incorrect entries
unique(Aug_Sales$Product)

#lets replace incorrect entries
Aug_Sales <- Aug_Sales %>%
  mutate(Product = replace(Product,
                           Product == "Product",NA))  

#"Purchase_Address" column
Aug_address_only_rows <- Aug_Sales %>%
  filter(Purchase_Address == "Purchase Address")
Aug_Sales <- Aug_Sales %>%
  mutate(Purchase_Address = replace(Purchase_Address,
                                    Purchase_Address == "Purchase Address",NA))

#lets split the column into three columns
Aug_Sales <- Aug_Sales %>%
  separate(Purchase_Address, into = c("Street_Address", "City", "State_Zip"), sep = ", ", remove = FALSE)

#now lets seperate the "state_zip" column by using " " as the delimiter.
Aug_Sales <- Aug_Sales %>%
  separate(State_Zip, into = c("State", "Zip"), sep = " ")

unique(Aug_Sales$City)
#there are no typing errors

#Order_ID column
Sep_Not_Numeric_Ord <- Aug_Sales[grepl("[^0-9]", Aug_Sales$`Order_ID`), ]
nrow(Sep_Not_Numeric_Ord) 

#lets replaces incorrect entries with NAs
Aug_Sales <- Aug_Sales %>%
  mutate(Order_ID = replace(Order_ID,
                            Order_ID == "Order ID",NA))

#lets remove all the NAs
sum(is.na(Aug_Sales))
str(Aug_Sales)

# There are 540 null values present out of 12,011 records in the dataset. 
#Now, lets check the location of the null values
colSums(is.na(Aug_Sales))
Aug_Sales_Clean <- na.omit(Aug_Sales)

write_csv(Aug_Sales_Clean,"New_Aug_Sales.csv")


#September Sales
Sep_Sales = read_csv(file="09_Sales_Sep.csv")

#show first six rows
head(Sep_Sales) 
#We need to change the columns name.
Sep_Sales <- Sep_Sales %>% rename(Order_ID = `Order ID`)
Sep_Sales <- Sep_Sales %>% rename(Quantity_Ordered = `Quantity Ordered`)
Sep_Sales <- Sep_Sales %>% rename(Price_Each = `Price Each`)
Sep_Sales <- Sep_Sales %>% rename(Order_Date = `Order Date`)
Sep_Sales <- Sep_Sales %>% rename(Purchase_Address = `Purchase Address`)

#show summary of the tibble
str(Sep_Sales)

# Lets work on column "Quantity_Ordered"
Not_num_Quantity_Sep <- Sep_Sales[grepl("[^0-9]", Sep_Sales$`Quantity_Ordered`), ]
nrow(Not_num_Quantity_Sep) 
#There are 17 non-numeric values present 

# Convert the data type to integer.
Sep_Sales$`Quantity_Ordered` <- as.integer(Sep_Sales$`Quantity_Ordered`)
typeof(Sep_Sales$`Quantity_Ordered`)

#"Price_Each" Column
Not_num_Price_Sep <- grepl("^[0-9]*\\.?[0-9]+$", Sep_Sales$Price_Each)
invalid_values_Price_Sep = Sep_Sales[!Not_num_Price_Sep, ]
#There are 57 rows which includes all the NAs and the non numeric values.

# Now lets convert from character to numeric
Sep_Sales$Price_Each <- as.numeric(Sep_Sales$Price_Each)
typeof(Sep_Sales$Price_Each)

#Lets work on the "Order_Date" column
Sep_Sales$Order_Date = mdy_hm(Sep_Sales$Order_Date)

#lets check the if there are any misspelling in columns "Product" and some incorrect entries
unique(Sep_Sales$Product)

#lets replace incorrect entries
Sep_Sales <- Sep_Sales %>%
  mutate(Product = replace(Product,
                           Product == "Product",NA))  

#"Purchase_Address" column
Sep_address_only_rows <- Sep_Sales %>%
  filter(Purchase_Address == "Purchase Address")
Sep_Sales <- Sep_Sales %>%
  mutate(Purchase_Address = replace(Purchase_Address,
                                    Purchase_Address == "Purchase Address",NA))

#lets split the column into three columns
Sep_Sales <- Sep_Sales %>%
  separate(Purchase_Address, into = c("Street_Address", "City", "State_Zip"), sep = ", ", remove = FALSE)

#now lets seperate the "state_zip" column by using " " as the delimiter.
Sep_Sales <- Sep_Sales %>%
  separate(State_Zip, into = c("State", "Zip"), sep = " ")

unique(Sep_Sales$City)
#there are no typing errors

#Order_ID column
Sep_Not_Numeric_Ord <- Sep_Sales[grepl("[^0-9]", Sep_Sales$`Order_ID`), ]
nrow(Sep_Not_Numeric_Ord) 

#lets replaces incorrect entries with NAs
Sep_Sales <- Sep_Sales %>%
  mutate(Order_ID = replace(Order_ID,
                            Order_ID == "Order ID",NA))

#lets remove all the NAs
sum(is.na(Sep_Sales))
str(Sep_Sales)

# There are 820 null values present out of 14,395 records in the dataset. 
#Now, lets check the location of the null values
colSums(is.na(Sep_Sales))
Sep_Sales_Clean <- na.omit(Sep_Sales)

write_csv(Sep_Sales_Clean,"New_Sep_Sales.csv")


#October Sales
Oct_Sales = read_csv(file="10_Sales_Oct.csv")

#show first six rows
head(Oct_Sales) 
#We need to change the columns name.
Oct_Sales <- Oct_Sales %>% rename(Order_ID = `Order ID`)
Oct_Sales <- Oct_Sales %>% rename(Quantity_Ordered = `Quantity Ordered`)
Oct_Sales <- Oct_Sales %>% rename(Price_Each = `Price Each`)
Oct_Sales <- Oct_Sales %>% rename(Order_Date = `Order Date`)
Oct_Sales <- Oct_Sales %>% rename(Purchase_Address = `Purchase Address`)

#show summary of the tibble
str(Oct_Sales)

# Lets work on column "Quantity_Ordered"
Not_num_Quantity_Oct <- Oct_Sales[grepl("[^0-9]", Oct_Sales$`Quantity_Ordered`), ]
nrow(Not_num_Quantity_Oct) 
#There are 17 non-numeric values present 

# Convert the data type to integer.
Oct_Sales$`Quantity_Ordered` <- as.integer(Oct_Sales$`Quantity_Ordered`)
typeof(Oct_Sales$`Quantity_Ordered`)

#"Price_Each" Column
Not_num_Price_Oct <- grepl("^[0-9]*\\.?[0-9]+$", Oct_Sales$Price_Each)
invalid_values_Price_Oct = Oct_Sales[!Not_num_Price_Oct, ]
#There are 95 rows which includes all the NAs and the non numeric values.

# Now lets convert from character to numeric
Oct_Sales$Price_Each <- as.numeric(Oct_Sales$Price_Each)
typeof(Oct_Sales$Price_Each)

#Lets work on the "Order_Date" column
Oct_Sales$Order_Date = mdy_hm(Oct_Sales$Order_Date)

#lets check the if there are any misspelling in columns "Product" and some incorrect entries
unique(Oct_Sales$Product)

#lets replace incorrect entries
Oct_Sales <- Oct_Sales %>%
  mutate(Product = replace(Product,
                           Product == "Product",NA))  

#"Purchase_Address" column
Oct_address_only_rows <- Oct_Sales %>%
  filter(Purchase_Address == "Purchase Address")
Oct_Sales <- Oct_Sales %>%
  mutate(Purchase_Address = replace(Purchase_Address,
                                    Purchase_Address == "Purchase Address",NA))

#lets split the column into three columns
Oct_Sales <- Oct_Sales %>%
  separate(Purchase_Address, into = c("Street_Address", "City", "State_Zip"), sep = ", ", remove = FALSE)

#now lets seperate the "state_zip" column by using " " as the delimiter.
Oct_Sales <- Oct_Sales %>%
  separate(State_Zip, into = c("State", "Zip"), sep = " ")

unique(Oct_Sales$City)
#there are no typing errors

#Order_ID column
Oct_Not_Numeric_Ord <- Oct_Sales[grepl("[^0-9]", Oct_Sales$`Order_ID`), ]
nrow(Oct_Not_Numeric_Ord) 

#lets replaces incorrect entries with NAs
Oct_Sales <- Oct_Sales %>%
  mutate(Order_ID = replace(Order_ID,
                            Order_ID == "Order ID",NA))

#lets remove all the NAs
sum(is.na(Oct_Sales))
str(Oct_Sales)

# There are 950 null values present out of  records in the dataset. 
#Now, lets check the location of the null values
colSums(is.na(Oct_Sales))
Oct_Sales_Clean <- na.omit(Oct_Sales)

write_csv(Oct_Sales_Clean,"New_Oct_Sales.csv")


#November Sales
Nov_Sales = read_csv(file="11_Sales_Nov.csv")

#show first six rows
head(Nov_Sales) 
#We need to change the columns name.
Nov_Sales <- Nov_Sales %>% rename(Order_ID = `Order ID`)
Nov_Sales <- Nov_Sales %>% rename(Quantity_Ordered = `Quantity Ordered`)
Nov_Sales <- Nov_Sales %>% rename(Price_Each = `Price Each`)
Nov_Sales <- Nov_Sales %>% rename(Order_Date = `Order Date`)
Nov_Sales <- Nov_Sales %>% rename(Purchase_Address = `Purchase Address`)

#show summary of the tibble
str(Nov_Sales)

# Lets work on column "Quantity_Ordered"
Not_num_Quantity_Nov <- Nov_Sales[grepl("[^0-9]", Nov_Sales$`Quantity_Ordered`), ]
nrow(Not_num_Quantity_Nov) 
#There are 17 non-numeric values present 

# Convert the data type to integer.
Nov_Sales$`Quantity_Ordered` <- as.integer(Nov_Sales$`Quantity_Ordered`)
typeof(Nov_Sales$`Quantity_Ordered`)

#"Price_Each" Column
Not_num_Price_Nov <- grepl("^[0-9]*\\.?[0-9]+$", Nov_Sales$Price_Each)
invalid_values_Price_Nov = Nov_Sales[!Not_num_Price_Nov, ]
#There are 81 rows which includes all the NAs and the non numeric values.

# Now lets convert from character to numeric
Nov_Sales$Price_Each <- as.numeric(Nov_Sales$Price_Each)
typeof(Nov_Sales$Price_Each)

#Lets work on the "Order_Date" column
Nov_Sales$Order_Date = mdy_hm(Nov_Sales$Order_Date)

#lets check the if there are any misspelling in columns "Product" and some incorrect entries
unique(Nov_Sales$Product)

#lets replace incorrect entries
Nov_Sales <- Nov_Sales %>%
  mutate(Product = replace(Product,
                           Product == "Product",NA))  

#"Purchase_Address" column
Nov_address_only_rows <- Nov_Sales %>%
  filter(Purchase_Address == "Purchase Address")
Nov_Sales <- Nov_Sales %>%
  mutate(Purchase_Address = replace(Purchase_Address,
                                    Purchase_Address == "Purchase Address",NA))

#lets split the column into three columns
Nov_Sales <- Nov_Sales %>%
  separate(Purchase_Address, into = c("Street_Address", "City", "State_Zip"), sep = ", ", remove = FALSE)

#now lets seperate the "state_zip" column by using " " as the delimiter.
Nov_Sales <- Nov_Sales %>%
  separate(State_Zip, into = c("State", "Zip"), sep = " ")

unique(Nov_Sales$City)
#there are no typing errors

#Order_ID column
Nov_Not_Numeric_Ord <- Nov_Sales[grepl("[^0-9]", Nov_Sales$`Order_ID`), ]
nrow(Nov_Not_Numeric_Ord) 

#lets replaces incorrect entries with NAs
Nov_Sales <- Nov_Sales %>%
  mutate(Order_ID = replace(Order_ID,
                            Order_ID == "Order ID",NA))

#lets remove all the NAs
sum(is.na(Nov_Sales))
str(Nov_Sales)

# There are 950 null values present out of  records in the dataset. 
#Now, lets check the location of the null values
colSums(is.na(Nov_Sales))
Nov_Sales_Clean <- na.omit(Nov_Sales)

write_csv(Nov_Sales_Clean,"New_Nov_Sales.csv")


#December Sales
Dec_Sales = read_csv(file="12_Sales_Dec.csv")

#show first six rows
head(Dec_Sales) 
#We need to change the columns name.
Dec_Sales <- Dec_Sales %>% rename(Order_ID = `Order ID`)
Dec_Sales <- Dec_Sales %>% rename(Quantity_Ordered = `Quantity Ordered`)
Dec_Sales <- Dec_Sales %>% rename(Price_Each = `Price Each`)
Dec_Sales <- Dec_Sales %>% rename(Order_Date = `Order Date`)
Dec_Sales <- Dec_Sales %>% rename(Purchase_Address = `Purchase Address`)

#show summary of the tibble
str(Dec_Sales)

# Lets work on column "Quantity_Ordered"
Not_num_Quantity_Dec <- Dec_Sales[grepl("[^0-9]", Dec_Sales$`Quantity_Ordered`), ]
nrow(Not_num_Quantity_Dec) 
#There are 17 non-numeric values present 

# Convert the data type to integer.
Dec_Sales$`Quantity_Ordered` <- as.integer(Dec_Sales$`Quantity_Ordered`)
typeof(Dec_Sales$`Quantity_Ordered`)

#"Price_Each" Column
Not_num_Price_Dec <- grepl("^[0-9]*\\.?[0-9]+$", Dec_Sales$Price_Each)
invalid_values_Price_Dec = Dec_Sales[!Not_num_Price_Dec, ]
#There are 129 rows which includes all the NAs and the non numeric values.

# Now lets convert from character to numeric
Dec_Sales$Price_Each <- as.numeric(Dec_Sales$Price_Each)
typeof(Dec_Sales$Price_Each)

#Lets work on the "Order_Date" column
Dec_Sales$Order_Date = mdy_hm(Dec_Sales$Order_Date)

#lets check the if there are any misspelling in columns "Product" and some incorrect entries
unique(Dec_Sales$Product)

#lets replace incorrect entries
Dec_Sales <- Dec_Sales %>%
  mutate(Product = replace(Product,
                           Product == "Product",NA))  

#"Purchase_Address" column
Dec_address_only_rows <- Dec_Sales %>%
  filter(Purchase_Address == "Purchase Address")
Dec_Sales <- Dec_Sales %>%
  mutate(Purchase_Address = replace(Purchase_Address,
                                    Purchase_Address == "Purchase Address",NA))

#lets split the column into three columns
Dec_Sales <- Dec_Sales %>%
  separate(Purchase_Address, into = c("Street_Address", "City", "State_Zip"), sep = ", ", remove = FALSE)

#now lets seperate the "state_zip" column by using " " as the delimiter.
Dec_Sales <- Dec_Sales %>%
  separate(State_Zip, into = c("State", "Zip"), sep = " ")

unique(Dec_Sales$City)
#there are no typing errors

#Order_ID column
Nov_Not_Numeric_Ord <- Dec_Sales[grepl("[^0-9]", Dec_Sales$`Order_ID`), ]
nrow(Nov_Not_Numeric_Ord) 

#lets replaces incorrect entries with NAs
Dec_Sales <- Dec_Sales %>%
  mutate(Order_ID = replace(Order_ID,
                            Order_ID == "Order ID",NA))

#lets remove all the NAs
sum(is.na(Dec_Sales))
str(Dec_Sales)

# There are 1290 null values present out of 25,133 records in the dataset. 
#Now, lets check the location of the null values
colSums(is.na(Dec_Sales))
Dec_Sales_Clean <- na.omit(Dec_Sales)

write_csv(Dec_Sales_Clean,"New_Dec_Sales.csv")

#Now lets merge all the clean files
file_names <- list(Jan_Sales_Clean, Feb_Sales_Clean, Mar_Sales_Clean,
                Apr_Sales_Clean, May_Sales_Clean, Jun_Sales_Clean,
                July_Sales_Clean, Aug_Sales_Clean, Sep_Sales_Clean,
                Oct_Sales_Clean, Nov_Sales_Clean, Dec_Sales_Clean)

# Read the files into a list of data frames
#file_list <- lapply(file_names, read.csv)

# Combine the data frames row-wise
Master_Dataset <- bind_rows(file_names)

#backing up master_dataset
write.csv(Master_Dataset, "Master_Dataset.csv")


head(Master_Dataset)
str(Master_Dataset)
unique(Master_Dataset$Product)

#creatring a column Order_Year,
Master_Dataset$Order_Year <- format(Master_Dataset$Order_Date, "%Y")

#Creating a column for Order_month
Master_Dataset = Master_Dataset%>%
  mutate(Order_Month = month(Order_Date, label = TRUE))

#Creating a column for Order_Day
Master_Dataset$Order_Day <- format(Master_Dataset$Order_Date, "%d")

#Crating a column for order month
Master_Dataset$Order_Hour <- format(Master_Dataset$Order_Date, "%H")

#Introducing a new variable Total_sales which is the product of price each and qty ordered
Master_Dataset$Total_sales = Master_Dataset$Quantity_Ordered* Master_Dataset$Price_Each

# Load necessary libraries
#library(ggplot2)

# Plot the distribution of Order_Date
#ggplot(Master_Dataset, aes(x = Order_Date)) +
#  geom_histogram(binwidth = 1000, fill = "blue", color = "black", alpha = 0.7) +
#  labs(title = "Distribution of Order Dates",
#       x = "Order Date",
#       y = "Frequency") +
#  theme_minimal()

summary(Master_Dataset)
unique(Master_Dataset$Order_Year)

# Calculate the number of sales for each year
sales_by_year = Master_Dataset %>%
  group_by(Order_Year) %>%
  summarise(No_of_sales = n())

# Calculate the percentage of total sales for each year
sales_by_year <- sales_by_year %>%
  mutate(Percentage_of_Sales = (No_of_sales / sum(No_of_sales)) * 100)

print(sales_by_year)

#Year 2001 and 2028 have only 1 sale each are also very separated from the rest of the data so obviously, this is erroneous data, and we'll remove it.

#Removing errornous data
Master_Dataset = subset(Master_Dataset, Order_Year %in% c('2019','2020','2021'))

#Unique values in city names
unique(Master_Dataset$City)

#Replacing wrong city names with correct ones
Master_Dataset <- Master_Dataset %>%
  mutate(City = str_replace_all(City, "Las Angeles","Los Angeles")) %>%
  mutate(City = str_replace_all(City, "SanFrancisco", "San Francisco"))

# Calculate the number of sales for each year
sales_by_year_clean = Master_Dataset %>%
  group_by(Order_Year) %>%
  summarise(No_of_sales = n())

# Calculate the percentage of total sales for each year
sales_by_year_clean <- sales_by_year_clean %>%
  mutate(Percentage_of_Sales = (No_of_sales / sum(No_of_sales)) * 100)

# TASK2(ANSWERING THE QUESTIONS)#########################################################################

# PART A: Determine the worst year for sales and the total sales for that year

# Summarize the total sales for each year
total_sales_by_year <- Master_Dataset %>%
  group_by(Order_Year) %>%
  summarise(Total_Sales = sum(Quantity_Ordered * Price_Each, na.rm = TRUE))  # Calculate total sales by multiplying Quantity_Ordered by Price_Each and summing the result, while ignoring missing values

# Find the year with the minimum total sales
worst_year <- total_sales_by_year %>%
  filter(Total_Sales == min(Total_Sales))  # Filter to get the year with the minimum total sales
print(worst_year)  # Print the worst year and its total sales

# The worst year for sales was 2021, with total sales of only $3927


# Part B: Determine the best year for sales and the total sales for that year

# Find the year with the maximum total sales
best_year <- total_sales_by_year %>%
  filter(Total_Sales == max(Total_Sales))  # Filter to get the year with the maximum total sales

print(best_year)  # Print the best year and its total sales

# The best year for sales was 2019, with a total sales figure of $34,483,366


# Part C: In the best year of sales which was the best month for sales?

# Subset the dataset to include only the sales data for 2019
sales_data2019 <- subset(Master_Dataset, Order_Year == 2019)

# Summarize the total sales for each month in 2019
monthly_sales2019 <- sales_data2019 %>%
  mutate(Order_Month = month(Order_Date, label = TRUE)) %>%  # Extract the month from the order date and add it as a new column
  group_by(Order_Month) %>%
  summarise(Total_sales = sum(Quantity_Ordered * Price_Each, na.rm = TRUE))  # Calculate total sales for each month by multiplying Quantity_Ordered by Price_Each and summing the result, while ignoring missing values

# Find the month with the maximum total sales in 2019
best_month <- monthly_sales2019 %>%
  filter(Total_sales == max(Total_sales))  # Filter to get the month with the maximum total sales

print(best_month)  # Print the best month and its total sales

# The best month of sales in 2019 was December (12) with a total sales amount of $4,613,443


# Part D: Determine the total revenue earned in the best month of the best year (2019)

# Print the total sales for the best month
print(best_month$Total_sales)  # Extract and print the total sales value for the best month

# The total revenue earned in the best month of the best year of sales is $4,613,443

# Part E: Identify the city with the most sales in the best year (2019)

# Summarize the total sales for each city in 2019
citywise_sales2019 <- sales_data2019 %>%
  group_by(City) %>%
  summarise(Total_sales = sum(Quantity_Ordered * Price_Each, na.rm = TRUE))  # Calculate total sales for each city by multiplying Quantity_Ordered by Price_Each and summing the result, while ignoring missing values

# Find the city with the maximum total sales in 2019
best_city <- citywise_sales2019 %>%
  filter(Total_sales == max(Total_sales))  # Filter to get the city with the maximum total sales

print(best_city)  # Print the best city and its total sales

# San Francisco is the city with the best sales figures, totaling $8,259,719

# Part F: Determine the best time to advertise to maximize the likelihood of making sales, based on the best year (2019)

# Summarize the number of sales for each hour of the day in 2019
sales_by_hour2019 <- sales_data2019 %>%
  group_by(Order_Hour) %>%
  summarise(No_of_sales = n())  # Count the number of sales for each hour

# Find the hour with the maximum number of sales in 2019
best_hour <- sales_by_hour2019 %>%
  filter(No_of_sales == max(No_of_sales))  # Filter to get the hour with the maximum number of sales

print(best_hour)  # Print the best hour and the number of sales

# The time between 19:00 - 19:59 is the best to advertise for maximum sales conversion.


# Part G: Identify the products most often sold together

# Identify orders that include multiple products
order_product_pairs <- Master_Dataset %>%
  select(Order_ID, Product) %>%
  group_by(Order_ID) %>%
  filter(n() > 1) %>%
  ungroup()  # Select orders with more than one product

# Find pairs of products that are sold together in the same order
product_pairs <- order_product_pairs %>%
  inner_join(order_product_pairs, by = "Order_ID") %>%
  filter(Product.x != Product.y) %>%  # Exclude pairs where the products are the same
  group_by(Product.x, Product.y) %>%
  summarise(count = n()) %>%  # Count the number of times each pair of products is sold together
  arrange(desc(count))  # Arrange pairs in descending order of their count

# Display the most common pairs of products sold together
most_common_pairs <- product_pairs %>%
  top_n(10, count)  # Select the top 10 most common pairs

# Print the result
print(most_common_pairs)  # Print the most common pairs of products sold together


# PART H: Identify the most sold product

# Calculate the total quantity ordered for each product
total_quantity_by_product <- Master_Dataset %>%
  group_by(Product) %>%
  summarise(Total_Quantity = sum(Quantity_Ordered, na.rm = TRUE))  # Sum the quantity ordered for each product, ignoring missing values

# Identify the product with the highest total quantity
most_sold_product <- total_quantity_by_product %>%
  filter(Total_Quantity == max(Total_Quantity))  # Filter to get the product with the maximum total quantity

# Print the result
print(most_sold_product)  # Print the most sold product and its total quantity

# The product "AAA Batteries (4-pack)" is the most sold product overall with 31,020 units sold.
# This can be due to various reasons, such as its usefulness and versatility, as it is needed to operate many different products.
# Additionally, it is a low-priced product.



# PART I: Identify the least sold product in the best year of sales (2019)

# Calculate the total quantity ordered for each product in 2019
total_quantity_by_product2019 <- sales_data2019 %>%
  group_by(Product) %>%
  summarise(Total_Quantity = sum(Quantity_Ordered, na.rm = TRUE))  # Sum the quantity ordered for each product, ignoring missing values

# Identify the product with the lowest total quantity in 2019
least_sold_product2019 <- total_quantity_by_product2019 %>%
  filter(Total_Quantity == min(Total_Quantity))  # Filter to get the product with the minimum total quantity

# Print the result
print(least_sold_product2019)  # Print the least sold product and its total quantity

# The least sold product in 2019 was "LG Dryer" with only 646 quantities sold in the entire year.


#TASK 3(VISUALISATIONS)#########################################################################

# MONTHLY SALES TREND VS MONTHLY AVERAGE SALES
# As we already know the best year of sales is 2019

# Calculate the average monthly sales for 2019
average_monthly_sales <- mean(monthly_sales2019$Total_sales)

# Plot the monthly sales trend vs monthly average sales
ggplot(monthly_sales2019, aes(x = Order_Month, y = Total_sales, group = 1)) +
  geom_line(color = "blue", size = 1) +  # Line plot for total sales by month
  geom_point(size = 2, color = "red") +  # Points for total sales by month
  geom_text(aes(label = scales::label_number(scale = 1e-6, suffix = "M", accuracy = 0.001)(Total_sales)), 
            vjust = -0.7, hjust = 0.8, color = "black", size = 3) +  # Annotate points with sales values
  geom_hline(yintercept = average_monthly_sales, linetype = "dashed", color = "red") +  # Add a horizontal line for average sales
  annotate("text", x = 2.1, y = average_monthly_sales + 0.1e6, 
           label = paste("avg =", scales::label_number(scale = 1e-6, suffix = "M", accuracy = 0.01)(average_monthly_sales)), 
           color = "red", hjust = 1) +  # Annotate average sales
  labs(title = "Monthly Sales Trend vs Monthly Average Sales",
       y = "Sales (in Millions)",
       x = "Month") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M", accuracy = 0.1), limits = c(1.75e6, 5e6)) +  # Customize y-axis labels and limits
  theme_minimal()  # Apply minimal theme

# SALES BY STATE

# Calculate total sales by state in 2019
sales_by_state <- sales_data2019 %>%
  group_by(State) %>%
  summarise(Total_Sales = sum(Total_sales))  # Sum total sales for each state

# Create a color for each of the states
colors <- scales::hue_pal()(length(unique(sales_by_state$State)))

# Create a vertical lollipop chart for sales by state
ggplot(sales_by_state, aes(x = reorder(State, -Total_Sales), y = Total_Sales, color = State)) +
  geom_segment(aes(x = State, xend = State, y = 0, yend = Total_Sales), color = "brown") +  # Add lollipop sticks
  geom_point(size = 5) +  # Add lollipop heads
  geom_text(aes(label = scales::label_number(scale = 1e-6, suffix = "M")(Total_Sales)), 
            vjust = -1.2, hjust = 0.5, color = "black", size = 3) +  # Annotate points with sales values
  scale_color_manual(values = colors) +  # Apply custom colors
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"), limits = c(0, 15e6)) +  # Customize y-axis labels and limits
  labs(title = "Sales by State",
       y = "Total Sales (in Millions)",
       x = "State") +
  theme_minimal() +  # Apply minimal theme
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5))  # Customize x-axis text

# TOP 10 PRODUCTS SOLD IN THE BEST YEAR OF SALES

# Calculate total quantity ordered for each product in 2019
top_10_products <- sales_data2019 %>%
  group_by(Product) %>%
  summarise(Quantity_Ordered = sum(Quantity_Ordered, na.rm = TRUE)) %>%
  arrange(desc(Quantity_Ordered)) %>%
  head(10)  # Select the top 10 products

# Plot the top 10 products sold in 2019
ggplot(top_10_products, aes(x = reorder(Product, -Quantity_Ordered), y = Quantity_Ordered)) +
  geom_bar(stat = "identity", fill = "purple") +  # Create a bar plot
  geom_text(aes(label = Quantity_Ordered), hjust = 1.1, color = "white", size = 3) +  # Annotate bars with quantities
  coord_flip() +  # Flip coordinates for horizontal bars
  labs(title = "Top 10 Products Sold in the Best Year of Sales",
       x = "Product",
       y = "Quantity Ordered") +
  theme_minimal() +  # Apply minimal theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())  # Remove grid lines

# MONTHLY ORDER TREND VS MONTHLY AVERAGE ORDER

# Calculate the number of orders per month in 2019
monthly_orders2019 <- sales_data2019 %>%
  group_by(Order_Month) %>%
  summarise(Order_Count = n_distinct(Order_ID))  # Count distinct orders per month

# Calculate the average number of orders per month in 2019
monthly_avg_order_count <- mean(monthly_orders2019$Order_Count)

# Plot the monthly order count vs monthly average order count
ggplot() +
  geom_line(data = monthly_orders2019, aes(x = Order_Month, y = Order_Count, group = 1), color = "blue", size = 1) +  # Line plot for order count by month
  geom_point(data = monthly_orders2019, aes(x = Order_Month, y = Order_Count), color = "red", size = 2) +  # Points for order count by month
  geom_text(data = monthly_orders2019, aes(x = Order_Month, y = Order_Count, label = round(Order_Count, 0)), 
            vjust = -0.75, hjust = 0.9, color = "black", size = 3) +  # Annotate points with order counts
  geom_hline(yintercept = monthly_avg_order_count, linetype = "dashed", color = "red") +  # Add a horizontal line for average order count
  annotate("text", x = 2.5, y = monthly_avg_order_count + 350, 
           label = paste("avg =", round(monthly_avg_order_count, 2)), color = "red", hjust = 1) +  # Annotate average order count
  labs(title = "Monthly Order Count vs Monthly Average Order Count (Best Year)",
       x = "Month",
       y = "Number of Orders") +
  theme_minimal()  # Apply minimal theme

# DAILY ORDER TREND VS DAILY AVERAGE

# Calculate the number of orders per day in 2019
daily_order_count2019 <- sales_data2019 %>%
  group_by(Order_Day) %>%
  summarise(Order_Count = n_distinct(Order_ID))  # Count distinct orders per day

# Calculate the average number of orders per day in 2019
daily_avg_order_count <- mean(daily_order_count2019$Order_Count)

# Plot the daily order trend vs daily average order count
ggplot(daily_order_count2019, aes(x = Order_Day, y = Order_Count, group = 1)) +
  geom_line(color = "blue", size = 1) +  # Line plot for order count by day
  geom_point(color = "green", size = 2) +  # Points for order count by day
  geom_hline(yintercept = daily_avg_order_count, linetype = "dashed", color = "red") +  # Add a horizontal line for average order count
  annotate("text", x = 28, y = daily_avg_order_count - 65, 
           label = paste("avg =", round(daily_avg_order_count, 2)), color = "red", hjust = 1) +  # Annotate average order count
  labs(title = "Daily Order Trend vs Daily Average Order Count (Best Year)",
       x = "Date",
       y = "Number of Orders") +
  theme_minimal()  # Apply minimal theme

# HOURLY ORDER TREND VS HOURLY AVERAGE ORDER

# Calculate the number of orders per hour in 2019
hourly_order_count2019 <- sales_data2019 %>%
  group_by(Order_Hour) %>%
  summarise(Order_Count = n_distinct(Order_ID))  # Count distinct orders per hour

# Calculate the average number of orders per hour in 2019
hourly_avg_order_count <- mean(hourly_order_count2019$Order_Count)

# Plot the hourly order trend vs hourly average order count
ggplot(hourly_order_count2019, aes(x = as.numeric(Order_Hour), y = Order_Count, group = 1)) +
  geom_line(color = "orange", size = 1) +  # Line plot for order count by hour
  geom_point(color = "coral", size = 3) +  # Points for order count by hour
  geom_text(aes(label = round(Order_Count, 0)), vjust = -0.7, hjust = 0.5, color = "black", size = 2.6) +  # Annotate points with order counts
  geom_hline(yintercept = hourly_avg_order_count, linetype = "dashed", color = "red") +  # Add a horizontal line for average order count
  annotate("text", x = 0.9, y = hourly_avg_order_count + 300, 
           label = paste("avg =", round(hourly_avg_order_count, 2)), color = "red") +  # Annotate average order count
  labs(title = "Hourly Order Trend vs Hourly Average Order Count (Best Year)",
       x = "Hour of the Day",
       y = "Number of Orders") +
  scale_x_continuous(breaks = 0:23) +  # Customize x-axis breaks for hours
  theme_minimal()  # Apply minimal theme



#TASK4(PREDICTING)##########################################################################

# Creating merged_dataset as a copy of Master_Dataset to start working.
merged_dataset <- Master_Dataset
head(merged_dataset)  # Display the first few rows of the dataset
str(merged_dataset)  # Display the structure of the dataset

# Exploratory data analysis
# Looking at summary statistics
summary(merged_dataset)

# Our target variable is total sales, we need to predict future sales
# Checking for the distribution of the 'Total_sales'

# Plotting the distribution of Total_sales
ggplot(merged_dataset, aes(x = Total_sales)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Total_sales", x = "Total_sales", y = "Frequency")

# Here, we can see that the data for Total_sales is right skewed.
# In practical sense the data should be skewed, so there was no point doing normalization.

# Creating a box plot for Total_sales
ggplot(merged_dataset, aes(y = Total_sales)) +
  geom_boxplot(fill = "green", color = "blue") +
  labs(title = "Box Plot of Total_sales", y = "Total_sales")

# Q-Q plot to check for normality
# qqnorm(merged_dataset$Total_sales, main = "Q-Q Plot of Total_sales")
# qqline(merged_dataset$Total_sales, col = "red")

# In order to know what are the significant features that can predict sales we would make a correlation matrix.

# Loading necessary libraries
library(lubridate)
library(corrplot)

# Data Preparation
# Convert Order_Date to numeric features
merged_dataset <- merged_dataset %>%
  mutate(Year = year(Order_Date),
         Month = month(Order_Date),
         Day = day(Order_Date))

# Convert Product, City, State, Zip to numeric
merged_dataset <- merged_dataset %>%
  mutate(Product = as.numeric(factor(Product)),
         City = as.numeric(factor(City)),
         State = as.numeric(factor(State)),
         Zip = as.numeric(Zip))

str(merged_dataset)  # Display the structure of the modified dataset

# Select relevant columns for correlation analysis
correlation_data <- merged_dataset %>%
  select(Product, Quantity_Ordered, Year, Month, Day, City, State, Zip, Price_Each, Total_sales)

# Compute Correlation Matrix
correlation_matrix <- cor(correlation_data, use = "complete.obs")
correlation_matrix  # Display the correlation matrix

# Plot Correlation Matrix Heatmap
corrplot(correlation_matrix, method = "color", tl.col = "orange", tl.srt = 45)

# Extract correlations with Total_sales
cor_Total_sales <- as.data.frame(as.table(correlation_matrix)) %>%
  filter(Var1 == "Total_sales" & Var2 != "Total_sales" & abs(Freq) > 0.1) %>%
  arrange(desc(abs(Freq)))

# Rename columns for clarity
colnames(cor_Total_sales) <- c("total sales column", "Variable", "Correlation_with_Total_sales")

cor_Total_sales  # Display the significant correlations with Total_sales
# This table clearly signifies that correlation above 0.1 with Total_sales is just with Price_Each and Quantity_Ordered.

# Still, we can do another check by using Random Forests that will tell what variables are important in predicting Total_sales.
library(randomForest)

# Select relevant columns or features for the Random Forest model
feature_data <- merged_dataset %>%
  select(Product, Quantity_Ordered, Price_Each, City, State, Zip, Year, Month, Day, Total_sales)

# Train a Random Forest model to determine feature importance
rf_model <- randomForest(Total_sales ~ ., data = feature_data)
importance_Total_sales <- importance(rf_model)  # This gives the importance score to each variable.

# Converting it into a data frame.
importance_Total_sales_df <- as.data.frame(importance_Total_sales)

# Convert scientific notation to normal values
importance_Total_sales_df$IncNodePurity <- format(importance_Total_sales_df$IncNodePurity, scientific = FALSE)

# Ordering the values to make some sense out of the data. 
# This would give the order of importance from lowest to highest.
order(importance(rf_model))

# Visualizing which features are most important
varImpPlot(rf_model, pch = 18, col = "red", cex = 1.5)

# This plot suggests that top 3 most important features are - Price_Each, Product, and Quantity_Ordered.

str(merged_dataset)  # Display the structure of the dataset again

# There is obviously a linear relationship between the Total_sales and these 3 important variables.

# Installing and Loading the required libraries

# 1. Check Assumptions of Linear Regression
# 1a. Check for autocorrelation
dw_test <- durbinWatsonTest(lm(Total_sales ~ Product + Quantity_Ordered + Price_Each, data = merged_dataset))

# This test gives a Dw test value of around 2 which indicates that there is no autocorrelation.
# This means that the variables are not correlated with its lagged values.

# 1b. Check for heteroskedasticity
bp_test <- bptest(lm(Total_sales ~ Product + Quantity_Ordered + Price_Each, data = merged_dataset))
# This test is for knowing whether the data is heteroskedastic, i.e., if for higher values of explanatory variables, the value of errors also increase. 
# If p-value is less than the significance level then heteroskedasticity exists. 
# Here p-value is way below 0.05, thus heteroskedasticity exists.

# 1c. Check for multicollinearity
vif_test <- vif(lm(Total_sales ~ Product + Quantity_Ordered + Price_Each, data = merged_dataset))
# Here VIF values for each explanatory variable are- 1.01, 1.03, and 1.02.
# Higher the VIF values more is the correlation with other variables. 
# Multicollinearity indicates the extent to which a variable is correlated with other explanatory variables.

# Thus our model seems good to go as it does not violate the assumptions of autocorrelation and multicollinearity.
# Although the data is heteroskedastic, that is what you expect with this kind of time-series data.

# 2. Split the dataset into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(merged_dataset$Total_sales, p = .8, list = FALSE, times = 1)
train_data <- merged_dataset[trainIndex, ]
test_data <- merged_dataset[-trainIndex, ]

# 3. Create a regression model
model <- lm(Total_sales ~ Product + Quantity_Ordered + Price_Each, data = train_data)

# Predict on test data
predictions <- predict(model, test_data)

# Calculate RMSE
rmse <- sqrt(mean((predictions - test_data$Total_sales)^2))
rmse  # Display the RMSE
# The value of RMSE is  13.6533

# 4. Cross-Validation and Hyperparameter Tuning
# Set up train control
train_control <- trainControl(method = "cv", number = 10)

# Train the model using cross-validation
cv_model <- train(Total_sales ~ Product + Quantity_Ordered + Price_Each, 
                  data = train_data, method = "lm", trControl = train_control)

print(cv_model)  # Display the cross-validated model results

# Make predictions on test data
cv_predictions <- predict(cv_model, test_data)

# Calculate RMSE for the cross-validated model
cv_rmse <- sqrt(mean((cv_predictions - test_data$Total_sales)^2))
cv_rmse  # Display the RMSE for the cross-validated model

# Model 2: Random Forest Model

# Brief introduction - 

# Prepare the data
features <- c('Product', 'Quantity_Ordered', 'Price_Each', 'City', 'State', 'Month')
X <- merged_dataset[, features]
y <- merged_dataset$Total_sales

# 1. Split the data into train and test sets
set.seed(42)
trainIndex <- createDataPartition(y, p = .8, list = FALSE, times = 1)

X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# 2. Run the Random Forest model on train data
rf_model <- randomForest(x = X_train, y = y_train, random_state = 42)

# 3. Test the model using test data
y_pred <- predict(rf_model, X_test)

# 4. Give an RMSE score
rmse_rf <- sqrt(mean((y_pred - y_test)^2))
cat('RMSE (Initial Random Forest):', rmse_rf, '\n')
# Here RMSE is 8.737875, which is lower than the regression model.

# 5. Tune the hyperparameters or pursue cross-validation to improve the model
# Define the parameter grid
tune_grid <- expand.grid(
  mtry = c(2, 3, 4)
)

# Perform Grid Search with cross-validation
control <- trainControl(method = "cv", number = 3)
tuned_rf_model <- train(
  x = X_train,
  y = y_train,
  method = "rf",
  tuneGrid = tune_grid,
  trControl = control,
  ntree = 100
)

# Get the best parameters
best_params <- tuned_rf_model$bestTune
best_rf_model <- randomForest(
  x = X_train, y = y_train,
  mtry = best_params$mtry,
  ntree = 100
)

# Predict on test data using the best model
best_y_pred_rf <- predict(best_rf_model, X_test)
best_rmse_rf <- sqrt(mean((best_y_pred_rf - y_test)^2))

# Create a data frame for visualization
rmse_data <- data.frame(
  Model = c("Initial Linear Regression", "CV Linear Regression", "Initial Random Forest", "Tuned Random Forest"),
  RMSE = c(rmse, cv_rmse, rmse_rf, best_rmse_rf)
)

# Create a bar plot to compare RMSE values of different models
ggplot(rmse_data, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(RMSE, 2)), vjust = 1.3, color = "black", size = 5) +
  theme_minimal() +
  labs(title = "RMSE Comparison of Models",
       x = "Model",
       y = "RMSE") +
  theme(legend.position = "none")

# Thus we can see that RMSE for random forest is lower than a linear regression model.
# Also, after tuning the hyperparameter, the best model gave an RMSE of 2.32 (approx.). This has significantly improved the accuracy of the model.