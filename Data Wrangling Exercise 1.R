## Load libraries
library(dplyr)
library(tidyr)

## Uploading data into R
refine_original <- read.csv("refine_original.csv", stringsAsFactors = FALSE)
View(refine_original)

## Assigning the original data frame to a new data frame   
refine_original2 <- refine_original   
   
## Cleaning up brand names
is_phillips <- grepl("p", refine_original2$company, ignore.case = TRUE)
refine_original2$company[is_phillips] <- "phillips"

is_akzo <- grepl("ak", refine_original2$company, ignore.case = TRUE)
refine_original2$company[is_akzo] <- "akzo"

is_van <- grepl("van", refine_original2$company, ignore.case = TRUE)
refine_original2$company[is_van] <- "van houten"

is_unilever <- grepl("uni", refine_original2$company, ignore.case = TRUE)
refine_original2$company[is_unilever] <- "unilever"

   
## Seperating product code and number
refine_original2 <- separate(refine_original,Product.code...number,c("product_code","product_number"), sep = "-")


## Adding full address for geocoding
refine_original2<-unite(refine_original2,"full_address",address, city, country, sep = ",  ")


## Adding product categories
mutate(refine_original2,product_categories = product_code)
refine_original2<-mutate(refine_original2,product_categories = product_code)


## creating product categories defined as (p = "Smartphone", v ="TV",x = "Laptop",q ="Tablet")
is_Smartphone <- grepl("p", refine_original2$product_categories)
refine_original2$product_categories[is_Smartphone] <- "Smartphone"

is_TV <- grepl("v", refine_original2$product_categories)
refine_original2$product_categories[is_TV] <- "TV"

is_Laptop <- grepl("x", refine_original2$product_categories)
refine_original2$product_categories[is_Laptop] <- "Laptop"

is_Tablet <- grepl("q", refine_original2$product_categories)
refine_original2$product_categories[is_Tablet] <- "Tablet"


## Creating dummy variables for company 
refine_original2 = within(refine_original2, {
  company_phillips = ifelse(company == "phillips", 1, 0)
  company_akzo = ifelse(company == "akzo", 1, 0)
  company_unilever = ifelse(company == "unilever",1,0)
  'company_van houten' = ifelse(company == "van houten",1,0)
})

## Creating dummy variables for product category
refine_original2 = within(refine_original2, {
  product_smartphone = ifelse(product_code == "p", 1, 0)
  product_tv = ifelse(product_code == "v", 1, 0)
  product_laptop = ifelse(product_code == "x",1,0)
  product_tablet = ifelse(product_code == "q",1,0)
})


##Print out my new dataframe
refine_original2

## Specifying the objects in the  new dataframe
str(refine_original2)

## Write the new dataframe to a cvs file called refine_clean
write.csv(refine_original2, file = "refine_clean.csv")

