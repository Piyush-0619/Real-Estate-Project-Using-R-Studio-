library(car)
library(tidymodels)
library(tidyr)
library(visdat)
setwd("C:/Users/singh/OneDrive/Desktop/project 1")
re_train=read.csv("housing_train (1).csv")
re_test=read.csv("housing_test (1).csv")
#re_test$Price=NA
#re_test$data='test'
#re_train$data='train'

cor.test(x=re_train$Rooms,y=re_train$Bathroom)


glimpse(re_train)

func_year=function(x){
  x=2022-x
  return(x)
}


dp_pipe=recipe(Price~.,data = re_train)%>%
  update_role(Address,Bedroom2,new_role = "drop_vars")%>%
  step_rm(has_role("drop_vars"))%>%
  
 
  update_role(Suburb,Method,Type,SellerG,
              CouncilArea,new_role =  "to_dummies")%>%
   
  step_other(has_role("to_dummies"),threshold =0.02,other="__other__") %>%
  step_dummy(has_role("to_dummies"))%>%
  step_mutate_at(YearBuilt,fn=func_year)%>%
  step_impute_median(all_numeric(),-all_outcomes())

dp_pipe=prep(dp_pipe)



train=bake(dp_pipe,new_data = NULL)
test=bake(dp_pipe,new_data=re_test)

colSums(is.na(re_train))


set.seed(2)
s=sample(1:nrow(train),0.8*nrow(train))
t1=train[s,]
t2=train[-s,]
fit=lm(Price~.,data=t1)
summary(fit)
sort(vif(fit),decreasing = T)
fit=stats::step(fit)
formula(fit)

fit_final=lm(Price ~ Rooms + Distance + Postcode + Bathroom + Car + Landsize + 
               BuildingArea + YearBuilt + Suburb_Reservoir + Suburb_Richmond + 
               Type_t + Type_u + Method_S + Method_SP +  
               SellerG_Biggin + SellerG_hockingstuart + SellerG_Jellis + 
               SellerG_Marshall  + SellerG_X__other__ + CouncilArea_Banyule + 
               CouncilArea_Bayside + CouncilArea_Boroondara + CouncilArea_Brimbank + 
               CouncilArea_Darebin + CouncilArea_Glen.Eira + 
               CouncilArea_Manningham + CouncilArea_Maribyrnong + CouncilArea_Melbourne + 
               CouncilArea_Moonee.Valley + CouncilArea_Moreland + CouncilArea_Port.Phillip + 
                CouncilArea_Yarra + CouncilArea_X__other__,data = t1)
summary(fit_final)

t2.pred=predict(fit_final,newdata=t2)

errors=t2$Price-t2.pred

rmse=errors**2 %>% mean() %>% sqrt()
Score = 212467/rmse
Score
###


