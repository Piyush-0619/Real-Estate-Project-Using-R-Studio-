**Problem Statement & Other Instructions**
Part 1 score should be grater than 7
Part 2 score should be greater than 0.51

**Part 1**
Part 1 of the project contains a quiz which is present in the section 'submit your part 1 solution here'. There are 10 questions of 1 mark each and you need to score atleast 7 out of 10 in order to pass the quiz i.e. to pass part 1 of the project.

**Part 2**
Price of a property is one of the most important decision criterion when people buy homes. Real state firms need to be consistent in their pricing in order to attract buyers . Having a predictive model for the same will be great tool to have , which in turn can also be used to tweak development of properties , putting more emphasis on qualities which increase the value of the property.

We have given you two datasets , housing_train.csv and housing_test.csv . You need to use data housing_train to build predictive model for response variable "Price". Housing_test data contains all other factors except "Price", you need to predict that using the model that you developed and submit your predicted values in a csv files.

Evaluation Criterion : 
Score will be calculated as:
Score = 212467/RMSE (Note : Dont worry about change in scoring method , this is just a cosmetic change to alter scale of score , passing criterion hasn't changed and you dont need to resubmit )
Where RMSE is root mean square error on test file. 

Please read through the points given below before you begin : 
1. Your score for test data should come out to be more than 0.51
2. You are NOT required to submit R script. However in some cases , we might ask you to send your script separately in order to verify that your submissions is a result of models that you built .
3. you can submit as many times as you want, we'll put best score obtained on the respective leader board. 
4. Your predictions should not contain any NA values.
5. You are are free to use any predictive modelling technique
errors=t2$Price-t2.pred
rmse=errors**2 %>% mean() %>% sqrt()
Score = 212467/rmse
Score
###
