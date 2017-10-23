# NLP of Amazon Review


Could download the dataset via following link:
# https://www.kaggle.com/snap/amazon-fine-food-reviews/data



For this project I only random slect 20K records. 

lowcase 
n't to not
remove punctuation
Porter's stemming
sentiment the review using AFINN-111
select the top 6 most of reviews product to compare "myscore" and Amazon 5 stars rating



I modified the stopword list "english" and created "mystoplist" neglect the "no" and "not" cuz i see them as negative and should be given marks when sentiment the reviews

#SUBSTITUTE n't to not
top_25k$normalized <- sub(c("isn't|aren't|wasn't|weren't|hasn't|
       haven't|hadn't|doesn't|don't|didn't|
       won't|wouldn't|shan't|shouldn't|can't|
       cannot|couldn't|mustn't"),"not", top_25k$normalized)


