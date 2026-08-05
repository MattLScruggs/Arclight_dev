library(tidyverse)
library(ggridges)
source("lit_review_functions_final.R")

#### Load dataset ####
nlp_df<-read.delim("Large_NLP_news_df.csv", sep="|", quote= "",header=TRUE,fill=TRUE)
nlp_df<-nlp_df[!duplicated(nlp_df),]
colnames(nlp_df)<-c("url","text","class")
nlp_df$class<-sapply(nlp_df$class,function(i) gsub(",","",i))
nlp_df<-nlp_df[nlp_df$class %in% c("1","0"),]
nlp_df$text<-sapply(nlp_df$text, function(i) gsub("|","",i))

write.table(nlp_df, file="final_dataset.csv",sep="|",row.names=FALSE, qmethod="double")
test<-read.table("final_dataset.csv", sep="|", header=TRUE)

#### Create word df and tfidf ####
word_df<-split_words_text(nlp_df$text,nlp_df$url)
tfidf_df<-calc_baseline_tf_idf(word_df, "word", "doc")


#### Make tfidf matrix ####
high_dim_matrix<-make_tf_idf_matrix(tfidf_df, word_col = "word", line_col = "line")
tiny_matrix<-high_dim_matrix[1:100,15:30]

vari_list<-seq(from=0.5,to=0.5,by=0.1)
run_list<-seq(from=1,to=50, by=1)
tt_split<-seq(from=0.25,to=0.25, by=0.1)

param_df<-expand.grid(vari_list,
                      run_list,
                      tt_split)


result_df<-matrix(nrow=nrow(param_df), ncol=6, data=NA)
colnames(result_df)<-c("run","tt_split","variance","accuracy","precision","recall")


#### loop ####

for (i in 1:nrow(param_df)){
  

variance_set<-param_df[i,1]
tt_ratio<-param_df[i,3 ]
run_nr<-param_df[i, 2]

low_dim_embedding<-map_to_low_dim(high_dim_matrix,variance_threshold = variance_set)

#### Low-dim embedding ####
pca_df<-low_dim_embedding$pc_df

#### Assign class value to each ####
class_idx<-NULL

for (j in 1:nrow(pca_df)){
  nme<-rownames(pca_df)[j]
  cls<-nlp_df[nlp_df$url==nme,3]
  class_idx[j]<-cls
}
pca_df$class<-class_idx

#### train test split ####
idx<-sample(1:nrow(pca_df), nrow(pca_df)*tt_ratio)
train_pca_df<-pca_df[-idx,]
test_pca_df<-pca_df[idx, ]

#### Fit LDA ####
lda_results<-fit_lda(train_pca_df, "class")

train_pca_df$predictions<-lda_results$pred_y$predictions
train_pca_df$correct_pred<-0
train_pca_df$correct[train_pca_df$class==train_pca_df$predictions]<-1
table(train_pca_df$predictions,train_pca_df$class)


lda_test_results<-predict_lda(test_pca_df,"class",lda_results)
test_pca_df$predictions<-lda_test_results$predictions
test_pca_df$correct_pred<-0
test_pca_df$correct[test_pca_df$class==test_pca_df$predictions]<-1
table(test_pca_df$predictions,test_pca_df$class)


acc<-round(sum(test_pca_df$correct)/length(test_pca_df$correct),2)
prec<-round(sum(test_pca_df$correct[test_pca_df$predictions==1])/
              length(test_pca_df$predictions[test_pca_df$predictions==1]),2)
rec<-round(sum(test_pca_df$correct[test_pca_df$predictions==1])/
             length(test_pca_df$class[test_pca_df$class==1]),2)


result_df[i,1]<-run_nr
result_df[i,2]<-tt_ratio
result_df[i,3]<-variance_set
result_df[i,4]<-acc
result_df[i,5]<-prec
result_df[i,6]<-rec


}


result_df<-as.data.frame(result_df)

result_df$F1<-2*((result_df$precision*result_df$recall)/(result_df$precision+result_df$recall))

write.csv(result_df, "multiple_run_results_same_variance_and_split.csv", quote=FALSE, row.names = FALSE)

### Stability
result_df%>%
  select(F1)%>%
  ggplot(aes(x=F1))+
  geom_histogram(color="darkblue", fill="lightblue", bins=12)+
  geom_density(alpha = 0.2, fill = "lightblue")+
  labs(title="Distribution of F1 Score at Optimal Parameters",
       x="F1 Score",
       y="Number of Runs")

result_df%>%
  select(recall)%>%
  ggplot(aes(x=recall))+
  geom_histogram(color="darkgreen", fill="lightgreen", bins=12)+
  geom_density(alpha = 0.2, fill = "lightgreen")+
  labs(title="Distribution of Recall at Optimal Parameters",
       x="Recall",
       y="Number of Runs")

result_df%>%
  select(precision)%>%
  ggplot(aes(x=precision))+
  geom_histogram(color="darkred", fill="red1" , bins=12)+
  geom_density(alpha = 0.2, fill = "red1")+
  labs(title="Distribution of Precision at Optimal Parameters",
       x="Precision",
       y="Number of Runs")

median(result_df$F1)
sd(result_df$F1)

median(result_df$recall)
sd(result_df$recall)

mean(result_df$precision)
sd(result_df$precision)

###Variance
#result_df%>%
#  select(variance, F1)%>%
#  ggplot(aes(x=F1, y=factor(variance), fill=factor(variance)))+
#  geom_density_ridges()+
#  labs(title= "Distribution of F1 Score, by % of Variance Included",
#       x="F1 Score",
#       y="% of Variance Included")+
#  theme(legend.position = "none")

### Train Test Split
#result_df%>%
#  select(tt_split, F1)%>%
#  ggplot(aes(x=F1, y=factor(tt_split), fill=factor(tt_split)))+
#  geom_density_ridges()+
#  labs(title="Distribution of F1 score, by Train/Test Split %",
#       y = "Train/Test Split %",
#       x= "F1 Score")+
#  theme(legend.position="none")



#plot(x=seq(1,20,1), yvarianceplot(x=seq(1,20,1), y=result_df$recall, type = 'l', col = 'green',
#     xlab = "% of Variance", ylab = "Metric %", main = '20 Runs at 40% Variance')

#lines(x=seq(1,20,1),result_df$precision, type = 'l', col='red')
#lines(x=seq(1,20,1), result_df$accuracy, type = 'l', col = 'blue')
#legend("bottomright",legend = c("Recall","Precision","Accuracy"),fill = c('green','red','blue'))

## track how it varies with same parameters and random splits
## track how it varies with different % for test and control
## track how it varies with different included variance


# Draw two different normal distributions in R

# Parameters for the first normal distribution
mean1 <- 0
sd1 <- 2

# Parameters for the second normal distribution
mean2 <- 10
sd2 <- 2

# Create a sequence of x values covering both distributions
x <- seq(min(mean1 - 4*sd1, mean2 - 4*sd2),
         max(mean1 + 4*sd1, mean2 + 4*sd2),
         length.out = 500)

# Compute the density values
y1 <- dnorm(x, mean = mean1, sd = sd1)
y2 <- dnorm(x, mean = mean2, sd = sd2)

# Plot the first distribution
plot(x, y1, type = "l", col = "blue", lwd = 2,
     ylab = "Probability Density", xlab = "Coordinate Along Eigenvector",
     main = "Example of Gaussian Discriminant Function")

# Add the second distribution
lines(x, y2, col = "red", lwd = 2, lty = 2)

# Add a legend
legend("bottomright",
       legend = c("Distribution for Class A",
                  "Distribution for Class B"),
       col = c("blue", "red"),
       lty = c(1, 2),
       lwd = 2)


