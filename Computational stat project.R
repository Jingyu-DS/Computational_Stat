library(data.table)

# read in the data 
happiness <- fread("2017.csv")
country <- fread("country_profile_variables.csv")

# colnames(country)
country <- country[,-40] # remove the duplicate column
setnames(country, c("country"),c("Country"))

# Merge the two data table to get more info about each country
universe <- merge(happiness, country, by="Country")

# Have a look at the region of countries 
table(universe$Region)
fwrite(universe,"happiness_country_data.csv")

# Creat correlation plot 
# Exclude country name, rank, high and low values
cor_mat <- cor(happiness[,-c(1,2,4,5)])

# install.packages("corrplot")
library(corrplot)
corrplot(cor_mat, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Health life expectancy, GDP per capita, family and freedom indicate strong correlation with the Happiness Score

############################################
############# Permutation test##############
############################################

# We will perform the permutation test to see whether the difference among different region exists 
# America: CentralAmerica, NorthernAmerica, SouthAmerica
America <- universe[(universe$Region == "CentralAmerica" | 
                       universe$Region == "NorthernAmerica" | universe$Region == "SouthAmerica")]
mean(America$Happiness.Score)

# Europe: EasternEurope, NorthernEurope, SouthernEurope, WesternEurope
Europe <- universe[(universe$Region == "EasternEurope" | 
                       universe$Region == "NorthernEurope" | universe$Region == "SouthernEurope" |
                     universe$Region == "WesternEurope")]
mean(Europe$Happiness.Score)

# Asia: CentralAsia, EasternAsia, South-easternAsia, SouthernAsia, WesternAsia
Asia <- universe[(universe$Region == "CentralAsia" | 
                      universe$Region == "EasternAsia" | universe$Region == "South-easternAsia" |
                     universe$Region == "SouthernAsia" | universe$Region == "WesternAsia")]
mean(Asia$Happiness.Score)

# Africa: EasternAfrica, MiddleAfrica, NorthernAfrica, SouthernAfrica, WesternAfrica
Africa <- universe[(universe$Region == "EasternAfrica" | 
                    universe$Region == "MiddleAfrica" | universe$Region == "NorthernAfrica" |
                    universe$Region == "SouthernAfrica" | universe$Region == "WesternAfrica")]
mean(Africa$Happiness.Score)

# Permutation test on the average happiness score between Asia and Europe 
data <- c(Europe$Happiness.Score, Asia$Happiness.Score)
# length(Europe$Happiness.Score)
# length(Asia$Happiness.Score)
perm_Euro<-matrix(NA,1000,35) 
perm_Asia <-matrix(NA,1000,37) 
for (i in 1:1000){
  perm_Euro[i,]<-sample(data,35)
  perm_Asia[i,]<-data[!data %in% perm_Euro[i,]]
}
difMean<-rowMeans(perm_Euro)-rowMeans(perm_Asia) #get the difference of means for each row
## visualize the differences in mean
hist(difMean, xlim=c(-1,1), main="Test on the average happiness score between Asia and Europe")
abline(v=mean(Europe$Happiness.Score)-mean(Asia$Happiness.Score),col="red",lwd=3)

# Based on the plot, average happiness score of Europe is different from that of Asia. 


# The same thing can be done on Asia and Africa 
# Permutation test on the average happiness score between Asia and Africa
data_2 <- c(Asia$Happiness.Score, Africa$Happiness.Score)
# length(Asia$Happiness.Score)
# length(Africa$Happiness.Score)
perm_Asia<-matrix(NA,1000,37) 
perm_Africa <-matrix(NA,1000,40) 
for (i in 1:1000){
  perm_Asia[i,]<-sample(data_2,37)
  perm_Africa[i,]<-data_2[!data_2 %in% perm_Asia[i,]]
}
difMean_2<-rowMeans(perm_Asia)-rowMeans(perm_Africa) #get the difference of means for each row
## visualize the differences in mean
hist(difMean_2, xlim = c(-1.5,1.5), main="Test on the average happiness score between Asia and Africa")
abline(v=mean(Asia$Happiness.Score)-mean(Africa$Happiness.Score),col="red",lwd=3)

# Based on the plot, average happiness score of Asia is different from that of Africa 

############################################
###### Bootstrap to see the slope of #######
############ GDP per capita ################
####### Happiness score as response#########
############################################

# fit model
K <- 10000
n <- length(happiness$Happiness.Score)
lm_fit <- lm(Happiness.Score~Economy..GDP.per.Capita., data = happiness)
beta_hat<-lm_fit$coefficients
e_hat<-lm_fit$residuals

# Nonparametric bootstrap
bhat_boot<-matrix(nrow=K,ncol=2)

for(i in 1:K){
  Xb<-sample(happiness$Economy..GDP.per.Capita.,n,replace=TRUE)
  eb<-sample(e_hat,n,replace=TRUE)
  Yb<-beta_hat[1]+beta_hat[2]*Xb+eb
  bhat_boot[i,] = lm(Yb~Xb)$coefficients
}
hist(bhat_boot[,2], xlim=c(1, 3))
abline(v=beta_hat[2], col="red")
quantile(bhat_boot[,2], probs = c(0.025, 0.975))

############################################
## Cross Validation to pich up the model ###
############################################

# Based on the correlation plot 
# In order to predict Happiness score, we choose to pick the predictors that are highly correlated to happiness score
# The four predictors are: Health..Life.Expectancy.; Economy..GDP.per.Capita.; Family; Freedom 

# Because among those four predictors, the correlation value of Family is the smallest
# Then, we can use the Cross Validation to determine which model we will pick 

# model1 <- glm(Happiness.Score ~ Health..Life.Expectancy.+Economy..GDP.per.Capita.+Family+Freedom, data = happiness)
# model2 <- glm(Happiness.Score ~ Health..Life.Expectancy.+Economy..GDP.per.Capita.+Family, data = happiness)

# Now Leave-one-out CV
# Initialize
N <- nrow(happiness)
predictors <- happiness[,c("Health..Life.Expectancy.", "Economy..GDP.per.Capita.", "Family", "Freedom")]
E_m1 <- numeric(N)
E_m2 <- numeric(N)
# CV loop
for(i in 1:N){
  # Fit the models without the i^th data point
  m1_i <- lm(Happiness.Score ~ Health..Life.Expectancy.+Economy..GDP.per.Capita.+Family+Freedom,
              data=happiness[-i,])
  m2_i <- lm(Happiness.Score ~ Health..Life.Expectancy.+Economy..GDP.per.Capita.+Family,
              data=happiness[-i,])
  # Predict CHD for the unobserved data point i
  Y1 <- predict(m1_i,newdata = predictors[i,])
  Y2 <- predict(m2_i,newdata = predictors[i,])
  # Compute the error
  E_m1[i] <- (happiness$Happiness.Score[i] - Y1)^2
  E_m2[i] <- (happiness$Happiness.Score[i] - Y2)^2
}
# Compare the average error of each model
c(mean(E_m1),mean(E_m2))

# Based on the result, the first model is better than the second one. 


############################################
########### Bayesian Statistics ############
############################################

hist(happiness$Happiness.Score)
### Follows an approximately normal distribution
### We decide to use the unknown mean and unknown variance to explore the world happiness score
### We will use fairly weak priors.
### Easier to interpret priors in terms of means and variances
### Inversegamma prior for variance:
n <- length(happiness$Happiness.Score)
X <- happiness$Happiness.Score
prior_var_mean<-1
prior_var_var<-10
alpha0<- 2 + prior_var_mean^2/prior_var_var
beta0<- prior_var_mean*(alpha0-1)
# Normal Prior for mean, given variance
mu0<-6
n0<-1
# Posterior
n_post<-n+n0
mu0_post<-(n/n_post)*mean(X) + (n0/n_post)*mu0
alpha_post<-alpha0+(1/2)*n
beta_post<-beta0 + (1/2)*sum((X-mean(X))^2)+(n*n0/(2*n_post))*(mean(X)-mu0)^2
# We have the posterior paramters, but to do the MC draws, we have to sample the 
# variance before the mean.

############################################
############### Monte Carlo ################
############################################

MC_draws<-100000
sig2_post_draws<-1/rgamma(MC_draws,shape=alpha_post,scale=1/beta_post) # alternative way to sample from invGamma
hist(sig2_post_draws)
abline(v=var(X),col="red",lwd=3)
abline(v=quantile(sig2_post_draws,probs=c(alpha/2,1-alpha/2)), col="red",lty=3)

mu_post_draws<-rnorm(MC_draws,mean=mu0_post,sd=sqrt(sig2_post_draws/n_post))
hist(mu_post_draws)
abline(v=mean(X),col="red",lwd=3)
abline(v=quantile(mu_post_draws,probs=c(alpha/2,1-alpha/2)), col="red",lty=3)

# CI
alpha<-0.05
# mu
quantile(mu_post_draws,probs=c(alpha/2,1-alpha/2))
# sigma^2
quantile(sig2_post_draws,probs=c(alpha/2,1-alpha/2))

             
             
             