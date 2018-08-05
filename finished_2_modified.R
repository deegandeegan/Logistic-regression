# Load Owls 15 
input_csv <- read.csv(file.choose(), sep = ",")

# Use the structure function to understand your dataset
# str(input_csv)

# Rename the last column in the dataset into type
colnames(input_csv)[ncol(input_csv)]<-"type"

#Rename other columns in a format like  Predictor_1, Predictor_2 ...
colnames(input_csv)[-ncol(input_csv)]<-paste("Predictor_",c(1:(ncol(input_csv)-1)),sep = "")

# Turn the last column, type into a factor - 1, 2 and 3
input_csv$type <- factor(input_csv$type)
number_of_classes<-length(levels(input_csv$type))
number_of_features<-ncol(input_csv)-1

# Create an accuracy_vector, which is intialized with 0's 
accuracy_vector <- rep(0,10)

#Sigmoid function (logistic function)
Sigmoid <- function(real_number)
{
  sigmoid <- 1/(1+exp(-real_number))
  return(sigmoid)
}
# Randomly divide the file into 2/3 for training, 1/3 for testing
# Loop the formula in a for loop, which reproduces the 
for(seed in 1:10)
{
  
  set.seed(seed)
  
  index <- sample(1:2, nrow(input_csv), replace = TRUE, prob = c(0.67, 0.33))
  training <- input_csv[index == 1,]
  validation <- input_csv[index == 2,]
  # head(training)
  # head(validation)
  
  #Predictor variables
  pred_var <- as.matrix(training[1:(ncol(input_csv)-1)])
  pred_test <- as.matrix(validation[1:(ncol(input_csv)-1)]) 
  
  #Add ones to pred_var and pred_test
  pred_var <- cbind(rep(1,nrow(pred_var)),pred_var)
  pred_test <- cbind(rep(1,nrow(pred_test)),pred_test)
  
  #The for loop below carries out the following steps, but for any number of attributes >= 3
  # # Create y1, y2, y3 for owls dataset
  # training$y1 <- ifelse(as.integer(training$type) == 1, 1, 0)
  # training$y2 <- ifelse(as.integer(training$type) == 2, 1, 0)
  # training$y3 <- ifelse(as.integer(training$type) == 3, 1, 0)
  
validation$y_label<-as.integer(as.factor(validation$type))
  
class_number <-1
for (class_number in 1:number_of_classes)
  {
  y_variable_name<- paste("y",class_number,sep = "") #y1, y2 or y3
  #print(y_variable_name)
  assign(y_variable_name, ifelse(as.integer(training$type) == class_number, 1, 0) ) 
  #print(get(y_variable_name))
  training[ ,number_of_features+class_number+1]<-get(y_variable_name)
  #Creating response variables in matrix form for matrix multiplication later
  #the line below in the for loop is a gneralized version of the following
  # y1 <- as.matrix(training$y1)
  # y2 <- as.matrix(training$y2)
  # y3 <- as.matrix(training$y3)
  assign(y_variable_name,as.matrix(get(y_variable_name)))
  cost_variable<-paste("cost",class_number,sep="")
  assign(cost_variable, function(theta)
  {
    # m = size of data/or number of instances
    instances <- nrow(pred_var)
    # g is the sigmoid function applied to X.theta
    #%*% is matrix multiplacation
    g <- Sigmoid(pred_var %*% theta)
    J <- (1/instances) * sum(((get(paste("y",class_number,sep="")))-g)^2) #squared error cost function that we're trying to minimize
    return(J)
  })
  
  #Initializing all coefficients to be 0s . example below
  #initial_theta1 <- rep(0,ncol(pred_var))
  initial_theta_variable_name<-paste("initial_theta",class_number,sep="")
  assign(initial_theta_variable_name,rep(0,ncol(pred_var)))
  
  #running the cost function with the initial theta coefficients. example below
  #cost1(initial_theta1)
  get(cost_variable)(get(initial_theta_variable_name))
  
  # Derive theta using gradient descent using optim function , example below
  #  theta_optim1 <- optim(par=initial_theta1,fn=cost1)
  
  theta_optim_variable_name<-paste("theta_optim",class_number,sep="")
  assign(theta_optim_variable_name, optim(par=get(initial_theta_variable_name),
                                          fn=get(cost_variable)) )
  
  #Extracting the theta parameters from the optim output, which is a list of 5, example
  #  theta1 <- theta_optim1$par
  theta_variable_name<-paste("theta",class_number,sep = "")
  assign(theta_variable_name,get(theta_optim_variable_name)$par)
  
# Create prediction columns for each of the classes

  validation[ ,number_of_features+class_number+2]<-
    Sigmoid(pred_test  %*% as.matrix(get(theta_variable_name)))
  
  }
  
  # validation$y_label<-as.integer(as.factor(validation$type))
  # validation$y1<- Sigmoid(pred_test %*% as.matrix(theta1))
  # validation$y2<- Sigmoid(pred_test %*% as.matrix(theta2))
  # validation$y3<- Sigmoid(pred_test %*% as.matrix(theta3))
  validation$prediction<- 0
  for(i in 1:nrow(validation))
  {
    class_prob<-validation[i,(number_of_features+3):(number_of_features+3+number_of_classes)]
    validation$prediction[i]<-which(class_prob==max(class_prob))
  }
  
  accuracy<-sum(validation$y_label==validation$prediction)/nrow(validation)
  print(accuracy)
  # Use validation
  
  accuracy_vector[seed]<-accuracy
}

cat("Overall accuracy on 10 different splits of 1/3 and 2/3 = ", mean(accuracy_vector))


###############################
# Evaluation

install.packages("pROC")
library(pROC)
library(caret)
library(ggplot2)
confusionMatrix(validation$prediction, validation$y_label)

plot1 <- ggplot(data = input_csv, mapping = aes(x = Predictor_2, y = Predictor_4, color = type))+ 
  labs(x = "Wing-Length") + labs(y = "Wing-Width") +
  geom_point() 
plot1
plot2 <- ggplot(data = input_csv, mapping = aes(x = Predictor_3, y = Predictor_4, color = type))+ 
  labs(x = "Body-Width") + labs(y = "Wing-Width") +
  geom_point() 
plot2
