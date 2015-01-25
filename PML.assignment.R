library(caret)
#
training <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testing <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))
#
#
# these are the only variables that actually have useful data (ie not just NA) in the testing set, so it is not worth including any others:
training.cut <- subset(training, select=c(roll_belt, pitch_belt, yaw_belt, as.numeric(total_accel_belt), gyros_belt_x, gyros_belt_y, gyros_belt_z, as.numeric(accel_belt_x), as.numeric(accel_belt_y), as.numeric(accel_belt_z), as.numeric(magnet_belt_x), as.numeric(magnet_belt_y), as.numeric(magnet_belt_z), roll_arm, pitch_arm, yaw_arm, as.numeric(total_accel_arm), gyros_arm_x, gyros_arm_y, gyros_arm_z, as.numeric(accel_arm_x), as.numeric(accel_arm_y), as.numeric(accel_arm_z), as.numeric(magnet_arm_x), as.numeric(magnet_arm_y), as.numeric(magnet_arm_z), roll_dumbbell, pitch_dumbbell, yaw_dumbbell, as.numeric(total_accel_dumbbell), gyros_dumbbell_x, gyros_dumbbell_y, gyros_dumbbell_z, as.numeric(accel_dumbbell_x), as.numeric(accel_dumbbell_y), as.numeric(accel_dumbbell_z), as.numeric(magnet_dumbbell_x), as.numeric(magnet_dumbbell_y), magnet_dumbbell_z, roll_forearm, pitch_forearm, yaw_forearm, as.numeric(total_accel_forearm), gyros_forearm_x, gyros_forearm_y, gyros_forearm_z, as.numeric(accel_forearm_x), as.numeric(accel_forearm_y), as.numeric(accel_forearm_z), as.numeric(magnet_forearm_x), magnet_forearm_y, magnet_forearm_z, classe))
#
#
#
# create a training set and a testing set within the training set
inTrain <- createDataPartition(y=training.cut$classe, p=0.75, list=FALSE)
Train <- training.cut[inTrain,]
Test <- training.cut[-inTrain,]
dim(Train)
#
set.seed(1981)
#
#
# cross validation with 3-fold repeats
ctrl <- trainControl(method = "repeatedcv", repeats = 3)
#
#
# run model using knn method to classify 
model <- train(classe ~ ., data = Train, method = "knn", tuneLength = 12, trControl = ctrl)
#
# try data on "test" set
prediction <- predict(model, newdata=Test)
confusionMatrix(prediction, Test$classe)
# Accuracy is 87%
#
#
# this sets all the variables to have the same structure as the training set
testing.cut <- subset(testing, select=c(roll_belt, pitch_belt, yaw_belt, as.numeric(total_accel_belt), gyros_belt_x, gyros_belt_y, gyros_belt_z, as.numeric(accel_belt_x), as.numeric(accel_belt_y), as.numeric(accel_belt_z), as.numeric(magnet_belt_x), as.numeric(magnet_belt_y), as.numeric(magnet_belt_z), roll_arm, pitch_arm, yaw_arm, as.numeric(total_accel_arm), gyros_arm_x, gyros_arm_y, gyros_arm_z, as.numeric(accel_arm_x), as.numeric(accel_arm_y), as.numeric(accel_arm_z), as.numeric(magnet_arm_x), as.numeric(magnet_arm_y), as.numeric(magnet_arm_z), roll_dumbbell, pitch_dumbbell, yaw_dumbbell, as.numeric(total_accel_dumbbell), gyros_dumbbell_x, gyros_dumbbell_y, gyros_dumbbell_z, as.numeric(accel_dumbbell_x), as.numeric(accel_dumbbell_y), as.numeric(accel_dumbbell_z), as.numeric(magnet_dumbbell_x), as.numeric(magnet_dumbbell_y), magnet_dumbbell_z, roll_forearm, pitch_forearm, yaw_forearm, as.numeric(total_accel_forearm), gyros_forearm_x, gyros_forearm_y, gyros_forearm_z, as.numeric(accel_forearm_x), as.numeric(accel_forearm_y), as.numeric(accel_forearm_z), as.numeric(magnet_forearm_x), magnet_forearm_y, magnet_forearm_z, problem_id))
#
#
# final predictions on the testing set
finalPred <- prediction <- predict(model, newdata=testing.cut)
#
#
#
#
# to make files for submission
pml_write_files = function(x){
+   n = length(x)
+   for(i in 1:n){
+     filename = paste0("problem_id_",i,".txt")
+     write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
+   }
+ }
pml_write_files(finalPred)
#
#
#
#
