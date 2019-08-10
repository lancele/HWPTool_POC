#splits datasets up randomly into training, validation, and test datasets

#source("C:/Users/marc.morandi/Documents/Healthy_Watersheds/California/Statistics/GBM/splitsets.r")

splitsets <- function(input, prop=c(.50,.25,.25))  {  #prop - three proportions equaling 1: first number is training dataset, second is validation, third is test

if (prop[1]+prop[2]+prop[3] != 1 | length(prop) != 3)  {
	stop("Check proportions")
}

res <- vector(mode="list", length=3)

n.train <- round(nrow(input) * prop[1])
n.val <- round(nrow(input) * prop[2])
n.test <- (nrow(input)-n.train-n.val)
print(nrow(input))
print(c(n.train, n.val, n.test))
print(n.train + n.val + n.test)

choice <- input
train <- input[input$CATCHID %in% sample(input$CATCHID,n.train),]
choice <- input[!(input$CATCHID %in% train$CATCHID),]
val <- input[input$CATCHID %in% sample(choice$CATCHID,n.val),]
test <- input[!(input$CATCHID %in% train$CATCHID | input$CATCHID %in% val$CATCHID),]

res[[1]] <- train
res[[2]] <- val
res[[3]] <- test

res
}