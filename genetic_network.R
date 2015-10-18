# Library to evolve neural networks using genetic algorithms
# 
# Chromosome definition:
#     
# {LFS$}
# 
# - L >> number of hidden layers in the neural network
# - F >> number of neurons in the first hidden layer
# - S >> number of neurons in the second hidden layer
# - $ >> seed to make each net reproducible
# 
# Maximal and minimal values:
#     
# - 0 <= L <= 2
# - 0 <= F < Inf
# - 0 <= S < Inf
# - 0 <= $ < Inf

library(genalg)
library(neuralnet)

Config <- function(max.number.of.hidden.layers = 2,
                   min.number.of.hidden.layers = 1,
                   max.neurons.hidden.layer.1 = 100,
                   min.neurons.hidden.layer.1 = 0,
                   max.neurons.hidden.layer.2 = 100,
                   min.neurons.hidden.layer.2 = 0){
    
    list("min.values" = c(min.number.of.layers,
                          min.neurons.hidden.layer.1,
                          min.neurons.hidden.layer.2),
         "max.values" = c(max.number.of.layers,
                          max.neurons.hidden.layer.1,
                          max.neurons.hidden.layer.2))
    
}

Evaluation <- function(chromosome, training.data, test.data, formula){
    
    set.seed(chromosome[4])
    
    brain <- neuralnet(formula = formula,
                       data = training.data,
                       hidden = chromosome[2:3][1:chromosome[1]],
                       lifesign = "minimal")
    
    prediction <- 
        neuralnet::compute(brain, test.data[, 1:(ncol(test.data) - 1)])
    
    error <- (test.data[, ncol(test.data)] - prediction)^2/length(prediction)
    
    error
    
}

Evolution <- function(config, eval.func = Evaluation, good.chromosomes = NULL){
    
    rbga(stringMin = config$min.values,
         stringMax = config$max.values,
         suggestions = good.chromosomes,
         popSize = 50,
         iters = 1000,
         elitism = 2,
         mutationChance = 0.5,
         showSettings = TRUE,
         verbose = TRUE,
         evalFunc = evaluation) -> model
    
    model
    
}

CreateNet <- function(chromosome, training.data, formula){
    
    set.seed(chromosome[4])
    
    brain <- neuralnet(formula = formula,
                       data = training.data,
                       hidden = chromosome[2:3][1:chromosome[1]],
                       lifesign = "minimal")
    
}