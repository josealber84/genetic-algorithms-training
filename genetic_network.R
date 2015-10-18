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
                   min.neurons.hidden.layer.2 = 0,
                   min.seed = 0,
                   max.seed = 99999){
    
    list("min.values" = c(min.number.of.hidden.layers,
                          min.neurons.hidden.layer.1,
                          min.neurons.hidden.layer.2,
                          min.seed),
         "max.values" = c(max.number.of.hidden.layers,
                          max.neurons.hidden.layer.1,
                          max.neurons.hidden.layer.2,
                          max.seed))
    
}

Evaluation <- function(chromosome, training.set, cv.set, formula){
    
    set.seed(chromosome[4])
    
    brain <- neuralnet(formula = formula,
                       data = training.set,
                       hidden = chromosome[2:3][1:chromosome[1]],
                       lifesign = "minimal")
    
    prediction <- 
        neuralnet::compute(brain, cv.set[, 1:(ncol(cv.set) - 1)])
    
    error <- (cv.set[, ncol(cv.set)] - prediction)^2/length(prediction)
    
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