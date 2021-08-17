#SynthETIC Main
#The main module for generating loss triangles

#To generate any of the 4 environments:
# 1. Source the GenTriangle.R module
# 2. Go to the section below corresponding to the environment to simulate
# 3. Set the seed, and run the bracket below
# 4. Triangle is generated and stored in its default directory Datasets/Environment X/Table Y.csv

library(SynthETIC)
library(data.table)


#Source the triangle generating function
source('R Modules/Data Simulation/GenTriangle.R', echo=TRUE)
source('R Modules/Data Simulation/plotClaims.R', echo=TRUE)




##############
#Environment 1
##############
seed = 1 #set the seed, then run the bracket below
runoff = 1 #runoff aggregates claims beyond the last development period

{
source('R Modules/Data Simulation/Environment1.R', echo=TRUE)
Environment = 1
triangle_directory = paste0("Datasets/Environment ",Environment)
dir.create(file.path(triangle_directory), showWarnings = FALSE)
Triangle = GenTriangle(seed = seed, runoff = runoff)

}

plotClaims(Triangle, AQs = c(1,10,20,30,40), Environment = Environment)
fwrite(Triangle, paste0(triangle_directory, '/Table ', seed, '.csv'))



##############
#Environment 2
##############
seed = 1 #set the seed, then run the bracket below
runoff = 0 #runoff aggregates claims beyond the last development period
{
Environment = 2
short = 1
long = 0
source('R Modules/Data Simulation/Environment2.R', echo=TRUE)
triangle_directory = paste0("Datasets/Environment ",Environment)
dir.create(file.path(triangle_directory), showWarnings = FALSE)
ShortTriangle = GenTriangle(seed = seed, runoff = runoff)



short = 0
long = 1
source('R Modules/Data Simulation/Environment2.R', echo=TRUE)
LongTriangle = GenTriangle(seed = seed, runoff = runoff)

Triangle = ShortTriangle + LongTriangle
Triangle$AQ = Triangle$AQ/2

}
#plotClaims(Triangle, AQs = c(1,10,20,30,40), Environment = Environment)
fwrite(Triangle, paste0(triangle_directory, '/Table ', seed, '.csv'))


##############
#Environment 3
##############



seed = 1 #set the seed, the run the bracket below
runoff = 0 #runoff aggregates claims beyond the last development period
{
Environment = 3

source('R Modules/Data Simulation/Environment3.R', echo=TRUE)
triangle_directory = paste0("Datasets/Environment ",Environment)
dir.create(file.path(triangle_directory), showWarnings = FALSE)
Triangle = GenTriangle(seed = seed, runoff = runoff)

}

#plotClaims(Triangle, AQs = c(1,10,20,30,40), Environment = Environment)
fwrite(Triangle, paste0(triangle_directory, '/Table ', seed, '.csv'))





##############
#Environment 4
##############




seed = 1 #set the seed, then run the bracket below
runoff = 0 #runoff aggregates claims beyond the last development period

{
Environment = 4

source('R Modules/Data Simulation/Environment4.R', echo=TRUE)
triangle_directory = paste0("Datasets/Environment ",Environment)
dir.create(file.path(triangle_directory), showWarnings = FALSE)
Triangle = GenTriangle(seed = seed, runoff = runoff)

}

plotClaims(Triangle, AQs = c(1,10,20,30,40), Environment = Environment)
fwrite(Triangle, paste0(triangle_directory, '/Table ', seed, '.csv'))





