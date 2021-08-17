# reserving-MDN-ResMDN

MDN Workspace README

This MDN workspace is divided into two main processes:
1. Data simulation using SynthETIC
2. Data processing, MDN fitting and analysis



SynthETIC: Data Simulation

The triangles were simulated using the SynthETIC Simulator. To simulate the same triangles used in the paper:

1. Open R Modules/Data Simulation/SynthETIC_Main.R
2. Follow instructions inside the module




MDN Modelling

The Modelling Section consists of several stages:

1. Loading the triangle, processing it into a data.table, and partitioning it into training, validation and testing sets
2. Fitting the MDN on the triangle
3. Analysing the MDN's fit
4. Simulating total outstanding claims

More detailed instructions are provided in R Modules/MDN Modelling/0.0 MDN_Main.R
