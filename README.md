# reserving-MDN-ResMDN

MDN Workspace README

This workspace performed the modelling for the related paper:

"Stochastic loss reserving with mixture density networks"

Authors: Muhammed Taher Al-Mudafer, Benjamin Avanzi, Greg Taylor, Bernard Wong

This MDN workspace is divided into two main processes:
1. Data simulation using SynthETIC (code found in R Modules/Data Simulation)
2. Data processing, MDN fitting and analysis (code found in R Modules/MDN Modelling)

This workspace is tailored to working with the simulated triangles produced in the SynthETIC modules. 
However, the code provided in this workspace can be altered and transferred to any setting that suits the user's requirements.


SynthETIC: Data Simulation

The triangles were simulated using the SynthETIC Simulator. To simulate the same triangles used in the paper:

1. Open R Modules/Data Simulation/SynthETIC_Main.R
2. Follow instructions inside the module




MDN Modelling

The Modelling Section consists of several stages:

1. Loading and processing the triangle
2. Fitting the MDN on the triangle
3. Analysing the MDN's fit
4. Simulating total outstanding claims

The master script is located in R Modules/MDN Modelling/0.0 MDN_Main.R, which provides a detailed run-through of the entire process.

Once the triangle is simulated, open R Modules/MDN Modelling/0.0 MDN_Main.R to begin the modelling process



We hope you enjoy using this workspace. If you have any queries, please contact me at muhammed.almudafer97@gmail.com

Kind Regards

Muhammed



