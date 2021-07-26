# UEVBS_net_gen
R code for creation of custom networks in randomly genarated spatial data (UEVBS project).

The purpose of the project is to create an accurate simulation of the network and extract performance metrics form the results.

The R files all have numbers in their title signifying the order they should be run in.

"analysis_5.r" and new file called "Analytical outage_7.r" have been added which calculates the outage probability, Spectral Efficiency and Energy Efficiency given some assumptions, numerically and analytically, respectfully.

An early example of network formation in generated spatial data:
![large net example](https://github.com/OrestisN/UEVBS_net_gen/blob/main/early_example.PNG?raw=true)
A newer and clearer example of network formation in generated spatial data:
![samller net examples](https://github.com/OrestisN/UEVBS_net_gen/blob/main/new_example.png?raw=true)
P.S. The ranmdom red triangle in the graphs are supposed to be areas of bad coverage.

When "Analytical outage_7.r" is run, results may look like this:

![OUT results](https://github.com/OrestisN/UEVBS_net_gen/blob/main/OUT_plot.png?raw=true)
![SE results](https://github.com/OrestisN/UEVBS_net_gen/blob/main/SE_plot.png?raw=true)
![EE results](https://github.com/OrestisN/UEVBS_net_gen/blob/main/EE_plot.png?raw=true)
