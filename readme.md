### Reproducibility of the Research

The **script** folder contains all the necessary code to replicate the results of this study. The **result** folder stores the intermediate outputs from the code execution, while the **figure** folder contains the figures used in the manuscript. 

The model runs for the benchmark, three case studies and noise sensitivity analysis can be replicated in the following scripts:  
- **script/benchmark.r**
- **script/case1.r**  
- **script/case2.r**  
- **script/case3.r**
- **script/sensitivity.r**

To process the output case results for models like GCMC, use the script **script/case_results_process.r**. The replication of Figures 4-6 can be done through **script/case_figures.r**. For the sensitivity analysis, refer to **script/sensitivity.r** to replicate Figure 7.

We implemented the GCMC model using efficient computational methods provided by C++ and exposed the functionality through the **gcmc** function in the [**spEDM**](https://github.com/stscl/spEDM) package, which enables R-level invocation.