# Change Point Support Vector Machine with L1 Regularization

This is my final project for my MSc. Mathematics at Washington State University. In a nutshell, a change point is a specific value of a covariate in some dataset in which we recognize that data points having value below this change point would have a separate SVM than data points having values larger than this change point. We regularize in order to accommodate the high-dimensional data setting.

Prequisites
-----------

- R

- RStudio

- RMosek

- We use an abundance of packages, all of which can be found at the top of each script of code.


Description of Items
--------

- `Report.pdf`
  - Final report that includes in-depth description of all processes throughout the project.
- `Presentation.pptx`
  - Powerpoint presentation used in final project defense.
- `SVM_single.R`
  - Uses RMosek to fit our L1 regularized SVM
- `dat.csv`,`feature.csv`, `response.csv`, `changepoint.csv`, `preprocess_data.R`,`application.R`
  - All used for the application of our project, which was finding a changepoint based on median income in crime data.
- `bic_v2.R`,`cls.R`,`znorm.R`
  - Information criterion function, classification error computing function, zero norm function (details of this are rather technical).
- `svmcp_v2.R`
  - The bread and butter of the project - this is the script that puts all the peices together to run our algorithm and fit the two SVMs according to the changepoint (if a changepoint is found).
- `sim.R`
  - Simulation code that compares our proposed model to a nonlinear SVM as well as a single linear SVM over the whole dataset.
- `inittau.R`,`roc.R`
  - These are a couple of illustrations to show the robustness of our algorithm as well as its performance compared to the other models mentioned above.
    
    
Contributors
--------------

Ryan Lattanzi
