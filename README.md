# IDHprediction

This is an easy-to-use intuitive R-shiny app that facilitates the prediction of the IDH mutation status. Please note that our approach achieved on independent test data an average accuracy of 76.1%, a sensitivity of 82.6%, and a specificity of 72.7%. Please keep this in mind when interpreting obtained results. Further details can be found in “Bumes E., Fellner C., Fellner F.A., Fleischanderl K., Häckl M., Lenz S., Linker R., Mirus T., Oefner5 P.J., Paar C., Proescholdt M., Riemenschneider M.J., Rosengarth K., Weis S., Wendl C., Wimmer S., Hau P., Gronwald W., Hutterer M. Validation Study for non-invasive Prediction of IDH Mutation Status in Patients with Glioma using In Vivo 1H-Magnetic Resonance Spectroscopy and Machine Learning. Cancers, 2022 (DOI: 10.3390/cancers14112762)

Note that the application is for research purposes only.

## Installation

1) First you need to install R (free). 
    - For windows see:  https://cran.r-project.org/bin/windows/base/
 	- For Linux  see:  https://cran.r-project.org/bin/linux/ or use your distribution’s package manager
    - For MacOS see: https://cran.r-project.org/bin/macosx/  

It is not required but the additional installation of R-Studio helps to work with R  
See: https://www.rstudio.com/products/rstudio/  
We recommend the free Desktop version.

2) The IDHpredicion.zip file for the IDHprediction package can be saved locally in the any folder. We recommend the Download or Document folder. For windows this could be for example the following path:  
“C:/Users/Username/Downloads/”, for Linux it might be “/home/Username/Downloads/”. Unzip the file here (you should see a folder IDH prediction). Now start R  and start the installation within R with: 
```
install.packages("C:/Users/Username>/Downloads/IDHprediction", method = "source", repos = NULL)
```
If you encounter any problems, you might want to use devtools for installation:
```
install.packages(“devtools”)
devtools::install("C:/Users/Username/Downloads")
```

3) Afterwards the IDHprediction package can be loaded with library(IDHprediction).
4) To start the App IDHprediction::runWebApp()

Note, for the next start of the app. You only have to start R and execute points 3 and 4.


## File format of MRS spectrum used for prediction

The input file must be a .csv file containing at least rows. The first row is the header, 
containing the indices of the data points, the following rows are the
actual measurements (one spectrum per row).  
The first column must contain the sample name. Values must be separated by ";", decimal points are represented by dots.

*Example:*  
"Sample";"1", "2", "3",...  
"S1";0;100;95.5;...

Two example input files are provided in the “Example” subfolder of the IDHprediction folder.  

You can easily obtain such a .csv file by processing your raw spectra data for example with the free program jMRUI http://www.jmrui.eu/ (free for noncomercial users). In jMRUI choose “File->save_as->Mrui->Files of type->Text format .txt
Note, your .csv file may contain data from one or several spectra.

## Getting started

Once installed, the included shiny-application can be started from R via
```
library(IDHprediction)
IDHprediction::runWebApp()
```

The application should open on a browser.
You can now enter the bandwidth and magnetic field strength of your NMR measurements
and select a file to classify by clicking on `browse`.
