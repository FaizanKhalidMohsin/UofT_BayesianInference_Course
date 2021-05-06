


This week we look at why the MCMC can easily solve problems which use to be very difficult.

The main slides are in the file: lectwk4Y13.pdf

The code is in the following files:

1) There is an example which looks at the R code to perform the simple MCMC for the heights data.  These files are: SimpMCMC.r and SimpMCMCJpeg.r.  The first file creates "pdf" files while the second creates "jpeg" files.


2) There is an example of linear regression in WinBugs/OpenBugs/Jags.  The files associated with this model are the following:


a) The file "brnbdyNC.txt" can be cut-and-pasted into Winbugs or opened in OpenBugs and then used to run the program interactively.  This will require a bit of "mousing around".  

b) The files "brnbdy.csv" contains the data.  It is a "comma separated variable" file.  That is, the variables are separated by commas. This is a common way to store data.  The spreadsheet program Excel.  (Although, if you try and save a worksheet like this, it does complain a bit.)  This file can be read by R with the "read.table" function. 

c) brnbdyR2Openbugs.txt:  This file can be cut and pasted right into R and it will run Openbugs through R.  NOTE: after each plot, the plot the program will stop.  To get next plot, click on the plot in R.  

d) The basic WinBugs/OpenBugs model file is "NonCenterMod.txt".  When using these two program via a script or through R, this file is read and used for the model.  Note: this model does not work in Jags.  See the Jags file for the modification of this model to meet the Jags syntax requirements.

e) brnbdyR2Jags.txt: This program can be cut-and-pasted into R and it will run the model using JAGS.  You need to change the working directory.  Also, there is a different model file that is used.  It is actually defined in the file.  There are some different plots that are produced in this program.

f) NonCenterALLSCRIPTFiles.txt: This file contains several little files to run WinBugs in Script mode. To run the scripts, you actually have to cut up the big file back into all those little files.  Also, you will need to change the directory reference into what/where you put those files.  NOTE: YOU WILL PROBABLY NOT WANT TO RUN IN SCRIPT MODE... RUN THROUGH R INSTEAD.

h) lect4CompNote.pdf: A description of some of the features in the program files.

