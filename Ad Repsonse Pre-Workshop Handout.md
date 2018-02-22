---
output:
  pdf_document: default
  html_document: default
---
# Is My Advertising Working?: Pre-Workshop Handout
#### Elea McDonnell Feit
#### 21 February 2018

Welcome to the workshop on “Is My Advertising Working?”  During this session, you will gain experience with four different approaches to determining advertising response: 

– Attribution rules such as last-click and first-click  
– Holdout experiments  
– Marketing mix modeling and other time series methods  
– Algorithmic attribution and other model-based approaches  

To make these approaches concrete, we will apply all four methods to the same data set. By working through this example, you will develop a better understanding of how each method works, as well as the potential pitfalls of each method. We also illustrate the data manipulation that is required to prepare typical raw advertising data for analysis. In four hourse, you won't become an expert in any one of these methods, but you will have a solid foundation for learning more. 

The examples will be worked in the R statistical language, which is an open-source statistical programming language that has good tools for data manipulation and statistical analysis. We will go from raw advertising data all the way to presentable findings. 

#### What if I don’t know R? 
Don’t worry!  You don’t need to know R to learn a lot from the workshop. We’ll provide all the analysis in the workshop slides. You can ignore the R syntax and focus on the data that goes into the analysis, the output of the analysis, and how we interpret it. No need to bring a laptop, but you can, if you want. 

#### What if I know R or am learning R? 
You can use this workshop to develop your R skills, so come to class with a laptop with R and RStudio installed (installation instructions below.) We also provide a code file, so it will be easy to keep up, even if you are unfamiliar with some of the R syntax. We will also have TA’s available to help you, if you get stuck. 

#### What if I’m an experienced programmer?  
All tutorial files are available on github at https://github.com/eleafeit/ad_response_tutorial including R code, data files and the source code for the LaTeX slides. Pull requests are welcome, as are adaptations into other tools like SQL, Python or Matlab. I’m particularly interested in having someone translate my base R data manipulations into dplyr/Tidyverse. 

## R Installation Instructions

#### 1. Download & install the R software
The base R software is maintained on the Comprehensive R Network Archive (cran.r-project.org).  Base R is maintained by collective of voluntary contributors, so the website is not fancy and can sometimes be confusing. Please follow the links below. 

**Windows Users:** Go to http://cran.r-project.org/bin/windows/base/.  Click on ‘Download R 3.4.3’ link in the top-left-hand-corner of the screen. Download and run the Application file.

**Mac Users:** Visit the R for Macs Download website: http://cran.r-project.org/bin/macosx/.  Click on the link which corresponds to your operating system (i.e., Snow Leopard, Mavericks) and run the application file. 

In the installation process, use all default options. Once installation is complete, R should be listed as a program on your computer and you can run the base R package.  

#### 2. Download and install RStudio

R Studio is an add-on interface for R that provides a lot of additional handy features.  While it is possible to follow the workshop using base R, the workshop instructors will demonstrate in R Studio. So, if you want to follow along exactly, it is a good idea to download R Studio. 

You can download the free version of R Studio at https://www.rstudio.com/products/rstudio/download3/. The installation is straightforward. Note you must have base R installed before installing R Studio. When you launch R Studio, it will start R automatically. 

#### 3. Download the slides and code files for the tutorial

**Slides (pdf):**  https://github.com/eleafeit/ad_response_tutorial/blob/master/Ad%20Response%20Tutorial%20Slides.pdf  

**R Code:**  https://github.com/eleafeit/ad_response_tutorial/blob/master/R%20code/AdResponseTutorial.R  
You can save this file to your local machine and then open in RStudio using the command File > Open File. Don’t worry if you don’t know what to do next. We’ll start the workshop from this point. 

**Data (only needed for those using tools other than R): **
The links to the data files are included in the R code file and we’ll download the data directly into R. If you are using another tool like SAS or Python, you can download the data files directly using these links:  
**customer table:** https://github.com/eleafeit/ad_response_tutorial/blob/master/R%20code/customer.csv   
**impressions table:** https://github.com/eleafeit/ad_response_tutorial/blob/master/R%20code/impressions.csv  
**transactions table:** https://github.com/eleafeit/ad_response_tutorial/blob/master/R%20code/transactions.csv  
