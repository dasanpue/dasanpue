## trend predictor
### Description
This project is designed to predict emerging topics in various academic fields. It employs a comprehensive big data pipeline, incorporating a range of tools to forecast concepts that will gain prominence in a given field.

To achieve this, the tool starts with an extensive 800GB dataset comprising academic papers. This dataset undergoes processing to extract a cleaned, parsed, and categorized set of 2,000,000 papers, each containing essential information such as an abstract, references, author list, publication date, and field of study. Subsequently, for a selected field of study, the application executes two distinct PageRank algorithms on this dataset. 

Firstly, it ranks authors based on their popularity using the author lists of each paper, and secondly, it ranks individual papers by popularity using their references. By combining both these measures and considering other relevant indicators, it organizes all publications by their popularity.

The popularity is calculated within a defined time window, for instance, a 5-year timeframe. This window is then moved across different years to generate a series of popularity lists. For each of these lists, the abstract of every paper is parsed, and keywords are extracted. These keywords are assigned the popularity of their respective papers and aggregated to identify the popularity of specific terms within that time frame.

By comparing popularity across multiple time frames, the evolution of terms appearing in more than one window is normalized over 'n' timeframes. This normalized data serves as the foundation for fitting a regression model to explore various potential trends. Once this regression model is constructed, it is applied to all the terms in the latest time frame to predict their future popularity, ultimately yielding a result that highlights emerging topics of interest in a given field.

Given the substantial data processing requirements, the tool uses distributed systems for many of its operations. The databases are stored in an AWS S3 bucket using the Boto3 Python library, while operations are executed on an AWS EMR cluster running Spark, utilizing SQL and built-in SparkDataFrame functions. Smaller dataset operations are performed using Pandas DataFrames, and the prediction model is implemented using SciKit-Learn.
### Context
This tool was developed as a final project for the Big Data Analytics course in the University of Pennsylvania. It was developed over the course of 3 weeks by me and Joel W.. The project recieved a final score of A+.
### Repository
The root folder of the repository contains an ipynb file that holds all the code for the project along with a walkthrough of all it's functionality. Along with this file there is also a *data* folder containing the necessary pre-processed data to run the notebook.
### [Back to start](../)
