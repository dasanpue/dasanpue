## pennbook
### Description
*pennbook* is a website that emulates a social network similar to facebook. It allows users to perform various actions, such as posting updates, sending and accepting friend requests, creating a personal feed, navigating friends' feeds, commenting on other people's posts and comments, sending private messages, and participating in group chats. Additionally, the platform generates a unique news feed for each user by running a ranking algorithm over an article database based on the user's interests. 

The program is developed using Node.js with the Express.js framework and utilizes AWS's DynamoDB for storing user data, posts, and chats. It also incorporates a server-side Java application that periodically runs a PageRank algorithm using Livy to interface with an AWS EMR cluster running Spark.
### Context
This tool was developed as a final project for the Scalable and Cloud Computing course in the University of Pennsylvania. It was developed over the course of 3 weeks with 3 other students: Awad I., Aditi C. and M. Abdullah K.. The project recieved a final score of A.
### Repository
The root folder of the repository contains a PDF with the final report, along with a folder named *G31* with the code for the project.
### [Back to start](../)
