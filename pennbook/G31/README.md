## NETS 2120 Final Project Group G31

### Group Members

* M. Abdullah Khalid (mkhalid)
* Aditi Chandra (adichan)
* Awad Irfan (awadirf)
* Daniel Santamarina Puertas (dasanpue)



### Implemented Features 

All required features in the writeup have been implemented.
* User Registration
* Account Changes
* Walls
* Home Page
* Commenting
* Search Users
* Friends and Visualizer
* Chat (groupchats, invites and persistence)
* News Feed (recommendation algorithm) with likes feature
* News Search



### Extra Credit Tasks

(1) PennBook allows users to reply to comments on posts, reply to the replies, and so on, in a nested fashion.

(2) Chats content is persistent for group chats, in addition to only direct messages as required for the project.

The project report expands more on these extra credit features

### Source Files

- app.js
	
- package.json

- package-lock.json
	
- pom.xml

- models/database.js

- routes/routes.js

- views/chat.ejs

- views/friendvisualizer.ejs

- views/home.ejs

- views/login.ejs

- views/main.ejs

- views/newsfeed_search.ejs

- views/newsfeed.ejs

- views/settings.ejs

- views/signup.ejs

- views/wall.ejs

- news_algo/src/main/java/edu/upenn/cis/nets2120/hw3/livy/ComputeRanksLivy.java

- news_algo/src/main/java/edu/upenn/cis/nets2120/hw3/livy/MyPair.java

- news_algo/src/main/java/edu/upenn/cis/nets2120/hw3/livy/SocialRankJob.java

- news_algo/src/main/java/edu/upenn/cis/nets2120/storage/SparkConnector.java

- target/nets212-hw3-0.0.1-SNAPSHOT.jar


### Declaration that code submitted was written by us

We (Awad Irfan, Daniel Santamarina Puertas, Aditi Chandra, M. Abdullah Khalid), declare that all code submitted within this project was written by us (excluding code given to us from previous HW files)


### Instructions

- Establish DynamoDB tables on AWS and store AWS credentials in /.aws for connection

	- **users**: username (String, primary key)
	
	- **chat_groups**: username (String, primary key), chat_id (String, sort key)
	
	    - chat_id-index (Index on chat_id)
	
	- **invites**: username (String, primary key), invited_chat_id (String, sort key)
	
	- **messages**: chat_id (String, primary key), create_time (String, sort key)
	
	- **comments**: post_id (String, primary key), number (Number, sort key)
	
	- **friends**: username (String, primary key), timestamp (Number, sort key)
	
	- **news_articles**: link (String, primary key)
	
	- **news_likes**: username (String, primary key), link (String, sort key)
	
	- **posts**: user_wall (String, primary key), create_time (String, sort key)
	
	- **search_index**: keyword (String, primary key)
	
	- **spark_user_article**: link (String, primary key), username (String, sort key)
	
	    - username-index (Index on username)
	
	- **user_prefixes**: prefix (String, primary key)
	
- Run `npm install` on command line (in the root directory of the project)

- Start a new cluster in AWS EMR. Enter the cluster public IP address inside the file: news_algo/src/main/java/edu/upenn/cis/nets212/hw3/livy/ComputeRanksLivy.java

- Run `mvn compile` on command line inside the news_algo folder

- Run `mvn package` on command line inside the news_algo folder

- To run the recommendation algorithm, run `mvn exec:java@livy` on command line inside the news_algo folder. This will start populating the spark_user_article table. Alternatively, go to the next step and wait one hour for the job to automatically run.

- Finally `node app.js` on command line (in the root directory of the project) to run the web app!