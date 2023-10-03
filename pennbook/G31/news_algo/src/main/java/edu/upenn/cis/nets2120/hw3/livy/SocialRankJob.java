package edu.upenn.cis.nets2120.hw3.livy;

import org.apache.livy.Job;
import org.apache.livy.JobContext;
import org.apache.spark.api.java.JavaPairRDD;
import org.apache.spark.api.java.JavaRDD;
import org.apache.spark.api.java.JavaSparkContext;
import org.apache.spark.sql.SparkSession;

import edu.upenn.cis.nets2120.config.Config;
import edu.upenn.cis.nets2120.storage.SparkConnector;
import scala.Tuple2;
import software.amazon.awssdk.services.dynamodb.model.DynamoDbException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.spark.api.java.JavaPairRDD;
import org.apache.spark.api.java.JavaRDD;
import org.apache.spark.api.java.JavaSparkContext;
import org.apache.spark.sql.SparkSession;

import edu.upenn.cis.nets2120.storage.SparkConnector;
import scala.Tuple2;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder;
import com.amazonaws.services.dynamodbv2.document.BatchWriteItemOutcome;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.services.dynamodbv2.document.TableWriteItems;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.ScanRequest;
import com.amazonaws.services.dynamodbv2.model.ScanResult;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class SocialRankJob implements Job<Integer> {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Connection to Apache Spark
	 */
	SparkSession spark;
	
	JavaSparkContext context;

	private boolean useBacklinks;
	

	/**
	 * Initialize the database connection and open the file
	 * 
	 * @throws IOException
	 * @throws InterruptedException 
	 * @throws DynamoDbException 
	 */
	public void initialize() throws IOException, InterruptedException {
		System.out.println("Connecting to Spark...");
		spark = SparkConnector.getSparkConnection();
		context = SparkConnector.getSparkContext();
		
		System.out.println("Connected!");
	}

	/**
	 * Main functionality in the program: read and process the social network
	 * 
	 * @throws IOException File read, network, and other errors
	 * @throws DynamoDbException DynamoDB is unhappy with something
	 * @throws InterruptedException User presses Ctrl-C
	 */
	public Integer run() throws IOException, InterruptedException {
		System.out.println("Running");
		
		//get today date
		DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
		Date date = new Date();
		String today_date = dateFormat.format(date);

		//Add edges between friends 
	    List<Tuple2<String, String>> friend_edge_list = new ArrayList<>();
	    AmazonDynamoDB dynamoDBClient = AmazonDynamoDBClientBuilder.standard().build();
	    ScanRequest scan_friends_table = new ScanRequest().withTableName("friends");
	    ScanResult result_friends = dynamoDBClient.scan(scan_friends_table);
	    for (Map<String, AttributeValue> item : result_friends.getItems()) {
		    friend_edge_list.add(new Tuple2<String, String>(item.get("username").getS(), 
															item.get("friend").getS()));
	    }
	    JavaRDD<Tuple2<String, String>> friend_edges_raw_rdd = context.parallelize(friend_edge_list);
	    JavaPairRDD<String, String> friend_edges_rdd = friend_edges_raw_rdd.mapToPair(
			x -> new Tuple2<String, String>(x._1, x._2));
	    JavaPairRDD<String, Double> friend_normalized_rdd = friend_edges_rdd.mapValues(x->1).reduceByKey(
			(a, b) -> a+b).mapValues(i -> (double)0.3/i);;
	    JavaPairRDD<String, Tuple2<String, Double>> friend_weight_rdd = friend_edges_rdd.join(friend_normalized_rdd);

	    //Scan table for newslikes: add edges b/w users, urls
	    List<Tuple2<String, String>> likes_array_list = new ArrayList<>();
	    ScanRequest scan_news_likes_table = new ScanRequest()
	        .withTableName("news_likes");
	    ScanResult likes_result = dynamoDBClient.scan(scan_news_likes_table);
	    for (Map<String, AttributeValue> item : likes_result.getItems()) {
	    	likes_array_list.add(new Tuple2<String, String>(item.get("username").getS(), item.get("link").getS()));
	    }
	    JavaRDD<Tuple2<String, String>> like_edges = context.parallelize(likes_array_list);
	    JavaPairRDD<String, String> like_edges_rdd = like_edges.mapToPair(x -> 
	      new Tuple2<String, String>(x._1, x._2));
	    JavaPairRDD<String, Double> like_normalized_rdd = like_edges_rdd.mapValues(x->1).reduceByKey(
			(x, y) -> x+y).mapValues(i -> (double)(0.4)/i);
	    JavaPairRDD<String, Tuple2<String, Double>> user_article_final_rdd = like_edges_rdd.join(like_normalized_rdd);	    
	    
		JavaPairRDD<String, String> reverse_like_edges_rdd= like_edges.mapToPair(x -> 
	    	new Tuple2<String, String>(x._2, x._1));
	    JavaPairRDD<String, Double> reverse_liked_prop_rdd = reverse_like_edges_rdd.mapValues(x->1).reduceByKey(
			(x, y) -> x+y).mapValues(i -> (double)(0.4)/i);
	    JavaPairRDD<String, Tuple2<String, Double>> article_user_final_rdd = reverse_like_edges_rdd.join(reverse_liked_prop_rdd);
	    
	    
	    //Add edges between users and interests
	    List<Tuple2<String, String>> user_interest_list = new ArrayList<>();
	    ScanRequest user_scan = new ScanRequest()
	        .withTableName("users");
	    ScanResult users_result = dynamoDBClient.scan(user_scan);
	    for (Map<String, AttributeValue> item : users_result.getItems()) {
	    	for (String s: item.get("interests").getSS()) {
	    		user_interest_list.add(new Tuple2<String, String>(item.get("username").getS(), s));
	        }
	    }
	    JavaPairRDD<String, String> user_interest_rdd = context.parallelizePairs(user_interest_list);
	    JavaPairRDD<String, Integer> number_of_interests_rdd = user_interest_rdd.mapValues(x->1).reduceByKey(
			(x, y) -> x+y);
	    JavaPairRDD<String, Double> number_of_interests_norm_rdd = number_of_interests_rdd.mapValues(i -> (double)(0.3)/i);
	    JavaPairRDD<String, Tuple2<String, Double>> user_category_final_rdd = user_interest_rdd.join(number_of_interests_norm_rdd);
	    
	    JavaPairRDD<String, String> reverse_user_rdd = user_interest_rdd.mapToPair(x ->
	    	new Tuple2<String, String>(x._2, x._1));
	    JavaPairRDD<String, Double> cat_user_norm_rdd = reverse_user_rdd.mapValues(x->1).reduceByKey(
			(x,y) -> x+y).mapValues(i -> (double)(0.3)/i);
	    JavaPairRDD<String, Tuple2<String, Double>> category_user_final_rdd = reverse_user_rdd.join(cat_user_norm_rdd);
	    
	    //Add edges between articles and categories
	    List<Tuple2<String, String>> article_cat_array_list = new ArrayList<>();
	    ScanRequest news_scan = new ScanRequest().withTableName("news_articles");
	    ScanResult news_result = dynamoDBClient.scan(news_scan);
	    for (Map<String, AttributeValue> item: news_result.getItems()) {
			if (today_date.compareTo(item.get("date").getS()) >= 0) {
				article_cat_array_list.add(new Tuple2<>(item.get("link").getS(), item.get("category").getS()));
			}
	    }
	    JavaPairRDD<String, String> article_cat_rdd = context.parallelizePairs(article_cat_array_list);
	    JavaPairRDD<String, Tuple2<String, Double>> article_cat_weight_rdd = article_cat_rdd.mapToPair(x ->
	    		new Tuple2<>(x._1, new Tuple2<>(x._2, (double)(1.0))));
	    
	    JavaPairRDD<String, String> cat_article_rdd = article_cat_rdd.mapToPair(x -> new Tuple2<>(x._2, x._1));
	    JavaPairRDD<String, Double> cat_article_norm_rdd = cat_article_rdd.mapValues(x->1).reduceByKey(
			(x,y)->x+y).mapValues(i -> (double)(1.0)/i);
	    JavaPairRDD<String, Tuple2<String, Double>> cat_article_final_rdd = cat_article_rdd.join(cat_article_norm_rdd);

		//concating all rdds
	    JavaPairRDD<String, Tuple2<String, Double>> all_edges = friend_weight_rdd.union(
	//		user_article_final_rdd).union(
//				article_user_final_rdd).union(
					user_category_final_rdd).union(
						category_user_final_rdd).union(
							article_cat_weight_rdd).union(
								cat_article_final_rdd);
	    
	    
	    JavaPairRDD<Tuple2<String, String>, Double> user_self_edges = number_of_interests_rdd.mapToPair(x -> 
	    		new Tuple2<>(new Tuple2<>(x._1, x._1), (double)1.0));
	    
	    JavaPairRDD<String, Tuple2<String, Double>> node_rdd = number_of_interests_rdd.mapToPair(
	    		x -> new Tuple2<String, Tuple2<String, Double>>(x._1, new Tuple2<String, Double>(x._1, 1.0)));
	    
	    JavaPairRDD<Tuple2<String, String>, Double> nodes_with_labels = node_rdd.mapToPair(x ->
	    		new Tuple2<>(new Tuple2<>(x._1, x._2._1), x._2._2));

	    
	    for (int i = 0; i < 15; ++i) {
	    	System.out.println("Iteration Number: " + i);
	    	JavaPairRDD<String, Tuple2<String, Double>> current_nodes = node_rdd.join(all_edges).mapToPair(
	    			x -> new Tuple2<>(x._2._2._1, new Tuple2<>(x._2._1._1,x._2._1._2 * x._2._2._2)));
	    	JavaPairRDD<String, Double> sum_of_nodes = current_nodes.mapToPair(x -> new Tuple2<>(
				x._1, x._2._2)).reduceByKey((x, y) -> x+y);
	    	current_nodes = current_nodes.join(sum_of_nodes).mapToPair(x -> new Tuple2<>(x._1, new Tuple2<>(
				x._2._1._1, x._2._1._2 / x._2._2)));
	    	JavaPairRDD<Tuple2<String, String>, Double> join_label = current_nodes.mapToPair(x ->
	    			new Tuple2<>(new Tuple2<>(x._1, x._2._1),( x._2._2))).reduceByKey((x, y)->x+y);		    
	    	join_label = join_label.subtractByKey(user_self_edges).union(user_self_edges);	    	
	    	JavaRDD<Double> diff_ranks = nodes_with_labels.join(join_label).map(t -> Math.abs(t._2._2-t._2._1));	    	
	    	if (i > 0) {
	    		boolean cond = diff_ranks.filter(x -> x > 0.15).count() == 0;
		    	if (cond == true) {
		    		break;
		    	}
	    	}
	    	nodes_with_labels = join_label;
	    	current_nodes = join_label.mapToPair(x -> new Tuple2<>(x._1._1, new Tuple2<>(x._1._2, x._2)));
	    	node_rdd = current_nodes;
	    }

		System.out.println("Now writing to dynamoDB");
		
		node_rdd.foreachPartition((p) -> {
			AmazonDynamoDB second_client = AmazonDynamoDBClientBuilder.standard().build();
			DynamoDB write_client = new DynamoDB(second_client);
			TableWriteItems items_to_put = new TableWriteItems("spark_user_article");
			Set<String> seen = new HashSet<String>();
			int count = 0;
			while (p.hasNext()) {
				Tuple2<String, Tuple2<String, Double>> tup = p.next();
				String link = tup._1;
				String username = tup._2._1;
				String coded = link + "&" + username;
				if (!seen.contains(coded)) {
					Double weight = tup._2._2;
					Item item = new Item().withPrimaryKey("link", link).withString(
							"username", username).withNumber("weight", weight);
					items_to_put.addItemToPut(item);
					count++;
					if (count == 20) {
						BatchWriteItemOutcome outcome = write_client.batchWriteItem(items_to_put);
						while (outcome.getUnprocessedItems().size() > 0) {
							outcome = write_client.batchWriteItemUnprocessed(outcome.getUnprocessedItems());
						}
						count = 0;
						items_to_put = new TableWriteItems("spark_user_article");
					}
				}
			}
			if (count > 0) {
				BatchWriteItemOutcome outcome = write_client.batchWriteItem(items_to_put);
				while (outcome.getUnprocessedItems().size() > 0) {
					outcome = write_client.batchWriteItemUnprocessed(outcome.getUnprocessedItems());
				}
			}  
		});
		return 1;
	}


	/**
	 * Graceful shutdown
	 */
	public void shutdown() {
		System.out.println("Shutting down");
	}
	
	public SocialRankJob() {
		System.setProperty("file.encoding", "UTF-8");
		
	}

	@Override
	public Integer call(JobContext arg0) throws Exception {
		initialize();
		return run();
	}

}
