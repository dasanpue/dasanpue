/* This code loads some test data into a DynamoDB table. You _could_ modify this
   to upload test data for HW4 (which has different tables and different data),
   but you don't have to; we won't be grading this part. If you prefer, you can
   just stick your data into DynamoDB tables manually, using the AWS web console. */

var AWS = require('aws-sdk');
AWS.config.update({ region: 'us-east-1' });
var db = new AWS.DynamoDB();
var async = require('async');
const {readFileSync, promises: fsPromises} = require('fs');
const stemmer = require('stemmer');
//import {stemmer} from 'stemmer';

/* populate calls initTable and then, once that finishes, it uploads all the words
   in parallel and waits for all the uploads to complete (async.forEach). */

var populate = function (tableName, keyName, columnNames, values) {
  initTable(tableName, keyName, function (err, data) {
    if (err)
      console.log("Error while initializing table: " + err);
    else {
      async.forEach(values, function (item, callback) {
        //console.log("Uploading item: " + item[0]);
        putIntoTable(tableName, keyName, item[0], columnNames, item.slice(1), function (err, data) {
          if (err)
            console.log("Oops, error when adding data for " + item[0] + ": " + err);
        });
      }, function () { console.log("Upload complete") });
    }
  });
}
/* The function below checks whether a table with the above name exists, and if not,
   it creates such a table with a hashkey called 'keyword', which is a string. 
   Notice that we don't have to specify the additional columns in the schema; 
   we can just add them later. (DynamoDB is not a relational database!) */

var initTable = function (tableName, keyName, callback) {
  db.listTables(function (err, data) {
    if (err) {
      console.log(err, err.stack);
      callback('Error when listing tables: ' + err, null);
    } else {
      console.log("Connected to AWS DynamoDB");

      var tables = data.TableNames.toString().split(",");
      console.log("Tables in DynamoDB: " + tables);
      if (tables.indexOf(tableName) == -1) {
        console.log("Creating new table '" + tableName + "'");

        var params = {
          AttributeDefinitions:
            [
              {
                AttributeName: keyName,
                AttributeType: 'S'
              }
            ],
          KeySchema:
            [
              {
                AttributeName: keyName,
                KeyType: 'HASH'
              }
            ],
          ProvisionedThroughput: {
            ReadCapacityUnits: 20,       // DANGER: Don't increase this too much; stay within the free tier!
            WriteCapacityUnits: 20       // DANGER: Don't increase this too much; stay within the free tier!
          },
          TableName: tableName /* required */
        };

        db.createTable(params, function (err, data) {
          if (err) {
            console.log(err)
            callback('Error while creating table ' + tableName + ': ' + err, null);
          }
          else {
            console.log("Table is being created; waiting for 20 seconds...");
            setTimeout(function () {
              console.log("Success");
              callback(null, 'Success');
            }, 20000);
          }
        });
      } else {
        console.log("Table " + tableName + " already exists");
        callback(null, 'Success');
      }
    }
  });
}

// cleanTables() delets all tables present in the aws DynamoDB
var cleanTables = function () {
  db.listTables(function (err, data) {
    if (err) {
      console.log(err, err.stack);
      console.log('Error when listing tables: ' + err, null);
    } else {
      console.log("Connected to AWS DynamoDB");
      var tables = data.TableNames.toString().split(",");
      console.log("Tables in DynamoDB: " + tables);
      tables.forEach(table =>
        db.deleteTable({ TableName: table }, function (err, data) {
          if (err) {
            console.log('Error when deleting table ' + table + ': ' + err);
          } else {
            console.log('Table ' + table + ' successfully deleted');
          }
        }));
    }
  });
}

// cleanTables(tableName) delets table with name [tableName] present in the aws DynamoDB
var cleanTable = function (tableName) {
  db.listTables(function (err, data) {
    if (err) {
      console.log(err, err.stack);
      console.log('Error when listing tables: ' + err, null);
    } else {
      console.log("Connected to AWS DynamoDB");
      var tables = data.TableNames.toString().split(",");
      console.log("Tables in DynamoDB: " + tables);
      if (tables.indexOf(tableName) != -1) {
        db.deleteTable({ TableName: tableName }, function (err, data) {
          if (err) {
            console.log('Error when deleting table ' + tableName + ': ' + err);
          } else {
            console.log('Table ' + tableName + ' successfully deleted');
          }
        });
      } else {
        console.log('table not found');
      }
    }
  });
}

/* This function puts an item into the table. Notice that the column is a parameter;
   hence the unusual [column] syntax. This function might be a good template for other
   API calls, if you need them during the project. */

var putIntoTable = function (tableName, keyName, key, columns, values, callback) {
  var params = {
    Item: {
      [keyName]: {
        S: key
      }
    },
    TableName: tableName,
    ReturnValues: 'NONE'
  };

  for (var i = 0; i < columns.length; i++) {
    params.Item[[columns[i]]] = { S: values[i] }
  }

  db.putItem(params, function (err, data) {
    if (err)
      callback(err)
    else
      callback(null, 'Success')
  });
}

console.log("Loading function");
console.log(process.argv);


if (process.argv[2] == 'start') {

  //DO NOT REMOVE - creates and populates search index table for news
  const contents = readFileSync('nlp_en_stop_words.txt', 'utf-8');
  var stopwords = contents.split('\n');
  var data_to_upload = [];
  var params = {
    TableName: "news_articles",
  };
  db.scan(params, function (err, data) {
    if (err || data.Items.length == 0) {
      console.log('not found')
    } else {
      data.Items.forEach(function(item) {
        var article_url = item.link.S;
        var headline = item.headline.S;
        headline.split(" ").forEach(async function(word) {
          if (/^[a-zA-Z]*$/.test(word)) {
            if (!stopwords.includes(word) && !(word === "")) {
              var keyword = stemmer(word.toLowerCase());
              var row = [];
              row.push(keyword);
              row.push(article_url);
              data_to_upload.push(row);
            }
          }
        }); 
      });
    }
  });
  setTimeout(function () {
    populate('search_index', 'keyword', [
    'link']
    , data_to_upload);
  }, 15000);
  


  //DO NOT REMOVE - get data from news json and upload
  // const ReadLines = require('n-readlines')
  // const readlines = new ReadLines('News_Category_Dataset_v2.json')
  // //const readlines = new ReadLines('temp.json')
  // var data_to_upload = [];
  // let line;
  // while ((line = readlines.next())) {
  //   var single_json = JSON.parse(line.toString('ascii'))
  //   // var single_json_list = [];
  //   // single_json_list.push(single_json['link']);
  //   // single_json_list.push(single_json['category']);
  //   // single_json_list.push(single_json['headline']);
  //   // single_json_list.push(single_json['authors']);
  //   // single_json_list.push(single_json['short_description']);
  //   var new_year = parseInt(single_json['date'].slice(2, 4)) + 5;
  //   if (new_year >= 23 || new_year <= 20) {
  //     continue;
  //   }
  //   var new_date = single_json['date'].slice(0, 2) + new_year.toString() + single_json['date'].slice(4)
  //   //single_json_list.push(new_date);
  //   putIntoTable('news_articles', 'link', single_json['link'], ['category', 
  //         'headline', 'authors', 'short_description', 'date'], [single_json['category'], 
  //         single_json['headline'], single_json['authors'], single_json['short_description'], new_date], 
  //         function(err, message) {})
  //   //data_to_upload.push(single_json_list)
  // }

  // data_to_upload type is a list of lists

  // populate('news_articles', 'link', [
  //   'category',
  //   'headline',
  //   'authors',
  //   'short_description',
  //   'date',]
  //   , data_to_upload);
} else if (process.argv[2] == 'end') {
  if (process.argv.length == 3) {
    console.log('plase specify an table to delete, or \'all\' for all tables')
  } else if (process.argv[3] == 'all') {
    cleanTables();
  } else {
    cleanTable(process.argv[3]);
  }
} else {
  console.log('Please specify an action parameter from: {\'start\',\'end\'}')
}