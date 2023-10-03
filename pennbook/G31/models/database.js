const { IniLoader } = require('aws-sdk');
var AWS = require('aws-sdk');
AWS.config.update({ region: 'us-east-1' });
var db = new AWS.DynamoDB();
var SHA256 = require("crypto-js/sha256");
//const { search_newsfeed } = require('../routes/routes');

// Validates the login credentials for a user 
var checkLogin = function (username, password, callback) {
  console.log('Checking login for: ' + username);

  var params = {
    KeyConditions: {
      username: {
        ComparisonOperator: 'EQ',
        AttributeValueList: [{ S: username }]
      }
    },
    TableName: "users",
    AttributesToGet: ['password']
  };

  db.query(params, function (err, data) {
    if (err || data.Items.length == 0) {
      callback(err, null);
    } else {
      // Set online status to true if login is successful
      setUserOnlineStatus(username, true);
      callback(err, data.Items[0].password.S == SHA256(password).toString());
    }
  });
}

var getPassword = function (username, callback) {
  var params = {
    KeyConditions: {
      username: {
        ComparisonOperator: 'EQ',
        AttributeValueList: [{ S: username }]
      }
    },
    TableName: "users",
    AttributesToGet: ['password']
  };

  db.query(params, function (err, data) {
    if (err || data.Items.length == 0) {
      callback(err, null);
    } else {
      callback(err, data.Items[0].password.S);
    }
  });
}

var setPassword = function (username, password, callback) {
  const params = {
    TableName: "users",
    Key: {
      username: { S: username },
    },
    UpdateExpression: "set #password = :v_password",
    ExpressionAttributeNames: {
      "#password": "password",
    },
    ExpressionAttributeValues: {
      ":v_password": { S: password }
    }
  };

  db.updateItem(params, function (err, data) {
    if (err) {
      callback('Error updating user data', null);
    } else {
      callback(null, 'Success');
    }
  });
}

var addPost = function (post, callback) {
  console.log('Adding post: ' + post.content);

  var params = {
    Item: {
      creator: { S: post.creator },
      user_wall: { S: post.user_wall },
      create_time: { S: post.create_time },
      content: { S: post.content }
    },
    TableName: "posts"
  };

  console.log(params);
  db.putItem(params, function (err, data) {
    if (err) {
      callback(err, null);
    } else {
      callback(err, 'Success');
    }
  });
}

var addComment = function (comment, callback) {
  console.log('Adding comment: ' + comment.content);

  var params = {
    Item: {
      post_id: { S: comment.post_id },
      number: { N: '' + comment.number },
      content: { S: comment.content },
      create_time: { S: comment.create_time },
      creator: { S: comment.creator },
      nested: { N: '' + comment.nested }
    },
    TableName: "comments"
  };

  console.log(params);
  db.putItem(params, function (err, data) {
    if (err) {
      callback(err, null);
    } else {
      callback(err, 'Success');
    }
  });
}

var getPosts = function (username, callback) {
  var params = {
    KeyConditionExpression: "user_wall = :username",
    ExpressionAttributeValues: { ":username": { S: username } },
    TableName: "posts"
  };

  db.query(params, function (err, data) {
    if (err || data.Items.length == 0) {
      callback(err, null);
    } else {
      callback(err, data.Items);
    }
  });
}

var getComments = function (post_id, callback) {
  var params = {
    KeyConditionExpression: "post_id = :post",
    ExpressionAttributeValues: { ":post": { S: post_id } },
    TableName: "comments"
  };

  db.query(params, function (err, data) {
    if (err || data.Items.length == 0) {
      callback(err, null);
    } else {
      callback(err, data.Items);
    }
  });
}

// Retrieves information about a user
var getUser = function (username, callback) {
  console.log('Checking login for: ' + username);

  var params = {
    KeyConditions: {
      username: {
        ComparisonOperator: 'EQ',
        AttributeValueList: [{ S: username }]
      }
    },
    TableName: "users",
  };

  db.query(params, function (err, data) {
    if (err || data.Items.length == 0) {
      callback(err, null);
    } else {
      callback(err, data.Items[0]);
    }
  });
}

// Modifies existing user in the database with the given username
var modifyUser = function (username, email, interests, affiliation, callback) {
  // Failiure edge case: multiple equal interests
  console.log('Updating user: ' + username);
  modifications = [["email_address", { S: email }], ["interests", { SS: interests.split(',') }], ["affiliation", { S: affiliation }]];

  const params = {
    TableName: "users",
    Key: {
      username: { S: username },
    },
    UpdateExpression: "set ",
    ExpressionAttributeNames: {},
    ExpressionAttributeValues: {},
  };

  modifications.forEach(element => {
    if (Object.values(element[1])[0] != '' && Object.values(element[1])[0][0] != '') {
      console.log(Object.values(element[1]))
      console.log(Object.values(element[1])[0][0] == '')
      params.UpdateExpression = params.UpdateExpression + "#" + element[0] + " = :v_" + element[0] + ', ';
      params.ExpressionAttributeNames["#" + element[0]] = element[0];
      params.ExpressionAttributeValues[":v_" + element[0]] = element[1];
    }
  });

  params.UpdateExpression = params.UpdateExpression.slice(0, -2);

  db.updateItem(params, function (err, data) {
    if (err) {
      console.log(err);
      callback('Error updating user data', null);
    } else {
      callback(null, 'Success');
    }
  });
}

// Adds a new user to the database with the given username, password, and full name
var addUser = function (username, password, firstname, lastname, email, interests, birthday, affiliation, callback) {
  console.log('Adding user: ' + username);

  // Create prefix table on the username for the user
  for (let i = 0; i < username.length; i++) {
    const prefix_params = {
      Key: {
        prefix: {
          S: username.substring(0, i + 1)
        }
      },
      TableName: "user_prefixes",
      UpdateExpression: 'ADD usernames :u',
      ExpressionAttributeValues: {
        ':u': { SS: [username] }
      }
    };

    db.updateItem(prefix_params, function (err) {
      if (err) {
        console.log(err);
      }
    });
  }

  const user_params = {
    Item: {
      username: { S: username },
      password: { S: SHA256(password).toString() },
      first_name: { S: firstname },
      last_name: { S: lastname },
      email_address: { S: email },
      interests: { SS: interests.split(',') },
      birthday: { S: birthday },
      affiliation: { S: affiliation },
      is_online: { BOOL: true }
    },
    TableName: "users",
    ConditionExpression: 'attribute_not_exists(username)',
  };

  console.log('user' + user_params);

  db.putItem(user_params, function (err, data) {
    if (err) {
      errorMsg = err.code == 'ConditionalCheckFailedException' ? 'Username already exists' : 'Error adding user';
      callback(errorMsg, null);
    } else {
      callback(err, 'Success');
    }
  });

}

// Add row to DynamoDb table
var putIntoTable = function (tableName, keyName, key, columns, values, callback) {
  // Table and key to be specified
  var params = {
    Item: {
      [keyName]: {
        S: key
      }
    },
    TableName: tableName,
    ReturnValues: 'NONE'
  };

  // For each column, add value to params
  for (var i = 0; i < columns.length; i++) {
    params.Item[[columns[i]]] = { S: values[i] };
  }

  // Call DynamoDB for insertion
  db.putItem(params, function (err, data) {
    if (err)
      callback(err);
    else
      callback(null, 'Success');
  });
}

// Get users affiliation from the database
var getAffiliation = function (username, callback) {
  console.log('Getting affiliation for: ' + username);

  var params = {
    KeyConditions: {
      username: {
        ComparisonOperator: 'EQ',
        AttributeValueList: [{ S: username }]
      }
    },
    TableName: "users",
    AttributesToGet: ['affiliation']
  };

  db.query(params, function (err, data) {
    if (err || data.Items.length == 0) {
      callback(err, null);
    } else {
      callback(err, data.Items[0].affiliation.S);
    }
  });
}

var areUsersFriends = function (username1, username2, callback) {
  console.log('Checking if ' + username1 + ' and ' + username2 + ' are friends');

  var params = {
    KeyConditions: {
      username: {
        ComparisonOperator: 'EQ',
        AttributeValueList: [{ S: username1 }]
      }
    },
    TableName: "friends",
    AttributesToGet: ['friend', 'timestamp']
  };

  db.query(params, function (err, data) {
    if (err) {
      callback(err, null);
    } else {
      const friends = data.Items.map(function (item) {
        return item.friend.S;
      });
      if (friends.includes(username2)) {
        // return the timestamp of the friendship
        console.log(data.Items);
        const timestamp = data.Items.filter(function (item) {
          return item.friend.S == username2;
        })[0].timestamp.N;
        callback(null, timestamp);
      } else {
        callback(err, null);
      }
    }
  });
}

// Remove row from DynamoDb table
var takeFromTable = function (tableName, keyName, key, callback) {
  // Table and key to be specified
  var params = {
    Key: {
      [keyName]: {
        S: key
      }
    },
    TableName: tableName
  };
  console.log(params)
  // Call DynamoDB for insertion
  db.deleteItem(params, callback);
}

//returns all friends of a given username
var userFriends = function (username, callback) {
  var params = {
    KeyConditions: {
      username: {
        ComparisonOperator: 'EQ',
        AttributeValueList: [{ S: username }]
      }
    },
    TableName: "friends",
    AttributesToGet: ['friend']
  };
  db.query(params, function (err, data) {
    if (err) {
      callback(err, null)
    } else {
      callback(null, data.Items.map(item => item.friend.S))
    }
  })
}

// Get all friends of the current user
var getFriends = function (username, affiliation, callback) {
  console.log('Getting friends for: ' + username)

  var params = {
    KeyConditions: {
      username: {
        ComparisonOperator: 'EQ',
        AttributeValueList: [{ S: username }]
      }
    },
    TableName: "friends",
    AttributesToGet: ['friend']
  };

  db.query(params, function (err, data) {
    if (err) {
      callback(err, null);
    } else {
      // For each friend, get their details from the users table
      var friends = data.Items.map(item => item.friend.S);

      if (friends.length == 0) {
        callback(err, []);
        return;
      }

      var params = {
        RequestItems: {
          'users': {
            Keys: friends.map(friend => ({ username: { S: friend } }))
          }
        }
      };

      db.batchGetItem(params, function (err, data) {
        if (err) {
          callback(err, null);
        } else {
          // Filter out friends that are not in the same affiliation if expanding the friends list
          if (affiliation != 'All') {
            data.Responses.users = data.Responses.users.filter(user => user.affiliation.S == affiliation);
          }
          callback(err, data.Responses.users.map(user => ({
            username: user.username.S,
            first_name: user.first_name.S,
            last_name: user.last_name.S,
            is_online: user.is_online.BOOL
          })));
        }
      });
    }
  });
}

var setUserOnlineStatus = function (username, isOnline) {
  console.log('Setting users online status: ' + username);

  var params = {
    Key: {
      username: {
        S: username
      }
    },
    TableName: "users",
    UpdateExpression: 'set is_online = :o',
    ExpressionAttributeValues: {
      ':o': {
        BOOL: isOnline
      }
    }
  };
  db.updateItem(params, function (err) {
    if (err) {
      console.log(err)
    }
  });
}

var getOnlineUsers = function (callback) {
  console.log('Getting online users');

  var params = {
    TableName: "users",
    AttributesToGet: ['username'],
    FilterExpression: 'is_online = :o',
    ExpressionAttributeValues: {
      ':o': {
        BOOL: true
      }
    }
  };

  db.query(params, function (err, data) {
    if (err) {
      callback(err, null);
    } else {
      callback(err, data.Items.map(item => item.username.S));
    }
  });
}

var deleteFriend = function (username, friend, timestamp, callback) {
  console.log('Deleting friend: ' + friend + ' for user: ' + username);

  var params = {
    Key: {
      username: {
        S: username
      },
      timestamp: {
        N: timestamp
      }
    },
    TableName: "friends"
  };

  db.deleteItem(params, function (err) {
    if (err) {
      callback(err);
    } else {
      var params = {
        Key: {
          username: {
            S: friend
          },
          timestamp: {
            N: timestamp
          }
        },
        TableName: "friends"
      };
      db.deleteItem(params, callback);
    }
  });
}

var addFriend = function (username, friend, callback) {
  console.log('Adding friend: ' + friend + ' for user: ' + username);
  const timestamp = Date.now().toString();

  var params = {
    Item: {
      username: {
        S: username
      },
      friend: {
        S: friend
      },
      timestamp: {
        N: timestamp
      }
    },
    TableName: "friends"
  };

  db.putItem(params, function (err) {
    if (err) {
      console.log(err)
      callback(err);
    } else {
      var params = {
        Item: {
          username: {
            S: friend
          },
          friend: {
            S: username
          },
          timestamp: {
            N: timestamp
          }
        },
        TableName: "friends"
      };
      db.putItem(params, callback);
    }
  });
}

var getRecentFriends = function (username, callback) {
  console.log('Getting recent friends for: ' + username);

  var params = {
    KeyConditions: {
      username: {
        ComparisonOperator: 'EQ',
        AttributeValueList: [{ S: username }]
      }
    },
    TableName: "friends",
    AttributesToGet: ['friend', 'timestamp'],
    ScanIndexForward: false,
    Limit: 5
  };

  db.query(params, function (err, data) {
    if (err) {
      callback(err, null);
    } else {
      callback(err, data.Items.map(item => {
        return {
          friend: item.friend.S,
          timestamp: item.timestamp.N
        }
      }));
    }
  });
}

var get_home_posts = function (username, callback) {
  getFriends(username, 'All', function (err, data) {
    if (err) {
      console.log(err);
    } else {
      posts_processed = 0;
      err = false;
      data.push({ username: username });
      friend_posts = [];
      data.forEach((item, index, array) => {
        getPosts(item.username, function (err, data) {
          if (err) {
            console.log(err);
            err = true;
          } else {
            if (data == null) data = [];

            posts = [];
            for (i = 0; i < data.length; ++i) {
              post = {}
              post.creator = data[i].creator.S;
              post.user_wall = data[i].user_wall.S;
              post.create_time = new Date(data[i].create_time.S);
              post.content = data[i].content.S;
              post.type = (data[i].content.S).startsWith('Status Update: ') ? 'status' : 'post';
              if (post.type == 'status') {
                post.content = (data[i].content.S).replace('Status Update: ', '');
              }
              posts.push(post);
            }

            posts.forEach(post => friend_posts.push(post));
          }
          posts_processed++;
          if (posts_processed == array.length) {
            callback(err, friend_posts);
          }
        })
      })
    }
  })
}

// Get all news articles from DynamoDB table
var get_newsfeed_db = function (username, callback) {
  var params = {
    KeyConditions: {
      username: {
        ComparisonOperator: 'EQ',
        AttributeValueList: [{ S: username }]
      }
    },
    IndexName: 'username-index',
    TableName: "spark_user_article",
    AttributesToGet: ['link', 'username', 'weight']
  };
  db.query(params, async function (err, data) {
    if (err) {
      console.log(err);
      callback(err, null);
    } else {
      var all_items = data.Items;
      all_items = all_items.filter(function (item) { return item.link.S.slice(0, 4) === "http" });
      //console.log("time 1");
      all_items.sort(function (a, b) {
        return b.weight.N - a.weight.N;
      })
      var all_items = all_items.slice(0, 10);
      //console.log("time 2");
      //console.log(all_items);
      var today_date = getTodayDate();
      var article_ids = [];
      for (const item of all_items) {
        //possibly check for current date here??
        //STILL TOO EXPENSIVE
        // var article_raw_date = await get_article_date(item.link.S);
        // var clean_article_date = article_raw_date
        // console.log(article_date)
        // if (article_date <= today_date) {
        //   console.log("pushing")
        //   article_ids.push(item.link.S);
        // }
        article_ids.push(item.link.S);
      }
      //console.log("time 3");
      //operation below is expensive
      var results_details = await getAllArticleDetails(article_ids, username);
      //console.log("time 4");
      var results_details = results_details.filter(function(i) {
        return (i[0].date <= today_date)
      })
      //console.log("time 5");
      var final_result_details = results_details.slice(0, 10)
      callback(err, final_result_details);
    }
  });
}

//helper function that gets date of a given article (as a promise)
function get_article_date (given_link, callback) {
  return new Promise((resolve, reject) => {
    var params = {
      KeyConditions: {
        link: {
          ComparisonOperator: 'EQ',
          AttributeValueList: [{ S: given_link }]
        }
      },
      TableName: "news_articles",
      AttributesToGet: ['date']
    }
    db.query(params, function (err, data) {
      if (err) {
        console.log(err)
      } else {
        resolve(data.Items)
      }
    })
  })
}

// get all news articles with a list of given keywords
//sort them based on number of keyword occurences and then spark weights
var get_news_search_db = function (username, keywords, callback) {

  const arrayOfPromises = [];
  const article_ids = [];
  for (const k of keywords) {
    var params = {
      TableName: "search_index",
      KeyConditionExpression: "keyword = :k",
      ExpressionAttributeValues: {
        ":k": { S: k },
      },
    }
    arrayOfPromises.push(db.query(params, function (err, data) { }).promise());
  }
  Promise.all(arrayOfPromises).then(
    async function (successfulDataArray) {
      successfulDataArray.forEach(function (result) {
        result.Items.forEach(function (article) {
          article_ids.push(article.link.S);
        })
      })
      const reduced = article_ids.reduce((occurrences, item) => {
        occurrences[item] = (occurrences[item] || 0) + 1;
        return occurrences;
      }, {});
      const count_map = Object.keys(reduced).map((item) => {
        return { key: item, value: reduced[item] };
      });
      var groupBy = function (xs, key) {
        return xs.reduce(function (rv, x) {
          (rv[x[key]] = rv[x[key]] || []).push(x);
          return rv;
        }, {});
      };
      var groupedByVal = groupBy(count_map, 'value');
      var sorted_categories = Object.keys(groupedByVal).sort().reverse();
      all_cat_links = []
      for (const category of sorted_categories) {
        cat_links = [];
        groupedByVal[category].forEach(function (i) {
          cat_links.push(i.key)
        })
        const results_weights = await getAllArticleWeights(username, cat_links);
        weights_to_sort = [];
        for (const weight_object of results_weights) {
          weight = weight_object[0]
          if (weight) {
            weights_to_sort.push(weight.weight)
          }
          else {
            weights_to_sort.push(0.0)
          }
        }
        var len = weights_to_sort.length;
        var indices = new Array(len);
        for (var i = 0; i < len; ++i) indices[i] = i;
        indices.sort(function (a, b) {
          return weights_to_sort[a] > weights_to_sort[b] ? -1 :
            weights_to_sort[a] < weights_to_sort[b] ? 1 : 0;
        });
        sorted_cat_links = []
        for (const i of indices) {
          sorted_cat_links.push(cat_links[i]);
        }
        all_cat_links = all_cat_links.concat(sorted_cat_links)
      }
      var results_details = await getAllArticleDetails(all_cat_links, username);
      var today_date = getTodayDate();
      var final_result_details = results_details.filter(function(i) {
        return (i[0].date <= today_date)
      })
      callback(null, final_result_details);
    }
  )
}

// Search user_prefix table based on search string
var searchUsers = function (searchString, callback) {
  console.log("searching for users with prefix: " + searchString);
  var params = {
    TableName: "user_prefixes",
    KeyConditionExpression: "prefix = :p",
    ExpressionAttributeValues: {
      ":p": { S: searchString },
    },
  };

  db.query(params, function (err, data) {
    if (err) {
      console.error("Unable to query. Error:", err);
    } else {
      callback(null, data.Items);
    }
  });
};

//adds message to database
var addMessage = function (sender, chatId, message_content, callback) {
  var time_stamp = new Date().getTime().toString();
  var params = {
    TableName: "messages",
    Item: {
      "chat_id": {
        "S": chatId
      },
      "create_time": {
        "S": time_stamp
      },
      "message": {
        "S": message_content
      },
      "sender": {
        "S": sender
      }
    }
  };
  db.putItem(params, function (err, data) {
    if (err) {
      callback(err);
    } else {
      callback(null, "Message sent");
    }
  });
}

//gets all messages and members for a given chat id from database
var getChatMessages = function (chat_id, callback) {
  var params = {
    TableName: "messages",
    KeyConditionExpression: "chat_id = :chatid",
    ExpressionAttributeValues: {
      ':chatid': { S: chat_id }
    },
  };
  db.query(params, function (err, data) {
    if (err) {
      callback(err);
    } else {
      var chat_data = data
      chat_data.Items.reverse()
      var params_members = {
        TableName: "chat_groups",
        IndexName: "chat_id-index",
        KeyConditionExpression: "chat_id = :chatidformember",
        ExpressionAttributeValues: {
          ':chatidformember': { S: chat_id }
        },
      };
      db.query(params_members, function(err, data) {
        if (err) {
          callback(err)
        } else {
          var members_data = data.Items.map(item => item.username.S);
          callback(null, [chat_data, members_data]);
        }
      })
    }
  });
}

//creates a group chat and adds info to database
var addChat = function (username, chat_id, from_invite, callback) {
  if (from_invite) {
	var params = {
        TableName: "chat_groups",
        Item: {
          "username": {
            "S": username
          },
          "chat_id": {
            "S": chat_id
          }
        }
      };
      db.putItem(params, function (err, data) {
        if (err) {
          callback(err);
        } else {
          callback(null, "Chat created");
        }
      });
  }	else {
	const params = {
    TableName: 'chat_groups',
    IndexName: 'chat_id-index',
    KeyConditionExpression: 'chat_id = :c',
    ExpressionAttributeValues: {
      ':c': {
        'S': chat_id
      }
    }
  };
  db.query(params, function (err, data) {
    if (err) {
      console.log(err)
    } else if (data.Items.length > 0) {
      callback(-1, "Pick a different groupchat name! This one already exists")
    } else {
      var params = {
        TableName: "chat_groups",
        Item: {
          "username": {
            "S": username
          },
          "chat_id": {
            "S": chat_id
          }
        }
      };
      db.putItem(params, function (err, data) {
        if (err) {
          callback(err);
        } else {
          callback(null, "Chat created");
        }
      });
    }
  });
	
  }
	
}

//leaves user from group chat
var leaveChat = function (username, chat_id, callback) {
  const params = {
    TableName: 'chat_groups',
    KeyConditionExpression: 'username = :u and chat_id = :c',

    ExpressionAttributeValues: {
      ':u': {
        'S': username
      },
      ':c': {
        'S': chat_id
      }
    }
  };
  db.query(params, function (err, data) {
    if (err || data.Items.length == 0) {
      //username was not in groupchat
      callback(err, "username was not in group chat!");
    } else {
      var params = {
        TableName: "chat_groups",
        Key: {
          username: {
            S: username
          },
          chat_id: {
            S: chat_id
          }
        }
      };
      db.deleteItem(params, function (err, data) {
        if (err) {
          callback(err);
        } else {
          callback(null, "Chat left");
        }
      });
    }
  });
}

//invites given user to a chat id
var addInvite = function (username, chat_id, callback) {
  const params = {
    TableName: 'invites',
    KeyConditionExpression: 'username = :u and invited_chat_id = :c',
    ExpressionAttributeValues: {
      ':u': {
        'S': username
      },
      ':c': {
        'S': chat_id
      }
    }
  };
  db.query(params, function (err, data) {
    if (err) {
      console.log(err)
    } else if (data.Items.length > 0) {
      callback(-1, "Invite to this user for current group chat is already sent!")
    } else {
      //add check if invited user is already in chat
      var params = {
		TableName: "chat_groups",
		KeyConditionExpression: 'username = :u and chat_id = :c',
    	ExpressionAttributeValues: {
      		':u': {
        		'S': username
      		},
      		':c': {
        		'S': chat_id
      		}
      	}
	  }
	  db.query(params, function (err, data) {
		if (err) {
      		console.log(err)
    	} else if (data.Items.length > 0) {
      		callback(-1, "This user is already in the groupchat!")
    	} else {
			var params = {
	        TableName: "invites",
	        Item: {
	          	"username": {
	            	"S": username
	          	},
	          	"invited_chat_id": {
	            	"S": chat_id
	          	}
	          }
	      	};
	      db.putItem(params, function (err, data) {
	        if (err) {
	          callback(err);
	        } else {
	          callback(null, "User invited to Chat");
	        }
      	  });
	    }
    })
    }
  })
}

//deletes invite for a user
var deleteInvite = function (username, chat_id, callback) {
  const params = {
    TableName: 'invites',
    KeyConditionExpression: 'username = :u and invited_chat_id = :c',
    ExpressionAttributeValues: {
      ':u': {
        'S': username
      },
      ':c': {
        'S': chat_id
      }
    }
  };
  db.query(params, function (err, data) {
    if (err || data.Items.length == 0) {
      //username was not in groupchat
      console.log(err)
      callback(err, "invite was not present!");
    } else {
      var params = {
        TableName: "invites",
        Key: {
          username: {
            S: username
          },
          invited_chat_id: {
            S: chat_id
          }
        }
      };
      db.deleteItem(params, function (err, data) {
        if (err) {
          callback(err);
        } else {
          callback(null, "Invite deleted");
        }
      });
    }
  });
}

//gets all rooms for a user (as a promise)
var getRooms = function (username, callback) {
  return new Promise((resolve, reject) => {
    const params = {
      TableName: 'chat_groups',
      KeyConditionExpression: 'username = :u',
      ExpressionAttributeValues: {
        ':u': {
          'S': username
        }
      }
    };
    db.query(params, function (err, data) {
      if (err) {
        console.log(err);
        console.error("Unable to query");
      } else {
        var result = data.Items.map(item => item.chat_id.S);
        resolve(result);
      }
    });
  });
}

//gets all invites for a user (as a promise)
var getInvites = function (username, callback) {
  return new Promise((resolve, reject) => {
    const params = {
      TableName: 'invites',
      KeyConditionExpression: 'username = :u',
      ExpressionAttributeValues: {
        ':u': {
          'S': username
        }
      }
    };
    db.query(params, function (err, data) {
      if (err) {
        console.error("Unable to query");
      } else {
        var result = data.Items.map(item => item.invited_chat_id.S);
        resolve(result);
      }
    });
  });
}

//get all Rooms and Invites
var getAllRoomsInvites = async function (username, callback) {
  rooms_res = await getRooms(username);
  invites_res = await getInvites(username);
  callback(null, [rooms_res, invites_res]);
}

//adds like for given username and article 
var addNewsLike = function (given_username, given_link, callback) {
  var params = {
    Item: {
      username: { S: given_username },
      link: { S: given_link },
    },
    TableName: "news_likes"
  };
  db.putItem(params, function (err, data) {
    if (err) {
      console.log(err);
    } else {
      callback(err, 'Success adding like');
    }
  });
}

//deletes like for given username and article
var deleteNewsLike = function(given_username, given_link, callback) {
  var params = {
    Key: {
      username: {
        S: given_username
      },
      link: {
        S: given_link
      }
    },
    TableName: "news_likes"
  };
  
  db.deleteItem(params, function (err) {
    if (err) {
      console.log(err);
    } else {
      callback(err, 'Success deleting like');
    }
  });
}

//helper function to get today's date in the required format
function getTodayDate() {
  var today = new Date();
  var dd = String(today.getDate()).padStart(2, '0');
  var mm = String(today.getMonth() + 1).padStart(2, '0');
  var yyyy = today.getFullYear();
  return yyyy + '-' + mm + '-' + dd;
}



//helper function to fetch article details and is_liked by user for list of articles
function getAllArticleDetails(listOfIds, username) {
  return new Promise(async function (res, rej) {
    var allArticleDetails = []
    for (var i = 0; i < listOfIds.length; i++) {
      allArticleDetails.push(await getArticleDetails(listOfIds[i], username));
    }
    res(allArticleDetails);
  });
}

// helper function that returns article details and is_liked by user given article link/id
function getArticleDetails(id, username) {
  return new Promise((resolve, reject) => {
    var docClient = new AWS.DynamoDB.DocumentClient();
    var params = {
      TableName: "news_articles",
      KeyConditionExpression: "link = :l",
      ExpressionAttributeValues: {
        ":l": id,
      },
    };
    docClient.query(params, function (err, data) {
      if (err) {
        console.error("Unable to query. Error:", JSON.stringify(err, null, 2));
      } else {
        var result = data.Items;
        var likes_params = {
          TableName: "news_likes",
          KeyConditionExpression: "username = :u and link = :l",
          ExpressionAttributeValues: {
            ":u": username,
            ":l": id,
          },
        }
        docClient.query(likes_params, function (err, data) {
          if (err) {
            console.log(err)
          } else {
            if (data.Items.length > 0) {
              result[0].is_liked = true  
            } else {
              result[0].is_liked = false
            }
            resolve(result);
          }
        })
      }
    });
  });
}

//helper function to fetch weight for list of articles (given username)
function getAllArticleWeights(username, listOfIds) {
  return new Promise(async function (res, rej) {
    var allArticleWeights = []
    for (var i = 0; i < listOfIds.length; i++) {
      allArticleWeights.push(await getArticleWeight(username, listOfIds[i]));
    }
    res(allArticleWeights);
  });
}

// helper function that returns article weights given article link/id and username
function getArticleWeight(username, id) {
  return new Promise((resolve, reject) => {
    var docClient = new AWS.DynamoDB.DocumentClient();
    var params = {
      TableName: "spark_user_article",
      KeyConditionExpression: "link = :l and username = :u",
      ExpressionAttributeValues: {
        ":l": id, ":u": username,
      },
    };
    docClient.query(params, function (err, data) {
      if (err) {
        console.error("Unable to query. Error:", JSON.stringify(err, null, 2));
      } else {
        var result = data.Items;
        resolve(result);
      }
    });
  });
}


/* We define an object with one field for each method. For instance, below we have
   a 'lookup' field, which is set to the myDB_lookup function. In routes.js, we can
   then invoke db.lookup(...), and that call will be routed to myDB_lookup(...). */
var database = {
  checkLogin: checkLogin,
  addUser: addUser,
  getNewsfeedDB: get_newsfeed_db,
  getFriends: getFriends,
  userFriends: userFriends,
  setUserOnlineStatus, setUserOnlineStatus,
  getOnlineUsers: getOnlineUsers,
  getAffiliation: getAffiliation,
  deleteFriend: deleteFriend,
  addFriend: addFriend,
  getRecentFriends: getRecentFriends,
  searchUsers: searchUsers,
  searchNewsFeed: get_news_search_db,
  getUser: getUser,
  modifyUser: modifyUser,
  getPassword: getPassword,
  setPassword: setPassword,
  addPost: addPost,
  getPosts: getPosts,
  getComments: getComments,
  addComment: addComment,
  areUsersFriends: areUsersFriends,
  addMessage: addMessage,
  getChatMessages: getChatMessages,
  addChat: addChat,
  leaveChat: leaveChat,
  addInvite: addInvite,
  deleteInvite: deleteInvite,
  getAllRoomsInvites: getAllRoomsInvites,
  get_home_posts: get_home_posts,
  addNewsLike: addNewsLike,
  deleteNewsLike, deleteNewsLike
};

module.exports = database;
