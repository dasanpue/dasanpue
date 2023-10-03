//const session = require('express-session');
var db = require('../models/database.js');
const stemmer = require('stemmer');
var message = null;
var SHA256 = require("crypto-js/sha256");
const database = require('../models/database.js');
//const { ConfigurationServicePlaceholders } = require('aws-sdk/lib/config_service_placeholders.js');

var getMain = function (req, res) {
  // Render the login page
  // if user is already logged in (e.g from another tab), go direct to /home
  if (req.session.username) {
    res.redirect('/home');
  }
  else {
    res.render('main.ejs', { message: message });
  }

  // if main page was redirected from somewhere (e.g /newsfeed without login),
  // clear the message variable here
  message = null;
};

var getLogin = function (req, res) {
  // Render the login page
  res.render('login.ejs', { message: null });
};

var getSignup = function (req, res) {
  // Render the signup page
  res.render('signup.ejs', { message: null });
};

var getWall = function (req, res) {
  if (req.session.username) {
    const user_wall = req.params.user == null ? req.session.username : req.params.user;
    db.getPosts(user_wall, function (err, data) {
      if (err) {
        console.log(err);
      } else {
        if (data == null) data = [];
        posts = [];
        for (i = 0; i < data.length; ++i) {
          data[i].create_time.S = new Date(data[i].create_time.S)
          posts.push(data[i]);
        }
        posts.sort(function (a, b) { return b.create_time.S - a.create_time.S; })

        db.areUsersFriends(req.session.username, user_wall, function (err, areFriends) {
          if (err) {
            console.log(err);
          } else {
            console.log('are friends: ' + areFriends);
            res.render('wall.ejs', { posts: posts, username: req.session.username, user_wall: user_wall, are_friends: areFriends });
          }
        })
      };
    });
  } else {
    message = 'Login before viewing wall';
    res.redirect('/');
  }
};

var getPosts = function (req, res) {
  console.log('get posts called')
  const user_wall = req.params.user == null ? req.session.username : req.params.user;
  db.getPosts(user_wall, function (err, data) {
    if (err) {
      console.log(err);
      res.send(JSON.stringify({ message: { type: 'danger', content: 'Error retrieving posts' }, posts: [] }));
    } else {
      if (data == null) data = [];

      posts = [];
      for (i = 0; i < data.length; ++i) {
        post = {}
        post.creator = data[i].creator.S;
        post.user_wall = data[i].user_wall.S;
        post.create_time = new Date(data[i].create_time.S);
        post.content = data[i].content.S;
        posts.push(post);
      }

      posts.sort(function (a, b) { return b.create_time.S - a.create_time.S; })

      res.send(JSON.stringify({ message: { type: 'success', content: 'Sucessfully retrieved posts' }, posts: posts }));
    }
  })
};

var postPost = function (req, res) {
  post = req.body;
  db.addPost(post, function (err, data) {
    if (err) {
      console.log(err);
      res.send(JSON.stringify({ type: "danger", message: "Error updating data, please try again" }));
    } else {
      console.log('success!!!');
      res.send(JSON.stringify({ type: "success", message: "Successfully added post" }));
    }
  });
}

var postComment = function (req, res) {
  comment = req.body;
  db.addComment(comment, function (err, data) {
    if (err) {
      console.log(err);
      res.send(JSON.stringify({ type: "danger", message: "Error updating data, please try again" }));
    } else {
      console.log('success!!!');
      res.send(JSON.stringify({ type: "success", message: "Successfully added comment" }));
    }
  });
}

var getComments = function (req, res) {
  console.log('Retrieving comments corresponding to post with key: ' + req.query.user_wall + ',' + req.query.create_time);
  db.getComments(req.query.user_wall + ',' + req.query.create_time, function (err, data) {
    if (err) {
      console.log('Error retrieving comments' + err);
      res.send(JSON.stringify({ message: { type: 'danger', content: 'Error retrieving comments' }, comments: [] }));
    } else {
      if (data == null) data = [];

      comments = [];
      for (let i = 0; i < data.length; ++i) {
        comment = {}
        comment.post_id = data[i].post_id.S;
        comment.number = data[i].number.N;
        comment.content = data[i].content.S;
        comment.create_time = new Date(data[i].create_time.S);
        comment.creator = data[i].creator.S;
        comment.nested = data[i].nested.N;
        comments.push(comment);
      }

      for (let i = 0; i < comments.length; ++i) {
        date_arr = [];
        curr = comments[i];
        while (curr.nested != -1) {
          date_arr.unshift(curr);
          curr = comments.filter(e => e.number == curr.nested)[0];
        }
        date_arr.unshift(curr);
        comments[i]['date_nest'] = date_arr;
      }

      comments.sort(function (a, b) {
        for (let i = 0; i < Math.min(a.date_nest.length, b.date_nest.length); ++i) {
          if (a.date_nest[i].number != b.date_nest[i].number) {
            return b.date_nest[i].create_time - a.date_nest[i].create_time;
          }
        }
        return a.date_nest.length - b.date_nest.length
      })

      if (comments[0]) comments[0]['nested'] = 1;
      for (let i = 1; i < comments.length; ++i) {
        comments[i]['nested'] = comments[i].date_nest.length - comments[i - 1].date_nest.length;
      }

      comments.map(comment => delete comment['date_nest']);

      res.send(JSON.stringify({ message: { type: 'success', content: 'Sucessfully retrieved posts' }, comments: comments }));
    }
  })
};

var getAccountSettings = function (req, res) {
  if (req.session.username) {
    db.getUser(session.username, function (err, data) {
      if (err) {
        res.render('login.ejs', { message: err });
      } else if (data) {
        data.username = session.username;
        res.render('settings.ejs', { user: data, message: null });
      } else {
        res.render('login.ejs', { message: 'Information for user not found, please try again' });
      }
    });
  } else {
    res.render('login.ejs', { message: 'Please log in to view your account settings' });
  }
}

var postSettings = function (req, res) {
  console.log(req.body);
  var exec = require('child_process').exec;
  exec('cd news_algo && mvn exec:java@livy');
  db.modifyUser(session.username, req.body.email, req.body.interests, req.body.affiliation, function (err, data) {
    if (err) {
      console.log(err);
      res.send(JSON.stringify({ type: "danger", message: "Error updating data, please try again" }));
    } else {
      console.log('success!!!');
      res.send(JSON.stringify({ type: "success", message: "Successfully updated settings" }));
    }
  });
}

var changePassword = function (req, res) {
  console.log(req.body);
  db.getPassword(session.username, function (err, data) {
    if (!err) {
      if (data != SHA256(req.body.old).toString()) {
        res.send(JSON.stringify({ type: "danger", message: "Incorrect password, please try again" }));
      } else {
        console.log('new pass: ' + req.body.new);
        db.setPassword(session.username, SHA256(req.body.new).toString(), function (err, data) {
          if (err) {
            res.send(JSON.stringify({ type: "danger", message: "Error updating data, please try again" }));
          } else {
            res.send(JSON.stringify({ type: "success", message: "Successfully updated password" }));
          }
        });
      }
    }
  });
}

var checkLogin = function (req, res) {
  var username = req.body.username;
  var password = req.body.password;

  if (!username || !password || username.trim() == '' || password.trim() == '') {
    // If the user didn't enter all the required fields, show an error message
    res.render('login.ejs', { message: 'Please enter both a username and password that all contain at least one character' });
  } else {
    // Check the user's login credentials and redirect to the restaurants page on success
    // On failure, show an error message
    db.checkLogin(username, password, function (err, data) {
      if (err) {
        res.render('login.ejs', { message: err });
      } else if (data) {
        session = req.session;
        session.username = username;
        // Redirect to home page
        res.redirect('/home');
      } else {
        res.render('login.ejs', { message: 'Login failed!' });
      }
    });
  }
};

var createAccount = function (req, res) {
  var username = req.body.username;
  var password = req.body.password;
  var firstname = req.body.firstname;
  var lastname = req.body.lastname;
  var email = req.body.email;
  var interests = req.body.interests;
  var birthday = req.body.birthday;
  var affiliation = req.body.affiliation;

  if (username.trim() == '' || password.trim() == '' || firstname.trim() == '' || lastname.trim() == '' || email.trim() == '' || interests.trim() == '' || birthday.trim() == '' || affiliation.trim() == '') {
    // If the user didn't enter all the required fields, show an error message
    res.render('signup.ejs', { message: 'Please fill out all fields before submitting' });
  } else {
    // Add the user to the database and redirect to the restaurants page on success
    // On failure, show an error message
    db.addUser(username, password, firstname, lastname, email, interests, birthday, affiliation, function (err, data) {
      if (err) {
        res.render('signup.ejs', { message: err });
      } else if (data) {
        session = req.session;
        session.username = username;
        res.redirect('/home');
      } else {
        res.render('signup.ejs', { message: 'Account creation failed!' });
      }
    });
  }
};

var getHome = function (req, res) {

  if (!req.session.username) {
    message = 'Login before viewing Home page';
    res.redirect('/');
  } else {
    db.get_home_posts(session.username, function (err, friend_posts) {
      if (err) {
        res.render('home.ejs', { message: { type: 'danger', content: 'Error retrieving posts' }, posts: [] });
      } else {
        // Sort posts by create_time
        friend_posts.sort(function (a, b) {
          return new Date(b.create_time) - new Date(a.create_time);
        });
        // Convert create_time to a human readable string
        //friend_posts.forEach(post => post.create_time = post.create_time.toLocaleString());

        db.getRecentFriends(req.session.username, function (err, recentFriends) {
          if (err) {
            res.render('home.ejs', { message: { type: 'success', content: 'Sucessfully retrieved posts' }, posts: friend_posts, recentFriends: [], username: req.session.username });
          } else {
            res.render('home.ejs', { message: { type: 'success', content: 'Sucessfully retrieved posts' }, posts: friend_posts, recentFriends: recentFriends, username: req.session.username });
          }
        }
        );
      }
    });
  }
};

var getFriendsPosts = function (req, res) {
  db.get_home_posts(session.username, function (err, friend_posts) {
    if (err) {
      res.send(JSON.stringify({ message: { type: 'danger', content: 'Error retrieving posts' }, posts: [] }));
    } else {
      // Sort posts by create_time
      friend_posts.sort(function (a, b) {
        return new Date(b.create_time) - new Date(a.create_time);
      });
      // Convert create_time to a human readable string
      //riend_posts.forEach(post => post.create_time = post.create_time.toLocaleString());

      res.send(JSON.stringify({ message: { type: 'success', content: 'Sucessfully retrieved posts' }, posts: friend_posts }));
    }
  });
};

var getLogout = function (req, res) {
  // Remove the session when logging out, set user to offline and redirect to the login page
  db.setUserOnlineStatus(req.session.username, false)
  req.session.destroy();
  res.redirect('/');
};

var getNewsfeed = function (req, res) {

  if (req.session.username) {
    // TODO implement getNewsfeedDB in database.js
    db.getNewsfeedDB(req.session.username, function (err, data) {
      if (err) {
        res.render('newsfeed.ejs', { message: err, newsfeedDB: [], username: req.session.username });
      } else {
        res.render('newsfeed.ejs', { message: message, newsfeedDB: data, username: req.session.username });
        // if redirected from somewhere clear the message variable here
        message = null;
      }
    });
  } else {
    message = 'Login before viewing Newsfeed';
    res.redirect('/');
  }

};

var getFriends = function (req, res) {
  if (req.session.username) {
    db.getFriends(req.session.username, 'All', function (err, data) {
      if (err) {
        res.render('friendvisualizer.ejs', { message: err, friends: [], username: req.session.username });
      } else {
        res.render('friendvisualizer.ejs', {
          message: message, friends: data.sort((a, b) => (a.username < b.username) ? -1 : 1), username: req.session.username
        });
      }
    });
  }
  else {
    message = 'Login before viewing friends';
    res.redirect('/');
  }
}

var getFriendsUpdate = function (req, res) {
  if (req.session.username) {
    db.getFriends(req.session.username, 'All', function (err, data) {
      if (data) {
        res.json(data);
      }
    });
  } else {
    message = 'Login before viewing friends';
    res.redirect('/');
  }
}

var getFriendsVisualizer = function (req, res) {
  const username = req.session.username;
  if (username) {
    db.getFriends(username, 'All', function (err, data) {
      console.log(data)
      json = {
        'id': username,
        'name': username,
        'children': data.map(function (friend) {
          return {
            'id': friend.username,
            'name': friend.username,
            'children': [],
            'data': [],
          }
        }),
        'data': [],
      };
      res.json(json);
    });
  } else {
    message = 'Login before viewing friends';
    res.redirect('/');
  }
}

var getFriendsVisualizerExpanded = function (req, res) {
  const username = req.params.user
  // Get current users affliation
  db.getAffiliation(username, function (err, affiliation) {
    if (err) {
      console.log(err);
    } else {
      // Get all users with the same affiliation
      console.log('affiliation', affiliation)
      db.getFriends(username, affiliation, function (err, data) {
        json = {
          'id': username,
          'name': username,
          'children': data.map(function (friend) {
            return {
              'id': friend.username,
              'name': friend.username,
              'children': [],
              'data': [],
            }
          }),
          'data': [],
        };
        res.json(json);
      });
    }
  });
}
//searches news feed for given keywords
var searchNewsFeed = function (req, res) {
  if (req.session.username) {
    var keyword_string = req.query.keyword
    var raw_keywords = keyword_string.split(" ");
    var final_keywords = [];
    for (let i = 0; i < raw_keywords.length; i++) {
      final_keywords.push(stemmer(raw_keywords[i].toLowerCase()));
    }
    db.searchNewsFeed(req.session.username, final_keywords, function (err, data) {
      if (err) {
        res.render('newsfeed_search.ejs', {
          message: err,
          newsfeedDB: [], username: req.session.username, keywords: keyword_string
        });
      } else {
        res.render('newsfeed_search.ejs', {
          message: "",
          newsfeedDB: data, username: req.session.username, keywords: keyword_string
        });
      }
    })
  } else {
    message = 'Login before viewing Newsfeed';
    res.redirect('/');
  }

}

var deleteFriend = function (req, res) {
  if (req.session.username) {
    db.deleteFriend(req.session.username, req.params.user, req.params.timestamp, function (err, data) {
      if (err) {
        console.log(err)
        res.sendStatus(500);
      } else {
        res.sendStatus(200);
      }
    });
  }
  else {
    message = 'Login before viewing friends';
    res.redirect('/');
  }
}

var addFriend = function (req, res) {
  if (req.session.username) {
    db.addFriend(req.session.username, req.params.user, function (err, data) {
      if (err) {
        console.log(err)
        res.sendStatus(500);
      } else {
        res.sendStatus(200);
      }
    });
  }
  else {
    message = 'Login before viewing friends';
    res.redirect('/');
  }
}

var searchUsers = function (req, res) {
  console.log('keyword', req.params.term)
  if (req.session.username) {
    db.searchUsers(req.params.term, function (err, data) {
      if (data) {
        res.json(data);
      } else {
        console.err(err);
        res.sendStatus(500);
      }
    });
  }
  else {
    message = 'Login before viewing users';
    res.redirect('/');
  }
}

var chat = function (req, res) {

  if (req.session.username) {
    res.render('chat.ejs');
  } else {
    message = 'Login before viewing Chat';
    res.redirect('/');
  }

}

var chat_socket_io = function (socket) {

  if (socket.handshake.session.username) {
    var username = socket.handshake.session.username
    socket.emit('init', username)
    var invs = null;
    var chats = null;
    db.getAllRoomsInvites(username, function (err, data) {
      chats = data[0]
      invs = data[1]
      if (chats.length != 0) {
        var chat_id = chats[0];
        db.getChatMessages(chat_id, function (err, all_data) {
          if (err) {
            console.log(err)
          } else {
            var sidebar_data = {
              user: username,
              chats: chats,
              invites: invs,
              members: all_data[1],
              currentChat: chats[0],
            }
            var chat_data = all_data[0]
            socket.emit('render_content', [chat_data, sidebar_data]);
          }
        })
      } else if (invs.length != 0) {
        var sidebar_data = {
          user: username,
          chats: chats,
          invites: invs,
          members: null,
          currentChat: null
        }
        socket.emit('render_content', [null, sidebar_data]);
      }
    })

    socket.on("sendMessage", arg => {
      db.addMessage(username, arg.chat, arg.message, function (err, data) {
        if (err) {
          console.log(err)
        }
      });
      socket.emit('renderMessage', arg);
    });

    socket.on("refresh", arg => {
      db.getAllRoomsInvites(username, function (err, data) {
        chats = data[0]
        invs = data[1]
        if (chats.length != 0) {
          db.getChatMessages(arg, function (err, all_data) {
            if (err) {
              console.log(err)
            } else {
              var sidebar_data = {
                user: username,
                chats: chats,
                invites: invs,
                members: all_data[1],
                currentChat: arg
              }
              var chat_data = all_data[0]
              socket.emit('render_content', [chat_data, sidebar_data]);
            }
          })
        } else if (invs.length != 0) {
          var sidebar_data = {
            user: username,
            chats: chats,
            invites: invs,
            members: null,
            currentChat: null
          }
          socket.emit('render_content', [null, sidebar_data]);
        }
      })
    });

    socket.on("addNewChat", arg => {
      db.addChat(username, arg.message, false, function (err, dat) {
        if (err && typeof err != 'number') { console.log(err) }
        else if (err == -1) {
          socket.emit('Error', dat);
        }
        else {
          db.getAllRoomsInvites(username, function (err, data) {
            if (err) { console.log(err) }
            chats = data[0]
            invs = data[1]
            if (chats.length > 0) {
              var chat_id = arg.message
              db.getChatMessages(chat_id, function (err, all_data) {
                var sidebar_data = {
                  user: username,
                  chats: chats,
                  invites: invs,
                  members: all_data[1],
                  currentChat: chat_id
                }
                var chat_data = all_data[0]
                socket.emit('render_content', [chat_data, sidebar_data]);
              })
            }
          })
        }
      })
    })

    socket.on("addExistingChat", arg => {
      db.addChat(username, arg.message, true, function (err, dat) {
        if (err) { console.log(err) }
        db.deleteInvite(username, arg.message, function (err, d) {
          if (err) {
            console.log(err)
          }
          db.getAllRoomsInvites(username, function (err, data) {
            chats = data[0]
            invs = data[1]
            if (chats.length > 0) {
              var chat_id = arg.message
              db.getChatMessages(chat_id, function (err, all_data) {
                var sidebar_data = {
                  user: username,
                  chats: chats,
                  invites: invs,
                  members: all_data[1],
                  currentChat: chat_id
                }
                var chat_data = all_data[0]
                socket.emit('render_content', [chat_data, sidebar_data]);
              })
            }
          })
        })
      })
    })

    socket.on("changeChat", arg => {
      db.getAllRoomsInvites(username, function (err, data) {
        chats = data[0]
        invs = data[1]
        db.getChatMessages(arg, function (err, all_data) {
          if (err) {
          } else {
            var sidebar_data = {
              user: username,
              chats: chats,
              invites: invs,
              members: all_data[1],
              currentChat: arg
            }
            var chat_data = all_data[0]
            socket.emit('renderChangeChat', [chat_data, sidebar_data]);
          }
        })
      })
    })

    socket.on("deleteChat", arg => {
      db.leaveChat(username, arg.message, function (err, data) {
        if (data) {
          db.getAllRoomsInvites(username, function (err, data) {
            chats = data[0]
            invs = data[1]
            if (chats.length > 0) {
              var chat_id = chats[0]
              db.getChatMessages(chat_id, function (err, all_data) {
                var sidebar_data = {
                  user: username,
                  chats: chats,
                  invites: invs,
                  members: all_data[1],
                  currentChat: chat_id
                }
                var chat_data = all_data[0]
                socket.emit('render_content', [chat_data, sidebar_data]);
              })
            } else if (invs.length > 0) {
              var sidebar_data = {
                user: username,
                chats: chats,
                invites: invs,
                members: null,
                currentChat: null
              }
              socket.emit('render_content', [null, sidebar_data]);
            } else {
              var sidebar_data = {
                user: username,
                chats: chats,
                invites: invs,
                members: null,
                currentChat: null
              }
              socket.emit('render_content', [null, sidebar_data]);
            }
          }
          )
        }
      })
    })

    socket.on("sendInvite", arg => {
      var other_user = arg.message
      db.addInvite(other_user, arg.chat, function (err, d) {
        if (err && typeof err != 'number') {
          console.log(err)
        }
        else if (err == -1) {
          socket.emit('Error', d)
        }
      })
    })

    socket.on("deleteInvite", arg => {
      db.deleteInvite(username, arg.message, function (err, data) {
        if (err) { console.log(err) }
        if (data) {
          db.getAllRoomsInvites(username, function (err, data2) {
            chats = data2[0]
            invs = data2[1]
            if (chats.length > 0) {
              db.getChatMessages(arg.chat, function (err, all_data) {
                var sidebar_data = {
                  user: username,
                  chats: chats,
                  invites: invs,
                  members: all_data[1],
                  currentChat: arg.chat
                }
                var chat_data = all_data[0]
                socket.emit('render_content', [chat_data, sidebar_data]);
              })
            } else if (invs.length > 0) {
              var sidebar_data = {
                user: username,
                chats: chats,
                invites: invs,
                members: null,
                currentChat: null
              }
              socket.emit('render_content', [null, sidebar_data]);
            } else {
              var sidebar_data = {
                user: username,
                chats: chats,
                invites: invs,
                members: null,
                currentChat: null
              }
              socket.emit('render_content', [null, sidebar_data]);
            }
          })
        }
      })
    })
  }

}

//add a news like to news_likes
var addNewsLike = function (req, res) {
  if (req.session.username) {
    db.addNewsLike(req.params.username, req.params.link, function (err, data) {
      if (err) {
        console.log(err)
      } else {
        res.sendStatus(200);
      }
    });
  }
  else {
    res.redirect('/');
  }
}

//delete a news like from news_likes
var deleteNewsLike = function (req, res) {
  if (req.session.username) {
    db.deleteNewsLike(req.params.username, req.params.link, function (err, data) {
      if (err) {
        console.log(err)
        res.sendStatus(500);
      } else {
        res.sendStatus(200);
      }
    });
  }
  else {
    res.redirect('/');
  }
}


var routes = {
  get_main: getMain,
  get_login: getLogin,
  get_signup: getSignup,
  check_login: checkLogin,
  create_account: createAccount,
  get_logout: getLogout,
  get_home: getHome,
  get_newsfeed: getNewsfeed,
  get_friends: getFriends,
  get_friends_update: getFriendsUpdate,
  get_friends_visualizer: getFriendsVisualizer,
  get_friends_visualizer_expanded: getFriendsVisualizerExpanded,
  delete_friend: deleteFriend,
  add_friend: addFriend,
  search_newsfeed: searchNewsFeed,
  search_users: searchUsers,
  get_account_settings: getAccountSettings,
  post_settings: postSettings,
  change_password: changePassword,
  get_wall: getWall,
  post_post: postPost,
  get_posts_ajax: getPosts,
  get_comments: getComments,
  post_comment: postComment,
  chat: chat,
  chat_socket_io: chat_socket_io,
  get_friends_posts: getFriendsPosts,
  add_news_like: addNewsLike,
  delete_news_like: deleteNewsLike
};

module.exports = routes;
