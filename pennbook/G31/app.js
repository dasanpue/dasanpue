/* Some initialization boilerplate. Also, we include the code from
   routes/routes.js, so we can have access to the routes. Note that
   we get back the object that is defined at the end of routes.js,
   and that we use the fields of that object (e.g., routes.get_main)
   to access the routes. */

var express = require('express');
var bodyParser = require('body-parser')
var morgan = require('morgan')
var cookieParser = require('cookie-parser')
var sessions = require('express-session')
var serveStatic = require('serve-static')
var path = require('path')
var routes = require('./routes/routes.js');
var app = express();
const socketSession = require("express-socket.io-session");
const http = require('http');
const server = http.createServer(app);
const { Server } = require("socket.io");
const io = new Server(server);
var session = sessions({
   secret: 'nets2120',
   resave: false,
   saveUninitialized: true,
   cookie: {}
});
io.use(socketSession(session, {
   autoSave: true
}));
io.on('connection', routes.chat_socket_io);

app.use(session);
app.use(express.urlencoded({ extended: false }));
app.use(bodyParser.urlencoded({ extended: false }));
app.use(serveStatic(path.join(__dirname, 'public')))
app.use(bodyParser.json())

//run news adsoprtion algo every hour
//'mvn compile' in news_algo before running
function news_algo() {
   console.log("About to run spark AWS EMR job!")
   var exec = require('child_process').exec;
   exec('cd news_algo && mvn exec:java@livy');
}
setInterval(news_algo, 180000);

/* Below we install the routes. The first argument is the URL that we
   are routing, and the second argument is the handler function that
   should be invoked when someone opens that URL. Note the difference
   between app.get and app.post; normal web requests are GETs, but
   POST is often used when submitting web forms ('method="post"'). */

app.get('/', routes.get_main);
app.get('/login', routes.get_login);
app.get('/signup', routes.get_signup);
app.get('/home', routes.get_home);
app.get('/logout', routes.get_logout);
app.get('/newsfeed', routes.get_newsfeed);
app.get('/friends', routes.get_friends);
app.get('/friendsupdate', routes.get_friends_update);
app.get('/account', routes.get_account_settings);
app.get('/wall/:user?', routes.get_wall);
app.post('/checklogin', routes.check_login);
app.post('/createaccount', routes.create_account);
app.get('/searchnewsfeed', routes.search_newsfeed);
app.get('/friendvisualization', routes.get_friends_visualizer);
app.get('/getFriends/:user', routes.get_friends_visualizer_expanded);
app.get('/searchusers/:term', routes.search_users);

app.delete('/deletefriend/:user/:timestamp', routes.delete_friend);
app.post('/addfriend/:user', routes.add_friend);

app.post('/postsettings', routes.post_settings);
app.post('/changepassword', routes.change_password);
app.post('/postpost', routes.post_post);
app.get('/getposts/:user?', routes.get_posts_ajax);
app.get('/getcomments', routes.get_comments);
app.post('/postcomment', routes.post_comment);
app.get('/getfriendsposts', routes.get_friends_posts);

app.get('/chat', routes.chat);

app.post('/addnewslikes/:username/:link', routes.add_news_like);
app.post('/deletenewslikes/:username/:link', routes.delete_news_like);

/* Run the server */
server.listen(8080);
console.log('Server running on port 8080. Now open http://localhost:8080/ in your browser!');