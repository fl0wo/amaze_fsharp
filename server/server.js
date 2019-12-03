var Server = require('fast-tcp').Server;
var Socket = require('fast-tcp').Socket;

var server = new Server();

let users = {};

let lobbys = {};

function printAll() {
    console.log(JSON.stringify(users) + " : " + JSON.stringify(lobbys));
}

server.on('connection', function (socket) {

    console.log("pazzesco")

    socket.on('user/login', function (username) {
        console.log(username + ' joined the server ');

        if (!users.username) {
            users[username] = {
                name: username,
                x: 0,
                y: 0,
                color: 0
            };
        }

        printAll()
    });

    socket.on('lobby/name', (name, callback) => {
        callback(lobbys[name]);
    });

    socket.on('lobby/all', (callback) => {
        callback(lobbys);
    });

    socket.on('lobby/create', function (data) {
        lobbys["lobbyof" + data.creator] = {
            name: "lobbyof" + data.creator,
            players: [data.creator],
            maze: data.maze
        };
        printAll()

    });

    socket.on('lobby/join', function (data, callback) {
        lobbys[data.lobbyname].players.push(data.user);
        printAll()

    });

    socket.on('user/up', function (username, callback) {
        console.log(username + " goUp");

        users[username].y--;
        printAll()

        callback(users[username]);
    });

    socket.on('user/down', function (username, callback) {
        console.log(username + " goDown");

        users[username].y++;
        printAll()

        callback(users[username]);
    });

    socket.on('user/left', function (username, callback) {
        console.log(username + " goLeft");

        users[username].x--;
        printAll()

        callback(users[username]);
    });

    socket.on('user/right', function (username, callback) {
        console.log(username + " goRight");

        users[username].x++;
        printAll()

        callback(users[username]);
    });
});

server.listen(5000);