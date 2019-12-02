var Socket = require('fast-tcp').Socket;

var socket = new Socket({
    host: 'localhost',
    port: 5000,
    reconnect: true, // (true by default)
    reconnectInterval: 2000 // (1000ms by default)
});

let name = "xilo";

socket.emit('user/login', name);

socket.emit('lobby/create', {
    creator: name,
    maze: "011001010"
});

socket.emit('user/up', name, response => {
    console.log('me : ' + JSON.stringify(response));
});