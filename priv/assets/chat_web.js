$(function() {
  if(!("WebSocket" in window)) {
    $("#message-window").append("<h1>WebSockets are required to use the chat</h1>");
  }

  var conn, nick, pass, current_room;
  var rooms = [];
  var badlogin = true;

  function connect() {
    if(badlogin) {
      nick = $("#nick").val();
      pass = $("#password").val();
    }
    $("#password").val("");
    $("#creds").hide();

    info_message("Trying to connect...");
    conn = new WebSocket("ws://" + window.location.host + "/chat");
    conn.onopen = function(e) {
      conn.send(JSON.stringify({nick: nick, password: pass}));
    };
    conn.onclose = function(e) {
      error_message("Disconected from the server");
      if (! badlogin) {
        setTimeout(function() { connect(); }, 2000);
      }
    };
    conn.onmessage = function(e) {
      var m = JSON.parse(e.data);
      switch(m.action) {
        case "log_on":
          badlogin = false;
          update_rooms(m.rooms);
          $("#creds").hide();
          info_message("Successfully logged on");
          info_message('Type "/join <room>" to start chatting');
          $("#text-input").focus();
          break;
        case "badauth":
          error_message("That nickname is already in use with a different password");
          badlogin = true;
          $("#creds").show();
          break;
        case "error":
          error_message(m.what);
          break;
        case "chat":
          chat_message(m);
          break;
        case "join":
          update_rooms(m.rooms);
          join_message(m);
          break;
        case "part":
          update_rooms(m.rooms);
          part_message(m);
          break;
      }
    };
    conn.error = function(e) {
      error_message(e.toString());
    };
  }

  function send_chat() {
    var s = $("#text-input").val();
    conn.send(JSON.stringify({input: s}));
    $("#text-input").val("");
  }

  function chat_message(m) {
    if (m.room == current_room) {
      $('<div class="message chat"></div>').text("<" + m.what[0] + "> " + m.what[1]).appendTo("#message-window");
    }
    else {
      $('<div class="message chat bg-channel"></div>').
        text("(" + m.room + ")<" + m.what[0] + "> " + m.what[1]).appendTo("#message-window");
    }
    $("#message-window > .message").last()[0].scrollIntoView();
  }
  function info_message(s) {
    $('<div class="message info"></div>').text(s).appendTo("#message-window");
    $("#message-window > .message").last()[0].scrollIntoView();
  }
  function error_message(s) {
    $('<div class="message info error"></div>').text("Error: " + s).appendTo("#message-window");
    $("#message-window > .message").last()[0].scrollIntoView();
  }
  function join_message(m) {
    info_message("Joins " + m.room + ": " + m.what);
  }
  function part_message(m) {
    if(m.what == nick) {
      info_message("Left " + m.room);
    }
    else {
      info_message("Departs from " + m.room + ": " + m.what);
    }
  }

  function update_rooms(rs) {
    current_room = rs[0];
    rooms = rs.sort();
    $("#current").text("Current room: " + current_room);
    $("#rooms").text("All rooms: " + rooms.join(", "));
  }

  $(document).on("keypress", "textarea#text-input", function(e) {
      var code = e.which;
      if (code === 13 || code === 10) {
        e.preventDefault();
        send_chat();
      }
  });

  $("#creds").on("submit", function(e) {
    e.preventDefault();
    connect();
  });
});
