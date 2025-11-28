const TASK_SOCKET_SOURCE = "task-socket";
const WS_URL = `${self.location.origin.replace(/^http/, "ws")}/tasks/ws`;

let socket = null;
let reconnectDelay = 2000;
let connecting = false;

function backoffReconnect() {
  reconnectDelay = Math.min(reconnectDelay * 1.5, 15000);
  setTimeout(connectSocket, reconnectDelay);
}

async function broadcast(event) {
  const clients = await self.clients.matchAll({ type: "window", includeUncontrolled: true });
  clients.forEach((client) => {
    client.postMessage({ source: TASK_SOCKET_SOURCE, event });
  });
}

function handleSocketMessage(event) {
  const raw = event.data;
  let parsed = raw;
  if (typeof raw === "string") {
    try {
      parsed = JSON.parse(raw);
    } catch (_) {
      /* ignore parse errors, forward raw */
    }
  }
  void broadcast(parsed);
}

function connectSocket() {
  if (socket || connecting) return;
  connecting = true;
  try {
    socket = new WebSocket(WS_URL);
  } catch (err) {
    connecting = false;
    backoffReconnect();
    return;
  }
  socket.addEventListener("open", () => {
    connecting = false;
    reconnectDelay = 2000;
    void broadcast({ type: "connected" });
  });
  socket.addEventListener("message", handleSocketMessage);
  socket.addEventListener("close", () => {
    socket = null;
    connecting = false;
    backoffReconnect();
  });
  socket.addEventListener("error", () => {
    if (socket) {
      socket.close();
    }
  });
}

self.addEventListener("message", (event) => {
  const data = event.data || {};
  if (data.type === "task-socket-subscribe") {
    connectSocket();
  }
});

self.addEventListener("install", (event) => {
  event.waitUntil(self.skipWaiting());
});

self.addEventListener("activate", (event) => {
  event.waitUntil(self.clients.claim());
});
