const VERSION = "fantasy-hub-v2";

self.addEventListener("install", () => {
  self.skipWaiting();
});

self.addEventListener("activate", event => {
  event.waitUntil(self.clients.claim());
});

// Shiny requires a live server connection, so requests remain network only.
self.addEventListener("fetch", event => {
  if (event.request.method === "GET") {
    event.respondWith(fetch(event.request));
  }
});
