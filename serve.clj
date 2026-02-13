(ns serve
  (:require [babashka.process :as p]
            [org.httpkit.server :as server]))

(defn listen-on-websocket [port]
  (p/process (str "websocat --buffer-size 99999999 --text ws-l:127.0.0.1:" port " broadcast:mirror: --exit-on-eof"))
  (fn [payload]
    @(p/process (str "websocat ws://localhost:" port "/ws --buffer-size 9999999") {:in payload})))

(defn serve [handler port]
  (server/run-server handler {:port port}))

(defn serve-channels
  ([handler port channels]
   (serve-channels handler port channels {}))
  ([handler port channels {:keys [on-open on-receive on-close]
                           :or {on-open (fn [_])
                                on-receive (fn [_ _])
                                on-close (fn [_ _])}}]
   (server/run-server
     (fn [{:keys [websocket?] :as req}]
       (if websocket?
         (server/as-channel req {:on-open
                                 , (fn [ch]
                                     (swap! channels conj ch)
                                     (on-open ch))
                                 :on-receive
                                 , (fn on-receive [ch msg]
                                     (println "Received data on" ch ":" (pr-str msg))
                                     (on-receive ch msg))
                                 :on-close
                                 , (fn [ch x]
                                     (swap! channels disj ch)
                                     (on-close ch x))})
         (handler req)))
     {:port port})))

(defn listen-and [js]
  (str "
function listenOnWebsocket() {
  const listen = () => {
    const conn = new WebSocket(`ws://${window.location.host}${window.location.pathname}`);
    conn.onopen = () => console.log(`Socket connected to server`);
    conn.onclose = () => {
      console.warn(`Socket to server closed; retrying...`);
      setTimeout(listen, 3000);
    };
    conn.onmessage = (event) => {
      " js "
    }
  }
  listen();
}
listenOnWebsocket();
"))
