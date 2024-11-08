(ns tmux
  (:refer-clojure :exclude [send filter])
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [babashka.process :as p]))

; helper functions

(defn id
  ([session window-index-or-name]
   (str session \: window-index-or-name))
  ([session window-index-or-name pane-index]
   (str session \: window-index-or-name \. pane-index)))

; shell out to `tmux` command

(defn command [cmd args]
  (into ["tmux" (name cmd)]
        (map (fn [arg]
               (if (keyword? arg)
                 (str \- (name arg))
                 arg)))
        args))

(defn tmux* [opts cmd & args]
  (apply p/shell opts (command cmd args)))

(defn tmux [cmd & args]
  (let [{:keys [out]} (apply tmux* {:out :string} cmd args)]
    (when-not (str/blank? out)
      (str/trim out))))

(defn split-horizontal []
  (tmux :split-window :h))

(defn send [target & ss]
  (apply tmux :send :t target ss))

(defn submit [target & ss]
  (apply tmux :send :t target (concat ss ["C-m"])))

(defn new-window [target & opts]
  (apply tmux :new-window :t target opts))

(defn rename-window [target nm]
  (tmux :rename-window :t target nm))

(defn kill-window [target]
  (tmux :kill-window :t target))

(defn link-window [target]
  (tmux :link-window :s target))

(defn unlink-window [target]
  (tmux :unlink-window :t target))

(defn switch-client [target]
  (tmux :switch-client :t target))

(defn has-target? [t]
  (-> (tmux* {:err nil, :continue true} :has-session :t t)
      :exit zero?))

(defn session-name []
  (str/trim (tmux :display-message :p "#{session_name}")))

(defn tmux-lines [cmd & args]
  (some-> (apply tmux cmd args) str/trim str/split-lines))

(defn pane-indices [target]
  (tmux-lines :list-panes :t target :F "#{pane_index}"))

(defn pane-contents [pane & {:keys [start end] :or {start "-" end "-"}}]
  (apply tmux-lines
         (cond-> [:capture-pane :pJ :t pane]
           start (conj :S (str start))
           end (conj :E (str end)))))

(defn last-command-lines [pane]
  (-> (pane-contents pane :start "-" :end "-")
      reverse
      (->> (drop-while (partial re-find #"^§")))
      (->> (take-while (complement (partial re-find #"^§"))))
      reverse))

(defn last-command-output [pane]
  (str/join "\n" (last-command-lines pane)))

(defn last-line [pane]
  (last (last-command-lines pane)))

(defn sessions-with-windows-named [nm]
  (set (tmux-lines :list-panes :aF "#{session_name}" :f (str "#{m:" nm ",#{window_name}}"))))

(defn copy-mode? [target]
  (let [pane-index (some-> (re-find #"\.(\d+)$" target) second Integer/parseInt)]
    (-> (tmux-lines :list-panes :t target :F "#{pane_in_mode}")
        (nth pane-index)
        (= "1"))))

(defn ensure-not-copy-mode [target]
  ; https://stackoverflow.com/a/50392365
  ; Don't use `send` here because the pane might not be in a mode that can be
  ; canceled, in which case any errors should be ignored.
  (tmux* {:err nil, :continue true} :send :t target :X "cancel"))

(defn window-zoomed? [target]
  ; https://stackoverflow.com/a/38780608
  (-> (tmux-lines :list-panes :t target :F "#F")
      (->> (some (partial re-find #"Z")))
      boolean))

(defn ensure-not-zoomed [target]
  ; https://stackoverflow.com/a/38780608
  (when (window-zoomed? target)
    (tmux-lines :resize-pane :t (doto target prn) :Z)))

(defn filter [form]
  (walk/postwalk
    (fn [node]
      (cond
        (keyword? node)
        , (str "#{" (str/replace (name node) #"-" "_") "}")
        (seq? node)
        , (let [[op a b] node]
            (condp = (symbol (name op)) ; drop namespaces
              'and (str "#{&&:" a "," b "}")
              'or (str "#{||:" a "," b "}")
              'match? (str "#{m:" a "," b "}")
              '= (str "#{=:" a "," b "}")
              'not= (str "#{!=:" a "," b "}")))
        :else
        , node))
    form))
