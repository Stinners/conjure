(module conjure.client.scala.stdio
  {autoload {a conjure.aniseed.core
             extract conjure.extract
             str conjure.aniseed.string
             nvim conjure.aniseed.nvim
             stdio conjure.remote.stdio
             config conjure.config
             text conjure.text
             mapping conjure.mapping
             client conjure.client
             log conjure.log
             ts conjure.tree-sitter}
   require-macros [conjure.macros]})


(config.merge
  {:client
   {:scala
    {:stdio
     {:command "scala --color never"
      :prompt_pattern "scala> "}}}})

;; I don't understand what this does
(when (config.get-in [:mapping :enable_defaults])
  (config.merge
    {:client
     {:sql
      {:stdio
       {:mapping {:start "cs"
                  :stop "cS"
                  :interrupt "ei"}}}}}))

(def- cfg (config.get-in-fn [:client :scala :stdio]))

(defonce state (client.new-state #(do {:repl nil})))

(def buf-suffix ".scala")
(def comment-prefix "// ")

;; See https://github.com/tree-sitter/tree-sitter-scala/blob/master/src/node-types.json
(defn form-node? [node]
  (or (= "_definition" (node:type))
      (= "expression" (node:type))
      (= "assignment_expression" (node:type))))

(defn comment-node? [node]
  (or (= "comment" (node:type))))


(defn- with-repl-or-warn [f opts]
  (let [repl (state :repl)]
    (if repl
      (f repl)
      (log.append [(.. comment-prefix "No REPL running")]))))


(defn- format-message [msg]
  (str.split (or msg.out msg.err) "\n"))

(defn- remove-blank-lines [msg]
  (->> (format-message msg)
       (a.filter #(not (= "" $1)))))

(defn- display-result [msg]
  (log.append (remove-blank-lines msg)))

(defn ->list [s]
  (if (a.first s)
    s
    [s]))

(defn eval-str [opts]
  (with-repl-or-warn
    (fn [repl]
      (repl.send
        (.. opts.code "\n")
        (fn [msgs]
          (let [msgs (->list msgs)]
            (when opts.on-result
              (opts.on-result (str.join "\n" (remove-blank-lines (a.last msgs)))))
            (a.run! display-result msgs)))
          
        {:batch? false}))))


(defn eval-file [opts]
  (eval-str (a.assoc opts :code (a.slurp opts.file-path))))


(defn interrupt []
  (with-repl-or-warn
    (fn [repl]
      (log.append [(.. comment-prefix " Sending interrupt signal.")] {:break? true})
      (repl.send-signal vim.loop.constants.SIGINT))))

(defn- display-repl-status [status]
  (let [repl (state :repl)]
    (when repl
      (log.append
        [(.. comment-prefix (a.pr-str (a.get-in repl [:opts :cmd])) " (" status ")")]
        {:break? true}))))

(defn stop []
  (let [repl (state :repl)]
    (when repl
      (repl.destroy)
      (display-repl-status :stopped)
      (a.assoc (state) :repl nil))))

(defn start []
  (log.append [(.. comment-prefix "start [] ")])
  (if (state :repl)
    (log.append [(.. comment-prefix "Can't start, REPL is already running.")
                 (.. comment-prefix "Stop the REPL with "
                     (config.get-in [:mapping :prefix])
                     (cfg [:mapping :stop]))]
                {:break? true})
    (do
      (a.assoc
        (state) :repl
        (stdio.start
          {:prompt-pattern (cfg [:prompt_pattern])
           :cmd (cfg [:command])

           :on-success
           (fn []
             (display-repl-status :started))

           :on-error
           (fn [err]
             (display-repl-status err))

           :on-exit
           (fn [code signal]
             (when (and (= :number (type code)) (> code 0))
               (log.append [(.. comment-prefix "process exited with code " code)]))
             (when (and (= :number (type signal)) (> signal 0))
               (log.append [(.. comment-prefix "process exited with signal " signal)]))
             (stop))

           :on-stray-output
           (fn [msg]
             (display-result msg))
           })))))

(defn on-load []
  (when (config.get-in [:client_on_load])
    (start)))

(defn on-exit []
  (stop))

(defn on-filetype [] {})
