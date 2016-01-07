(ns om-pouch.core
  (:require [goog.dom :as gdom]
            [goog.object :as gobj]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            cljsjs.pouchdb))

(enable-console-print!)

(defonce db (js/PouchDB. "om-pouch"))

(defonce app-state (atom {}))

(comment
  (.put db (clj->js {:_id "1" :type "person"
                     :name "Sally" :age 22
                     :married-to ["6"]}))

  (.put db (clj->js {:_id "2" :type "person"
                     :name "Bob" :age 33
                     :married-to ["3" "4"]}))

  (.put db (clj->js {:_id "3" :type "person"
                     :name "Jim" :age 32
                     :married-to ["2" "4"]}))

  (.put db (clj->js {:_id "4" :type "person"
                     :name "Robert" :age 28
                     :married-to ["2" "3"]}))

  (.put db (clj->js {:_id "5" :type "person"
                     :name "Rey" :age 23
                     :married-to []}))

  (.put db (clj->js {:_id "6" :type "person"
                     :name "Ahmad" :age 27
                     :married-to ["1"]}))
  )

(defmulti read om/dispatch)

(defmethod read :pouch/by-id
  [{:keys [ast parser state query] :as env} key params]
  (.log js/console "read " (str (:key ast)))
  (if-let [doc (get-in @state (:key ast))]
    (let [_ (.log js/console "found " (str (:key ast)) " in state, making recursive call")
          pouch (parser (assoc env :context doc) query :pouch)
          value (parser (assoc env :context doc) query)]
      (do
        (.log js/console "recursive call value: " (str value))
        (.log js/console "recursive call pouch: " (str pouch))
        {:value value}))
    (let [_ (.log js/console "did not find " (str (:key ast)) " in state, returning :loading and :pouch query " (str (first (:children (om/query->ast [(:key ast)])))))]
      {:value :loading
       ;; At some point we have to flatten the query to only retrieve docs.
       ;; Not sure this is where we do it.
       :pouch (first (:children (om/query->ast [(:key ast)])))})))

(defmethod read :default
  [{:keys [ast context parser query] :as env} key params]
  (case (:type ast)
    :prop
    {:value (get context key)}

    :join
    (let [join-key (get context (:key ast))
          ;; Om does not distinguish 1:1 1:N N:1 N:M joins
          ;; We don't really want to call `the` all the time, so we try to autodetect it.
          ;; BUG Unfortunately, this use of om/ident? is not safe (we apply it to data from the state, not a query.
          ;; TODO Make this configurable (pass a parameter to the join?) and possibly remove autodetection / warn.
          ;; one-to vs many-to
          join-keys (if (om/ident? join-key) [join-key] join-key)
          result (mapv (fn [join-key]
                         ;; TODO What should the context be in env?
                         (second (first (parser env [{join-key query}]))))
                       join-keys)
          ;; to-one vs to-many
          result (if (= :pouch/by-id (first join-key)) (nth result 0) result)]
      {:value result})))

(defn postprocess-doc [doc]
  (-> doc
      (update :married-to (partial mapv (partial vector :pouch/by-id)))))

(declare reconciler)

(defn send
  [{:keys [pouch] :as remote-queries} cb]
  (.log js/console "send query:" (str pouch))
  ;; We might need to do some normalization or something, split doc fetches from view fetches or whatnot.
  (let [ids (map second pouch)]
    (.log js/console "fetch docs: " (str ids))
    (.allDocs
     db
     (clj->js {:keys ids
               :include_docs true})
     (fn [err res]
       (if err
         (.alert js/window err)
         (let [rows (gobj/get res "rows")
               ;; _ (.log js/console "rows: " rows)
               pdocs (map #(postprocess-doc (js->clj (gobj/get % "doc") :keywordize-keys true))
                          rows)
               ;; _ (.log js/console "postprocessed docs: " (str pdocs))
               ]
           ;; this overrides all existing docs :/
           ;; (cb {:pouch/by-id (zipmap ids pdocs)})

           ;; TODO merge order? deeper merge?
           (om/merge! reconciler {:pouch/by-id (merge (:pouch/by-id @app-state)
                                                      (zipmap ids pdocs))})
           ;; BUG shit doesn't rerender
           ;; om/merge! docstring says: "Affected components managed by the reconciler will re-render."
           ;; Doesn't seem to work.
           (om/force-root-render! reconciler)))))))

(def parser
  (om/parser {:read read}))

(def reconciler
  (om/reconciler {:state app-state
                  :parser parser
                  :send send
                  :remotes [:remote :pouch]}))

(def query [{[:pouch/by-id "6"] [:name {:married-to [:name {:married-to [:name]}]}]}])

(defui RootView
  static om/IQuery
  (query [this]
         query)

  Object
  (render [this]
          (let [props (om/props this)
                six (get props [:pouch/by-id "6"])]
            (dom/p nil (str six)))))

(om/add-root! reconciler
              RootView (gdom/getElement "app"))
