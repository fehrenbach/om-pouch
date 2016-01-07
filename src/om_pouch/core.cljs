(ns om-pouch.core
  (:require [goog.dom :as gdom]
            [goog.object :as gobj]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            cljsjs.pouchdb))

(enable-console-print!)

(defonce db (js/PouchDB. "om-pouch"))

(defonce app-state
  (atom {:window/size [1920 1200]}))

(declare reconciler)

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


(defn postprocess-doc [doc]
  (.log js/console "postprocess " (str doc))
  (-> doc
      (update :married-to (partial mapv (partial vector :pouch/by-id)))))

;; TODO should probably use a channel, deduplicate and group for bulk fetching
(defn fetch-doc! [id]
  (let [ids [id]]
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

(defmulti read om/dispatch)

(defmethod read :window/size
  [{:keys [state]} key params]
  {:value (:window/size @state)})

(defmethod read :pouch/by-id
  [{:keys [ast parser state query] :as env} key params]
  ;; (.log js/console "read " (str (:key ast)))
  (if-let [doc (get-in @state (:key ast))]
    (let [;; _ (.log js/console "found " (str (:key ast)) " in state, making recursive call")
          value (parser (assoc env :context doc) query)]
      (do
        ;; (.log js/console "recursive call value: " (str value))
        {:value value}))
    (let [;; _ (.log js/console "did not find " (str (:key ast)) " in state, returning :loading and firing off external load")
          _ (fetch-doc! (second (:key ast)))]
      {:value :loading})))

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

(def parser
  (om/parser {:read read}))

(def reconciler
  (om/reconciler {:state app-state
                  :parser parser}))

(def query [:window/size
            {[:pouch/by-id "3"] [:name {:married-to [:name {:married-to [:name]}]}]}
            {[:pouch/by-id "4"] [:name {:married-to [:name {:married-to [:name]}]}]}
            {[:pouch/by-id "6"] [:name {:married-to [:name {:married-to [:name]}]}]}
            ])

(defui RootView
  static om/IQuery
  (query [this]
         query)

  Object
  (render [this]
          (let [props (om/props this)
                three (get props [:pouch/by-id "3"])
                four (get props [:pouch/by-id "4"])
                six (get props [:pouch/by-id "6"])
                ]
            (dom/div nil
                     (dom/p nil "three" (str three))
                     (dom/p nil "four" (str four))
                     (dom/p nil "six" (str six))
                     ))))

(comment
  ;; ID not actually unique..
  ({[:pouch/view "harvestsByFooBar"] [:foo :bar]}
   {:startkey "abc"
    :reduce true})
  
  ({:view/currentHives [:abc]} {:startkey "yard"})
  ;; So how would I do
  ;; YardName (current occupation / max hives)
  ;; e.g.  - Locherhof        19/20
  ;;       - Unterwaldhausen  22/24
  ({:view/hivesAtYard [{:yard [:name :config]} :max]}
   {:grouplevel 1
    :reduce true})
  ;; No good, because it requires postprocessing

  ;; How about custom code for reading views?
  ;; Problem: asynchronicity
  ;; Solution: Cache custom view result(s) in a :ephemeral/name toplevel key. Clear on database change.

  ;; Meh.. not sure.

  ;; User provides some arbitrary name.
  ;; We store and retrieve view results keyed by a hash of their parameters.

  [({:view/hivesAtYard_sub [{:yard [:name :config]}]}
    {:view "hivesAtYard"
     :grouplevel 1
     :reduce true})]
  ;; Still need custom postprocessing, but we need that for docs, too.
  )

(om/add-root! reconciler
              RootView (gdom/getElement "app"))
