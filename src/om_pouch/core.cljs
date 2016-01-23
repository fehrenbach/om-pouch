(ns om-pouch.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [goog.dom :as gdom]
            [goog.object :as gobj]
            [cljs.core.async :refer [<! >! put! chan alts! timeout]]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            cljsjs.pouchdb))

(enable-console-print!)

(defonce db (js/PouchDB. "om-pouch"))

(defonce app-state
  (atom {}))

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

  (def married-to-map "function (doc) {
  if (doc.type === \"person\") {
    doc[\"married-to\"].forEach(function (id) {
      emit(doc._id, id);
    });
  }
  }")

  (def married-to-ddoc #js {:_id "_design/married-to"
                            :views #js {"married-to"
                                        #js {:map married-to-map}}})

  ;; (.put db married-to-ddoc alert-cb)

  ;; (.query db "married-to/married-to" #js {:include_docs true} alert-cb)
  )


(defn postprocess-doc [doc]
  (-> doc
      (update :married-to (partial mapv (partial vector :pouch/by-id)))))

(defn postprocess-view [params rows]
  (case (:view params)
    "married-to/married-to"
    (mapv (fn [row] [:pouch/by-id (gobj/get row "value")]) rows)))


(def +fetch-timeout+ 100)

(def db-request-chan (chan))
(def db-return-chan (chan))

;; We avoid duplicated fetches by keeping track of what is being fetched.
;; We also collect document (not view) fetches to issue a bulk (.allDocs ...)
;; request, because we potentially issue many doc fetches from a single call to
;; the parser (in many calls to read).
;; TODO Get rid of the timeout, and instead detect the return of the parser somehow.
(go-loop [bulk-timeout (chan)
          bulk-docs #{}
          in-transit #{}]
  (let [[value channel] (alts! [db-request-chan bulk-timeout db-return-chan])]
    (.log js/console "db loop" (str value) (str bulk-docs) (str in-transit))
    (condp = channel
      db-request-chan
      (if (contains? in-transit value)
        (recur bulk-timeout bulk-docs in-transit)
        (case (nth value 0)
          :doc
          (if (contains? bulk-docs value)
            (recur bulk-timeout bulk-docs in-transit)
            (if (empty? bulk-docs)
              (recur (timeout +fetch-timeout+) (conj bulk-docs value) in-transit)
              (recur bulk-timeout (conj bulk-docs value) in-transit)))

          :view
          (let [[_ h params] value]
            (.query db
                    (:view params)
                    (clj->js params)
                    (fn [err res]
                      (if err
                        (.alert js/window err)
                        (let [rows (gobj/get res "rows")
                              prows (postprocess-view params rows)]
                          (om/merge! reconciler {:view-result/by-hash (merge (:view-result/by-hash @app-state)
                                                                             {h prows})})
                          ;; This is a bit finicky. We put #{value} because we remove with coljure.set/difference.
                          (put! db-return-chan #{value})))))
            (recur bulk-timeout bulk-docs (conj in-transit value)))))

      bulk-timeout
      (let [ids (map #(nth % 1) bulk-docs)]
        (.allDocs db
                  (clj->js {:keys ids :include_docs true})
                  (fn [err res]
                    (if err
                      (.alert js/window err)
                      (let [rows (gobj/get res "rows")
                            pdocs (map #(postprocess-doc (js->clj (gobj/get % "doc") :keywordize-keys true))
                                       rows)]
                        ;; TODO merge order? deeper merge?
                        (om/merge! reconciler {:pouch/by-id (merge (:pouch/by-id @app-state)
                                                                   (zipmap ids pdocs))})
                        (put! db-return-chan bulk-docs)))))
        (recur (chan) #{} (into in-transit bulk-docs)))

      db-return-chan
      (do
        ;; BUG shit doesn't rerender
        ;; om/merge! docstring says: "Affected components managed by the reconciler will re-render."
        ;; Doesn't seem to work though, workaround: force render
        (js/setTimeout #(om/force-root-render! reconciler) 0)
        (recur bulk-timeout bulk-docs (clojure.set/difference in-transit value))))))

(defn fetch-doc! [id]
  (put! db-request-chan [:doc id]))

(defn fetch-view! [h params]
  (put! db-request-chan [:view h params]))

(defmulti read om/dispatch)

(defmethod read :friends
  [{:keys [parser query] :as env} _ _]
  (let [friendlist (mapv (partial vector :pouch/by-id) (map str (range 1 7)))
        parsed (mapv (fn [friend] (second (first (parser env [{friend query}])))) friendlist)]
    {:value parsed}))

(defmethod read :pouch/by-id
  [{:keys [ast parser state query] :as env} key params]
  (if-let [doc (get-in @state (:key ast))]
    {:value (parser (assoc env :context doc) query)}
    (do
      (fetch-doc! (second (:key ast)))
      {:value :loading})))

(defn read-view
  [{:keys [parser state query] :as env} key params]
  (let [h (hash params)]
    (if-let [fetched (get-in @state [:view-result/by-hash h])]
      ;; TODO This kinda assumes that the view-result is a vector, which might not be true for reductions or in general...
      ;; TODO The {ref query} subquery assumes that ref is an ident. I think we can assume it's either a map OR an ident. TODO handle the plain data map case
      (let [recursion (mapv (fn [ref] (second (first (parser env [{ref query}])))) fetched)]
        {:value recursion})
      (do
        (fetch-view! h params)
        {:value :loading}))))

(defmethod read :default
  [{:keys [ast context parser query] :as env} key params]
  (if (= "view" (namespace key))
    (read-view env key params)
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
        {:value result}))))

(def parser
  (om/parser {:read read}))

(def reconciler
  (om/reconciler {:state app-state
                  :parser parser}))

(def query '[{[:pouch/by-id "3"] [:name {:married-to [:name {:married-to [:name]}]}]}
             {[:pouch/by-id "4"] [:name {:married-to [:name {:married-to [:name]}]}]}
             {[:pouch/by-id "6"] [:name {:married-to [:name {:married-to [:name]}]}]}
             {:friends [:name]}
             ({:view/marriedToBob [:name]} {:view "married-to/married-to"
                                            :startkey "2"
                                            :endkey "2"})])


(defui RootView
  static om/IQuery
  (query [this]
         query)

  Object
  (render [this]
          (let [props (om/props this)
                friends (get props :friends)
                three (get props [:pouch/by-id "3"])
                four (get props [:pouch/by-id "4"])
                six (get props [:pouch/by-id "6"])
                mtb (get props :view/marriedToBob)
                ]
            (dom/div nil
                     (dom/p nil "friends" (str friends))
                     (dom/p nil "mtb" (str mtb))
                     (dom/p nil "three" (str three))
                     (dom/p nil "four" (str four))
                     (dom/p nil "six" (str six))
                     ))))

(om/add-root! reconciler
              RootView (gdom/getElement "app"))
