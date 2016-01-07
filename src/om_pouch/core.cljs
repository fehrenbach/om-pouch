(ns om-pouch.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            cljsjs.pouchdb))

(enable-console-print!)

(defonce db (js/PouchDB. "om-pouch"))

(defonce app-state
  (atom {:window/size [1920 1200]
         :pouch/by-id {1 {:id 1 :name "Sally" :age 22 :married false}
                       2 {:id 2 :name "Joe" :age 33 :married false}
                       3 {:id 3 :name "Paul" :age 45 :married true :married-to [:pouch/by-id 1]}
                       4 {:id 4 :name "Bob" :age 55 :married true :married-to [[:pouch/by-id 1]
                                                                               [:pouch/by-id 3]]}
                       5 {:id 5 :name "Jim" :age 35 :married-to [:pouch/by-id 4]}}}))

(defmulti read om/dispatch)

(defmethod read :window/size
  [{:keys [state]} key params]
  {:value (:window/size @state)})

(defmethod read :pouch/by-id
  [{:keys [ast parser state query] :as env} key params]
  (if-let [doc (get-in @state (:key ast))]
    {:value (parser (assoc env :context doc) query)}
    {:value :loading}))

(defmethod read :default
  [{:keys [ast context parser query] :as env} key params]
  (case (:type ast)
    :prop
    {:value (get context key)}

    :join
    (let [join-key (get context (:key ast))
          ;; BUG Om does not distinguish 1:1 1:N N:1 N:M joins
          ;; We don't really want to call `the` all the time, so we try to autodetect it.
          ;; Unfortunately, this use of om/ident? is not safe (we apply it to data from the state, not a query.
          ;; TODO Make this configurable (pass a parameter to the join?) and possibly remove autodetection / warn.
          ;; one-to vs many-to
          join-keys (if (om/ident? join-key) [join-key] join-key)
          _ (.log js/console "join key" (str join-key))
          _ (.log js/console "join keys" (str join-keys))
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

(def query [:window/size {[:pouch/by-id 5] [:name {:married-to [:name {:married-to [:name]}]}]}])

;; (om/add-root! reconciler
;;               RootView (gdom/getElement "app"))

(parser {:state app-state} query)
