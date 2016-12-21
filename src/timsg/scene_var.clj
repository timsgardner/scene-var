(ns timsg.scene-var
  (:refer-clojure :exclude [def])
  (:use [arcadia.core])
  (:require [clojure.spec :as s]
            [clojure.core :as c])
  (:import [UnityEngine GameObject Component]))

(defn init-def
  "Returns nil if the referenced object is destroyed. Maybe that's not a great idea."
  [{:keys [name init update]
    :as init-spec}]
  (-> (or (when-let [v (resolve name)] ;; name is fully qualified
            (and (bound? v)
                 (when-let [v (var-get v)]
                   (if (instance? UnityEngine.Object v)
                     (obj-nil v)
                     v))))
          (and init (init)))
      (as-> ent
            (if update (update ent) ent))))

(defmacro def [& args]
  (let [{:keys [name] :as init-spec} (parse-def-form args)]
    `(let [x# (init-def  ~(update init-spec :name #(list 'quote %)))]
       (def ~name x#))))
