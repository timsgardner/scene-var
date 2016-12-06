(ns timsg.scene-var
  (:use [arcadia.core])
  (:require [clojure.spec :as s]
            [clojure.core :as c]
            [arcadia.internal.spec :as as])
  (:import [UnityEngine GameObject Component]))

;; ============================================================
;; utils

;; until it's a thing:
(defn- hook-replace
  "Remove all `hook` components attached to `obj` at `k` and replace with `f`"
  [obj hook k f]
  (hook- obj hook k)
  (hook+ obj hook k f))

;; ============================================================
;; plumbing

;; think you only need one piece of state and a callback on the object

(defonce ^:private name-registry
  (atom {::objs->names {}
         ::names->objs {}}))

(defn- remove-name-obj [reg name obj]
  (-> reg
      (update ::objs->names dissoc obj)
      (update ::names->objs dissoc name)))

(defn- put-name-obj [{:keys [::objs->names ::names->objs]
                      :as reg},
                     name, obj]
  (let [old-obj (get names->objs name)
        old-name (get objs->names obj)]
    (-> reg
        (remove-name-obj old-name old-obj)
        (update ::objs->names assoc obj name)
        (update ::names->objs assoc name obj))))

(defn- handle-obj-destroy [^GameObject obj]
  (swap! name-registry
    (fn [reg]
      (when-let [name (-> reg (get ::objs->names) (get obj))]
        (remove-name-obj reg name obj)))))

(defn- register-name [^GameObject obj, name]
  (hook-replace obj :on-destroy ::scene-var #'handle-obj-destroy)
  (swap! name-registry #(put-name-obj % name obj)))

(defn find-entity [name]
  (-> @name-registry
      (get ::names->objs)
      (get name)))

(defn- run-init [init-fn name]
  (when-let [ent (init-fn)]
    (when-not (instance? GameObject ent)
      (throw
        (Exception.
          (str "Zero-argument arity of def-entity, if present, must return a GameObject; instead getting a " (type ent)))))
    ent))


(defn init-entity [{:keys [name init update] :as init-spec}]
  (-> (or (find-entity name)
          (and init (run-init init name)))
      (as-> ent
            (if update (update ent) ent)
            (register-name ent name))))


;; ============================================================
;; def-entity

(s/def ::tail-impl
  (s/cat
    :args (s/and vector? #(#{0 1} (count %)))
    :tail (s/* any?)))

(s/def ::def-entity-args
  (s/cat
    :name symbol?
    :tails (s/alt
             :single-arity ::tail-impl
             :n-arities (s/* (s/spec ::tail-impl)))))

(s/fdef def-entity
  :args ::def-entity-args 
  :ret any?)

(defmacro def-entity [& args]
  (as/qwik-conform [{:keys [name tails] :as spec} ::def-entity-args args]
    (let [qualified-name  (symbol
                            (c/name (ns-name *ns*))
                            (c/name name))
          qualified-kw (keyword qualified-name) ;; faster lookup!
          ftails (case (first tails)
                   :single-arity [(second tails)]
                   :n-arities (second tails))
          fmap (into {}
                 (for [{:keys [tail args]} ftails]
                   (let [k (case (count args)
                             0 :init
                             1 :update)]
                     [k `(fn ~args ~@tail)])))
          init-spec (assoc fmap :name qualified-kw)]
      `(do (init-entity ~init-spec)
           (defn ~name ^GameObject []
             (find-entity ~qualified-kw))))))


;; desired:

(comment
  ;; try it:
  (def-entity mr-humms
    ([] ;; initialize if no connection found
     (GameObject. "mr-humms"))
    ([x] ;; then feed into here I guess..?
     ()))
  )


