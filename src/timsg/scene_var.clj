(ns timsg.scene-var
  (:refer-clojure :exclude [def])
  (:use [arcadia.core])
  (:require [clojure.spec :as s]
            [clojure.core :as c]
            [arcadia.internal.spec :as as])
  (:import [UnityEngine GameObject Component]))

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

(defn- parse-def-form [args]
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
          init-spec (assoc fmap
                      :kw qualified-kw
                      :name qualified-name)]
      init-spec)))

;; Returns nil if the referenced object is destroyed. Maybe that's not a great idea.
(defn init-def
  "Internal, don't use."
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

(defmacro def
  "def form for binding vars to GameObjects. Syntax is close to that of a defn, with
zero or one arguments supported:

(def car 
  ([] (or (object-named \"the car\")
          (let [car (create-primitive :cube)]
            (set! (.name car) \"the car\")
            car)))
  ([car]
   (with-cmpt car [tr Transform]
     (set! (.position tr) (arcadia.linear/v3 0 2 10))
     (set! (.rotation tr) (arcadia.linear/aa 90 0 1 0))
     car)))

The 0-ary body, if supplied, runs if the var bound by this def
  form ('car', in the example above) is currently unbound, and should
  return a GameObject. Use this to either find an object in the scene
  graph, or create a new object if there currently is none. This arity
  typically only runs once per session, it will be ignored on
  subsequent redefs.

The 1-ary body, if supplied, mutates or replaces an existing value,
  and is where most of the work of redefinition should take place. If
  the var bound by this def form is currently unbound, the argument of
  the 1-ary body will be the return of the 0-ary body, if that is
  supplied, or nil if it is not. If, on the other hand, the var bound
  by this def form *is* currently bound, the argument to the 1-ary
  body will be set to whatever that value is. The var will then be
  rebound to the return of the 1-ary body.

If the var is bound to a null GameObject (one that has been destroyed,
  for example), the 1-ary body will receive a nil argument.

Another example:

(def truck [x]                          ;; Just the 1-ary body supplied
  (when x (destroy x))                  ;; Whatever it is, get rid of it
  (let [truck (create-primitive :cube)] ;; Make a new one
    (set! (.name truck) \"truck\")
    (with-cmpt truck [tr Transform]
      (set! (.position tr) (arcadia.linear/v3 0 5 10))
      (set! (.rotation tr) (arcadia.linear/aa 45 0 1 0))
      (set! (.localScale tr) (arcadia.linear/v3 1 2 4)))
    truck))
"
  [& args]
  (let [{:keys [name] :as init-spec} (parse-def-form args)]
    `(let [x# (init-def  ~(update init-spec :name #(list 'quote %)))]
       (def ~name x#))))
