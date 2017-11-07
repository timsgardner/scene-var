(ns timsg.scene-var
  (:use [arcadia.core])
  (:require [clojure.spec :as s]
            [clojure.core :as c]
            [arcadia.internal.spec :as as]
            [timsg.scene-var.types :as svt]
            [arcadia.internal.map-utils :as mu]
            [arcadia.internal.functions :as af])  
  (:import [UnityEngine GameObject Component]
           [timsg.scene_var.types Registry]
           SceneVarAnchor))

;; consider elevating this to the main file soon

(s/def ::tail-impl
  (s/cat
    :args (s/and vector? #(#{0 1} (count %)))
    :tail (s/* any?)))

(s/def ::def-entity-args
  (s/cat
    :name symbol?
    :scenes (s/? vector)
    :tails (s/alt
             :single-arity ::tail-impl
             :n-arities (s/* (s/spec ::tail-impl)))))

(defn- parse-def-form [args]
  (as/qwik-conform [{:keys [name tails scenes] :as spec} ::def-entity-args args]
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
                      :scenes (or scenes :all)
                      :kw qualified-kw
                      :name qualified-name)]
      init-spec)))

;; ============================================================
;; world and anchors

(defonce world-ref
  (atom (Registry. {} {})))

(defn obj->anchors [obj]
  (let [^Registry world @world-ref]
    (get (.obj->anchors world) obj)))

(defn anchor->obj [anchor]
  (let [^Registry world @world-ref]
    (get (.anchor->obj world) anchor)))

(defn gc []
  (swap! world-ref
    (fn [world]
      (let [a->o (.anchor->obj world)
            o->a (.obj->anchors world)]
        (let [nulls (into []
                      (filter null-obj?)
                      (keys o->a))]
          (Registry.
            (persistent!
              (transduce (mapcat o->a) (completing dissoc!) (transient a->o) nulls))
            (persistent!
              (reduce (completing dissoc!) (transient o->a) nulls))))))))

(defn- assoc-anchor! [anchor obj]
  (swap! world-ref
    (fn [^Registry world]
      (let [a->o (.anchor->obj world)
            o->a (.obj->anchors world)]
        (Registry.
          (assoc a->o anchor obj)
          (-> (if-let [old-obj (get a->o anchor)]
                (dissoc o->a old-obj)
                o->a)
              (update obj
                (fn [anchors]
                  (conj (or anchors #{}) anchor)))))))))

(defn- dissoc-anchor! [anchor]
  (swap! world-ref
    (fn [^Registry world]
      (let [a->o (.anchor->obj world)
            o->a (.obj->anchors world)]
        (Registry.
          (dissoc a->o anchor)
          (if-let [obj (get a->o anchor)]
            (let [anchors2 (disj (get o->a obj) anchor)]
              (if-not (empty? anchors2)
                (assoc o->a obj anchors2)
                (dissoc o->a obj)))
            o->a))))))

;; ============================================================ most
;; of the following assumes it's being done on main thread (since
;; we're not using refs, and Unity won't let us do it elswhere anyway)

;; could be faster
(defn sva-serialize [^SceneVarAnchor sva]
  (set! (.edn sva)
    (pr-str
      (obj->anchors sva))))

(defn sva-destroy [^SceneVarAnchor sva]
  (doseq [anchor (obj->anchors sva)]
    (dissoc-anchor! anchor)))

(defn sva-deserialize [^SceneVarAnchor sva]
  (let [world @world-ref
        anchors (read-string (.edn sva))]
    (doseq [anchor anchors]
      (if-not (contains? world anchor)
        (assoc-anchor! anchor sva)))))

;; all on main thread
(defn- add-anchor [^SceneVarAnchor sva, anchor]
  (assoc-anchor! anchor sva))

(defn ensure-anchor [^UnityEngine.GameObject obj, anchor]
  (let [^SceneVarAnchor sva (or (obj-nil (.GetComponent obj SceneVarAnchor))
                                (.AddComponent obj SceneVarAnchor))]
    (add-anchor sva anchor)
    obj))

;; ==================================================
;; initialization

(defn- game-object? [x]
  (instance? UnityEngine.GameObject x))

(defn- run-update [update-fn obj]
  (when update-fn
    (try
      (update-fn obj)
      (catch Exception e
        (UnityEngine.Debug/Log
          (str "Exception encountered in defgetter for " name "."))
        (UnityEngine.Debug/Log e)
        nil))))

(defn- run-init [init-fn]
  (when init-fn
    (let [obj (init-fn)]
      ;; Test that we in fact have a GameObject.
      (when-not (game-object? obj)
        (throw (Exception.
                 (str
                   "0-arity defgetter body must return instance of UnityEngine.GameObject. "
                   "Instead returning instance of class: " (pr-str (class obj))))))
      obj)))

(defn current-scene? [scenes]
  (boolean
    (or (= scenes :all) ;; the default if no scenes specified
        (let [this-scene (.name (UnityEngine.SceneManagement.SceneManager/GetActiveScene))]
          (some #(cond (string? %) (= % this-scene)
                       (instance? System.Text.RegularExpressions.Regex %) (re-matches % this-scene)
                       (ifn? %) (% this-scene)
                       :else false)
            scenes)))))

;; gotta happen on main thread!
(defn init-def
  "Internal, don't use."
  [{:keys [name kw init scenes]
    update-fn :update
    :as init-spec}]
  (let [anchor kw
        ;; If an existing GameObject corresponds to this anchor, get
        ;; it. If not, construct it.
        existing-obj (or (when-let [x (anchor->obj anchor)]
                           (if (null-obj? x)
                             (do (gc) ;; there shouldn't be any of these
                                 nil)
                             (gobj x)))
                         (run-init init))]    
    ;; If it isn't nil, make it correspond to the anchor.
    (when-not (nil? existing-obj)
      (ensure-anchor existing-obj anchor))
    ;; If the 1-ary update body to defgetter is defined, run it on the GameObject now.
    (if update-fn
      (let [updated-obj (run-update update-fn existing-obj)]
        (cond
          (game-object? updated-obj)
          (ensure-anchor updated-obj anchor)
          
          (nil? init)
          (throw (Exception.
                   (str "0-ary body of defgetter is not provided, and 1-ary body of "
                        "defgetter does not return GameObject instance. Instead returns "
                        "instance of: " (pr-str (class updated-obj)))))

          :else existing-obj))
      existing-obj)))

;; to def and get an object from the scene
(defmacro defgetter
  "(defgetter name 0-ary-body? 1-ary-body?)

  def form for defining GameObject getter functions that reach into the Unity scene graph.

  Establishes and maintains a live, serializable correspondence between the var named by `name` and an individual GameObject, and binds this var to a 0-ary getter function instance that will always return the currently-corresponding GameObject. 

  The syntax is close to that of a defn, with bodies of 0 or 1 arguments supported:

  (defgetter the-floor
    ([]
     (let [floor (arcadia.core/create-primitive :cube)]
       (set! (.name floor) \"the-floor\")
       floor))
    ([floor]
     (arcadia.core/with-cmpt floor [tr Transform]
       (set! (.position tr) (v3 0 -0.5 0))
       (set! (.localScale tr) (v3 1000 1 100)))))

  Both arities only run when the `defgetter` form itself is evaluated (they are not the body of the produced getter function).

  The 0-ary body is useful for constructing GameObject, the 1-ary body is body is useful for mutating existing GameObjects or changing the association to a different GameObject.

  0-ary-body:
  ([] exprs*)

  The 0-ary body, if supplied, runs only if the var bound by this def form (#'car, in the example above) is currently not associated with a live GameObject, and should return a GameObject (an error is thrown if it does not). The var will then be associated with the returned GameObject. This arity can be used to create a new GameObject, or to find one already in the scene.

  1-ary-body:
  ([obj] exprs*)

  The 1-ary body, if supplied, runs whenever the `defgetter` form is evaluated (just after the 0-ary body, if it is present). `obj` is the GameObject currently associated with the var, or `nil` if no such var exists.

  If the 1-ary body returns a GameObject, the var will be associated with that GameObject. If it does not, or if an error is thrown in `exprs*`, the var will remain associated with `obj`.

  After `defgetter` is evaluated, the var `name` will be bound to a getter function that will return the GameObject associated with `name`, or `nil` if no such GameObject exists. If the GameObject associatd with `name` changes, the getter function will subsequently return that new GameObject."
  [& args]
  (let [{:keys [name kw scenes] :as init-spec} (parse-def-form args)]
    `(do (when (current-scene? ~scenes) ; happens at runtime, so `scenes` gets eval'd
           (init-def ~(update init-spec :name #(list 'quote %))))
         (defn ^UnityEngine.GameObject ~name
           ([] (~name @world-ref))
           ([^timsg.scene_var.types.Registry world#]
            (when-let [^SceneVarAnchor sva# (obj-nil (anchor->obj ~kw))]
              (.gameObject sva#)))))))
