(ns timsg.scene-var.get
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
    :name (s/? symbol?)
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


;; ============================================================
;; world and anchors

(defonce world-ref
  (atom (Registry. {} {})))

(defn- obj->anchors []
  (let [^Registry world @world-ref]
    (.obj->anchors world)))

(defn- anchor->obj []
  (let [^Registry world @world-ref]
    (.anchor->obj world)))

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
      (get (obj->anchors) sva))))

(defn sva-destroy [^SceneVarAnchor sva]
  (doseq [anchor (get (obj->anchors) sva)]
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

;; gotta happen on main thread!
(defn init-def
  "Internal, don't use."
  [{:keys [name kw init update]
    :as init-spec}]
  (let [anchor kw]
    (-> (or (when-let [x (get (anchor->obj) anchor)]
              (if (null-obj? x)
                (do (gc) ;; there shouldn't be any of these
                    nil)
                (gobj x)))
            (and init (init)))
        (as-> ent
              (if update (update ent) ent))
        (ensure-anchor anchor) ;; logic in here will force main thread, I think
        )))

(defmacro getter [anchor & args])

;; to def and get an object from the scene
(defmacro defgetter [& args]
  (let [{:keys [name kw] :as init-spec} (parse-def-form args)]
    `(do (init-def ~(update init-spec :name #(list 'quote %)))
         (defn ~name
           ([] (~name @world-ref))
           ([^timsg.scene_var.types.Registry world#]
            (when-let [^SceneVarAnchor sva# (obj-nil (get (.anchor->obj world#) ~kw))]
              (.gameObject sva#)))))))
