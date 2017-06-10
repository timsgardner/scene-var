# timsg.scene-var

An (Arcadia)[https://github.com/arcadia-unity/Arcadia] library for interacting with the Unity scene graph.

## Clojars

`[timsg.scene-var "0.1.0-SNAPSHOT"]`

## Usage

`defgetter` defines getter functions for constructing, mutating, and retrieving individual GameObjects from the Unity scene graph.

The syntax is close to that of a defn, with bodies of 0 or 1 arguments supplied:
```clojure
(defgetter the-floor
  ([]
   (let [floor (arcadia.core/create-primitive :cube)]
     (set! (.name floor) \"the-floor\")
     floor))
  ([floor]
   (arcadia.core/with-cmpt floor [tr Transform]
     (set! (.position tr) (v3 0 -0.5 0))
     (set! (.localScale tr) (v3 1000 1 100)))))
```
Both arities only run when the `defgetter` form itself is evaluated (they are not the body of the produced getter function).

The 0-ary body is useful for constructing GameObject, the 1-ary body is body is useful for mutating existing GameObjects or changing the association to a different GameObject.

### 0-ary body
`([obj] exprs*)`

The 0-ary body, if supplied, runs only if the var bound by this def form (#'car, in the example above) is currently not associated with a live GameObject, and should return a GameObject (an error is thrown if it does not). The var will then be associated with the returned GameObject. This arity can be used to create a new GameObject, or to find one already in the scene.

### 1-ary body
`([obj] exprs*)`

The 1-ary body, if supplied, runs whenever the `defgetter` form is evaluated (just after the 0-ary body, if it is present). `obj` is the GameObject currently associated with the var, or `nil` if no such var exists.

If the 1-ary body returns a GameObject, the var will be associated with that GameObject. If it does not, or if an error is thrown in `exprs*`, the var will remain associated with `obj`.

After `defgetter` is evaluated, the var `name` will be bound to a getter function that will return the GameObject associated with `name`, or `nil` if no such GameObject exists. If the GameObject associated with `name` changes, the getter function will subsequently return that new GameObject.

## License

Copyright © 2016 Tims Gardner

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
