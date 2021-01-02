(ns com.evocomputing.macros)

(defmacro def-color-bin-op
  "Macro for creating binary operations between two colors.

  Arguments
  name - the name of the operation.

  bin-op - the function that takes two values, producing one from
  those. (eg: + - * /). This op will be applied pairwise to the
  repective color's rgba components to create a new color with the
  resultant rgba components.

  Result
  color - a new color that is the result of the binary operation."
  [name docstring bin-op]
  `(defn ~name
     ~docstring
     [color1# color2#]
     (com.evocomputing.colors/create-color (vec (map com.evocomputing.colors/clamp-rgb-int (map ~bin-op (:rgba color1#) (:rgba color2#)))))))

(defmacro create-color-with-meta
  "Create color with type metadata."
  [& body]
  `(with-meta
     ~@body
     {:type :com.evocomputing.colors/color}))
