(ns moar.infer
  (:require [moar.protocols :refer :all]
            [moar.core :refer :all]))

(derive (:on-interface MonadInstance) ::monad-instance)
(derive (:on-interface Applicative)   ::applicative)
(derive (:on-interface Functor)       ::functor)

(defmulti infer (fn [record proto] [record (:on-interface proto)]))

(prefer-method infer ::monad-instance ::applicative)

(defmethod infer [::monad-instance ::functor] [record _]
  (extend record Functor
          {:fmap*
           (fn [m-val fun]
             (let [monad (monad-implementation m-val)]
               (bind* monad
                      m-val
                      (fn [val] (wrap* monad (fun val))))))}))

(defmethod infer [::applicative ::functor] [record _]
  (extend record Functor
          {:fmap*
           (fn [m-val fun]
             (apply fapply* (pure* m-val fun) [m-val]))}))

(defmethod infer [::monad-instance ::applicative] [record _]
  (extend record Applicative
          {:pure* (fn [f-val val]
                    (wrap* (monad-implementation f-val)
                           val))
           :fapply* (fn [f-fun f-vals]
                      (mapply (monad-implementation f-fun)
                              f-fun
                              f-vals))}))
