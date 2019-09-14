(ns bananas.core
  (:require [cats.core :as m]
            [cats.protocols :as p]
            [cats.monad.either :as either]
            [cats.builtin]))

(defmacro <<< [& args]
  `(fn [x#]
     ((comp ~@args) x#)))

(defmacro >>> [& args]
  `(fn [x#]
     ((comp ~@(reverse args)) x#)))

(defn &&&
  [f g]
  (fn [x]
    [(f x) (g x)]))

(defn |||
  [lf rf]
  (fn [x]
    (either/branch x lf rf)))

(defn functor?
  [v]
  (and (satisfies? p/Contextual v)
       (satisfies? p/Functor (p/-get-context v))))

(defn fmap*
  [f v]
  (cond->> v
    (functor? v)
    (m/fmap f)))

(def fmap (m/curry fmap*))

(defn cata
  [f]
  (>>> (fmap (cata f)) f))

(defn ana
  [f]
  (<<< (fmap (ana f)) f))

(defn para
  [f]
  (>>> (&&& identity (fmap (para f))) f))

(defn apo
  [f]
  (<<< (||| identity (fmap (apo f))) f))

(defn histo
  [f]
  (>>> (cata (&&& f identity)) first))

(defn futu
  [f]
  (<<< first (ana (||| f identity))))
