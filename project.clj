(defproject roar "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.3.442"]
                 ]
  :main roar.core
  :target-path "target/%s"
  :manifest {"Class-Path" "lib/clojure-1.8.0.jar"}
  )
