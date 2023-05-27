(ns build
  (:require
   [clojure.string :as string]
   [clojure.tools.build.api :as b]))

;;; See also https://clojure.org/guides/tools_build
;;;   clj -T:build clean
;;;   clj -T:build jar
;;;   clj -T:build install
;;;
;;; To install jar:  clj -T:build all-jar
;;; For a uber-jar:  clj -T:build all-uber

(def lib 'com.github.pdenno/rad-mapper)
(def version (format "1.0.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(def main-cls (string/join "." (filter some? [(namespace lib) (name lib) "core"])))
(def target-dir "target")
(def uber-file (format "%s/%s-standalone.jar" target-dir (name lib)))

(defn clean [_]
  (println "Doing the clean")
  (b/delete {:path "target"}))

(defn prep [_]
  (println "Writing the pom.")
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src/lib"]})
  (b/copy-dir {:src-dirs ["src/lib"] :target-dir class-dir}))

(defn jar [_]
  (println "writing the jar")
  (b/jar {:class-dir class-dir :jar-file jar-file}))

(defn uber [_]
  (println "Compiling Clojure...")
  (b/compile-clj {:basis basis
                  :src-dirs ["src/lib" "src/server" "resources" #_"env/prod/resources" #_"env/prod/clj"]
                  :class-dir class-dir})
  (println "Making uberjar...")
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :main main-cls
           :basis basis}))

;;; :basis - required, used for :mvn/local-repo
(defn install [_]
  (println "Installing: class-dir =" class-dir "version = " version)
  (let [opts {:lib lib :basis basis :jar-file jar-file :class-dir class-dir :version version}]
    (b/install opts)))

(defn all-uber [_]
  (clean nil) (prep nil) (uber nil))

(defn all-jar [_]
  (clean nil) (prep nil) (jar nil) (install nil))
