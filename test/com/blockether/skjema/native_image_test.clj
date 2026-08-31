(ns com.blockether.skjema.native-image-test
  "The eight meta-schemas are read from the classpath with `io/resource`, which
   answers nil in a native image for a resource nobody registered - and a nil
   there is an exception at the first `compile`, in a consumer's binary, with
   this suite green. The registration travels inside the jar, so a consumer
   builds a working image without knowing this library reads anything."
  (:require [clojure.java.io :as io]
            [com.blockether.skjema.json :as json]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]])
  (:import (java.nio.file FileSystems Paths)))

(def ^:private metadata-path
  "resources/META-INF/native-image/com.blockether/skjema/reachability-metadata.json")

(def ^:private meta-schema-dir "resources/com/blockether/skjema/meta/2020-12")

(defn- globs []
  (->> (json/read-str (slurp (io/file metadata-path)))
       (#(get % "resources"))
       (map #(get % "glob"))))

(defn- matches? [glob path]
  (.matches (.getPathMatcher (FileSystems/getDefault) (str "glob:" glob))
            (Paths/get path (into-array String []))))

(deftest every-bundled-meta-schema-is-registered
  (let [files (->> (.listFiles (io/file meta-schema-dir))
                   (map #(.getName ^java.io.File %))
                   (filter #(.endsWith ^String % ".json"))
                   sort)
        patterns (globs)]
    (testing "the jar carries native-image metadata at all"
      (is (seq patterns)))
    (testing "eight documents, and every one of them is both registered and readable"
      (is (= 8 (count files)))
      (doseq [f files
              :let [classpath-path (str "com/blockether/skjema/meta/2020-12/" f)]]
        (is (some #(matches? % classpath-path) patterns)
            (str classpath-path " is read at runtime but no glob registers it"))
        (is (some? (io/resource classpath-path)))))))
