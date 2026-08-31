(ns build
  "Build/deploy for skjema. One small jar of pure Clojure, no dependencies."
  (:require [clojure.string :as str]
            [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'com.blockether/skjema)

(def declared-version
  "This library's own release number. The repo-root `SKJEMA_VERSION` file is its
   single source of truth; the release tag mirrors it."
  (str/trim (slurp "SKJEMA_VERSION")))

(def version
  "What an artifact is stamped with. CI exports SKJEMA_VERSION from the release
   tag and publishes that exact number; every other build is a `-SNAPSHOT`, so a
   local install cannot shadow a release in ~/.m2."
  (if-let [tag (System/getenv "SKJEMA_VERSION")]
    (str/replace tag #"^v" "")
    (str declared-version "-SNAPSHOT")))

(defn- check-version!
  "Refuse to build artifacts whose version sources disagree: the tag names the
   Clojars coordinate and `SKJEMA_VERSION` is what the pom declares, so drift
   between them publishes a version nobody asked for."
  []
  (let [release (str/replace version #"-SNAPSHOT$" "")]
    (when-not (= release declared-version)
      (throw (ex-info (format "version mismatch: tag %s, SKJEMA_VERSION %s"
                              release declared-version)
                      {:release release :declared declared-version})))))

(def class-dir "target/classes")
(def jar-file (format "target/%s.jar" (name lib)))
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_] (b/delete {:path "target"}))

(defn- pom-data []
  [[:description "JSON Schema 2020-12 validation for Clojure. No dependencies, no reflection, native-image safe."]
   [:url "https://github.com/Blockether/skjema"]
   [:licenses [:license [:name "MIT License"] [:url "https://opensource.org/licenses/MIT"]]]
   [:scm [:url "https://github.com/Blockether/skjema"]
    [:connection "scm:git:https://github.com/Blockether/skjema.git"]
    [:developerConnection "scm:git:ssh://git@github.com/Blockether/skjema.git"]]])

(defn jar [_]
  (check-version!)
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src"]
                :pom-data (pom-data)})
  (b/copy-dir {:src-dirs ["src" "resources"] :target-dir class-dir})
  ;; MIT asks that the notice travel with every copy, so the jar carries the
  ;; license text itself — an audit of the artifact alone still sees the terms.
  (b/copy-file {:src "LICENSE" :target (str class-dir "/META-INF/LICENSE")})
  (b/copy-file {:src "NOTICE" :target (str class-dir "/META-INF/NOTICE")})
  (b/jar {:class-dir class-dir :jar-file jar-file})
  ;; The bundled 2020-12 meta-schemas live in `resources/`, and a jar without
  ;; them throws on the first `compile` in every consumer — which is exactly how
  ;; 0.1.0 shipped. The build refuses to hand out an artifact that cannot validate.
  (let [entry "com/blockether/skjema/meta/2020-12/schema.json"]
    (with-open [zip (java.util.zip.ZipFile. (java.io.File. ^String jar-file))]
      (when-not (.getEntry zip entry)
        (throw (ex-info (str "the jar is missing " entry) {:jar jar-file})))))
  (println "Built:" jar-file "version:" version))

(defn deploy [_]
  (jar nil)
  (dd/deploy {:installer :remote :artifact jar-file :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))

(defn install [_]
  (jar nil)
  (dd/deploy {:installer :local :artifact jar-file :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))
