(ns youtube.client
  "Portable core for talking to the YouTube Data API v3 upload surface --
  OAuth2 refresh-token exchange + one injectable HTTP boundary for every
  youtube.* namespace in this library.

  Extracted/re-ported from `kotoba-lang/youtube-upload` (a Python client
  already extracted from `ai-gftd-project-yukkuri`'s
  `lg_yukkuri/graphs/upload_youtube.py`). That Python package's own README
  explicitly recommends wrapping rather than rewriting for Clojure
  consumers -- this repo does the rewrite anyway (explicit request), so it
  re-derives the same request/response shapes from the original httpx
  client's behavior (`kotoba-lang/youtube-upload/src/youtube_upload/client.py`)
  rather than guessing at the API.

  Request/response shaping is pure `.cljc`. The actual HTTP call is
  JVM-only by default (`java.net.http`) but every function takes an
  injectable `:http-fn` -- the same `{:url :method :headers :body} ->
  {:status :body}` convention `kotoba-lang/com-cloudflare` uses -- so every
  namespace here is testable with a stub, never only against a live
  account. `:body` may be a String or a JVM byte array (video/image bytes);
  the default transport handles both.

  Credentials: never held or defaulted by this library. Callers pass
  `:client-id`/`:client-secret`/`:refresh-token`/`:access-token` explicitly
  (or read them from wherever their own env/secret store keeps them) --
  mirrors `ai-gftd-project-yukkuri/docs/youtube-upload-setup.md`'s policy
  that these are operator-injected, never code-held."
  (:require [clojure.string :as str]
            #?(:clj [jsonista.core :as json])))

(def token-url "https://oauth2.googleapis.com/token")
(def data-api "https://www.googleapis.com/youtube/v3")
(def upload-api "https://www.googleapis.com/upload/youtube/v3")

#?(:clj (def mapper (json/object-mapper {:decode-key-fn keyword})))
#?(:clj (defn write-json [x] (json/write-value-as-string x mapper)))
#?(:clj (defn read-json [s] (json/read-value s mapper)))

#?(:clj
(defn jvm-http-fn
  "Real java.net.http transport. {:url :method :headers :body} ->
  {:status :body :response-headers}. `:body` may be a String or a byte[]
  (BodyPublishers dispatches on type); response body is always read as a
  UTF-8 String (fine for JSON responses -- binary GETs aren't part of this
  API's upload surface)."
  ([] (jvm-http-fn {}))
  ([{:keys [timeout-seconds] :or {timeout-seconds 900}}]
   (fn [{:keys [url method headers body]}]
     (let [publisher (cond
                       (nil? body) (java.net.http.HttpRequest$BodyPublishers/noBody)
                       (bytes? body) (java.net.http.HttpRequest$BodyPublishers/ofByteArray body)
                       :else (java.net.http.HttpRequest$BodyPublishers/ofString body))
           builder (-> (java.net.http.HttpRequest/newBuilder (java.net.URI/create url))
                      (.timeout (java.time.Duration/ofSeconds timeout-seconds))
                      (as-> b (reduce-kv (fn [b k v] (.header b (name k) (str v))) b headers)))
           request (case method
                     :post (-> builder (.POST publisher) .build)
                     :put (-> builder (.PUT publisher) .build)
                     :get (-> builder .GET .build)
                     (throw (ex-info "Unsupported HTTP method" {:method method})))
           resp (.send (java.net.http.HttpClient/newHttpClient) request
                      (java.net.http.HttpResponse$BodyHandlers/ofString))]
       {:status (.statusCode resp)
        :body (.body resp)
        :response-headers (into {} (map (fn [[k vs]] [(str/lower-case k) (first vs)]))
                                (.map (.headers resp)))})))))

#?(:clj
(defn refresh-access-token!
  "Exchange a stored OAuth2 refresh token for a short-lived access token.
  {:client-id :client-secret :refresh-token} -> the access-token string.
  Throws ex-info on a non-2xx response or a response with no access_token."
  ([creds] (refresh-access-token! creds {}))
  ([{:keys [client-id client-secret refresh-token]} {:keys [http-fn] :or {http-fn (jvm-http-fn)}}]
   (let [form (str "client_id=" (java.net.URLEncoder/encode client-id "UTF-8")
                   "&client_secret=" (java.net.URLEncoder/encode client-secret "UTF-8")
                   "&refresh_token=" (java.net.URLEncoder/encode refresh-token "UTF-8")
                   "&grant_type=refresh_token")
         resp (http-fn {:url token-url :method :post
                        :headers {"Content-Type" "application/x-www-form-urlencoded"}
                        :body form})]
     (when (>= (:status resp) 400)
       (throw (ex-info "youtube oauth refresh failed" {:stage "oauth" :status (:status resp) :body (:body resp)})))
     (let [token (:access_token (read-json (:body resp)))]
       (when-not token
         (throw (ex-info "youtube oauth response has no access_token" {:stage "oauth" :body (:body resp)})))
       token)))))

#?(:clj
(defn auth-header [access-token]
  {"Authorization" (str "Bearer " access-token)}))
