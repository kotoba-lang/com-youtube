(ns youtube.videos-test
  (:require [clojure.test :refer [deftest is testing]]
            [youtube.videos :as videos]))

(deftest video-metadata-test
  (let [m (videos/video-metadata {:title (apply str (repeat 150 "x"))
                                  :description (apply str (repeat 6000 "y"))
                                  :tags ["a" "b"] :category-id "27"
                                  :default-language "ja" :privacy-status "unlisted"})]
    (is (= 100 (count (get-in m [:snippet :title]))))
    (is (= 5000 (count (get-in m [:snippet :description]))))
    (is (= ["a" "b"] (get-in m [:snippet :tags])))
    (is (= "unlisted" (get-in m [:status :privacyStatus])))
    (is (false? (get-in m [:status :selfDeclaredMadeForKids])))
    (is (true? (get-in m [:status :embeddable])))))

(deftest insert-video-test
  (testing "happy path: init then PUT, returns video id"
    (let [calls (atom [])
          stub (fn [{:keys [method url] :as req}]
                 (swap! calls conj req)
                 (case method
                   :post {:status 200 :body "{}" :response-headers {"location" "https://upload.example/abc"}}
                   :put {:status 200 :body "{\"id\":\"yt123\"}" :response-headers {}}))
          video-id (videos/insert-video! "tok" (byte-array [1 2 3])
                                         (videos/video-metadata {:title "t" :description "d" :category-id "27"
                                                                 :default-language "ja" :privacy-status "unlisted"})
                                         {:http-fn stub})]
      (is (= "yt123" video-id))
      (is (= 2 (count @calls)))
      (is (= :post (:method (first @calls))))
      (is (= :put (:method (second @calls))))
      (is (= "https://upload.example/abc" (:url (second @calls))))))

  (testing "init failure raises with :upload-init stage"
    (let [stub (fn [_] {:status 403 :body "denied" :response-headers {}})]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"init"
                            (videos/insert-video! "tok" (byte-array [1]) {} {:http-fn stub})))))

  (testing "missing Location header raises"
    (let [stub (fn [_] {:status 200 :body "{}" :response-headers {}})]
      (is (thrown? clojure.lang.ExceptionInfo
                   (videos/insert-video! "tok" (byte-array [1]) {} {:http-fn stub})))))

  (testing "PUT failure raises with :upload-put stage"
    (let [stub (fn [{:keys [method]}]
                 (case method
                   :post {:status 200 :body "{}" :response-headers {"location" "https://upload.example/x"}}
                   :put {:status 500 :body "err" :response-headers {}}))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (videos/insert-video! "tok" (byte-array [1]) {} {:http-fn stub}))))))

(deftest update-video-test
  (let [calls (atom [])
        stub (fn [req] (swap! calls conj req) {:status 200 :body "{\"id\":\"yt123\"}" :response-headers {}})
        result (videos/update-video! "tok" "yt123" {:snippet {:title "new"}} {:http-fn stub})]
    (is (= "yt123" (:id result)))
    (is (= :put (:method (first @calls))))))
