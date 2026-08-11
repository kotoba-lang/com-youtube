(ns youtube.videos
  "`videos.insert` (resumable, single-chunk PUT) and `videos.update`. Ported
  1:1 from `kotoba-lang/youtube-upload`'s `client.py` `upload_video` (the
  metadata shape, header names, and status-code checks are unchanged)."
  (:require [youtube.client :as client]))

(def videos-insert-url (str client/upload-api "/videos?uploadType=resumable&part=snippet,status"))
(def videos-update-url (str client/data-api "/videos?part=snippet,status"))

(defn video-metadata
  "{:title :description :tags :category-id :default-language :privacy-status
    :made-for-kids? :embeddable?} -> the videos.insert/update request body.
  Title/description are clamped to the API's 100/5000-char limits."
  [{:keys [title description tags category-id default-language privacy-status
           made-for-kids? embeddable?]
    :or {tags [] made-for-kids? false embeddable? true}}]
  {:snippet {:title (subs (or title "") 0 (min 100 (count (or title ""))))
             :description (subs (or description "") 0 (min 5000 (count (or description ""))))
             :tags tags
             :categoryId category-id
             :defaultLanguage default-language
             :defaultAudioLanguage default-language}
   :status {:privacyStatus privacy-status
           :selfDeclaredMadeForKids made-for-kids?
           :embeddable embeddable?}})

(defn insert-video!
  "Resumable videos.insert. `video-bytes` is a JVM byte array (mp4).
  Returns the new YouTube video id. Throws ex-info on any transport/API
  failure (mirrors client.py's YouTubeUploadError stages: :upload-init /
  :upload-put)."
  ([access-token video-bytes metadata] (insert-video! access-token video-bytes metadata {}))
  ([access-token video-bytes metadata {:keys [http-fn] :or {http-fn (client/default-http-fn)}}]
   (let [init-resp (http-fn {:url videos-insert-url
                             :method :post
                             :headers (merge (client/auth-header access-token)
                                            {"Content-Type" "application/json; charset=UTF-8"
                                             "X-Upload-Content-Type" "video/mp4"
                                             "X-Upload-Content-Length" (str (client/byte-count video-bytes))})
                             :body (client/write-json metadata)})]
     (when-not (#{200 201} (:status init-resp))
       (throw (ex-info "youtube videos.insert init failed"
                       {:stage :upload-init :status (:status init-resp) :body (:body init-resp)})))
     (let [upload-url (get (:response-headers init-resp) "location")]
       (when-not upload-url
         (throw (ex-info "youtube videos.insert init: no Location header" {:stage :upload-init})))
       (let [put-resp (http-fn {:url upload-url
                                :method :put
                                :headers {"Content-Type" "video/mp4"
                                         "Content-Length" (str (client/byte-count video-bytes))}
                                :body video-bytes})]
         (when-not (#{200 201} (:status put-resp))
           (throw (ex-info "youtube videos.insert upload failed"
                           {:stage :upload-put :status (:status put-resp) :body (:body put-resp)})))
         (let [video-id (:id (client/read-json (:body put-resp)))]
           (when-not video-id
             (throw (ex-info "youtube videos.insert: no id in response" {:stage :upload-put :body (:body put-resp)})))
           video-id))))))

(defn update-video!
  "videos.update -- PUT a full snippet/status body for an existing video.
  `video-id` is merged into the body as :id (required by the API)."
  ([access-token video-id metadata] (update-video! access-token video-id metadata {}))
  ([access-token video-id metadata {:keys [http-fn] :or {http-fn (client/default-http-fn)}}]
   (let [resp (http-fn {:url videos-update-url
                        :method :put
                        :headers (merge (client/auth-header access-token)
                                       {"Content-Type" "application/json; charset=UTF-8"})
                        :body (client/write-json (assoc metadata :id video-id))})]
     (when-not (#{200} (:status resp))
       (throw (ex-info "youtube videos.update failed" {:stage :update :status (:status resp) :body (:body resp)})))
     (client/read-json (:body resp)))))
