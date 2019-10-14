# KoAzBlob

## Status

Very basic functionality.

You may also read Blob REST API.
https://docs.microsoft.com/en-us/rest/api/storageservices/blob-service-rest-api

## Usage

Setup Blob service context.

```
;; pass connection string as is.
(setq ac (koazblob:az-storage-account "DefaultEndpointsProtocol=https;AccountName=...;...=core.windows.net"))
```

List containers.

```
(koazblob:az-list-containers ac)
```

Create container.

```
(koazblob:az-create-container ac :container "example")
```

Delete container.

```
(koazblob:az-delete-container ac :container "example")
```

Get Blob contents.

```
(koazblob:az-get-blob ac :container "mycontainer" :path "/file/2019/10/log.txt")
```

Get Blob properties.

```
(koazblob:az-get-blob-props ac :container "mycontainer" :path "/file/2019/10/log.txt")
```

Operation on Block Blob.

```
;; Put (create new, or replace exists).
(koazblob:az-put-blob ac
                      :container "example"
                      :path "/test.txt"
                      :content "hello, world"
                      :headers '(("content-type" . "text/plain; charset=utf-8")))
```

Operation on Append Blob.

```
;; First. Empty append entry.
(koazblob:az-put-blob ac
                      :container "example"
                      :path "/append.txt"
                      :headers '(("content-type" . "text/plain"))
                      :blob-type koazblob:+az-blob-type-append+)
;; Append block.
(koazblob:az-append-block ac
                      :container "example"
                      :path "/append.txt"
                      :content "hello")
```

Delete Blob.

```
(koazblob:az-delete-blob ac :container "mycontainer" :path "/file/2019/10/log.txt")
```

## Installation
