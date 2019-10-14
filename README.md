# KoAzBlob

## Status

Very basic functionality.

## Usage

Setup account.

```
;; pass connection string as is.
(setq ac (koazblob:az-storage-account "DefaultEndpointsProtocol=https;AccountName=...;...=core.windows.net"))
```

Setup Blob service context.

```
(koazblob:az-list-containers ac)
```

Get Blob contents.

```
(koazblob:az-get-blob ac :container "mycontainer" :path "/file/2019/10/log.txt")
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

## Installation
