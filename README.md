# KoAzBlob

## Status

Not ready.

## Usage

Setup account.

```
;; pass connection string as is.
(setq ac (koazblob:az-storage-account "DefaultEndpointsProtocol=https;AccountName=...;...=core.windows.net"))
```

Call API.

```
(koazblob:az-list-containers ac)
```

```
(koazblob:az-get-blob ac :container "mycontainer" :path "/file/2019/10/log.txt")
```

## Installation
