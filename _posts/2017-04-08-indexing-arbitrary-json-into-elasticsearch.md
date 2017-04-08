---
layout: post
title: Indexing arbitrary JSON into Elasticsearch
categories:
- blog
---

Using the [Nested datatype][1], [Multi-fields][2], [`ignore_malformed`][3], and adding a pre-processing step to flatten nested JSON into key-value pairs, it's possible to index arbitrary JSON (even with type conflicts) into Elasticsearch.

Index definition:

```json
{
  "settings": {
    "mapper.dynamic": false
  },
  "mappings": {
    "sometype": {
      "dynamic": "strict",
      "_all" : { "enabled" : true },
      "properties": {
        "kv_pairs": {
          "type": "nested",
          "properties": {
            "key": { "type": "string", "index": "not_analyzed" },
            "value": {
              "type": "string",
              "fields": {
                "raw_string": { "type": "string", "index": "not_analyzed" },
                "analyzed_string": { "type": "string", "index": "analyzed" },
                "date": { "type": "date", "ignore_malformed": true },
                "long": { "type": "long", "ignore_malformed": true },
                "double": { "type": "double", "ignore_malformed": true }
              }
            }
          }
        }
      }
    }
  }
}
```

Pre-processing step example:

```
$ echo '{"integer":3,"nested":{"inner":"object"}}' | \
    jq '[leaf_paths as $path | {"key": $path | map(tostring) | join("."), "value": getpath($path)}] | {kv_pairs: .}'
{
  "kv_pairs": [
    {
      "key": "integer",
      "value": 3
    },
    {
      "key": "nested.inner",
      "value": "object"
    }
  ]
}
```

Example query:

```json
{
  "query": {
    "nested": {
      "path": "kv_pairs",
      "query": {
        "bool": {
          "must": [
            {
              "match": {
                "kv_pairs.key.raw_string": "nested.inner"
              }
            },
            {
              "match": {
                "kv_pairs.value.raw_string": "object"
              }
            }
          ]
        }
      }
    }
  }
}
```

- Flattening the JSON into key-value pairs is necessary to avoid a [mapping explosion][4]
- The nested type is necessary to associate key-value pairs so that `"bool":{"must":[...]}` clauses operate on individual pairs
- `ignore_malformed` suppresses Elasticsearch mapping conflicts

[1]: https://www.elastic.co/guide/en/elasticsearch/reference/2.4/nested.html
[2]: https://www.elastic.co/guide/en/elasticsearch/reference/2.4/multi-fields.html
[3]: https://www.elastic.co/guide/en/elasticsearch/reference/2.4/ignore-malformed.html
[4]: https://www.elastic.co/blog/found-crash-elasticsearch#mapping-explosion
