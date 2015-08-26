# jsonmerge + jsonfold


## jsonmerge

Merge JSON leaf values





## jsonfold


```
jsonfold

Usage: jsonfold DSL [-d|--debug]
  Fold merge JSON leaves with arrays

Available options:
  -h,--help                Show this help text
  DSL                      Path directives DSL
  -d,--debug               Debug directive parser. Does not parse STDIN

```


### Examples

test.json

```json
{"test":["apple","banana","apple","pear"]}
```


```
$ jsonfold 'test( sortfreq.desc | head ) '  < test.json 
{"test":"apple"}

$ jsonfold 'test( sort.desc | head ) '  < test.json 
{"test":"pear"}

$ jsonfold 'test(  concatsep " + " ) '  < test.json 
{"test":"apple + banana + apple + pear"}

```
