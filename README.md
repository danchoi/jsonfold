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


```
# fruit.json
{"fruit":["apple","banana","apple","pear"]}

$ jsonfold 'fruit( sortfreq.desc | head ) '  < fruit.json 
{"fruit":"apple"}

$ jsonfold 'fruit( sort.desc | head ) '  < fruit.json 
{"fruit":"pear"}

$ jsonfold 'fruit( concatsep " + " ) '  < fruit.json 
{"fruit":"apple + banana + apple + pear"}


# fruit-and-numbers.json
# {"fruits":["apple","banana","apple","pear"], "numbers":[[8,2,3],[1,2,3,4],[null]]}

$ jsonfold 'fruit(sort.asc|head) numbers(concat|compact|sort.asc|nub|reverse)' < fruit-and-numbers.json 
{"fruit":"apple","numbers":[8,4,3,2,1]}

```


