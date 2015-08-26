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


# numbers.json
# {"numbers":[[8,2,3],[1,2,3,4],[null]]}

$ jsonfold 'numbers(concat|compact|sort.asc|nub)' < numbers.json 
{"numbers":[1,2,3,4,8]}

```


