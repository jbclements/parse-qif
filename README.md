# parse-qif
Racket code to parse QIF files (and associate categories with transactions)

No real docs, sorry... putting this out there because I've been using this
for something like ten years now.

Includes a raco command, and some semi-decent command-line parsing. Run
it like this:

```
raco parse-qif src.qif dst.qif
```

You can run

```
raco parse-qif --help
```

... for some very limited ``documentation.''
