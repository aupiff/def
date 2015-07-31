# def

A command line interface to wiktionary. Look up any word in any language,
defined in any language. Scrapes the absymally inadequate wiktionary API to
display a nice, pretty-printed definition.

[example api call](http://en.wiktionary.org/w/api.php?action=parse&format=xml&prop=text|revid|displaytitle&callback=?&page=obnubiler)

## usage

The program takes command line arguments `source-lang definition-lang`

```
stack exec -- def fr en
```

Available language codes:
 * "en" -> English
 * "fr" -> French
 * "ru" -> Russian
 * "es" -> Spanish


saves all successfully found definitions to `dict.log` file for future reference.

example output:
```

----------------------------
crasse

Adjective
 * crass
 * (of humor) dirty, filthy
Noun
 * filth, muck
 * (especially, dirty) froth, foam
Verb
 * first-person singular present indicative of crasser
 * third-person singular present indicative of crasser
 * first-person singular present subjunctive of crasser
 * first-person singular present subjunctive of crasser
 * second-person singular imperative of crasser
----------------------------
```
