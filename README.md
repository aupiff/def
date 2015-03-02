# def

A command line interface to wiktionary. Look up any word in any language,
defined in any language. Scrapes the absymally inadequate wiktionary API to
display a nice, pretty-printed definition.

[example api call](http://en.wiktionary.org/w/api.php?action=parse&format=xml&prop=text|revid|displaytitle&callback=?&page=obnubiler)

## usage

The program takes command line arguments `source-lang definition-lang`

```
cabal run -- fr en
```

Available language codes:
 * "en" -> English
 * "fr" -> French
 * "ru" -> Russian
 * "es" -> Spanish


saves all successfully found definitions to `dict.log` file for future reference.
