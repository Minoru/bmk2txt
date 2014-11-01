bmk2txt is a converter from CoolReader's bookmark files to simple text files

(I avoid saying "plain text" because bookmark files *are* in fact plain text.)

The output looks like this:

```
tawdry
sponging off (...someone)
Major System (remembering numbers)
```

The thing you bookmarked is printed out, and if you supplied a comment, it will
be put into parentheses and printed out as well. Clean and simple, isn't it?

It can also strip punctuation (and any other characters you want) from the
beginning and the end of the bookmarked text. That's a workaround for the way
CoolReader's text selection works. See help message for details.



I don't really expect that this will be useful to anyone, but still:

* it scratches my own itch — I use CoolReader bookmarks to store new English
  words I need to learn;
* it served as a playground where I could familiarize myself with the basics of
  `cmdargs`, `lens` and `attoparsec`.
