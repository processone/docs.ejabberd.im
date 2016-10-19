# ejabberd Documentation

This repository holds the source code of the ejabberd documentation website, available at [docs.ejabberd.im](https://docs.ejabberd.im). This is a community effort and you are welcome to submit issues or pull requests in order to improve the docs and benefit the ejabberd community.

# Contributing

If you want to contribute to this ejabberd documentation, here are some helpful guidelines regarding the syntax:

- use Markdown, specifically the [kramdown flavour](http://kramdown.gettalong.org/syntax.html)

- when creating code blocks, use `~~~ language` fencing, where `language` represents what's in the code block

- for code blocks with terminal listings, use `~~~ bash` or `~~~ python`, whichever looks best

- ordered lists don't need to be enumerated 1,2,3... in the source - they can all start with `1.`, the generator will enumerate them properly

- for code blocks within lists, make sure indentation of first `~~~` is vertically aligned with list item text, for example:
    ```
    1. Item list text
    
       ~~~ perl
       my $variable = 'something';
       ...
       ~~~
    ```
    This is generally 3 spaces for numbered list item and 2 for bullet items.

- It is usually better to indent at same level the whole code block (so in list item), otherwise, empty lines will break rendering.
