# ejabberd Documentation

This repository holds the source code of the ejabberd documentation website, available at [docs.ejabberd.im](https://docs.ejabberd.im). This is an Open Documentation, and you are welcome to submit issues or pull requests in order to improve the docs and benefit the ejabberd community.

# Contributing

If you want to contribute to this ejabberd documentation, here are some helpful guidelines regarding the syntax:

- please refer to [README-deploy.md](README-deploy.md) on how to create a local testing environment
- use Markdown, specifically the [kramdown flavour](http://kramdown.gettalong.org/syntax.html)
- when creating code blocks, use `~~~ language` fencing, where `language` represents what's in the code block
- for code blocks with terminal listings, use `~~~ bash` or `~~~ python`, whichever looks best
- for code blocks within lists, make sure indentation of first `~~~` is vertically aligned with list item text, for example:
  ```
  1. Item list text
  
     ~~~ perl
  my $variable = 'something';
  ~~~
  
     • Nested item list text
     
       ~~~ perl
  my $nested_var = 'or nothing';
  ~~~
  ```
- while we are at it, ordered lists don't need to be enumerated 1,2,3... in the source - they can all start with `1.`, the generator will enumerate them properly