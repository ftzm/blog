---
title: Syntax Highlighting with Markdow in Python
published: 2015-09-23
teaser: Syntax highlighting is just as useful in a blog as in an IDE. Your code will be easier for readers to follow, and it will look more polished to boot. It's a simple feature, adding very little code.
tags: [python, flask]
---

It seems that most of today's coding blogs have some form of syntax highlighting built in. I wanted to join in on this trend while making this blog, but found surprisingly few explanations of how to implement it. It tuns out it's a simple feature, adding very little code. Below I will show how to implement it in a python application.

## Markdown

Markdown is now a well established markup language that greatly simplifies the process of writing for the web. The minimalistic markdown syntax is converted to html that can be inserted into a web page. Additionally, some implementations of Markdown are extended to allow Syntax highlighting. All that is required is to designate lengths of code, and different terms will automatically be wrapped in html tags to which colorizing css is later applied.

## Necessary Packages

There are multiple python packages for Markdown, but I found [markdown2](https://github.com/trentm/python-markdown2) to be the most straightforward when it came to implementing syntax highlighting. For syntax highlighting, you will also need pygments. You should be using pip to install packages.

``` bash
$ pip install markdown2 pygments
```

## Designating code

In markdown2, syntax highlighting is done via an 'extra' called 'fenced-code-blocks'. With this,  all this is required is to insert three backticks on the lines preceding and following a stretch of code. To ensure the right programming language is selected for highlighting, append its name to the first set of backticks.

``` markdown
    ``` python
    class SomeCode():
        pass
    ```
```


## Converting Markdown

At some point, markdown formatted text  will have to be converted into html. All that is required for this is the following code, where "markdown_version" refers to the string of markdown text you wish to convert.

``` python
import markdown2
...
html_version = markdown2.markdown(markdown_version, extras=["fenced-code-blocks"])
```

It is necessary to specify "fenced-code-blocks" as an extra so that markdown2 uses it to apply syntax highlighting tags. Otherwise those backtick-fenced stretches of code will be ignored by markdown. If everything works as it should, html_version should contain a string like the following:

``` html
<div class="codehilite"><pre><code><span class="k">class</span> <span class="nc">SomeCode</span><span class="p">():</span>
<span class="k">pass</span>
</code></pre></div>
```

As you can see, different syntactic categories are tagged. However, accompanying css is necessary to apply color.

## The CSS

Applying css is as simple as downloading a colorscheme and applying it as you would any other css stylesheet. Colorschemes for pygments can be downloaded [here](https://github.com/richleland/pygments-css). Pick one that suits your taste, and plug it into your base.html, or wherever your <head> lives:

``` html
<link rel="stylesheet" type="text/css" href="url/for/your/colorscheme.css" />
```
