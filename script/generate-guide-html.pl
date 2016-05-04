#!/usr/bin/perl

use strict;
use File::Slurp;
use IPC::Run3;

my $index = read_file("content/admin/guide/index.md");
my @files;
my %refs;
my $idx = 0;

while ($index =~ /\[.*?\]\[(.*?)\]/g) {
    $refs{$1} = $idx++;
}

while ($index =~ /\[(.*?)\]:\s*(\S+)/g) {
    next unless exists $refs{$1};
    print "$2\n";
    $files[$refs{$1}] = -f "content".$2."index.md" ? $2."index/" : $2;
}

my @css = ("static/shared/css/docs.style.css", "static/shared/css/pygments.min.css");
my $css = join("\n", map {read_file($_)} @css);
$css =~ s/\s*\n\s*//g;
$css =~ s/\/\*.*?\*\///g;

$css.= "
.TOC { float: left; width: 20% }
#content { float: left; width: 75%; padding-left: 2em; padding-right: 2em; }
code {
    background: inherit;
    color: inherit;
    margin: 0px;
    padding: 0px;
    font-size: inherit !important;
    font-family: inherit;
}
#sidebar ul ul { display: block !important; }
";
$css =~ s/\n/\n    /g;

my $in;

$in="---
title: ejabberd Installation and Operation Guide
bodyclass: nocomment
HTML header: <style>$css</style>
    <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/8.8.0/styles/solarized_dark.min.css\">
    <script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/8.8.0/highlight.min.js\"></script>
    <script>hljs.initHighlightingOnLoad();</script>
---

{{TOC}}
<div id='content'>
";

sub fix_link {
    my ($t) = @_;
    $t =~ s/-//g;
    return $t;
}

$idx = 1;
for (@files) {
    s/\/$/.md/;
    my $content = read_file("content".$_);
    $content =~ s/^---.*?---\s*//s;
    $content =~ s/\[(.*?)\]\[(.*?)\]/[$1][$idx$2]/g;
    $content =~ s/\[(.*?)\]\((.*?)\)/"[$1](".fix_link($2).")"/ge;
    $content =~ s/\[(.*?)\]:\s*(?:\/admin\S+(#\S+))?/"[$idx$1]: ".($2 ? fix_link($2) : "")/ge;
#    $content =~ s/\[(.*?)\]:/[$idx$1]:/g;
    $content =~ s/.*\n\{:toc\}//g;
    $in.= "$content\n\n";
    $idx++;
}

$in =~ s/\xe2\x80\x93/-/g;
$in =~ s/\xe2\x80\x94/-/g;
$in =~ s/\xe2\x80\xa6/.../g;
$in =~ s/\xe2\x80\x9c/"/g;
$in =~ s/\xe2\x80\x9d/"/g;
$in =~ s/\xe2\x80\x98/'/g;
$in =~ s/\xe2\x80\x99/'/g;

write_file("combined.md", $in);
my $out;
run3("./multimarkdown --nosmart", \$in, \$out, \*STDERR);
#my $out = markdown($in, {document_format => "complete"});

$out =~ s/class="TOC"/id="sidebar" class="TOC"/;
my %maptype = ("console" => "bash");
$out =~ s/<code>\#!(\S+)\n?/"<code class=\"".($maptype{$1}||$1)."\">"/eg;

write_file("guide.html", $out);
