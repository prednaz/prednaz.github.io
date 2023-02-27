<?php include "/home/webpages/lima-city/rednaz/html/log.php" ?>
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="generator" content="pandoc">
  <meta name="author" content="Philipp Zander">
  <title>Ghosts of Departed Proofs by Matt Noonan [3]</title>
  <meta name="apple-mobile-web-app-capable" content="yes">
  <meta name="apple-mobile-web-app-status-bar-style" content="black-translucent">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no, minimal-ui">
  <link rel="stylesheet" href="https://unpkg.com/reveal.js@^4//dist/reset.css">
  <link rel="stylesheet" href="https://unpkg.com/reveal.js@^4//dist/reveal.css">
  <style>
    code{white-space: pre-wrap;}
    span.smallcaps{font-variant: small-caps;}
    span.underline{text-decoration: underline;}
    div.column{display: inline-block; vertical-align: top; width: 50%;}
    div.hanging-indent{margin-left: 1.5em; text-indent: -1.5em;}
    ul.task-list{list-style: none;}
    pre > code.sourceCode { white-space: pre; position: relative; }
    pre > code.sourceCode > span { display: inline-block; line-height: 1.25; }
    pre > code.sourceCode > span:empty { height: 1.2em; }
    code.sourceCode > span { color: inherit; text-decoration: inherit; }
    div.sourceCode { margin: 1em 0; }
    pre.sourceCode { margin: 0; }
    @media screen {
    div.sourceCode { overflow: auto; }
    }
    @media print {
    pre > code.sourceCode { white-space: pre-wrap; }
    pre > code.sourceCode > span { text-indent: -5em; padding-left: 5em; }
    }
    pre.numberSource code
      { counter-reset: source-line 0; }
    pre.numberSource code > span
      { position: relative; left: -4em; counter-increment: source-line; }
    pre.numberSource code > span > a:first-child::before
      { content: counter(source-line);
        position: relative; left: -1em; text-align: right; vertical-align: baseline;
        border: none; display: inline-block;
        -webkit-touch-callout: none; -webkit-user-select: none;
        -khtml-user-select: none; -moz-user-select: none;
        -ms-user-select: none; user-select: none;
        padding: 0 4px; width: 4em;
        color: #aaaaaa;
      }
    pre.numberSource { margin-left: 3em; border-left: 1px solid #aaaaaa;  padding-left: 4px; }
    div.sourceCode
      {   }
    @media screen {
    pre > code.sourceCode > span > a:first-child::before { text-decoration: underline; }
    }
    code span.al { color: #ff0000; font-weight: bold; } /* Alert */
    code span.an { color: #60a0b0; font-weight: bold; font-style: italic; } /* Annotation */
    code span.at { color: #7d9029; } /* Attribute */
    code span.bn { color: #40a070; } /* BaseN */
    code span.bu { } /* BuiltIn */
    code span.cf { color: #007020; font-weight: bold; } /* ControlFlow */
    code span.ch { color: #4070a0; } /* Char */
    code span.cn { color: #880000; } /* Constant */
    code span.co { color: #60a0b0; font-style: italic; } /* Comment */
    code span.cv { color: #60a0b0; font-weight: bold; font-style: italic; } /* CommentVar */
    code span.do { color: #ba2121; font-style: italic; } /* Documentation */
    code span.dt { color: #902000; } /* DataType */
    code span.dv { color: #40a070; } /* DecVal */
    code span.er { color: #ff0000; font-weight: bold; } /* Error */
    code span.ex { } /* Extension */
    code span.fl { color: #40a070; } /* Float */
    code span.fu { color: #06287e; } /* Function */
    code span.im { } /* Import */
    code span.in { color: #60a0b0; font-weight: bold; font-style: italic; } /* Information */
    code span.kw { color: #007020; font-weight: bold; } /* Keyword */
    code span.op { color: #666666; } /* Operator */
    code span.ot { color: #007020; } /* Other */
    code span.pp { color: #bc7a00; } /* Preprocessor */
    code span.sc { color: #4070a0; } /* SpecialChar */
    code span.ss { color: #bb6688; } /* SpecialString */
    code span.st { color: #4070a0; } /* String */
    code span.va { color: #19177c; } /* Variable */
    code span.vs { color: #4070a0; } /* VerbatimString */
    code span.wa { color: #60a0b0; font-weight: bold; font-style: italic; } /* Warning */
    .display.math{display: block; text-align: center; margin: 0.5rem auto;}
  </style>
  <link rel="stylesheet" href="https://unpkg.com/reveal.js@^4//dist/theme/white.css" id="theme">
  <link rel="stylesheet" href="../css/slides.css"/>
</head>
<body>
  <div class="reveal">
    <div class="slides">

<section id="title-slide">
  <h1 class="title">Ghosts of Departed Proofs by Matt Noonan <span class="citation" data-cites="noonan2018">[3]</span></h1>
  <p class="author">Philipp Zander</p>
</section>

<section class="slide level5">

<ul>
<li>slides on <a href="https://prednaz.github.io/posts/2021-03-08-gdp_talk.html" class="uri">https://prednaz.github.io/posts/2021-03-08-gdp_talk.html</a></li>
<li>complete example code on <a href="https://gitlab.com/rdnz/ghosts-proofs-map" class="uri">https://gitlab.com/rdnz/ghosts-proofs-map</a></li>
</ul>
</section>
<section>
<section id="expanded-return-type" class="title-slide slide level4">
<h4>expanded return type</h4>

</section>
<section id="section" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb1"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb1-1"><a href="#cb1-1" aria-hidden="true"></a><span class="co">-- | Lookup the value at a key in the map</span></span>
<span id="cb1-2"><a href="#cb1-2" aria-hidden="true"></a><span class="co">-- if the key is in the map.</span></span>
<span id="cb1-3"><a href="#cb1-3" aria-hidden="true"></a><span class="fu">lookup</span><span class="ot"> ::</span> <span class="dt">Ord</span> key <span class="ot">=&gt;</span> key <span class="ot">-&gt;</span> <span class="dt">Map</span> key value <span class="ot">-&gt;</span> <span class="dt">Maybe</span> value</span>
<span id="cb1-4"><a href="#cb1-4" aria-hidden="true"></a></span>
<span id="cb1-5"><a href="#cb1-5" aria-hidden="true"></a><span class="ot">member ::</span> <span class="dt">Ord</span> key <span class="ot">=&gt;</span> key <span class="ot">-&gt;</span> <span class="dt">Map</span> key value <span class="ot">-&gt;</span> <span class="dt">Bool</span></span></code></pre></div>
<aside class="notes">
<ul>
<li>example of a function requiring a precondition as stated in its specification</li>
<li>possibility of violated precondition represented by <code>Maybe</code></li>
<li>return type <em>expanded</em></li>
<li>traditional but problematic</li>
<li><code>member</code> checks <code>lookup</code>’s precondition</li>
</ul>
</aside>
</section>
<section id="section-1" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb2"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb2-1"><a href="#cb2-1" aria-hidden="true"></a><span class="co">-- MainExpand.hs</span></span>
<span id="cb2-2"><a href="#cb2-2" aria-hidden="true"></a></span>
<span id="cb2-3"><a href="#cb2-3" aria-hidden="true"></a><span class="kw">data</span> <span class="dt">AppError</span> <span class="ot">=</span></span>
<span id="cb2-4"><a href="#cb2-4" aria-hidden="true"></a>  <span class="dt">InvalidKey</span> <span class="dt">Integer</span> <span class="op">|</span> <span class="dt">SomeOtherProblems</span></span>
<span id="cb2-5"><a href="#cb2-5" aria-hidden="true"></a>  <span class="kw">deriving</span> (<span class="dt">Show</span>)</span>
<span id="cb2-6"><a href="#cb2-6" aria-hidden="true"></a></span>
<span id="cb2-7"><a href="#cb2-7" aria-hidden="true"></a><span class="co">-- | Validate key and map and potentially return an appropriate AppError.</span></span>
<span id="cb2-8"><a href="#cb2-8" aria-hidden="true"></a><span class="ot">validateMapKey ::</span></span>
<span id="cb2-9"><a href="#cb2-9" aria-hidden="true"></a>  <span class="dt">Integer</span>              <span class="ot">-&gt;</span></span>
<span id="cb2-10"><a href="#cb2-10" aria-hidden="true"></a>  <span class="dt">M.Map</span> <span class="dt">Integer</span> <span class="dt">String</span> <span class="ot">-&gt;</span></span>
<span id="cb2-11"><a href="#cb2-11" aria-hidden="true"></a>  <span class="dt">Maybe</span> <span class="dt">AppError</span></span>
<span id="cb2-12"><a href="#cb2-12" aria-hidden="true"></a>validateMapKey key <span class="fu">map</span> <span class="ot">=</span></span>
<span id="cb2-13"><a href="#cb2-13" aria-hidden="true"></a>  <span class="kw">case</span> M.member key <span class="fu">map</span> <span class="kw">of</span></span>
<span id="cb2-14"><a href="#cb2-14" aria-hidden="true"></a>    <span class="dt">False</span> <span class="ot">-&gt;</span> <span class="dt">Nothing</span></span>
<span id="cb2-15"><a href="#cb2-15" aria-hidden="true"></a>    <span class="dt">True</span>  <span class="ot">-&gt;</span> <span class="dt">Just</span> <span class="op">$</span> <span class="dt">InvalidKey</span> key</span>
<span id="cb2-16"><a href="#cb2-16" aria-hidden="true"></a></span>
<span id="cb2-17"><a href="#cb2-17" aria-hidden="true"></a><span class="ot">processMapKey ::</span></span>
<span id="cb2-18"><a href="#cb2-18" aria-hidden="true"></a>  <span class="dt">Integer</span>              <span class="ot">-&gt;</span></span>
<span id="cb2-19"><a href="#cb2-19" aria-hidden="true"></a>  <span class="dt">M.Map</span> <span class="dt">Integer</span> <span class="dt">String</span> <span class="ot">-&gt;</span></span>
<span id="cb2-20"><a href="#cb2-20" aria-hidden="true"></a>  <span class="dt">String</span></span>
<span id="cb2-21"><a href="#cb2-21" aria-hidden="true"></a>processMapKey key <span class="fu">map</span> <span class="ot">=</span></span>
<span id="cb2-22"><a href="#cb2-22" aria-hidden="true"></a>  <span class="kw">case</span> validateMapKey key <span class="fu">map</span> <span class="kw">of</span></span>
<span id="cb2-23"><a href="#cb2-23" aria-hidden="true"></a>    <span class="dt">Just</span> appError <span class="ot">-&gt;</span> <span class="st">&quot;Error: &quot;</span> <span class="op">&lt;&gt;</span> <span class="fu">show</span> appError</span>
<span id="cb2-24"><a href="#cb2-24" aria-hidden="true"></a>    <span class="dt">Nothing</span>       <span class="ot">-&gt;</span></span>
<span id="cb2-25"><a href="#cb2-25" aria-hidden="true"></a>      <span class="kw">case</span> M.lookup key <span class="fu">map</span> <span class="kw">of</span></span>
<span id="cb2-26"><a href="#cb2-26" aria-hidden="true"></a>        <span class="dt">Nothing</span> <span class="ot">-&gt;</span> <span class="fu">error</span> <span class="st">&quot;impossible because we just applied validateMapKey&quot;</span></span>
<span id="cb2-27"><a href="#cb2-27" aria-hidden="true"></a>        <span class="dt">Just</span> result <span class="ot">-&gt;</span> result</span></code></pre></div>
<aside class="notes">
<ul>
<li>therefore the file name</li>
<li>problems
<ul>
<li>propagated multiple levels up the call tree. information, which specific precondition of which functions was violated, is lost. repair with <code>Either</code>. The Trouble with Typed Errors</li>
<li>inefficiency of checking twice</li>
<li><code>error</code>. know preconditions satisfied, cannot reasonably handle violated precondition. change</li>
</ul></li>
<li>fundamental obstacle. refer to term level variables on the type level</li>
</ul>
</aside>
</section></section>
<section>
<section id="expanded-return-type-vs-restricted-argument-types" class="title-slide slide level4">
<h4>expanded return type vs restricted argument types</h4>

</section>
<section id="expanded-return-type-vs-restricted-argument-types-1" class="slide level5">
<h5>expanded return type vs restricted argument types</h5>
<ul>
<li><em>Parse, don’t validate</em> by Alexis King</li>
<li><em>Type Safety Back and Forth</em> by Matt Parsons</li>
<li><em>Keep your types small…</em> by Matt Parsons</li>
</ul>
</section></section>
<section>
<section id="first-attempt-at-restricted-argument-types" class="title-slide slide level4">
<h4>first attempt at restricted argument types</h4>

</section>
<section id="section-2" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb3"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb3-1"><a href="#cb3-1" aria-hidden="true"></a><span class="co">-- NamedFirst.hs</span></span>
<span id="cb3-2"><a href="#cb3-2" aria-hidden="true"></a></span>
<span id="cb3-3"><a href="#cb3-3" aria-hidden="true"></a><span class="kw">newtype</span> <span class="dt">Named</span> name a <span class="ot">=</span> <span class="dt">Named</span> a</span>
<span id="cb3-4"><a href="#cb3-4" aria-hidden="true"></a></span>
<span id="cb3-5"><a href="#cb3-5" aria-hidden="true"></a><span class="ot">name ::</span> a <span class="ot">-&gt;</span> <span class="dt">Named</span> name a</span>
<span id="cb3-6"><a href="#cb3-6" aria-hidden="true"></a>name a <span class="ot">=</span> <span class="dt">Named</span> a</span>
<span id="cb3-7"><a href="#cb3-7" aria-hidden="true"></a></span>
<span id="cb3-8"><a href="#cb3-8" aria-hidden="true"></a><span class="ot">forgetName ::</span> <span class="dt">Named</span> name a <span class="ot">-&gt;</span> a</span>
<span id="cb3-9"><a href="#cb3-9" aria-hidden="true"></a>forgetName (<span class="dt">Named</span> a) <span class="ot">=</span> a</span></code></pre></div>
<div class="sourceCode" id="cb4"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb4-1"><a href="#cb4-1" aria-hidden="true"></a><span class="co">-- MapWithProofsFirst.hs</span></span>
<span id="cb4-2"><a href="#cb4-2" aria-hidden="true"></a></span>
<span id="cb4-3"><a href="#cb4-3" aria-hidden="true"></a><span class="kw">data</span> <span class="dt">Member</span> key <span class="fu">map</span> <span class="ot">=</span> <span class="dt">Member</span></span></code></pre></div>
<aside class="notes">
<ul>
<li>Noonan’s first key idea</li>
<li>wrap term level values into pantom types</li>
<li>type variable used to refer to the wrapped term level value. name</li>
<li>example. guarantees if in scope. interpret as proof</li>
</ul>
</aside>
</section>
<section id="section-3" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb5"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb5-1"><a href="#cb5-1" aria-hidden="true"></a><span class="co">-- MapWithProofsFirst.hs</span></span>
<span id="cb5-2"><a href="#cb5-2" aria-hidden="true"></a></span>
<span id="cb5-3"><a href="#cb5-3" aria-hidden="true"></a><span class="kw">module</span> <span class="dt">MapWithProofsFirst</span> (<span class="dt">Member</span> ()) <span class="kw">where</span></span>
<span id="cb5-4"><a href="#cb5-4" aria-hidden="true"></a></span>
<span id="cb5-5"><a href="#cb5-5" aria-hidden="true"></a><span class="ot">member ::</span></span>
<span id="cb5-6"><a href="#cb5-6" aria-hidden="true"></a>  (<span class="dt">Ord</span> key)                        <span class="ot">=&gt;</span></span>
<span id="cb5-7"><a href="#cb5-7" aria-hidden="true"></a>  <span class="dt">Named</span> keyName key                <span class="ot">-&gt;</span></span>
<span id="cb5-8"><a href="#cb5-8" aria-hidden="true"></a>  <span class="dt">Named</span> mapName (<span class="dt">M.Map</span> key _value) <span class="ot">-&gt;</span></span>
<span id="cb5-9"><a href="#cb5-9" aria-hidden="true"></a>  <span class="dt">Maybe</span> (<span class="dt">Member</span> keyName mapName)</span>
<span id="cb5-10"><a href="#cb5-10" aria-hidden="true"></a>member key <span class="fu">map</span> <span class="ot">=</span></span>
<span id="cb5-11"><a href="#cb5-11" aria-hidden="true"></a>  <span class="kw">if</span> M.member (forgetName key) (forgetName <span class="fu">map</span>)</span>
<span id="cb5-12"><a href="#cb5-12" aria-hidden="true"></a>  <span class="kw">then</span> <span class="dt">Just</span> <span class="dt">Member</span></span>
<span id="cb5-13"><a href="#cb5-13" aria-hidden="true"></a>  <span class="kw">else</span> <span class="dt">Nothing</span></span></code></pre></div>
<aside class="notes">
<ul>
<li>matching names in type signature</li>
<li>fabricating proofs from thin air but not exported (better <code>Internal</code> module). only library author can assert propositions. their mistakes will not be caught</li>
<li>expressed postcondition through its type</li>
</ul>
</aside>
</section>
<section id="section-4" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb6"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb6-1"><a href="#cb6-1" aria-hidden="true"></a><span class="co">-- MapWithProofsFirst.hs</span></span>
<span id="cb6-2"><a href="#cb6-2" aria-hidden="true"></a></span>
<span id="cb6-3"><a href="#cb6-3" aria-hidden="true"></a><span class="fu">lookup</span> <span class="ot">::</span></span>
<span id="cb6-4"><a href="#cb6-4" aria-hidden="true"></a>  (<span class="dt">Ord</span> key)                       <span class="ot">=&gt;</span></span>
<span id="cb6-5"><a href="#cb6-5" aria-hidden="true"></a>  <span class="dt">Named</span> keyName key               <span class="ot">-&gt;</span></span>
<span id="cb6-6"><a href="#cb6-6" aria-hidden="true"></a>  <span class="dt">Named</span> mapName (<span class="dt">M.Map</span> key value) <span class="ot">-&gt;</span></span>
<span id="cb6-7"><a href="#cb6-7" aria-hidden="true"></a>  <span class="dt">Member</span> keyName mapName          <span class="ot">-&gt;</span></span>
<span id="cb6-8"><a href="#cb6-8" aria-hidden="true"></a>  value</span></code></pre></div>
<aside class="notes">
<ul>
<li>now precondition through its type</li>
<li>no <code>Maybe</code></li>
<li>no redundant check</li>
</ul>
</aside>
</section>
<section id="section-5" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb7"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb7-1"><a href="#cb7-1" aria-hidden="true"></a><span class="co">-- MainFirst.hs</span></span>
<span id="cb7-2"><a href="#cb7-2" aria-hidden="true"></a></span>
<span id="cb7-3"><a href="#cb7-3" aria-hidden="true"></a><span class="ot">validateMapKey ::</span></span>
<span id="cb7-4"><a href="#cb7-4" aria-hidden="true"></a>  <span class="dt">Named</span> keyName <span class="dt">Integer</span>              <span class="ot">-&gt;</span></span>
<span id="cb7-5"><a href="#cb7-5" aria-hidden="true"></a>  <span class="dt">Named</span> mapName (<span class="dt">Map</span> <span class="dt">Integer</span> <span class="dt">String</span>) <span class="ot">-&gt;</span></span>
<span id="cb7-6"><a href="#cb7-6" aria-hidden="true"></a>  <span class="dt">Either</span> <span class="dt">AppError</span> (<span class="dt">Member</span> keyName mapName)</span>
<span id="cb7-7"><a href="#cb7-7" aria-hidden="true"></a>validateMapKey key <span class="fu">map</span> <span class="ot">=</span></span>
<span id="cb7-8"><a href="#cb7-8" aria-hidden="true"></a>  <span class="kw">case</span> member key <span class="fu">map</span> <span class="kw">of</span></span>
<span id="cb7-9"><a href="#cb7-9" aria-hidden="true"></a>    <span class="dt">Just</span> proof <span class="ot">-&gt;</span> <span class="dt">Right</span> proof</span>
<span id="cb7-10"><a href="#cb7-10" aria-hidden="true"></a>    <span class="dt">Nothing</span>    <span class="ot">-&gt;</span> <span class="dt">Left</span> <span class="op">$</span> <span class="dt">InvalidKey</span> (forgetName key)</span>
<span id="cb7-11"><a href="#cb7-11" aria-hidden="true"></a></span>
<span id="cb7-12"><a href="#cb7-12" aria-hidden="true"></a><span class="ot">processMapKey ::</span></span>
<span id="cb7-13"><a href="#cb7-13" aria-hidden="true"></a>  <span class="dt">Integer</span>            <span class="ot">-&gt;</span></span>
<span id="cb7-14"><a href="#cb7-14" aria-hidden="true"></a>  <span class="dt">Map</span> <span class="dt">Integer</span> <span class="dt">String</span> <span class="ot">-&gt;</span></span>
<span id="cb7-15"><a href="#cb7-15" aria-hidden="true"></a>  <span class="dt">String</span></span>
<span id="cb7-16"><a href="#cb7-16" aria-hidden="true"></a>processMapKey key <span class="fu">map</span> <span class="ot">=</span></span>
<span id="cb7-17"><a href="#cb7-17" aria-hidden="true"></a>  <span class="kw">let</span></span>
<span id="cb7-18"><a href="#cb7-18" aria-hidden="true"></a><span class="ot">    keyNamed ::</span> <span class="dt">Named</span> keyName <span class="dt">Integer</span></span>
<span id="cb7-19"><a href="#cb7-19" aria-hidden="true"></a>    keyNamed <span class="ot">=</span> name key</span>
<span id="cb7-20"><a href="#cb7-20" aria-hidden="true"></a><span class="ot">    mapNamed ::</span> <span class="dt">Named</span> mapName (<span class="dt">Map</span> <span class="dt">Integer</span> <span class="dt">String</span>)</span>
<span id="cb7-21"><a href="#cb7-21" aria-hidden="true"></a>    mapNamed <span class="ot">=</span> name <span class="fu">map</span></span>
<span id="cb7-22"><a href="#cb7-22" aria-hidden="true"></a>  <span class="kw">in</span> <span class="kw">case</span> validateMapKey keyNamed mapNamed <span class="kw">of</span></span>
<span id="cb7-23"><a href="#cb7-23" aria-hidden="true"></a>    <span class="dt">Left</span> appError <span class="ot">-&gt;</span> <span class="st">&quot;Error: &quot;</span> <span class="op">&lt;&gt;</span> <span class="fu">show</span> appError</span>
<span id="cb7-24"><a href="#cb7-24" aria-hidden="true"></a>    <span class="dt">Right</span> proof   <span class="ot">-&gt;</span> <span class="fu">lookup</span> keyNamed mapNamed proof</span></code></pre></div>
<aside class="notes">
<ul>
<li>switch between previous version</li>
<li>no <code>error</code></li>
</ul>
</aside>
</section></section>
<section>
<section id="second-attempt-at-restricted-argument-types" class="title-slide slide level4">
<h4>second attempt at restricted argument types</h4>

</section>
<section id="section-6" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb8"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb8-1"><a href="#cb8-1" aria-hidden="true"></a><span class="co">-- MainFirst.hs</span></span>
<span id="cb8-2"><a href="#cb8-2" aria-hidden="true"></a></span>
<span id="cb8-3"><a href="#cb8-3" aria-hidden="true"></a><span class="ot">validateMapKey ::</span></span>
<span id="cb8-4"><a href="#cb8-4" aria-hidden="true"></a>  <span class="dt">Named</span> keyName <span class="dt">Integer</span>              <span class="ot">-&gt;</span></span>
<span id="cb8-5"><a href="#cb8-5" aria-hidden="true"></a>  <span class="dt">Named</span> mapName (<span class="dt">Map</span> <span class="dt">Integer</span> <span class="dt">String</span>) <span class="ot">-&gt;</span></span>
<span id="cb8-6"><a href="#cb8-6" aria-hidden="true"></a>  <span class="dt">Either</span> <span class="dt">AppError</span> (<span class="dt">Member</span> keyName mapName)</span>
<span id="cb8-7"><a href="#cb8-7" aria-hidden="true"></a>validateMapKey key <span class="fu">map</span> <span class="ot">=</span></span>
<span id="cb8-8"><a href="#cb8-8" aria-hidden="true"></a>  <span class="kw">case</span> member key <span class="fu">map</span> <span class="kw">of</span></span>
<span id="cb8-9"><a href="#cb8-9" aria-hidden="true"></a>    <span class="dt">Just</span> proof <span class="ot">-&gt;</span> <span class="dt">Right</span> proof</span>
<span id="cb8-10"><a href="#cb8-10" aria-hidden="true"></a>    <span class="dt">Nothing</span>    <span class="ot">-&gt;</span> <span class="dt">Left</span> <span class="op">$</span> <span class="dt">InvalidKey</span> (forgetName key)</span>
<span id="cb8-11"><a href="#cb8-11" aria-hidden="true"></a></span>
<span id="cb8-12"><a href="#cb8-12" aria-hidden="true"></a><span class="ot">processMapKey ::</span></span>
<span id="cb8-13"><a href="#cb8-13" aria-hidden="true"></a>  <span class="dt">Integer</span>            <span class="ot">-&gt;</span></span>
<span id="cb8-14"><a href="#cb8-14" aria-hidden="true"></a>  <span class="dt">Map</span> <span class="dt">Integer</span> <span class="dt">String</span> <span class="ot">-&gt;</span></span>
<span id="cb8-15"><a href="#cb8-15" aria-hidden="true"></a>  <span class="dt">String</span></span>
<span id="cb8-16"><a href="#cb8-16" aria-hidden="true"></a>processMapKey key <span class="fu">map</span> <span class="ot">=</span></span>
<span id="cb8-17"><a href="#cb8-17" aria-hidden="true"></a>  <span class="kw">let</span></span>
<span id="cb8-18"><a href="#cb8-18" aria-hidden="true"></a><span class="ot">    zeroNamed ::</span> <span class="dt">Named</span> zeroName <span class="dt">Integer</span></span>
<span id="cb8-19"><a href="#cb8-19" aria-hidden="true"></a>    zeroNamed <span class="ot">=</span> name <span class="dv">0</span></span>
<span id="cb8-20"><a href="#cb8-20" aria-hidden="true"></a><span class="ot">    keyNamed  ::</span> <span class="dt">Named</span> keyName <span class="dt">Integer</span></span>
<span id="cb8-21"><a href="#cb8-21" aria-hidden="true"></a>    keyNamed  <span class="ot">=</span> name key</span>
<span id="cb8-22"><a href="#cb8-22" aria-hidden="true"></a><span class="ot">    mapNamed  ::</span> <span class="dt">Named</span> mapName (<span class="dt">Map</span> <span class="dt">Integer</span> <span class="dt">String</span>)</span>
<span id="cb8-23"><a href="#cb8-23" aria-hidden="true"></a>    mapNamed  <span class="ot">=</span> name <span class="fu">map</span></span>
<span id="cb8-24"><a href="#cb8-24" aria-hidden="true"></a>  <span class="kw">in</span> <span class="kw">case</span> validateMapKey zeroNamed mapNamed <span class="kw">of</span></span>
<span id="cb8-25"><a href="#cb8-25" aria-hidden="true"></a>    <span class="dt">Left</span> appError <span class="ot">-&gt;</span> <span class="st">&quot;Error: &quot;</span> <span class="op">&lt;&gt;</span> <span class="fu">show</span> appError</span>
<span id="cb8-26"><a href="#cb8-26" aria-hidden="true"></a>    <span class="dt">Right</span> proof   <span class="ot">-&gt;</span> <span class="fu">lookup</span> keyNamed mapNamed proof</span></code></pre></div>
<aside class="notes">
<ul>
<li>should not work</li>
<li>but run time error</li>
</ul>
</aside>
</section>
<section id="section-7" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb9"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb9-1"><a href="#cb9-1" aria-hidden="true"></a><span class="ot">wellTyped ::</span> <span class="dt">Int</span></span>
<span id="cb9-2"><a href="#cb9-2" aria-hidden="true"></a>wellTyped <span class="ot">=</span></span>
<span id="cb9-3"><a href="#cb9-3" aria-hidden="true"></a>  <span class="kw">let</span></span>
<span id="cb9-4"><a href="#cb9-4" aria-hidden="true"></a><span class="ot">    aList    ::</span> [variable1]</span>
<span id="cb9-5"><a href="#cb9-5" aria-hidden="true"></a>    aList    <span class="ot">=</span> []</span>
<span id="cb9-6"><a href="#cb9-6" aria-hidden="true"></a><span class="ot">    bList    ::</span> [variable2]</span>
<span id="cb9-7"><a href="#cb9-7" aria-hidden="true"></a>    bList    <span class="ot">=</span> aList</span>
<span id="cb9-8"><a href="#cb9-8" aria-hidden="true"></a><span class="ot">    boolList ::</span> [<span class="dt">Bool</span>]</span>
<span id="cb9-9"><a href="#cb9-9" aria-hidden="true"></a>    boolList <span class="ot">=</span> aList</span>
<span id="cb9-10"><a href="#cb9-10" aria-hidden="true"></a>  <span class="kw">in</span></span>
<span id="cb9-11"><a href="#cb9-11" aria-hidden="true"></a>    <span class="fu">length</span> bList</span></code></pre></div>
</section>
<section id="section-8" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb10"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb10-1"><a href="#cb10-1" aria-hidden="true"></a><span class="ot">illTyped ::</span> [variable1] <span class="ot">-&gt;</span> ([<span class="dt">Bool</span>], [variable2])</span>
<span id="cb10-2"><a href="#cb10-2" aria-hidden="true"></a>illTyped aList <span class="ot">=</span></span>
<span id="cb10-3"><a href="#cb10-3" aria-hidden="true"></a>  <span class="kw">let</span></span>
<span id="cb10-4"><a href="#cb10-4" aria-hidden="true"></a><span class="ot">    boolList ::</span> [<span class="dt">Bool</span>]</span>
<span id="cb10-5"><a href="#cb10-5" aria-hidden="true"></a>    boolList <span class="ot">=</span> aList</span>
<span id="cb10-6"><a href="#cb10-6" aria-hidden="true"></a><span class="ot">    bList   ::</span> [variable2]</span>
<span id="cb10-7"><a href="#cb10-7" aria-hidden="true"></a>    bList   <span class="ot">=</span> aList</span>
<span id="cb10-8"><a href="#cb10-8" aria-hidden="true"></a>  <span class="kw">in</span> (boolList, bList)</span></code></pre></div>
<pre><code>    • Couldn&#39;t match type ‘variable1’ with ‘Bool’
   |
42 |     boolList = aList
   |                ^^^^^

    • Couldn&#39;t match type ‘variable2’ with ‘variable1’
   |
44 |     bList = aList
   |             ^^^^^</code></pre>
</section>
<section id="section-9" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb12"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb12-1"><a href="#cb12-1" aria-hidden="true"></a><span class="co">-- MainFirst.hs</span></span>
<span id="cb12-2"><a href="#cb12-2" aria-hidden="true"></a></span>
<span id="cb12-3"><a href="#cb12-3" aria-hidden="true"></a><span class="ot">validateMapKey ::</span></span>
<span id="cb12-4"><a href="#cb12-4" aria-hidden="true"></a>  <span class="dt">Named</span> keyName <span class="dt">Integer</span>              <span class="ot">-&gt;</span></span>
<span id="cb12-5"><a href="#cb12-5" aria-hidden="true"></a>  <span class="dt">Named</span> mapName (<span class="dt">Map</span> <span class="dt">Integer</span> <span class="dt">String</span>) <span class="ot">-&gt;</span></span>
<span id="cb12-6"><a href="#cb12-6" aria-hidden="true"></a>  <span class="dt">Either</span> <span class="dt">AppError</span> (<span class="dt">Member</span> keyName mapName)</span>
<span id="cb12-7"><a href="#cb12-7" aria-hidden="true"></a>validateMapKey key <span class="fu">map</span> <span class="ot">=</span></span>
<span id="cb12-8"><a href="#cb12-8" aria-hidden="true"></a>  <span class="kw">case</span> member key <span class="fu">map</span> <span class="kw">of</span></span>
<span id="cb12-9"><a href="#cb12-9" aria-hidden="true"></a>    <span class="dt">Just</span> proof <span class="ot">-&gt;</span> <span class="dt">Right</span> proof</span>
<span id="cb12-10"><a href="#cb12-10" aria-hidden="true"></a>    <span class="dt">Nothing</span>    <span class="ot">-&gt;</span> <span class="dt">Left</span> <span class="op">$</span> <span class="dt">InvalidKey</span> (forgetName key)</span>
<span id="cb12-11"><a href="#cb12-11" aria-hidden="true"></a></span>
<span id="cb12-12"><a href="#cb12-12" aria-hidden="true"></a><span class="ot">processMapKey ::</span></span>
<span id="cb12-13"><a href="#cb12-13" aria-hidden="true"></a>  <span class="dt">Integer</span>            <span class="ot">-&gt;</span></span>
<span id="cb12-14"><a href="#cb12-14" aria-hidden="true"></a>  <span class="dt">Map</span> <span class="dt">Integer</span> <span class="dt">String</span> <span class="ot">-&gt;</span></span>
<span id="cb12-15"><a href="#cb12-15" aria-hidden="true"></a>  <span class="dt">String</span></span>
<span id="cb12-16"><a href="#cb12-16" aria-hidden="true"></a>processMapKey key <span class="fu">map</span> <span class="ot">=</span></span>
<span id="cb12-17"><a href="#cb12-17" aria-hidden="true"></a>  <span class="kw">let</span></span>
<span id="cb12-18"><a href="#cb12-18" aria-hidden="true"></a><span class="ot">    zeroNamed ::</span> <span class="dt">Named</span> zeroName <span class="dt">Integer</span></span>
<span id="cb12-19"><a href="#cb12-19" aria-hidden="true"></a>    zeroNamed <span class="ot">=</span> name <span class="dv">0</span></span>
<span id="cb12-20"><a href="#cb12-20" aria-hidden="true"></a><span class="ot">    keyNamed  ::</span> <span class="dt">Named</span> keyName <span class="dt">Integer</span></span>
<span id="cb12-21"><a href="#cb12-21" aria-hidden="true"></a>    keyNamed  <span class="ot">=</span> name key</span>
<span id="cb12-22"><a href="#cb12-22" aria-hidden="true"></a><span class="ot">    mapNamed  ::</span> <span class="dt">Named</span> mapName (<span class="dt">Map</span> <span class="dt">Integer</span> <span class="dt">String</span>)</span>
<span id="cb12-23"><a href="#cb12-23" aria-hidden="true"></a>    mapNamed  <span class="ot">=</span> name <span class="fu">map</span></span>
<span id="cb12-24"><a href="#cb12-24" aria-hidden="true"></a>  <span class="kw">in</span> <span class="kw">case</span> validateMapKey zeroNamed mapNamed <span class="kw">of</span></span>
<span id="cb12-25"><a href="#cb12-25" aria-hidden="true"></a>    <span class="dt">Left</span> appError <span class="ot">-&gt;</span> <span class="st">&quot;Error: &quot;</span> <span class="op">&lt;&gt;</span> <span class="fu">show</span> appError</span>
<span id="cb12-26"><a href="#cb12-26" aria-hidden="true"></a>    <span class="dt">Right</span> proof   <span class="ot">-&gt;</span> <span class="fu">lookup</span> keyNamed mapNamed proof</span></code></pre></div>
<aside class="notes">
<ul>
<li>how consistently force a library user into the situation of <code>validateMapKey</code>?</li>
</ul>
</aside>
</section>
<section id="section-10" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb13"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb13-1"><a href="#cb13-1" aria-hidden="true"></a><span class="co">-- Named.hs</span></span>
<span id="cb13-2"><a href="#cb13-2" aria-hidden="true"></a></span>
<span id="cb13-3"><a href="#cb13-3" aria-hidden="true"></a><span class="ot">{-# language RankNTypes #-}</span></span>
<span id="cb13-4"><a href="#cb13-4" aria-hidden="true"></a><span class="kw">module</span> <span class="dt">Named</span> (<span class="dt">Named</span> (), name, forgetName) <span class="kw">where</span></span>
<span id="cb13-5"><a href="#cb13-5" aria-hidden="true"></a></span>
<span id="cb13-6"><a href="#cb13-6" aria-hidden="true"></a><span class="kw">newtype</span> <span class="dt">Named</span> name a <span class="ot">=</span> <span class="dt">Named</span> a</span>
<span id="cb13-7"><a href="#cb13-7" aria-hidden="true"></a></span>
<span id="cb13-8"><a href="#cb13-8" aria-hidden="true"></a><span class="ot">name ::</span></span>
<span id="cb13-9"><a href="#cb13-9" aria-hidden="true"></a>  a                                     <span class="ot">-&gt;</span></span>
<span id="cb13-10"><a href="#cb13-10" aria-hidden="true"></a>  (<span class="kw">forall</span> name<span class="op">.</span> <span class="dt">Named</span> name a <span class="ot">-&gt;</span> result) <span class="ot">-&gt;</span></span>
<span id="cb13-11"><a href="#cb13-11" aria-hidden="true"></a>  result</span>
<span id="cb13-12"><a href="#cb13-12" aria-hidden="true"></a>name a continuation <span class="ot">=</span> continuation (<span class="dt">Named</span> a)</span>
<span id="cb13-13"><a href="#cb13-13" aria-hidden="true"></a></span>
<span id="cb13-14"><a href="#cb13-14" aria-hidden="true"></a><span class="ot">forgetName ::</span> <span class="dt">Named</span> name a <span class="ot">-&gt;</span> a</span>
<span id="cb13-15"><a href="#cb13-15" aria-hidden="true"></a>forgetName (<span class="dt">Named</span> a) <span class="ot">=</span> a</span></code></pre></div>
<aside class="notes">
<ul>
<li>Noonan’s most important key idea</li>
<li>“Give me an <code>a</code> and tell me what you would do with a <code>Named name a</code>. I will do that <em>for</em> you and return you the result of that.”</li>
</ul>
</aside>
</section>
<section id="section-11" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb14"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb14-1"><a href="#cb14-1" aria-hidden="true"></a><span class="co">-- Main.hs</span></span>
<span id="cb14-2"><a href="#cb14-2" aria-hidden="true"></a></span>
<span id="cb14-3"><a href="#cb14-3" aria-hidden="true"></a><span class="ot">processMapKey ::</span></span>
<span id="cb14-4"><a href="#cb14-4" aria-hidden="true"></a>  <span class="dt">Integer</span>            <span class="ot">-&gt;</span></span>
<span id="cb14-5"><a href="#cb14-5" aria-hidden="true"></a>  <span class="dt">Map</span> <span class="dt">Integer</span> <span class="dt">String</span> <span class="ot">-&gt;</span></span>
<span id="cb14-6"><a href="#cb14-6" aria-hidden="true"></a>  <span class="dt">String</span></span>
<span id="cb14-7"><a href="#cb14-7" aria-hidden="true"></a>processMapKey key <span class="fu">map</span> <span class="ot">=</span></span>
<span id="cb14-8"><a href="#cb14-8" aria-hidden="true"></a>  name key <span class="op">$</span> \keyNamed <span class="ot">-&gt;</span></span>
<span id="cb14-9"><a href="#cb14-9" aria-hidden="true"></a>  name <span class="fu">map</span> <span class="op">$</span> \mapNamed <span class="ot">-&gt;</span></span>
<span id="cb14-10"><a href="#cb14-10" aria-hidden="true"></a>  <span class="kw">case</span> validateMapKey keyNamed mapNamed <span class="kw">of</span></span>
<span id="cb14-11"><a href="#cb14-11" aria-hidden="true"></a>    <span class="dt">Left</span> appError <span class="ot">-&gt;</span> <span class="st">&quot;Error: &quot;</span> <span class="op">&lt;&gt;</span> <span class="fu">show</span> appError</span>
<span id="cb14-12"><a href="#cb14-12" aria-hidden="true"></a>    <span class="dt">Right</span> proof   <span class="ot">-&gt;</span> <span class="fu">lookup</span> keyNamed mapNamed proof</span></code></pre></div>
<aside class="notes">
<ul>
<li><code>validateMapKey</code> unsurprisingly remains the same</li>
<li>the library happens to remain the same too</li>
</ul>
</aside>
</section>
<section id="section-12" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb15"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb15-1"><a href="#cb15-1" aria-hidden="true"></a><span class="co">-- Main.hs</span></span>
<span id="cb15-2"><a href="#cb15-2" aria-hidden="true"></a></span>
<span id="cb15-3"><a href="#cb15-3" aria-hidden="true"></a><span class="ot">processMapKey ::</span></span>
<span id="cb15-4"><a href="#cb15-4" aria-hidden="true"></a>  <span class="dt">Integer</span>            <span class="ot">-&gt;</span></span>
<span id="cb15-5"><a href="#cb15-5" aria-hidden="true"></a>  <span class="dt">Map</span> <span class="dt">Integer</span> <span class="dt">String</span> <span class="ot">-&gt;</span></span>
<span id="cb15-6"><a href="#cb15-6" aria-hidden="true"></a>  <span class="dt">String</span></span>
<span id="cb15-7"><a href="#cb15-7" aria-hidden="true"></a>processMapKey key <span class="fu">map</span> <span class="ot">=</span></span>
<span id="cb15-8"><a href="#cb15-8" aria-hidden="true"></a>  name key <span class="op">$</span> \(<span class="ot">keyNamed ::</span> <span class="dt">Named</span> keyName <span class="dt">Integer</span>)              <span class="ot">-&gt;</span></span>
<span id="cb15-9"><a href="#cb15-9" aria-hidden="true"></a>  name <span class="fu">map</span> <span class="op">$</span> \(<span class="ot">mapNamed ::</span> <span class="dt">Named</span> mapName (<span class="dt">Map</span> <span class="dt">Integer</span> <span class="dt">String</span>)) <span class="ot">-&gt;</span></span>
<span id="cb15-10"><a href="#cb15-10" aria-hidden="true"></a>  <span class="kw">case</span> validateMapKey keyNamed mapNamed <span class="kw">of</span></span>
<span id="cb15-11"><a href="#cb15-11" aria-hidden="true"></a>    <span class="dt">Left</span> appError <span class="ot">-&gt;</span> <span class="st">&quot;Error: &quot;</span> <span class="op">&lt;&gt;</span> <span class="fu">show</span> appError</span>
<span id="cb15-12"><a href="#cb15-12" aria-hidden="true"></a>    <span class="dt">Right</span> proof   <span class="ot">-&gt;</span> <span class="fu">lookup</span> keyNamed mapNamed proof</span></code></pre></div>
<aside class="notes">
<ul>
<li>ScopedTypeVariables not necessary</li>
</ul>
</aside>
</section>
<section id="section-13" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb16"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb16-1"><a href="#cb16-1" aria-hidden="true"></a><span class="co">-- Main.hs</span></span>
<span id="cb16-2"><a href="#cb16-2" aria-hidden="true"></a></span>
<span id="cb16-3"><a href="#cb16-3" aria-hidden="true"></a><span class="ot">processMapKey ::</span></span>
<span id="cb16-4"><a href="#cb16-4" aria-hidden="true"></a>  <span class="dt">Integer</span>            <span class="ot">-&gt;</span></span>
<span id="cb16-5"><a href="#cb16-5" aria-hidden="true"></a>  <span class="dt">Map</span> <span class="dt">Integer</span> <span class="dt">String</span> <span class="ot">-&gt;</span></span>
<span id="cb16-6"><a href="#cb16-6" aria-hidden="true"></a>  <span class="dt">String</span></span>
<span id="cb16-7"><a href="#cb16-7" aria-hidden="true"></a>processMapKey key <span class="fu">map</span> <span class="ot">=</span></span>
<span id="cb16-8"><a href="#cb16-8" aria-hidden="true"></a>  name <span class="dv">0</span>   <span class="op">$</span> \zeroNamed <span class="ot">-&gt;</span></span>
<span id="cb16-9"><a href="#cb16-9" aria-hidden="true"></a>  name key <span class="op">$</span> \keyNamed  <span class="ot">-&gt;</span></span>
<span id="cb16-10"><a href="#cb16-10" aria-hidden="true"></a>  name <span class="fu">map</span> <span class="op">$</span> \mapNamed  <span class="ot">-&gt;</span></span>
<span id="cb16-11"><a href="#cb16-11" aria-hidden="true"></a>  <span class="kw">case</span> validateMapKey zeroNamed mapNamed <span class="kw">of</span></span>
<span id="cb16-12"><a href="#cb16-12" aria-hidden="true"></a>    <span class="dt">Left</span> appError <span class="ot">-&gt;</span> <span class="st">&quot;Error: &quot;</span> <span class="op">&lt;&gt;</span> <span class="fu">show</span> appError</span>
<span id="cb16-13"><a href="#cb16-13" aria-hidden="true"></a>    <span class="dt">Right</span> proof   <span class="ot">-&gt;</span> <span class="fu">lookup</span> keyNamed mapNamed proof</span></code></pre></div>
<pre><code>    • Couldn&#39;t match type ‘name1’ with ‘name’
      Expected type: Member name1 name2
        Actual type: Member name name2
   |
37 |     Right proof -&gt; lookup keyNamed mapNamed proof
   |                                             ^^^^^</code></pre>
</section></section>
<section>
<section id="propositional-logic" class="title-slide slide level4">
<h4>propositional logic</h4>

</section>
<section id="section-14" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb18"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb18-1"><a href="#cb18-1" aria-hidden="true"></a><span class="co">-- MapWithProofs.hs</span></span>
<span id="cb18-2"><a href="#cb18-2" aria-hidden="true"></a></span>
<span id="cb18-3"><a href="#cb18-3" aria-hidden="true"></a><span class="ot">insert ::</span></span>
<span id="cb18-4"><a href="#cb18-4" aria-hidden="true"></a>  (<span class="dt">Ord</span> key)                       <span class="ot">=&gt;</span></span>
<span id="cb18-5"><a href="#cb18-5" aria-hidden="true"></a>  <span class="dt">Named</span> keyName key               <span class="ot">-&gt;</span></span>
<span id="cb18-6"><a href="#cb18-6" aria-hidden="true"></a>  value                           <span class="ot">-&gt;</span></span>
<span id="cb18-7"><a href="#cb18-7" aria-hidden="true"></a>  <span class="dt">Named</span> mapName (<span class="dt">M.Map</span> key value) <span class="ot">-&gt;</span></span>
<span id="cb18-8"><a href="#cb18-8" aria-hidden="true"></a>  (<span class="kw">forall</span> resultName<span class="op">.</span> <span class="dt">InsertResult</span> keyName key value mapName resultName <span class="ot">-&gt;</span> r) <span class="ot">-&gt;</span></span>
<span id="cb18-9"><a href="#cb18-9" aria-hidden="true"></a>  r</span>
<span id="cb18-10"><a href="#cb18-10" aria-hidden="true"></a></span>
<span id="cb18-11"><a href="#cb18-11" aria-hidden="true"></a><span class="kw">type</span> <span class="dt">InsertResult</span> keyName key value mapName resultName <span class="ot">=</span></span>
<span id="cb18-12"><a href="#cb18-12" aria-hidden="true"></a>  (</span>
<span id="cb18-13"><a href="#cb18-13" aria-hidden="true"></a>    <span class="dt">Named</span> resultName (<span class="dt">M.Map</span> key value),</span>
<span id="cb18-14"><a href="#cb18-14" aria-hidden="true"></a>    <span class="dt">Member</span> keyName resultName,</span>
<span id="cb18-15"><a href="#cb18-15" aria-hidden="true"></a>    mapName <span class="ot">`KeySubset`</span> resultName</span>
<span id="cb18-16"><a href="#cb18-16" aria-hidden="true"></a>  )</span>
<span id="cb18-17"><a href="#cb18-17" aria-hidden="true"></a></span>
<span id="cb18-18"><a href="#cb18-18" aria-hidden="true"></a><span class="kw">data</span> map1 <span class="ot">`KeySubset`</span> map2 <span class="ot">=</span> <span class="dt">KeySubset</span></span></code></pre></div>
</section>
<section id="section-15" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb19"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb19-1"><a href="#cb19-1" aria-hidden="true"></a><span class="co">-- Main.hs</span></span>
<span id="cb19-2"><a href="#cb19-2" aria-hidden="true"></a></span>
<span id="cb19-3"><a href="#cb19-3" aria-hidden="true"></a><span class="ot">insertExample ::</span> <span class="dt">String</span></span>
<span id="cb19-4"><a href="#cb19-4" aria-hidden="true"></a>insertExample <span class="ot">=</span></span>
<span id="cb19-5"><a href="#cb19-5" aria-hidden="true"></a>  name <span class="dv">0</span>                   <span class="op">$</span> \key0                             <span class="ot">-&gt;</span></span>
<span id="cb19-6"><a href="#cb19-6" aria-hidden="true"></a>  name <span class="dv">1</span>                   <span class="op">$</span> \key1                             <span class="ot">-&gt;</span></span>
<span id="cb19-7"><a href="#cb19-7" aria-hidden="true"></a>  name (M.singleton <span class="dv">0</span> <span class="st">&quot;a&quot;</span>) <span class="op">$</span> \mapOld                           <span class="ot">-&gt;</span></span>
<span id="cb19-8"><a href="#cb19-8" aria-hidden="true"></a>  insert key1 <span class="st">&quot;b&quot;</span> mapOld   <span class="op">$</span> \(mapNew, _proofKey, proofSubset) <span class="ot">-&gt;</span></span>
<span id="cb19-9"><a href="#cb19-9" aria-hidden="true"></a>  <span class="kw">case</span> member key0 mapOld <span class="kw">of</span></span>
<span id="cb19-10"><a href="#cb19-10" aria-hidden="true"></a>    <span class="dt">Nothing</span>    <span class="ot">-&gt;</span> <span class="fu">error</span> <span class="st">&quot;impossible because `0` is member of `M.singleton 0 a`&quot;</span></span>
<span id="cb19-11"><a href="#cb19-11" aria-hidden="true"></a>    <span class="dt">Just</span> proof <span class="ot">-&gt;</span> <span class="fu">lookup</span> key0 mapNew (keySubset proofSubset proof)</span></code></pre></div>
<div class="sourceCode" id="cb20"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb20-1"><a href="#cb20-1" aria-hidden="true"></a><span class="co">-- MapWithProofs.hs</span></span>
<span id="cb20-2"><a href="#cb20-2" aria-hidden="true"></a></span>
<span id="cb20-3"><a href="#cb20-3" aria-hidden="true"></a><span class="ot">keySubset ::</span></span>
<span id="cb20-4"><a href="#cb20-4" aria-hidden="true"></a>  map1 <span class="ot">`KeySubset`</span> map2 <span class="ot">-&gt;</span></span>
<span id="cb20-5"><a href="#cb20-5" aria-hidden="true"></a>  <span class="dt">Member</span> key map1       <span class="ot">-&gt;</span></span>
<span id="cb20-6"><a href="#cb20-6" aria-hidden="true"></a>  <span class="dt">Member</span> key map2</span>
<span id="cb20-7"><a href="#cb20-7" aria-hidden="true"></a>keySubset _proof1 _proof2 <span class="ot">=</span> <span class="dt">Member</span></span></code></pre></div>
</section>
<section id="section-16" class="slide level5">
<h5></h5>
<div class="sourceCode" id="cb21"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb21-1"><a href="#cb21-1" aria-hidden="true"></a><span class="co">-- MapWithProofs.hs</span></span>
<span id="cb21-2"><a href="#cb21-2" aria-hidden="true"></a></span>
<span id="cb21-3"><a href="#cb21-3" aria-hidden="true"></a><span class="ot">keySubset ::</span></span>
<span id="cb21-4"><a href="#cb21-4" aria-hidden="true"></a>  <span class="dt">Either</span></span>
<span id="cb21-5"><a href="#cb21-5" aria-hidden="true"></a>    ((map1 <span class="ot">`KeySubset`</span> map2, <span class="dt">Member</span> key map1) <span class="ot">-&gt;</span> <span class="dt">Void</span>)</span>
<span id="cb21-6"><a href="#cb21-6" aria-hidden="true"></a>    (<span class="dt">Member</span> key map2)</span></code></pre></div>
<p><span class="math display">((<em>S</em> ∧ <em>M</em>) ⇒ ⊥) ∨ <em>N</em></span></p>
<div class="sourceCode" id="cb22"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb22-1"><a href="#cb22-1" aria-hidden="true"></a><span class="co">-- Main.hs</span></span>
<span id="cb22-2"><a href="#cb22-2" aria-hidden="true"></a></span>
<span id="cb22-3"><a href="#cb22-3" aria-hidden="true"></a><span class="ot">myKeySubset ::</span></span>
<span id="cb22-4"><a href="#cb22-4" aria-hidden="true"></a>  map1 <span class="ot">`KeySubset`</span> map2 <span class="ot">-&gt;</span></span>
<span id="cb22-5"><a href="#cb22-5" aria-hidden="true"></a>  <span class="dt">Member</span> key map1       <span class="ot">-&gt;</span></span>
<span id="cb22-6"><a href="#cb22-6" aria-hidden="true"></a>  <span class="dt">Member</span> key map2</span>
<span id="cb22-7"><a href="#cb22-7" aria-hidden="true"></a>myKeySubset proof1 proof2 <span class="ot">=</span></span>
<span id="cb22-8"><a href="#cb22-8" aria-hidden="true"></a>  <span class="kw">case</span> keySubset <span class="kw">of</span></span>
<span id="cb22-9"><a href="#cb22-9" aria-hidden="true"></a>    <span class="dt">Right</span> proof      <span class="ot">-&gt;</span> proof</span>
<span id="cb22-10"><a href="#cb22-10" aria-hidden="true"></a>    <span class="dt">Left</span> implication <span class="ot">-&gt;</span> absurd <span class="op">$</span> implication (proof1, proof2)</span></code></pre></div>
<p><span class="math display"><em>S</em> ⇒ (<em>M</em> ⇒ <em>N</em>)</span></p>
</section></section>
<section>
<section id="other" class="title-slide slide level4">
<h4>other</h4>

</section>
<section id="lightweight-existential-types" class="slide level5">
<h5>lightweight existential types</h5>
<div class="sourceCode" id="cb23"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb23-1"><a href="#cb23-1" aria-hidden="true"></a><span class="co">-- Named.hs</span></span>
<span id="cb23-2"><a href="#cb23-2" aria-hidden="true"></a></span>
<span id="cb23-3"><a href="#cb23-3" aria-hidden="true"></a><span class="ot">{-# language RankNTypes #-}</span></span>
<span id="cb23-4"><a href="#cb23-4" aria-hidden="true"></a><span class="ot">name ::</span></span>
<span id="cb23-5"><a href="#cb23-5" aria-hidden="true"></a>  a                                     <span class="ot">-&gt;</span></span>
<span id="cb23-6"><a href="#cb23-6" aria-hidden="true"></a>  (<span class="kw">forall</span> name<span class="op">.</span> <span class="dt">Named</span> name a <span class="ot">-&gt;</span> result) <span class="ot">-&gt;</span></span>
<span id="cb23-7"><a href="#cb23-7" aria-hidden="true"></a>  result</span>
<span id="cb23-8"><a href="#cb23-8" aria-hidden="true"></a>name a continuation <span class="ot">=</span> continuation (<span class="dt">Named</span> a)</span></code></pre></div>
<div class="sourceCode" id="cb24"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb24-1"><a href="#cb24-1" aria-hidden="true"></a><span class="co">-- Main.hs</span></span>
<span id="cb24-2"><a href="#cb24-2" aria-hidden="true"></a></span>
<span id="cb24-3"><a href="#cb24-3" aria-hidden="true"></a><span class="ot">insertExample ::</span> <span class="dt">String</span></span>
<span id="cb24-4"><a href="#cb24-4" aria-hidden="true"></a>insertExample <span class="ot">=</span></span>
<span id="cb24-5"><a href="#cb24-5" aria-hidden="true"></a>  name <span class="dv">0</span>                   <span class="op">$</span> \key0                             <span class="ot">-&gt;</span></span>
<span id="cb24-6"><a href="#cb24-6" aria-hidden="true"></a>  name <span class="dv">1</span>                   <span class="op">$</span> \key1                             <span class="ot">-&gt;</span></span>
<span id="cb24-7"><a href="#cb24-7" aria-hidden="true"></a>  name (M.singleton <span class="dv">0</span> <span class="st">&quot;a&quot;</span>) <span class="op">$</span> \mapOld                           <span class="ot">-&gt;</span></span>
<span id="cb24-8"><a href="#cb24-8" aria-hidden="true"></a>  insert key1 <span class="st">&quot;b&quot;</span> mapOld   <span class="op">$</span> \(mapNew, _proofKey, proofSubset) <span class="ot">-&gt;</span></span>
<span id="cb24-9"><a href="#cb24-9" aria-hidden="true"></a>  <span class="kw">case</span> member key0 mapOld <span class="kw">of</span></span>
<span id="cb24-10"><a href="#cb24-10" aria-hidden="true"></a>    <span class="dt">Nothing</span>    <span class="ot">-&gt;</span> <span class="fu">error</span> <span class="st">&quot;impossible because `0` is member of `M.singleton 0 a`&quot;</span></span>
<span id="cb24-11"><a href="#cb24-11" aria-hidden="true"></a>    <span class="dt">Just</span> proof <span class="ot">-&gt;</span> <span class="fu">lookup</span> key0 mapNew (keySubset proofSubset proof)</span></code></pre></div>
</section>
<section id="lightweight-existential-types-1" class="slide level5">
<h5>lightweight existential types</h5>
<ul>
<li>GHC core developer Richard Eisenberg hopes to submit paper on <em>lightweight existential types</em> for ICFP 2021 by 2021-03-02 <span class="citation" data-cites="eisenberg_berlin">[1]</span></li>
<li>not motivated by <em>Ghosts of Departed Proofs</em> but interestingly by the other two approaches to formal verification of <em>Dependent Haskell</em> and <em>LiquidHaskell</em> <span class="citation" data-cites="eisenberg_tweag">[2]</span></li>
<li><code>exists</code> keyword that syntactically works like <code class="sourceCode haskell"><span class="kw">forall</span></code> <span class="citation" data-cites="eisenberg_tweag">[2]</span></li>
<li>hopes to have a new extension to GHC roughly by May <span class="citation" data-cites="eisenberg_berlin">[1]</span></li>
</ul>
</section>
<section id="lightweight-existential-types-2" class="slide level5">
<h5>lightweight existential types</h5>
<div class="sourceCode" id="cb25"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb25-1"><a href="#cb25-1" aria-hidden="true"></a><span class="ot">name ::</span> exists name<span class="op">.</span> a <span class="ot">-&gt;</span> <span class="dt">Named</span> name a</span></code></pre></div>
</section>
<section id="comparison-with-smt-based-verification-and-type-theory-based-verification" class="slide level5">
<h5>comparison with SMT based verification and type theory based verification</h5>
<ul>
<li>propositional logic only, no predicate logic
<ul>
<li>lacks numbers and data structures</li>
<li>there is a reason why formal verification uses SMT instead of SAT</li>
<li>often impossible to export enough axioms to support all kinds of use cases</li>
</ul></li>
<li>manual proofs like with dependent types and unlike SMT based verification</li>
<li>library code is not verified</li>
</ul>
</section>
<section id="reading-recommendation" class="slide level5">
<h5>reading recommendation</h5>
<p><a href="https://ocharles.org.uk/blog/posts/2019-08-09-who-authorized-these-ghosts.html" class="uri">https://ocharles.org.uk/blog/posts/2019-08-09-who-authorized-these-ghosts.html</a> contains a great example demonstrating the Curry–Howard isomorphism well and using the technique to avoid redundant checks</p>
<aside class="notes">
<ul>
<li>ADTs instead of tuple and <code>Either</code></li>
</ul>
</aside>
</section>
<section id="references" class="slide level5 unnumbered">
<h5 class="unnumbered">References</h5>
<div id="refs" class="references" role="doc-bibliography">
<div id="ref-eisenberg_berlin">
<p>[1] Richard Eisenberg. 2020. Richard eisenberg on simplifying constraint solving in ghc. Retrieved February 18, 2021 from <a href="https://www.youtube.com/watch?v=flwz6V5Ps8w&amp;t=3m41s">https://www.youtube.com/watch?v=flwz6V5Ps8w&amp;t=3m41s</a></p>
</div>
<div id="ref-eisenberg_tweag">
<p>[2] Richard Eisenberg. 2021. Update on dependent haskell. Retrieved February 18, 2021 from <a href="https://www.youtube.com/watch?v=TXDivoj1v6w&amp;t=5m17s">https://www.youtube.com/watch?v=TXDivoj1v6w&amp;t=5m17s</a></p>
</div>
<div id="ref-noonan2018">
<p>[3] Matt Noonan. 2018. Ghosts of departed proofs (functional pearl). In <em>Proceedings of the 11th acm sigplan international symposium on haskell</em> (Haskell 2018), Association for Computing Machinery, New York, NY, USA, 119–131. DOI:<a href="https://doi.org/10.1145/3242744.3242755">https://doi.org/10.1145/3242744.3242755</a></p>
</div>
</div>
</section></section>
    </div>
  </div>

  <script src="https://unpkg.com/reveal.js@^4//dist/reveal.js"></script>

  // reveal.js plugins
  <script src="https://unpkg.com/reveal.js@^4//plugin/notes/notes.js"></script>
  <script src="https://unpkg.com/reveal.js@^4//plugin/search/search.js"></script>
  <script src="https://unpkg.com/reveal.js@^4//plugin/zoom/zoom.js"></script>

  <script>

      // Full list of configuration options available at:
      // https://revealjs.com/config/
      Reveal.initialize({
        // Push each slide change to the browser history
        history: true,
        // The "normal" size of the presentation, aspect ratio will be preserved
        // when the presentation is scaled to fit different resolutions. Can be
        // specified using percentage units.
        width: 1060,

        // reveal.js plugins
        plugins: [
          RevealNotes,
          RevealSearch,
          RevealZoom
        ]
      });
    </script>
    </body>
</html>