<?php include "/home/webpages/lima-city/rednaz/html/log.php" ?>
<!doctype html>
<html lang="en">
    <head>
        <meta charset="utf-8">
        <meta http-equiv="x-ua-compatible" content="ie=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <title>philipp - a trick from Matt Noonan's Ghosts of Departed Proofs</title>
        <link rel="stylesheet" href="../css/pandoc_syntax.css" />
        <link rel="stylesheet" href="../css/default.css" />
    </head>
    <body>
        <header>
            <div class="logo">
                <a href="../">philipp</a>
            </div>
            <nav>
                <a href="../">home</a>
                <a href="../about.php">about</a>
                <a href="../projects.php">projects</a>
            </nav>
        </header>

        <main role="main">
            <h1>a trick from Matt Noonan's Ghosts of Departed Proofs</h1>
            <article>
    <section class="header">
        Posted on February  1, 2021
        
    </section>
    <section>
        <p>The violation of preconditions is a main cause of incorrect semantics
including run time errors. The approach to functions requiring
preconditions that is traditionally applied in the Haskell community
as described below addresses the problem poorly and suffers from
multiple problems. Noonan <span class="citation" data-cites="noonan2018">[@noonan2018]</span> presents a novel technique to
design library APIs leveraging the type system to ensure that any
preconditions are satisfied. Work currently in progress by researchers
and GHC core developers has the potential to make this technique
vastly more ergonomic.</p>
<h1 id="the-problem">The Problem</h1>
<p>For example, consider a Haskell library for working with sorted
lists. The entire example code from this paper can be found on
<a href="https://gitlab.com/rdnz/ghosts-proofs-example" class="uri">https://gitlab.com/rdnz/ghosts-proofs-example</a>. The library shall
provide a function to merge two lists into a single sorted list if the
input lists were already sorted.</p>
<div class="sourceCode" id="cb1"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb1-1"><a href="#cb1-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MergeExpand.hs</span></span>
<span id="cb1-2"><a href="#cb1-2" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb1-3"><a href="#cb1-3" aria-hidden="true" tabindex="-1"></a><span class="co">-- | Merge two lists into one sorted list</span></span>
<span id="cb1-4"><a href="#cb1-4" aria-hidden="true" tabindex="-1"></a><span class="co">-- if the input lists are already sorted.</span></span>
<span id="cb1-5"><a href="#cb1-5" aria-hidden="true" tabindex="-1"></a><span class="co">-- The first argument defines the ordering.</span></span>
<span id="cb1-6"><a href="#cb1-6" aria-hidden="true" tabindex="-1"></a><span class="ot">mergeBy ::</span> (a <span class="ot">-&gt;</span> a <span class="ot">-&gt;</span> <span class="dt">Ordering</span>) <span class="ot">-&gt;</span> [a] <span class="ot">-&gt;</span> [a] <span class="ot">-&gt;</span> <span class="dt">Maybe</span> [a]</span></code></pre></div>
<p>This is an example of a function requiring a precondition as stated in
its specification. The input lists must already be sorted in the order
defined by the first argument, the comparator. The possibility of a
violated precondition is represented by the <code class="sourceCode haskell"><span class="dt">Maybe</span></code> return
type forcing library users to handle a violated precondition. The
return type is <em>expanded</em> by the value <code class="sourceCode haskell"><span class="dt">Nothing</span></code> because the
function cannot reasonably return a list if the precondition is
violated. That is why I called the file <code>MergeExpand.hs</code>.</p>
<p>This approach to functions requiring preconditions is common in the
Haskell community. Examples are the <code class="sourceCode haskell"><span class="fu">lookup</span></code> functions in
the <code>base</code> and <code>containers</code> package <span class="citation" data-cites="hoogle_lookup">[@hoogle_lookup]</span>. But there are
multiple problems with this approach as will soon become evident.</p>
<p>I will later call the file containing Noonan’s <span class="citation" data-cites="noonan2018">[@noonan2018]</span> novel
approach simply <code>Merge.hs</code> to distinguish it from the traditional
approach of expanding the return type. But before that, I will present
a first unsuccessful attempt and call that file <code>MergeFirst.hs</code>.</p>
<p>But let us get back to <code>MergeExpand.hs</code> for now. This library shall
also provide a function to check <code class="sourceCode haskell">mergeBy</code>’s precondition.</p>
<div class="sourceCode" id="cb2"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb2-1"><a href="#cb2-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MergeExpand.hs</span></span>
<span id="cb2-2"><a href="#cb2-2" aria-hidden="true" tabindex="-1"></a><span class="ot">sortedBy ::</span> (a <span class="ot">-&gt;</span> a <span class="ot">-&gt;</span> <span class="dt">Ordering</span>) <span class="ot">-&gt;</span> [a] <span class="ot">-&gt;</span> <span class="dt">Bool</span></span></code></pre></div>
<p>Now consider an example of how this library might be used. The library
user might have a data type to represent errors in their application</p>
<div class="sourceCode" id="cb3"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb3-1"><a href="#cb3-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MainExpand.hs</span></span>
<span id="cb3-2"><a href="#cb3-2" aria-hidden="true" tabindex="-1"></a><span class="kw">data</span> <span class="dt">AppError</span> <span class="ot">=</span></span>
<span id="cb3-3"><a href="#cb3-3" aria-hidden="true" tabindex="-1"></a>  <span class="dt">List1Unsorted</span> <span class="op">|</span> <span class="dt">List2Unsorted</span> <span class="op">|</span> <span class="dt">SomeOtherProblems</span></span>
<span id="cb3-4"><a href="#cb3-4" aria-hidden="true" tabindex="-1"></a>  <span class="kw">deriving</span> <span class="dt">Show</span></span></code></pre></div>
<p>and a function validating input lists and potentially returning an
appropriate value of that error data type <code class="sourceCode haskell"><span class="dt">AppError</span></code>.</p>
<div class="sourceCode" id="cb4"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb4-1"><a href="#cb4-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MainExpand.hs</span></span>
<span id="cb4-2"><a href="#cb4-2" aria-hidden="true" tabindex="-1"></a><span class="ot">validateLists ::</span></span>
<span id="cb4-3"><a href="#cb4-3" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Ordering</span>) <span class="ot">-&gt;</span></span>
<span id="cb4-4"><a href="#cb4-4" aria-hidden="true" tabindex="-1"></a>  [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb4-5"><a href="#cb4-5" aria-hidden="true" tabindex="-1"></a>  [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb4-6"><a href="#cb4-6" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Maybe</span> <span class="dt">AppError</span></span>
<span id="cb4-7"><a href="#cb4-7" aria-hidden="true" tabindex="-1"></a>validateLists comparator list1 list2 <span class="ot">=</span></span>
<span id="cb4-8"><a href="#cb4-8" aria-hidden="true" tabindex="-1"></a>  <span class="kw">case</span> (sortedBy comparator list1, sortedBy comparator list2) <span class="kw">of</span></span>
<span id="cb4-9"><a href="#cb4-9" aria-hidden="true" tabindex="-1"></a>    (<span class="dt">True</span>, <span class="dt">True</span>) <span class="ot">-&gt;</span> <span class="dt">Nothing</span></span>
<span id="cb4-10"><a href="#cb4-10" aria-hidden="true" tabindex="-1"></a>    (<span class="dt">False</span>, _)   <span class="ot">-&gt;</span> <span class="dt">Just</span> <span class="dt">List1Unsorted</span></span>
<span id="cb4-11"><a href="#cb4-11" aria-hidden="true" tabindex="-1"></a>    _            <span class="ot">-&gt;</span> <span class="dt">Just</span> <span class="dt">List2Unsorted</span></span></code></pre></div>
<p>The <code class="sourceCode haskell"><span class="dt">AppError</span></code> might carry as its payload the index of the
first list element that is out of order to render this information as
an error message later but I have left that out for brevity.</p>
<p><code class="sourceCode haskell">validateLists</code> could be used as follows.</p>
<div class="sourceCode" id="cb5"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb5-1"><a href="#cb5-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MainExpand.hs</span></span>
<span id="cb5-2"><a href="#cb5-2" aria-hidden="true" tabindex="-1"></a><span class="ot">processLists ::</span></span>
<span id="cb5-3"><a href="#cb5-3" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Ordering</span>) <span class="ot">-&gt;</span></span>
<span id="cb5-4"><a href="#cb5-4" aria-hidden="true" tabindex="-1"></a>  [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb5-5"><a href="#cb5-5" aria-hidden="true" tabindex="-1"></a>  [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb5-6"><a href="#cb5-6" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Either</span> <span class="dt">AppError</span> [<span class="dt">Integer</span>]</span>
<span id="cb5-7"><a href="#cb5-7" aria-hidden="true" tabindex="-1"></a>processLists comparator list1 list2 <span class="ot">=</span></span>
<span id="cb5-8"><a href="#cb5-8" aria-hidden="true" tabindex="-1"></a>  <span class="kw">case</span> validateLists comparator list1 list2 <span class="kw">of</span></span>
<span id="cb5-9"><a href="#cb5-9" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Just</span> appError <span class="ot">-&gt;</span> <span class="dt">Left</span> appError</span>
<span id="cb5-10"><a href="#cb5-10" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Nothing</span>       <span class="ot">-&gt;</span></span>
<span id="cb5-11"><a href="#cb5-11" aria-hidden="true" tabindex="-1"></a>      <span class="kw">case</span> mergeBy comparator list1 list2 <span class="kw">of</span></span>
<span id="cb5-12"><a href="#cb5-12" aria-hidden="true" tabindex="-1"></a>        <span class="dt">Just</span> result <span class="ot">-&gt;</span> <span class="dt">Right</span> result</span>
<span id="cb5-13"><a href="#cb5-13" aria-hidden="true" tabindex="-1"></a>        <span class="dt">Nothing</span>     <span class="ot">-&gt;</span></span>
<span id="cb5-14"><a href="#cb5-14" aria-hidden="true" tabindex="-1"></a>          <span class="fu">error</span> <span class="st">&quot;impossible because validateLists guarantees order&quot;</span> <span class="co">-- (1)</span></span>
<span id="cb5-15"><a href="#cb5-15" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb5-16"><a href="#cb5-16" aria-hidden="true" tabindex="-1"></a><span class="ot">example ::</span> <span class="dt">String</span></span>
<span id="cb5-17"><a href="#cb5-17" aria-hidden="true" tabindex="-1"></a>example <span class="ot">=</span></span>
<span id="cb5-18"><a href="#cb5-18" aria-hidden="true" tabindex="-1"></a>  <span class="kw">case</span> processLists descending [<span class="dv">3</span>,<span class="dv">1</span>] [<span class="dv">5</span>,<span class="dv">4</span>,<span class="dv">2</span>] <span class="kw">of</span></span>
<span id="cb5-19"><a href="#cb5-19" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Right</span> result           <span class="ot">-&gt;</span> <span class="fu">show</span> result</span>
<span id="cb5-20"><a href="#cb5-20" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Left</span> <span class="dt">List1Unsorted</span>     <span class="ot">-&gt;</span> <span class="st">&quot;The first list needs to be sorted.&quot;</span></span>
<span id="cb5-21"><a href="#cb5-21" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Left</span> <span class="dt">List2Unsorted</span>     <span class="ot">-&gt;</span> <span class="st">&quot;The second list needs to be sorted.&quot;</span></span>
<span id="cb5-22"><a href="#cb5-22" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Left</span> <span class="dt">SomeOtherProblems</span> <span class="ot">-&gt;</span> <span class="st">&quot;some other problems&quot;</span></span>
<span id="cb5-23"><a href="#cb5-23" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb5-24"><a href="#cb5-24" aria-hidden="true" tabindex="-1"></a><span class="ot">descending ::</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Ordering</span></span>
<span id="cb5-25"><a href="#cb5-25" aria-hidden="true" tabindex="-1"></a>descending a1 a2</span>
<span id="cb5-26"><a href="#cb5-26" aria-hidden="true" tabindex="-1"></a>  <span class="op">|</span> a1 <span class="op">&gt;</span> a2   <span class="ot">=</span> <span class="dt">LT</span></span>
<span id="cb5-27"><a href="#cb5-27" aria-hidden="true" tabindex="-1"></a>  <span class="op">|</span> a1 <span class="op">==</span> a2  <span class="ot">=</span> <span class="dt">EQ</span></span>
<span id="cb5-28"><a href="#cb5-28" aria-hidden="true" tabindex="-1"></a>  <span class="op">|</span> <span class="fu">otherwise</span> <span class="ot">=</span> <span class="dt">GT</span></span></code></pre></div>
<p>The approach of <em>expanding</em> the return type of functions that require
preconditions demonstrated in <code class="sourceCode haskell">MergeExpand.hs</code> suffers from
the following problems.</p>
<ul>
<li><p>If the <code class="sourceCode haskell"><span class="dt">Nothing</span></code> value representing a violated
precondition needs to be propagated multiple levels up the call
tree, there might be multiple preconditions of multiple functions
that would be represented by the <code class="sourceCode haskell"><span class="dt">Nothing</span></code> value. The
information, which specific precondition of which functions was
violated, is lost if they are all represented by the same value
<code class="sourceCode haskell"><span class="dt">Nothing</span></code>. If you try to repair this by using
<code class="sourceCode haskell"><span class="dt">Either</span></code> return types instead of <code class="sourceCode haskell"><span class="dt">Maybe</span></code> return
types, you will face the problem that simple
<code class="sourceCode haskell"><span class="dt">Either</span></code>-based checked exceptions do not compose well
<span class="citation" data-cites="parsons_trouble_typed_errors">[@parsons_trouble_typed_errors]</span>.</p></li>
<li><p>The argument and return types of <code class="sourceCode haskell">mergeBy</code> are larger than
necessary. That means that <code class="sourceCode haskell">mergeBy</code>’s type accepts more
incorrect implementations than necessary. The more refined your
types are, the more the type system helps you avoid bugs
<span class="citation" data-cites="parsons_type_back_forth">[@parsons_type_back_forth]</span>.</p></li>
<li><p>A much worse problem is the performance cost of checking <em>twice</em>
that both lists are sorted, once when <code class="sourceCode haskell">validateLists</code>
calls <code class="sourceCode haskell">sortedBy</code>, and again when <code class="sourceCode haskell">mergeBy</code> <em>has
to</em> examine if both lists are sorted in order to decide whether to
return <code class="sourceCode haskell"><span class="dt">Nothing</span></code> or <code class="sourceCode haskell"><span class="dt">Just</span></code>. I have not shown any
implementation of <code class="sourceCode haskell">mergeBy</code> yet because it does not matter
much but here is one to illustrate the need to examine if both lists
are sorted.</p>
<div class="sourceCode" id="cb6"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb6-1"><a href="#cb6-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MergeExpand.hs</span></span>
<span id="cb6-2"><a href="#cb6-2" aria-hidden="true" tabindex="-1"></a><span class="ot">mergeBy ::</span> (a <span class="ot">-&gt;</span> a <span class="ot">-&gt;</span> <span class="dt">Ordering</span>) <span class="ot">-&gt;</span> [a] <span class="ot">-&gt;</span> [a] <span class="ot">-&gt;</span> <span class="dt">Maybe</span> [a]</span>
<span id="cb6-3"><a href="#cb6-3" aria-hidden="true" tabindex="-1"></a>mergeBy comparator list1 list2</span>
<span id="cb6-4"><a href="#cb6-4" aria-hidden="true" tabindex="-1"></a>  <span class="op">|</span> sortedBy comparator list1 <span class="op">&amp;&amp;</span> sortedBy comparator list2 <span class="ot">=</span></span>
<span id="cb6-5"><a href="#cb6-5" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Just</span> (go list1 list2)</span>
<span id="cb6-6"><a href="#cb6-6" aria-hidden="true" tabindex="-1"></a>  <span class="op">|</span> <span class="fu">otherwise</span> <span class="ot">=</span> <span class="dt">Nothing</span></span>
<span id="cb6-7"><a href="#cb6-7" aria-hidden="true" tabindex="-1"></a>  <span class="kw">where</span></span>
<span id="cb6-8"><a href="#cb6-8" aria-hidden="true" tabindex="-1"></a>    go list1 [] <span class="ot">=</span> list1</span>
<span id="cb6-9"><a href="#cb6-9" aria-hidden="true" tabindex="-1"></a>    go [] list2 <span class="ot">=</span> list2</span>
<span id="cb6-10"><a href="#cb6-10" aria-hidden="true" tabindex="-1"></a>    go (head1 <span class="op">:</span> tail1) (head2 <span class="op">:</span> tail2)</span>
<span id="cb6-11"><a href="#cb6-11" aria-hidden="true" tabindex="-1"></a>      <span class="op">|</span> <span class="dt">LT</span> <span class="ot">&lt;-</span> comparator head2 head1 <span class="ot">=</span></span>
<span id="cb6-12"><a href="#cb6-12" aria-hidden="true" tabindex="-1"></a>        head2 <span class="op">:</span> go (head1 <span class="op">:</span> tail1) tail2</span>
<span id="cb6-13"><a href="#cb6-13" aria-hidden="true" tabindex="-1"></a>      <span class="op">|</span> <span class="fu">otherwise</span> <span class="ot">=</span></span>
<span id="cb6-14"><a href="#cb6-14" aria-hidden="true" tabindex="-1"></a>        head1 <span class="op">:</span> go tail1 (head2 <span class="op">:</span> tail2)</span></code></pre></div></li>
<li><p>And worst of all, the library user is forced to use
<code class="sourceCode haskell"><span class="fu">error</span></code> in line (1) of the implementation of
<code class="sourceCode haskell">processLists</code> in the “impossible” case because they know
that <code class="sourceCode haskell">mergeBy</code>’s precondition is <em>satisfied</em> and therefore
cannot reasonably handle a <em>violated</em> precondition. But what if
<code class="sourceCode haskell">validateLists</code> were modified in the future to stop
checking that the lists are sorted, intentionally or
unintentionally? The library user might not remember to update
<code class="sourceCode haskell">processLists</code>, and suddenly the “impossible” error
becomes a very possible run time error <span class="citation" data-cites="king_parse_validate">[@king_parse_validate]</span>.</p></li>
</ul>
<h1 id="a-first-attempt-at-a-solution">A First Attempt at a Solution</h1>
<p>The fact that you cannot express propositions and postconditions about
arguments and variables through types, because you cannot refer to
<em>term</em>-level variables on the <em>type</em> level, is the fundamental
obstacle to expressing <code class="sourceCode haskell">mergeBy</code>’s precondition through its
type. Noonan’s <span class="citation" data-cites="noonan2018">[@noonan2018]</span> first key idea is to wrap values into a
<code class="sourceCode haskell"><span class="kw">newtype</span></code> that has got a phantom type variable and is
defined in its own module.</p>
<div class="sourceCode" id="cb7"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb7-1"><a href="#cb7-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- NamedFirst.hs</span></span>
<span id="cb7-2"><a href="#cb7-2" aria-hidden="true" tabindex="-1"></a><span class="kw">newtype</span> <span class="dt">Named</span> name a <span class="ot">=</span> <span class="dt">Named</span> a</span></code></pre></div>
<p>The library module provides functions to wrap and unwrap too.</p>
<div class="sourceCode" id="cb8"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb8-1"><a href="#cb8-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- NamedFirst.hs</span></span>
<span id="cb8-2"><a href="#cb8-2" aria-hidden="true" tabindex="-1"></a><span class="ot">name ::</span> a <span class="ot">-&gt;</span> <span class="dt">Named</span> name a</span>
<span id="cb8-3"><a href="#cb8-3" aria-hidden="true" tabindex="-1"></a>name <span class="ot">=</span> <span class="dt">Named</span></span>
<span id="cb8-4"><a href="#cb8-4" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb8-5"><a href="#cb8-5" aria-hidden="true" tabindex="-1"></a><span class="ot">forgetName ::</span> <span class="dt">Named</span> name a <span class="ot">-&gt;</span> a</span>
<span id="cb8-6"><a href="#cb8-6" aria-hidden="true" tabindex="-1"></a>forgetName (<span class="dt">Named</span> a) <span class="ot">=</span> a</span></code></pre></div>
<p>This phantom <em>type</em> variable shall then be used to refer to the
wrapped <em>term</em>-level value on the <em>type</em> level. In a way, it is
providing a type-level <em>name</em>.</p>
<p>An example of that is the following data type in the <code>MergeFirst.hs</code>
library module.</p>
<div class="sourceCode" id="cb9"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb9-1"><a href="#cb9-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MergeFirst.hs</span></span>
<span id="cb9-2"><a href="#cb9-2" aria-hidden="true" tabindex="-1"></a><span class="kw">data</span> <span class="dt">SortedBy</span> comparator list <span class="ot">=</span> <span class="dt">SortedBy</span></span></code></pre></div>
<p>It does not contains any information at run time. But its phantom type
variables refer to a comparator’s <em>name</em> and a list’s <em>name</em>. And if a
value of <code class="sourceCode haskell"><span class="dt">SortedBy</span> comparator list</code> is in scope, then any
list <code class="sourceCode haskell"><span class="dt">Named</span> list [<span class="dt">Integer</span>]</code> is guaranteed at compile time to
be sorted according to any comparator <code class="sourceCode haskell"><span class="dt">Named</span> comparator (<span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Ordering</span>)</code>. A value of that type can be
understood as a proof or a witness of this guarantee.</p>
<p>How can a <code class="sourceCode haskell"><span class="dt">SortedBy</span></code> value make such a guarantee? Because
the only way to obtain a <code class="sourceCode haskell"><span class="dt">SortedBy</span></code> value is by calling a
library function like <code class="sourceCode haskell">sortedBy</code> that provides you with it
only after confirming that the referred to list is indeed sorted
according to the referred to comparator. Otherwise, you get
<code class="sourceCode haskell"><span class="dt">Nothing</span></code>.</p>
<div class="sourceCode" id="cb10"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb10-1"><a href="#cb10-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MergeFirst.hs</span></span>
<span id="cb10-2"><a href="#cb10-2" aria-hidden="true" tabindex="-1"></a><span class="kw">import</span> <span class="kw">qualified</span> <span class="dt">MergeExpand</span> <span class="kw">as</span> <span class="dt">ME</span></span>
<span id="cb10-3"><a href="#cb10-3" aria-hidden="true" tabindex="-1"></a><span class="ot">sortedBy ::</span></span>
<span id="cb10-4"><a href="#cb10-4" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Named</span> comparator (a <span class="ot">-&gt;</span> a <span class="ot">-&gt;</span> <span class="dt">Ordering</span>) <span class="ot">-&gt;</span> <span class="co">-- (1)</span></span>
<span id="cb10-5"><a href="#cb10-5" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Named</span> list       [a]                  <span class="ot">-&gt;</span> <span class="co">-- (2)</span></span>
<span id="cb10-6"><a href="#cb10-6" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Maybe</span> (<span class="dt">SortedBy</span> comparator list)         <span class="co">-- (3)</span></span>
<span id="cb10-7"><a href="#cb10-7" aria-hidden="true" tabindex="-1"></a>sortedBy comparator list <span class="ot">=</span></span>
<span id="cb10-8"><a href="#cb10-8" aria-hidden="true" tabindex="-1"></a>  <span class="kw">if</span> ME.sortedBy (forgetName comparator) (forgetName list)</span>
<span id="cb10-9"><a href="#cb10-9" aria-hidden="true" tabindex="-1"></a>  <span class="kw">then</span> <span class="dt">Just</span> <span class="dt">SortedBy</span></span>
<span id="cb10-10"><a href="#cb10-10" aria-hidden="true" tabindex="-1"></a>  <span class="kw">else</span> <span class="dt">Nothing</span></span></code></pre></div>
<p>Notice, how the return type’s type variables in line (3) must equal
the ones in the argument types in line (1) and (2) respectively.</p>
<p>In a way, we have just expressed <code class="sourceCode haskell">sortedBy</code>’s postcondition
through its type. We can now express <code class="sourceCode haskell">mergeBy</code>’s
precondition through its type.</p>
<div class="sourceCode" id="cb11"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb11-1"><a href="#cb11-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MergeFirst.hs</span></span>
<span id="cb11-2"><a href="#cb11-2" aria-hidden="true" tabindex="-1"></a><span class="ot">mergeBy ::</span></span>
<span id="cb11-3"><a href="#cb11-3" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Named</span> comparator (a <span class="ot">-&gt;</span> a <span class="ot">-&gt;</span> <span class="dt">Ordering</span>)         <span class="ot">-&gt;</span></span>
<span id="cb11-4"><a href="#cb11-4" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Named</span> list1  [a], <span class="dt">SortedBy</span> comparator list1) <span class="ot">-&gt;</span></span>
<span id="cb11-5"><a href="#cb11-5" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Named</span> list2  [a], <span class="dt">SortedBy</span> comparator list2) <span class="ot">-&gt;</span></span>
<span id="cb11-6"><a href="#cb11-6" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Named</span> result [a], <span class="dt">SortedBy</span> comparator result)</span></code></pre></div>
<p>The function can only be called if <code class="sourceCode haskell"><span class="dt">SortedBy</span></code> values for
both input lists are available, which guarantee <code class="sourceCode haskell">mergeBy</code>‘s
precondition. Consequently, <code class="sourceCode haskell">mergeBy</code> need not return a
<code class="sourceCode haskell"><span class="dt">Maybe</span></code> type anymore nor check the lists’ order at run time
anymore either.</p>
<div class="sourceCode" id="cb12"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb12-1"><a href="#cb12-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MergeFirst.hs</span></span>
<span id="cb12-2"><a href="#cb12-2" aria-hidden="true" tabindex="-1"></a>mergeBy comparator (list1, _proof1) (list2, _proof2) <span class="ot">=</span></span>
<span id="cb12-3"><a href="#cb12-3" aria-hidden="true" tabindex="-1"></a>  <span class="kw">let</span> result <span class="ot">=</span> name <span class="op">$</span> go (forgetName list1) (forgetName list2)</span>
<span id="cb12-4"><a href="#cb12-4" aria-hidden="true" tabindex="-1"></a>    <span class="kw">in</span> (result, <span class="dt">SortedBy</span>)</span>
<span id="cb12-5"><a href="#cb12-5" aria-hidden="true" tabindex="-1"></a>  <span class="kw">where</span></span>
<span id="cb12-6"><a href="#cb12-6" aria-hidden="true" tabindex="-1"></a>    go list1 [] <span class="ot">=</span> list1</span>
<span id="cb12-7"><a href="#cb12-7" aria-hidden="true" tabindex="-1"></a>    go [] list2 <span class="ot">=</span> list2</span>
<span id="cb12-8"><a href="#cb12-8" aria-hidden="true" tabindex="-1"></a>    go (head1 <span class="op">:</span> tail1) (head2 <span class="op">:</span> tail2)</span>
<span id="cb12-9"><a href="#cb12-9" aria-hidden="true" tabindex="-1"></a>      <span class="op">|</span> <span class="dt">LT</span> <span class="ot">&lt;-</span> (forgetName comparator) head2 head1 <span class="ot">=</span></span>
<span id="cb12-10"><a href="#cb12-10" aria-hidden="true" tabindex="-1"></a>        head2 <span class="op">:</span> go (head1 <span class="op">:</span> tail1) tail2</span>
<span id="cb12-11"><a href="#cb12-11" aria-hidden="true" tabindex="-1"></a>      <span class="op">|</span> <span class="fu">otherwise</span> <span class="ot">=</span></span>
<span id="cb12-12"><a href="#cb12-12" aria-hidden="true" tabindex="-1"></a>        head1 <span class="op">:</span> go tail1 (head2 <span class="op">:</span> tail2)</span></code></pre></div>
<p>Taking inspiration from the library functions <code class="sourceCode haskell">sortedBy</code> and
<code class="sourceCode haskell">mergeBy</code> fabricating arbitrary <code class="sourceCode haskell"><span class="dt">SortedBy</span></code> values
from thin air by simply using its data constructor
<code class="sourceCode haskell"><span class="dt">SortedBy</span></code>, you might ask “What stops a library user from
doing the same?”</p>
<div class="sourceCode" id="cb13"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb13-1"><a href="#cb13-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MainFirst.hs</span></span>
<span id="cb13-2"><a href="#cb13-2" aria-hidden="true" tabindex="-1"></a><span class="ot">preconditionViolated ::</span> [<span class="dt">Integer</span>]</span>
<span id="cb13-3"><a href="#cb13-3" aria-hidden="true" tabindex="-1"></a>preconditionViolated <span class="ot">=</span></span>
<span id="cb13-4"><a href="#cb13-4" aria-hidden="true" tabindex="-1"></a>  forgetName <span class="op">$</span></span>
<span id="cb13-5"><a href="#cb13-5" aria-hidden="true" tabindex="-1"></a>  <span class="fu">fst</span> <span class="op">$</span></span>
<span id="cb13-6"><a href="#cb13-6" aria-hidden="true" tabindex="-1"></a>  mergeBy</span>
<span id="cb13-7"><a href="#cb13-7" aria-hidden="true" tabindex="-1"></a>    (name descending)</span>
<span id="cb13-8"><a href="#cb13-8" aria-hidden="true" tabindex="-1"></a>    (name [<span class="dv">3</span>,<span class="dv">1</span>,<span class="dv">99</span>], <span class="dt">SortedBy</span>)</span>
<span id="cb13-9"><a href="#cb13-9" aria-hidden="true" tabindex="-1"></a>    (name [<span class="dv">5</span>,<span class="dv">4</span>,<span class="dv">2</span>],  <span class="dt">SortedBy</span>)</span></code></pre></div>
<p>The answer is in the library’s module declaration.</p>
<div class="sourceCode" id="cb14"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb14-1"><a href="#cb14-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MergeFirst.hs</span></span>
<span id="cb14-2"><a href="#cb14-2" aria-hidden="true" tabindex="-1"></a><span class="kw">module</span> <span class="dt">MergeFirst</span> (<span class="dt">SortedBy</span> (), mergeBy, sortedBy) <span class="kw">where</span></span></code></pre></div>
<p><code class="sourceCode haskell"><span class="dt">SortedBy</span></code>’s is an abstract data type, its data constructor
is not exported. This restricts the power to fabricate its values from
thin air to the library code, where the data type is defined. Mistakes
in the library will not be caught by the type system. But the user
cannot use the library in an unsafe way because it can only obtain a
<code class="sourceCode haskell"><span class="dt">SortedBy</span></code> value from library functions like
<code class="sourceCode haskell">sortedBy</code>.</p>
<p>Let us see what the library user’s <code class="sourceCode haskell">validateLists</code> and
<code class="sourceCode haskell">processLists</code> look like when using the new
<code>MergeFirst.hs</code>. The rest of the user code stays the same.</p>
<div class="sourceCode" id="cb15"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb15-1"><a href="#cb15-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MainFirst.hs</span></span>
<span id="cb15-2"><a href="#cb15-2" aria-hidden="true" tabindex="-1"></a><span class="ot">validateLists ::</span></span>
<span id="cb15-3"><a href="#cb15-3" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Named</span> comparator (<span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Ordering</span>) <span class="ot">-&gt;</span></span>
<span id="cb15-4"><a href="#cb15-4" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Named</span> list1      [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb15-5"><a href="#cb15-5" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Named</span> list2      [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb15-6"><a href="#cb15-6" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Either</span> <span class="dt">AppError</span> (<span class="dt">SortedBy</span> comparator list1, <span class="dt">SortedBy</span> comparator list2)</span>
<span id="cb15-7"><a href="#cb15-7" aria-hidden="true" tabindex="-1"></a>validateLists comparator list1 list2 <span class="ot">=</span></span>
<span id="cb15-8"><a href="#cb15-8" aria-hidden="true" tabindex="-1"></a>  <span class="kw">case</span> (sortedBy comparator list1, sortedBy comparator list2) <span class="kw">of</span></span>
<span id="cb15-9"><a href="#cb15-9" aria-hidden="true" tabindex="-1"></a>    (<span class="dt">Just</span> proof1, <span class="dt">Just</span> proof2) <span class="ot">-&gt;</span> <span class="dt">Right</span> (proof1, proof2)</span>
<span id="cb15-10"><a href="#cb15-10" aria-hidden="true" tabindex="-1"></a>    (<span class="dt">Nothing</span>, _)               <span class="ot">-&gt;</span> <span class="dt">Left</span> <span class="dt">List1Unsorted</span></span>
<span id="cb15-11"><a href="#cb15-11" aria-hidden="true" tabindex="-1"></a>    _                          <span class="ot">-&gt;</span> <span class="dt">Left</span> <span class="dt">List2Unsorted</span></span>
<span id="cb15-12"><a href="#cb15-12" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb15-13"><a href="#cb15-13" aria-hidden="true" tabindex="-1"></a><span class="ot">processLists ::</span></span>
<span id="cb15-14"><a href="#cb15-14" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Ordering</span>) <span class="ot">-&gt;</span></span>
<span id="cb15-15"><a href="#cb15-15" aria-hidden="true" tabindex="-1"></a>  [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb15-16"><a href="#cb15-16" aria-hidden="true" tabindex="-1"></a>  [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb15-17"><a href="#cb15-17" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Either</span> <span class="dt">AppError</span> [<span class="dt">Integer</span>]</span>
<span id="cb15-18"><a href="#cb15-18" aria-hidden="true" tabindex="-1"></a>processLists comparator list1 list2 <span class="ot">=</span></span>
<span id="cb15-19"><a href="#cb15-19" aria-hidden="true" tabindex="-1"></a>  <span class="kw">let</span></span>
<span id="cb15-20"><a href="#cb15-20" aria-hidden="true" tabindex="-1"></a><span class="ot">    comparatorNamed ::</span> <span class="dt">Named</span> comparator (<span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Ordering</span>)</span>
<span id="cb15-21"><a href="#cb15-21" aria-hidden="true" tabindex="-1"></a>    comparatorNamed <span class="ot">=</span> name comparator</span>
<span id="cb15-22"><a href="#cb15-22" aria-hidden="true" tabindex="-1"></a><span class="ot">    list1Named      ::</span> <span class="dt">Named</span> list1 [<span class="dt">Integer</span>]</span>
<span id="cb15-23"><a href="#cb15-23" aria-hidden="true" tabindex="-1"></a>    list1Named      <span class="ot">=</span> name list1</span>
<span id="cb15-24"><a href="#cb15-24" aria-hidden="true" tabindex="-1"></a><span class="ot">    list2Named      ::</span> <span class="dt">Named</span> list2 [<span class="dt">Integer</span>]</span>
<span id="cb15-25"><a href="#cb15-25" aria-hidden="true" tabindex="-1"></a>    list2Named      <span class="ot">=</span> name list2</span>
<span id="cb15-26"><a href="#cb15-26" aria-hidden="true" tabindex="-1"></a>  <span class="kw">in</span></span>
<span id="cb15-27"><a href="#cb15-27" aria-hidden="true" tabindex="-1"></a>    <span class="kw">case</span> validateLists comparatorNamed list1Named list2Named <span class="kw">of</span> <span class="co">-- (1)</span></span>
<span id="cb15-28"><a href="#cb15-28" aria-hidden="true" tabindex="-1"></a>      <span class="dt">Left</span> appError          <span class="ot">-&gt;</span> <span class="dt">Left</span> appError</span>
<span id="cb15-29"><a href="#cb15-29" aria-hidden="true" tabindex="-1"></a>      <span class="dt">Right</span> (proof1, proof2) <span class="ot">-&gt;</span></span>
<span id="cb15-30"><a href="#cb15-30" aria-hidden="true" tabindex="-1"></a>        <span class="kw">let</span></span>
<span id="cb15-31"><a href="#cb15-31" aria-hidden="true" tabindex="-1"></a>          (result, _proof) <span class="ot">=</span></span>
<span id="cb15-32"><a href="#cb15-32" aria-hidden="true" tabindex="-1"></a>            mergeBy comparatorNamed (list1Named, proof1) (list2Named, proof2)</span>
<span id="cb15-33"><a href="#cb15-33" aria-hidden="true" tabindex="-1"></a>        <span class="kw">in</span> <span class="dt">Right</span> (forgetName result)</span></code></pre></div>
<p><code class="sourceCode haskell">mergeBy</code> does not return a <code class="sourceCode haskell"><span class="dt">Maybe</span></code> anymore as
mentioned before. Therefore, the library user is not forced to use
<code class="sourceCode haskell"><span class="fu">error</span></code> in any “impossible” case in the implementation of
<code class="sourceCode haskell">processLists</code> anymore. The user can communicate their
conviction that <code class="sourceCode haskell">mergeBy</code>’s precondition is satisfied to the
library in a way that the library can verify.</p>
<p>At least, that would be the case, if there was not still a big problem
with this <em>first attempt</em>. Look at <code class="sourceCode haskell">processLists</code> again and
imagine, only the first input list was sorted but not the second
one. In this case, there should be no way to apply <code>mergeBy</code> to the
lists because the necessary <code class="sourceCode haskell"><span class="dt">SortedBy</span> comparator list2</code>
value cannot be obtained from <code class="sourceCode haskell">sortedBy</code>, which only returns
<code class="sourceCode haskell"><span class="dt">Nothing</span></code>. But what if the library user does not pass the
second unsorted list to <code class="sourceCode haskell">validateLists</code> but the first sorted
list again instead in line (1)?</p>
<div class="sourceCode" id="cb16"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb16-1"><a href="#cb16-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MainFirst.hs</span></span>
<span id="cb16-2"><a href="#cb16-2" aria-hidden="true" tabindex="-1"></a><span class="ot">processLists ::</span></span>
<span id="cb16-3"><a href="#cb16-3" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Ordering</span>) <span class="ot">-&gt;</span></span>
<span id="cb16-4"><a href="#cb16-4" aria-hidden="true" tabindex="-1"></a>  [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb16-5"><a href="#cb16-5" aria-hidden="true" tabindex="-1"></a>  [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb16-6"><a href="#cb16-6" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Either</span> <span class="dt">AppError</span> [<span class="dt">Integer</span>]</span>
<span id="cb16-7"><a href="#cb16-7" aria-hidden="true" tabindex="-1"></a>processLists comparator list1 list2 <span class="ot">=</span></span>
<span id="cb16-8"><a href="#cb16-8" aria-hidden="true" tabindex="-1"></a>  <span class="kw">let</span></span>
<span id="cb16-9"><a href="#cb16-9" aria-hidden="true" tabindex="-1"></a><span class="ot">    comparatorNamed ::</span> <span class="dt">Named</span> comparator (<span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Ordering</span>)</span>
<span id="cb16-10"><a href="#cb16-10" aria-hidden="true" tabindex="-1"></a>    comparatorNamed <span class="ot">=</span> name comparator</span>
<span id="cb16-11"><a href="#cb16-11" aria-hidden="true" tabindex="-1"></a><span class="ot">    list1Named      ::</span> <span class="dt">Named</span> list1 [<span class="dt">Integer</span>]</span>
<span id="cb16-12"><a href="#cb16-12" aria-hidden="true" tabindex="-1"></a>    list1Named      <span class="ot">=</span> name list1</span>
<span id="cb16-13"><a href="#cb16-13" aria-hidden="true" tabindex="-1"></a><span class="ot">    list2Named      ::</span> <span class="dt">Named</span> list2 [<span class="dt">Integer</span>]</span>
<span id="cb16-14"><a href="#cb16-14" aria-hidden="true" tabindex="-1"></a>    list2Named      <span class="ot">=</span> name list2</span>
<span id="cb16-15"><a href="#cb16-15" aria-hidden="true" tabindex="-1"></a>  <span class="kw">in</span></span>
<span id="cb16-16"><a href="#cb16-16" aria-hidden="true" tabindex="-1"></a><span class="co">--  case validateLists comparatorNamed list1Named list2Named of</span></span>
<span id="cb16-17"><a href="#cb16-17" aria-hidden="true" tabindex="-1"></a>    <span class="kw">case</span> validateLists comparatorNamed list1Named list1Named <span class="kw">of</span> <span class="co">-- (1)</span></span>
<span id="cb16-18"><a href="#cb16-18" aria-hidden="true" tabindex="-1"></a>      <span class="dt">Left</span> appError          <span class="ot">-&gt;</span> <span class="dt">Left</span> appError</span>
<span id="cb16-19"><a href="#cb16-19" aria-hidden="true" tabindex="-1"></a>      <span class="dt">Right</span> (proof1, proof2) <span class="ot">-&gt;</span></span>
<span id="cb16-20"><a href="#cb16-20" aria-hidden="true" tabindex="-1"></a>        <span class="kw">let</span></span>
<span id="cb16-21"><a href="#cb16-21" aria-hidden="true" tabindex="-1"></a>          (result, _proof) <span class="ot">=</span></span>
<span id="cb16-22"><a href="#cb16-22" aria-hidden="true" tabindex="-1"></a>            <span class="co">-- (2)</span></span>
<span id="cb16-23"><a href="#cb16-23" aria-hidden="true" tabindex="-1"></a>            mergeBy comparatorNamed (list1Named, proof1) (list2Named, proof2)</span>
<span id="cb16-24"><a href="#cb16-24" aria-hidden="true" tabindex="-1"></a>        <span class="kw">in</span> <span class="dt">Right</span> (forgetName result)</span></code></pre></div>
<p><code class="sourceCode haskell">proof2</code> is now of type <code class="sourceCode haskell"><span class="dt">SortedBy</span> comparator list1</code>, <code class="sourceCode haskell">list2Named</code> is still of type <code class="sourceCode haskell"><span class="dt">Named</span> list2 [<span class="dt">Integer</span>]</code>. You might expect a type error from the line
below line (2) considering line (3) of <code class="sourceCode haskell">mergeBy</code>’s type
signature.</p>
<div class="sourceCode" id="cb17"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb17-1"><a href="#cb17-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MergeFirst.hs</span></span>
<span id="cb17-2"><a href="#cb17-2" aria-hidden="true" tabindex="-1"></a><span class="ot">mergeBy ::</span></span>
<span id="cb17-3"><a href="#cb17-3" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Named</span> comparator (a <span class="ot">-&gt;</span> a <span class="ot">-&gt;</span> <span class="dt">Ordering</span>)         <span class="ot">-&gt;</span></span>
<span id="cb17-4"><a href="#cb17-4" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Named</span> list1  [a], <span class="dt">SortedBy</span> comparator list1) <span class="ot">-&gt;</span></span>
<span id="cb17-5"><a href="#cb17-5" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Named</span> list2  [a], <span class="dt">SortedBy</span> comparator list2) <span class="ot">-&gt;</span> <span class="co">-- (3)</span></span>
<span id="cb17-6"><a href="#cb17-6" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Named</span> result [a], <span class="dt">SortedBy</span> comparator result)</span></code></pre></div>
<p><code class="sourceCode haskell">list2</code> and <code class="sourceCode haskell">list1</code> would need to be the same type
to satisfy the type checker. But there is no type error
unfortunately. The reason is that we are indeed allowed to assume that
the type variables <code class="sourceCode haskell">list2</code> and <code class="sourceCode haskell">list1</code> are the
same type. They are universally quantified and therefore
unify. Indeed, we may assume each of them to be any arbitrary
type. There is no type error just like there is no type error in line
(1) and (2) of the following code for example.</p>
<div class="sourceCode" id="cb18"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb18-1"><a href="#cb18-1" aria-hidden="true" tabindex="-1"></a><span class="ot">wellTyped ::</span> <span class="dt">Int</span></span>
<span id="cb18-2"><a href="#cb18-2" aria-hidden="true" tabindex="-1"></a>wellTyped <span class="ot">=</span></span>
<span id="cb18-3"><a href="#cb18-3" aria-hidden="true" tabindex="-1"></a>  <span class="kw">let</span></span>
<span id="cb18-4"><a href="#cb18-4" aria-hidden="true" tabindex="-1"></a><span class="ot">    aList    ::</span> [variable1]</span>
<span id="cb18-5"><a href="#cb18-5" aria-hidden="true" tabindex="-1"></a>    aList    <span class="ot">=</span> []</span>
<span id="cb18-6"><a href="#cb18-6" aria-hidden="true" tabindex="-1"></a><span class="ot">    bList    ::</span> [variable2]</span>
<span id="cb18-7"><a href="#cb18-7" aria-hidden="true" tabindex="-1"></a>    bList    <span class="ot">=</span> aList <span class="co">-- (1)</span></span>
<span id="cb18-8"><a href="#cb18-8" aria-hidden="true" tabindex="-1"></a><span class="ot">    boolList ::</span> [<span class="dt">Bool</span>]</span>
<span id="cb18-9"><a href="#cb18-9" aria-hidden="true" tabindex="-1"></a>    boolList <span class="ot">=</span> aList <span class="co">-- (2)</span></span>
<span id="cb18-10"><a href="#cb18-10" aria-hidden="true" tabindex="-1"></a>  <span class="kw">in</span></span>
<span id="cb18-11"><a href="#cb18-11" aria-hidden="true" tabindex="-1"></a>    <span class="fu">length</span> bList</span></code></pre></div>
<p>So violating <code class="sourceCode haskell">mergeBy</code>’s precondition is not a type
error. We could accidentally apply the latest version of
<code class="sourceCode haskell">processLists</code> to an unsorted list resulting in unspecified
behavior by <code class="sourceCode haskell">MergeFirst.mergeBy</code>. This is worse than
<code>MergeExpand.hs</code>.</p>
<h1 id="the-solution">The Solution</h1>
<p>Let us again try to trick the library into evaluating
<code class="sourceCode haskell">mergeBy</code> without its precondition satisfied just as we did
before my manipulating <code class="sourceCode haskell">processLists</code>, this time by
manipulating <code class="sourceCode haskell">validateLists</code>’s implementation. Again, we
imagine only the first input list was sorted but not the second one.</p>
<div class="sourceCode" id="cb19"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb19-1"><a href="#cb19-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MainFirst.hs</span></span>
<span id="cb19-2"><a href="#cb19-2" aria-hidden="true" tabindex="-1"></a><span class="ot">validateLists ::</span></span>
<span id="cb19-3"><a href="#cb19-3" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Named</span> comparator (<span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Ordering</span>) <span class="ot">-&gt;</span></span>
<span id="cb19-4"><a href="#cb19-4" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Named</span> list1      [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb19-5"><a href="#cb19-5" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Named</span> list2      [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb19-6"><a href="#cb19-6" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Either</span> <span class="dt">AppError</span> (<span class="dt">SortedBy</span> comparator list1, <span class="dt">SortedBy</span> comparator list2)</span>
<span id="cb19-7"><a href="#cb19-7" aria-hidden="true" tabindex="-1"></a>validateLists comparator list1 list2 <span class="ot">=</span></span>
<span id="cb19-8"><a href="#cb19-8" aria-hidden="true" tabindex="-1"></a><span class="co">--case (sortedBy comparator list1, sortedBy comparator list2) of</span></span>
<span id="cb19-9"><a href="#cb19-9" aria-hidden="true" tabindex="-1"></a>  <span class="kw">case</span> (sortedBy comparator list1, sortedBy comparator list1) <span class="kw">of</span> <span class="co">-- (1)</span></span>
<span id="cb19-10"><a href="#cb19-10" aria-hidden="true" tabindex="-1"></a>    (<span class="dt">Just</span> proof1, <span class="dt">Just</span> proof2) <span class="ot">-&gt;</span> <span class="dt">Right</span> (proof1, proof2)</span>
<span id="cb19-11"><a href="#cb19-11" aria-hidden="true" tabindex="-1"></a>    (<span class="dt">Nothing</span>, _)               <span class="ot">-&gt;</span> <span class="dt">Left</span> <span class="dt">List1Unsorted</span></span>
<span id="cb19-12"><a href="#cb19-12" aria-hidden="true" tabindex="-1"></a>    _                          <span class="ot">-&gt;</span> <span class="dt">Left</span> <span class="dt">List2Unsorted</span></span></code></pre></div>
<p>We do not apply <code class="sourceCode haskell">sortedBy</code> to <code class="sourceCode haskell">list2</code> but again to
<code class="sourceCode haskell">list1</code> instead in line (1). Similarly to before,
<code class="sourceCode haskell">proof2</code> now is of type <code class="sourceCode haskell"><span class="dt">SortedBy</span> comparator list1</code> while expected to be of type <code class="sourceCode haskell"><span class="dt">SortedBy</span> comparator list2</code>. But very much unlike before, we now do get the exact
type error that we hoped for.</p>
<pre><code>• Couldn't match type ‘list1’ with ‘list2’
[...]
Expected type: Either AppError
                 (SortedBy comparator list1, SortedBy comparator list2)
  Actual type: Either AppError
                 (SortedBy comparator list1, SortedBy comparator list1)</code></pre>
<p>Why may we not assume that the type variables <code class="sourceCode haskell">list2</code> and
<code class="sourceCode haskell">list1</code> are the same type anymore? Because they are now part
of the function’s parameter types. Naturally, we may not make any
assumptions about type variables in function parameters because we
have to be able to deal with whatever types the user chooses to apply
that function too. In the implementation of a function <code class="sourceCode haskell"><span class="fu">length</span><span class="ot"> ::</span> [a] <span class="ot">-&gt;</span> <span class="dt">Integer</span></code>, for example, we may not assume that
<code class="sourceCode haskell">a</code> is <code class="sourceCode haskell"><span class="dt">Bool</span></code> because the caller might choose to
apply <code class="sourceCode haskell"><span class="fu">length</span></code> to a list of <code class="sourceCode haskell"><span class="dt">Char</span></code>’s.</p>
<p>So how do we consistently force a library user into the situation of
<code class="sourceCode haskell">validateLists</code> where they cannot make any assumptions about
the <code class="sourceCode haskell"><span class="dt">Named</span></code> type variables?</p>
<div class="sourceCode" id="cb21"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb21-1"><a href="#cb21-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- Named.hs</span></span>
<span id="cb21-2"><a href="#cb21-2" aria-hidden="true" tabindex="-1"></a><span class="ot">{-# language RankNTypes #-}</span></span>
<span id="cb21-3"><a href="#cb21-3" aria-hidden="true" tabindex="-1"></a><span class="kw">module</span> <span class="dt">Named</span> (<span class="dt">Named</span> (), name, forgetName) <span class="kw">where</span></span>
<span id="cb21-4"><a href="#cb21-4" aria-hidden="true" tabindex="-1"></a><span class="ot">name ::</span></span>
<span id="cb21-5"><a href="#cb21-5" aria-hidden="true" tabindex="-1"></a>  a <span class="ot">-&gt;</span></span>
<span id="cb21-6"><a href="#cb21-6" aria-hidden="true" tabindex="-1"></a>  (<span class="kw">forall</span> name<span class="op">.</span> <span class="dt">Named</span> name a <span class="ot">-&gt;</span> result) <span class="ot">-&gt;</span></span>
<span id="cb21-7"><a href="#cb21-7" aria-hidden="true" tabindex="-1"></a>  result</span>
<span id="cb21-8"><a href="#cb21-8" aria-hidden="true" tabindex="-1"></a>name a continuation <span class="ot">=</span> continuation (<span class="dt">Named</span> a)</span></code></pre></div>
<p>The answer is that whenever the library user wants to use a
<code class="sourceCode haskell"><span class="dt">Named</span></code> value, they have to write a function because the
library function <code class="sourceCode haskell">name</code> does not return <code class="sourceCode haskell"><span class="dt">Named</span></code>
values anymore. It does not offer “Give me an <code class="sourceCode haskell">a</code> and I will
return you a <code class="sourceCode haskell"><span class="dt">Named</span> name a</code>” anymore. It now offers “Give me
an <code class="sourceCode haskell">a</code> and tell me what you would do with a <code class="sourceCode haskell"><span class="dt">Named</span> name a</code>. I will do that <em>for</em> you and return you the result of
that.” This is also called “continuation-passing style”. That is
Noonan’s <span class="citation" data-cites="noonan2018">[@noonan2018]</span> most important key idea.</p>
<p>Let us see what the library user’s <code class="sourceCode haskell">processLists</code> look like
when using the library adjusted to the new <code>Named.hs</code>. The rest of the
user code stays the same including <code class="sourceCode haskell">validateLists</code>. That is
unsurprising because we have just determined that
<code class="sourceCode haskell">validateLists</code> is exactly the kind of code we want to force
the library user to write all the time.</p>
<div class="sourceCode" id="cb22"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb22-1"><a href="#cb22-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- Main.hs</span></span>
<span id="cb22-2"><a href="#cb22-2" aria-hidden="true" tabindex="-1"></a><span class="ot">processLists ::</span></span>
<span id="cb22-3"><a href="#cb22-3" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Ordering</span>) <span class="ot">-&gt;</span></span>
<span id="cb22-4"><a href="#cb22-4" aria-hidden="true" tabindex="-1"></a>  [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb22-5"><a href="#cb22-5" aria-hidden="true" tabindex="-1"></a>  [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb22-6"><a href="#cb22-6" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Either</span> <span class="dt">AppError</span> [<span class="dt">Integer</span>]</span>
<span id="cb22-7"><a href="#cb22-7" aria-hidden="true" tabindex="-1"></a>processLists comparator list1 list2 <span class="ot">=</span></span>
<span id="cb22-8"><a href="#cb22-8" aria-hidden="true" tabindex="-1"></a>  name comparator <span class="op">$</span> \comparatorNamed <span class="ot">-&gt;</span></span>
<span id="cb22-9"><a href="#cb22-9" aria-hidden="true" tabindex="-1"></a>  name list1      <span class="op">$</span> \list1Named      <span class="ot">-&gt;</span></span>
<span id="cb22-10"><a href="#cb22-10" aria-hidden="true" tabindex="-1"></a>  name list2      <span class="op">$</span> \list2Named      <span class="ot">-&gt;</span></span>
<span id="cb22-11"><a href="#cb22-11" aria-hidden="true" tabindex="-1"></a>  <span class="kw">case</span> validateLists comparatorNamed list1Named list2Named <span class="kw">of</span></span>
<span id="cb22-12"><a href="#cb22-12" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Left</span> appError          <span class="ot">-&gt;</span> <span class="dt">Left</span> appError</span>
<span id="cb22-13"><a href="#cb22-13" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Right</span> (proof1, proof2) <span class="ot">-&gt;</span></span>
<span id="cb22-14"><a href="#cb22-14" aria-hidden="true" tabindex="-1"></a>      mergeBy comparatorNamed (list1Named, proof1) (list2Named, proof2) <span class="op">$</span></span>
<span id="cb22-15"><a href="#cb22-15" aria-hidden="true" tabindex="-1"></a>        \(result, _proof) <span class="ot">-&gt;</span></span>
<span id="cb22-16"><a href="#cb22-16" aria-hidden="true" tabindex="-1"></a>        <span class="dt">Right</span> (forgetName result)</span></code></pre></div>
<p>Here is <code class="sourceCode haskell">MainFirst.processLists</code> from our previous
unsuccessful first attempt again in case you want to compare them.</p>
<div class="sourceCode" id="cb23"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb23-1"><a href="#cb23-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- MainFirst.hs</span></span>
<span id="cb23-2"><a href="#cb23-2" aria-hidden="true" tabindex="-1"></a><span class="ot">processLists ::</span></span>
<span id="cb23-3"><a href="#cb23-3" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Ordering</span>) <span class="ot">-&gt;</span></span>
<span id="cb23-4"><a href="#cb23-4" aria-hidden="true" tabindex="-1"></a>  [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb23-5"><a href="#cb23-5" aria-hidden="true" tabindex="-1"></a>  [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb23-6"><a href="#cb23-6" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Either</span> <span class="dt">AppError</span> [<span class="dt">Integer</span>]</span>
<span id="cb23-7"><a href="#cb23-7" aria-hidden="true" tabindex="-1"></a>processLists comparator list1 list2 <span class="ot">=</span></span>
<span id="cb23-8"><a href="#cb23-8" aria-hidden="true" tabindex="-1"></a>  <span class="kw">let</span></span>
<span id="cb23-9"><a href="#cb23-9" aria-hidden="true" tabindex="-1"></a><span class="ot">    comparatorNamed ::</span> <span class="dt">Named</span> comparator (<span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Ordering</span>)</span>
<span id="cb23-10"><a href="#cb23-10" aria-hidden="true" tabindex="-1"></a>    comparatorNamed <span class="ot">=</span> name comparator</span>
<span id="cb23-11"><a href="#cb23-11" aria-hidden="true" tabindex="-1"></a><span class="ot">    list1Named      ::</span> <span class="dt">Named</span> list1 [<span class="dt">Integer</span>]</span>
<span id="cb23-12"><a href="#cb23-12" aria-hidden="true" tabindex="-1"></a>    list1Named      <span class="ot">=</span> name list1</span>
<span id="cb23-13"><a href="#cb23-13" aria-hidden="true" tabindex="-1"></a><span class="ot">    list2Named      ::</span> <span class="dt">Named</span> list2 [<span class="dt">Integer</span>]</span>
<span id="cb23-14"><a href="#cb23-14" aria-hidden="true" tabindex="-1"></a>    list2Named      <span class="ot">=</span> name list2</span>
<span id="cb23-15"><a href="#cb23-15" aria-hidden="true" tabindex="-1"></a>  <span class="kw">in</span></span>
<span id="cb23-16"><a href="#cb23-16" aria-hidden="true" tabindex="-1"></a>    <span class="kw">case</span> validateLists comparatorNamed list1Named list2Named <span class="kw">of</span></span>
<span id="cb23-17"><a href="#cb23-17" aria-hidden="true" tabindex="-1"></a>      <span class="dt">Left</span> appError          <span class="ot">-&gt;</span> <span class="dt">Left</span> appError</span>
<span id="cb23-18"><a href="#cb23-18" aria-hidden="true" tabindex="-1"></a>      <span class="dt">Right</span> (proof1, proof2) <span class="ot">-&gt;</span></span>
<span id="cb23-19"><a href="#cb23-19" aria-hidden="true" tabindex="-1"></a>        <span class="kw">let</span></span>
<span id="cb23-20"><a href="#cb23-20" aria-hidden="true" tabindex="-1"></a>          (result, _proof) <span class="ot">=</span></span>
<span id="cb23-21"><a href="#cb23-21" aria-hidden="true" tabindex="-1"></a>            mergeBy comparatorNamed (list1Named, proof1) (list2Named, proof2)</span>
<span id="cb23-22"><a href="#cb23-22" aria-hidden="true" tabindex="-1"></a>        <span class="kw">in</span> <span class="dt">Right</span> (forgetName result)</span></code></pre></div>
<h1 id="new-developments">New Developments</h1>
<p>In summary, you have got a way of referring to term-level variables on
the type level. This allows you to express function’s preconditions
and postconditions through their type. This technique can easily be
applied to many other libraries.</p>
<p>But there is the drawback of making libraries less ergonomic and
harder to understand.</p>
<div class="sourceCode" id="cb24"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb24-1"><a href="#cb24-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- Named.hs</span></span>
<span id="cb24-2"><a href="#cb24-2" aria-hidden="true" tabindex="-1"></a><span class="ot">{-# language RankNTypes #-}</span></span>
<span id="cb24-3"><a href="#cb24-3" aria-hidden="true" tabindex="-1"></a><span class="ot">name ::</span></span>
<span id="cb24-4"><a href="#cb24-4" aria-hidden="true" tabindex="-1"></a>  a <span class="ot">-&gt;</span></span>
<span id="cb24-5"><a href="#cb24-5" aria-hidden="true" tabindex="-1"></a>  (<span class="kw">forall</span> name<span class="op">.</span> <span class="dt">Named</span> name a <span class="ot">-&gt;</span> result) <span class="ot">-&gt;</span></span>
<span id="cb24-6"><a href="#cb24-6" aria-hidden="true" tabindex="-1"></a>  result</span></code></pre></div>
<p><code class="sourceCode haskell">name</code> and its type is somewhat alien and complicated. And
it is somewhat unergonomic to use as you can see in the lines below
line (1) in <code class="sourceCode haskell">Main.processLists</code>.</p>
<div class="sourceCode" id="cb25"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb25-1"><a href="#cb25-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- Main.hs</span></span>
<span id="cb25-2"><a href="#cb25-2" aria-hidden="true" tabindex="-1"></a><span class="ot">processLists ::</span></span>
<span id="cb25-3"><a href="#cb25-3" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Integer</span> <span class="ot">-&gt;</span> <span class="dt">Ordering</span>) <span class="ot">-&gt;</span></span>
<span id="cb25-4"><a href="#cb25-4" aria-hidden="true" tabindex="-1"></a>  [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb25-5"><a href="#cb25-5" aria-hidden="true" tabindex="-1"></a>  [<span class="dt">Integer</span>]                        <span class="ot">-&gt;</span></span>
<span id="cb25-6"><a href="#cb25-6" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Either</span> <span class="dt">AppError</span> [<span class="dt">Integer</span>]</span>
<span id="cb25-7"><a href="#cb25-7" aria-hidden="true" tabindex="-1"></a>processLists comparator list1 list2 <span class="ot">=</span></span>
<span id="cb25-8"><a href="#cb25-8" aria-hidden="true" tabindex="-1"></a>  <span class="co">-- (1)</span></span>
<span id="cb25-9"><a href="#cb25-9" aria-hidden="true" tabindex="-1"></a>  name comparator <span class="op">$</span> \comparatorNamed <span class="ot">-&gt;</span></span>
<span id="cb25-10"><a href="#cb25-10" aria-hidden="true" tabindex="-1"></a>  name list1      <span class="op">$</span> \list1Named      <span class="ot">-&gt;</span></span>
<span id="cb25-11"><a href="#cb25-11" aria-hidden="true" tabindex="-1"></a>  name list2      <span class="op">$</span> \list2Named      <span class="ot">-&gt;</span></span>
<span id="cb25-12"><a href="#cb25-12" aria-hidden="true" tabindex="-1"></a>  <span class="kw">case</span> validateLists comparatorNamed list1Named list2Named <span class="kw">of</span></span>
<span id="cb25-13"><a href="#cb25-13" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Left</span> appError          <span class="ot">-&gt;</span> <span class="dt">Left</span> appError</span>
<span id="cb25-14"><a href="#cb25-14" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Right</span> (proof1, proof2) <span class="ot">-&gt;</span></span>
<span id="cb25-15"><a href="#cb25-15" aria-hidden="true" tabindex="-1"></a>      <span class="co">-- (2)</span></span>
<span id="cb25-16"><a href="#cb25-16" aria-hidden="true" tabindex="-1"></a>      mergeBy comparatorNamed (list1Named, proof1) (list2Named, proof2) <span class="op">$</span></span>
<span id="cb25-17"><a href="#cb25-17" aria-hidden="true" tabindex="-1"></a>        \(result, _proof) <span class="ot">-&gt;</span></span>
<span id="cb25-18"><a href="#cb25-18" aria-hidden="true" tabindex="-1"></a>        <span class="dt">Right</span> (forgetName result)</span></code></pre></div>
<p>But it does not stop there. Other library functions or even user
functions might become infected with this <em>continuation-passing style</em>
too as you can see in the lines below line (2) where
<code class="sourceCode haskell">mergeBy</code> is used. This happens when functions return new
<code class="sourceCode haskell"><span class="dt">Named</span></code> values. <code class="sourceCode haskell">mergeBy</code> needs to wrap its result
into <code class="sourceCode haskell"><span class="dt">Named</span></code> in order to express its postcondition that its
result is sorted.</p>
<div class="sourceCode" id="cb26"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb26-1"><a href="#cb26-1" aria-hidden="true" tabindex="-1"></a><span class="co">-- Merge.hs</span></span>
<span id="cb26-2"><a href="#cb26-2" aria-hidden="true" tabindex="-1"></a><span class="ot">{-# language RankNTypes #-}</span></span>
<span id="cb26-3"><a href="#cb26-3" aria-hidden="true" tabindex="-1"></a><span class="ot">mergeBy ::</span></span>
<span id="cb26-4"><a href="#cb26-4" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Named</span> comparator (a <span class="ot">-&gt;</span> a <span class="ot">-&gt;</span> <span class="dt">Ordering</span>)                                <span class="ot">-&gt;</span></span>
<span id="cb26-5"><a href="#cb26-5" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Named</span> list1 [a], <span class="dt">SortedBy</span> comparator list1)                         <span class="ot">-&gt;</span></span>
<span id="cb26-6"><a href="#cb26-6" aria-hidden="true" tabindex="-1"></a>  (<span class="dt">Named</span> list2 [a], <span class="dt">SortedBy</span> comparator list2)                         <span class="ot">-&gt;</span></span>
<span id="cb26-7"><a href="#cb26-7" aria-hidden="true" tabindex="-1"></a>  (<span class="kw">forall</span> result<span class="op">.</span> (<span class="dt">Named</span> result [a], <span class="dt">SortedBy</span> comparator result) <span class="ot">-&gt;</span> r) <span class="ot">-&gt;</span></span>
<span id="cb26-8"><a href="#cb26-8" aria-hidden="true" tabindex="-1"></a>  r</span>
<span id="cb26-9"><a href="#cb26-9" aria-hidden="true" tabindex="-1"></a>mergeBy comparator (list1, _proof1) (list2, _proof2) continuation <span class="ot">=</span></span>
<span id="cb26-10"><a href="#cb26-10" aria-hidden="true" tabindex="-1"></a>  name (go (forgetName list1) (forgetName list2)) <span class="op">$</span> \result <span class="ot">-&gt;</span></span>
<span id="cb26-11"><a href="#cb26-11" aria-hidden="true" tabindex="-1"></a>    continuation (result, <span class="dt">SortedBy</span>)</span>
<span id="cb26-12"><a href="#cb26-12" aria-hidden="true" tabindex="-1"></a>  <span class="kw">where</span></span>
<span id="cb26-13"><a href="#cb26-13" aria-hidden="true" tabindex="-1"></a>    go list1 [] <span class="ot">=</span> list1</span>
<span id="cb26-14"><a href="#cb26-14" aria-hidden="true" tabindex="-1"></a>    go [] list2 <span class="ot">=</span> list2</span>
<span id="cb26-15"><a href="#cb26-15" aria-hidden="true" tabindex="-1"></a>    go (head1 <span class="op">:</span> tail1) (head2 <span class="op">:</span> tail2)</span>
<span id="cb26-16"><a href="#cb26-16" aria-hidden="true" tabindex="-1"></a>      <span class="op">|</span> <span class="dt">LT</span> <span class="ot">&lt;-</span> (forgetName comparator) head2 head1 <span class="ot">=</span></span>
<span id="cb26-17"><a href="#cb26-17" aria-hidden="true" tabindex="-1"></a>        head2 <span class="op">:</span> go (head1 <span class="op">:</span> tail1) tail2</span>
<span id="cb26-18"><a href="#cb26-18" aria-hidden="true" tabindex="-1"></a>      <span class="op">|</span> <span class="fu">otherwise</span> <span class="ot">=</span></span>
<span id="cb26-19"><a href="#cb26-19" aria-hidden="true" tabindex="-1"></a>        head1 <span class="op">:</span> go tail1 (head2 <span class="op">:</span> tail2)</span></code></pre></div>
<p>GHC core developer Richard Eisenberg has recently revealed
<span class="citation" data-cites="eisenberg_compositional">[@eisenberg_compositional]</span> that he is working on a keyword to declare
about type variables that no assumptions may be made about them,
getting rid of the continuation-passing style and their ergonomic
overhead entirely.</p>
<p>He is working on a paper on lightweight existential types with some
collaborators, which he hopes to submit for ICFP 2021 by 2021-03-02
<span class="citation" data-cites="eisenberg_berlin">[@eisenberg_berlin]</span>.</p>
<p>This work is not motivated by <em>Ghosts of Departed Proofs</em> but
interestingly by the other two approaches to formal verification of
<em>Dependent Haskell</em> and <em>LiquidHaskell</em> <span class="citation" data-cites="eisenberg_tweag">[@eisenberg_tweag]</span>.</p>
<p>He thinks we will have a way of getting an <code>exists</code> keyword that
syntactically works like <code class="sourceCode haskell"><span class="kw">forall</span></code> <span class="citation" data-cites="eisenberg_tweag">[@eisenberg_tweag]</span> and
hopes to have a new extension to GHC roughly by May
<span class="citation" data-cites="eisenberg_berlin">[@eisenberg_berlin]</span>. <code class="sourceCode haskell">name</code>’s type might then look as
straightforward as follows again.</p>
<div class="sourceCode" id="cb27"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb27-1"><a href="#cb27-1" aria-hidden="true" tabindex="-1"></a><span class="ot">name ::</span> exists name<span class="op">.</span> a <span class="ot">-&gt;</span> <span class="dt">Named</span> name a</span></code></pre></div>
<h1 id="references">References</h1>
    </section>
</article>

        </main>

        <footer>
            Site proudly generated by
            <a href="http://jaspervdj.be/hakyll">Hakyll</a>
        </footer>
    </body>
</html>