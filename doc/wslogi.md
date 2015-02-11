

# Module wslogi #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


A websocket logging framework for Erlang/OTP.
Copyright (c) 2015 Hinagiku Soranoba All Rights Reserved.

<a name="description"></a>

## Description ##
<br />
<a name="types"></a>

## Data Types ##




### <a name="type-header_key">header_key()</a> ###



<pre><code>
header_key() = atom()
</code></pre>





### <a name="type-header_value">header_value()</a> ###



<pre><code>
header_value() = term()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#alert-2">alert/2</a></td><td>Equivalent to <a href="#alert-3"><tt>alert("/", Format, Args)</tt></a>.</td></tr><tr><td valign="top"><a href="#alert-3">alert/3</a></td><td>Output the log in alert level.</td></tr><tr><td valign="top"><a href="#clear_headers-0">clear_headers/0</a></td><td>Delete the process-specific all log header.</td></tr><tr><td valign="top"><a href="#critical-2">critical/2</a></td><td>Equivalent to <a href="#critical-3"><tt>critical("/", Format, Args)</tt></a>.</td></tr><tr><td valign="top"><a href="#critical-3">critical/3</a></td><td>Output the log in critical level.</td></tr><tr><td valign="top"><a href="#debug-2">debug/2</a></td><td>Equivalent to <a href="#debug-3"><tt>debug("/", Format, Args)</tt></a>.</td></tr><tr><td valign="top"><a href="#debug-3">debug/3</a></td><td>Output the log in debug level.</td></tr><tr><td valign="top"><a href="#delete_headers-1">delete_headers/1</a></td><td>Delete the process-specific log header corresponding to Keys.</td></tr><tr><td valign="top"><a href="#emergency-2">emergency/2</a></td><td>Equivalent to <a href="#emergency-3"><tt>emergency("/", Format, Args)</tt></a>.</td></tr><tr><td valign="top"><a href="#emergency-3">emergency/3</a></td><td>Output the log in emergency level.</td></tr><tr><td valign="top"><a href="#error-2">error/2</a></td><td>Equivalent to <a href="#error-3"><tt>error("/", Format, Args)</tt></a>.</td></tr><tr><td valign="top"><a href="#error-3">error/3</a></td><td>Output the log in error level.</td></tr><tr><td valign="top"><a href="#get_headers-0">get_headers/0</a></td><td>Get the process-specific log header.</td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td>Equivalent to <a href="#info-3"><tt>info("/", Format, Args)</tt></a>.</td></tr><tr><td valign="top"><a href="#info-3">info/3</a></td><td>Output the log in info level.</td></tr><tr><td valign="top"><a href="#notice-2">notice/2</a></td><td>Equivalent to <a href="#notice-3"><tt>notice("/", Format, Args)</tt></a>.</td></tr><tr><td valign="top"><a href="#notice-3">notice/3</a></td><td>Output the log in notice level.</td></tr><tr><td valign="top"><a href="#set_headers-1">set_headers/1</a></td><td>Add the process-specific log header.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Start the websocket server for wslogi in this port.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stop the websocket server of this port.</td></tr><tr><td valign="top"><a href="#verbose-2">verbose/2</a></td><td>Equivalent to <a href="#verbose-3"><tt>verbose("/", Format, Args)</tt></a>.</td></tr><tr><td valign="top"><a href="#verbose-3">verbose/3</a></td><td>Output the log in verbose level.</td></tr><tr><td valign="top"><a href="#warning-2">warning/2</a></td><td>Equivalent to <a href="#warning-3"><tt>warning("/", Format, Args)</tt></a>.</td></tr><tr><td valign="top"><a href="#warning-3">warning/3</a></td><td>Output the log in warning level.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="alert-2"></a>

### alert/2 ###


<pre><code>
alert(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Equivalent to [`alert("/", Format, Args)`](#alert-3).
<a name="alert-3"></a>

### alert/3 ###


<pre><code>
alert(Path::<a href="file.md#type-filename">file:filename()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Output the log in alert level.
<a name="clear_headers-0"></a>

### clear_headers/0 ###


<pre><code>
clear_headers() -&gt; ok
</code></pre>
<br />

Delete the process-specific all log header.
<a name="critical-2"></a>

### critical/2 ###


<pre><code>
critical(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Equivalent to [`critical("/", Format, Args)`](#critical-3).
<a name="critical-3"></a>

### critical/3 ###


<pre><code>
critical(Path::<a href="file.md#type-filename">file:filename()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Output the log in critical level.
<a name="debug-2"></a>

### debug/2 ###


<pre><code>
debug(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Equivalent to [`debug("/", Format, Args)`](#debug-3).
<a name="debug-3"></a>

### debug/3 ###


<pre><code>
debug(Path::<a href="file.md#type-filename">file:filename()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Output the log in debug level.
<a name="delete_headers-1"></a>

### delete_headers/1 ###


<pre><code>
delete_headers(Keys::[<a href="#type-header_key">header_key()</a>]) -&gt; ok
</code></pre>
<br />

Delete the process-specific log header corresponding to Keys.
<a name="emergency-2"></a>

### emergency/2 ###


<pre><code>
emergency(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Equivalent to [`emergency("/", Format, Args)`](#emergency-3).
<a name="emergency-3"></a>

### emergency/3 ###


<pre><code>
emergency(Path::<a href="file.md#type-filename">file:filename()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Output the log in emergency level.
<a name="error-2"></a>

### error/2 ###


<pre><code>
error(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Equivalent to [`error("/", Format, Args)`](#error-3).
<a name="error-3"></a>

### error/3 ###


<pre><code>
error(Path::<a href="file.md#type-filename">file:filename()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Output the log in error level.
<a name="get_headers-0"></a>

### get_headers/0 ###


<pre><code>
get_headers() -&gt; [{<a href="#type-header_key">header_key()</a>, <a href="#type-header_value">header_value()</a>}]
</code></pre>
<br />

Get the process-specific log header.
<a name="info-2"></a>

### info/2 ###


<pre><code>
info(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Equivalent to [`info("/", Format, Args)`](#info-3).
<a name="info-3"></a>

### info/3 ###


<pre><code>
info(Path::<a href="file.md#type-filename">file:filename()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Output the log in info level.
<a name="notice-2"></a>

### notice/2 ###


<pre><code>
notice(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Equivalent to [`notice("/", Format, Args)`](#notice-3).
<a name="notice-3"></a>

### notice/3 ###


<pre><code>
notice(Path::<a href="file.md#type-filename">file:filename()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Output the log in notice level.
<a name="set_headers-1"></a>

### set_headers/1 ###


<pre><code>
set_headers(KVs::[{<a href="#type-header_key">header_key()</a>, <a href="#type-header_value">header_value()</a>}]) -&gt; ok
</code></pre>
<br />


Add the process-specific log header.



Specific key-value


| key-value                 | description                            |
|:--------------------------|:---------------------------------------|
| `{ip, inet:ip_address()}` | It is used in the `filter ip` command. |

<a name="start-1"></a>

### start/1 ###


<pre><code>
start(Port::<a href="inet.md#type-port_number">inet:port_number()</a>) -&gt; ok | {error, Reason::term()}
</code></pre>
<br />

Start the websocket server for wslogi in this port.

__See also:__ [wslogi:set_headers/1](wslogi.md#set_headers-1).
<a name="stop-1"></a>

### stop/1 ###


<pre><code>
stop(Port::<a href="inet.md#type-port_number">inet:port_number()</a>) -&gt; ok | {error, not_found}
</code></pre>
<br />

Stop the websocket server of this port.
<a name="verbose-2"></a>

### verbose/2 ###


<pre><code>
verbose(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Equivalent to [`verbose("/", Format, Args)`](#verbose-3).
<a name="verbose-3"></a>

### verbose/3 ###


<pre><code>
verbose(Path::<a href="file.md#type-filename">file:filename()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Output the log in verbose level.
<a name="warning-2"></a>

### warning/2 ###


<pre><code>
warning(Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Equivalent to [`warning("/", Format, Args)`](#warning-3).
<a name="warning-3"></a>

### warning/3 ###


<pre><code>
warning(Path::<a href="file.md#type-filename">file:filename()</a>, Format::<a href="io.md#type-format">io:format()</a>, Args::[term()]) -&gt; ok
</code></pre>
<br />

Output the log in warning level.
