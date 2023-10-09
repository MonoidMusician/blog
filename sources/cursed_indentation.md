---
title: Cused Mixed Indentation
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---


```erlang
result() ->
	(((uncurriedMore())
	  ({ 3
	   , 4
	   , <<"hi">>
	   , <<"there">>
	   , case someFn() of
		     1 -> 5
		     2 -> 4
		     3 -> 2
		     4 -> 1
		     _ -> 0
	     end}))
	 (fun
		  (_) ->
			  <<"">>
	  end))
	({7, <<"hithere">>}).
```
