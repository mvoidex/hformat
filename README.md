hformat
=======

Format string with named args
<pre>
format "My name is $name, I am $age years old" ["name" %= "Joe", "age" %= 24]
</pre>

To escape '$', double it. Format string can be also used recursively with <code>(%%)</code> instead of <code>(%=)</code>
<pre>
format "$$x is $x, and $$r is $r" ["x" %= 5, "r" %% "$x + $x"] -- Note (%%) instead of (%=)
</pre>
