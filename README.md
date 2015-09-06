hformat
=======

Format string with named args
<pre>
-- Named args
"My name is {name}, I am {age} years old" ~~ ("name" %= "Joe") ~~ ("age" %= 24) ≡ "My name is Joe, I am 24 years old"
-- Arg can have default value
"{var:x} = {val:10}" ~~ ("var" %= y) ≡ "y = 10"
-- Numeric position can be used
"{0} {1} {0}" ~~ "foo" ~~ "bar" ≡ "foo bar foo"
-- Positions can be omitted
"{} {}" ~~ "foo" ~~ 10 ≡ "foo 10"
-- Double braces to escape them
"{} and {{}}" ~~ 10 ≡ "10 and {}"
</pre>
