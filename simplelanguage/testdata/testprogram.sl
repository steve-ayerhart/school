proc main
	input x, y
	while x != y do
		call testit
		input x, y
	endwhile
return

proc blah
	set x to 1 * 1
return

proc testit
	if x < y then
		set x to 3 * 3
		set x to x + x
		output y
	{
		output x
	} endif
reTurn

