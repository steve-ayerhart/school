proc main
	input x, y
	if x < y then 
		output y
		call blah
	{
		output x
	} endif
	return

proc blah
	call yeah 
	output x
	return

proc yeah
	output y
	return 
