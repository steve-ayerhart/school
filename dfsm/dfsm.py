#! /usr/bin/python
import sys, re, os.path

# Steve Erhart
# CSCI354
# DFSM Simulator


# transitions is a list of dictionaries, ie, [{'a' : 0}] has 1 state (state 0) and acceptins the char 'a' and 0 is the next state
# the list indexes represent the state and the dictionary looks up the next state depending on the char provided

# alphabet is just a list of chars

# accepting_states is a list of integers
def build_machine(filename):
	"""Returns the list the transitions, alphabet, and accepting_states from machine definition file provided"""
	temp = open(filename, 'r')
	def_lines = temp.readlines()
	temp.close()

	# i just need the # of transitions to create the transitions list
	transitions = [{} for x in range(0, int(re.match(r"^([0-9]),", def_lines[0]).group(1)))]

	# anything but a newline or a comma should be in the alphabet
	alphabet = re.findall(r"[^\n,]", def_lines[1])
	# assumming no more than "9" accepting states
	accepting_states = map(int, (re.findall(r"[0-9]", def_lines[2])))

	for line in def_lines[3:]:
		# transitions should of the form <number>,<alphabet char>,<number>
		cur, alpha, next = re.match(r"^([0-9]),([^\n,]),([0-9])", line).groups()
		transitions[int(cur)].update({alpha : int(next)})

	return transitions, alphabet, accepting_states

def run_machine(trans, alphabet, accepting_states, str):
	current_state = 0
	for char in str:
		# make sure we are in alphabet and we have a transition
		if char in alphabet and char in trans[current_state]:
			current_state = trans[current_state][char]
		else:
			return False

	return current_state in accepting_states


if __name__ == "__main__":
	if len(sys.argv) != 2:
		print "Usage: dfsm <machine definition> [string]"
		sys.exit(1)

	if os.path.isfile(sys.argv[1]):
		transitions, alphabet, accepting_states = build_machine(sys.argv[1])
	else:
		print "Not a Valid File"
		sys.exit(1)
	
	if len(sys.argv) == 3:
		instring = argv[2]
	else:
		print "Enter test string: "
		instring =sys.stdin.readline()[:-1]

	if run_machine(transitions, alphabet, accepting_states, instring):
		print "Accepted"
	else:
		print "Rejected"
