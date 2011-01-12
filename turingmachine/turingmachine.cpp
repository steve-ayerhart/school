// Steven Erhart
// CSCI 385

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <boost/regex.hpp>
#include <fstream>
#include <string>
#include <vector>
using namespace std;

int next_state(char, char, vector< vector<char> >);
bool can_move(char, int);
char* itoa(int n);
string tr_wrap(string, int, char, int, char, char);
string li_wrap(char, char, char, char, char);
string tr_halt(string curtape, int pos);

vector<char> read_tm(ifstream&);
int main(int argc, char *argv[]){

	if(argc < 3){
		cerr << "I REQUIRE 2 ARGUMENTS: inputfile & outputfile" << endl;
		return 1;
	}

	string html_head = "<html><head><title>Steve Erhart: CSCI385</title></head><body>";
	string html_foot = "</body></html>";
	string tabl_head = "<table border = \"0\" width=\"50%\" cellpadding = \"5\"><tr><th>Tape</th><th>State</th><th>Rule</th><th>Symbol</th><th>Dir</th></tr>";
	string tabl_foot = "</table>";
	string list_head = "<h1>rules</h1><ol>";

	ofstream tablefile(argv[2]);
	ifstream inputfile(argv[1]);
	char num_rules[3];
	inputfile.getline(num_rules, 3);
	vector< vector<char> > trans;
	trans.reserve(atoi(num_rules));

	vector<char> blah;
	for(int i = 0; i < atoi(num_rules); i++){
		trans.push_back(read_tm(inputfile));
	}

    // setup tape
	string tape;
	inputfile >> tape;
	tape.insert(0, "_");
	tape += "__________...";

	int tape_pos = 1;

	char current_state = '1';
	char current_symbol = tape[tape_pos];
	int rule = next_state(current_state, current_symbol, trans);	

	tablefile << html_head << tabl_head;
	tablefile << list_head;

	for(int i = 0; i < atoi(num_rules); i++)
		tablefile << li_wrap(trans[i][0], trans[i][1], trans[i][2], trans[i][3], trans[i][4]);

	tablefile << "</ol><h1>trace</h1>";

    // run machine
	for(;;){
		current_symbol = tape[tape_pos];
		rule = next_state(current_state, current_symbol, trans);	
		current_state = trans[rule][2];
		tablefile << tr_wrap(tape, tape_pos, current_state,  rule, trans[rule][3], trans[rule][4]) << endl;
		//	(current_state, input, output, next_state, direction)

		if(current_state == 'F')
			break;


		if(!can_move(trans[rule][4], tape_pos))
			break;

		tape[tape_pos] = trans[rule][3];
		if(trans[rule][4] == 'R')
			tape_pos++;
		else
			tape_pos--;

	}

	//rule = next_state(current_state, current_symbol, trans);	
	//current_state = trans[rule][2];
	if(trans[rule][4] == 'R')
		tape_pos++;
	else
		tape_pos--;

	tablefile << tr_halt(tape, tape_pos);

	tablefile << tabl_foot << html_foot;
	tablefile.close();
	inputfile.close();
	return 0;
}

int next_state(char state, char symbol, vector< vector<char> > rules){
	for(int i = 0; i < rules.size(); i++){
		if(state == rules[i][0] && symbol == rules[i][1])
			return i;
	}
}

bool can_move(char direc, int pos){
	if(direc == 'R')
		pos++;
	else
		pos--;

	if(pos >= 0)
		return true;
	else 
		return false;
}

string tr_wrap(string curtape, int pos, char st, int rl, char sym, char dir){
	rl++;
	string trstr = "<tr>";
	string td = "<td align = \"center\">";
	string tde = "</td>";
	string diamond = "&loz;";
	boost::regex underpat("_", boost::regex_constants::icase);

	curtape.insert(pos + 1, "</font>");
	curtape.insert(pos, "<font color = \"red\">");

	trstr.append(td); trstr.append(curtape); trstr.append(tde);
	trstr.append(td); trstr += st; trstr.append(tde);
	trstr.append(td); trstr += itoa(rl); trstr.append(tde);
	trstr.append(td); trstr += sym; trstr.append(tde);
	trstr.append(td); trstr += dir; trstr.append(tde);

	trstr.append("</tr>");

	return boost::regex_replace(trstr, underpat, diamond);
}

string tr_halt(string curtape, int pos){
	string trstr = "<tr>";
	string td = "<td align = \"center\">";
	string tde = "</td>";
	string diamond = "&loz;";
	boost::regex underpat("_", boost::regex_constants::icase);

	curtape.insert(pos + 1, "</font>");

	curtape.insert(pos + 1, "</font>");
	curtape.insert(pos, "<font color = \"red\">");

	trstr.append(td); trstr.append(curtape); trstr.append(tde);
	trstr.append(td); trstr.append("HALT"); trstr.append(tde);
	trstr.append("</tr>");
	return boost::regex_replace(trstr, underpat, diamond);
}


string li_wrap(char st, char in, char nst, char out, char dir){
	string listr = "<li>&delta;(";
	listr += st; listr += ", ";
	listr += in; listr += ", ";
	listr += ") = (";
	listr += nst; listr += ", ";
	listr += out; listr += ", ";
	listr += dir; listr += ", ";
	listr += ")</li>";

	return listr;
}



vector<char> read_tm(ifstream& infile){
	char input[20];
	infile.getline(input,20);
	vector<char> temp;
	temp.push_back(input[1]);
	temp.push_back(input[4]);
	temp.push_back(input[7]);
	temp.push_back(input[10]);
	temp.push_back(input[13]);
	return temp;
}

char* itoa(int n){
	char* blah;
	blah = (char*) malloc(17);
	sprintf(blah, "%d", n);
	return blah;
}

