#include <string>
#include <sstream>
#include <map>
#include <vector>
#include <iostream>

using namespace std;

const vector <string> terminalSymbols = {"BOF", "BECOMES", "COMMA", "ELSE", "EOF", "EQ", "GE", "GT", "ID", "IF", "INT", "LBRACE", "LE", "LPAREN",
										"LT", "MINUS", "NE", "NUM", "PCT", "PLUS", "PRINTLN", "RBRACE", "RETURN", "RPAREN", "SEMI", "SLASH",
										"STAR", "WAIN", "WHILE", "AMP", "LBRACK", "RBRACK", "NEW", "DELETE", "NULL"};

enum type {integer = 0, intStar = 1, other = -1};

struct Tree{
	string LHS;
	vector <string> RHS;
	vector<Tree *> children;
	string lexeme; //var name
	string kind; //var type
	enum type type;
};

typedef struct symbol_table{
	string functionName;
	vector<pair<string, string>> functionParams;
	map<string, string> symbols; //<var name, var type>
	map<string, int> symbolIndex;

	bool add_symbol(string lexeme, string type) {
		int index = symbols.size() * 4;
		if (symbols.emplace(lexeme, type).second == false) {
			cerr << "ERROR: failed to add symbol " << lexeme << " to symbol table." << endl;;
			return false;
		} else {
			symbolIndex.emplace(lexeme, index);
			return true;
		}
	}

} symbol_table;

map<string, symbol_table*> globalSymbolTable; //function name and symbol table for the function
string currentProcedure; // stores current procedure
int loopCount = 0; //for when translating to mips
int ifCount = 0;
int deleteCount = 0;
int arglistExprCount = 0;

void parseTreeDestructor(Tree* tree){
	for(auto & i: tree->children){ parseTreeDestructor(i); }
	delete tree;
}

bool checkTerminal(string s) { // returns true if its a terminal
	for(int i = 0; i < terminalSymbols.size(); i++){ if(s == terminalSymbols[i]) return true; }
	return false;
}

Tree* newChild(){
	string s;
	Tree* t = new Tree();
	string word;
	if(getline(cin, s)){
		istringstream iss(s);
		iss >> word;
		if(checkTerminal(word)){
			t->kind = word;
			iss >> word;
			t->lexeme = word;
		}
		t->LHS = word;
		while(iss >> word){ (t->RHS).push_back(word); }
	}
	return t;
}

Tree* makeTree(){
	Tree* tree = newChild();
	if(!checkTerminal(tree->LHS)){
		for(int i = 0; i < (tree->RHS).size(); i++){ tree->children.push_back(makeTree()); }
	}
	return tree;
}

void print(Tree *tree){
	if(!tree) return;
	for(auto t: tree->children){ print(t); }
	if(tree->kind != "") { cout << tree->kind << " " << tree->lexeme << endl; }
	else{
		cout << tree->LHS << " ";
		for(int i = 0; i < (tree->RHS).size(); i++){ cout << (tree->RHS)[i] << " "; }
		cout << endl;
	}
}

void destructSymbolTable() { for (auto & p: globalSymbolTable) { delete p.second; } }

void printSymbolTable(){
	for(auto & p: globalSymbolTable) {
		cerr << p.first;
		for(auto& i: p.second->functionParams){ cerr << " " << i.second; }
		cerr << endl;
		for (auto & id_type: p.second->symbols) { cerr << id_type.first << " " << id_type.second << endl; }
		cerr << endl;
	}
}

bool checkDuplicateSignature(vector<pair<string, string>> signature){
	for (int i = 0; i < signature.size(); i++){
		for (int j = 0; j < signature.size(); j++){
			if (j != i && signature[i].first == signature[j].first) { return true; }
		}
	}
	return false;
}

pair<string, string> parseDcl(Tree * tree) {
	string type = tree->children[0]->children.size() == 1 ? "int" : "int*";
	string id = tree->children[1]->lexeme;
	return pair<string, string>(id, type);
}

void getSignature(Tree* tree, vector<pair<string, string>>& sig) {
	sig.push_back(parseDcl(tree->children[0]));
	if(tree->children.size() == 3) { getSignature(tree->children[2], sig); }
}

void addNewSignature(Tree* tree) {
	vector<pair<string, string>> sig;
	if(tree->children[3]->children.size()) { getSignature(tree->children[3]->children[0], sig); } 
	if(checkDuplicateSignature(sig)){ cerr << "ERROR: duplicate signature" << endl; return; }
	symbol_table* st = new symbol_table();
	globalSymbolTable[currentProcedure] = st;
	globalSymbolTable[currentProcedure]->functionName = currentProcedure;
	globalSymbolTable[currentProcedure]->functionParams = sig;
	for (auto& p: sig) { globalSymbolTable[currentProcedure]->add_symbol(p.first, p.second); } 
}

void updateTable(Tree* tree) {
	if(tree->LHS == "dcl") {
		pair<string, string> p = parseDcl(tree);
		if (!globalSymbolTable[currentProcedure]->add_symbol(p.first, p.second)){
			cerr << "ERROR: double decleration of " << p.first << "with type " << p.second << endl;
			return;
		}
	}
}

void countExpr(Tree* tree){
	if(tree->RHS.size() == 1) { arglistExprCount++; }
	else{
		arglistExprCount++;
		countExpr(tree->children[2]);
	}
}

void parseFunction(Tree* tree);

void getArgListType(Tree* tree, vector<string>& exprType){
	parseFunction(tree->children[0]);
	if(tree->children[0]->type == integer) { exprType.push_back("int"); }
	else { exprType.push_back("int*"); }
	if(tree->RHS.size() == 3){ getArgListType(tree->children[2], exprType); }
}

bool isMatch(Tree* tree, string functionName){
	arglistExprCount = 0;
	countExpr(tree);
	vector<pair<string, string>>& parameterList = globalSymbolTable[functionName]->functionParams;
	if(arglistExprCount == parameterList.size()){
		vector<string> exprTypes;
		getArgListType(tree, exprTypes);
		for(int i = 0; i < parameterList.size(); i++){
			string t1 = parameterList[i].second;
			string t2 = exprTypes[i];
			if(t1 != t2){ return false; }
			else{ continue; }
		}
		return true;
	}
	return false;
}

void parseFunction(Tree* tree){
	map<string, string>& symbolTable = globalSymbolTable[currentProcedure]->symbols;
	if(tree->LHS == "dcl") updateTable(tree);
	else if(tree->LHS == "factor" && tree->children.size() >= 2 && tree->RHS[0] == "ID"
			&& tree->children[1]->lexeme == "(" && (globalSymbolTable.count(tree->children[0]->lexeme) == 0 || symbolTable.count(tree->children[0]->lexeme))){
		cerr << "ERROR: double declaration with factor or lvalue as LHS" << endl;
		return;
	}
	else if(tree->LHS == "factor" && tree->children.size() >= 2 && tree->RHS[0] == "ID" && tree->children[1]->lexeme == "("
					&& (globalSymbolTable.count(tree->children[0]->lexeme) == 0 || symbolTable.count(tree->children[0]->lexeme))) {
		cerr << "ERROR: can't call this function" << endl;
		return;
	}
	else if(((tree->RHS).size() == 1) && ((tree->LHS == "factor") || (tree->LHS == "lvalue")) && (tree->RHS[0] == "ID") && (symbolTable.count(tree->children[0]->lexeme) == 0)) {
		cerr << "ERROR: undefined variable" << endl;
		return;
	}
	else if(tree->LHS == "dcls" && tree->RHS.size() == 5){ // dcls → dcls dcl BECOMES NUM SEMI OR dcls → dcls dcl BECOMES NULL SEMI
		if(tree->RHS[3] == "NUM"){
			if(tree->children[1]->children[0]->RHS.size() != 1){ cerr << "ERROR: derived dcl doesn't derive a sequence containing a type that derives INT" << endl; return; }
			else{
				parseFunction(tree->children[1]);
				parseFunction(tree->children[0]);
			}
		}
		else if(tree->RHS[3] == "NULL"){
			if(tree->children[1]->children[0]->RHS.size() != 2){ cerr << "ERROR: derived dcl doesn't derive a sequence containing a type that derives INT*" << endl; return; }
			else{
				parseFunction(tree->children[1]);
				parseFunction(tree->children[0]);
			}
		}
	}
	else if(tree->LHS == "statement") {
		if (tree->RHS[0] == "lvalue") {
			parseFunction(tree->children[0]);
			parseFunction(tree->children[2]);
			if (tree->children[0]->type != tree->children[2]->type) { cerr << "ERROR: lvalue and expr type missmatch" << endl; return; }

		} else if (tree->RHS[0] == "PRINTLN") {
			parseFunction(tree->children[2]);
			if (tree->children[2]->type != integer){ cerr << "ERROR: PRINTLN on non integer" << endl; return; }

		} else if (tree->RHS[0] == "DELETE") {
			parseFunction(tree->children[3]);
			if (tree->children[3]->type != intStar) { cerr << "ERROR: cannot delete a non pointer type" << endl; return; }

		}
		else { for(auto& i: tree->children) parseFunction(i); }
	}
	else if (tree->LHS == "test" && tree->RHS.size() == 3){
		parseFunction(tree->children[0]);
		parseFunction(tree->children[2]);
		if(tree->children[0]->type != tree->children[2]->type){ cerr << "ERROR: invalid type in tests derivation" << endl; return;}
	}
	else if(tree->LHS == "expr"){
		if(tree->RHS.size() == 1){
			parseFunction(tree->children[0]);
			tree->type = tree->children[0]->type;
			return;
		}

		parseFunction(tree->children[0]);
		parseFunction(tree->children[2]);

		if(tree->RHS[1] == "PLUS"){ //error if expr and term have type int*
			int type = static_cast<int>(tree->children[0]->type) + static_cast<int>(tree->children[2]->type);
			if(type >= 2){ cerr << "ERROR: invalid type in expr PLUS derivation" << endl; return; }
			tree->type = static_cast<enum type>(type);
		}
		if(tree->RHS[1] == "MINUS"){   //error if expr has type int and term has int *
			int type = static_cast<int>(tree->children[0]->type) - static_cast<int>(tree->children[2]->type);
			if(type <= -1){ cerr << "ERROR: invalid type in expr MINUS derivation" << endl; return; }
			tree->type = static_cast<enum type>(type);
		}
	}
	else if(tree->LHS == "term"){
		if(tree->RHS.size() == 1){ //term → factor
			parseFunction(tree->children[0]);
			tree->type = tree->children[0]->type;
		}
		else if(tree->RHS.size() == 3){ //term → term STAR factor, term → term SLASH factor, term → term PCT factor
			parseFunction(tree->children[2]);
			parseFunction(tree->children[0]);
			if(tree->children[0]->type == integer && tree->children[2]->type == integer) { tree->type = integer; }
			else { cerr << "ERROR: invalid input term -> term STAR/SLASH/PCT factore derivation" << endl; return;}
		}
	}
	else if(tree->LHS == "factor"){
		if(tree->RHS.size() == 1){ //ID   NUM   NULL
			if(tree->RHS[0] == "NUM") { tree->type = integer; }
			else if(tree->RHS[0] == "NULL") { tree->type = intStar; }
			else if(tree->RHS[0] == "ID"){
				auto it = globalSymbolTable.find(tree->children[0]->lexeme);  //check if ID is a procedure
				if(it == globalSymbolTable.end()){ //id is not a procedure
					if(symbolTable[tree->children[0]->lexeme] == "int") { tree->type = integer; }
					else if(symbolTable[tree->children[0]->lexeme] == "int*") { tree->type = intStar; }
					else { cerr << "ERROR: variable not in symbol table for function in factor -> ID derivation " << endl; return; }
				}
				else{ tree->type = integer; } // it is a procedure
			}
		}
		else if(tree->RHS.size() == 2){ //AMP lvalue or STAR factor
			parseFunction(tree->children[1]);
			if(tree->RHS[1] == "lvalue" && tree->children[1]->type == integer) { tree->type = intStar; }
			else if(tree->RHS[1] == "factor" && tree->children[1]->type == intStar) { tree->type = integer;}
			else{ cerr << "ERROR: invalid type in factor -> AMP lvalue or STAR factor derivation" << endl;  return; }
		}
		else if(tree->RHS.size() == 3){ //LPAREN expr RPAREN or ID LPAREN RPAREN
			if(tree->RHS[1] == "expr"){
				parseFunction(tree->children[1]);
				tree->type = tree->children[1]->type;
			}
			else if(tree->RHS[0] == "ID"){ // procedure whose name is ID must have an empty signature
				string procedureName = tree->children[0]->lexeme;
				if(globalSymbolTable[procedureName]->functionParams.size() == 0){ tree->type = integer; } // signature is empty
				else{ cerr << "ERROR: invalid type in factor -> ID LPAREN RPAREN derivation" << endl; return; }
			}
			else{ cerr << "ERROR: invalid type of factor -> LPAREN expr RPAREN or ID LPAREN RPAREN derivation" << endl; return; }
		}
		else if(tree->RHS.size() == 4){ // ID LPAREN arglist RPAREN
			if(tree->RHS[2] == "arglist"){
				parseFunction(tree->children[2]);
				string procedureName = tree->children[0]->lexeme;
				if(isMatch(tree->children[2], procedureName)) { tree->type = integer; }
				else{ cerr << "ERROR: invalid type of factor -> ID LPAREN arglist RPAREN derivation" << endl; return; }
				/*procedure whose name is ID must have a signature whose length is equal to the number of expr strings (separated by COMMA)
				that are derived from arglist. Further the types of these expr strings must exactly match, in order, the types in the procedure's signature.*/
			}
		}
		else if(tree->RHS.size() == 5){ //NEW INT LBRACK expr RBRACK
			parseFunction(tree->children[3]);
			if(tree->children[3]->type == integer) { tree->type = intStar; }
			else { cerr << "ERROR: invalid type of factor -> NEW INT LBRACK expr RBRACK derivation" << endl; return; }
		}
	}
	else if(tree->LHS == "lvalue"){
		if(tree->RHS.size() == 1){ // lvalue → ID, same as the type of that ID
			auto it = globalSymbolTable.find(tree->children[0]->lexeme);  //check if ID is a procedure
			if(it == globalSymbolTable.end()){
				string idName = tree->children[0]->lexeme;
				string type = symbolTable[idName];
				if(type == "int") { tree->type = integer; }
				else if(type == "int*") { tree->type = intStar; }
				else { cerr << "ERROR: invalid type of lvalue -> ID derivation" << endl; return;}
			}
			else { tree->type = integer; }
		}
		else if(tree->RHS.size() == 2){ //lvalue → STAR factor
			parseFunction(tree->children[1]);
			if(tree->RHS[1] == "factor" && tree->children[1]->type == intStar){ tree->type = integer; }
			else{ cerr << "ERROR: invalid type of lvalue -> STAR factor derivation" << endl;  return;}
		}
		else if(tree->RHS.size() == 3){ // LPAREN lvalue RPAREN
			parseFunction(tree->children[1]);
			tree->type = tree->children[1]->type;
		}
	}
	else{ for(auto& i: tree->children) parseFunction(i); }
}

void lookUpTree(Tree* tree){
	if(tree->LHS == "main") {
		if(globalSymbolTable.count("wain") != 0){ cerr << "ERROR: double function declaration" << endl; return; }
		currentProcedure = "wain";
		symbol_table* st = new symbol_table();
		st->functionName = "wain";
		pair<string, string> param1 = parseDcl(tree->children[3]);
		pair<string, string> param2 = parseDcl(tree->children[5]);
		if(param1.first == param2.first){ cerr << "ERROR: double declaration" << endl; return;}
		if(param2.second == "int*"){
			cerr << "ERROR: second parameter of wain can't be int*" << endl;
			delete st;
			return;
		}
		st->add_symbol(param1.first, param1.second);
		st->add_symbol(param2.first, param2.second);
		st->functionParams.push_back(param1);
		st->functionParams.push_back(param2);
		globalSymbolTable["wain"] = st;
		parseFunction(tree->children[8]);
		parseFunction(tree->children[9]);
		parseFunction(tree->children[11]);
		if(tree->children[11]->type != integer) { cerr << "ERROR: it's not returning an integer" << endl; return;}
	}
	else if (tree->LHS == "procedure") {
		currentProcedure = tree->children[1]->lexeme;
		if(globalSymbolTable.count(currentProcedure) != 0){ cerr << "ERROR: double function declaration" << endl; return; }
		addNewSignature(tree);
		parseFunction(tree->children[6]);
		parseFunction(tree->children[7]);
		parseFunction(tree->children[9]);
		if(tree->children[9]->type != integer) { cerr << "ERROR: it's not returning an integer" << endl; return;}
	}
	else { for(auto& i: tree->children) lookUpTree(i); }
}

void lis(int d) { cout << "lis $" << d << endl; }
void word(int i){ cout << ".word " << i << endl; }
void word(string functionName) { cout << ".word " << functionName << endl; }
void storeWord(int t, int i, int s){ cout << "sw $" << t << ", " << i << "($" << s << ")" << endl; }
void loadWord(int t, int i, int s) { cout << "lw $" << t << ", " << i << "($" << s << ")" << endl; }
void sub(int d, int s, int t) { cout << "sub $" << d << ", $" << s << ", $" << t << endl; }
void add(int d, int s, int t) { cout << "add $" << d << ", $" << s << ", $" << t << endl; }
void mult(int s, int t){ cout << "mult $" << s << ", $" << t << endl; }
void divide(int s, int t){ cout << "div $" << s << ", $" << t << endl; }
void mflo(int d){ cout << "mflo $" << d << endl; }
void mfhi(int d){ cout << "mfhi $" << d << endl; }
void jr(int s){ cout << "jr $" << s << endl; }
void jalr(int s){ cout << "jalr $" << s << endl; }
void slt(int d, int s, int t, bool pointer){
	string instruction = pointer ? "sltu $" : "slt $";
	cout << instruction << d << ", $" << s << ", $" << t << endl;
}
void push(int r){ storeWord(r, -4, 30); sub(30, 30, 4); }
void pop(int r){ add(30, 30, 4); loadWord(r, -4, 30); }
void beq(int s, int t, string label){ cout << "beq $" << s << ", $" << t << ", " << label << endl;}
void beq(int s, int t, int i){ cout << "beq $" << s << ", $" << t << ", " << i << endl;}
void bne(int s, int t, int i){ cout << "bne $" << s << ", $" << t << ", " << i << endl;}

int frameSize(string procedure){ return globalSymbolTable[procedure]->symbols.size() * 4; }

int offsetSize(string procedure, string idName){
	if (!globalSymbolTable.count(procedure)) {
		cerr << "ERROR: function is not in global symbol table." << endl;
		return -1;
	}
	else if (!globalSymbolTable[procedure]->symbolIndex.count(idName)) {
		cerr << "ERROR: id is not in offset table." << endl;
		return -1;
	}
	else { return -1 * globalSymbolTable[procedure]->symbolIndex[idName]; }
}

void codeSetUp(){
	cout << ".import print\n";
	cout << ".import init\n";
	cout << ".import new\n";
	cout << ".import delete\n";
	storeWord(4, -4, 30);
	storeWord(5, -8, 30);
	storeWord(11, -12, 30);
	storeWord(1, -16, 30);
	storeWord(2, -20, 30);
	storeWord(29, -24, 30);
	storeWord(6, -28, 30);
	storeWord(10, -32, 30);
	storeWord(7, -36, 30);
	storeWord(8, -40, 30);
	storeWord(9, -44, 30);
	lis(4);
	word(4);
	lis(5);
	word(44);
	lis(11);
	word(1);
	sub(30, 30, 5);
	push(2);
	if(globalSymbolTable["wain"]->functionParams[0].second == "int"){ add(2, 0, 0); } //checks if the first parameter is an int
	push(31);
	lis(8);
	word("init");
	jalr(8);
	pop(31);
	pop(2);
	lis(8);
	word("Fwain");
	jr(8);
}

void wainPrologue(){
	cout << "Fwain:" << endl;
	lis(5);
	word(frameSize("wain"));
	sub(29, 30, 4);
	sub(30, 30, 5);
	storeWord(1, 0, 29);
	storeWord(2, -4, 29);
}

void wainEpilogue(){
	add(30, 29, 4);
	lis(5);
	word(44);
	add(30, 30, 5);
	loadWord(4, -4, 30);
	loadWord(5, -8, 30);
	loadWord(11, -12, 30);
	loadWord(1, -16, 30);
	loadWord(2, -20, 30);
	loadWord(29, -24, 30);
	loadWord(6, -28, 30);
	loadWord(10, -32, 30);
	loadWord(7, -36, 30);
	loadWord(8, -40, 30);
	loadWord(9, -44, 30);
	jr(31);
}

int lvalueOffset(Tree* lvalue, string function_name){ return offsetSize(function_name, lvalue->children[0]->lexeme); }

Tree* lvalueRoot(Tree* node){
	/* lvalue → LPAREN lvalue RPAREN
	*/
	if(node->RHS[0] == "LPAREN") { return lvalueRoot(node->children[1]); }
	else { return node; }
}

void code(Tree* tree);

void codeArgList(Tree* tree, int offset){
	code(tree->children[0]);
	storeWord(3, offset, 30);
	if(tree->RHS.size() == 3) { codeArgList(tree->children[2], offset - 4); }
}

void code(Tree* tree){
	if(tree->LHS == "main"){
		currentProcedure = "wain";
		wainPrologue();
		for(auto &i: tree->children) { code(i); }
		wainEpilogue();
	}
	else if(tree->LHS == "factor"){
		if(tree->RHS.size() == 1 && tree->RHS[0] == "ID" ){
			/*  factor → ID
			*/
			string name = tree->children[0]->lexeme;
			int offset = offsetSize(currentProcedure, name);
			loadWord(3, offset, 29);
		}
		else if(tree->RHS.size() == 3 && tree->RHS[0] == "LPAREN"){
			/* factor → LPAREN expr RPAREN
			*/
			code(tree->children[1]);
		}
		else if(tree->RHS[0] == "NUM"){
			int i = stoi(tree->children[0]->lexeme);
			lis(3);
			word(i);
		}
		else if(tree->RHS[0] == "NULL"){ add(3, 0, 11); }
		else if(tree->RHS[0] == "STAR"){
			/* factor → STAR factor
			*/
			code(tree->children[1]);
			loadWord(3, 0, 3);
		}
		else if(tree->RHS[0] == "AMP"){
			/* factor → AMP lvalue
			*/
			Tree* t = lvalueRoot(tree->children[1]);
			if(t->RHS[0] == "ID"){
				/* lvalue → ID
				*/
				int i = lvalueOffset(t, currentProcedure);
				lis(3);
				word(i);
				add(3, 3, 29);
			}
			else if(t->RHS[0] == "STAR"){
				/* lvalue → STAR factor
				*/
				code(t->children[1]);
			}
		}
		else if(tree->RHS[0] == "NEW"){
			/* factor → NEW INT LBRACK expr RBRACK
			*/
			code(tree->children[3]);
			add(1, 3, 0);
			push(31);
			lis(5);
			word("new");
			jalr(5);
			pop(31);
			bne(3, 0, 1);
			add(3, 11, 0);
		}
		else if(tree->RHS[0] == "ID" && tree->RHS.size() == 3){
			/* factor → ID LPAREN RPAREN
			*/
			string functionName = "F" + tree->children[0]->lexeme;
			push(29);
			push(31);
			lis(9);
			word(functionName);
			jalr(9);
			pop(31);
			pop(29);
		}
		else if(tree->RHS[0] == "ID" && tree->RHS.size() == 4){
			/* factor → ID LPAREN arglist RPAREN
			*/
			string functionName = "F" + tree->children[0]->lexeme;
			push(29);
			push(31);
			codeArgList(tree->children[2], -4);
			lis(9);
			word(functionName);
			jalr(9);
			pop(31);
			pop(29);
		}
	}
	else if(tree->LHS == "expr"){
		/*  expr → expr PLUS term
			expr → expr MINUS term
		*/
		if(tree->RHS.size() == 3){
			code(tree->children[0]);
			push(3);
			code(tree->children[2]);
			pop(6); //value of expr
			if(tree->RHS[1] == "PLUS"){
				if(tree->children[0]->type == 1 && tree->children[2]->type == 0){
					/* type(expr) = int*, type(term) =  int
					*/
					mult(3, 4);
					mflo(3);
				}
				else if(tree->children[0]->type == 0 && tree->children[2]->type == 1){
					/* type(expr) = int, type(term) =  int*
					*/
					mult(6, 4);
					mflo(6);
				}
				add(3, 3, 6);
			}
			else if(tree->RHS[1] == "MINUS"){
				if(tree->children[0]->type == 1 && tree->children[2]->type == 0){
					/* type(expr) = int*, type(term) =  int
					*/
					mult(3, 4);
					mflo(3);
				}
				sub(3, 6, 3);
				if(tree->children[0]->type == 1 && tree->children[2]->type == 1){
					/* type(expr) = int*, type(term) =  int*
					*/
					divide(3, 4);
					mflo(3);
				}
			}
		}
		else if(tree->RHS.size() == 1){ code(tree->children[0]); } //expr → term
	}
	else if(tree->LHS == "term"){
		/*  term →  term STAR factor
			term → term SLASH factor
			term → term PCT factor
		*/
		if(tree->RHS.size() == 3){
			code(tree->children[0]);
			push(3);
			code(tree->children[2]);
			pop(6);
			if(tree->RHS[1] == "STAR"){
				mult(3, 6);
				mflo(3);
			}
			else{
				divide(6, 3);
				if(tree->RHS[1] == "SLASH") mflo(3);
				else mfhi(3);
			}
		}
		else if(tree->RHS.size() == 1){ code(tree->children[0]); } //term → factor
	}
	else if(tree->LHS == "statement"){
		if(tree->RHS[0] == "PRINTLN"){
			/* statement → PRINTLN LPAREN expr RPAREN SEMI
			*/
			code(tree->children[2]);
			push(1);
			push(31);
			add(1, 3, 0);
			lis(10);
			word("print");
			jalr(10);
			pop(31);
			pop(1);
		}
		else if(tree->RHS.size() == 4){
			/* statement → lvalue BECOMES expr SEMI
			*/
			code(tree->children[2]);
			Tree* lvalueNode = lvalueRoot(tree->children[0]);
			if(lvalueNode->RHS[0] == "ID"){
				/* lvalue → ID
				 */
				int i = lvalueOffset(lvalueNode, currentProcedure);
				storeWord(3, i, 29);
			}
			else{
				/* lvalue → STAR factor
				 */
				push(3);
				code(lvalueNode->children[1]);
				pop(6);
				storeWord(6, 0, 3);
			}
		}
		else if(tree->RHS.size() == 7){
			int labelCount = loopCount++;
			/* statement → WHILE LPAREN test RPAREN LBRACE statements RBRACE
			*/
			cout << "loopStart" << labelCount << ": ";
			code(tree->children[2]);
			string loopEndLabel = "loopEnd" + to_string(labelCount);
			beq(3, 0, loopEndLabel);
			code(tree->children[5]);
			string loopStartLabel = "loopStart" + to_string(labelCount);
			beq(0, 0, loopStartLabel);
			cout << "loopEnd" << labelCount << ":" << endl;
		}
		else if(tree->RHS.size() == 11){
			int label_number = ifCount++;
			code(tree->children[2]);
			string elseLabel = "else" + to_string(label_number);
			beq(3, 0, elseLabel);
			code(tree->children[5]);
			string endifLabel = "endif" + to_string(label_number);
			beq(0, 0, endifLabel);
			cout << "else" << label_number << ": ";
			code(tree->children[9]);
			cout << "endif" << label_number << ":" << endl;
		}
		else if(tree->RHS.size() == 5){
			/* statement → DELETE LBRACK RBRACK expr SEMI
			*/
			int deleteNumber = deleteCount++;
			code(tree->children[3]);
			string skipDeleteLabel = "skipDelete" + to_string(deleteNumber);
			beq(3, 11, skipDeleteLabel);
			add(1, 3, 0);
			push(31);
			lis(5);
			word("delete");
			jalr(5);
			pop(31);
			cout << skipDeleteLabel << ":" << endl;
		}
	}
	else if(tree->LHS == "dcls"){
		if(tree->RHS.size() == 0) return; // dcls →
		else if(tree->RHS.size() == 5){
			/*  dcls → dcls dcl BECOMES NUM SEMI
				dcls → dcls dcl BECOMES NULL SEMI
			*/
			code(tree->children[0]);
			int i = tree->RHS[3] == "NUM" ? stoi(tree->children[3]->lexeme) : 1;
			lis(3);
			word(i);
			int offset = offsetSize(currentProcedure, tree->children[1]->children[1]->lexeme);
			storeWord(3, offset, 29);
		}
	}
	else if(tree->LHS == "test" && tree->RHS.size() == 3){
		/*  test → expr LT expr
			test → expr GT expr
			test → expr NE expr
			test → expr EQ expr
			test → expr LE expr
			test → expr GE expr
		*/
		bool pointer = false;
		if(tree->children[0]->type == intStar){ pointer = true; }
		code(tree->children[0]);
		push(3);
		code(tree->children[2]);
		pop(6); //contains expr return value
		if(tree->RHS[1] == "LT"){
			slt(7, 6, 3, pointer);
			add(3, 7, 0);
		}
		else if(tree->RHS[1] == "GT"){
			slt(7, 3, 6, pointer);
			add(3, 7, 0);
		}
		else if(tree->RHS[1] == "NE"){
			slt(5, 6, 3, pointer);
			slt(7, 3, 6, pointer);
			add(3, 5, 7);
		}
		else if(tree->RHS[1] == "EQ"){
			slt(5, 6, 3, pointer);
			slt(7, 3, 6, pointer);
			add(3, 5, 7);
			sub(3, 11, 3);
		}
		else if(tree->RHS[1] == "LE"){ //same as !GT
			slt(7, 3, 6, pointer);
			add(3, 7, 0);
			sub(3, 11, 3);
		}
		else if(tree->RHS[1] == "GE"){ //same as !LT
			slt(7, 6, 3, pointer);
			add(3, 7, 0);
			sub(3, 11, 3);
		}
	}
	else if(tree->LHS == "procedure"){
		/* procedure → INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE
		*/
		currentProcedure = tree->children[1]->lexeme;
		cout << "F" << currentProcedure << ": ";
		sub(29, 30, 4);
		lis(5);
		word(frameSize(currentProcedure));
		sub(30, 30, 5);
		code(tree->children[6]);
		code(tree->children[7]);
		code(tree->children[9]);
		add(30, 29, 4);
		jr(31);
	}
	else if(tree->LHS == "params"){ return; }
	else if(tree->LHS == "paramlist"){ return; }
	else { for(auto &i: tree->children) { code(i); } }
}

int main(){
	Tree* parseTree = makeTree();
	lookUpTree(parseTree);
	//printSymbolTable();
	codeSetUp();
	code(parseTree);
	parseTreeDestructor(parseTree);
	destructSymbolTable();
}