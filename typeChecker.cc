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
	string lexeme;
	string kind;
    enum type type; 
};

typedef struct symbol_table{
	string func_name;
	vector<pair<string, string>> func_parameters;
	map<string, string> symbols;
} symbol_table;

map<string, symbol_table*> global_st; //function name and symbol table for the function 

string currentProcedure;

void parseTreeDestructor(Tree* tree){
	for(auto & i: tree->children){
		parseTreeDestructor(i);
	}
	delete tree; 
}

bool checkTerminal(string s) { // returns true if its a terminal
	for(int i = 0; i < terminalSymbols.size(); i++){
		if(s == terminalSymbols[i]) return true;
	}
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
		while(iss >> word){
			(t->RHS).push_back(word);
		}
	}
	return t;
}

Tree* makeTree(){
	Tree* tree = newChild();
	if(!checkTerminal(tree->LHS)){
		for(int i = 0; i < (tree->RHS).size(); i++){
			tree->children.push_back(makeTree());
		}
	}
	return tree;
}

void print(Tree *tree){
	if(!tree) return;
	for(auto t: tree->children){
		print(t);
	}
	if(tree->kind != ""){
		cout << tree->kind << " " << tree->lexeme << endl;
	}
	else{
		cout << tree->LHS << " ";
		for(int i = 0; i < (tree->RHS).size(); i++){
			cout << (tree->RHS)[i] << " ";
		}
		cout << endl;
	}
}

void destruct_symbol_table(void) {
	for (auto & p: global_st) {
		delete p.second;
	}
}

void printSymbolTable(){
	for(auto & p: global_st) {
		cerr << p.first;
		for(auto& i: p.second->func_parameters){
			cerr << " " << i.second;
		}
		cerr << endl;
		for (auto & id_type: p.second->symbols) {
			cerr << id_type.first << " " << id_type.second << endl;
		}
		cerr << endl;
	}
}

bool checkDuplicateSignature(vector<pair<string, string>> signature){
    for (int i = 0; i < signature.size(); i++){
		for (int j = 0; j < signature.size(); j++){
			if (j != i && signature[i].first == signature[j].first) {
				return true;
			}
		}
	}
    return false;
}

pair<string, string> parse_dcl(Tree * tree) {
	string type = tree->children[0]->children.size() == 1 ? "int" : "int*";
	string id = tree->children[1]->lexeme;
	return pair<string, string>(id, type);
}

void getSignature(Tree* tree, vector<pair<string, string>>& sig) {
	sig.push_back(parse_dcl(tree->children[0]));
	if(tree->children.size() == 3) getSignature(tree->children[2], sig);
}

void addNewSignature(Tree* tree) {
	vector<pair<string, string>> sig;
	if(tree->children[3]->children.size()) getSignature(tree->children[3]->children[0], sig);
    if(checkDuplicateSignature(sig)){
        cerr << "ERROR: duplicate signature" << endl;
        return;
    }
	symbol_table* st = new symbol_table();
	global_st[currentProcedure] = st;
	global_st[currentProcedure]->func_name = currentProcedure;
	global_st[currentProcedure]->func_parameters = sig;
	for (auto& p: sig) global_st[currentProcedure]->symbols.emplace(p.first, p.second);
}

void updateTable(Tree* tree) {
	if(tree->LHS == "dcl") {
		pair<string, string> p = parse_dcl(tree);
		if (!global_st[currentProcedure]->symbols.emplace(p.first, p.second).second){ 
            cerr << "ERROR: double decleration of " << p.first << "with type " << p.second << endl;
            return;
        }
	}
}

int countExpr(Tree* tree){
    int count = 0;
    if(tree->RHS.size() == 1) count ++;
    else{
        count++;
        countExpr(tree->children[2]);
    }
    return count;
}

void parseFunction(Tree* tree);

vector<string> getArgListType(Tree* tree){
    vector<string> exprType; 
    if(tree->RHS.size() == 1){
        parseFunction(tree->children[0]);
        if(tree->children[0]->type == integer) exprType.push_back("int");
        else exprType.push_back("int*");
    }
    else{
        getArgListType(tree->children[2]); 
    }
    return exprType;
}

bool isMatch(Tree* tree, string functionName){
    int number_of_expr_strings = countExpr(tree);
    vector<pair<string, string>> parameterList = global_st[functionName]->func_parameters;
    if(number_of_expr_strings == parameterList.size()){
        vector<string> exprTypes = getArgListType(tree);
        for(int i = 0; i < parameterList.size(); i++){
            string t1 = parameterList[i].second;
            string t2 = exprTypes[i];
            if(t1 != t2) return false;
            else continue;
        }                        
        return true;
    }
    return false;
}

void parseFunction(Tree* tree){
	map<string, string>& symbolTable = global_st[currentProcedure]->symbols;
	if(tree->LHS == "dcl") updateTable(tree);
	else if(tree->LHS == "factor" && tree->children.size() >= 2 && tree->RHS[0] == "ID" 
            && tree->children[1]->lexeme == "(" && (global_st.count(tree->children[0]->lexeme) == 0 || symbolTable.count(tree->children[0]->lexeme))){
		cerr << "ERROR: double declaration with factor or lvalue as LHS" << endl;
        return;
	}
    else if(tree->LHS == "factor" && tree->children.size() >= 2 && tree->RHS[0] == "ID" && tree->children[1]->lexeme == "(" 
                    && (global_st.count(tree->children[0]->lexeme) == 0 || symbolTable.count(tree->children[0]->lexeme))) {
        cerr << "ERROR: can't call this function" << endl;
        return;
    }
    else if(((tree->RHS).size() == 1) && ((tree->LHS == "factor") || (tree->LHS == "lvalue")) && (tree->RHS[0] == "ID") && (symbolTable.count(tree->children[0]->lexeme) == 0)) {                 
		//cerr << tree->children[0]->lexeme << endl; 
        cerr << "ERROR: undefined variable" << endl; 
        return;
	}
    else if(tree->LHS == "dcls" && tree->RHS.size() == 5){ // dcls → dcls dcl BECOMES NUM SEMI OR dcls → dcls dcl BECOMES NULL SEMI
        if(tree->RHS[3] == "NUM"){
             /*cerr << tree->children[1]->children[1]->lexeme << " ";
            cerr << tree->children[0]->RHS.size() << " ";
            cerr << tree->children[1]->children[0]->RHS.size() << endl;*/
            if(tree->children[1]->children[0]->RHS.size() != 1){ 
                cerr << "ERROR: derived dcl doesn't derive a sequence containing a type that derives INT" << endl; return; 
            }
            else{ 
                parseFunction(tree->children[1]); 
                parseFunction(tree->children[0]); 
            }
        }
        else if(tree->RHS[3] == "NULL"){
            /*cerr << tree->children[1]->children[1]->lexeme << " ";
            cerr << tree->children[0]->RHS.size() << " ";
            cerr << tree->children[1]->children[0]->RHS.size() << endl;*/
            if(/*(tree->children[0]->RHS.size() == 0) &&*/ (tree->children[1]->children[0]->RHS.size() != 2)){ 
                cerr << "ERROR: derived dcl doesn't derive a sequence containing a type that derives INT*" << endl; return; 
            }
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
            if(tree->children[0]->type == integer && tree->children[2]->type == integer) tree->type = integer;
            else { cerr << "ERROR: invalid input term -> term STAR/SLASH/PCT factore derivation" << endl; return;}
        }
    }
    else if(tree->LHS == "factor"){
        if(tree->RHS.size() == 1){ //ID   NUM   NULL 
            if(tree->RHS[0] == "NUM") tree->type = integer;
            else if(tree->RHS[0] == "NULL") tree->type = intStar; 
            else if(tree->RHS[0] == "ID"){
                auto it = global_st.find(tree->children[0]->lexeme);  //check if ID is a procedure 
                if(it == global_st.end()){ //id is not a procedure 
                    if(symbolTable[tree->children[0]->lexeme] == "int") tree->type = integer; 
                    else if(symbolTable[tree->children[0]->lexeme] == "int*") tree->type = intStar;
                    else { cerr << "ERROR: variable not in symbol table for function in factor -> ID derivation " << endl; return;}
                }
                else{ // it is a procedure 
                    tree->type = integer; 
                }
            }
        }
        else if(tree->RHS.size() == 2){ //AMP lvalue or STAR factor
            parseFunction(tree->children[1]);   
            if(tree->RHS[1] == "lvalue" && tree->children[1]->type == integer) tree->type = intStar; 
            else if(tree->RHS[1] == "factor" && tree->children[1]->type == intStar) tree->type = integer; 
            else{ cerr << "ERROR: invalid type in factor -> AMP lvalue or STAR factor derivation" << endl;  return; }
        }
        else if(tree->RHS.size() == 3){ //LPAREN expr RPAREN or ID LPAREN RPAREN
            if(tree->RHS[1] == "expr"){
                parseFunction(tree->children[1]); 
                tree->type = tree->children[1]->type; 
            }
            else if(tree->RHS[0] == "ID"){ // procedure whose name is ID must have an empty signature
                string procedureName = tree->children[0]->lexeme; 
                if(global_st[procedureName]->func_parameters.size() == 0){ // signature is empty 
                    tree->type = integer;
                }
                else{ cerr << "ERROR: invalid type in factor -> ID LPAREN RPAREN derivation" << endl; return; }
            }
            else{ cerr << "ERROR: invalid type of factor -> LPAREN expr RPAREN or ID LPAREN RPAREN derivation" << endl; return; }
        }
        else if(tree->RHS.size() == 4){ // ID LPAREN arglist RPAREN
            if(tree->RHS[2] == "arglist"){
                parseFunction(tree->children[2]);
                string procedureName = tree->children[0]->lexeme; 
                if(isMatch(tree->children[2], procedureName)) tree->type = integer;
                else{ cerr << "ERROR: invalid type of factor -> ID LPAREN arglist RPAREN derivation" << endl; return; }
                /*procedure whose name is ID must have a signature whose length is equal to the number of expr strings (separated by COMMA) 
                that are derived from arglist. Further the types of these expr strings must exactly match, in order, the types in the procedure's signature.*/ 
            }
        }
        else if(tree->RHS.size() == 5){ //NEW INT LBRACK expr RBRACK
            parseFunction(tree->children[3]);
            if(tree->children[3]->type == integer) tree->type = intStar;
            else { cerr << "ERROR: invalid type of factor -> NEW INT LBRACK expr RBRACK derivation" << endl; return; }
        }
    }
    else if(tree->LHS == "lvalue"){
        if(tree->RHS.size() == 1){ // lvalue → ID, same as the type of that ID
            auto it = global_st.find(tree->children[0]->lexeme);  //check if ID is a procedure
            if(it == global_st.end()){
                string idName = tree->children[0]->lexeme;
                string type = symbolTable[idName]; 
                if(type == "int") tree->type = integer;
                else if(type == "int*") tree->type = intStar;
                else { cerr << "ERROR: invalid type of lvalue -> ID derivation" << endl; return;}
            }
            else tree->type = integer;
        }
        else if(tree->RHS.size() == 2){ //lvalue → STAR factor
            parseFunction(tree->children[1]);   
            if(tree->RHS[1] == "factor" && tree->children[1]->type == intStar){
                tree->type = integer; 
            }
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
		if(global_st.count("wain") != 0){ cerr << "ERROR: double function declaration" << endl; return; }
        //tree->type = integer;
		currentProcedure = "wain";
		symbol_table* st = new symbol_table();
		st->func_name = "wain";
		pair<string, string> param1 = parse_dcl(tree->children[3]);
		pair<string, string> param2 = parse_dcl(tree->children[5]);
        if(param1.first == param2.first){ cerr << "ERROR: double declaration" << endl; return;}
        if(param2.second == "int*"){ 
            cerr << "ERROR: second parameter of wain can't be int*" << endl; 
            delete st;
            return; 
        }
		st->symbols.emplace(param1.first, param1.second);
		st->symbols.emplace(param2.first, param2.second);
		st->func_parameters.push_back(param1);
		st->func_parameters.push_back(param2);
		global_st["wain"] = st;
		parseFunction(tree->children[8]);
		parseFunction(tree->children[9]);
		parseFunction(tree->children[11]);
        if(tree->children[11]->type != integer) { cerr << "ERROR: it's not returning an integer" << endl; return;}
	}
	else if (tree->LHS == "procedure") {
		currentProcedure = tree->children[1]->lexeme;
		if(global_st.count(currentProcedure) != 0){ cerr << "ERROR: double function declaration" << endl; return; }
		addNewSignature(tree);
		parseFunction(tree->children[6]);
		parseFunction(tree->children[7]);
		parseFunction(tree->children[9]);
        if(tree->children[9]->type != integer) { cerr << "ERROR: it's not returning an integer" << endl; return;}
	}
	else {
		for(auto& i: tree->children) lookUpTree(i);
	}
}

int main(){
	Tree* parseTree = makeTree();
	lookUpTree(parseTree);
	printSymbolTable();
	parseTreeDestructor(parseTree);
	destruct_symbol_table();
}