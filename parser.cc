#include <vector> 
#include <iostream>
#include <string>
#include <sstream>
#include <unordered_map>
#include <fstream>
#include <algorithm>

using namespace std;

struct ProductionRule{
    string LHS; 
    string RHS;
    int lengthofRHS; 
    ProductionRule(string s) {
        istringstream ss(s);
        ss >> LHS;
        string temp;
        lengthofRHS = 0;
        while(ss >> temp) {
            RHS.append(temp);
            RHS.append(" ");
            lengthofRHS += 1;
        }
        if (lengthofRHS > 0) {
            RHS.pop_back();
        }
    }
};

struct State{
    int stateNum; 
    unordered_map<string, int> transitions; 
    unordered_map<string, int> reduction; //<symbol, pRule number> 
    State(int i): stateNum{i} {}
};

struct Tree{
    string symbolName;
    string lexemeName;
    vector<Tree> children; 
    Tree(string name, string LName): symbolName{name}, lexemeName{LName} {}
};

vector<ProductionRule> pRules; 

vector<State> states; 

vector<int> pushAndPopStates;

vector<Tree> pushAndPopSymbol;

vector<string> inputs;

vector<string> lexemes; 

int transitions(int stateNum, string symbol){
    if(states.at(stateNum).transitions.count(symbol))
        return states.at(stateNum).transitions.at(symbol);
    else{
        return -1;
    }
}

bool reduce(string symbol, int stateNum){
    return (states.at(stateNum).reduction.count(symbol) > 0);
}

ProductionRule& reduceRule(int stateNum, string followSet){
    int index = states[stateNum].reduction[followSet];
    return pRules[index];
}

void readFile(bool debug){
    ifstream ifs("input.txt",ifstream::in);
    string s; 
    int number;
    ifs >> number;
    for (int i = 0; i < number; i++){
        ifs >> s;
    }

    if (debug) cerr << "readFile: terminals" << endl;
    
    ifs >> number;
    for (int i = 0; i <= number; i++){
        ifs >> s;
    }

    if (debug) cerr << "readFile: nonterminals" << endl;
    
    ifs >> number;
    getline(ifs, s);
    for(int i = 0; i < number; i++){
        getline(ifs, s);
        pRules.push_back(ProductionRule(s));
    }

    if (debug) cerr << "readFile: pRules" << endl;

    ifs >> number;

   if (debug) cerr << number << endl;
    for(int i = 0; i < number; i++){
        states.push_back(State(i));
    }
    if (debug) cerr << "readFile: states" << endl;
    ifs >> number;
    getline(ifs, s); 
    if (debug) cerr << number  << endl;
    
    for(int i = 0; i < number; i++){
        int stateNum;
        string s; 
        string input;
        string type;
        int productionRuleNum;
        getline(ifs, s);

        if (debug) cerr << s << endl;

        istringstream ss(s);
        ss >> stateNum; 
        ss >> input; 
        ss >> type; 
        ss >> productionRuleNum; 
        if(type == "reduce"){
            states.at(stateNum).reduction[input] = productionRuleNum;
        }
        else{
            states.at(stateNum).transitions[input] = productionRuleNum;
        }
    }
    if (debug) cerr << "readFile: reductions" << endl;
    ifs.close();
}

void readLexemes(){
    string s;
    string type;
    string lexeme; 
    inputs.push_back("BOF");
    lexemes.push_back("BOF");
    while(getline(cin, s)){
        istringstream ss(s);
        ss >> type; 
        inputs.push_back(type);
        ss >> lexeme;
        lexemes.push_back(lexeme);
    }
    inputs.push_back("EOF");
    lexemes.push_back("EOF");
    reverse(lexemes.begin(), lexemes.end());
}

Tree parseInput(vector<string>& inputs){

    pushAndPopStates.push_back(0);
    pushAndPopSymbol.push_back(Tree(inputs[0], lexemes.back()));
    
    //cout << inputs[0] << " " << lexemes.back() << endl;   
    lexemes.pop_back();
    
    pushAndPopStates.push_back(transitions(0, inputs[0]));

    for(int i = 1; i < inputs.size(); i++){
        string symbol = inputs[i]; 
        while(reduce(symbol, pushAndPopStates.back())){
            ProductionRule pRule = reduceRule(pushAndPopStates.back(), symbol);
            int popNumber = pRule.lengthofRHS; 
            Tree tree = Tree(pRule.LHS, pRule.RHS);
            for(int i = 0; i < popNumber; i++){
                pushAndPopStates.pop_back();
                tree.children.push_back(pushAndPopSymbol.back());
                pushAndPopSymbol.pop_back();
            }

            reverse(tree.children.begin(), tree.children.end());

            pushAndPopSymbol.push_back(tree);
            pushAndPopStates.push_back(transitions(pushAndPopStates.back(), pRule.LHS));

            // cout << pRule.LHS;
            // if(pRule.lengthofRHS > 0) cout << " " << pRule.RHS <<  endl;
            // else cout << endl;
        }
        pushAndPopSymbol.push_back(Tree(symbol, lexemes.back()));
        
        //cout << symbol << " " << lexemes.back() << endl;
        lexemes.pop_back(); 

        pushAndPopStates.push_back(transitions(pushAndPopStates.back(), symbol));

        if(pushAndPopStates.back() == -1){
            cerr << "ERROR at " << i << endl;
            return Tree("", "");
        }
    }
    // ProductionRule lastElement = pRules[0];
    // cout << lastElement.LHS;
    // if(lastElement.lengthofRHS > 0) cout << " " << lastElement.RHS <<  endl;
    //else cout << endl; 
    Tree startTree = Tree("start", "BOF procedures EOF");
    startTree.children = pushAndPopSymbol;
    // for(auto &i : startTree.children){
    //     cout << i.lexemeName << " " << i.symbolName << endl;
    // }
    return startTree;
}

void printTree(Tree& tree){
    cout << tree.symbolName;
    if(tree.lexemeName.empty()) cout << endl;
    else{
        cout << " " << tree.lexemeName << endl;
    }
    for(auto &i : tree.children){
        printTree(i);
    }
}

int main(){
    readFile(false);
    readLexemes();
    Tree tree = parseInput(inputs);
    printTree(tree);
}