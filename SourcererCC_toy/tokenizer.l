%{
	#include <iostream>
	#include <unordered_map>
	std::unordered_map<std::string, size_t> token_count;
	size_t total_tokens, unique_tokens;
%}

seperator [;\.\[\]\(\)~!\+\-&\*\/\%\<\>\^\|\?\{\}=#,\\:$\"\' \n\t]
comment \/\/[^\n]*|"/*"([^\*]|(\*)*[^\*/])*(\*)*"*/"

%%

{comment} 	
{seperator} 	
[^;\.\[\]\(\)~!\+\-&\*\/\%\<\>\^\|\?\{\}=#,\\:$\"\' \n\t]+ { std::string token(yytext); ++token_count[token]; ++total_tokens; }

%%

int main()
{
	yylex();
	unique_tokens = token_count.size();
	std::cout << "Total tokens: " << total_tokens << ", Unique tokens: "
		<< unique_tokens << std::endl;
	for (const auto &tc : token_count) 
		std::cout << tc.first << ' ' << tc.second << std::endl; 
	yywrap();
	return 0;
}

int yywrap()
{
	return 1;
}

