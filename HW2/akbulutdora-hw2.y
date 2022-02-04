%{
#include <stdio.h>
void yyerror (const char *msg){
return; }
%}

%token tMAIL tENDMAIL tSCHEDULE tENDSCH tSEND tSET tTO tFROM tAT tCOMMA tCOLON tLPR tRPR tLBR tRBR tIDENT tSTRING tADDRESS tDATE tTIME

/* %start html /* start of the program */

%%

program	: components
	| 
;

components	: mail_b components
		| set_stmt components
		| mail_b
		| set_stmt
;

set_stmt	: tSET tIDENT tLPR tSTRING tRPR
;

mail_b	: tMAIL tFROM tADDRESS tCOLON statement_list tENDMAIL
	| tMAIL tFROM tADDRESS tCOLON tENDMAIL
;

statement_list	: set_stmt
		| send_stmt
		| schedule_stmt
		| set_stmt statement_list
		| send_stmt statement_list
		| schedule_stmt statement_list
;

send_stmt	: tSEND tLBR tIDENT tRBR tTO recip_list
		| tSEND tLBR tSTRING tRBR tTO recip_list
;

recip_list	: tLBR recipients tRBR
;

recipients	: recipient tCOMMA recipients
		| recipient
;

recipient	: tLPR tADDRESS tRPR
		| tLPR tIDENT tCOMMA tADDRESS tRPR
		| tLPR tSTRING tCOMMA tADDRESS tRPR
;

schedule_stmt	: tSCHEDULE tAT tLBR tDATE tCOMMA tTIME tRBR tCOLON send_list tENDSCH
;

send_list	: send_stmt send_list
		| send_stmt
;
%%

int main ()
{
	if (yyparse())
	{
		// parse error
		printf("ERROR\n");
		return 1;
	}
	else
	{
		// successful parsing
		printf("OK\n");
		return 0;
	}
}

