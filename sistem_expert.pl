:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(file_systems)).

:- op(900,fy,not).
:- dynamic fapt/3.

:- dynamic interogat/1.
:- dynamic scop/1.
:- dynamic interogabil/3.
:- dynamic regula/3.
:- dynamic intrebare_curenta/3.

:- dynamic used_language/1.

:- dynamic calendar/1.

%---------------------------------------------------------------------------------------------------
/* This dynamic predicate designates the language that is currently in use. Its default value is
defined here. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% used_language(+LanguageID)
%---------------------------------------------------------------------------------------------------
used_language(ro).
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate designates the languages that are known by the expert system. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% known_language(?LanguageID).
%---------------------------------------------------------------------------------------------------
known_language(en).
known_language(ro).
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to obtain the text for a certain action in the language that is currently
in use. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% prompter(?LanguageID, ?ActionID, ?Text)
%---------------------------------------------------------------------------------------------------
prompter(en, ask_for_rules_file_name, 'Enter the name of the file that contains the goal, rules and questions: ').
prompter(ro, ask_for_rules_file_name, 'Introduceti numele fisierului care contine scopul, regulile si intrebarile: ').

prompter(en, ask_for_solution_info_file_name, 'Enter the name of the file that contains the information for the solutions: ').
prompter(ro, ask_for_solution_info_file_name, 'Introduceti numele fisierului care contine informatiile despre solutii: ').

prompter(en, file_does_not_exist, 'Could not open the specified file. Please check if you entered the correct name!').
prompter(ro, file_does_not_exist, 'Fisierul specificat nu a putut fi deschis. Verificati daca ati introdus numele corect!').

prompter(en, display_facts, 'The facts that exist in the knowledge base:').
prompter(ro, display_facts, 'Faptele prezente in baza de cunostinte:').

prompter(en, display_facts_header, '(Attribute, Value) - Certainty factor').
prompter(ro, display_facts_header, '(Atribut, Valoare) - Factor de certitudine').

prompter(en, display_fact, '(~p, ~p) - Certainty factor = ~p').
prompter(ro, display_fact, '(~p, ~p) - Factor de certitudine = ~p').

prompter(en, display_goal_result_select, 'Do you wish to display the details for the results? ( yes / no )').
prompter(ro, display_goal_result_select, 'Doriti o afisare detaliata a rezultatelor? ( da / nu )').

prompter(en, yes_no_question_wrong_answer, 'Please answer yes or no! Any other answer is invalid!').
prompter(ro, yes_no_question_wrong_answer, 'Raspundeti prin da sau nu! Orice alt raspuns este invalid!').

prompter(en, detail_goal_3, 'You can stil participate at this conference! Date: ~p/~p/~p').
prompter(ro, detail_goal_3, 'Mai puteti participa la aceasta conferinta! Data: ~p/~p/~p').
%---------------------------------------------------------------------------------------------------

not(P) :- P, !, fail.
not(_).

scrie_lista([]) :- nl.
scrie_lista([H|T]) :-
	write(H), tab(1),
	scrie_lista(T).

%---------------------------------------------------------------------------------------------------
/* This predicate is used to display a list of all the facts in the knowledge base. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% displayFactsFromKnowledgeBase
%---------------------------------------------------------------------------------------------------
displayFactsFromKnowledgeBase :-
	used_language(Lang),
	prompter(Lang, display_facts, FactsPrompt),
	write(FactsPrompt), nl, nl,
	prompter(Lang, display_facts_header, FactsHeaderPrompt),
	write(FactsHeaderPrompt), nl, nl,
	displayFacts, nl.
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to get and display each fact from the knowledge base. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% displayFacts
%---------------------------------------------------------------------------------------------------
displayFacts :-
	fapt(av(Atr, Val), FC, _),
	FC1 is integer(FC),
	used_language(Lang),
	prompter(Lang, display_fact, FactPrompt),
	format(FactPrompt, [Atr, Val, FC1]), 	nl,
	fail.
displayFacts.
%---------------------------------------------------------------------------------------------------

% lista_float_int(+ListaFloat, -ListaIn)
lista_float_int([],[]).
lista_float_int([Regula|Reguli],[Regula1|Reguli1]) :-
	(Regula \== utiliz,
	Regula1 is integer(Regula);
	Regula ==utiliz, Regula1=Regula),
	lista_float_int(Reguli,Reguli1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate retracts all dynamic predicates used to store the knowledge base for the expert system.
resetKnowledgeBase :-
	retractall(interogat(_)),
	retractall(fapt(_, _, _)),
	retractall(scop(_)),
	retractall(interogabil(_, _, _)),
	retractall(regula(_, _, _)),
	retractall(solution_info(_, _, _, _)).

un_pas(Rasp,OptiuniUrm,MesajUrm) :-
	scop(Atr),
	(
		Rasp \== null,
		intreaba_acum(Rasp)
		;
		true
	),
	determina1(Atr,OptiuniUrm,MesajUrm),
	afiseaza_scop(Atr).

intreaba_acum(Rasp) :-
	intrebare_curenta(Atr,OptiuniV,MesajV),
	interogheaza1(Rasp,Atr,MesajV,OptiuniV,Istorie),
	nl,
	asserta( interogat(av(Atr,_)) ).

interogheaza1(X, Atr, Mesaj, [da, nu], Istorie) :-
	!,
	de_la_utiliz1(X, Istorie, [da,nu]),
	det_val_fc(X, Val, FC),
	asserta( fapt(av(Atr, Val), FC, [utiliz]) ).

interogheaza1(VLista,Atr,Mesaj,Optiuni,Istorie) :-
	de_la_utiliz1(VLista,Optiuni,Istorie),
	assert_fapt(Atr,VLista).


%de_la_utiliz1(+Rasp,?Istorie,+Lista_opt)
de_la_utiliz1(X,Istorie,Lista_opt) :-
	proceseaza_raspuns([X],Istorie,Lista_opt).

determina1(Atr,OptiuniUrm,MesajUrm) :-
	realizare_scop1(av(Atr, _), _, [scop(Atr)], OptiuniUrm, MesajUrm),
	!.
determina1(_,_,_).

realizare_scop1(not Scop, Not_FC, Istorie, OptiuniUrm, MesajUrm) :-
	realizare_scop1(Scop, FC, Istorie, OptiuniUrm, MesajUrm),
	Not_FC is - FC,
	!.
realizare_scop1(Scop, FC, _, _, _) :-
	fapt(Scop, FC, _),
	!.
realizare_scop1(Scop, FC, Istorie, OptiuniUrm, MesajUrm) :-
	pot_interoga1(Scop, Istorie, OptiuniUrm, MesajUrm),
	!.

%realizare_scop1(Scop,FC,Istorie,OptiuniUrm,MesajUrm).

realizare_scop1(Scop,FC_curent,Istorie,OptiuniUrm,MesajUrm) :-
fg1(Scop,FC_curent,Istorie,OptiuniUrm,MesajUrm).


pot_interoga1(av(Atr,_),Istorie, Optiuni, Mesaj) :-
not interogat(av(Atr,_)),
interogabil(Atr,Optiuni,Mesaj),
retractall(intrebare_curenta(_,_,_)),
assert(intrebare_curenta(Atr, Optiuni,Mesaj)), !.


pornire1 :-
	retractall(interogat(_)),
	retractall(fapt(_,_,_)),
	retractall(intrebare_curenta(_,_,_)),
	retractall(scop(_)),
	retractall(interogabil(_)),
	retractall(regula(_,_,_)),
	incarca('sist_expert.txt').


fg1(Scop,FC_curent,Istorie,OptiuniUrm,MesajUrm) :-
	regula(N, premise(Lista), concluzie(Scop,FC)),
	demonstreaza1(N,Lista,FC_premise,Istorie,OptiuniUrm,MesajUrm),
	(
		nonvar(FC),
		nonvar(FC_premise),
		ajusteaza(FC, FC_premise, FC_nou),
		actualizeaza(Scop, FC_nou, FC_curent, N),
		FC_curent == 100
		;
		true
	),
	!.
fg1(Scop,FC,_,_,_) :- fapt(Scop,FC,_).

demonstreaza1(N, ListaPremise, Val_finala, Istorie, OptiuniUrm, MesajUrm) :-
	dem1(ListaPremise, 100, Val_finala, [N|Istorie], OptiuniUrm, MesajUrm),
	!.

dem1([], Val_finala, Val_finala, _, _, _).
dem1([H|T], Val_actuala, Val_finala, Istorie, OptiuniUrm, MesajUrm) :-
	realizare_scop1(H, FC, Istorie, OptiuniUrm, MesajUrm),
	(
		nonvar(FC),
		Val_interm is min(Val_actuala, FC),
		Val_interm >= 20,
		dem1(T, Val_interm, Val_finala, Istorie, OptiuniUrm, MesajUrm)
		;
		true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pornire :-
	retractall(interogat(_)),
	retractall(fapt(_, _, _)),
	retractall(intrebare_curenta(_, _, _)),
	repeat,
	write('Introduceti una din urmatoarele optiuni: '),
	nl,nl,
	write(' (Incarca Consulta Reinitiaza  Afisare_fapte  Cum Calendar   Iesire) '),
	nl,nl,write('|: '),
	citeste_linie([H|T]),
	executa([H|T]), H == iesire.

executa([incarca]) :-
	incarca, !, nl,
	write('Fisierul dorit a fost incarcat'), nl.
executa([consulta]) :-
	scopuri_princ,
	!.
executa([reinitiaza]) :-
	retractall(interogat(_)),
	retractall(fapt(_, _, _)),
	!.
executa([afisare_fapte]) :-
	displayFactsFromKnowledgeBase,
	!.
executa([cum|L]) :-
	cum(L),
	!.
	
executa([calendar]) :-
	displayCalendar,
	!.
executa([iesire]) :- !.
executa([_|_]) :-
	write('Comanda incorecta! '), nl.

%---------------------------------------------------------------------------------------------------
/* This predicate is used to ask the user which is the preffered display mode for the solutions. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% askWhichDisplayModeShouldBeUsed(-DisplayMode)
%---------------------------------------------------------------------------------------------------
askWhichDisplayModeShouldBeUsed(DisplayMode) :-
	used_language(Lang),
	prompter(Lang, display_goal_result_select, DetailsPrompt),
	write(DetailsPrompt), nl,
	repeat,
		citeste_linie(TokenList),
		solutionDisplayMode(DisplayMode, TokenList, []),
		(
			DisplayMode \== invalid,
			!
			;
			prompter(Lang, yes_no_question_wrong_answer, WrongAnsPrompt),
			write(WrongAnsPrompt), nl,
			fail
		).
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to extract the display mode for the solutions from the given token list. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% solutionDisplayMode(-SolutionDisplayMode, +TokenList, [])
%---------------------------------------------------------------------------------------------------
solutionDisplayMode(detail) -->
	[da],
	!.
solutionDisplayMode(summary) -->
	[nu],
	!.
solutionDisplayMode(invalid) -->
	[_].
%---------------------------------------------------------------------------------------------------

scopuri_princ :-
	scop(Atr),
	determina(Atr),
	appendSolutionsToFile(Atr),
	askWhichDisplayModeShouldBeUsed(DisplayMode),
	afiseaza_scop(DisplayMode, Atr),
	fail.
scopuri_princ.

%---------------------------------------------------------------------------------------------------
/* This predicate is used to log the solutions obtained by the expert system in the logging directory. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% appendSolutionsToFile(+Goal)
%---------------------------------------------------------------------------------------------------
appendSolutionsToFile(Goal) :-
	(
		directory_exists('./fisiere_conferinte'),
		!
		;
		make_directory('./fisiere_conferinte')
	),
	open('./fisiere_conferinte/log_solutii.txt', append, Stream),
	datime(datime(Year, Month, Day, Hour, Minute, Second)),
	format(Stream, '(~p/~p/~p, ~p:~p:~p) Solutii sistem expert:\n', [Year, Month, Day, Hour, Minute, Second]),
	logSolutions(Stream, Goal),
	close(Stream),
	!.
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to log all the solutions deducted by the expert system for the given goal
in the specified file. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% logSolutions(+Stream, +Goal)
%---------------------------------------------------------------------------------------------------
logSolutions(Stream, Goal) :-
	fapt(av(Goal, Val), FC, _),
	FC >= 20,
	logSolution(Stream, av(Goal, Val), FC),
	fail.
logSolutions(_, _).
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to log a solution in the logging file. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% logSolution(+Stream, av(+Goal, +Value), +CertaintyFactor)
%---------------------------------------------------------------------------------------------------
logSolution(Stream, av(Goal, Val), FC) :-
	format(Stream, '~p este ~p\n', [Goal, Val]),
	FC1 is integer(FC),
	format(Stream, 'Factorul de certitudine este: ~p\n', [FC1]).
%---------------------------------------------------------------------------------------------------

determina(Atr) :-
	realizare_scop(av(Atr, _), _, [scop(Atr)]),
	!.
determina(_).

afiseaza_scop(summary, Atr) :-
	nl,
	fapt(av(Atr, Val), FC, _),
	FC >= 20,
	scrie_scop(av(Atr, Val), FC),
	nl,
	fail.
afiseaza_scop(summary, _) :-
	nl, nl.

afiseaza_scop(detail, Atr) :-
	nl,
	fapt(av(Atr, Val), FC, _),
	FC >= 20,
	scrie_scop(av(Atr, Val), FC),
	solution_info(Val, Description, _, datime(Year, Month, Day, _, _, _)),
	nl, nl, write(Description), nl, nl,
	datime(datime(CurrentYear, CurrentMonth, CurrentDay, _, _, _)),
	Year =:= CurrentYear,
	Month >= CurrentMonth,
	used_language(Lang),
	prompter(Lang, detail_goal_3, Prompt),
	Day1 is integer(Day),
	Month1 is integer(Month),
	Year1 is integer(Year),
	format(Prompt, [Day1, Month1, Year1]), nl, nl,
	fail.
afiseaza_scop(detail, _) :-
	nl, nl.

scrie_scop(av(Atr, Val), FC) :-
	transformare(av(Atr, Val), X),
	scrie_lista(X),
	tab(2),
	write(' '),
	write('factorul de certitudine este '),
	FC1 is integer(FC),
	write(FC1).

realizare_scop(not Scop, Not_FC, Istorie) :-
	realizare_scop(Scop, FC, Istorie),
	Not_FC is - FC,
	!.
realizare_scop(av(Atr, _), FC, _) :-
	fapt(Scop, FC, _),
	Scop = av(Atr, nu_conteaza), !.
realizare_scop(Scop,FC,_) :-
	fapt(Scop, FC, _),
	!.
realizare_scop(Scop, FC, Istorie) :-
	pot_interoga(Scop, Istorie),
	!,
	realizare_scop(Scop, FC, Istorie).
realizare_scop(Scop, FC_curent, Istorie) :-
	fg(Scop, FC_curent, Istorie).

fg(Scop, FC_curent, Istorie) :-
	regula(N, premise(Lista), concluzie(Scop, FC)),
	demonstreaza(N, Lista, FC_premise, Istorie),
	ajusteaza(FC, FC_premise, FC_nou),
	actualizeaza(Scop, FC_nou, FC_curent, N),
	FC_curent == 100,
	!.
fg(Scop, FC, _) :-
	fapt(Scop, FC, _).

pot_interoga(av(Atr, _), Istorie) :-
	not interogat(av(Atr, _)),
	interogabil(Atr, Optiuni, Mesaj),
	interogheaza(Atr, Mesaj, Optiuni, Istorie), nl,
	asserta( interogat(av(Atr, _)) ).

cum([]) :-
	write('Scop? '), nl,
	write('|:'),
	citeste_linie(Linie), nl,
	transformare(Scop, Linie),
	cum(Scop).
cum(L) :-
	transformare(Scop, L), nl,
	cum(Scop).
cum(not Scop) :-
	fapt(Scop, FC, Reguli),
	lista_float_int(Reguli, Reguli1),
	FC < -20,
	transformare(not Scop, PG),
	append(PG, [a,fost,derivat,cu, ajutorul, 'regulilor: '|Reguli1], LL),
	scrie_lista(LL), nl,
	afis_reguli(Reguli),
	fail.
cum(Scop) :-
	fapt(Scop, FC, Reguli),
	lista_float_int(Reguli, Reguli1),
	FC > 20,
	transformare(Scop, PG),
	append(PG, [a, fost, derivat, cu, ajutorul, 'regulilor: ' | Reguli1], LL),
	scrie_lista(LL), nl,
	afis_reguli(Reguli),
	fail.
cum(_).

afis_reguli([]).
afis_reguli([N|X]) :-
	afis_regula(N),
	premisele(N),
	afis_reguli(X).
afis_regula(N) :-
	regula(N, premise(Lista_premise),
	concluzie(Scop, FC)),
	NN is integer(N),
	scrie_lista(['regula  ', NN]),
	scrie_lista(['  Daca']),
	scrie_lista_premise(Lista_premise),
	scrie_lista(['  Atunci']),
	transformare(Scop, Scop_tr),
	append(['   '], Scop_tr, L1),
	FC1 is integer(FC),
	append(L1, [FC1], LL),
	scrie_lista(LL), nl.

scrie_lista_premise([]).
scrie_lista_premise([H|T]) :-
	transformare(H,H_tr),
	tab(5),
	scrie_lista(H_tr),
	scrie_lista_premise(T).

transformare(av(A, da), [A]) :- !.
transformare(not av(A, da), [not, A]) :- !.
transformare(av(A, nu), [not, A]) :- !.
transformare(av(A, V), [A, este, V]).

premisele(N) :-
	regula(N, premise(Lista_premise), _),
	!,
	cum_premise(Lista_premise).

cum_premise([]).
cum_premise([Scop|X]) :-
	cum(Scop),
	cum_premise(X).
        
interogheaza(Atr, Mesaj, [da, nu], Istorie) :-
	!,
	write(Mesaj), nl,
	scrie_lista(['(', da, nu, nu_stiu, nu_conteaza, ')']),
	de_la_utiliz(X, Istorie, [da, nu, nu_stiu, nu_conteaza]),
	det_val_fc(X, Val, FC),
	asserta( fapt(av(Atr, Val), FC, [utiliz]) ).
interogheaza(Atr, Mesaj, Optiuni, Istorie) :-
	write(Mesaj), nl,
	citeste_opt(VLista, Optiuni, Istorie),
	assert_fapt(Atr, VLista).

citeste_opt(X, Optiuni, Istorie) :-
	append(['('], Optiuni, Opt1),
	append(Opt1, [nu_stiu, nu_conteaza], Opt2),
	append(Opt2, [')'], Opt),
	append(Optiuni, [nu_stiu, nu_conteaza], Optiuni1),
	scrie_lista(Opt),
	de_la_utiliz(X, Istorie, Optiuni1).

de_la_utiliz(X, Istorie, Lista_opt) :-
	repeat,
	write(': '),
	citeste_linie(X),
	proceseaza_raspuns(X, Istorie, Lista_opt).

proceseaza_raspuns([de_ce], Istorie, _) :-
	nl,
	afis_istorie(Istorie),
	!,
	fail.

proceseaza_raspuns([X], _, Lista_opt) :-
	member(X, Lista_opt).
proceseaza_raspuns([X, fc, FC], _, Lista_opt) :-
	member(X, Lista_opt),
	float(FC).

assert_fapt(Atr, [Val, fc, FC]) :-
	!,
	asserta( fapt(av(Atr, Val), FC,[utiliz]) ).
assert_fapt(Atr, [Val]) :-
	asserta( fapt(av(Atr, Val), 100, [utiliz]) ).

det_val_fc([nu], da, -100).
det_val_fc([nu, FC], da, NFC) :-
	NFC is -FC.
det_val_fc([nu, fc, FC], da, NFC) :-
	NFC is -FC.
det_val_fc([Val, FC], Val, FC).
det_val_fc([Val, fc, FC], Val, FC).
det_val_fc([Val], Val, 100).
        
afis_istorie([]) :-
	nl.
afis_istorie([scop(X) | T]) :-
	scrie_lista([scop,X]),
	!,
	afis_istorie(T).
afis_istorie([N | T]) :-
	afis_regula(N),
	!,
	afis_istorie(T).

demonstreaza(N, ListaPremise, Val_finala, Istorie) :-
	dem(ListaPremise, 100, Val_finala, [N | Istorie]),
	!.

dem([], Val_finala, Val_finala, _).
dem([H | T], Val_actuala, Val_finala, Istorie) :-
	realizare_scop(H, FC, Istorie),
	Val_interm is min(Val_actuala, FC),
	Val_interm >= 20,
	dem(T, Val_interm, Val_finala, Istorie).
 
actualizeaza(Scop, FC_nou, FC, RegulaN) :-
	fapt(Scop, FC_vechi, _),
	combina(FC_nou, FC_vechi, FC),
	retract( fapt(Scop, FC_vechi, Reguli_vechi) ),
	asserta( fapt(Scop, FC, [RegulaN | Reguli_vechi]) ),
	!.
actualizeaza(Scop, FC, FC, RegulaN) :-
	asserta( fapt(Scop, FC, [RegulaN]) ).

ajusteaza(FC1, FC2, FC) :-
	X is FC1 * FC2 / 100,
	FC is round(X).
combina(FC1, FC2, FC) :-
	FC1 >= 0,
	FC2 >= 0,
	X is FC2 * (100 - FC1) / 100 + FC1,
	FC is round(X).
combina(FC1, FC2, FC) :-
	FC1 < 0,
	FC2 < 0,
	X is - ( -FC1 -FC2 * (100 + FC1)/100),
	FC is round(X).
combina(FC1, FC2, FC) :-
	(
		FC1 < 0
		;
		FC2 < 0
	),
	(
		FC1 > 0
		;
		FC2 > 0
	),
	FCM1 is abs(FC1),
	FCM2 is abs(FC2),
	MFC is min(FCM1, FCM2),
	X is 100 * (FC1 + FC2) / (100 - MFC),
	FC is round(X).

incarca :-
	used_language(Lang),
	prompter(Lang, ask_for_rules_file_name, RulesPrompt),
	write(RulesPrompt), nl,
	write('|: '),
	read(RulesFileName),
	file_exists(RulesFileName),
	prompter(Lang, ask_for_solution_info_file_name, SolutionInfoPrompt),
	write(SolutionInfoPrompt), nl,
	read(SolutionInfoFileName),
	file_exists(SolutionInfoFileName),
	!,
	incarca(RulesFileName, SolutionInfoFileName).
incarca :-
	used_language(Lang),
	prompter(Lang, file_does_not_exist, Prompt),
	write(Prompt), nl,
	fail.

%---------------------------------------------------------------------------------------------------
/* This predicate loads the goal, rules and questions from the first file and the information about
the solutions from the second file into the knowledge base. */
%---------------------------------------------------------------------------------------------------
% incarca(+RulesFileName, +SolutionInfoFileName)
%---------------------------------------------------------------------------------------------------
incarca(RulesFileName, SolutionInfoFileName) :-
	resetKnowledgeBase,
	see(RulesFileName),
	loadRules,
	seen,
	see(SolutionInfoFileName),
	loadSolutionsInformation,
	seen,
	computeCalendar,
	!.
%---------------------------------------------------------------------------------------------------
/* This predicate is used to get the list of domains for the solutions. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% getDomainList(-DomainList)
%---------------------------------------------------------------------------------------------------
getDomainList(Domains) :-
	setof(
		Domain,
		Name^ Description^ Domain^ Year^ Month^ Day^ Hour^ Minute^ Second^
			solution_info(Name, Description, Domain, datime(Year, Month, Day, Hour, Minute, Second)),
		Domains
		).
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to compute and assert the calendar for the domains. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% computeCalendar
%---------------------------------------------------------------------------------------------------
computeCalendar :-
	retractall(calendar(_)),
	getDomainList(DomainList),
	CalendarHeader = [' ', 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12],
	computeCalendarEntries(CalendarEntries, DomainList),
	append([CalendarHeader], CalendarEntries, Calendar),
	assertz(calendar(Calendar)).
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to compute the entries in the calendar. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% computeCalendarEntries(-CalendarEntries, +Domains)
%---------------------------------------------------------------------------------------------------
computeCalendarEntries([Entry | OtherEntries], [Domain | OtherDomains]) :-
	getDomainMonths(Domain, DomainMonths),
	Entry = [Domain | DomainMonths],
	computeCalendarEntries(OtherEntries, OtherDomains).
computeCalendarEntries([], []) :-
	!.
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to extract how many conferences per month are. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% getDomainMonths(+Domain, -DomainMonths)
%---------------------------------------------------------------------------------------------------
getDomainMonths(Domain, DomainMonths) :-
	findall(
		Month,
		Name^ Description^ Year^ Month^ Day^ Hour^ Minute^ Second^
		solution_info(Name, Description, Domain, datime(Year, Month, Day, Hour, Minute, Second)),
		Months
		),
	clumped(Months, Months1),
	sort(Months1, Months2),
	computeDomainMonths(DomainMonths, 1.0, Months2).
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to create a list with the number of conferences per each month based on
the given input. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% computeDomainMonths(-MonthlyCount, +CurrentMonthIndex, +DomainMonths)
%---------------------------------------------------------------------------------------------------
computeDomainMonths([], 13.0, _) :-
	!.
computeDomainMonths([MonthCount | Rest], CurrentMonth, Months) :-
	(
		Months = [CurrentMonth-MonthCount | RemainingMonths]
		;
		RemainingMonths = Months,
		MonthCount = 0
	),
	NextMonth is CurrentMonth + 1.0,
	computeDomainMonths(Rest, NextMonth, RemainingMonths).
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to get the calendar from the knowledge base and pass it to the display
predicate. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% displayCalendar
%---------------------------------------------------------------------------------------------------
displayCalendar :-
	calendar(Calendar),
	displayCalendar(Calendar), nl,
	!.
displayCalendar :-
	write('Nu exista niciun calendar de afisat!'), nl, nl.
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to display the calendar that shows how many conferences from each domain
are in each month. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% displayCalendar(+Calendar)
%---------------------------------------------------------------------------------------------------
displayCalendar([[DomainColumn | MonthColumns] | NextEntries]) :-
	format('~30s', [DomainColumn]),
	displayCalendarMonthColumns(MonthColumns),
	displayCalendar(NextEntries).
displayCalendar([]).
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to display the elements of the calendar that are part of the columns with
the month count. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% displayCalendarMonthColumns(+MonthColumns)
%---------------------------------------------------------------------------------------------------
displayCalendarMonthColumns([]) :- nl.
displayCalendarMonthColumns([H|T]) :-
	number_chars(H,NN),
	atom_chars(H1, NN),
	format('~2s', [H1]), tab(1),
	displayCalendarMonthColumns(T).
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate loads the information about the expert system's solutions and adds them in the
knowledge base. */
%---------------------------------------------------------------------------------------------------
% loadSolutionInformation
%---------------------------------------------------------------------------------------------------
loadSolutionsInformation :-
	repeat,
		readSolutionInfo(TokenList),
		(
			append(SolutionInfo, [end_of_file], TokenList),
			Last = [end_of_file]
			;
			SolutionInfo = TokenList
		),
		processSolutionInformation(SolutionInfo),
		Last == [end_of_file],
	nl.
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate extracts the information about the solution from the given list of tokens and adds
that information in the knowledge base. */
%---------------------------------------------------------------------------------------------------
% processSolutionInformation(+TokenList)
%---------------------------------------------------------------------------------------------------
processSolutionInformation([end_of_file]) :-
	!.
processSolutionInformation(TokenList) :-
	parseSolutionInformation(Result, TokenList, []),
	assertz(Result).
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This DCG rule extracts the information about the solution from the given list of tokens. */
%---------------------------------------------------------------------------------------------------
% parseSolutionInformation(-SolutionInfo, +TokenList, [])
%---------------------------------------------------------------------------------------------------
parseSolutionInformation(solution_info(Name, Description, Domain, Date)) -->
	parseSolutionName(Name),
	parseSolutionDescription(Description),
	parseSolutionDomain(Domain),
	parseSolutionDate(Date).
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This DCG rule extracts the name of the solution from given list of tokens. */
%---------------------------------------------------------------------------------------------------
% parseSolutionName(-SolutionName, +TokenList, [])
%---------------------------------------------------------------------------------------------------
parseSolutionName(Name) -->
	['{', Name, '}'].
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This DCG rule extracts the description of the solution from the given list of tokens. */
%---------------------------------------------------------------------------------------------------
% parseSolutionDescription(-SolutionDescription, +TokenList, [])
%---------------------------------------------------------------------------------------------------
parseSolutionDescription(Description) -->
	['{', Description, '}'].
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This DCG rule extracts the domain of the solution from the given list of tokens.  */
%---------------------------------------------------------------------------------------------------
% parseSolutionDomain(-SolutionDomain, +TokenList, [])
%---------------------------------------------------------------------------------------------------
parseSolutionDomain(Domain) -->
	['{', domeniu, ':', Domain, '}'].
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This DCG rule extracts the starting date of the solution from the given list of tokens.  */
%---------------------------------------------------------------------------------------------------
% parseSolutionDate(-SolutionDate, +TokenList, [])
%---------------------------------------------------------------------------------------------------
parseSolutionDate(datime(Year, Month, Day, 0, 0, 0)) -->
	['{', data, ':', Day, '/', Month, '/', Year, '}'].
%---------------------------------------------------------------------------------------------------

loadRules :-
	repeat,
	citeste_propozitie(L),
	proceseaza(L),
	L == [end_of_file],
	nl.

proceseaza([end_of_file]) :-
	!.
proceseaza(L) :-
	parseTokens(R, L, []),
	assertz(R),
	!.
parseTokens(scop(AtributScop)) -->
	[scop, '-', '>', '(', AtributScop, ')'].
parseTokens(interogabil(Atr, M, P)) --> 
	[q, '-', '>', Atr],
	parseOptions(M),
	afiseaza(Atr, P).
parseTokens(regula(RuleID, premise(Premises), concluzie(Conclusion, CertaintyFactor))) -->
	parseRuleID(RuleID),
	parseRulePremises(Premises),
	parseRuleConclusion(Conclusion, CertaintyFactor).
parseTokens('Eroare la parsare' -L, L, _).

parseOptions(M) -->
	[variante, '('],
	parseOptionList(M).
parseOptionList([Element]) -->
	[Element, ')'].
parseOptionList([Element | T]) -->
	[Element, '(', '&', ')'],
	parseOptionList(T).

afiseaza(_, P) -->
	[text, '-', '>', '(', P, ')'].
afiseaza(P, P) -->
	[].

%---------------------------------------------------------------------------------------------------
/* This predicate is used to extract the identifier of the rule that is currently parsed. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% parseRuleID(-RuleID, +TokenList, [])
%---------------------------------------------------------------------------------------------------
parseRuleID(RuleID) -->
	[rg, '-', '>', RuleID].
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to parse past the rule premises header and extract the list of premises. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% parseRulePremises(-RulePremises, +TokenList, [])
%---------------------------------------------------------------------------------------------------
parseRulePremises(Premises) -->
	[if,'('],
	parsePremiseList(Premises).
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to extract the list of premises of the current rule from the given list of
tokens. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% parsePremiseList(-PremiseList, +TokenList, [])
%---------------------------------------------------------------------------------------------------
parsePremiseList([Premise]) -->
	parseRulePremise(Premise),
	[')', then].
parsePremiseList([Premise | NextPremises]) -->
	parseRulePremise(Premise),
	['&', '&'],
	parsePremiseList(NextPremises).
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to extract a premise from the given list of tokens. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% parseRulePremise(-Premise, +TokenList, [])
%---------------------------------------------------------------------------------------------------
parseRulePremise(not av(AttributeName, da)) -->
	[AttributeName, '-', '>', '(', 0.0, ')'].
parseRulePremise(av(AttributeName, da)) -->
	[AttributeName, '-', '>', '(', 1.0, ')'].
parseRulePremise(av(AttributeName, AttributeValue)) -->
	[AttributeName, '-', '>', '(', AttributeValue,')'].
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
/* This predicate is used to extract the conclusion of the rule from the given list of tokens. */
%---------------------------------------------------------------------------------------------------
% Recommended usage:
% parseRuleConclusion(-Conclusion, -CertaintyFactor, +TokenList, [])
%---------------------------------------------------------------------------------------------------
parseRuleConclusion(Conclusion, CertaintyFactor) -->
	['('],
	parseRulePremise(Conclusion),
	[:],
	[fc, '-', '>'],
	[CertaintyFactor],
	[')'].
parseRuleConclusion(Conclusion, 100) -->
	['('],
	parseRulePremise(Conclusion),
	[')'].
%---------------------------------------------------------------------------------------------------

citeste_linie([Cuv | Lista_cuv]) :-
	get_code(Car),
	citeste_cuvant(Car, Cuv, Car1), 
	rest_cuvinte_linie(Car1, Lista_cuv). 

% -1 este codul ASCII pt EOF

rest_cuvinte_linie(-1, []) :-
	!.
rest_cuvinte_linie(Car, []) :-
	(
		Car == 13
		;
		Car == 10
	),
	!.
rest_cuvinte_linie(Car, [Cuv1 | Lista_cuv]) :-
	citeste_cuvant(Car, Cuv1, Car1),      
	rest_cuvinte_linie(Car1, Lista_cuv).

citeste_propozitie([Cuv | Lista_cuv]) :-
	get_code(Car),
	citeste_cuvant(Car, Cuv, Car1),
	rest_cuvinte_propozitie(Car1, Lista_cuv). 

rest_cuvinte_propozitie(-1, []) :-
	!.
rest_cuvinte_propozitie(Car, []) :-
	Car == 46,
	!.
rest_cuvinte_propozitie(Car, [Cuv1 | Lista_cuv]) :-
	citeste_cuvant(Car, Cuv1, Car1),      
	rest_cuvinte_propozitie(Car1, Lista_cuv).

citeste_cuvant(-1, end_of_file, -1) :-
	!.
citeste_cuvant(Caracter, Cuvant, Caracter1) :-
	caracter_cuvant(Caracter),
	!,
	name(Cuvant, [Caracter]),
	get_code(Caracter1).
citeste_cuvant(Caracter, Numar, Caracter1) :-
	caracter_numar(Caracter),
	!,
	citeste_tot_numarul(Caracter, Numar, Caracter1).

citeste_tot_numarul(Caracter, Numar, Caracter1) :-
	determina_lista(Lista1, Caracter1),
	append([Caracter], Lista1, Lista),
	transforma_lista_numar(Lista, Numar).

determina_lista(Lista, Caracter1) :-
	get_code(Caracter), 
	(
		caracter_numar(Caracter),
		determina_lista(Lista1, Caracter1),
		append([Caracter], Lista1, Lista)
		;
		\+(caracter_numar(Caracter)),
		Lista=[],
		Caracter1 = Caracter
	).

transforma_lista_numar([], 0).
transforma_lista_numar([H | T], N) :-
	transforma_lista_numar(T, NN), 
	lungime(T, L), Aux is exp(10, L),
	HH is H - 48, N is HH * Aux + NN.

lungime([], 0).
lungime([_| T], L) :-
	lungime(T, L1),
	L is L1 + 1.

tab(N) :-
	N > 0,
	write(' '),
	N1 is N - 1,
	tab(N1).
tab(0).

% 39 este codul ASCII pt '

citeste_cuvant(Caracter, Cuvant, Caracter1) :-
	Caracter==39,
	!,
	pana_la_urmatorul_apostrof(Lista_caractere),
	L = [Caracter | Lista_caractere],
	name(Cuvant, L),
	get_code(Caracter1).

pana_la_urmatorul_apostrof(Lista_caractere) :-
	get_code(Caracter),
	(
		Caracter == 39,
		Lista_caractere = [Caracter]
		;
		Caracter\==39,
		pana_la_urmatorul_apostrof(Lista_caractere1),
		Lista_caractere = [Caracter | Lista_caractere1]
	).

citeste_cuvant(Caracter, Cuvant, Caracter1) :-
	caractere_in_interiorul_unui_cuvant(Caracter),
	!,
	(
		(Caracter > 64, Caracter < 91),
		!,
		Caracter_modificat is Caracter+32
		;
		Caracter_modificat is Caracter
	),
	citeste_intreg_cuvantul(Caractere, Caracter1),
	name(Cuvant, [Caracter_modificat|Caractere]).

citeste_intreg_cuvantul(Lista_Caractere, Caracter1) :-
	get_code(Caracter),
	(
		caractere_in_interiorul_unui_cuvant(Caracter),
		(
			(Caracter > 64, Caracter < 91),
			!,
			Caracter_modificat is Caracter + 32
			;
			Caracter_modificat is Caracter
		),
		citeste_intreg_cuvantul(Lista_Caractere1, Caracter1),
		Lista_Caractere=[Caracter_modificat|Lista_Caractere1]
		;
		\+(caractere_in_interiorul_unui_cuvant(Caracter)),
		Lista_Caractere = [],
		Caracter1 = Caracter
	).

citeste_cuvant(_, Cuvant, Caracter1) :-
	get_code(Caracter),       
	citeste_cuvant(Caracter, Cuvant, Caracter1).

caracter_cuvant(C) :-
	member(C, [33, 38, 40, 41, 44, 45, 46, 47, 58, 59, 62, 63, 123, 125]).

% am specificat codurile ASCII pentru , ; : ? ! . ) ( - > &

caractere_in_interiorul_unui_cuvant(C) :-
	C>64,
	C<91;
	C>47, C<58;
	C==95;
	C>96, C<123.
caracter_numar(C) :-
	C < 58,
	C >= 48.

%---------------------------------------------------------------------------------------------------
/* This predicate reads tokens from the file that contains the information about the solutions until
it reaches a delimiter or the end of the file. */
%---------------------------------------------------------------------------------------------------
% readSolutionInfo(-TokenList)
%---------------------------------------------------------------------------------------------------
readSolutionInfo(TokenList) :-
	citeste_linie(Line),
	(
		Line == [end_of_file],
		TokenList = [end_of_file],
		!
		;
		Line = [FirstToken | _],
		FirstToken == '-',
		TokenList = [],
		!
		;
		readSolutionInfo(OtherLines),
		append(Line, OtherLines, TokenList)
	).
%---------------------------------------------------------------------------------------------------
