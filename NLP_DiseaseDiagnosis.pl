% FINAL PROJECT: Rithikha and Bashara

% Model queries:
/*question(['Ive', had, joint, pain, for, a, really, long, time], [], A).
question(['Ive', had, fever, for, a, really, long, time], [], A).
question(['I', have, had, itchy, spots, for, days], [], A).
question(['I', have, been, feeling, tired, for, a, really, long, time], [], A).
question(['My', knees, hurt, since, last, year], [], A).*/


% description_phrase(T0,T4,Diag) is true if
% T0 and T4 are list of words, such that T4 is an ending of T0
% and T0-T4 form a description phrase of the symptoms.
% Diag is the Disease that the description phrase is potentialy referring to
description_phrase(T0, T4, Diag, T1, T2, T3) :-
  det_phrase(T0, T1, Diag),
  symptom_phrase(T1, T2, Diag),
  preposition_phrase(T2, T3, Diag),
  duration_phrase(T3, T4, Diag).

% det_phrase(T0,T1,Diag) is true if T0-T1 is a det is true of Diag
det_phrase(T0,T2,Diag) :-
  det(T0,T1,Diag),
  det_phrase(T1,T2,Diag).
det_phrase(T,T,_).

opt_connector_phrase(T,T,_).
opt_connector_phrase(T0, T1, Diag) :-
  opt(T0,T1,Diag).


% symptom_phrase(T0,T1,Diag) is true if T0-T1 is a symptom is true of Diag
% symptom_phrase(T0,T3,Diag) :-
%    symp(T0,T1,Diag),
%    opt_connector_phrase(T1, T2, Diag),
%    symp(T2, T3, Diag).
% symptom_phrase(T,T,_).

% symptom_phrase(T0,T1,Diag) is true if T0-T1 is a symptom is true of Diag
symptom_phrase(T0,T2,Diag) :-
    symp(T0,T1,Diag),
    symptom_phrase(T1,T2,Diag).
symptom_phrase(T,T,_).

% preposition_phrase(T0,T1,Diag) is true if T0-T1 is a preposition/adjective is true of Diag
preposition_phrase(T0,T2, Diag) :-
   prep(T0,T1, Diag),
   preposition_phrase(T1,T2, Diag).
preposition_phrase(T,T,_).

 % duration_phrase(T0,T1,Diag) is true if T0-T1 is a duration is true of Diag
 duration_phrase(T0,T2,Diag) :-
     dur(T0,T1,Diag),
     duration_phrase(T1,T2,Diag).
 duration_phrase(T,T,_).

% DICTIONARY
opt([and | T],T,_).
opt([',' | T],T,_).

det([had | T],T,_).
det([a | T],T,_).
det([have| T],T,_).
det([been| T],T,_).
det([having| T],T,_).
det([feeling| T],T,_).
det(['I'| T],T,_).
det(['My'| T],T,_).
det(['Ive'| T],T,_). %Note Caps words needs to be in '' because prolog would otherwise think it's a variable

symp([H1 | T],T,Diag) :- symptom(H1, Diag).
symp([H1, H2 | T],T,Diag) :- symptom([H1, H2], Diag).
symp([H1, H2, H3 | T],T,Diag) :- symptom([H1, H2, H3], Diag).

prep([for | T],T,_).
prep([since | T],T,_).
prep([a| T],T,_).
prep([really| T],T,_).
prep([very| T],T,_).
prep([last| T],T,_).

dur([H1 | T],T,Diag) :- duration(H1, Diag).
dur([H1, H2 | T],T,Diag) :- duration([H1, H2], Diag).
dur([H1, H2, H3 | T],T,Diag) :- duration([H1, H2, H3], Diag).

% DATABASE

%1 symp chickenPox
symptom(fever, chickenPox).
symptom(tired, chickenPox).
symptom([loss, of, appetite], chickenPox).
symptom([appetite,loss], chickenPox).
symptom([not, hungry], chickenPox).
symptom([dont, want, to, eat], chickenPox).
symptom([itchy, spots], chickenPox).
symptom([itchy, bumps], chickenPox).
%1 symp arthritis
symptom([joint, pain], arthritis).
symptom(tired, arthritis).
symptom([knees, hurt], arthritis).
symptom([wrists, hurt], arthritis).
symptom([fingers, hurt], arthritis).
symptom(stiff, arthritis).
symptom(swelling, arthritis).

%2 symp chickenPox
symptom([loss, of, appetite, and, itchy, spots], chickenPox).
%2 symp arthritis

%3 symp chickenPox
%3 symp arthritis

duration([long,time], arthritis).
duration(month,arthritis).
duration(months,arthritis).
duration(year,arthritis).
duration(years,arthritis).

duration([couple, of, days], chickenPox).
duration([long,time], chickenPox).
duration(week,chickenPox).
duration(weeks,chickenPox).
duration(day,chickenPox).
duration(days,chickenPox).

% DIAGNOSIS

diagnosis(S1,S2,S3,S4,chickenPox):-
  symp(S1,S2, chickenPox),
  dur(S3,S4,chickenPox).

diagnosis(S1,S2,S3,S4,arthritis):-
  symp(S1,S2, arthritis),
  dur(S3,S4, arthritis).

% USER INPUT AND OUTPUT

% question(Question,QR,Diagnosis) is true if Query provides an answer about Object to Question
question(T0,T1,Diag) :-
  description_phrase(T0,T1,Diag, S1, S2, S3), diagnosis(S1, S2, S3, T1, Diag).

% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
  question(Q,[],A).

% To get the input from a line:
q(Ans) :-
  write("Hi there, welcome to the online healthcare portal. Please tell us at most 3 symptoms you are experiencing,
followed by how long you have had them for. We will do our best to figure out what's going on: "), flush_output(current_output),
  readln(Ln),
  question(Ln,End,Ans),
  member(End,[[],['?'],['.']]).
